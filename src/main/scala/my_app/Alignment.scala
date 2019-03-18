package reader

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.furman.classics.citealign._
import scala.scalajs.js.Dynamic.{ global => g }
import scala.concurrent._
//import ExecutionContext.Implicits.global

import scala.scalajs.js.annotation.JSExport
import js.annotation._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

@JSExportTopLevel("Alignment")
object Alignment {

	// Alignment relation urn
	val relationUrn:Cite2Urn = Cite2Urn("urn:cite2:cite:verbs.v1:aligns")

	case class BoundAlignment(alignment:Cite2Urn, texts:Vector[CtsUrn], index:Int)	

	// Alignments already in the CEX
	val currentAlignments:Vars[BoundAlignment] = Vars.empty

	// for saving unsaved alignments
	val unsavedAlignmentUrn:Cite2Urn = Cite2Urn("urn:cite2:temp:alignment.v:temp")
	val unsavedAlignmentLabel:Var[String] = Var("new alignment")

	// Clicked but not saved
	val unsavedAlignment:Var[Option[CiteAlignment]] = Var[Option[CiteAlignment]](None)
	// Mapping by text-version for display
	val unsavedAlignmentMap:Vars[((CtsUrn,Vars[(CtsUrn,String)]),Int)] = Vars.empty

	val alignmentMgr:Var[Option[CiteAlignmentManager]] = Var[Option[CiteAlignmentManager]](None)

	// shorten long passages for display
	def ellipsisText(s:String):String = {
		s.size match {
			case n if (n > 15) => {
				s"${s.take(7)}…${s.takeRight(7)}"
			}
			case _ => s
		}
	}

	// build out
	def clearAll:Unit = {
		g.console.log("clear all")
	}

	// add alignments for the texts displayed to the bound corpus
	def alignmentsForCorpus(vCorp:O2Model.BoundCorpus):Unit = {
		alignmentMgr.value match {
			case Some(am) => {
				val u:CtsUrn = vCorp.versionUrn.value
				val av:Set[CiteAlignment] = am.alignmentsForText(u)
				vCorp.alignments.value = av.size
			}
			case None => 
		}
	}

	// Click on a citable-node to start or add to a new alignment
	def newAlignmentClick(vCorp:O2Model.BoundCorpus, node:CitableNode):Unit = {
		/* mark or unmark element */
		val textIndex:Int = {
			val tempV:Vector[CtsUrn] = O2Model.currentCorpus.value.toVector.map(_.versionUrn.value)
			val vu:CtsUrn = vCorp.versionUrn.value
			tempV.indexOf(vu)
		}
		val el = js.Dynamic.global.document.getElementById(s"node_${node.urn}").asInstanceOf[HTMLAnchorElement]
		val classList:String = el.getAttribute("class")
		if (classList.contains("newAlignment")) {
			val newCL:String = classList.replaceAll(" newAlignment[0-9]+","").replaceAll("newAlignment","") .replaceAll("  "," ")
			el.setAttribute("class",newCL)
			Alignment.removeNodeFromUnsavedAlignment(node)
		} else {
			val newCL:String = classList + s" newAlignment newAlignment${textIndex}"
			el.setAttribute("class",newCL)
			Alignment.addNodeToAlignment(node)
		}
	}

	// remove a citable-node from the current alignment-in-progress
	def removeNodeFromAlignment(node:CitableNode):Unit = {
		removeNodeFromUnsavedAlignment(node:CitableNode)
	}

	// continues the work of a click by adding the urn to the in-progress alignment,
	// creating a new tempAlignment if necessary
	def addNodeToAlignment(node:CitableNode):Unit = {
		unsavedAlignment.value match {
			case Some(ua) => {
				addNodeToUnsavedAlignment(node)	
			}
			case None => {
				createUnsavedAlignment(node)
				addNodeToUnsavedAlignment(node)	
			}
		}
	}

	// remove a citable-node from the current alignment-in-progress
	def removeNodeFromUnsavedAlignment(n:CitableNode):Unit = {
		// do map first for quick display
		unsavedAlignmentMap.value.size match {
			case n if (n < 1) => {
				// do nothing
			}
			case _ => {
				//val newMap:Vars[((CtsUrn,Vars[(CtsUrn,String)]),Int)] = Vars.empty
				val usaVec:Vector[((CtsUrn,Vector[(CtsUrn,String)]),Int)] = unsavedAlignmentMap.value.toVector.map(vv => ((vv._1._1, vv._1._2.value.toVector),vv._2))
				val usaSimpleMap:Vector[(CtsUrn, Vector[(CtsUrn,String)])] = usaVec.map(_._1)
				val tupleMap:Vector[(CtsUrn,String)] = usaSimpleMap.map(_._2).flatten
				val newTuple:Vector[(CtsUrn,String)] = Vector((n.urn,ellipsisText(n.text)))
				val sortedMap:Vector[(CtsUrn,String)] = sortUrns(tupleMap ++ newTuple)
				val filteredMap:Vector[(CtsUrn,String)] = sortedMap.filter(_._1 != n.urn)		
				val groupedAndSorted:Vector[(CtsUrn, Vector[(CtsUrn,String)])] = groupByOrdered(filteredMap)
				val andIndexed:Vector[((CtsUrn,Vector[(CtsUrn,String)]),Int)] = groupedAndSorted.zipWithIndex
				unsavedAlignmentMap.value.clear
				for (t <- andIndexed) {
					val thisWorkUrn = t._1._1
					val thisVec = t._1._2
					val thisIndex = t._2
					val tempVars:Vars[(CtsUrn,String)] = Vars.empty
					for (v <- thisVec) {
						val tup:(CtsUrn,String) = (v._1, v._2)
						tempVars.value += tup
					}
					val mapTup:(CtsUrn, Vars[(CtsUrn,String)]) = (thisWorkUrn,tempVars)
					val indexedMapTup:((CtsUrn, Vars[(CtsUrn,String)]),Int) = (mapTup,thisIndex)
					unsavedAlignmentMap.value += indexedMapTup
				}
				// now remove from actual object
				try {
					unsavedAlignment.value match {
						case Some(ua) => {
							val urn:Cite2Urn = ua.urn
							val label:String = ua.label
							val passages:Vector[CtsUrn] = filteredMap.map(_._1)
							val newAlignment:CiteAlignment = CiteAlignment(urn,label,passages)
							unsavedAlignment.value = Some(newAlignment)
						}
						case None => {
							throw new Exception(s"Unaccountably, we got here when there is no existing unsaved alighment: addNodeToTempAlignment(n = ${n}.")
						}
					}
				} catch {
					case e:Exception => {
						val errorStr:String = s"Failed to add node to temporary alignment: ${e}."
						O2Controller.updateUserMessage(errorStr,2)
					}
				}	
			}
		}
	}
	// Add a node to the alignment-in-progress
 	def addNodeToUnsavedAlignment(n:CitableNode):Unit = {
		// First add to unsavedAlignmentMap for quick display
		val usaVec:Vector[((CtsUrn,Vector[(CtsUrn,String)]),Int)] = unsavedAlignmentMap.value.toVector.map(vv => ((vv._1._1, vv._1._2.value.toVector),vv._2))
		val usaSimpleMap:Vector[(CtsUrn, Vector[(CtsUrn,String)])] = usaVec.map(_._1)
		val tupleMap:Vector[(CtsUrn,String)] = usaSimpleMap.map(_._2).flatten
		val newTuple:Vector[(CtsUrn,String)] = Vector((n.urn,ellipsisText(n.text)))
		val sortedMap:Vector[(CtsUrn,String)] = sortUrns(tupleMap ++ newTuple)
		val groupedAndSorted:Vector[(CtsUrn, Vector[(CtsUrn,String)])] = groupByOrdered(sortedMap)
		val andIndexed:Vector[((CtsUrn,Vector[(CtsUrn,String)]),Int)] = groupedAndSorted.zipWithIndex
		unsavedAlignmentMap.value.clear
		for (t <- andIndexed) {
			val thisWorkUrn = t._1._1
			val thisVec = t._1._2
			val thisIndex = t._2
			val tempVars:Vars[(CtsUrn,String)] = Vars.empty
			for (v <- thisVec) {
				val tup:(CtsUrn,String) = (v._1, v._2)
				tempVars.value += tup
			}
			val mapTup:(CtsUrn, Vars[(CtsUrn,String)]) = (thisWorkUrn,tempVars)
			val indexedMapTup:((CtsUrn, Vars[(CtsUrn,String)]),Int) = (mapTup,thisIndex)
			unsavedAlignmentMap.value += indexedMapTup
		}
		// now add to unsavedAlignment
		try {
			unsavedAlignment.value match {
				case Some(ua) => {
					val urn:Cite2Urn = ua.urn
					val label:String = ua.label
					val passages:Vector[CtsUrn] = sortedMap.map(_._1)
					val newAlignment:CiteAlignment = CiteAlignment(urn,label,passages)
					unsavedAlignment.value = Some(newAlignment)
				}
				case None => {
					throw new Exception(s"Unaccountably, we got here when there is no existing unsaved alighment: addNodeToTempAlignment(n = ${n}.")
				}
			}
		} catch {
			case e:Exception => {
				val errorStr:String = s"Failed to add node to temporary alignment: ${e}."
				O2Controller.updateUserMessage(errorStr,2)
			}
		}
	}

	def createUnsavedAlignment(n:CitableNode):Unit = {
		val label:String = unsavedAlignmentLabel.value
		val urn:Cite2Urn = unsavedAlignmentUrn
		val ca:CiteAlignment = CiteAlignment(urn, label, Vector(n.urn))
		unsavedAlignment.value = Some(ca)
	}

	def sortUrns(vu:Vector[(CtsUrn,String)]):Vector[(CtsUrn,String)] = {
		alignmentMgr.value match {
			case Some(am) => {
				val justUs:Vector[CtsUrn] = vu.map(_._1)
				val sortedUs:Vector[CtsUrn] = am.sortPassages(justUs)
				val sortedTuples:Vector[(CtsUrn,String)] = {
					sortedUs.map( u => {
						val t:(CtsUrn,String) = vu.find(_._1 == u).get
						t
					})
				}
				sortedTuples
			}
			case None => {
				vu
			}
		}
	}

	def groupByOrdered(nv:Vector[(CtsUrn,String)]):Vector[(CtsUrn, Vector[(CtsUrn,String)])] = {
		/* Example of how to use groupBy in Scala without losing object order */
		import scala.collection.mutable.LinkedHashMap
		val v2 = nv.zipWithIndex.groupBy( n => n._1._1.dropPassage)
		val v3 = LinkedHashMap(v2.toSeq sortBy (_._2.head._2): _*)
		val v4 = v3 mapValues (_ map (_._1))
		val v5:Vector[(CtsUrn, scala.collection.immutable.Vector[(CtsUrn, String)])] = v4.toVector
		v5
	}

	def clearUnsavedAlignment(ua:CiteAlignment):Unit = {
		try {
			val passages:Vector[CtsUrn] = ua.passages
			for (p <- passages) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = classList.replaceAll("newAlignment[0-9]+","").replaceAll("newAlignment","")
				el.setAttribute("class",newClass)
			}
			unsavedAlignment.value = None
			unsavedAlignmentMap.value.clear
		} catch {
			case e:Exception => {
				val errorStr:String = s"Failed to clear temporary alignment: ${e}."
				O2Controller.updateUserMessage(errorStr,2)
			}
		}
	}

	@dom
	def saveTempAlignment:Unit = {
		try {
			unsavedAlignment.value match {
				case Some(ua) => {
					val newIndex:Int = {
						if (currentAlignments.value.toVector.size < 1) 0
						else currentAlignments.value.toVector.last.index + 1
					}
					val passageVec:Vector[CtsUrn] = {
						unsavedAlignmentMap.value.toVector.map(_._1._2.value.toVector.map(_._1)).flatten
					}
					val newBoundAlignment:BoundAlignment = BoundAlignment(ua.urn, passageVec, newIndex)
					// add to list
					currentAlignments.value += newBoundAlignment
					// add to all bound-corpora
					val corps:Vector[O2Model.BoundCorpus] = O2Model.currentCorpus.value.toVector
					val alignUrns:Vector[CtsUrn] = currentAlignments.value.toVector.map(_.texts).flatten
					for (c <- O2Model.currentCorpus.value) {

							val nodes:Vector[CtsUrn] = c.versionNodes.value.toVector.map(_.nodes.value.toVector).flatten.map(_.urn)
							val sharedUrns:Vector[CtsUrn] = nodes.intersect(alignUrns)
							c.alignments.value = sharedUrns.size
					}

					clearUnsavedAlignment(ua)
				}
				case None => throw new Exception("Did Alignment.saveTempAlighment with no unsavedAlignment!")
			}
		} catch {
			case e:Exception => {
				val errorStr:String = s"Failed to save temporary alignment: ${e}."
				O2Controller.updateUserMessage(errorStr,2)
			}
		}
	}

	def deleteAlignment(dela:BoundAlignment):Unit = {
		// delete from currentAlignments
		val thisIndex:Int = dela.index
		unHighlightMarkedNodes(thisIndex)
		val tempVec:Vector[BoundAlignment] = currentAlignments.value.toVector.filter( ca => {
			ca.index != thisIndex
		})

	 	val renumedVec:Vector[BoundAlignment] = tempVec.zipWithIndex.map( tv => {
	 		BoundAlignment(tv._1.alignment, tv._1.texts, tv._2)
	 	})
		currentAlignments.value.clear
		for (al <- tempVec) currentAlignments.value += al
		// recalculate alignments…
		val corps:Vector[O2Model.BoundCorpus] = O2Model.currentCorpus.value.toVector
		val alignUrns:Vector[CtsUrn] = currentAlignments.value.toVector.map(_.texts).flatten
		for (c <- O2Model.currentCorpus.value) {

				val nodes:Vector[CtsUrn] = c.versionNodes.value.toVector.map(_.nodes.value.toVector).flatten.map(_.urn)
				val sharedUrns:Vector[CtsUrn] = nodes.intersect(alignUrns)
				c.alignments.value = sharedUrns.size
		}
	}

	def highlightMarkedNodes(i:Int) = {
		val currentList:Vector[CtsUrn] = O2Model.currentListOfUrns.value.toVector
		val urns:Vector[CtsUrn] = currentAlignments.value.toVector.filter(_.index == i).map(_.texts).flatten
		for (p <- urns) {
			if (currentList.contains(p)) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = s"${classList} tokenForMarker${i % 20}"
				el.setAttribute("class",newClass)
			}
		}
	}

	def unHighlightMarkedNodes(i:Int) = {
		val currentList:Vector[CtsUrn] = O2Model.currentListOfUrns.value.toVector
		val urns:Vector[CtsUrn] = currentAlignments.value.toVector.filter(_.index == i).map(_.texts).flatten
		for (p <- urns) {
			if (currentList.contains(p)) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class").replaceAll("tokenForMarker[0-9]+"," ").replaceAll(" +"," ")	
				el.setAttribute("class",classList)
			}		
		}
	}

	def markAlignmentsInText:Unit = {
		val alVec:Vector[BoundAlignment] = currentAlignments.value.toVector

	}

// display aligments (from the CEX) as markers on the on-screen text
	def loadAlignments(vCorp:O2Model.BoundCorpus):Unit = {
		var timeStart = new js.Date().getTime()
		val wholeTimeStart = timeStart

		// Turn on alignment mode (affects navigation)
		O2Model.clearPassage
		//Stash URN
		val u:CtsUrn = vCorp.versionUrn.value
		g.console.log(s"u = ${u}")
		val alignmentsForThisCorpus:Vector[Int] = {
			val alignedNodes:Vector[CtsUrn] = vCorp.versionNodes.value.toVector.map(_.nodes.value.toVector).flatten.map(_.urn)
			val alignments:Vector[Int] = {
				currentAlignments.value.toVector.filter( ca => {
					alignedNodes.intersect(ca.texts).size > 0
				}).map(_.index)
			}
			alignments
		}
		g.console.log(s"""alignmentsForThisCorpus =  ${alignmentsForThisCorpus.mkString(" ")}""")

		val allUrnsForAlignments:Vector[CtsUrn] = {
			val allAlignments:Vector[BoundAlignment] = {
				currentAlignments.value.toVector.filter(ca => {
					alignmentsForThisCorpus.contains(ca.index)
				})
			}
			allAlignments.map( aa => {
				aa.texts
			}).flatten.distinct
		}

		g.console.log(s"""allUrnsForAlignments =  ${allUrnsForAlignments.mkString(" ")}""")

		val groupedByVersion:Vector[(CtsUrn,Vector[CtsUrn])] = allUrnsForAlignments.groupBy(_.dropPassage).toVector

		g.console.log(s"""groupedByVersion =  ${groupedByVersion.mkString(" ")}""")

		val rangeUrns:Vector[CtsUrn] = {
			groupedByVersion.map( gbv => {
				val urns:Vector[CtsUrn] = gbv._2
				val version:CtsUrn = gbv._1	
				val trCorpus:Vector[CtsUrn] = O2Model.textRepo.value.get.corpus.nodes.map(_.urn)
				val indexedNodes:Vector[(Int, CtsUrn)] = urns.map( u => {
					val i:Int = trCorpus.indexOf(u)
					(i, u)
				})

				val sortedUrns:Vector[CtsUrn] = indexedNodes.sortBy(_._1).map(_._2)

				val depth:Int = sortedUrns.head.citationDepth.head
				val expandedUrns:Vector[CtsUrn] = {
					depth match {
						case 1 => sortedUrns
						case _ => {
							sortedUrns.map(_.collapsePassageBy(1)).distinct
						}
					}
				}
				val fromUrn:CtsUrn = expandedUrns.head
				val toUrn:CtsUrn = 	expandedUrns.last
				if ( fromUrn == toUrn ) fromUrn
				else CtsUrn(s"${fromUrn}-${toUrn.passageComponent}")
			})
		}

		for (u <- rangeUrns) O2Model.displayPassage(u)

		/* report */
		var timeEnd = new js.Date().getTime()
		g.console.log("==========================")
		g.console.log(s"Ran test in ${(timeEnd - timeStart)/1000} seconds.")
	}

	
}
