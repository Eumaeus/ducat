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

	// Alignments already in the CEX
	val currentAlignments:Vars[(Cite2Urn,Int)] = Vars.empty
	// Map of CtsUrn to Int for current corpus
	val currentAlignmentMap:Vars[(CtsUrn,Int)] = Vars.empty

	// Alignments created by clicking and hitting the disk icon
	val newAlignments:Vars[(CiteAlignment,Int)] = Vars.empty

	// for saving unsaved alignments
	val unsavedAlignmentUrn:Cite2Urn = Cite2Urn("urn:cite2:temp:alignment.v:temp")
	val unsavedAlignmentLabel:Var[String] = Var("new alignment")

	// Clicked but not saved
	val unsavedAlignment:Var[Option[CiteAlignment]] = Var[Option[CiteAlignment]](None)
	// Mapping by text-version for display
	val unsavedAlignmentMap:Vars[((CtsUrn,Vars[(CtsUrn,String)]),Int)] = Vars.empty



	val alignmentMgr:Var[Option[CiteAlignmentManager]] = Var[Option[CiteAlignmentManager]](None)


	def mergeAllAlignments(s:String):Unit = {
		alignmentMgr.value match {
			case Some(am) => {
				g.console.log("mergeAllAlignments " + s)	
				val tempNewAlignments:Vector[CiteAlignment] = newAlignments.value.toVector.map(_._1)
				val tempCurrentAlignments:Vector[(Cite2Urn,Int)] = currentAlignments.value.toVector
				val mappedCurrent:Vector[CiteAlignment] = tempCurrentAlignments.map( tca => {
						am.getAlignments(tca._1)
				}).flatten
				val newCurrent:Vector[(Cite2Urn,Int)] = (tempNewAlignments ++ mappedCurrent).map(_.urn).zipWithIndex			
				val newNew:Vector[(CiteAlignment,Int)] = (tempNewAlignments ++ mappedCurrent).zipWithIndex			
				clearAll
				for (a <- newCurrent) {
					currentAlignments.value += a
				}
				for (a <- newNew) {
					newAlignments.value += a
				}
				for ( (a,i) <- newCurrent ){
						val urns:Vector[CtsUrn] = am.passagesForAlignment(a)
						for ( u <- urns) {
							val tempMap:(CtsUrn,Int) = (u, i)	
							currentAlignmentMap.value += tempMap
						}
					}
				//g.console.log(s"${newCurrent.mkString("\n")}")	

			}
			case None => 
		}	

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

	// display aligments (from the CEX) as markers on the on-screen text
	def loadAlignments(vCorp:O2Model.BoundCorpus):Unit = {
		alignmentMgr.value match {
			case Some(am) => {
				// Turn on alignment mode (affects navigation)
				O2Model.alignMode.value = true
				O2Model.clearPassage
				//Stash URN
				val u:CtsUrn = vCorp.versionUrn.value
				val als:Vector[Cite2Urn] = am.alignmentsForText(u).map(_.urn).toVector
				for (a <- als.zipWithIndex) currentAlignments.value += a
				if (als.size > 0){
					val c:Corpus = am.corpusForAlignments(als, false)
					for (vu <- c.citedWorks){ 
						// make a range from the first and last
						val u:CtsUrn = {
							val uus:Vector[CtsUrn] = (c ~~ vu).urns
							val u1:CtsUrn = uus.head
							val u2:CtsUrn = uus.last
							val u2psg:String = u2.passageComponent
							CtsUrn(s"${u1}-${u2psg}")
						}
						O2Model.displayPassage(u) 
					}
					// This also takes forever, but the visual display is done at this point
					for ( (a,i) <- als.zipWithIndex ){
						val urns:Vector[CtsUrn] = am.passagesForAlignment(a)
						for ( u <- urns) {
							val tempMap:(CtsUrn,Int) = (u, i)	
							currentAlignmentMap.value += tempMap
						}
					}
					mergeAllAlignments("Alignments.loadAlignments")
				}				
			}
			case None => {}
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
			Alignment.removeNodeFromAlignment(node)
		} else {
			val newCL:String = classList + s" newAlignment newAlignment${textIndex}"
			el.setAttribute("class",newCL)
			Alignment.addNodeToAlignment(node)
		}
	}

	// remove a citable-node from the current alignment-in-progress
	def removeNodeFromAlignment(node:CitableNode):Unit = {
		removeNodeFromTempAlignment(node:CitableNode)
	}

	// continues the work of a click by adding the urn to the in-progress alignment,
	// creating a new tempAlignment if necessary
	def addNodeToAlignment(node:CitableNode):Unit = {
		unsavedAlignment.value match {
			case Some(ua) => {
				addNodeToTempAlignment(node)	
			}
			case None => {
				createTempAlignment(node)
				addNodeToTempAlignment(node)	
			}
		}
	}

	// shorten long passages for display
	def ellipsisText(s:String):String = {
		s.size match {
			case n if (n > 15) => {
				s"${s.take(7)}…${s.takeRight(7)}"
			}
			case _ => s
		}
	}

	// remove a citable-node from the current alignment-in-progress
	def removeNodeFromTempAlignment(n:CitableNode):Unit = {
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
 	def addNodeToTempAlignment(n:CitableNode):Unit = {
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

	def createTempAlignment(n:CitableNode):Unit = {
		val label:String = unsavedAlignmentLabel.value
		val urn:Cite2Urn = unsavedAlignmentUrn
		val ca:CiteAlignment = CiteAlignment(urn, label, Vector(n.urn))
		unsavedAlignment.value = Some(ca)
	}

	def saveTempAlignment:Unit = {
	//	val newAlignments:Vars[(CiteAlignment,Int)] = Vars.empty
		try {
			unsavedAlignment.value match {
				case Some(ua) => {
					val newIndex:Int = newAlignments.value.length
					val newTuple:(CiteAlignment,Int) = (ua, newIndex)
					newAlignments.value += newTuple
					// Add to currentAlignmentMap
							
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

	def removeHighlightingFromPassages:Unit = {
		try {
			val bc:Vector[O2Model.BoundCorpus] = O2Model.currentCorpus.value.toVector
			val urnVec:Vector[CtsUrn] = bc.map(b => {
				val vn:Vector[O2Model.VersionNodeBlock] = b.versionNodes.value.toVector
				val uv:Vector[CtsUrn] = vn.map( v => {
					v.nodes.value.toVector
				}).flatten.map(_.urn)
				uv
			}).flatten
			for (p <- urnVec) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = classList.replaceAll("newAlignment[0-9]+","").replaceAll("newAlignment","")
				el.setAttribute("class",newClass)
			}
			// But if there are unsaved ones… restore them 
			restoreUnsavedAlignmentHilighting

		} catch {
			case e:Exception => {
				val errorStr:String = s"Failed to clear highlighting: ${e}."
				O2Controller.updateUserMessage(errorStr,2)
			}
		}
	}

	def restoreUnsavedAlignmentHilighting:Unit = {
		val usas:Vector[(CtsUrn, Int)] = {
				val corpVec:Vector[(CtsUrn,Int)] = {
					unsavedAlignmentMap.value.toVector.map( a => {
						val i:Int = a._2
						val v:Vector[(CtsUrn,String)] = a._1._2.value.toVector
						val vi:Vector[(CtsUrn,Int)] = v.map( u => {
							(u._1,i)	
						})
						vi
					}).flatten
				}
				corpVec
			}
			for (p <- usas) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p._1}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = s"o2_passage newAlignment newAlignment${p._2}"
				el.setAttribute("class",newClass)
			}
	}

	def highlightMarkedNodes(i:Int) = {
		val urns:Vector[CtsUrn] = currentAlignmentMap.value.toVector.filter(_._2 == i).map(_._1)
		for (p <- urns) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = s"${classList} tokenForMarker${i % 20}"
				el.setAttribute("class",newClass)
			}
	}

	def unHighlightMarkedNodes(i:Int) = {
		val urns:Vector[CtsUrn] = currentAlignmentMap.value.toVector.filter(_._2 == i).map(_._1)
		for (p <- urns) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class").replaceAll("tokenForMarker[0-9]+"," ").replaceAll(" +"," ")	
				el.setAttribute("class",classList)
			}		
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

	def clearAll = {
		Alignment.unsavedAlignmentMap.value.clear
		Alignment.currentAlignmentMap.value.clear
		Alignment.newAlignments.value.clear
		Alignment.currentAlignments.value.clear
		Alignment.newAlignments.value.clear
	}

	def deleteUnsavedAlignment(sa:(CiteAlignment, Int)):Unit = {
	//	val newAlignments:Vars[(CiteAlignment,Int)] = Vars.empty
		// Remove any highlighting
		removeHighlightingFromPassages
		val nas:Vector[(CiteAlignment,Int)] = newAlignments.value.toVector
		val newVec:Vector[(CiteAlignment,Int)] = nas.filter(_._2 != sa._2).map(_._1).zipWithIndex
		newAlignments.value.clear
		for (a <- newVec) { newAlignments.value += a }
	}


	/* HERE!!! FIX!!! Look for present passages first. 
		Don't throw a million errors.
		Fade saved alignments not visible on screen!
	*/
	def loadFromNewAlignments(i:Int):Unit = {
		if (newAlignments.value.size > 0) {
			unsavedAlignment.value match {
				case Some(ua) => {
					//clearUnsavedAlignment(ua)
				}
				case None => // do nothing
			}
			// clear all anyway
			val allUrns:Vector[CtsUrn] = {
				newAlignments.value.toVector.map(_._1.passages).flatten
			}
			for (p <- allUrns){
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = classList.replaceAll("newAlignment[0-9]+","")
				el.setAttribute("class",newClass)
			}

			val als:Vector[(CiteAlignment,Int)] = newAlignments.value.toVector
			val thisOne:CiteAlignment = als.filter(_._2 == i)(0)._1
			val passages:Vector[CtsUrn] = thisOne.passages
			for (p <- passages) {
				val el = js.Dynamic.global.document.getElementById(s"node_${p}").asInstanceOf[HTMLSpanElement]
				val classList:String = el.getAttribute("class")	
				val newClass:String = classList + s"newAlignment newAlignment${i}"
				el.setAttribute("class",newClass)
			}
		}
	}

	def toCEX:String = {
		if (newAlignments.value.size > 0) {
			// Assign Urns
			val boundAs:Vector[(CiteAlignment,Int)] = newAlignments.value.toVector
			val dlVector:Vector[(CiteAlignment,Int)] = boundAs.map( a => {
						val d = new js.Date()
						val urn:Cite2Urn = {
							val urnBase:String = SaveDialog.newAlignmentCollectionUrn.value.toString
							val yearMonth:String = s"${d.getFullYear()}${d.getMonth()}"
							val hours:String = s"${d.getHours()}_${d.getMinutes()}_${d.getSeconds()}_${d.getMilliseconds()}"
							val index:Int = a._2
							val urnString:String = s"${urnBase}${yearMonth}${hours}_${index}"
							Cite2Urn(urnString)
						}
						val newAlignment:CiteAlignment = CiteAlignment(urn,a._1.label,a._1.passages)
						(newAlignment, a._2)
					})

			// Collection Def
			val aCollDef:String = {
				val header1:String = "#!citecollections"
				val header2:String = "URN#Description#Labelling property#Ordering property#License"
				val urn:String = SaveDialog.newAlignmentCollectionUrn.value.toString
				val labelProp:String = SaveDialog.newAlignmentCollectionUrn.value.addProperty("label").toString
				val desc:String = "Citation Alignments"
				val orderProp:String  = ""
				val lic:String = "CC-BY 3.0"

				val header:Vector[String] = Vector(header1, header2)
				val bodyLine:Vector[String] = Vector(Vector(urn, desc, labelProp, orderProp, lic).mkString("#"))
				(header ++ bodyLine).mkString("\n")
			}
			// Property Def
			val aPropsDef:String = {
				val header1:String = "#!citeproperties"
				val header2:String = "Property#Label#Type#Authority list"
				val props:Vector[(String,String)] = {
					Vector(("urn","#Alignment Record#Cite2Urn#"),
					("label","#Label#String#"),
					("description","#Description#String#"),
					("editor","#Editor#String#"),
					("date","#Date#String#") )
				}
				val propLines:Vector[String] = props.map(p => {
					val urn:String = SaveDialog.newAlignmentCollectionUrn.value.addProperty(p._1).toString
					s"${urn}${p._2}"
				})
				val header:Vector[String] = Vector(header1, header2)
				(header ++ propLines).mkString("\n")
			}

			// Collection Data
			val aCollData:String = {
				val header1:String = "#!citedata"
				val header2:String = "urn#label#description#editor#date"
				val header:Vector[String] = Vector(header1, header2)
				val alignStrings:Vector[String] = {
					dlVector.map( a => {
						val d = new js.Date()
						val label:String = s"Alignment ${a._2}"
						val desc:String = s"Textual Alignment created with the Ducat tool on: ${new js.Date()}."
						val editor:String = SaveDialog.defaultEditorName.value
						val date:String = d.toUTCString
						val strVec:Vector[String] = Vector(a._1.urn.toString,label,desc,editor,date)
						strVec.mkString("#")
					})
				}
				(header ++ alignStrings).mkString("\n")
			}
			// Relations
			val aRelations:String = {
				val header:String = "#!relations"
				val aRelations:String = dlVector.map( a => {
					val s:Cite2Urn = a._1.urn
					val v:Cite2Urn = relationUrn
					val rels:String = a._1.passages.map( p => {
						s"${s}#${v}#${p}"
					}).mkString("\n")
					rels
				}).mkString("\n")
				header + "\n" + aRelations
			}

			val cexVec:String = Vector(aCollDef,aPropsDef,aCollData,aRelations).mkString("\n\n")
			cexVec
		} else {
			""
		}
	}	


}
