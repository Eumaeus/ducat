package reader

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.Dynamic.{ global => g }
import scala.collection.mutable.LinkedHashMap
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import scala.collection.immutable.SortedMap
import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("O2Model")
object O2Model {

	var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null

	case class VersionNodeBlock(versionUrn:Var[CtsUrn],nodes:Vars[CitableNode])  {
		val groupedNodes:Vars[(CtsUrn,Vars[CitableNode])] = {
			val v1:Vector[CitableNode] = nodes.value.toVector
			val v2 = v1.zipWithIndex.groupBy( n => n._1.urn.collapsePassageBy(1))
			val v3 = LinkedHashMap(v2.toSeq sortBy (_._2.head._2): _*)
			val v4 = v3 mapValues (_ map (_._1))
			val v5:Vector[(CtsUrn, scala.collection.immutable.Vector[CitableNode])] = v4.toVector
			val emptyGroupedNodes:Vars[(CtsUrn,Vars[CitableNode])] = Vars.empty[(CtsUrn,Vars[CitableNode])]
			for ( gn <- v5) {
				val emptyNodeVars:Vars[CitableNode] = Vars.empty[CitableNode]
				for (cn <- gn._2) emptyNodeVars.value += cn
				val newGroup:(CtsUrn,Vars[CitableNode]) = (gn._1, emptyNodeVars)
				emptyGroupedNodes.value += newGroup
			}
			emptyGroupedNodes
		}
	}

	case class BoundCorpus(versionUrn:Var[CtsUrn], versionLabel:Var[String], versionNodes:Vars[VersionNodeBlock], currentPrev:Var[Option[CtsUrn]] = Var[Option[CtsUrn]](None), currentNext:Var[Option[CtsUrn]] = Var[Option[CtsUrn]](None), versionsAvailable:Var[Int] = Var(1), alignments:Var[Int] = Var(0)  )

	val currentCorpus = Vars.empty[BoundCorpus]

	val currentNumberOfCitableNodes = Var(0)
	val currentListOfUrns = Vars.empty[CtsUrn]
	val isRtlPassage = Var(false)

	// for navigation
	val urnHistory = Vars.empty[(Int,CtsUrn,String)]

	// urn is what the user requested
	val urn = Var(CtsUrn("urn:cts:ns:group.work.version.exemplar:passage"))
	// displayUrn is what will be shown
	val displayUrn = Var(CtsUrn("urn:cts:ns:group.work.version.exemplar:passage"))
	//val versionsForCurrentUrn = Var(1)

	val userMessage = Var("")
	val userAlert = Var("default")
	val userMessageVisibility = Var("app_hidden")

	val textRepo = Var[Option[TextRepository]](None)
	val citedWorks = Vars.empty[CtsUrn]

	val currentNext = Var[Option[CtsUrn]](None)
	val currentPrev = Var[Option[CtsUrn]](None)


	/* Some methods for working the model */
	def versionsForUrn(urn:CtsUrn):Int = {
		O2Model.textRepo.value match {
			case None => 0
			case Some(tr) => {
				val s = s"urn:cts:${urn.namespace}:${urn.textGroup}.${urn.work}:"
				val versionVector = tr.catalog.entriesForUrn(CtsUrn(s))
				versionVector.size
			}
		}
	}


	def collapseToWorkUrn(urn:CtsUrn):CtsUrn = {
		val s = {
			s"urn:cts:${urn.namespace}:${urn.textGroup}.${urn.work}:${urn.passageComponent}"
		}
		val u = CtsUrn(s)
		u
	}

	def updateCurrentListOfUrns:Unit = {
		MainModel.waiting.value = true
		O2Model.currentListOfUrns.value.clear
		val clouVec:Vector[BoundCorpus] = currentCorpus.value.toVector
		val vnbVec:Vector[VersionNodeBlock] = clouVec.map(_.versionNodes.value.toVector).flatten
		val cnVec:Vector[CitableNode] = vnbVec.map(_.nodes.value.toVector).flatten
		val urnVec:Vector[CtsUrn] = cnVec.map(_.urn)
		for (n <- urnVec){
			O2Model.currentListOfUrns.value += n
		}	
		MainModel.waiting.value = false 
	}

	def removeTextFromCurrentCorpus(vCorp:O2Model.BoundCorpus):Unit = {
		MainModel.waiting.value = true
		try {
			val tempCorpus:Vector[O2Model.BoundCorpus] = {
				O2Model.currentCorpus.value.toVector.filter(_ != vCorp)
			}
			O2Model.currentCorpus.value.clear
			for ( tc <- tempCorpus){
				// updating Current Corpus
				O2Model.currentCorpus.value += tc 
			}	
			O2Model.updateCurrentListOfUrns

		} catch {
			case e:Exception => {
				O2Controller.updateUserMessage(s"O2Model Exception in 'removeTextFromCurrentCorpus': ${e}",2)
			}
		}
		MainModel.waiting.value = false 
	}

	def removeTextFromCurrentCorpus(urn:CtsUrn):Unit = {
		MainModel.waiting.value = true
		try {
			val tempCorpus:Vector[O2Model.BoundCorpus] = {
				O2Model.currentCorpus.value.toVector.filter( vc => (urn >= vc.versionUrn.value) == false )
			}
			O2Model.currentCorpus.value.clear
			// updating Current Corpus
			for ( tc <- tempCorpus){
				O2Model.currentCorpus.value += tc 
			}	
			O2Model.updateCurrentListOfUrns
		} catch {
			case e:Exception => {
				O2Controller.updateUserMessage(s"O2Model Exception in 'removeTextFromCurrentCorpus': ${e}",2)
			}
		}
		MainModel.waiting.value = false
	}

	def updateTextInCurrentCorpus(oldurn:CtsUrn, newurn:CtsUrn):Unit = {
		MainModel.waiting.value = true
		removeTextFromCurrentCorpus(oldurn)	
		displayPassage(newurn)	
		MainModel.waiting.value = false 
	}

	def updateCurrentCorpus(c:Corpus, u:CtsUrn):Unit = {
		MainModel.waiting.value = true
		try {
			//O2Model.currentCorpus.value.clear
			if (O2Model.textRepo.value != None) {
				// Since GroupBy doesn't preserve order, let's preserve our own order
				val versionLevelOrder:Vector[CtsUrn] = {
					c.urns.map(u => dropOneLevel(u)).distinct.toVector
				}
				// Get Corpus into a Vector of tuples: (version-level-urn, vector[CitableNode])
				val tempCorpusVector:Vector[(CtsUrn, Vector[CitableNode])] = c.nodes.groupBy(_.urn.dropPassage).toVector
				for (tc <- tempCorpusVector) {
					val versionLabel:String = O2Model.textRepo.value.get.catalog.label(tc._1)		
					val passageString:String = u.passageComponent
					val boundVersionLabel = Var(versionLabel)

					val versionUrn:CtsUrn = CtsUrn(s"${tc._1}${passageString}")
					val boundVersionUrn = Var(versionUrn)

					// Group node urns according to nodeBlocks
					val nodeBlocks:Vector[(CtsUrn, Vector[CitableNode])] = {
						tc._1.exemplarOption match {
							case Some(eo) => {
								val block:Vector[(CtsUrn,Vector[CitableNode])] = {
									tc._2.groupBy(n => dropOneLevel(n.urn)).toVector	
								}
								val block2 = tc._2.zipWithIndex.groupBy(n => dropOneLevel(n._1.urn))
								val lhm = LinkedHashMap(block2.toSeq sortBy (_._2.head._2): _*)
								val block3 = lhm mapValues (_ map (_._1))
								val sortedBlock = block3.toVector
								sortedBlock
							}
							case None => {
								Vector( (tc._1,tc._2)	)
							}
						}	
					}	
					// Get this nodeBlock into a versionNodeBlock
					val tempNodeBlockVec = Vars.empty[VersionNodeBlock]	
					for (b <- nodeBlocks){
						val tempBlockUrn = Var(b._1)
						val tempNodesVec = Vars.empty[CitableNode]
						for (n <- b._2) tempNodesVec.value += n
						tempNodeBlockVec.value += VersionNodeBlock(tempBlockUrn, tempNodesVec)
					}

					val newBoundCorpus:BoundCorpus = BoundCorpus(boundVersionUrn, boundVersionLabel, tempNodeBlockVec) 

					val sortingCorpus:Vector[BoundCorpus] = sortCorpora(O2Model.currentCorpus.value.toVector ++ Vector(newBoundCorpus))
					val sortedCorpora:Vector[BoundCorpus] = sortCorpora(sortingCorpus) 
					O2Model.currentCorpus.value.clear
					// updating Current Corpus
					for ( tc <- sortedCorpora){
						O2Model.currentCorpus.value += tc 
					}
					O2Model.updateCurrentListOfUrns
					val task2 = Task { newBoundCorpus.versionsAvailable.value = O2Model.versionsForUrn(newBoundCorpus.versionUrn.value) }
					val future2 = task2.runAsync
					val task3 = Task{ Alignment.alignmentsForCorpus(newBoundCorpus)}
					val future3 = task3.runAsync
				}	

			}

		} catch {
			case e:Exception => {
				O2Controller.updateUserMessage(s"O2Model Exception in 'updateCurrentCorpus': ${e}",2)
			}
		}
		MainModel.waiting.value = false
	}

	def dropOneLevel(u:CtsUrn):CtsUrn = { 
		try {
			val passage:String = u.passageComponent
			val plainUrn:String = u.dropPassage.toString
			val newPassage:String = passage.split('.').dropRight(1).mkString(".")
			val droppedUrn:CtsUrn = CtsUrn(s"${plainUrn}${newPassage}") 
			droppedUrn
		} catch {
			case e:Exception => {
				O2Controller.updateUserMessage(s"Error dropping one level from ${u}: ${e}",2)
				throw new Exception(s"${e}")
			}
		}
	}


	def displayNewPassage(urn:CtsUrn):Unit = {
		MainModel.waiting.value = true
		O2Model.displayPassage(urn)
		MainModel.waiting.value = false
	}

	@dom
	def clearPassage:Unit = {
		//O2Model.xmlPassage.innerHTML = ""
		//O2Model.versionsForCurrentUrn.value = 0
		O2Model.currentListOfUrns.value.clear
		O2Model.currentCorpus.value.clear
	}

	def passageLevel(u:CtsUrn):Int = {
		try {
			val urn = u.dropSubref
			if (urn.isRange) throw new Exception(s"Cannot report passage level for ${urn} becfause it is a range.")
			urn.passageComponent.size match {
				case n if (n > 0) => {
					urn.passageComponent.split('.').size
				}
				case _ => throw new Exception(s"Cannot report passage level for ${u} because it does not have a passage component.")
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")	
		}
	} 

	def valueForLevel(u:CtsUrn,level:Int):String = {
		try {
			val urn = u.dropSubref
			val pl:Int = passageLevel(urn)
			if (pl < level) throw new Exception(s"${u} has a ${pl}-deep citation level, which is less than ${level}.")
			urn.passageComponent.size match {
				case n if (n > 0) => {
					urn.passageComponent.split('.')(level-1)
				}
				case _ => throw new Exception(s"Cannot report passage level for ${u} because it does not have a passage component.")
			}
		} catch {
			case e:Exception => throw new Exception(s"${e}")	
		}	
	}

	@dom
	def displayPassage(newUrn: CtsUrn):Unit = {
		MainModel.waiting.value = true
		val tempCorpus: Corpus = O2Model.textRepo.value.get.corpus >= newUrn
	//	O2Model.updateCurrentListOfUrns(tempCorpus)
		O2Model.updateCurrentCorpus(tempCorpus, newUrn)
		O2Model.currentNumberOfCitableNodes.value = tempCorpus.size
		Alignment.recalculateAlignments
		MainModel.waiting.value = false
	}



	def checkForRTL(s:String):Boolean = {
		val sStart = s.take(10)
		val arabicBlock = "[\u0600-\u06FF]".r
		val hebrewBlock = "[\u0591-\u05F4]".r
		var isRtl:Boolean = ((arabicBlock findAllIn sStart).nonEmpty || (hebrewBlock findAllIn sStart).nonEmpty)
		isRtl
	}



	@dom
	def updateCitedWorks = {
		O2Model.citedWorks.value.clear
		for ( cw <- O2Model.textRepo.value.get.corpus.citedWorks){
			O2Model.citedWorks.value += cw
		}
	}

	def getUrlForCurrentState(cc:Vector[O2Model.BoundCorpus]):String = {
		var url:String = js.Dynamic.global.location.href.toString.split('?')(0)
		val urnString:String = {
			cc.length match {
				case n if (n > 0) => {
					val bs:Vector[O2Model.BoundCorpus] = cc
					val uv:Vector[String] = bs.map(_.versionUrn.value.toString)
					"?urn=" + uv.foldLeft("") { (acc, i) => acc + (i + "&urn=") }
				}
				case _ => {
					""
				}
			}
		}
		url + urnString
	}

	/*	Engage in a little trickery when updating the current corpus. 
	*	If we have two texts, and one is Greek/Latin, it should go first.
	*	If we have three, and one is Greek, put it in the middle
	*/
	def sortCorpora(tempCorpus:Vector[BoundCorpus]):Vector[BoundCorpus] = {
		textRepo.value match {	
			case Some(tr) => {	
					val langTuples:Vector[(String, BoundCorpus)] = {
						tempCorpus.sortBy(_.versionUrn.value.toString).map( c => {
								val u:CtsUrn = c.versionUrn.value
								val textVec:Vector[CatalogEntry] = tr.catalog.entriesForUrn(u)
								val langStr:String = {
									if (textVec.size > 0) {
										textVec.head.lang
									} else {
										"zzz"
									}
								}
								(langStr, c)
						}).sortBy(_._1)	
					}
					langTuples.size match {
						case n if (n == 1) => tempCorpus
						case n if (n == 2) => {
							val allGreek = langTuples.filter( t => ((t._1 == "grc") | (t._1 == "lat")))
							val notGreek = langTuples.filter( t => ((t._1 != "grc") & (t._1 != "lat")))
							(allGreek ++ notGreek).map(_._2)
						}
						case n if (n == 3) => {
							val allGreek = langTuples.filter( t => ((t._1 == "grc") | (t._1 == "lat")))
							val notGreek = langTuples.filter( t => ((t._1 != "grc") & (t._1 != "lat")))
							notGreek.size match {
								case n if (n == 0) => tempCorpus.sortBy(_.versionUrn.value.toString)
								case n if (n == 1) => {
									(Vector(notGreek.head) ++ allGreek).map(_._2)
								}
								case _ => {
									(Vector(notGreek.head) ++ allGreek ++ notGreek.tail).map(_._2)
								}
							}
						}
						case _ => tempCorpus.sortBy(_.versionUrn.value.toString)
					}
			}
			case None => tempCorpus.sortBy(_.versionUrn.value.toString)
		}
	}

}
