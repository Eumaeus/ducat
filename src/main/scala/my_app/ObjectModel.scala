package reader

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import scala.scalajs.js.Dynamic.{ global => g }
import js.annotation._
import collection.mutable
import collection.mutable._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._

import scala.scalajs.js.annotation.JSExport
import js.annotation._

@JSExportTopLevel("ObjectModel")
object ObjectModel {


	// The big data repo from the .cex file
	val collRep = Var[Option[CiteCollectionRepository]](None)

	val requiredColls:Vector[Cite2Urn] = Vector(Cite2Urn("urn:cite2:cite:datamodels.v1:"), Cite2Urn("urn:cite2:cite:verbs.v1:"))

	def toCEX(cr:Option[CiteCollectionRepository]):String = {
		val newCollUrn:Cite2Urn = SaveDialog.newAlignmentCollectionUrn.value
		val downloadOption:String = SaveDialog.downloadAlignmentOption.value
		cr match {
			case Some(cr) => {
				val collBlock:String = catalogBlockCEX(Some(cr),newCollUrn,downloadOption)
				collBlock
			}
			case None => ""
		}
	}	

	/* Here we're filtering out incomplete collections (no data, etc.) and
	*  also (for now) any collection matching SaveDialog.newAlignmentCollectionUrn
	*/
	def catalogBlockCEX(cr:Option[CiteCollectionRepository], newCollUrn:Cite2Urn, downloadOption:String):String = {
		cr match {
			case Some(cr) => {
				val collHeader:String = collectionsHeader
				// make lines for new collection
				//val newCollCEX:String = newAlignCollCEX(newCollUrn)
				// make lines for catalog
				val cat:Vector[CiteCollectionDef] = {
					downloadOption match {
						case "all" => {
							cr.catalog.collections.filter( _.urn != SaveDialog.newAlignmentCollectionUrn.value)
						}
						case _ => {
							val reqd:Vector[CiteCollectionDef] = cr.catalog.collections.filter(c => {
								requiredColls.contains(c.urn)
							})
							reqd
						}
					}
				}
				val cexDefs:String = collHeader + "\n" + cat.map(collectionDef2CEX(_)).mkString("\n")

				// While we're here, let's do the property blocks as well
				val cexPropertyBlocks:String = {
					cat.filter(_.urn != SaveDialog.newAlignmentCollectionUrn.value).filter(_.propertyDefs.size > 0).map(propertyDefs2CEX(_)).mkString("\n\n")
				}

				// And the CiteData itself
				val includeColls:Vector[Cite2Urn] = {
					val emptyFiltered:Vector[Cite2Urn] = {
						cr.catalog.collections.filter( c => { 
							cr.objectsForCollection(c.urn).size > 0 
						}).map(_.urn).filter(_ != SaveDialog.newAlignmentCollectionUrn.value)
					}
					val useColls:Vector[Cite2Urn] = {
						downloadOption match {
							case "all" => {
								emptyFiltered
							}
							case _ => {
								val reqd:Vector[Cite2Urn] = emptyFiltered.filter(c => {
									requiredColls.contains(c)
								})
								reqd
							}
						}
					}
					useColls
				}	
				val cexCiteDataBlocks:String = includeColls.map( collUrn => {
					val propDefs:Vector[CitePropertyDef] = cr.catalog.collection(collUrn) match {
						case Some(cd) => cd.propertyDefs
						case _ => Vector[CitePropertyDef]()
					}


					val dataHeader:String = {
						if (propDefs.size > 0) { 
							"\n#!citedata\n" + propDefs.map(_.urn.property).mkString("#") + "\n"
						} else { "" }
					}	
					val dataRecords:String = {
						if (propDefs.size > 0) { 
							val objUrns:Vector[Cite2Urn] = cr.collectionsMap(collUrn)
							if (objUrns.size > 0) {	
								val objData:String = objUrns.map( ou => {
									val obj:CiteObject = cr.citableObject(ou)
									val props:Vector[CitePropertyImplementation] = obj.propertyList
									val propStr:Vector[String] = props.map(p => {
										p.propertyValue.toString
									})
									val urnAndLabel:Vector[String] = {
										Vector( obj.urn.toString, obj.label)
									}
									( urnAndLabel ++ propStr).mkString("#")
								}).mkString("\n")
								objData
							} else { "" }
						} else { ""}
					}
					dataHeader + dataRecords
				}).mkString("\n")

				// don't forget the new alignments!
				cexDefs + "\n\n" + cexPropertyBlocks ++ "\n\n" ++ cexCiteDataBlocks
			}
			case None => ""
		}
	}

	def propertyDefs2CEX(cd: CiteCollectionDef):String = {
		val header:String = citePropertiesHeader
		val propDefs:Vector[CitePropertyDef] = cd.propertyDefs
		val propVec:Vector[String] = propDefs.map(pd => {
			s"""${pd.urn}#${pd.label}#${pd.propertyType.toString.replaceAll("Type","")}#${pd.vocabularyList.mkString(",")}"""
		})
		val cexBlock:String = (Vector(header) ++ propVec).mkString("\n")
		cexBlock
	}

	// def newAlignCollCEX(u:Cite2Urn):String = {
	// 	val catEntry:CiteCollectionDef = newAlignCollDef(u)
	// 	val c = catEntry
	// 	//URN#Description#Labelling property#Ordering property#License
	// 	collectionDef2CEX(c)
	// }

	def collectionDef2CEX(cd: CiteCollectionDef):String = {
		val urn:String = cd.urn.toString
		val collectionLabel:String = cd.collectionLabel
		val labelProperty:String = cd.labelProperty.toString
		val orderingProperty:String = {
			cd.orderingProperty match {
				case Some(op) => op.toString
				case None => ""
			}
		}
		val license:String = cd.license
		val stringVec:Vector[String] = Vector(urn, collectionLabel, labelProperty,orderingProperty,license)
		stringVec.mkString("#")

	}

	// def newAlignCollDef(u:Cite2Urn):CiteCollectionDef  = {
	// 	val collectionLabel:String = s"Alignment collection."
	// 	val isOrdered:Boolean = false
	// 	val labelProperty:Cite2Urn = u.addProperty("label")
	// 	val labellingProperty:Option[Cite2Urn] = Some(u)
	// 	val license:String = "CC BY 3.0"
	// 	val orderingProperty: Option[Cite2Urn] = None
	// 	val urn:Cite2Urn = u
	// 	val props:Vector[(String,String, CitePropertyType)] = Vector(("urn","URN",Cite2UrnType),("label","Label",StringType),("description","Description",StringType),("editor","Editor",StringType),("date","Date",StringType))
	// 	val propertyDefOptions:Vector[Option[CitePropertyDef]] = props.map(p => newPropDef(u.addProperty(p._1), p))
	// 	val filtPropertyDefOptions:Vector[Option[CitePropertyDef]] = propertyDefOptions.filter(pd => {
	// 			pd match {
	// 				case Some(propDef) => true
	// 				case None => false
	// 			}	
	// 	})
	// 	val propertyDefs:Vector[CitePropertyDef] = filtPropertyDefOptions.map( pdo => { pdo.get })
 // 		val ccf:CiteCollectionDef = new CiteCollectionDef(urn, collectionLabel, propertyDefs, license, labellingProperty, orderingProperty)
 // 		ccf
	// }

	// def newPropDef(u:Cite2Urn, p:(String,String,CitePropertyType)):Option[CitePropertyDef] = {
	// 	try {
	// 		val label:String = p._2
	// 		val propertyType:CitePropertyType = p._3
	// 		val urn:Cite2Urn = u
	// 		val vocabularyList:Vector[String] = Vector[String]()
	// 		val cpd:CitePropertyDef = new CitePropertyDef(urn,label,propertyType,vocabularyList)
	// 		Some(cpd)
	// 	} catch {
	// 		case e:Exception => {
	// 			None
	// 		}
	// 	}
	// }

      val requiredCiteCollections:String = s"""#!citecollections
|URN#Description#Labelling property#Ordering property#License
|urn:cite2:cite:datamodels.v1:#CITE data models#urn:cite2:cite:datamodels.v1.label:##Public domain
|urn:cite2:cite:verbs.v1:#Collection of verbal relations#urn:cite2:cite:verbs.v1.label:##Public Domain""".stripMargin

	val requiredCollData:String = s"""#!citeproperties
Property#Label#Type#Authority list
|urn:cite2:cite:verbs.v1.urn:#URN#Cite2Urn#
|urn:cite2:cite:verbs.v1.label:#label#String#
|urn:cite2:cite:verbs.v1.description:#description#String#

|#!citedata
|urn#label#description
|urn:cite2:cite:verbs.v1:commentsOn#comments on#subject[Urn] comments on object[Urn]
|urn:cite2:cite:verbs.v1:illustrates#illustrates#subject[Urn] comments on object[Urn]
|urn:cite2:cite:verbs.v1:hasOnIt#has on it#subject[Urn] comments on object[Urn]
|urn:cite2:cite:verbs.v1:aligns#aligns#subject[CiteUrn] is an alignment that includes passage[CtsUrn]

|#!citeproperties
|Property#Label#Type#Authority list
|urn:cite2:cite:datamodels.v1.urn:#Data model#Cite2Urn#
|urn:cite2:cite:datamodels.v1.label:#Label#String#
|urn:cite2:cite:datamodels.v1.description:#Description#String#

|#!citedata
|urn#label#description
|urn:cite2:cite:datamodels.v1:alignment#text alignment model#Aligning passages of OHCO2 texts. See <https://eumaeus.github.io/citealign/>.""".stripMargin

      val collectionsHeader:String = """#!citecollections
          |URN#Description#Labelling property#Ordering property#License""".stripMargin

      val citePropertiesHeader:String = """#!citeproperties
          |Property#Label#Type#Authority list""".stripMargin

	/* This is how to pass data to the global JS scope */
	/*
	js.Dynamic.global.currentObjectUrn = "urn:cts"
	js.Dynamic.global.roiArray = Array("one","two","three")
	*/

}
