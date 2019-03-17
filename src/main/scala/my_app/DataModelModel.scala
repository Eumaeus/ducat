package reader 

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import js.annotation._
import scala.concurrent._
//import ExecutionContext.Implicits.global
import collection.mutable
import collection.mutable._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.holycross.shot.scm._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("DataModelModel")
object DataModelModel {

	var implementedDataModels:Vector[Cite2Urn] = Vector(
		// Defines a basic citable image model
		//Cite2Urn("urn:cite2:cite:datamodels.v1:imagemodel"),
		// Defines an image that can be delivered as binary image data
		//Cite2Urn("urn:cite2:cite:datamodels.v1:binaryimg"),
		// Defines a "codex model" of ordered text-bearing surfaces
		//Cite2Urn("urn:cite2:cite:datamodels.v1:tbsmodel"),
		// Defines a "documented scholarly editions" model of surface + image + text
		//Cite2Urn("urn:cite2:cite:datamodels.v1:dse"),
		// Defines a CITE Collection property of type `text` more precisely
		Cite2Urn("urn:cite2:cite:datamodels.v1:extensions_text"),
		// Aligments between text passages 
		Cite2Urn("urn:cite2:cite:datamodels.v1:alignment")
	)

	val dataModels = Var[Option[Vector[DataModel]]](None)

	def clearDataModels:Unit = {
		DataModelModel.dataModels.value = None
	}

	def toCEX(dms:Option[Vector[DataModel]], newColl:Cite2Urn):String = {
		dms match {
			case Some(dmv) => {
				//See if our default collection URN is already there 
				val collUrn:Cite2Urn = SaveDialog.newAlignmentCollectionUrn.value
				val dmPresent:Boolean = {
					dmv.filter(_.collection == collUrn).size > 0
				}
				val newDmv:Vector[DataModel] = {
					if (dmPresent) dmv
					else {
						val coll:Cite2Urn = collUrn
						val model:Cite2Urn = Cite2Urn("urn:cite2:cite:datamodels.v1:alignment")
						val label:String = "Text Alignment Model"
						val description:String = "The CITE model for text alignment. See documentation at <https://eumaeus.github.io/citealign/>."
						dmv ++ Vector(DataModel(coll, model, label, description))
					}
				}
				val dmStringVec:Vector[String] = {
					newDmv.map(d => {
						s"${d.collection}#${d.model}#${d.label}#${d.description}"
					})
				}
				dataModelsHeader + "\n" + dmStringVec.mkString("\n") 
			}
		case None => { 
			val oneLine:String = s"""${SaveDialog.newAlignmentCollectionUrn.value}#urn:cite2:cite:datamodels.v1:alignment#Text Alignment Model#The CITE model for text alignment. See documentation at <https://eumaeus.github.io/citealign/>."""
			dataModelsHeader + "\n" + oneLine 
}
		} 
	}

      val dataModelsHeader:String = s"""#!datamodels
          |Collection#Model#Label#Description""".stripMargin
}
