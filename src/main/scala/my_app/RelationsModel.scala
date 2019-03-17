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
import edu.holycross.shot.citerelation._
import edu.holycross.shot.scm._

import monix.execution.Scheduler.Implicits.global
import monix.eval._

import scala.scalajs.js.annotation.JSExport

@JSExportTopLevel("RelationsModel")
object RelationsModel {
	val citeRelations = Var[Option[CiteRelationSet]](None)
	def clearRelations = { citeRelations.value = None }

	val cexHeader = "#!relations"

	def toCEX:String = {
		citeRelations.value match {
			case Some(cr) => {
				val relStrVec:Vector[String] = cr.relations.map( r => {
					s"${r.urn1}#${r.relation}#${r.urn2}"
				}).toVector
				(Vector(cexHeader) ++ relStrVec).mkString("\n")
			}
			case None => ""
		}

	}
}