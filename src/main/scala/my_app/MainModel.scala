package reader 
import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import com.thoughtworks.binding.Binding.{Var, Vars}
import com.thoughtworks.binding.dom
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import scala.concurrent
              .ExecutionContext
              .Implicits
              .global

import scala.scalajs.js
import scala.scalajs.js._
import js.annotation._
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._


@JSExportTopLevel("MainModel")
object MainModel {

		val userMessage = Var("Main loaded.")
		val userAlert = Var("default")
		val userMessageVisibility = Var("app_hidden")
		var msgTimer:scala.scalajs.js.timers.SetTimeoutHandle = null
		var waiting:Var[Boolean] = Var(false)

		val welcomeMessage = Var("")
		val requestParameterUrn = Var[Option[Vector[CtsUrn]]](None)

		val cexMainDelimiter:String = "#"
		val cexSecondaryDelimiter:String = ","

		val mainLibrary = Var[Option[CiteLibrary]](None)
		
		val showTexts = Var(true)
		val currentLibraryMetadataString = Var("No library loaded.")

		val saveDialogVisibility = Var("app_hidden")


	def readCookie: Map[String, String] = document.cookie
		.split(";")
		.toList
		.map(_.split("=").toList)
		.flatMap(x =>
			(x.headOption, x.drop(1).headOption) match {
				case (Some(k), Some(v)) => List((k.trim, v))
				case _                  => Nil
				})
		.toMap

	def readCookieValue(k:String):Option[String] = {
		val cm = readCookie
		if (cm.keys.toVector.contains(k)) {
			val r:Option[String] = Some(cm(k))
			r
		} else { 
			val r:Option[String] = None
			r
		}
	}

	def removeCookieField(k:String):Unit = {
		val cm = readCookie
		val nm = cm - k
		val d = new Date()
		writeCookie(nm)
	}

	def updateCookie(k:String, v:String):Unit = {
		val cm = readCookie
		val nm = cm ++ Map(k -> v)
		val d = new Date()
		writeCookie(nm)	
	}

	def writeCookie(values:Map[String, String]): Unit = {
		val d = new Date()
		//val nextYear = d.setYear(d.getYear() + 1)
		val expiry = new Date(d.getFullYear() + 1, d.getMonth())

		values.toList.foreach {
			case (k, v) => val expires = expiry.toUTCString
			document.cookie = s"$k=$v;expires=$expires;path=/"
		}
	}


}
