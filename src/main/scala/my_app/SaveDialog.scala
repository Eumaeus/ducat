package reader

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import edu.holycross.shot.cite._

import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom.raw._
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js.annotation.JSExport
import js.annotation._


@JSExportTopLevel("SaveDialog")
object SaveDialog {


	// name of CEX file
	val defaultFilename = Var("DucatDownload.cex")

	// for generating a collection of alignments, for the CEX
	val newAlignmentCollectionUrn:Var[Cite2Urn] = Var(Cite2Urn("urn:cite2:ducat:alignments.temp:"))

	val defaultEditorName = Var("DucatUser")

	// download corpus Options
	val downloadCorpusOption:Var[String] = Var("shown")
	// download asignment Options
	val downloadAlignmentOption:Var[String] = Var("shown")


	def updateCexSettings:Unit = {
		val userCorpOptions:String = {
			val validOpts:Vector[String] = Vector("shown","shownAll","all")
			val thisTarget = document.getElementById("corpusOptions").asInstanceOf[org.scalajs.dom.raw.HTMLSelectElement]
			val usr:String = thisTarget.value.toString
			if (validOpts.contains(usr)) usr else downloadCorpusOption.value
		}
		val userAlignmentOptions:String = {
			val validOpts:Vector[String] = Vector("shown","all")
			val thisTarget = document.getElementById("alignmentOptions").asInstanceOf[org.scalajs.dom.raw.HTMLSelectElement]
			val usr:String = thisTarget.value.toString
			if (validOpts.contains(usr)) usr else downloadAlignmentOption.value
		}
		val userUrn:Cite2Urn = {
			val thisTarget = document.getElementById("collectionUrnField").asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
			val usr:String = thisTarget.value.toString
			try {
				val u:Cite2Urn = Cite2Urn(usr)
				u
			} catch {
				case e:Exception => {
					newAlignmentCollectionUrn.value
				}
			}
		}
		val userFileName:String = {
			val thisTarget = document.getElementById("fileNameField").asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
			val usr:String = thisTarget.value.toString
			usr
		}

		val editorName:String = {
			val thisTarget = document.getElementById("editorNameField").asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
			val usr:String = thisTarget.value.toString
			usr
		}

		defaultFilename.value = userFileName
		defaultEditorName.value = editorName
		newAlignmentCollectionUrn.value = userUrn
		downloadCorpusOption.value = userCorpOptions
		downloadAlignmentOption.value = userAlignmentOptions
		MainModel.updateCookie("editorName",editorName)
		MainModel.updateCookie("defaultCexFn",userFileName)
		MainModel.updateCookie("defaultAlignmentUrn",userUrn.dropSelector.toString)
	}


	@dom
	def dialogWindow = {
			<div id="save_dialog" class={ MainModel.saveDialogVisibility.bind}  >
				<h1> Download CEX File </h1>
				<div id="saveConfigDiv">
					<form>
				{ corpusOptionsBlock.bind }		
				{ alignmentOptionsBlock.bind }
				{ namesBlock.bind }
				{ saveCancelButtons.bind }
					</form>
				</div>
			</div>
	}

	@dom
	def saveCancelButtons = {
		<span id="downloadCancelOkay">
			<button id="downloadOkay"
				type="button"
				disabled = { false }
				onclick = { event: Event => {
					updateCexSettings
					ReaderCexWriter.downloadCex
					MainModel.saveDialogVisibility.value = "app_hidden"
				}}
			>Download</button>
			<button id="downloadCancel"
				type="button"
				disabled = { false }
				onclick = { event: Event => {
					MainModel.saveDialogVisibility.value = "app_hidden"
				}}
			>Cancel</button>

		</span>
	}

	@dom
	def corpusOptionsBlock = {
		<div id="corpusOptionsBlock" class="saveDialogGroup">
			<label for="">Corpus Options:</label>
			<select id="corpusOptions">
				<option value="shown">Displayed passages only</option>
				<option value="shownAll">All passages for displayed texts</option>
				<option value="all">All passages of all texts</option>
			</select>
		</div>
	}

	@dom
	def alignmentOptionsBlock = {
		<div id="alignmentOptionsBlock" class="saveDialogGroup">
			<label for="">Alignment Options:</label>
			<select id="alignmentOptions">
				<option value="shown">Alignments only</option>
				<!--  <option value="all">All Objects &amp; Relations</option> -->
			</select>
		</div>
	}

	@dom
	def namesBlock = {
		<div id="namesBlock">
			<div class="saveDialogGroup">
				<span class="groupingSpan">
			<label for="collectionUrnField">Alignment Collection URN</label>
			<input id="collectionUrnField" type="text" size={30} value={ newAlignmentCollectionUrn.bind.toString }
				onchange={ event: Event => {
				validateCite2UrnEntry( event )
				}
			} />
			</span>
			</div>
			<div class="saveDialogGroup">
			<span class="groupingSpan">
			<label for="fileNameField">CEX File Name</label>
			<input id="fileNameField" type="text" size={30} value={ defaultFilename.bind } />
			</span>
			<span class="groupingSpan">
			<label for="editorNameField">Editor Name</label>
			<input id="editorNameField" type="text" size={30} value={ defaultEditorName.bind } />
			</span>
			</div>
		</div>
	}

	def doDownload = {
		ReaderCexWriter.downloadCex
	}

	def validateCite2UrnEntry(thisEvent: Event):Unit = {
		val thisTarget = thisEvent.target.asInstanceOf[org.scalajs.dom.raw.HTMLInputElement]
		val testText = thisTarget.value.toString
		try{
			val testCite2:Cite2Urn = Cite2Urn(testText)
			//currentCite2UrnQuery.value =  Some(testCite2)
		} catch {
			case e: Exception => {
				val badMo: String = testText
				thisTarget.value =  newAlignmentCollectionUrn.value.toString
				MainController.updateUserMessage(s"${badMo} is not a valid CITE2 URN.", 2)
			}
		}
	}

}
