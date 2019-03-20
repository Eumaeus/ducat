package reader

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import scala.scalajs.js.Dynamic.{ global => g }
import org.scalajs.dom.raw._
import org.scalajs.dom.document
import org.scalajs.dom.raw.Event
import org.scalajs.dom.ext.Ajax
import scala.scalajs.js.annotation.JSExport
import js.annotation._


@JSExportTopLevel("MainView")
object MainView {
	val textView = O2View.o2div
	val saveDialog = SaveDialog.dialogWindow

	@dom
	def mainMessageDiv = {
			<div id="main_message" class={ s"app_message ${MainModel.userMessageVisibility.bind} ${MainModel.userAlert.bind}" } >
				<p> { MainModel.userMessage.bind }  </p>
			</div>
	}

	/*
	@dom
	def cookieConsent = {
		<div id="cookieConsent">
		    <div id="closeCookieConsent">x</div>
		    This webpage is using cookies. <a href="https://github.com/cite-architecture/DUCAT-Daughter-of-Ugarit-Citation-Alignment-Tool" target="_blank">More info</a>. <a class="cookieConsentOK">That's Fine</a>
		</div>
	}
	*/

	@dom
	def mainDiv = {
		<div id="main-wrapper" class={ if (MainModel.waiting.bind) "waiting" else " "}>
		<header>
			<a href="https://www.dictionary.com/browse/ducat" target="_blank">Ducat</a> 
			<span id="app_header_versionInfo">“Daughter-of-<a href="">Ugarit</a> Citation Alignment Tool.” Version { BuildInfo.version }</span>
			{ filePicker.bind }
		</header>

		<article id="main_Container">

			{ mainMessageDiv.bind }
			{ textView.bind }
			{ saveDialog.bind }
			

		</article>
		 <div class="push"></div>
		<footer>
		{ footer.bind }
		</footer>
	</div>
	}


	@dom
	def footer = {
		<p>
		CITE/CTS is ©2002–2019 Neel Smith and Christopher Blackwell. Licensed under the <a href="https://www.gnu.org/licenses/gpl-3.0.en.html">GPL 3.0</a>. Sourcecode on <a href="https://github.com/cite-architecture/CITE-Text-Reader2">GitHub</a>.
		</p>
	}

	@dom
	def filePicker = {
		<span id="app_filePickerSpan">
			<label for="app_filePicker">Choose a local <code>.cex</code> file</label>
			<input
				id="app_filePicker"
				type="file"
				onchange={ event: Event => MainController.loadLocalLibrary( event )}
				></input>
		</span>
	}

}
