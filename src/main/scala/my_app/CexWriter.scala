package reader

import com.thoughtworks.binding.{Binding, dom}
import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars}
import scala.scalajs.js
import scala.scalajs.js._
import org.scalajs.dom._
import org.scalajs.dom.ext._
import org.scalajs.dom.raw._
import edu.holycross.shot.cite._
import edu.holycross.shot.scm._
import edu.holycross.shot.ohco2._
import edu.holycross.shot.citeobj._
import edu.furman.classics.citealign._
import edu.furman.classics.citewriter._
import scala.scalajs.js.Dynamic.{ global => g }
import scala.concurrent._
//import ExecutionContext.Implicits.global

import scala.scalajs.js.annotation.JSExport
import js.annotation._
import monix.execution.Scheduler.Implicits.global
import monix.eval._

@JSExportTopLevel("ReaderCexWriter")
object ReaderCexWriter {

 def getCexUrn:Cite2Urn = {
  val d = new js.Date()
  val nss:String = "urn:cite2:cex:ducatauto."
  val yearMonth:String = s"${d.getFullYear()}${d.getMonth()}"
  val hours:String = s"${d.getHours()}_${d.getMinutes()}_${d.getSeconds()}_${d.getMilliseconds()}"
  val urnString:String = s"${nss}${yearMonth}:${hours}"
  Cite2Urn(urnString)
} 

def downloadCex:Unit = {
  
    val task = Task{  saveCex(SaveDialog.defaultFilename.value, assembleCex) }
    val future = task.runAsync
  
}   

def sortPassages(tr:TextRepository, passages:Vector[CitableNode]):Vector[CitableNode] = {
  val vecUrns:Vector[CtsUrn] = sortUrns(tr, passages.map(_.urn).distinct)
  vecUrns.map(u => {
    passages.find(_.urn == u).get
  })
}

def sortUrns(tr:TextRepository, passages:Vector[CtsUrn]):Vector[CtsUrn] = {
    val passageSet:Set[CtsUrn] = passages.toSet
    val trc = tr.corpus
    val pv:Vector[CtsUrn] = passageSet.toVector.map(u => trc.validReff(u).filter(_.dropPassage == u.dropPassage)).flatten
    val pm:Vector[(CtsUrn,Vector[CtsUrn])] = pv.groupBy(_.dropPassage).toVector
    val workVec:Vector[CtsUrn] = pm.map(work => {
      val thisWorkUrns:Vector[(CtsUrn, Int)] = trc.urns.filter(_.dropPassage == work._1).zipWithIndex
      val theseUrns:Vector[(CtsUrn, Int)] = work._2.map( wu => {
        var thisIndex:Int = thisWorkUrns.find(_._1 == wu).get._2
        (wu, thisIndex)
      })
      theseUrns.sortBy(_._2).map(_._1)
    }).flatten
    workVec
  }


def assembleCex:String = {
  val cexString:String = Vector(
    cexHeader,
    s"// URL for state at export:\n// ${O2Model.getUrlForCurrentState(O2Model.currentCorpus.value.toVector)}\n",
    getCtsCollection(O2Model.textRepo.value, SaveDialog.downloadCorpusOption.value),
    getCtsData(O2Model.textRepo.value, SaveDialog.downloadCorpusOption.value),
    DataModelModel.toCEX(DataModelModel.dataModels.value, SaveDialog.newAlignmentCollectionUrn.value),
    ObjectModel.toCEX(ObjectModel.collRep.value),
    RelationsModel.toCEX,
    Alignment.toCEX
  ).mkString("\n\n")
  cexString
}

def getCtsCollection(textRepo:Option[TextRepository] = None, corpOption:String = "all"):String = {
  // urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang
  textRepo match {
    case Some(tr) => {
        corpOption match {
          case "shown" => {
            getCtsCollShown(tr)
          }
          case "shownAll" => {
            getCtsCollShown(tr)
          }
          case _ => {
            getCtsCollAll(tr)
          }
        }
    } 
    case None => {
      ""
    }
  }
}

def getCtsCollAll(tr:TextRepository):String = {
  val entryVec:Vector[String] = {
    for (t <- tr.catalog.texts) yield {
      t.cex("#")
    }
  }     
  val entries:String = entryVec.mkString("\n")
  s"${ctsCatalogHeader}\n${entries}"
}

def getCtsCollShown(tr:TextRepository):String = {
  val versionVec:Vector[CtsUrn] = {
    O2Model.currentCorpus.value.toVector.map(_.versionUrn.value.dropPassage)
  }
  val texts:Vector[CatalogEntry] = tr.catalog.texts.filter( t => {
    versionVec.contains(t.urn)
  })
  val entryVec:Vector[String] = {
    for (t <- texts) yield {
      t.cex("#")
    }
  }
  val entries:String = entryVec.mkString("\n")
  s"${ctsCatalogHeader}\n${entries}"
}


def getCtsData(textRepo:Option[TextRepository] = None, corpOption:String = "all"):String = {
  textRepo match {
    case Some(tr) => {
      corpOption match {
        case "shownAll" => getCtsDataAllShown(tr)
        case "shown" => getCtsDataShown(tr)
        case _ => getCtsDataAll(tr)
      }        
    } 
    case None => ""
    }
}

def getCtsDataAll(tr:TextRepository):String = {
   val ctsData:Vector[String] = tr.catalog.texts.map(t => {
        val intro:String = s"""\n//${t}"""
        val header:String = "#!ctsdata"
        val nodes:Vector[String] = (tr.corpus ~~ t.urn).nodes.map(n => {
          s"${n.urn}#${n.text}"
        })
        Vector(intro,header) ++ nodes
    }).flatten
    ctsData.mkString("\n") 
}

def getCtsDataAllShown(tr:TextRepository):String = {
  val versionVec:Vector[CtsUrn] = {
    O2Model.currentCorpus.value.toVector.map(_.versionUrn.value.dropPassage)
  }
  val texts:Vector[CatalogEntry] = tr.catalog.texts.filter( t => {
    versionVec.contains(t.urn)
  })  
  val ctsData:Vector[String] = texts.map(t => {
        val intro:String = s"""\n//${t}"""
        val header:String = "#!ctsdata"
        val nodes:Vector[String] = tr.corpus.nodes.filter(_.urn.dropPassage == t.urn).map(n => {
            s"${n.urn}#${n.text}"
        })
        Vector(intro,header) ++ nodes
    }).flatten
    ctsData.mkString("\n")  
}

def getCtsDataShown(tr:TextRepository):String = {
  val cc =  O2Model.currentCorpus.value.toVector
  val vn = cc.map(_.versionNodes.value.toVector).flatten
  val vnb = vn.map(_.nodes.value.toVector).flatten
  val corp = Corpus(vnb)
  val header:String = "#!ctsdata"
  val sortedNodes:Vector[CitableNode] = corp.nodes
  val nodeStrings:Vector[String] = sortedNodes.map(n => {
      s"${n.urn}#${n.text}"
  })
  val ctsData = Vector(header) ++ nodeStrings
  ctsData.mkString("\n")
}

/* Methods for connecting out to Javascript */
@JSGlobal("saveCex")
@js.native
object saveCex extends js.Any {
  def apply(filename:String, data:String): js.Dynamic = js.native  
}


  /* *************************************
        Boilerplate
**************************************** */
     val cexHeader:String = s"""#!cexversion
        |3.0
        |
        |#!citelibrary
        |name#CITE Library generated by the Ducat application, ${new Date()}
        |urn#${getCexUrn}
        |license#CC Share Alike.
        """.stripMargin

      val ctsCatalogHeader:String = """#!ctscatalog
          |urn#citationScheme#groupName#workTitle#versionLabel#exemplarLabel#online#lang""".stripMargin



    } 