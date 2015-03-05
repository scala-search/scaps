//package scala.tools.apiSearch.featureExtraction
//
//import scala.reflect.internal.util.SourceFile
//import scala.tools.nsc.interactive.Global
//import scala.tools.nsc.doc.Settings
//import scala.tools.nsc.reporters.ConsoleReporter
//import scala.tools.nsc.doc.model.Package
//import scala.tools.nsc.doc.model.DocTemplateEntity
//import scala.tools.nsc.doc.model.TemplateEntity
//import scala.tools.nsc.doc.model.MemberEntity
//import scala.tools.nsc.doc.base.comment.Comment
//import scala.tools.nsc.doc.model.Val
//
//class ScalaSourceExtractorScalaDoc() {
//  def apply(source: String): List[Entity] = {
//    val universe = factory.makeUniverse(Right(source)) getOrElse {
//      throw new Exception("No universe found")
//    }
//
//    collectTemplates(universe.rootPackage).collect {
//      case e: DocTemplateEntity => e.members
//    }.flatten.map(toEntity _)
//  }
//
//  def toEntity(me: MemberEntity) = {
//    val comment = me.comment.fold("")(_.body.blocks.mkString(""))
//    val tpe = me match {
//      case v: Val => Type(v.resultType.name)
//      case _      => Type("?")
//    }
//    Entity(me.definitionName, tpe, comment)
//  }
//
//  def commentToString(c: Comment) = ""
//
//  def collectTemplates(rootPkg: Package): List[TemplateEntity] = {
//    def allPkgs(p: Package): List[Package] =
//      p :: p.packages.flatMap(allPkgs(_))
//
//    allPkgs(rootPkg).flatMap(_.templates)
//  }
//
//  val factory = {
//    def classPath(className: String) =
//      Class.forName(className).getProtectionDomain.getCodeSource.getLocation.toExternalForm()
//    val scalaLibraryPath = classPath("scala.Unit")
//
//    val settings = new Settings(_ => ())
//    settings.classpath.append(scalaLibraryPath)
//    settings.bootclasspath.append(scalaLibraryPath)
//    val reporter = new ConsoleReporter(settings) {
//      override def hasErrors = false
//    }
//    new scala.tools.nsc.doc.DocFactory(reporter, settings)
//  }
//}