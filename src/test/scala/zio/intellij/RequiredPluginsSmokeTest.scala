package zio.intellij

import com.intellij.ide.plugins.PluginManagerCore
import com.intellij.testFramework.{TestApplicationManager, UsefulTestCase}
import org.junit.Assert.fail

import scala.jdk.CollectionConverters._

/**
 *  Fails fast, with an actionable message, when a plugin that the whole test suite depends on didn't load.
 */
class RequiredPluginsSmokeTest extends UsefulTestCase {

  private val requiredPlugins: Seq[(String, String)] = Seq(
    "com.intellij.java"         -> "Java",
    "org.intellij.scala"        -> "Scala",
    "zio.intellij.zio-intellij" -> "ZIO for IntelliJ"
  )

  def testRequiredPluginsAreLoaded(): Unit = {
    // Force the test application (and thus plugin-set resolution) to initialize without needing a project or JDK.
    TestApplicationManager.getInstance()

    val loadedIds = PluginManagerCore.getLoadedPlugins.asScala.map(_.getPluginId.getIdString).toSet
    val missing   = requiredPlugins.collect { case (id, name) if !loadedIds.contains(id) => name }

    if (missing.nonEmpty) {
      fail(
        s"""Required plugins failed to load: ${missing.mkString(", ")}.
           |
           |This might mean something is missing in project/dependencies.scala. If a required module is absent, the
           |Java -> Scala -> ZIO plugin chain is excluded and other tests fail obscurely inside setUp.
           |
           |To find the culprit, look at the test IDE log ("Test log file: ..." printed at the start of the
           |run, or ~/.<name>PluginIU/test-system/log/idea.log): the "Plugin set resolution" and
           |"Problems found loading plugins" blocks name the exact missing module. Map that module to the
           |bundled plugin that provides it via <sdk>/product-info.json, then add the plugin id here.
           |""".stripMargin
      )
    }
  }
}
