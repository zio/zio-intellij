package zio.intellij.utils

object VersionTestUtils {

  // source: https://repo1.maven.org/maven2/dev/zio/zio_2.11/maven-metadata.xml
  val zioVersionsFromMaven: Array[String] =
    """<version>1.0.0-RC6</version>
      |<version>1.0.0-RC8</version>
      |<version>1.0.0-RC8-4</version>
      |<version>1.0.0-RC8-7</version>
      |<version>1.0.0-RC8-8</version>
      |<version>1.0.0-RC8-9</version>
      |<version>1.0.0-RC8-10</version>
      |<version>1.0.0-RC8-11</version>
      |<version>1.0.0-RC8-12</version>
      |<version>1.0.0-RC9</version>
      |<version>1.0.0-RC9-4</version>
      |<version>1.0.0-RC10-1</version>
      |<version>1.0.0-RC11</version>
      |<version>1.0.0-RC11-1</version>
      |<version>1.0.0-RC12</version>
      |<version>1.0.0-RC12-1</version>
      |<version>1.0.0-RC13</version>
      |<version>1.0.0-RC14</version>
      |<version>1.0.0-RC15</version>
      |<version>1.0.0-RC16</version>
      |<version>1.0.0-RC17</version>
      |<version>1.0.0-RC18</version>
      |<version>1.0.0-RC18-1</version>
      |<version>1.0.0-RC18-2</version>
      |<version>1.0.0-RC19</version>
      |<version>1.0.0-RC19-1</version>
      |<version>1.0.0-RC19-2</version>
      |<version>1.0.0-RC20</version>
      |<version>1.0.0-RC21</version>
      |<version>1.0.0-RC21-1</version>
      |<version>1.0.0-RC21-2</version>
      |<version>1.0.0</version>
      |<version>1.0.1</version>
      |<version>1.0.2</version>
      |<version>1.0.3</version>
      |<version>1.0.4</version>
      |<version>1.0.4-1</version>
      |<version>1.0.4-2</version>""".stripMargin
      .replace("<version>", "")
      .replace("</version>", "")
      .split(System.lineSeparator())
}
