package scas.application

import scala.beans.BeanProperty
import javax.script.{AbstractScriptEngine, Bindings, ScriptContext, ScriptEngine, ScriptEngineFactory, ScriptException, SimpleBindings}
import java.io.{StringWriter, Reader}
import java.util.{Arrays, List}

class Engine(@BeanProperty val factory: ScriptEngineFactory) extends AbstractScriptEngine(new SimpleBindings) {
  def createBindings: Bindings = new SimpleBindings

  var code = ""

  @throws(classOf[ScriptException])
  def eval(script: String, context: ScriptContext): Object = {
    val cat = code + script
    Parsers(cat) match {
      case Right(result) => {
        code = ""
        result
      }
      case Left(msg) if (msg.endsWith("end of source found")) => {
        code = cat + "\n"
        null
      }
      case Left(msg) => throw new ScriptException(msg)
    }
  }

  @throws(classOf[ScriptException])
  def eval(reader: Reader, context: ScriptContext): Object = {
    val writer = new StringWriter()
    var c = reader.read()
    while(c != -1) {
      writer.write(c)
      c = reader.read()
    }
    reader.close()
    eval(writer.toString(), context)
  }
}

object Engine {
  class Factory extends ScriptEngineFactory {
    @BeanProperty
    val engineName = "ScAS Interface"

    @BeanProperty
    val engineVersion = "1.0"

    @BeanProperty
    val extensions: List[String] = Arrays.asList("txt", "TXT")

    @BeanProperty
    val languageName = "ScAS"

    @BeanProperty
    val languageVersion = "2.1"

    def getMethodCallSyntax(obj: String, m: String, args: String*): String = null

    @BeanProperty
    val mimeTypes: List[String] = Arrays.asList("text/plain")

    @BeanProperty
    val names: List[String] = Arrays.asList("scas")

    def getOutputStatement(toDisplay: String): String = null

    def getParameter(key: String): Object = key match {
      case ScriptEngine.ENGINE => engineName
      case ScriptEngine.ENGINE_VERSION => engineVersion
      case ScriptEngine.LANGUAGE => languageName
      case ScriptEngine.LANGUAGE_VERSION => languageVersion
      case ScriptEngine.NAME => names.get(0)
      case _ => null
    }

    def getProgram(statements: String*): String = null

    def getScriptEngine: ScriptEngine = new Engine(this)
  }
}
