package scas

import java.io.{Reader, StringReader, StringWriter}
import javax.xml.transform.{Transformer, TransformerFactory}
import javax.xml.transform.stream.{StreamSource, StreamResult}
import jscl.converter.Converter

class MathML(stylesheet: String) {
  def apply(reader: Reader): String = {
    val writer = new StringWriter
    var c = reader.read
    while(c != -1) {
      writer.write(c)
      c = reader.read
    }
    reader.close
    apply(writer.toString)
  }
  def apply(document: String): String = {
    val s = Converter.convert(document)
    val r = new StringReader(s)
    val w = new StringWriter
    transformer.transform(new StreamSource(r), new StreamResult(w))
    w.toString.replaceAll("\r", "").replaceAll("\u00a0{8}", "\t").replaceAll("\u00a0", " ").replaceAll(" +\n", "\n").trim
  }
  def apply(obj: Object): String = obj match {
    case null => null
    case obj: MathObject => apply(<math>{obj.toMathML}</math>)
    case obj => apply(obj.toString)
  }
  val transformer = TransformerFactory.newInstance.newTransformer(new StreamSource(Thread.currentThread.getContextClassLoader.getResource(stylesheet).toString))
}

object MathML {
  def apply(stylesheet: String) = new MathML(stylesheet)
}
