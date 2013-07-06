package scas

import java.awt.{Component, Graphics, Color}
import java.awt.event.{MouseAdapter, MouseEvent, MouseWheelEvent}

class Graph(f: Double => Double) extends Component {
  setSize(300, 300)
  val w = getWidth()
  val h = getHeight()
  var z = 1.0
  var x0 = 0.0
  var y0 = 0.0
  var x1 = 0
  var y1 = 0
  val madapter = new MouseAdapter {
    override def mouseDragged(e: MouseEvent) {
      x0 += (e.getX() - x1) / w.toDouble * 2.0 / z
      y0 += (e.getY() - y1) / h.toDouble * 2.0 / z
      repaint();
      x1 = e.getX()
      y1 = e.getY()
    }
    override def mouseMoved(e: MouseEvent) {
      x1 = e.getX()
      y1 = e.getY()
    }
    override def mouseWheelMoved(e: MouseWheelEvent) {
      z /= scala.math.pow(2.0, e.getWheelRotation())
      repaint();
    }
  }
  addMouseMotionListener(madapter)
  addMouseWheelListener(madapter)
  override def paint(g: Graphics) {
    val x = for (n <- 0 to w) yield n
    val y = for (n <- 0 to w) yield {
      val x = ((n.toDouble / w.toDouble) * 2.0 - 1.0) / z - x0
      val y = f(x)
      ((1.0 + (y0 - y) * z) / 2.0 * h.toDouble).toInt
    }
    val x2 = ((1.0 + x0 * z) / 2.0 * w.toDouble).toInt
    val y2 = ((1.0 + y0 * z) / 2.0 * h.toDouble).toInt
    val color = g.getColor()
    g.setColor(Color.RED)
    g.drawLine(0, y2, w, y2)
    g.drawLine(x2, 0, x2, h)
    g.setColor(color)
    g.drawPolyline(x.toArray, y.toArray, w)
  }
}

object Graph {
  def apply(f: Double => Double) = new Graph(f)
}
