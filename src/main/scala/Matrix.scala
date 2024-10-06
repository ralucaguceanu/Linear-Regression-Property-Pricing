import scala.annotation.targetName

type Mat = List[List[Double]]

class Matrix(m: Option[List[List[Double]]]) {

  def transpose: Matrix =
    m match {
      case None => Matrix(None)
      case Some(rows) => Matrix(rows.map(_.head) :: rows.map(_.tail).transpose)
    }

  def map(f: Double => Double): Matrix =
    m match {
      case None => Matrix(None)
      case Some(rows) => Matrix(Some(rows.map(_.map(f))))
    }
    
  def *(other: Matrix): Matrix =
    val transposedOther = other.transpose
    (m, transposedOther.data) match {
      case (Some(rowsM), Some(rowsO)) =>
        if (width != transposedOther.width) {
          new Matrix(None)
        } else {
          val multipliedRows = rowsM.map(rowM =>
            rowsO.map(rowO =>
              rowM.zip(rowO).map((elemM, elemO) => elemM * elemO).foldLeft(0.0)(_ + _)
            )
          )
          
          new Matrix(Some(multipliedRows))
        }
      case _ => new Matrix(None)
    }

  def ++(x: Double): Matrix =
    m match {
      case None => Matrix(None)
      case Some(rows) => Matrix(Some(rows.map(row => row ::: List(x))))
    }

  def -(other: Matrix): Matrix =
    (m, other.data) match {
      case (Some(rowsM), Some(rowsO)) =>
        if (width != other.width || height != other.height) {
          new Matrix(None)
        } else {
          val subtractedRows = rowsM.zip(rowsO).map((rowM, rowO) =>
            rowM.zip(rowO).map((elemM, elemO) => elemM - elemO)
          )
          
          new Matrix(Some(subtractedRows))
        }
      case _ => new Matrix(None)
    }

  def data: Option[Mat] = m
  def height: Option[Int] =
    m match {
      case Some(rows) => Some(rows.length)
      case None => None
    }
  def width: Option[Int] =
    m match {
      case Some(rows) => Some(rows.head.length)
      case None => None
    }

  override def toString: String = m.foldLeft("")((acc, line) =>
    acc + (if (acc.isEmpty) "" else "\n") + line.foldLeft("")((currLine, value) =>
      currLine + (if (currLine.isEmpty) "" else ",") + value
    )
  )
}

object Matrix {
  def apply(data: Mat): Matrix = new Matrix(Some(data))
  def apply(data: Option[Mat]): Matrix = new Matrix(data)
  def apply(dataset: Dataset): Matrix = new Matrix(Some(dataset.data.tail.map(_.map(_.toDouble))))
}
