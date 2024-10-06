import scala.io.Source

class Dataset(m: List[List[String]]) {
  val data: List[List[String]] = m
  override def toString: String = m.foldLeft("")((acc, line) =>
    acc + (if (acc.isEmpty) "" else "\n") + line.foldLeft("")((currLine, value) =>
      currLine + (if (currLine.isEmpty) "" else ",") + value
    )
  )

  def selectColumn(col: String): Dataset = {
    val columnIndex = getHeader.indexOf(col)
    val selectedColumn = m.map(row => row.drop(columnIndex).take(1))

    Dataset(selectedColumn)
  }

  def selectColumns(cols: List[String]): Dataset = {
    val selectedColumns = cols.foldLeft(List.fill(m.length)(List[String]()))((acc, currCol) =>
      val columnIndex = getHeader.indexOf(currCol)
      val selectedColumn = m.map(row => row.drop(columnIndex).take(1))
      acc.zip(selectedColumn).map(p => p._1 ::: p._2)
    )

    Dataset(selectedColumns)
  }

  def split(percentage: Double): (Dataset, Dataset) = {
    val sortedData: List[List[String]] = data.tail.sortBy(row => row.head)

    val (evalSet, count, testList) = sortedData.foldLeft((List[List[String]](), 0, List[List[String]]())) {
      case ((evalAcc, count, testAcc), row) =>
        if (count < (1 / percentage - 1)) {
          (row :: evalAcc, count + 1, testAcc)
        } else {
          (evalAcc, 0, row :: testAcc)
        }
    }

    val t = m.head :: testList.reverse
    val e = m.head :: evalSet.reverse

    (Dataset(e), Dataset(t))
  }

  def size: Int = m.length - 1
  def getRows: List[List[String]] = m.tail
  def getHeader: List[String] = m.head
}

object Dataset {
  def apply(csv_filename: String): Dataset = Dataset(Source.fromFile(csv_filename)
                                                .getLines()
                                                .map(_.split(",").toList)
                                                .toList)

  def apply(ds: List[List[String]]): Dataset = new Dataset(ds)
}
