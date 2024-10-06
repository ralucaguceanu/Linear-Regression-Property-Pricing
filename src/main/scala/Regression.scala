import scala.annotation.tailrec

object Regression {

  def regression(dataset_file: String,
                attribute_columns: List[String],
                value_column: String,
                test_percentage: Double,
                alpha: Double,
                gradient_descent_steps: Int): (Matrix, Double) = {

    val dataset = Dataset(dataset_file)
    val (trainSet: Dataset, evaluateSet: Dataset) = dataset.split(test_percentage)

    val attributeColumnsTrain = trainSet.selectColumns(attribute_columns)
    val attributeColumnsEval = evaluateSet.selectColumns(attribute_columns)

    val X = Matrix(attributeColumnsTrain) ++ 1.0
    val evalX = Matrix(attributeColumnsEval) ++ 1.0

    X.width match {
      case Some(w) =>
        val W: Matrix = Matrix(List.fill(w)(List(0.0)))

        @tailrec
        def regressionHelper(W: Matrix, steps: Int): Matrix = {
          if (steps < 1) return W
          val estimations = X * W
          val Y = Matrix(trainSet.selectColumn(value_column))
          val error = estimations - Y
          val gradient = X.transpose * error

          X.height match {
            case Some(h) =>
              val scaledGradient = gradient.map(x => x / h)
              val updatedW = W - scaledGradient.map(x => x * alpha)
              regressionHelper(updatedW, steps - 1)
            case None => Matrix(None)
          }
        }

        val resultW = regressionHelper(W, gradient_descent_steps)

        evalX.width match {
          case Some(w) =>
            val evalW: Matrix = Matrix(List.fill(w)(List(0.0)))

            val evalEstimations = evalX * resultW
            val evalY = Matrix(evaluateSet.selectColumn(value_column))

            val subtractedValues = evalEstimations - evalY

            val totalError: Double = subtractedValues.data match {
              case None => 0.0
              case Some(mat) => mat.map(row => row.foldLeft(0.0)(_ + _)).foldLeft(0.0)(_ + _)
            }

            val meanError = evalY.height match {
              case Some(h) => totalError / h
              case None => totalError
            }

            (resultW, meanError)

          case None => (Matrix(None), 0.0)
        }
      case None => (Matrix(None), 0.0)
    }
  }


  def main(args: Array[String]): Unit = {
    // Exemplu de utilizare
    print(regression("datasets/houseds.csv", List("GrLivArea", "YearBuilt"), "SalePrice", 0.1, 1e-7, 10000))
  }
}

