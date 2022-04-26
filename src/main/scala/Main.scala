import InputTypes.Lines

object InputTypes {
  type Lines = List[String]

  def Lines(xs: String*): Lines = List(xs: _*)

  type InputData = List[Lines]

  def InputData(xs: List[Lines]): InputData = xs
}

object FileReader {
  val Pattern = "(\\d+)\\s+(\\d+)".r

  def read_n_lines(iter: Iterator[String], n: Int): List[String] =
    n match {
      case 0 => Nil
      case other => iter.next() :: read_n_lines(iter, other - 1)
    }

  def read_n_lines2(iter: Iterator[String], n: Int): List[String] = {
    (1 to n).map(_ => iter.next()).toList
  }

  def read_number_of_data_sets(iter: Iterator[String]): Int = {
    iter.next().toInt
  }

  def parse_data_set(iter: Iterator[String]): Option[Lines] =
    Pattern.findFirstMatchIn(iter.next())
      .map(matcher => matcher.group(1))
      .map(_.toInt)
      .map(read_n_lines2(iter, _))

  def read_task_file(): InputTypes.InputData = {
    val filename = "input.txt"
    val source = io.Source.fromInputStream(getClass.getResourceAsStream("/" + filename))
    try {
      val iter = source.getLines()
      val numberOfInputs = read_number_of_data_sets(iter)
      val r = (1 to numberOfInputs)
        .map(_ => parse_data_set(iter))
        .flatten.toList
      InputTypes.InputData(r)
    } finally {
      source.close()
    }
  }
}

object Main extends App {
  private val inputData: InputTypes.InputData = FileReader.read_task_file()
  println("inputData", inputData)
}