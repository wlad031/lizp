package dev.vgerasimov.lizp

@main def run =

  val source =
    """|(def mult (a b)
       |  (def f (x acc)
       |    (println "heh")
       |    (if (>= (x) (a)) (acc) (f (+ 1 (x)) (+ (b) (acc)))))
       |  (f 0 0))
       |(println "hello")
       |(mult 5 6)
       |""".stripMargin

  Parser(source) match
    case ParsingError(message) => println(s"Parsing error:\n$message")
    case e: List[Expr] =>
      val res = eval(new Context())(Map(), e.map(x => () => x)*)
      println("Output:")
      println(res)
