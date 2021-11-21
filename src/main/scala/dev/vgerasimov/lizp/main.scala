package dev.vgerasimov.lizp

@main def run =

  val source =
    """|(val x () (println "x") 69)
       |(def lol () (val s "y") (val x 420) (println (s)) (x))
       |(def mult (a b)
       |  (def f (x acc)
       |    (if (>= (x) (a)) (acc) (f (+ 1 (x)) (+ (b) (acc)))))
       |  (f 0 0))
       |(println "hello")
       |(mult 10 (lol))
       |""".stripMargin

  Parser(source) match
    case ParsingError(message) => println(s"Parsing error:\n$message")
    case e: List[Expr] =>
      // println(e)
      val res = eval(new Context())(Map(), e.map(x => () => x)*)
      println("Output:")
      println(res)