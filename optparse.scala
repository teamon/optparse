package optparse

import shapeless._
import shapeless.HList._
import shapeless.Poly._
import shapeless.TypeOperators._

sealed trait Param[T] {
    def default: T
    def short: String
    def long: String
}
case class Opt[T](short: String, long: String, default: T) extends Param[T]
case class Flag(short: String, long: String) extends Param[Boolean] {
    def default = false
}

object OptParse {

    def opt[T](short: String, long: String, default: T): Param[T] = Opt(short, long, default)
    def flag(short: String, long: String): Param[Boolean] = Flag(short, long)

    type OptTyper[T] = String => Option[T]

    implicit def StringOptType: OptTyper[String] = s => Some(s)
    implicit def IntOptType: OptTyper[Int] = s => Some(s.toInt)
    implicit def FloatOptType: OptTyper[Float] = s => Some(s.toFloat)
    implicit def DoubleOptType: OptTyper[Double] = s => Some(s.toDouble)
    implicit def BooleanOptType: OptTyper[Boolean] = s => Some(false) // never really executed

    object value extends (Param ~> Id){
        def default[T](param: Param[T]) = param.default //checkValue(param)
    }

    implicit def valueString = value.λ[String](e => checkValue(e))
    implicit def valueInt = value.λ[Int](e => checkValue(e))
    implicit def valueFloat = value.λ[Float](e => checkValue(e))
    implicit def valueDouble = value.λ[Double](e => checkValue(e))
    implicit def valueBoolean = value.λ[Boolean](e => checkValue(e))

    val RxShort = "(-[^-]+)".r
    val RxLong = "(--.+)".r

    def splitArgs(list: List[String]) = list.flatMap {
        case RxShort(names) => names.drop(1) map ("-" + _)
        case e => List(e)
    }

    private var _opts: Map[String, Option[String]] = Map()

    def checkValue[T](param: Param[T])(implicit typer: OptTyper[T]): T = (param match {
        case Flag(short, long) =>
            _opts.contains(short) || _opts.contains(long)
        case Opt(short, long, default) =>
            (_values.get(param.short) orElse _values.get(param.long)).flatMap(typer) getOrElse default
    })

    private lazy val _values =  _opts.flatMap { case (key, opt) => opt map { o => Map(key -> o) } getOrElse Map() }

    def parse[L <: HList](list: L)(args: Array[String])(implicit m1: Mapper[value.type, L], l: ToList[L, Param[_]]) = {
        val byName = (Map[String, Param[_]]() /: list.toList){ case (map, d) =>
            map + (d.short -> d, d.long -> d)
        }

        val (opts, argv, _) = ((Map[String, Option[String]]().withDefaultValue(None), List[String](), None: Option[String]) /: splitArgs(args.toList)){
            case ((map, argv, last), arg) => (arg, last) match {
                case (RxShort(e), _) => (map + (e -> None), argv, Some(e))
                case (RxLong(e), _) => (map + (e -> None), argv, Some(e))
                case (a, Some(lst)) => byName get lst match {
                    case Some(Opt(_, _, _)) => (map + (lst -> Some(a)), argv, None)
                    case _ => (map, argv :+ a, None)
                }
                case (a, None) => (map, argv :+ a, None)
            }
        }

        _opts = opts
        list map value
    }
}

object Test {
    import OptParse._
    def main(args: Array[String]){
        val opts @ debug :: host :: port :: HNil = parse(
            flag("-d", "--debug") ::
            opt("-h", "--host", "localhost") ::
            opt("-p", "--port", 80) ::
            HNil
        )(args)

        println(opts)
    }
}

