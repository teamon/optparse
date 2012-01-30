## Scala type safe command line options parser built using [shapeless](https://github.com/milessabin/shapeless)

### Usage

    import optparse.OptParse._

    def main(args: Array[String]){
        val debug :: host :: port :: HNil = parse(
            flag("-d", "--debug") ::
            opt("-h", "--host", "localhost") ::
            opt("-p", "--port", 80) ::
            HNil
        )(args)

        // debug: Boolean
        // host: String
        // port: Int
    }
