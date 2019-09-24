package decaf.driver

/**
  * Entry of the compiler.
  */
object Launcher {

  /**
    * Launch the compiler with command line args.
    *
    * @param args command line args and options
    */
  def withArgs(args: Array[String]): Unit = {
    OptParser.parse(args, new Config).foreach { withConfig(_) }
  }

  /**
    * Launch the compiler with configuration.
    *
    * @param config compiler configuration
    */
  def withConfig(implicit config: Config): Unit = {
    val tasks = new Tasks
    val task = config.target match {
      case Config.Target.PA1 => tasks.parse
      case Config.Target.PA2 => tasks.typeCheck
      case Config.Target.PA3 => tasks.tac
      case Config.Target.PA3_JVM => tasks.jvm
      case Config.Target.PA4 => tasks.optimize
      case Config.Target.PA5 => tasks.mips
    }
    task(config.sourceStream)
  }
}
