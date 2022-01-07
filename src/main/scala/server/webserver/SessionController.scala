package server.webserver

import server.config.ConfigFile


object SessionController :
  val serverConfig=new ConfigFile("server.ini")
  def main(args:Array[String]): Unit =
    MyWebServer.start()
