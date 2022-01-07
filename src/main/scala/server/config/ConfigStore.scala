package server.config

import scala.collection.mutable
import scala.io.Source

trait ConfigStore :
  def getInt(category:String,name:String,default:Int=0):Int
  def getString(category:String,name:String,default:String=""):String

  def setInt(category:String,name:String,value:Int):Unit
  def setString(category:String,name:String,value:String):Unit



class Category(val name:String):
  val intValues: collection.mutable.Map[String, Int] =collection.mutable.TreeMap[String,Int]()
  val stringValues: collection.mutable.Map[String, String] =collection.mutable.TreeMap[String,String]()
  override def toString: String ="Category "+name+"\nInt values:\n"+intValues.mkString("\n")+
    "\nStringValues:\n"+stringValues.mkString("\n")


class ConfigFile(fileName:String) extends ConfigStore :
  val categories: mutable.Map[String, Category] =collection.mutable.TreeMap[String,Category]()

  readFile(fileName)
  
  def readFile(fn:String):Unit=
    var currentCategory:Option[Category]=None
    val source=Source.fromFile(fileName)
    try
      for(l<-source.getLines();line: String =l.trim;if line.length>3)
        if line.charAt(0)=='['&& line.charAt(line.length-1)==']' then
          val catName=line.substring(1,line.length-1)
          currentCategory=Some(Category(catName))
          categories(catName)=currentCategory.get
        else line.split('=') match {
          case Array(fieldName: String,fieldValue: String) if fileName.trim.nonEmpty =>
            currentCategory match {
              case Some(category)=>
                fieldValue.trim.toIntOption match {
                  case Some(intValue)=> category.intValues(fieldName.trim)=intValue
                  case None=> category.stringValues(fieldName.trim)=fieldValue.trim
                }
              case None => println(" no Category for line:"+line)
            }

          case _ => println("wrong line:"+line)
        }
    finally
      source.close()


  override def getInt(categoryName: String, name: String, default: Int): Int =
    if categories.contains(categoryName) then categories(categoryName).intValues.getOrElse(name,default)
    else default
     

  override def getString(categoryName: String, name: String, default: String): String =
    if categories.contains(categoryName) then categories(categoryName).stringValues.getOrElse(name,default)
    else default

  override def setInt(categoryName: String, name: String, value: Int): Unit =
    categories.getOrElseUpdate(categoryName,new Category(name)).intValues(name)=value
    

  override def setString(categoryName: String, name: String, value: String): Unit =
    categories.getOrElseUpdate(categoryName,new Category(name)).stringValues(name)=value



