package server.webserver

import definition.data.{EMPTY_REFERENCE, Reference}
import io.undertow.security.idm.*
import io.undertow.security.impl.DigestAuthenticationMechanism
import io.undertow.util.HexConverter
import server.webserver.SessionController.serverConfig
import util.StrToInt

import java.io.*
import java.nio.charset.StandardCharsets
import java.security.{MessageDigest, Principal}
import java.util
import java.util.Base64
import javax.crypto.spec.{IvParameterSpec, PBEKeySpec, SecretKeySpec}
import javax.crypto.{Cipher, SecretKeyFactory}
import scala.collection.mutable.ArrayBuffer
import scala.io.Source
import scala.util.Using
import scala.util.control.NonFatal

case class UserInfo(name:String,id:Int,password:String,role:Char,rootRef:Reference) extends Account with Principal:
  override def getPrincipal: Principal = this
  override def getRoles: util.Set[String] =java.util.Set.of(role.toString)
  override def getName: String = name

  def fileString: String =name.trim+"|"+id+"|"+UserStore.encrypt(password)+"|"+role+"|"+rootRef.sToString

object UserInfo:
  def unapply(str:String): Option[UserInfo] = str.split('|') match {
    case Array(nname:String,StrToInt(nid),npw,nr,Reference(ref))=> Some( UserInfo(nname,nid,UserStore.decrypt(npw),nr.charAt(0),ref))
    case _ => None
  }




object UserStore extends IdentityManager {
  private val salt="920#%3sn2394rna".getBytes()
  private val pw2="slaölmvlöASA2#"
  private val pw1="FÜG92093#,n.11"
  val keySpec: SecretKeySpec ={
    val keyFactory=SecretKeyFactory.getInstance("PBKDF2WithHmacSHA512")
    val spec=new PBEKeySpec((pw2+pw1).toCharArray,salt,30000,128)
    new SecretKeySpec(keyFactory.generateSecret(spec).getEncoded,"AES")
  }
  val userList: collection.mutable.ArrayBuffer[UserInfo] =ArrayBuffer[UserInfo]()
  readUserList()

  def baseEncode(bytes: Array[Byte]): String = Base64.getEncoder.encodeToString(bytes)
  def base64Decode(property: String): Array[Byte] = Base64.getDecoder.decode(property)
  def removeSpecialChars(st:String): String = st.replaceAll("[^a-zA-Z0-9_-]", "")

  private def createCipher()=Cipher.getInstance("AES/CBC/PKCS5Padding")

  def encrypt(property:String):String=
    val cipher=createCipher()
    cipher.init(Cipher.ENCRYPT_MODE,keySpec)
    val alParams=cipher.getParameters
    val ivParams=alParams.getParameterSpec(classOf[IvParameterSpec])
    val cryptoText=cipher.doFinal(property.getBytes("UTF-8"))
    val iv=ivParams.getIV
    baseEncode(iv)+":"+baseEncode(cryptoText)


  def decrypt(string:String):String=
    string.split(':') match {
      case Array(iv,property)=>
        val cipher=createCipher()
        cipher.init(Cipher.DECRYPT_MODE,keySpec,new IvParameterSpec(base64Decode(iv)))
        new String(cipher.doFinal(base64Decode(property)),"UTF-8")
      case _=> throw new IllegalArgumentException("Wrong encrypted string:"+string)
    }

  def readUserList():Unit=
    userList.clear()
    Using(Source.fromFile(serverConfig.getString("Paths", "userListFile"))){ reader=>
      for l<-reader.getLines() do
        l match {
          case UserInfo(uinfo)=>userList += uinfo
          case _ =>
        }
    }
    println("UserList \n  "+userList.mkString("\n  "))

  def storeUserList():Unit=
    Using(new PrintWriter(new BufferedWriter(new FileWriter(serverConfig.getString("Paths", "userListFile"))))){writer=>
      for(u<-userList)
        println(u.fileString)
        writer.println(u.fileString)
    }

  def main(args:Array[String]):Unit=
    readUserList()
    args match {
      case Array("add",userName:String,pw:String,role:String)=>
        val newID=if(userList.isEmpty)1 else userList.last.id+1
        val newUser=UserInfo(removeSpecialChars(userName.toLowerCase()),newID,pw,role.charAt(0),EMPTY_REFERENCE)
        userList+=newUser
        println("UserList \n  "+userList.mkString("\n  "))
        storeUserList()
      case Array("changePW",userName:String,pw:String)=>
        userList.indexWhere(_.name==userName.toLowerCase()) match
          case -1 => println("User "+userName+" unknown")
          case ix =>
            val oldData=userList(ix)
            userList(ix)=oldData.copy(password = pw)
            storeUserList()
    }

  override def verify(account: Account): Account =
    if userList.exists {
      _.name == account.getPrincipal.getName
    } then account else null

  override def verify(id: String, credential: Credential): Account =
    userList.find(_.name==id) match {
      case Some(user)=>
        credential match {
          case pc:PasswordCredential=>
            if(user.password==new String(pc.getPassword)) user else null
          case d:DigestCredential=>
            val digest=MessageDigest.getInstance(d.getAlgorithm.name())
            val dstring = user.name + ":" + d.getRealm + ":" + user.password
            digest.update(dstring.getBytes(StandardCharsets.UTF_8))
            val ha1 = HexConverter.convertToHexBytes( digest.digest())
            if (d.verifyHA1(ha1)) user
            else null
          case o => println("other Credential "+o+" "+o.getClass);null
        }
      case None => println("User not found "+id);null
    }

  override def verify(credential: Credential): Account = null
}
