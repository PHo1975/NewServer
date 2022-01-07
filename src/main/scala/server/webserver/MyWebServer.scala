package server.webserver

import io.undertow.Undertow
import io.undertow.security.api.{AuthenticationMode, SecurityContext}
import io.undertow.security.handlers.{AuthenticationCallHandler, AuthenticationConstraintHandler, AuthenticationMechanismsHandler, SecurityInitialHandler}
import io.undertow.security.idm.{Account, Credential, IdentityManager}
import io.undertow.security.impl.{BasicAuthenticationMechanism, DigestAuthenticationMechanism}
import io.undertow.server.handlers.resource.*
import io.undertow.server.handlers.{CanonicalPathHandler, PathHandler, SetHeaderHandler}
import io.undertow.server.session.*
import io.undertow.server.{DefaultResponseListener, HttpHandler, HttpServerExchange}
import io.undertow.util.Headers
import org.xnio.{Options, Sequence}
import server.webserver.SessionController.serverConfig

import java.io.{File, FileInputStream}
import java.net.Socket
import java.nio.file.{Files, Path}
import java.security.KeyStore
import java.security.cert.X509Certificate
import java.util.Date
import javax.net.ssl.*
import scala.jdk.CollectionConverters


object MyWebServer:
  val COOKIE_NAME="holzer"
  var sessionNr=1
  val allowedDomains = List("fonts.googleapis.com", "fonts.gstatic.com")
  val allowedStyleDomains = List("fonts.googleapis.com")
  val contentSecurityPolicy: String = "default-src 'none' " +
    " ; connect-src 'self' wss://db.holzer-architektur.de; script-src 'unsafe-inline' 'self' " + allowedDomains.mkString(" ") +
    "; style-src 'unsafe-inline' 'self' " +
    allowedStyleDomains.mkString(" ") + " ; frame-ancestors 'none'; form-action 'self'; img-src 'self' ; base-uri 'self' ; manifest-src 'self' ; font-src " + allowedDomains.mkString(" ") + " ;"
  val allowedCiphers: Sequence[String] =org.xnio.Sequence.of(
    "TLS_ECDHE_RSA_WITH_AES_128_GCM_SHA256",
    "TLS_AES_256_GCM_SHA384",
    "TLS_CHACHA20_POLY1305_SHA256",
    "TLS_ECDHE_ECDSA_WITH_AES_128_GCM_SHA256",
    "TLS_ECDHE_RSA_WITH_AES_256_GCM_SHA384",
    "TLS_ECDHE_ECDSA_WITH_AES_256_GCM_SHA384",
    "TLS_DHE_DSS_WITH_AES_128_GCM_SHA256",
    "TLS_ECDHE_RSA_WITH_AES_128_SHA256",
    "TLS_ECDHE_ECDSA_WITH_AES_128_SHA256",
    "TLS_ECDHE_RSA_WITH_AES_128_SHA",
    "TLS_ECDHE_ECDSA_WITH_AES_128_SHA",
    "TLS_ECDHE_RSA_WITH_AES_256_SHA384",
    "TLS_ECDHE_ECDSA_WITH_AES_256_SHA384",
    "TLS_ECDHE_RSA_WITH_AES_256_SHA",
    "TLS_ECDHE_ECDSA_WITH_AES_256_SHA",
    "TLS_DHE_RSA_WITH_AES_128_SHA256",
    "TLS_DHE_RSA_WITH_AES_128_SHA",
    "TLS_DHE_DSS_WITH_AES_128_SHA256",
    "TLS_DHE_RSA_WITH_AES_256_SHA256",
    "TLS_DHE_DSS_WITH_AES_256_SHA",
    "TLS_DHE_RSA_WITH_AES_256_SHA")

  val am=new DigestAuthenticationMechanism("holzer-datenbank", "holzer-architektur.de", "DIGEST")
  val sessionConfig=new SessionCookieConfig()
  sessionConfig.setSecure(true)
  sessionConfig.setPath("/hund")
  sessionConfig.setComment("Dies und das")
  sessionConfig.setDomain("holzer-architektur.de")
  sessionConfig.setCookieName(COOKIE_NAME)


  def createSSLContext(): SSLContext = {
    val loadedKeyStore: KeyStore = KeyStore.getInstance(KeyStore.getDefaultType)
    val storeStream = new FileInputStream(serverConfig.getString("Paths", "keystoreFile"))
    val password = "Pitpass1#".toCharArray
    loadedKeyStore.load(storeStream, password)
    val keym = KeyManagerFactory.getInstance(KeyManagerFactory.getDefaultAlgorithm)
    keym.init(loadedKeyStore, password)
    val trustm = TrustManagerFactory.getInstance(TrustManagerFactory.getDefaultAlgorithm)
    trustm.init(loadedKeyStore)
    val sslcon = SSLContext.getInstance("TLS")
    sslcon.init(keym.getKeyManagers, trustm.getTrustManagers, null)
    sslcon
  }

  def setHeaderFields(exchange:HttpServerExchange):Unit=
    exchange.getResponseHeaders.put(Headers.STRICT_TRANSPORT_SECURITY, "max-age="+31536000)
    exchange.getResponseHeaders.put(Headers.REFERRER_POLICY,"strict-origin")
    exchange.getResponseHeaders.put(Headers.X_FRAME_OPTIONS,"DENY")
    exchange.getResponseHeaders.put(Headers.X_XSS_PROTECTION,"1; mode=block")
    exchange.getResponseHeaders.put(Headers.X_CONTENT_TYPE_OPTIONS,"nosniff")
    exchange.getResponseHeaders.put(Headers.CONTENT_SECURITY_POLICY,contentSecurityPolicy)


  def start(): Unit =
    val pathHandler=new PathHandler(new unknownPageHandler())
    val hundPageHandler=new HttpHandler(){
      override def handleRequest(exchange: HttpServerExchange): Unit =
        exchange.getResponseHeaders.put(Headers.CONTENT_TYPE,"text/html")
        exchange.getResponseHeaders.put(Headers.ACCEPT_ENCODING,"")
        val hcookie=exchange.getRequestCookie(COOKIE_NAME)
        if(hcookie!=null) println("value:"+hcookie.getValue)
        else {
          //sessionConfig.setSessionId(exchange, (new Date()).getTime.toString)
        }        
        println("Path:"+exchange.getRelativePath)
        exchange.getResponseSender.send("<html><head><meta charset=\"UTF-8\"><title>Testseite</title></head><body>Hallo ihr <br> wie gehts ?<body></html>",
          java.nio.charset.Charset.forName("UTF-8"))
    }
    pathHandler.addExactPath("/hund",hundPageHandler)
    val resManager=new FileResourceManager(new File(serverConfig.getString("Paths","ressourcePath","/home/peter/Tools/testserver")))
    val resHandler=new ResourceHandler(resManager)
    resHandler.setDirectoryListingEnabled(true)
    pathHandler.addPrefixPath("/data", new CanonicalPathHandler(resHandler))
    val h1=new AuthenticationCallHandler(new ErrorHandler(pathHandler))
    val h2=new AuthenticationConstraintHandler(h1)
    val h3=new AuthenticationMechanismsHandler(h2,java.util.List.of(am))
    val mySetHeaderHandler=new HttpHandler() {
      override def handleRequest(exchange: HttpServerExchange): Unit =
        setHeaderFields(exchange)
        h3.handleRequest(exchange)
    }
    val h4=new SecurityInitialHandler(AuthenticationMode.PRO_ACTIVE,UserStore,mySetHeaderHandler)
    val h5=new SessionAttachmentHandler(h4,new InMemorySessionManager("SESSION_MANAGER"),sessionConfig)
    val server=Undertow.builder().setSocketOption(Options.SSL_ENABLED_CIPHER_SUITES,allowedCiphers).
      addHttpsListener(serverConfig.getInt("ServerAddress","port",8080),
      serverConfig.getString("ServerAddress","host","localhost"),createSSLContext()).setHandler(h5).build()
    server.start()
    Console.in.readLine()
    server.stop()


class ErrorHandler(next:HttpHandler) extends HttpHandler:
  override def handleRequest(exchange: HttpServerExchange): Unit =
    try
      next.handleRequest(exchange)
    catch
      case ex:Exception=> if(exchange.isRequestChannelAvailable) println("error "+ex)


class unknownPageHandler() extends HttpHandler:
  override def handleRequest(exchange: HttpServerExchange): Unit =
    exchange.addDefaultResponseListener((exchange: HttpServerExchange) =>{
      if !exchange.isRequestChannelAvailable then false
      else
        println("unknown Page "+exchange.getRequestPath+" "+exchange.getConnection.getPeerAddress.toString)
        val errorPage = "<html><head><meta charset=\"utf-8\"><title>Fehler</title></head><body>Interner Fehler</body></html>"
        exchange.getResponseHeaders.put(Headers.CONTENT_LENGTH, "" + errorPage.length)
        exchange.getResponseHeaders.put(Headers.CONTENT_TYPE, "text/html")
        exchange.getResponseHeaders.put(Headers.ACCEPT_ENCODING,"")
        exchange .getResponseSender.send(errorPage)
        true
     })