// funciones de bases de datos //
#INCLUDE "FIVEWIN.CH"

STATIC aODBC:={},aTablas:={},_oODbc,aDsn:={},aLink:={},aFields:={}
STATIC aZero:={} // Campos Zero
// STATIC aLogico:={}

MEMVAR oDp

*/
// Carga los Dns desde ODBC.INI
*/

FUNCTION LOADDSN(cFileIni,lIni,cDsnName)
 LOCAL cMemo,aLineas,nContar:=0,cLinea,cDsn,cUser,cPass,cType,aData,cPath,cServer,cDataBase,;
       cDescription:="",nPort:=oDp:nPort,cName:="",cEngine:="",nAt

 // LOCAL cServer:=""  // Nombre del Servidor

 LOCAL cDriver:=""

 DEFAULT cFileIni:=oDp:cFileOdbc,lIni:=.T. // "DP\ODBC.INI"

 // aDsn:=EJECUTAR("LOADDSNMYSQL")

 IF oDp:lNativo .AND. .F.
    aDsn:=EJECUTAR("LOADDSNMYSQL")
   RETURN aDsn
 ENDIF

 IF lIni
   aDsn:={}
 ENDIF


 IF !FILE(cFileIni)
    MsgAlert(MI("No se Encontró ",701)+cFileIni)
    RETURN {}
 ENDIF

 cMemo  :=STRTRAN(MEMOREAD(cFileIni),CHR(13),"")
 aLineas:=_VECTOR(cMemo,CHR(10))
 nContar:=1
 DO WHILE nContar<LEN(aLineas)
    cLinea:=aLineas[nContar]
    cLinea :=IIF(left(cLinea,2)="//",subs(cLinea,3,len(cLinea)),cLinea)
    nContar++
    IF !"["$cLinea
       LOOP
    ENDIF
    cDsn      :=STRTRAN(STRTRAN(cLinea,"]",""),"[","")
    IF !Empty(cDsnName)
      cDsn:=cDsnName
    ENDIF
    cPass     :=""
    cUser     :=""
    cType     :=""
    cPath     :=""
    cServer   :=""
    cDataBase :=""
    cDriver   :=""
    cName     :=""
    cEngine   :=""
    DO WHILE nContar<LEN(aLineas) .AND. !("["$aLineas[nContar])
       cLinea:=aLineas[nContar]
       cLinea :=IIF(left(cLinea,2)="//",subs(cLinea,3,len(cLinea)),cLinea)
       aData  :=_VECTOR(cLinea,"=")
       IF LEN(aData)>1
         aData[1]:=UPPE(ALLTRIM(aData[1]))
         aData[1]:=STRTRAN(aData[1],"=","")
         aData[2]:=ALLTRIM(aData[2])
         aData[2]:=IIF( ":"=LEFT(aData[2],1),Subs(aData[2],2,LEN(aData[2])),aData[2])
         IF len(aData)>2
            aData[2]:=" "+aData[2]
            AEVAL(aData,{|c,i|aData[2]:=aData[2]+IIF( i>2,"="+c,"")})
            // aData[2]:=" "+aData[2]"=""+aData[3]
         ENDIF
       ENDIF

       //? cLinea,aData[1],aData[2]

       DO CASE
          CASE EMPTY(aData)
              nContar++
              LOOP
          CASE "USER"$aData[1].AND.LEN(aData)>1
             cUser:=aData[2]
          CASE "PASS"$aData[1].AND.LEN(aData)>1
             cPass:=aData[2]
          CASE "TYPE"$aData[1].AND.LEN(aData)>1
             cType:=aData[2]
          CASE "PATH"$aData[1].AND.LEN(aData)>1
             cPath:=aData[2]
          CASE "SERVER"$aData[1].AND.LEN(aData)>1
             cServer:=aData[2]
          CASE "DATABASE"$aData[1].AND.LEN(aData)>1
             cDatabase:=aData[2]
          CASE "DRIVER"$aData[1].AND.LEN(aData)>1
             cDriver:=aData[2]
          CASE "DESCRIPTION"$aData[1].AND.LEN(aData)>1
             cDescription:=aData[2]
          CASE "PORT"$aData[1].AND.LEN(aData)>1
             nPort:=CTOO(aData[2],"N")
          CASE "NAME"$aData[1].AND.LEN(aData)>1
             cName:=aData[2]
          CASE "ENGINE"$aData[1].AND.LEN(aData)>1 // Nombre del Manejador
             cEngine:=aData[2]

             DEFAULT oDp:cDbEngine:=cEngine

       ENDCASE
       nContar++
    ENDDO

    // ? cDsn,cPass,cType,cName,cEngine

    AADD(aDsn,{UPPE(ALLTRIM(cDsn)),cUser,cPass,cType, cPath,cServer,cDataBase,cDriver,cDescription,nPort,cName,cEngine,cServer})

 ENDDO

 // ? oDp:cDbEngine,"AQUI"
 /*
 // Ahora debe Incorporar las demas Empresas
 */
 IF oDp:cEmpCod<>STRZERO(0,LEN(oDp:cEmpCod)) .AND. .F.

    nAt:=ASCAN(aDsn,{|a,n| a[1]=ALLTRIM(UPPE(oDp:cDsnData)) })

    IF nAt=0
      MensajeErr(oDp:cDsnData+ MI(" no estan Definido ",702)+cFileIni)
      MensajeErr(MemoRead(cFileIni),MI("Archivo ",703)+cFileIni)
    ENDIF

  ENDIF

  nAt:=ASCAN(aDsn,{|a,n| a[1]=ALLTRIM(UPPE(oDp:cDsnConfig)) })

  IF nAt=0
     MensajeErr(oDp:cDsnConfig+MI(" no estan Definido ",702)+cFileIni)
     MensajeErr(MemoRead(cFileIni),MI("Archivo ",703)+cFileIni)
  ENDIF

  // ? cFileINI,oDp:cDsnData,oDp:cDsnConfig

RETURN aDsn

/*
// Agrega las Empresas al DSN
*/
FUNCTION DSNADDEMP()
   LOCAL aLine,cLogin,cPass
   LOCAL oTable

   aDsn  :=LOADDSN()
   cLogin:=aDsn[1,2]
   cPass :=aDsn[1,3]
   aLine :=ACLONE(aDsn[1])

   oTable:=OpenTable("SELECT EMP_BD,EMP_DSN,EMP_NOMBRE,EMP_CLAVE,EMP_LOGIN FROM DPEMPRESA",.T.)
   oTable:GoTop()

   WHILE !oTable:Eof()

     aLine[1]:=UPPER(ALLTRIM(oTable:EMP_DSN   ))
     aLine[2]:=cLogin
     aLine[3]:=cPass

     IF !EMPTY(oTable:EMP_LOGIN)
       aLine[2]:=UPPER(ALLTRIM(oTable:EMP_LOGIN ))
     ENDIF

     IF !EMPTY(oTable:EMP_CLAVE)
       aLine[3]:=UPPER(ALLTRIM(oTable:EMP_CLAVE ))
     ENDIF

     aLine[7]:=UPPER(ALLTRIM(oTable:EMP_BD    ))
     aLine[9]:=UPPER(ALLTRIM(oTable:EMP_NOMBRE))

     AADD(aDsn,ACLONE(aLine))

     oTable:DbSkip(1)

   ENDDO

   oTable:End()

RETURN aDsn

FUNCTION OPENODBC(cDsn,cLogin,cPass,cType,cPath,lOpen,cEngine,lCreate,oServer)
   LOCAL nAt:=0,oOdbc // ,cLogin,cPass,cType,cPath

   DEFAULT lCreate:=.T.

   IIF( EMPTY(aDsn),LoadDsn(),NIL )

   IF EMPTY(cDsn)  // Asume el Dsn por Defecto para las Empresas
     cDsn  :=oDp:cDsnData // VP("DSNNAME" )
   ENDIF

   // JN 26/04/2016 Abre el DSN segun la variable, oDp:cDsnData
   cDsn:=IIF("<"$LEFT(cDsn,1),oDp:cDsnData  ,cDsn)
   cDsn:=IIF("."$LEFT(cDsn,1),oDp:cDsnConfig,cDsn)
   cDsn:=IIF("-"$LEFT(cDsn,1),oDp:cDsnDicc  ,cDsn)

   cDsn  :=UPPE(ALLTRIM(cDsn))

   IF Empty(cDsn)
      MensajeErr(MI("Requiere Indicar Base de Datos",704)+CRLF+oDp:oRunLine:oFunction:oScript:cProgram+" Function "+oDp:oRunLine:oFunction:cName+oDp:oRunLine:cLine,[FUNCTION OPENODBC(cDsn])
      RETURN NIL
   ENDIF

   nAt   :=ASCAN(aDsn,{|aVal| ALLTRIM(UPPE(aVal[1]))==cDsn })

   IF nAt>0
      cLogin :=aDsn[nAt,02]
      cPass  :=aDsn[nAt,03]
      cType  :=aDsn[nAt,04]
      cPath  :=aDsn[nAt,05]
      cEngine:=aDsn[nAt,12]
   ENDIF

   IF Empty(cPass)
      cPass :=oDp:cPass
      cLogin:=oDp:cLogin
   ENDIF

// ? cDsn,"cDsn"
// ViewArray(aOdbc)
// ? nAt,cDsn,"BUSCADO EN OPENODBC",cLogin,cPass,cType,cPath,cEngine

   nAt   :=ASCAN(aODBC,{|aVal| UPPE(ALLTRIM(aVal[1])) == cDsn })

   cLogin :=IIF( EMPTY(cLogin),NIL ,ALLTRIM(cLogin) )
   cPass  :=IIF( EMPTY(cPass ),NIL ,ALLTRIM(cPass)  )
   cType  :=IIF( EMPTY(cType ),""  ,cType  )
   cEngine:=IIF( EMPTY(cEngine),"" ,cEngine ) // Motor de la Base de Datos

   IF nAt=0 // no está Incluida


      lOpen:=.T. // Viere por Fererencia

//? oDp:cTypeBD,[oDp:cTypeBD="MYSQL"],oServer:ClassName()

      IF oDp:cTypeBD="MYSQL" .AND. oDp:lNativo

        IF oServer=NIL

//Errorsys(.T.)
            MySqlStart(oDp:lMySqlError)
           oServer:=oDp:oMySqlCon
        ENDIF

//? oServer:ClassName(),"OPENODBC"

        /*
        // Si la BD no existe, sera creada
        */

        // nAt:=ASCAN(oDp:oMySqlCon:aDataBases,{|a,n| lower(a)=lower(cDsn)})
        // 13/09/2016
        cDsn:=ALLTRIM(cDsn)

        nAt:=ASCAN(oServer:aDataBases,{|a,n| lower(a)=lower(cDsn)})

        IF nAt=0
          // oDp:oMySqlCon:CreateDB( Lower(cDsn) )
          oServer:CreateDB( Lower(cDsn) )
          oDp:lCreateDb:=.T.
        ENDIF

// ? cDsn,"ABRIR",GETPROCE()

        oOdbc:=MyOpenDataBase(Lower(cDsn),.F.,oServer)

//? oOdbc:ClassName(),"AQUI ES"

      ELSE

        oODbc:=TODBC():New(Lower(cDsn), cLogin, cPass )

      ENDIF

      oOdbc:cType:=IIF(Empty(cType), oDp:cTypeBD, cType)

      oOdbc:cPath:=cPath
      oOdbc:lDateAsStr:=.f.
      oOdbc:cEngine:=cEngine

      AADD(aODBC,{UPPE(ALLTRIM(cDsn)),oODbc})

      nAt:=LEN(aODBC)

   ELSE

      lOpen:=.F. // pasa por Referencia

   ENDIF

   oOdbc:=NIL
   aODBC[nAt,2]:cType:=IIF(Empty(cType),oDp:cTypeBD,cType)


RETURN aODBC[nAt,2]

FUNCTION GetAllOdbc()
RETURN aODBC

/*
// Cierra DSN
*/
FUNCTION CLOSEODBC(cDsn,lDelete,oServer)
   LOCAL nAt,oOdbc

//   IF ValType(cDsn)="C"
//     cDsn :=ALLTRIM(UPPE(cDsn))
//   ENDIF

   nAt   :=ASCAN(aODBC,{|aVal| ALLTRIM(UPPE(aVal[1])) == cDsn })

   // Cuando la conexion de la BD esta en otro Servidor genera incidencia,
   MyCloseDataBase(cDsn,lDelete,oServer)  // Aqui cierra MySql

   IF nAt=0
      RETURN .F.
   ENDIF

   oOdbc:=OPENODBC(cDsn,NIL,NIL,NIL,NIL,NIL,NIL,NIL,oServer) // Aqui la esta Abriendo, Debera buscarla si esta abierta
   //MensajeErr( cDsn + GetProce(),  "AQUI ENVIAN A CERRARLA DEBE CERRAR LA BD EN TMYTABLE" )

   IF ValType(oOdbc)="O"
      oOdbc:End()
   ENDIF

   nAt   :=ASCAN(aODBC,{|aVal| ALLTRIM(UPPE(aVal[1])) == cDsn })

   IF nAt>0
      aODBC:=ARREDUCE( aODBC , nAt)
      // aODBC[nAt,2]:End()
      // ADEL(aODBC,nAT)
      // ASIZE(aODBC,LEN(aODBC)-1)
   ENDIF

// MyCloseDataBase(cDsn,lDelete,oServer)  // Aqui cierra MySql

   IF cDsn==UPPE(ALLTRIM(oDp:cDsnData))
      aTablas:={}
      aLink  :={}
      oDp:aCamposOpc:={}
      aZero  :={}
//    GetTables() // Debe leer Nuevament las Tablas
   ENDIF

RETURN (nAt>0)

FUNCTION CLOSETABLA(oTabla) // cTabla)
   Local lRet:=IIF( ValType(oTabla)="O",(oTabla:End(),.t.),.f.)
   oTabla:=NIL
RETURN lRet

/*
// Apertura de Tablas
*/
FUNCTION OPENTABLA(cTabla,cDsn,cSql,lDirect,lExcluye)

   LOCAL nAt:=IIF(EMPTY(aTablas),LOADTABLAS(),NIL),oOdbc,oTabla,I,cVarName,aData
   LOCAL nAt2
   LOCAL aIndex
   LOCAL aExcluye  :=GetExcluye()


//   ? "OPEN TABLA"

   DEFAULT cSql    :="SELECT * FROM "+cTabla,;
           lDirect :=.F.,;
           aExcluye:={},;
           lExcluye:=.T.

   IF !"SELECT"$cSQL
      cSQL:="SELECT * FROM "+cTabla+" "+cSQL
   ENDIF


   nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

   IF nAt=0

     IF FILE("STRUCT\"+cTabla+".TXT")
        MsgRun("Importante Tabla "+cTabla)
        EJECUTAR("DPFILSTRTAB",cTabla,.T.)
        LOADTABLAS(.T.)
     ENDIF

     nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

   ENDIF

   // Aqui debe Descargarla desde AdaptaPro Server

   IF nAt=0
     EJECUTAR("MSGMEMO",MI("Tabla ",705)+cTabla +MI(" no está Incluida en el Diccionario de Datos ",706)+CRLF+GETPROCE(),STR(LEN(aTablas))+MI(" en aTablas",707))
     RETURN .F.
   ENDIF

  // MsgAlert(cSql,"OPENTABLA")

   oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN

//   IF !EMPTY(aExcluye)
//     ? cTabla,aExcluye[1,1],aExcluye[1,2]
//   ENDIF

   IF nAt>0 .AND. LEN(aTablas[nAt])>=9 .AND.  aTablas[nAt,9]!=NIL
      cSql:=STRTRAN(cSql," "+ALLTRIM(cTabla)+" "," "+ALLTRIM(aTablas[nAt,9])+" ")
      cSql:=SETFIELDAS(cSql,aTablas[nAt,11])
   ENDIF

   IF lExcluye .AND. (nAt2:=ASCAN(aExcluye,{|a,n|a[1]=cTabla}),nAt2>0)
      cSql:=BuildExcluye(cSql,aExcluye[nAt2,2])
   ENDIF

//  ? "OPENTABLA",cSql
// MEMOWRIT("\SQL.SQL",cSql)

  IF lDirect
    oTabla :=oOdbc:Query(cSql) // TDbOdbc():New(cSql, oODbc )
    oTabla:cFileName:=cTabla
    oTabla:oOdbc:=oOdbc // Implantado en TODBCDIRECT
    //oTabla:cSingular:=aTablas[nAt,8] // Nombre Singular
  ELSE
    oTabla :=TDbOdbc():New(cSql, oODbc )
    oTabla:cFileName:=cTabla
    AADD(oTabla:aIndex,{1})
  ENDIF

  SysRefresh()

  aTablas[nAt,4]:=NIL // oTabla

  oOdbc  :=NIL

RETURN oTabla

FUNCTION GETDSN(cTabla,lSay,lDown)
   LOCAL cDsn
   LOCAL nAt:=IIF(EMPTY(aTablas),LOADTABLAS(.T.),NIL),oOdbc,oTabla,I,cVarName,aData
   LOCAL aIndex

   DEFAULT lSay:=.T.,;
           lDown:=.T. // Descargar si no existe

   cTabla:=UPPER(cTabla) // JN 05/05/2020

   IF Empty(aTablas)
      LOADTABLAS(.T.)
   ENDIF

   // nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })
   // JN  11/01/2017 , Si no existe en el diccionario de datos, busca en datadbf/*.*

   WHILE .T.

     nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

     IF nAt=0

       nAt   :=ASCAN(oDp:aVistas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

       IF nAt>0
          RETURN oDp:aVistas[nAt,3]
       ENDIF

     ENDIF

     IF nAt=0
        LOADTABLAS(.T.)
        nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })
     ENDIF

     IF nAt=0

       // 24/08/2021 evitar incidencias
       IF nAt=0  .AND. lDown
         EJECUTAR("DPTABLANODICCDAT",cTabla,lSay)
         LOADTABLAS(.T.)
       ENDIF

       nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

       IF nAt>0
          RETURN ALLTRIM(aTablas[nAt,3])
       ENDIF

       IF lSay
         EJECUTAR("MSGMEMO",MI("Tabla ",705)+cTabla +MI(" no está Incluida en el Diccionario de Datos",706)+CRLF+GETPROCE())
       ENDIF

      RETURN ""

     ENDIF

   EXIT

   ENDDO

   cDsn:=ALLTRIM(aTablas[nAt,3])
   cDsn:=IIF("<"$LEFT(cDsn,1),oDp:cDsnData  ,cDsn)
   cDsn:=IIF("."$LEFT(cDsn,1),oDp:cDsnConfig,cDsn)
   cDsn:=IIF("-"$LEFT(cDsn,1),oDp:cDsnDicc  ,cDsn)

RETURN cDsn // ALLTRIM(aTablas[nAt,3])


/*
// Obtiene el Odbc según el Nombre de la Tabla
*/
FUNCTION GETODBC(cTabla)
RETURN OPENODBC(GETDSN(cTabla))
/*

   LOCAL nAt:=IIF(EMPTY(aTablas),LOADTABLAS(),NIL),oOdbc,oTabla,I,cVarName,aData
   LOCAL aIndex

   nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

   IF nAt=0
      ALERT("Tabla "+cTabla +" no está Incluida en el Diccionario de Datos")
      RETURN .F.
   ENDIF

   oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN

   // SysRefresh()

RETURN oOdbc
*/

FUNCTION CountOpenOdbc()
RETURN LEN(aODBC)

/*
// Carga de las tablas del Sistema
*/

FUNCTION LOADTABLAS(lIni,cDsn) // Debe Cargar en Memoria todas las tablas
 LOCAL oTablas,oOdbc,oTabla,cSql,nAt,cLogin,cPass,cType:="",oLink,I
 LOCAL aCopy:={} ,cWhere:=""

 IF oDp:lCgi // CGI NO REQUIERE ESTA OPCION LO HACE LENTO
   RETURN {}
 ENDIF

 DEFAULT lIni:=.F.,cDsn:=oDp:cDsnData    //  ? I,aTablas[I,3]

 //oDp:oFrameDp:SetText("LOADTABLAS "+LSTR(SECONDS())+" LSTR "+LSTR(LEN(aTablas))),ERRORSYS(.T.)

 IF lIni
    aTablas:={}
//  aDsn   :={}
    aLink  :={}
    oDp:aCamposOpc:={}
    aZero  :={}

    oDp:aFields2:={} // UTILIZADO POR LOADSTRUCT

// ? GetProce()
// aLogico:={}
 ENDIF

 oDp:aTablas:=aTablas

 IF !Empty(aTablas)
    RETURN aTablas
 ENDIF


 IF Empty(oDp:cDsnData)

    oDp:cDsnData:=oDp:aDsn[2,7] // GETINI(oDp:cPathExe+"DATAPRO.INI","DSNNAME" )    // DSN de la Empresa Principal

 ENDIF

 DEFAULT oDp:aVistas:={}

 DEFAULT aTablas :={}

 IF oDp:nVersion<6
   oDp:cDsnDicc:=oDp:cDsnConfig
// AADD(aTablas,{"0001","DPTABLAS",oDp:cDsnConfig ,"","",NIL,"TABLAS","Tablas",NIL,.T.})
// AADD(aTablas,{"0002","DPCONFIG",oDp:cDsn  Config ,"","",NIL,"CONFIG","Config",NIL,.T.})
// nAt     :=ASCAN(aDsn,{|aVal| ALLTRIM(aVal[1]) == ALLTRIM(oDp:cDsnConfig) })
 ENDIF

 AADD(aTablas,{"0001","DPTABLAS"  ,oDp:cDsnDicc ,"","",NIL,"TABLAS",MI("Tablas",705),NIL,.T.})
 AADD(aTablas,{"0002","DPCONFIG"  ,oDp:cDsnDicc ,"","",NIL,"CONFIG","Config",NIL,.T.})
 AADD(aTablas,{"0003","DPCAMPOS"  ,oDp:cDsnDicc,"","",NIL,,,,.T.})
 AADD(aTablas,{"0004","DPCAMPOSOP",oDp:cDsnDicc,"","",NIL,,,,.T.})
 AADD(aTablas,{"0005","DPVISTAS"  ,oDp:cDsnDicc,"","",NIL,,,,.T.})

 nAt     :=ASCAN(aDsn,{|aVal| ALLTRIM(aVal[1]) == ALLTRIM(oDp:cDsnDicc) })

 IF nAt>0 // Busca el Dsn
   cLogin:=aDsn[nAt,2]
   cPass :=aDsn[nAt,3]
   cType :=aDsn[nAt,4]
 ENDIF

// ?  oDp:cDsnConfig,cLogin,cPass,"DESDE LOADTABLAS"
//oOdbc   := OPENODBC(oDp:cDsnConfig,cLogin,cPass)
 oOdbc   := OPENODBC(oDp:cDsnDicc,cLogin,cPass)

 IF !oOdbc:lSuccess .AND. oDp:lNativo
   HRBLOAD("MYSQL.HRB")
   MACROEJE("MySqlConect()")
   oOdbc   := OPENODBC(oDp:cDsnDicc,cLogin,cPass)
 ENDIF

 IF !oOdbc:lSuccess
   MsgAlert("BD: "+oDp:cDsnDicc+MI(" sin conexión",708))
 ENDIF

 // ? oOdbc:hEnv, oOdbc:hDbc
 // ? oOdbc:lSuccess, oOdbc:lShowError, oOdbc:lAbort , oOdbc:smterror
 //  __QUIT()

 oOdbc:cType:=cType

// ? "BUSCAR TABLAS"

// AADD(aTablas,{"0001","DPTABLAS",oDp:cDsnConfig ,"","",NIL,,,,.T.})
// AADD(aTablas,{"0002","DPCAMPOS",oDp:cDsnConfig ,"","",NIL,,,,.T.})


// MsgAlert("AQUI ES LOADTABLAS")

 aCopy:=ACLONE(aTablas)

 cWhere:=" WHERE TAB_VISTA"+GetWhere("=",.F.) // No incluye las Vistas

/*
 IF ISFIELD("DPTABLAS","TAB_VISTA")
    cWhere:=" WHERE TAB_VISTA"+GetWhere("=",.F.) // No incluye las Vistas
//  cWhere:=" WHERE BLAS","TAB_VISTA",.F.,"LEFT(TAB_VISTA,5)"+GetWhere("<>","VIEW_"))
 ENDIF
*/

  oTablas:= OpenTable("SELECT * FROM DPTABLAS "+cWhere,.T.,oODbc )
  oTablas:lSetFieldDef:=.F.

//? oTablas:Browse()

  IF oTablas:RecCount()=0
     oTablas:End()
     RunMacro("IMPORTDPTABLAS()")
     oTablas := OpenTable("SELECT * FROM DPTABLAS",.T.,oODbc )
 ENDIF


 IF oTablas:RecCount()>0
//MsgAlert("DPTABLAS SI TIENE REGISTROS, ESTOY EN LOADTABLAS")
ELSE
//    MsgAlert("DPTABLAS NO TIENE REGISTROS, ESTOY LOADTABLAS")
    aTablas :={}
 ENDIF

 // ViewArray(oTablas:aFields,,,.f.)
 // oDp:lViewArrayMdi:=.F.
 // VIEWARRAY(oTablas:aFields)

// oTablas:CTODBF("TEST.DBF")
// ? oDp:cDsnConfig,"Gracias",oTablas:ClassName()
// ? oTablas:aFields,oTablas:cError

 IF oTablas:FieldPos("TAB_SINGUL")=0
   oTablas:AddFields("TAB_SINGUL","")
 ENDIF

//IF oTablas:FieldPos("TAB_VISTA")=0
//   oTablas:AddFields("TAB_VISTA","")
//ENDIF

/*
 DO({||!oTablas:Eof()},NIL,{||AADD(aTablas,{oTablas:TAB_NUMERO,;
                                            ALLTRIM(oTablas:TAB_NOMBRE),;
                                            ALLTRIM(oTablas:TAB_DSN),;
                                            oTablas:TAB_APLICA,;
                                            "",;
                                            NIL,;
                                            ALLTRIM(oTablas:TAB_DESCRI)+" ",;
                                            ALLTRIM(oTablas:TAB_SINGUL),;
                                            NIL,;
                                            oTablas:TAB_CONFIG,;
                                            NIL}),;
                                            oTablas:skip(1)})
*/

 DO({||!oTablas:Eof()},NIL,{||AADD(aTablas,{oTablas:TAB_NUMERO,;
                                            ALLTRIM(oTablas:TAB_NOMBRE),;
                                            ALLTRIM(oTablas:TAB_DSN),;
                                            oTablas:TAB_APLICA,;
                                            ALLTRIM(oTablas:TAB_PRIMAR),;
                                            NIL,;
                                            ALLTRIM(oTablas:TAB_DESCRI)+" ",;
                                            ALLTRIM(oTablas:TAB_SINGUL),;
                                            NIL,;
                                            !("<"$oTablas:TAB_DSN),;
                                            NIL}),;
                                            oTablas:skip(1)})

 /*
 DO({||!oTablas:Eof()},NIL,{||AADD(aTablas,{oTablas:TAB_NUMERO,;
                                            ALLTRIM(oTablas:TAB_NOMBRE),;
                                            ALLTRIM(oTablas:TAB_DSN),;
                                            oTablas:TAB_APLICA,;
                                            "",;
                                            NIL,;
                                            ALLTRIM(oTablas:TAB_DESCRI)+" ",;
                                            ALLTRIM(oTablas:TAB_SINGUL),;
                                            NIL,;
                                            !("<"$oTablas:TAB_DSN),;
                                            NIL}),;
                                            oTablas:skip(1)})
*/

// ? oTablas:TAB_CONFIG,"TAB_CONFIG"

 oTablas:End()

 //IF oDp:lVistas

  oDp:aVistas:={}

  /*
  // JN 25/05/2016 Cuando se instala no hay vistas
  */

  IF oOdbc:FILE("DPVISTAS")

    oTablas := OpenTable("SELECT * FROM DPVISTAS WHERE VIS_ACTIVO=1 ",.T.,oODbc )
/*
  DO({||!oTablas:Eof()},NIL,{||AADD(oDp:aVistas,{oTablas:VIS_VISTA,;
                                              ALLTRIM("VIEW_"+UPPER(oTablas:VIS_VISTA)),;
                                              IIF("<"$oTablas:VIS_DSN,oDp:cDsnData,oDp:cDsnConfig),;
                                              "00",;
                                              "",;
                                              NIL,;
                                              ALLTRIM(oTablas:VIS_NOMBRE)+" ",;
                                              "",;
                                              NIL,;
                                              !("<"$oTablas:VIS_DSN),;
                                              NIL}),;
                                              oTablas:skip(1)})
*/
   DO({||!oTablas:Eof()},NIL,{||AADD(oDp:aVistas,{oTablas:VIS_VISTA,;
                                                ALLTRIM("VIEW_"+UPPER(oTablas:VIS_VISTA)),;
                                                ALLTRIM(oTablas:VIS_DSN),;
                                                "00",;
                                                "",;
                                                NIL,;
                                                ALLTRIM(oTablas:VIS_NOMBRE)+" ",;
                                                "",;
                                                NIL,;
                                                !("<"$oTablas:VIS_DSN),;
                                                NIL}),;
                                                oTablas:skip(1)})


    oTablas:End()

//    oDp:oFrameDp:SetText(oTable:VIS_VISTA)

  ENDIF

  IF Empty(aTablas)
    aTablas:=ACLONE(aCopy)
  ENDIF

  IF oDp:nLang<>3
   aTablas:=EJECUTAR("LOADTABLAS",aTablas)
  ENDIF

 // Las tablas Asociadas a <MULTIPLE> Debe estar Asociado a oDp:cDsnData
 // ViewArray(aTablas,,,.F.)

 FOR I := 1 TO LEN(aTablas)

   IF ValType(aTablas[I,10])="C"
      aTablas[I,10]:= (aTablas[I,10]="1")
   ENDIF

   aTablas[I,10]:=CTOO(aTablas[I,10],"L")

   // Desde la Version 6.0, esta es nomenclatura
   IF oDp:nVersion>=5 .OR. .T.  // Todos los Binarios funcionan 5.1 Y 6.0

     IF "<"$aTablas[I,3]
       aTablas[I,3]:=oDp:cDsnData    //  I,aTablas[I,3]
     ENDIF

     IF "."$aTablas[I,3]
       aTablas[I,3]:=oDp:cDsnConfig  //  Configuración
     ENDIF

     IF "-"$aTablas[I,3]
       aTablas[I,3]:=oDp:cDsnDicc  //  Diccionario de Datos
     ENDIF

   ELSE

     IF aTablas[I,10]
       aTablas[I,3]:=oDp:cDsnConfig    //  ? I,aTablas[I,3]
     ELSE
       aTablas[I,3]:=oDp:cDsnData // cDsn
     ENDIF

   ENDIF

/*
 IF !oDp:IsDef(aTablas[I,2]) // Bautiza el Nombre de las Tablas
      oDp:Add(aTablas[I,2]    ,aTablas[I,7])
      oDp:Add("X"+aTablas[I,2],aTablas[I,8]) // Singular
   ELSE
*/
    oDp:Set(ALLTRIM(aTablas[I,2])    ,aTablas[I,7])
    oDp:Set("X"+ALLTRIM(aTablas[I,2]),aTablas[I,8])
//   ENDIF

 NEXT

 // necesario REPBDLIST  17/02/2021
 FOR I=1 TO LEN(oDp:aVistas)
   oDp:Set(ALLTRIM(oDp:aVistas[I,2]),oDp:aVistas[I,7])
   oDp:Set("X"+ALLTRIM(oDp:aVistas[I,2]),oDp:aVistas[I,7])
 NEXT I


// ? cDsn,oDp:cDsnData
// ViewArray(aTablas,NIL,NIL,.F.)

 //? "aqui es loadtablas",len(aTablas)
 // Carga los enlaces de las tablas
 // oLink:= TDbOdbc():New("SELECT * FROM DPLINK", oODbc )

 IF oOdbc:File("DPLINK")

   oLink:= OpenTable("SELECT * FROM DPLINK", NIL , oODbc )

   DO({||!oLink:Eof()},NIL,{||AADD(aLink,{ALLTRIM(UPPE(oLink:LNK_TABLES))+"."+ALLTRIM(UPPE(oLink:LNK_TABLED)),ALLTRIM(oLink:LNK_FIELDS),ALLTRIM(oLink:LNK_FIELDD),NIL             ,NIL}),;
                              AADD(aLink,{ALLTRIM(UPPE(oLink:LNK_TABLED))+"."+ALLTRIM(UPPE(oLink:LNK_TABLES)),ALLTRIM(oLink:LNK_FIELDD),ALLTRIM(oLink:LNK_FIELDS),oLink:LNK_UPDATE,NIL}),;
                              oLink:skip(1)})

//   DO({||!oLink:Eof()},NIL,{||AADD(aLink,{ALLTRIM(UPPE(oLink:LNK_TABLEG))+"."+ALLTRIM(UPPE(oLink:LNK_TABLER)),ALLTRIM(oLink:LNK_FIELDG),ALLTRIM(oLink:LNK_FIELDR)}),;
//                           AADD(aLink,{ALLTRIM(UPPE(oLink:LNK_TABLER))+"."+ALLTRIM(UPPE(oLink:LNK_TABLEG)),ALLTRIM(oLink:LNK_FIELDD),ALLTRIM(oLink:LNK_FIELDR),oLink:LNK_UPDATE}),;
//                            oLink:skip(1)})
   oLink:End()

   // 24/03/2023
   // Cuando se Instala y no Tiene Tablas no puede buscar DPCAMPOS JN 27/05/2016
   //

   DEFAULT  oDp:aLogico:=NIL


 ENDIF

 oLink  :=NIL
 oTablas:=NIL

// oDp:oFrameDp:SetText("Leyendo DPLOADTABLAS() "+LSTR(SECONDS())+" ATABLAS "+LSTR(LEN(aTablas)))
//? "AQUI FINALIZAR LOADTABLAS",GETPROCE()

 IF EMPTY(oDp:aCamposOpc)
   //  ? "AQUI LLAMA A LOADCAMPOSPC"
   // JN 26/09/2018, Genera incidencia, lectura de tabla dpcamposopc
   LOADCAMPOSOPC()
 ENDIF

 // IF ValType(oDp:oFrameDp)="0"
 //   ViewArray(aTablas)
 // ENDIF
 //? "AQUI TERMINA LOAD TABLAS", LEN(aTablas),GETPROCE()

 oDp:aTablas:=aTablas

 // 24/03/2023

 DEFAULT oDp:lLogicField:=.T.,;
         oDp:aLogico    :=NIL

 IF oOdbc:File("DPLINK") .AND. oDp:aLogico=NIL .AND. oDp:lLogicField

    oDp:aLogico:={}

    ADDTABLE("0000","DPCAMPOS",oDp:cDsnConfig,.T.) // Evita quedarse en LOOP

    oDp:aLogico:=ASQL("SELECT RTRIM(CAM_TABLE),CAM_NAME FROM DPCAMPOS WHERE CAM_TYPE='L' ORDER BY CAM_TYPE",OPENODBC(oDp:cDsnConfig))

    AEVAL(oDp:aLogico,{|a,n| oDp:aLogico[n,1]:=ALLTRIM(a[1]),;
                             oDp:aLogico[n,2]:=ALLTRIM(a[2])})
  ENDIF

RETURN LEN(aTablas)>0

FUNCTION GetLinkTable(cTablaE,cTableD,nLink,lArray)
   LOCAL cData:=ALLTRIM(UPPE(cTablaE))+"."+ALLTRIM(UPPE(cTableD)),nAT:=0,cLink

   DEFAULT nLink :=1 // 1=enlazador 2=Enlazado
   DEFAULT lArray:=.F.

// ? cData

   nAt   :=ASCAN(aLink,{|aVal| aVal[1] == cData })
   cLink :=IIF( nAt=0 , cLink , aLink[nAt,nLink+1])

   IF ValType(cLink)="C" .AND. ","$cLink
      cLink :=_VECTOR(cLink)
      lArray:=.f.
   ENDIF

  // ? cLink

   IF lArray.and.ValType(cLink)="C"  // los Convierte en Arreglos
      cLink:=_VECTOR(cLink,"+")
   ENDIF

//   ? ValType(cLink),cLink

RETURN cLink

FUNCTION GetLinks(cTablaE)
   LOCAL cData :=ALLTRIM(UPPE(cTablaE))+".",I
   LOCAL aLinks:={},cLinked

   // Busca los enlazados
   FOR I := 1 TO LEN(aLink)
     IF cData=LEFT(aLink[I,1],LEN(cData))
         cLinked:=aLink[I,1]
         cLinked:=SUBS(cLinked,AT(".",cLinked)+1,20)
         AADD(aLinks,aLink[I])
         // aLinks[LEN(aLinks),1]:=cLinked
      ENDIF
   NEXT I
   // aLinks:={}

RETURN aLinks

/*
// Cantidad de Tablas Enlazadas
*/
FUNCTION nGetLinks(cTablaE)
   LOCAL nLinks:=0 // Cantidad de Enlaces Válidos
   LOCAL aTablas:=GetLinks(cTablaE)

   AEVAL(aTablas,{|a,n|nLinks:=nLinks+IIF(ValType(4)="N",1 ,0 )})

RETURN nLinks




/*
// Devuelve la Lista de Tablas Definidas para el Programa
*/

FUNCTION DpGetTables(lLoad)

   local aLista:={},i

   DEFAULT lLoad:=.T.

   IF EMPTY(aTablas) .AND. lLoad
      LOADTABLAS()
   ENDIF

   FOR i=1 to len(aTablas)
     AADD(aLista,aTablas[i,2])
   NEXT i

RETURN aLista

FUNCTION CLOSEALLDSN()
   LOCAL I

//? "aqui es CLOSEALLDSN",oDp:cDpXbaseLine

   AEVAL(aTablas,{|a,i|IIF( ValType(aTablas[I,4])="O",aTablas[I,4]:End() ,NIL )}) // Quedan Cerradas las Tablas
   aTablas:={}
   AEVAL(aODBC,{|a,i| IIF( ValType(aODBC[I,2])="O",(aODBC[I,2]:lAbort:=.T.,aODBC[I,2]:End()),NIL )}) // Quedan Cerrados Todos los DSN
   aODBC:={}
   _oODbc:=NIL

   MyCloseDataBase()

RETURN NIL


/*
// Revisa la estructura de una Tabla
*/

FUNCTION Checktable(cNumTab,lDrop,lDroptable,lIni,lSay,cDsn,oOdbc)
   LOCAL lResult,nContar:=0,aFile,cTabla,aIndex,aStruct,lOk
   LOCAL oTabla,aFields,I,U,cFileTmp,cSql,aStructs,cTable
   LOCAL oOrigen,cSep:=","
   LOCAL aFieldsNew:={},aFieldsDel:={},aFieldsChange:={},nAt,cDec,cType
   LOCAL cMemo,cLen,cBuild:="DBFBUILD" //,cExt:=" DROP PRIMARY KEY "
   LOCAL aSql:={} //,aIndex:={}
   LOCAL lArray:=.T.,nT1 ,lAlter:=.F.
   LOCAL cNomTab:="",cMySqlErr:=""
   LOCAL lResp // Resultado de Alter Table
   LOCAL lIndex // Indica que tiene Indices
   LOCAL cNumTab_:=cNumTab
   LOCAL lOpenTableChk


   DEFAULT oDp:lLoadTablas  :=.T.,;
           oDp:lOpenTableChk:=.F.

   // 20/11/2020 No se puede ejecutar ASIMISMO, Desde revision de Tablas, desactiva CHEQUEAR DSN
   lOpenTableChk:=oDp:lOpenTableChk
   oDp:lOpenTableChk:=.F.

   DEFAULT oDp:cMemoSql:="",;
           lDrop       :=.F.,;
           lDropTable  :=.F.,;
           oDp:lSayCheckTable:=.F.,;
           lIni        :=oDp:lLoadTablas,;
           lSay        :=oDp:lSayCheckTab

//? "PARAMETRO",oOdbc:ClassName(),"CHECKTABLE",cNumTab,lDrop,lDroptable,lIni,lSay,cDsn,oOdbc,"<-cDsn"

   // JN 16/08/2014 Necesito los Campos desde las Versiones Anteriores

   lDrop  :=.F.
   cNumTab:=ALLTRIM(UPPER(cNumTab))

   IF cNumTab="DPDOCPLATILLA"
      cNumTab:="DPDOCPLANTILLA"
   ENDIF

   Sysrefresh(.T.)

   LOADTABLAS(lIni .OR. Empty(aTablas))

   // JN 22/09/2014
   IF IsAllDigit(cNumTab)

     // busca por Numero, necesitamos el Nombre de la Tabla

     nAt   :=ASCAN(aTablas,{|aVal| aVal[1] == cNumTab }) // Necesito el Dsn

     IF nAt>0
       cNomTab:=aTablas[nAt,2]
     ENDIF

   ELSE
     cNomTab:=cNumTab
   ENDIF

   // JN 22/02/2016
   // oDp:oFrameDp:SetText(cNomTab)
   IF "VIEW_"=LEFT(UPPER(cNomTab),5)
      RETURN .T.
   ENDIF

//      IF "DPPCLOG"$cNomTab
//? "AQUI ES LLAMADA ",cNomTab, oDp:cDpXbaseLine,oDp:cDpXBaseRun
//      ENDIF

//   ErrorSys(.T.)
   SysRefresh(.T.)
//   oDp:oFrameDp:SetText("CheckTable "+cNomTab)

   // Muestra la Ejecución del Revisión de la Tabla
   IF oDp:lSayCheckTable .AND. ValType(oDp:oSay)="O"
      oDp:oSay:SetText("Checktable "+cNomTab)
      Sysrefresh(.t.)
   ENDIF

   // Busca la Tabla por nombre
   cNomTab :=STRTRAN(cNomTab,CHR(10),"")
   cNomTab :=STRTRAN(cNomTab,CHR(13),"")
   cNomTab :=ALLTRIM(UPPER(cNomTab))
   nAt     :=ASCAN(aTablas,{|aVal| ALLTRIM(aVal[2]) == cNomTab }) // Necesito el Dsn
   // 05/10/2018, Si la tabla no existe, agrega diccionario de datos,
   IF nAt=0
      // ViewArray(aTablas)
      // ? cNomTab,cNumTab,cNumTab_,"cNomTab,cNumTab,cNumTab_"
      //  20/11/2020 EJECUTAR("DPTABLANODICCDAT",cNomTab,.T.)
//aTablas,cNumTab)
      LOADTABLAS(lIni .OR. Empty(aTablas))
      nAt     :=ASCAN(aTablas,{|aVal| ALLTRIM(aVal[2]) == cNomTab }) // Necesito el Dsn
   ENDIF

   IF nAt=0

      cNomTab:=CTOO(cNomTab,"C")

      DEFAULT oDp:cDpXBaseRun :=""
      DEFAULT oDp:cDpXbaseLine:=""

      oDp:cDpXBaseRun :=CTOO(oDp:cDpXBaseRun ,"C")
      oDp:cDpXbaseLine:=CTOO(oDp:cDpXbaseLine,"C")

      EJECUTAR("MSGMEMO",MI("Tabla ",705)+" ["+cNomTab+"] "+MI(" No está Registrada en el Diccionario [Checktable()]",709)+CRLF+"DpXbase:"+oDp:cDpXBaseRun+"/"+oDp:cDpXbaseLine+CRLF+GETPROCE())

      RETURN .F.
   ENDIF

// Primero busca por Nombre

   IF !IsDigit(cNumTab)

     // Tabla en Formato de Caracter
     nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == cNumTab }) // Necesito el Dsn

     IF nAt>0
       cNumTab:=aTablas[nAt,1]
     ENDIF

   ELSE

     // Busca la Tabla por nombre
     nAt     :=ASCAN(aTablas,{|aVal| aVal[1] == cNumTab }) // Necesito el Dsn

     IF nAt=0
       // Tabla no está Registrada
       MensajeErr("Número de Tabla "+cNumTab+" No está Registrada en el Diccionario [Checktable()]")
       RETURN .F.
     ENDIF

   ENDIF

   // oDp:lTracer:=.T.
   // JN 20/11/2020, se bloquea con los arreglos
   oDp:aStruct:={}

   SysRefresh(.T.)
//  oDp:oFrameDp:SetText("aStruct con ASQL "+cNomTab)

/*
// JN 20/11/2020
   aStructs:=EJECUTAR("LOADSTRUCT",cNumTab)
   SysRefresh(.T.)
   nContar :=1
   aStruct:=aStructs[nContar,2]
*/

   aStruct:=EJECUTAR("MYLOADSTRUCT",cNomTab)

 /*
   DEFAULT oDp:aFields2:={}

   IF Empty(oDp:aFields2)

      oDp:aFields2:=ASQL(" SELECT CAM_NAME,CAM_TYPE,CAM_LEN,CAM_DEC,CAM_COMMAN,RTRIM(CAM_TABLE) AS CAM_TABLE "+;
                         " FROM DPCAMPOS ORDER BY CAM_TABLE ")


   ENDIF

   aStruct:={}
   AEVAL(oDp:aFields2,{|a,n| IF( ALLTRIM(a[6])==cNumTab,AADD(aStruct,a),NIL)})
*/

// ASQL(" SELECT CAM_NAME,CAM_TYPE,CAM_LEN,CAM_DEC,CAM_COMMAN "+;
//                 " FROM DPCAMPOS WHERE CAM_TABLE "+GetWhere("=",cNumTab))

//ViewArray(aFields)

   // Busca Nuevamente la estructura
/*
   IF Empty(aStruct)
     aStructs:=EJECUTAR("LOADSTRUCT",cNumTab,NIL,NIL,.T.)
     aStruct:=aStructs[nContar,2]
   ENDIF
*/
  // Busca Nuevamente la estructura
/*
   IF Empty(aStruct)
     aStructs:=EJECUTAR("LOADSTRUCT",cNumTab,NIL,NIL,.T.,.T.)
     aStruct:=aStructs[nContar,2]
   ENDIF
*/

   cTabla :=aTablas[nAt,2]

//? "PARAMETRO",cDsn,"CHECKTABLE",cNumTab,lDrop,lDroptable,lIni,lSay,cDsn,"<-cDsn"

   IF nAt>0

     cDsn:=GETDSN(cTabla)

     DEFAULT cDsn    :=aTablas[nAt,3]  // JN 25/05/2018 (Viene por Parámetro

   ENDIF

//? cDsn,"cDsn",cTabla,"cTabla"

   DEFAULT oOdbc   :=OpenOdbc(cDsn)

//? ValType(oOdbc)

   cSep    :=IIF(oOdbc:cType$"ADT.ADS.DBF",CRLF,",") // Advantage Nativo o DBF Requiere MEMO
   nContar :=1

// ViewArray(aStructs)

   IF ValType(oOdbc)!="O"
      MsgAlert(MI("No hay Conexión con ",710)+cDsn)
      RETURN .F.
   ENDIF

   IF .T.

     IF Empty(aStruct) // aStructs=NIL .OR. LEN(aStructs)=0
       oDp:oFrameDp:SetText(cNomTab+" no posee Estructura")
       MensajeErr(MI("Tabla ",705)+cNomTab+ MI(" No posee estructura en el Diccionario de Datos [Checktable() ]",711))
       RETURN .F.
     ENDIF

// 20/11/2020
//     aStruct:=aStructs[nContar,2]
//     aIndex :=aStructs[nContar,3]

     IF LEN(aStruct)=0
       oDp:oFrameDp:SetText(cNomTab+" no posee Estructura")
       MensajeErr(MI("Tabla ",705)+cNomTab+MI(" No posee estructura [Checktable()]",711))
       RETURN .F.
     ENDIF

// 20/11/2020
//     IF !EJECUTAR("ISTABLE",cDsn,cTabla)
    IF !oOdbc:File(cTabla)

        EJECUTAR("DPCREATETABLE",cTabla,oOdbc,aStruct)
        oOdbc:Use()
        oOdbc:CreateTableDp(cTabla,aStruct,oDp:cDbEngine)

        SysRefresh(.T.)

     ELSEIF lDropTable .AND. COUNT(cTable)=0

         // JN 23/09/2014
         oOdbc:Execute("DROP TABLE "+cTable,"DROP",lSay)

     ENDIF

     IF !oOdbc:File(cTabla) .OR. !oOdbc:File(uppe(alltrim(cTabla)))

  //      oDp:oFrameDp:SetText(cNomTab+" creando ")
        EJECUTAR("DPCREATETABLE",cTabla,oOdbc)
        oOdbc:Use()
        oOdbc:CreateTableDp(cTabla,aStruct,oDp:cDbEngine)

        SysRefresh(.T.)

        IF oOdbc:File(cTabla) .OR. oOdbc:File(uppe(alltrim(cTabla)))
           RETURN .T.
        ENDIF

     ENDIF

     cSql   :="SELECT * FROM "+cTabla+" LIMIT 1"
     cSql   :=BuildLimit(cSql,oOdbc:cType)

// oDp:oFrameDp:SetText(cSql)

     // ErrorSys(.T.)
     // oDp:oFrameDp:SetText("Lectura de "+cTabla+" "+cDsn)

     aFields:=EJECUTAR("MYSQLSTRUCT",cTabla)

/*
// 20/11/2020
     IF oDp:lNativo

       nT1:=SECONDS()

       // No soportada por la Lib EAGLE para MySQL5
       // oOrigen:=MySqlTable():New(cSql,.F.,cDsn)
       // Requiere Agregar la tabla en el Diccionario
       oOrigen:=OpenTable(cSql,.F.)
       aFields:=oOrigen:aFields

     ELSE

       oOrigen:=oOdbc:Query(cSQL)
       aFields:=oOrigen:CursorFields( oOrigen:hStmt )

     ENDIF

*/

     IF ValType(aFields)<>"A"
         MensajeErr(MI("Tabla ",705)+cTabla+MI(" sin Estructura en la Base de Datos",712))
         aFields:={}
     ENDIF

     FOR I=1 TO LEN(aStruct)

        IF aStruct[I,2]="L"
          nAt:=ASCAN(aFields,{|a,n|a[1]=aStruct[I,1]})

          IF nAt>0
             aFields[nAt,2]= aStruct[I,2]
          ENDIF

        ENDIF
     NEXT I

     AEVAL(aFields,{|a,i|aFields[I,1]:=UPPE(aFields[I,1])})

     cMemo  :=""
     IIF(ValType(oOrigen)="O",oOrigen:End(),NIL)
     oOrigen:=NIL

     IF oOdbc:cType$"ADT.ADS.DBF" // Advantage Nativo o DBF Requiere MEMO
        ModyStruct(oOdbc:cPath+cTabla,aStruct)
        RETURN NIL // EXIT 28/01/2013 // ? "ADS LO HACE COMO MS-SQL"
     ENDIF

     // Campos Blob
     AEval(aFields,{|a,i|IIF( a[2]="B",aFields[I,3]:=0,NIL)}) // Blob

     AEVAL(aFields,{|a,i|cMemo:=cMemo+a[1]+" "+a[2]+" "+STR(a[3])+" "+STR(a[4])+CRLF})

     AEval(aFields,{|a,i|IIF( a[2]="M",aFields[I,3]:=10,NIL)}) // Memo es Igual a 10

     IF oDp:lNativo

        // Nativo con dos decimales viene con Entero -1
        // Antes estaba asi: AEval(aFields,{|a,i|IIF( a[4]>0  ,aFields[I,3]++,NIL)})   // Numericos con Decimales le Resta 1
         FOR I=1 TO LEN(aFields)


            // aFields ESTRUTURA DE LA TABLA FISICA
            // aStruct DICCIONARIO  DE DATOS

            IF aFields[I,4]>0 .AND. aFields[I,3]<15 .AND. aFields[I,3]<>19
              aFields[I,3]:=aFields[I,3]+aFields[I,4]- 2
            ENDIF

            //IF aFields[I,4]>0 .AND. aFields[I,3]>15 .AND. aFields[I,3]<>19               aFields[I,3]:=aFields[I,3]+2
            // ENDIF

            nAt:=ASCAN(aStruct,{|a,n| RTRIM(a[1])==RTRIM(aFields[I,1])})

// ? nAt, aFields[I,1],"Encontrado"

            // Con cinco decimales, viene con Cuatro
            IF aFields[I,4]=4 .AND. nAt>0 .AND. aStruct[nAt,4]=5
                aFields[I,4]:=aFields[I,4]+1
            ENDIF

            // JN 01/06/2016
            IF aFields[I,2]="L"
               aFields[I,2]:=1
            ENDIF

//         AEval(aFields,{|a,i|IIF( a[4]>0 , aFields[I,3]:=aFields[I,3]+aFields[I,4]- 2 ,NIL )})   // Numericos con Decimales le Resta 1
         NEXT I

     ENDIF

//ViewArray(aFields)
// ViewArray(aStruct)

     // Busca los que seran Borrados
     FOR I := 1 TO LEN(aFields) // Campos actuales

        nAt   :=ASCAN(aStruct,{|aVal| ALLTRIM(aVal[1]) == ALLTRIM(aFields[I,1]) })

        // MySql Nativo, Memo Viene con 0 en Len por eso ALTER TABLE
        IF nAt>0 .AND. aStruct[nAt,2]="M"
           aStruct[nAt,3]=10
        ENDIF

        // Validar Claves Primaria

        IF oDp:lNativo .AND. nAt>0 .AND. !Empty(aStruct[nAt,5]) .AND. oOdbc:IsPrimary(cTabla,aStruct[nAt,1])
          aStruct[nAt,5]:=""
        ENDIF

        IF nAt=0 // este Campo no está en la nueva lista hay que sacarlo

              AADD(aFieldsDel,aFields[I,1])

         ELSEIF (len(aFields)=len(aStruct)) .AND. ;
                !(aFields[I,2]=aStruct[nAt,2]  .AND.;
                  aFields[I,3]=aStruct[nAt,3]  .AND.;
                  aFields[I,4]=aStruct[nAt,4]) .AND.;
                !(aFields[I,2]="D" .AND. aStruct[nAt,2]="D") .OR. !Empty(aStruct[nAt,5]) .OR. aFields[I,2]="L"


            AADD(aFieldsChange,aStruct[nAt])

        ENDIF

    NEXT

    FOR I := 1 TO LEN(aStruct) // Campos Nuevos

        nAt   :=ASCAN(aFields,{|aVal| ALLTRIM(aVal[1]) == ALLTRIM(aStruct[I,1]) })
        IF nAt=0 // este Campo no está en la nueva lista hay que sacarlo
           AADD(aFieldsNew,aStruct[I])
        ENDIF

    NEXT I

// oOrigen:End()
// oOrigen:=NIL
// ERRORSYS(.T.)

    cSql:=""

    FOR I := 1 TO LEN(aFieldsDel)
       cSql:=cSql+IIF( EMPTY(cSql),"" ,"," )+" DROP "+aFieldsDel[I]

       IF lArray .OR. oOdbc:cType$"MSSQL,ADS,DBF"
          AADD(aSql,cSql)
          cSql:=""
       ENDIF

    NEXT

    FOR I := 1 TO LEN(aFieldsNew)

       cSQL:=cSQL+IIF( !empty(cSQL),cSep ,"" )

       cType:=""
       cType:=IIF( aFieldsNew[I,2]="C"," Char("     ,cType )
       cType:=IIF( aFieldsNew[I,2]="M"," longText(" ,cType )
       cType:=IIF( aFieldsNew[I,2]="B"," longblob(" ,cType )
/*
       IF !oOdbc:cType$"ADT.ADS.DBF"
          cType:=IIF( aFieldsNew[I,2]="L"," Bit("      ,cType )
       ELSE
          // MySql
          cType:=IIF( aFieldsNew[I,2]="L"," Numeric("      ,cType )
       ENDIF
*/

       cType:=IIF( aFieldsNew[I,2]="L"," Numeric("      ,cType )
//       cType:=IIF( aFieldsNew[I,2]="L"," Bit("      ,cType )    // 20/03/2016
       cType:=IIF( aFieldsNew[I,2]="D"," Date("     ,cType )
       cType:=IIF( aFieldsNew[I,2]="N"," Numeric("  ,cType )
       cType:=IIF( aFieldsNew[I,2]="I"," Int("  ,cType )


       // 07/03/2015
       IF (aFieldsNew[I,2]="N" .AND. "AUTO_INCRE"$aFieldsNew[I,5]) .OR. aFieldsNew[I,2]="I"
          cType:="INT("
       ENDIF

       IF oOdbc:cType$"ADT.ADS.DBF" // Advantage Nativo o DBF Requiere MEMO
          cType := STRTRAN(cType," Text"    ," Memo"   )
          cType := STRTRAN(cType," Date"    ," Date"   )
          cType := STRTRAN(cType," Bit"     ," Logical")
          aFieldsNew[I,5]:=""
       ENDIF

       cDec :=IIF( aFieldsNew[I,4]>0,","+ALLTRIM(STR(aFieldsNew[I,4])) ,"" )
       cLen :=IIF( "Text"$cType .OR. "Date"$cType ,"", ALLTRIM(STR(aFieldsNew[I,3])))

       cSql :=cSql+" ADD "+ALLTRIM(aFieldsNew[I,1])+" "+cType+cLen+cDec+")  " +ALLTRIM(aFieldsNew[I,5])
       cSql :=STRTRAN(cSql,"()","")

       IF lArray .OR. oOdbc:cType$"MSSQL,ADS,DBF"
          AADD(aSql,cSql)
          cSql:=""
       ENDIF

    NEXT

    oDp:aView:={aFields,aStruct,cSql}

    FOR I := 1 TO LEN(aFieldsChange)

       cSQL:=cSQL+IIF( !empty(cSQL) , cSep , "" )

       // JN 01/06/2016
       IF aFieldsChange[I,2]="L"
          aFieldsChange[I,3]=1 // 1 Entero
          aFieldsChange[I,4]=0 // Cero Decimal
       ENDIF

       cType:=""
       cType:=IIF( aFieldsChange[I,2]="C"," Char("     ,cType )
       cType:=IIF( aFieldsChange[I,2]="M"," Longtext(" ,cType )
       cType:=IIF( aFieldsChange[I,2]="B"," Longblob(" ,cType )
       // cType:=IIF( aFieldsChange[I,2]="L"," Bit("      ,cType )    // 20/03/2016
       cType:=IIF( aFieldsChange[I,2]="L"," Numeric("  ,cType )  // 20/03/2016
       cType:=IIF( aFieldsChange[I,2]="D"," Date("     ,cType )
       cType:=IIF( aFieldsChange[I,2]="N"," Numeric("  ,cType )
       cType:=IIF( aFieldsChange[I,2]="I"," Int("      ,cType )

//       IF "ADT"$oOdbc:cType.OR."DBF"$oOdbc:cType.OR."ADS"$oOdbc:cType // Advantage Nativo o DBF Requiere MEMO
       IF oOdbc:cType$"ADT.ADS.DBF" // Advantage Nativo o DBF Requiere MEMO
          cType := STRTRAN(cType," Text"    ," Memo"   )
 //       cType := STRTRAN(cType," Datetime"," Date"   )
          cType := STRTRAN(cType," Bit"     ," Logical")
          aFieldsChange[I,5]:="" // aFieldsNew[I,5]:=""
   //       cExt  := ""
       ENDIF

       cDec :=IIF( aFieldsChange[I,4]>0,","+ALLTRIM(STR(aFieldsChange[I,4])) ,"" )
       cLen :=IIF( "Text"$cType .OR. "Date"$cType ,"", ALLTRIM(STR(aFieldsChange[I,3])))

       // Campos, BLOB
//       IF aFieldsChange[I,2]="B"
//         cDec:=""
//         cLen:=""
//       ENDIF

       DEFAULT aFieldsChange [I,5]:=""

/*
IF !Empty(aFieldsChange) .AND. cTabla="DPGUIATRANSP"

  ViewArray(aFieldsChange,,,.F.)

  ? "a1->"+aFieldsChange[I,1],"a2->"+ValType(aFieldsChange[I,1])
  ? cType,cLen,cDec, "5->"+Valtype(aFieldsChange[I,5])

ENDIF
*/

       // ? cLen,cType,ALLTRIM(STR(aFieldsChange[I,3]))

     //cSql:=cSql+" CHANGE "+aFieldsChange[I,1]+" "+ALLTRIM(aFieldsChange[I,1])+" "+cType+ALLTRIM(STR(aFieldsChange[I,3]))+cDec+") "+ALLTRIM(aFieldsChange[I,5])

       IF oOdbc:cType="MSSQL" // Advantage Nativo o DBF Requiere MEMO

         IF aFieldsChange[I,1]<>ALLTRIM(aFieldsChange[I,1])
            cSql :=cSql+" CHANGE "+aFieldsChange[I,1]+" "+ALLTRIM(aFieldsChange[I,1])+" "+cType+cLen+cDec+")  " + ALLTRIM(aFieldsChange[I,5])
         ELSE
            cSql :=cSql+" CHANGE "+aFieldsChange[I,1]+" "+cType+cLen+cDec+")  " + ALLTRIM(aFieldsChange[I,5])
         ENDIF

//         cSql :=cSql+" CHANGE "+aFieldsChange[I,1]+" "+ALLTRIM(aFieldsChange[I,1])+" "+cType+cLen+cDec+")  " + ALLTRIM(aFieldsChange[I,5])
       ELSE

         cSql :=cSql+" CHANGE "+aFieldsChange[I,1]+" "+ALLTRIM(aFieldsChange[I,1])+" "+cType+cLen+cDec+")  " + ALLTRIM(aFieldsChange[I,5])

       ENDIF

       IF lArray .OR. oOdbc:cType$"MSSQL,ADS,DBF"
          AADD(aSql,cSql)
          cSql:=""
       ENDIF

//       IF !EMPTY(aFieldsChange[I,5])
//         ? I,aFieldsChange[I,5],cSql
//       ENDIF

       cSql :=STRTRAN(cSql,"()","")
       IF oOdbc:cType$"ADT.ADS.DBF.MSSQL" // Advantage Nativo o DBF Requiere MEMO
          cSql:=STRTRAN(cSql," CHANGE ", " ALTER COLUMN ")
       //   cSql:=STRTRAN(cSql,",",CRLF)
          cSql:=UPPE(cSql)
       ENDIF

    NEXT

//    IF EMPTY(aSql) .OR. oOdbc:cType="MYSQL"
//       aSql:={cSql}
//    ENDIF

    FOR I := 1 TO LEN(aSql)

      cSql:=aSql[I]
      cSql:=STRTRAN(cSql,"(0)","")
      cSql:=STRTRAN(cSql,"()","")
      // Cambio de Text por Long Text
      cSql:=STRTRAN(cSql," text("," longtext(")

      // jn 15/02/2015
      cSql:=STRTRAN(cSql,"Longtext(10)","Longtext")

      IF !EMPTY(cSql)

        cSql:=UPPE(cSql)

        cSql:="ALTER TABLE "+cTabla+" "+cSql // " DROP PRIMARY KEY, "+cSql

       IF oOdbc:cType$"ADS"

         cSql:=STRTRAN(cSql," ADD "," ADD COLUMN ")

       ENDIF

       oDp:cMsgOdbc:=""

       IF "ALTER TABLE"$cSql .AND. "PRIMARY KEY"$cSql .AND. oDp:cTypeBD="MSSQL"
          aIndex:=oOdbc:GetIndex( cTabla )
          IF ASCAN(aIndex,{|a,n|UPPE(cTabla)$UPPE("PK__"+cTabla)})>0 // Clave Primaria
             cSql:=""
          ENDIF
       ENDIF

       // MYSQL- SI YA EXISTE NO LO CREA
       IF "ALTER TABLE"$cSql .AND. "PRIMARY KEY"$cSql .AND. oDp:cTypeBD="MYSQL"

          aIndex:=oOdbc:GetIndex( cTabla )

          IF ASCAN(aIndex,{|a,n|UPPE(a)$"PRIMARY"})>0 // Clave Primaria
             cSql:=""
          ENDIF

       ENDIF

       IF oOdbc:cType="MSSQL"

         cSql:=STRTRAN(cSql," ALTER COLUMN "," MODIFY COLUMN ")
         cSql:=STRTRAN(cSql," CHANGE "," MODIFY COLUMN ")
         cSql:=STRTRAN(cSql," DATE "," DATETIME ")
         cSql:=STRTRAN(cSql," DROP "," DROP COLUMN ")

       ENDIF

       cSql:=ALLTRIM(cSql)
       oDp:cMsgOdbc:=""

       IF !lDrop .AND. " DROP "$cSql
         // No puede Borrar Campos, Desde Reinstall
         LOOP
       ENDIF

       IF !Empty(cSql)

           oDp:cMemoSql:=oDp:cMemoSql+CRLF+" DB: "+cDsn+" "+DTOC(oDp:dFecha)+" "+TIME()+CRLF+cSql

           DPWRITE("SQLTRAZA\"+cDsn+"_"+cTabla+".SQL",DTOC(oDp:dFecha)+" "+TIME()+CRLF+cSql)

           lIndex:=EJECUTAR("BEFOREALTERTABLE",cTabla) // Antes de Modificar la tabla debe Remover los Indices, PK, FK

           DPWRITE("TEMP\ALTERTABLE_"+cTabla+".SQL",cSql)

 //          oDp:oFrameDp:SetText("ALTER TABLE en "+cTabla)

           IF oOdbc:ClassName()="TODBC"
              lResp:=oOdbc:Execute( cSql,"ALTER" )
           ELSE
              lResp:=oOdbc:Execute( cSql,"ALTER",lSay )
           ENDIF

           oDp:cSql:=cSql
           DPWRITE("TEMP\ALTERTABLE_"+cTabla+".SQL",cSql)

           IF lResp .AND. lIndex
              EJECUTAR("AFTERALTERTABLE",cTabla) // Antes de Modificar la tabla debe Remover los Indices, PK, FK
           ENDIF

           lAlter:=.T.

       ENDIF

       oDp:aView:={aFields,aStruct,cSql}

       IF !Empty(oDp:cMsgOdbc)

          cMySqlErr:=IF(oOdbc:cType="MYSQL",oOdbc:oConnect:oError:GetError(),"")

          IF lSay
             MensajeErr(cSql+CRLF+cMySqlErr,oDp:cMsgOdbc) //,cSql
          ENDIF

          dpwrite("ERR"+STRTRAN(TIME(),":","")+".SQL",cSql)

       ENDIF


    ENDIF

    NEXT

   ENDIF

   IF lAlter
     // EJECUTAR("SET   RECNULOS",cTabla)
   ENDIF

   SysRefresh(.T.)

   oOdbc:=NIL

   oDp:lOpenTableChk:=lOpenTableChk // 20/11/2020 Restaura revision de la tabla.

  // oDp:oFrameDp:SetText("Concluido CheckTable")
   aStruct:=NIL
   aFieldsChange:=NIL

RETURN lResult

FUNCTION ABRIR()
   ALERT(MI("ABRIR NO PARA SQL",13))
RETURN NIL

FUNCTION GetTables()

   // IF !oDp:lLoadTablas
   //    aTablas:={}
   //    RETURN aTablas
   // ENDIF

   // ? LEN(aTablas),"LEN(aTablas)"

   IF EMPTY(aTablas)
      LOADTABLAS()
   ENDIF

Return ACLONE(aTablas)

FUNCTION RELEASETABLES()
    aTablas:={}
RETURN NIL


/*
// Genera el Valor de AutoIncremento de un Campo
*/
Function IncreMental(cTable,cField,lZero)
   LOCAL oCursor,uValue,cSql

   DEFAULT lZero:=.T.

   cSql:=[SELECT MAX(]+cField+[) AS ]+cField+[  FROM ]+cTable

   oCursor:=OpenTable(cSql,.T.)

   uValue :=oCursor:aDataFill[1,1]

   IF oCursor:RecCount()=0
      uValue:=CTOEMPTY(uValue) //CTOEMPY(RETURN NIL
   ENDIF

   oCursor:End()

   IF ValType(uValue)$"ND"
      RETURN uValue++
   ENDIF

   IF ValType(uValue)$"C" .AND. !lZero .AND. AllDigit(uValue)
      RETURN uValue:=PADR(STR(VAL(uValue)+1),LEN(uValue))
   ENDIF

   IF ValType(uValue)$"C" .AND. lZero .AND. AllDigit(uValue)
      RETURN uValue:=PADR(STRZERO(VAL(uValue)+1),LEN(uValue))
   ENDIF

RETURN uValue

/*
// Carga en Memoria los Campos con sus respectivas Opciones
*/
FUNCTION LOADCAMPOSOPC()
  LOCAL oTable,oOdbc:=OpenOdbc(oDp:cDsnConfig)

  DEFAULT oDp:aCamposOpc:={}


/*
  IF Empty(oDp:aCamposOpc) .AND. !EJECUTAR("DBISTABLE",oDp:cDsnConfig,"DPCAMPOSOP",.T.)
     EJECUTAR("DPCREATEFROMTXT","DPCAMPOSOP.TXT")
     LOADTABLAS(.T.)
  ENDIF
*/

/*
  IF !oOdbc:FILE("DPCAMPOSP")
    RETURN {}
  ENDIF
*/

/*
  IF !EJECUTAR("ISTABLE",oDp:cDsnConfig,"DPCAMPOSOP")
     RETURN {}
  ENDIF
*/

/*
// jn 03/12/2013
   oOdbc:=GetODBC("DPCONFIG")

   // Aun no Existe la Tabla
   IF !oOdbc:FILE("DPCAMPOSOP")
       RETURN {}
   ENDIF
*/

//   oTable    :=OPENTABLE("SELECT OPC_TABLE,OPC_CAMPO,OPC_TITULO FROM DPCAMPOSOP ORDER BY OPC_TABLE,OPC_CAMPO",.T.)
//   oDp:aCamposOpc:=oTable:aDataFill
//   oDp:aCamposOpc:=OpenTable("SELECT OPC_TABLE,OPC_CAMPO,OPC_TITULO FROM DPCAMPOSOP ORDER BY OPC_TABLE,OPC_CAMPO"):aDataFill


// ? "LOADCAMPOSOPC",GETPROCE()

   IF Empty(oDp:aCamposOpc) .AND. COUNT("DPCAMPOSOP")=0
      EJECUTAR("DPCAMPOSOPIMP")
   ENDIF

   // IF PROCE MAIN(oDb,cTable,cField,lCreate)
/*
   IF !EJECUTAR("ISFIELDMYSQL",oDp:cDsnConfig,"DPCAMPOSOP","OPC_COLOR",.F.)
     // EJECUTAR("DPCAMPOSADD","DPCAMPOSOP","OPC_COLOR","N",10)
     oDp:aCamposOpc:=ASQL("SELECT OPC_TABLE,OPC_CAMPO,OPC_TITULO,0 AS OPC_COLOR FROM DPCAMPOSOP ORDER BY OPC_TABLE,OPC_CAMPO")
   ELSE
*/

   DEFAULT oDp:lOPC_COLOR:=EJECUTAR("ISFIELDMYSQL",oDp:cDsnConfig,"DPCAMPOSOP","OPC_COLOR",.F.)

   IF Empty(oDp:aCamposOpc)

     IF oDp:lOPC_COLOR
       oDp:aCamposOpc:=ASQL("SELECT OPC_TABLE,OPC_CAMPO,OPC_TITULO,OPC_COLOR FROM DPCAMPOSOP ORDER BY OPC_TABLE,OPC_CAMPO")
     ELSE
       oDp:aCamposOpc:=ASQL("SELECT OPC_TABLE,OPC_CAMPO,OPC_TITULO,0 AS OPC_COLOR FROM DPCAMPOSOP ORDER BY OPC_TABLE,OPC_CAMPO")
     ENDIF

   ENDIF

   // oDp:aCamposOpc:=ASQL("SELECT OPC_TABLE,OPC_CAMPO,OPC_TITULO,OPC_COLOR FROM DPCAMPOSOP ORDER BY OPC_TABLE,OPC_CAMPO")

// ViewArray(oDp:aCamposOpc)

   AEVAL(oDp:aCamposOpc,{|a,i|oDp:aCamposOpc[I,1]:=ALLTRIM(UPPE(a[1])),oDp:aCamposOpc[I,2]:=ALLTRIM(UPPE(a[2]))})

//   oTable:End()

   IF Empty(aZero)
      oTable    :=OPENTABLE("SELECT CAM_TABLE,CAM_NAME,CAM_ZERO FROM DPCAMPOS WHERE CAM_ZERO=1 AND CAM_TYPE='C' ORDER BY CAM_TABLE",.T.)
      aZero     :=oTable:aDataFill
      AEVAL(aZero,{|a,i|aZero[I,1]:=ALLTRIM(UPPE(a[1])),aZero[I,2]:=ALLTRIM(UPPE(a[2]))})
      oTable:End()
   ENDIF

   // Campos Lógicos
   oDp:aLogico:=NIL // ASQL("SELECT RTRIM(CAM_TABLE),RTRIM(CAM_NAME) FROM DPCAMPOS WHERE CAM_TYPE='L'")
   //   ViewArray(aLogico)
   //  ENDIF

RETURN oDp:aCamposOpc

/*
// Devuelve el Texto                                
*/
FUNCTION SAYOPTIONS(cTable,cField,uValue,lLow,aItems)
   LOCAL cText  :=SPACE(10),nAt,nLen

   DEFAULT lLow:=.T.

   DEFAULT aItems:=GetOptions(cTable,cField),;
           oDp:aClrOptions:={}

   IF ValType(uValue)<>"C"
      RETURN CTOO(uValue,"C")
   ENDIF

// oDp:oFrameDp:SetText(cTable,cField)

   uValue:=UPPE(ALLTRIM(uValue))
   nLen  :=LEN(ALLTRIM(uValue))

   nAt=aScan( aItems, { |a| UPPE(LEFT(a,nLen))==uValue } )

   oDp:nClrOptions:=0

   IF nAt>0

      cText:=ALLTRIM(aItems[nAt])

      IF LEN(oDp:aClrOptions)>=nAt
        oDp:nClrOptions:=oDp:aClrOptions[nAt]
      ENDIF

      // cText:=UPPE(Left(cText,1))+LOW(SUBS(cText,2,LEN(cText)))
   ELSEIF !EMPTY(aItems)

      cText:=SPACE(LEN(aItems[1]))

   ENDIF

   IF Empty(cText)
      cText:=uValue
   ENDIF

RETURN " "+cText

/*
// Optiene las Opciones de los Campos, Según Tabla y Campo
*/
FUNCTION GETOPTIONS(cTable,cField,lSort,lNinguno)
   LOCAL aValues:={},nAt:=0

   DEFAULT lSort   :=.F.,;
           lNinguno:=.F.

   IIF( EMPTY(oDp:aCamposOpc),   LoadCamposOpc() ,NIL )

    // Color de las Opciones

   cTable:=ALLTRIM(UPPE(cTable))
   cField:=ALLTRIM(UPPE(cField))

   oDp:aClrOptions:={}

   nAt:=ASCAN(oDp:aCamposOpc,{|a,n|a[1]==cTable .AND. a[2]==cField })

   // JN 03/08/2019, en el caso que no se encuentre el registro, recarga los datos
   IF nAt=0
      //
      oDp:aCamposOpc :={}
      LoadCamposOpc()
      nAt:=ASCAN(oDp:aCamposOpc,{|a,n|a[1]==cTable .AND. a[2]==cField })
// ? "AQUI RECARGA LA TABLA Y POSIBLEMENTE SE QUEDA COLGADO",cTable,cField
// ViewArray(oDp:aCamposOpc)
      RETURN {}
   ENDIF

//? nAt,ValType(oDp:aCamposOpc),"oDp:aCamposOpc"

   WHILE nAt>0 .AND. LEN(oDp:aCamposOpc)>=nAt .AND. oDp:aCamposOpc[nAt,1]==cTable .AND. oDp:aCamposOpc[nAt,2]==cField
      AADD(aValues        ,ALLTRIM(oDp:aCamposOpc[nAt,3]))
      AADD(oDp:aClrOptions,oDp:aCamposOpc[nAt,4])
      nAt++
   ENDDO

   IF lSort
      ASORT(aValues)
   ENDIF

   IF Empty(aValues) .AND. lNinguno
      AADD(aValues,MI("Ninguno",714))
      AADD(oDp:aClrOptions,0)
   ENDIF

RETURN aValues

/*
// necesario para optimizar por campo  oCol:aOpcColor 28/02/2025
*/
FUNCTION GETOPCCOLOR(cTable,cField,lSort,lNinguno)
  LOCAL aItems:=GETOPTIONS(cTable,cField,lSort,lNinguno)
  LOCAL aNew  :={}

  AEVAL(aItems,{|a,n| AADD(aNew,{ALLTRIM(a),oDp:aClrOptions[n]}) })

RETURN aNew

/*
// Elimina Registro de la Tabla
*/
FUNCTION DELETESQL(cTable,cWhere,oOdbc)
RETURN SQLDELETE(cTable,cWhere,oOdbc)
/*  LOCAL cSql

  DEFAULT oOdbc  :=GetOdbc(cTable)

  cSql:="DELETE FROM "+cTable+IIF( "WHERE "$UPPE(cWhere)," "," WHERE ")+cWhere

  // IF oDp:lSqlTracer
  //  ? cSql
  //  ENDIF
  //? cSql
  oOdbc:Execute(cSql)

RETURN .T.
*/


/*
// Creación de Tablas Temporales
*/
FUNCTION SQLCREATEMPO(cName,cTable,cWhere,cDsn,lInstance)
// RETURN MensajeErr("Funcion  SQLCREATEMPO removida")


    LOCAL nContar:=0,cTempo:=cName,cSql,cSelect,oOdbc,nIntentos:=0,nAt,nVeces:=100
    LOCAL cFile,oTable

    DEFAULT cDsn     :=oDp:cDsnData
    DEFAULT cWhere   :=" WHERE 1=0"
    DEFAULT cTable   :=cName // Temporal de Si Mismo
    DEFAULT nVeces   :=10
    DEFAULT lInstance:=.F. // Puede hacer Varios Temporales

    oOdbc  :=OPenOdbc(cDsn) // Apertura del DSN
    cTempo :=ALLTRIM(UPPE(cTempo))

    nAt:=ASCAN(aTablas,{|a,i|a[1]="TEMPO" .AND. a[2]=cName})
    nAt:=IIF(oOdbc:cType$"ADS,DBF",0,nAt)

    IF nAt>0 .AND. !lInstance // Ya Existe
       RETURN cName // aTablas[nAt,2]
    ENDIF

    IF !"*"$cTable
       cSelect:="SELECT * FROM "+cTable
    ELSE
       cSelect:=cName // Table
    ENDIF

    IF oOdbc:cType$"ADS,DBF"

       cFile :=oOdbc:cPath+cTempo+".DBF"
       cTempo:=cFileNoExt(cFileNoPath(cFile))

       IF !FILE(cFile)
          oTable :=OpenTable(cSelect,.F.)
          DBCREATE(cFile,oTable:aFields,"DBFCDX")
       ELSE
          oOdbc:Execute("DELETE FROM "+cTempo)
       ENDIF

    ELSEIF !oDp:lTemporal // CREA PRENOMINA COMO TEMPORAL

      IF ASCAN(aTablas,{|a,i|a[2]=cTempo})=0
         AADD(aTablas,{"TEMPO",cTempo,cDsn,"","",NIL})
//       ? "agregado en atablas",cTempo,LEN(aTablas)
      ENDIF

//      ? "aqui, tamporal",cTempo

      IF oOdbc:File(cTempo)
  //       ? "si existe",cTempo
         RETURN cTempo
      ENDIF

      oTable:=OpenTable("SELECT * FROM "+cTable,.F.)
      oOdbc:CREATE(cTempo,oTable:aFields)
      oTable:End()

      RETURN cTempo

    ENDIF

    WHILE nVeces>nIntentos++ .AND.;
          !oOdbc:cType$"ADS,DBF"  // Cantidad de Intentos

  //     ? cTempo,"cTempo",oOdbc:File(cTempo)

       IF .F.  .AND. oOdbc:File(cTempo) // Evita Repetir Nombre del Mismo

            cTempo:=cName+STRZERO(nContar++,2)
//          ? cTempo,"cTempo,SQLTEMPO"

          LOOP

       ENDIF

       cSql:="CREATE TEMPORARY TABLE IF NOT EXISTS "+cTempo+" "+cSelect+" "+cWhere

       IF !oOdbc:Execute(cSql,cSql) // Repite la Petición
         SysRefresh()
//         cTempo:=cName+STRZERO(nContar++,2)
         LOOP
       ENDIF

       SysRefresh()

       EXIT

    ENDDO

    cTempo:=ALLTRIM(UPPE(cTempo))
    nAt   :=ASCAN(aTablas,{|a,i|a[2]=cTempo})

    IF nAt=0
      AADD(aTablas,{"TEMPO",cTempo,cDsn,"","",NIL})
    ENDIF

RETURN cTempo


/*
// Borra los Registros de una Tabla
*/
FUNCTION SQLZAP(cName)
    LOCAL nAt,cDsn,oOdbc,cSql

    nAt:=ASCAN(aTablas,{|a,i|a[2]=cName})

    IF nAt=0
       RETURN .F.
    ENDIF

    cSql   :="DELETE FROM "+cName+" WHERE 1=1" // +" WHERE HIS_VARIAC=0"
    oOdbc  :=OPENODBC(oDp:cDsnData) // aTablas[nAt,3]) // Apertura del DSN

    IF EMPTY(oOdbc:QueryData( "SELECT COUNT(*) FROM "+cName))
       RETURN .T.
    ENDIF

    IF !oOdbc:File(cName)
       MensajeErr(MI("Tabla ",705)+cName+MI(" no existe en la Base de Datos: ",715)+oDp:cDsnData)
       RETURN .F.
    ENDIF

    oDp:cMsgOdbc:=""
    oOdbc:Execute(cSql)

    IF !EMPTY(oOdbc:QueryData( "SELECT COUNT(*) FROM "+cName)) .AND. ;
       !oOdbc:Execute(cSql) // Repite la Petición
       MensajeErr(oDp:cMsgOdbc+CRLF+cSql,"ERROR:NO SQLZAP") // ,cSql
       RETURN .F.
    ENDIF

RETURN .T.

/*
// Eliminar Tablas Temporales
*/
FUNCTION SQLDROPTEMPO(cName)
    LOCAL nAt,cDsn,oOdbc,cSql

    nAt:=ASCAN(aTablas,{|a,i|a[1]="TEMPO" .AND. a[2]=cName})

    IF nAt=0
      RETURN .F.
    ENDIF

    cSql   :="DROP TABLE "+cName
    oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN
    IF !oOdbc:Execute(cSql,cSql) // Repite la Petición
       RETURN .F.
    ELSE
      ADEL(aTablas,nAt)
      ASIZE(aTablas,LEN(aTablas)-1)
    ENDIF

RETURN .T.

/*
// Agrega Tablas en la Lista
*/
FUNCTION ADDTABLE(cNum,cTable,cDsn,lConfig)
   LOCAL nAt

   DEFAULT aTablas:={},lConfig:=.F.

   nAt:=ASCAN(aTablas,{|a,n| UPPER(a[2])=UPPER(cTable)})

   IF nAt=0

     AADD(aTablas,{cNum  ,; //1
                   cTable,; //2
                   cDsn  ,; //3
                   ""    ,; //4
                   ""    ,; //5
                   NIL   ,; //6
                   ""    ,; //7
                   ""    ,; //8
                   NIL   ,; //9
                   lConfig})
   ELSE

     aTablas[nAt,3]:=cDsn

   ENDIF

 RETURN aTablas

// SELECT CONCAT( 'DELETE FROM
// DPCAMPOS WHERE DPCAMPOS.CAM_NUMTAB = ', DPTABLAS.TAB_NUMERO, ';') FROM DPCAMPOS LEFT JOIN DPTABLAS on CAM_NUMTAB = TAB_NUMERO WHERE
// DPTABLAS.TAB_APLICA='01'
/*
FUNCTION ClearTablas()
//  aODBC  :={}
  aTablas:={}
//  aDsn   :={}
  aLink  :={}
  oDp:aCamposOpc:={}
RETURN .T.
*/
/*
// Determina si una Tabla Existe
*/
FUNCTION ISTABLA(cTabla,lChkDsn)
 LOCAL nAt,oOdbc

 DEFAULT aTablas :={},cTabla:="",lChkDsn:=.F.

 cTabla:=ALLTRIM(UPPE(cTabla))
 nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == cTabla })

 IF nAt>0
     // Ahora lo Busca en el Dsn
     oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN
     RETURN oOdbc:File(cTabla)
 ENDIF

 // nAt     :=ASCAN(aTablas,{|aVal| UPPE(ALLTRIM(aVal[2])) == UPPE(ALLTRIM(cTable)) })

RETURN nAt>0

/*
// Obtiene el Nombre de la Tabla
*/
FUNCTION GETTABLENAME(cTabla)
   LOCAL cName:=cTabla,nAt

   cTabla:=UPPE(ALLTRIM(cTabla))
   nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == cTabla })

   IF nAt>0
      cName:=aTablas[nAt,7]
   ENDIF

RETURN cName

/*
// Campos con Zero Izquierda
*/
FUNCTION FIELDZERO(cTable,cField)
LOCAL nAt:=0

  cTable:=UPPE(ALLTRIM(cTable))
  cField:=UPPE(ALLTRIM(cField))
  nAt   :=ASCAN(aZero,{|a,i|aZero[I,1]==cTable .AND. aZero[I,2]==cField})

RETURN nAt>0
/*
//
*/
FUNCTION ModyStruct(cFile,aStruct)
   ? "DEBE CERRAR, LOS FORMULARIOS",cFile
  CloseAllDpLbx() // Cierra todos los Browses
  CloseAllDsn()
  ? "CERRO el Dsn"
RETURN .T.


/*
// Asigna Nuevo Dsn para determinada Tabla
*/
FUNCTION PUTDSN(cTabla,cNewName,cDsn,cFields)

   LOCAL nAt:=IIF(EMPTY(aTablas),LOADTABLAS(),NIL)
   LOCAL aFields:={},I

   DEFAULT cFields:=""

   aFields:=_VECTOR(cFields)

   FOR I := 1 TO  LEN(aFields)
      aFields[I]:=_Vector(aFields[I],"=")
   NEXT

   IF Empty(cNewName)
      cNewName:=cTabla
   ENDIF

   nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

   IF nAt>0

      aTablas[nAt,3]:=cDsn

      IF !cNewName=cTabla
        aTablas[nAt,09]:=cNewName
        aTablas[nAt,11]:=ACLONE(aFields)
      ENDIF

   ENDIF

RETURN nAt>0
/*
// Busca si un Existe el Campo en una Tabla
*/
FUNCTION ISFIELD(cTable,cField,lRelease)
   LOCAL nAt:=0,aStruct:={},oTable,cDsn

   DEFAULT aFields :={},;
           lRelease:=.F.

   IF lRelease
     aFields:={}
   ENDIF

   // la tabla no Existe, ni muesra Mensaje y ni Descarga
   cDsn:=GETDSN(cTable,.F.,.F.)

   IF Empty(cDsn)
      EJECUTAR("DBISTABLE",NIL,cTable,.T.)
      cDsn:=GETDSN(cTable,.F.,.T.)
   ENDIF

   IF Empty(cDsn)
      RETURN .F.
   ENDIF

   cTable:=UPPE(ALLTRIM(cTable))
   cField:=UPPE(ALLTRIM(cField))

//   nAt:=ASCAN(aFields,{|a,n|cTable=a[1]})
//   IF nAt=0

   aStruct:=EJECUTAR("MYSQLSTRUCT",cTable)
   nAt    :=ASCAN(aStruct,{|a,n| ALLTRIM(a[1])=cField })

   // oTable:=OpenTable("SELECT * FROM "+cTable , .F.)
   // nAt:=oTable:FieldPos(cField)
   // oTable:End()

//     AEVAL(oTable:aFields,{|a,n| AADD(aStruct,UPPE(a[1])) })
//     AADD(aFields,{cTable,aStruct})
//     nAt:=LEN(aFields)
//   ENDIF

//   nAt:=ASCAN(aFields[nAt,2],cField)

RETURN nAt>0

PROCEDURE SETDSNTABLE(cDsn)
   LOCAL I

   aTablas:=GetTables()

   FOR I=1 TO LEN(aTablas)
       IF !aTablas[I,10]
          aTablas[I,3]:=cDsn
       ENDIF
   NEXT I

   oDp:cDsnData:=cDsn

RETURN

/*
// Obtiene la Mascarca desde la Tabla DPCAMPOS
// Si no está en el Arreglo no debe Buscarlo, sino el sistema sera Lento
*/
FUNCTION DPGETPICTURE(cTable,cField)
   LOCAL nAt,cPicture:=NIL

   DEFAULT oDp:aPicture:={}

   cTable:=ALLTRIM(UPPER(cTable))
   cField:=ALLTRIM(UPPER(cField))

   nAt:=ASCAN(oDp:aPicture,{|a,n| a[1]=cTable .AND. a[2]=cField })

   IF nAt>0
      RETURN oDp:aPicture[nAt,3]
   ENDIF

/*
   cPicture:=SQLGET("DPCAMPOS","CAM_FORMAT","CAM_TABLE"+GetWhere("=",cTable)+" AND "+;
                                            "CAM_NAME" +GetWhere("=",cField))
   IF !Empty(cPicture)
      cPicture:=ALLTRIM(cPicture)
   ELSE
      cPicture:=NIL
   ENDIF
*/

RETURN cPicture

/*
// Debe ser utilizado en el metodo COMMIT de la clase ttable
// Remueve los Residuos del Picture
// 001-000- SEGUN MASCARA 999-999-999 , quedara 001-000 (el residuo - es removido)
*/
FUNCTION DPDELPICTURE(cValue,cTable,cField)
   LOCAL nAt,cPicture,nLen

   DEFAULT cValue:="000-001-  ",;
           cTable:="DPGRU",;
           cField:="GRU_CODIGO"

   cPicture:=DPGETPICTURE(cTable,cField)

   IF Empty(cPicture)
      RETURN cValue
   ENDIF

   nLen    :=LEN(cValue)
   cValue  :=ALLTRIM(cValue)

   IF RIGHT(cValue,1)=SUBS(cPicture,LEN(cValue),1)
     cValue:=LEFT(cValue,LEN(cValue)-1)
   ENDIF

   cValue:=PADR(cValue,nLen)

// ? cValue,LEN(cValue)

RETURN cValue

/*
// Necesita sumar los DSN para las funciones OPENODBC y CLOSEODBC, utilizada conjuntamente com
// MyOpenDataBase(cTable,lTable,oServer,lSelect)
*/
FUNCTION ADD_DSN(cDsn,cUser,cPass,cType, cPath,cServer,cDataBase,cDriver,cDescription,nPort,cName,cEngine)
   LOCAL nAt

   DEFAULT aDsn:={}

   nAt:=ASCAN(aDsn,{|a,n| a[1]=ALLTRIM(UPPE(cDsn))})

   IF nAt=0 .AND. !Empty(cDsn)
     AADD(aDsn,{UPPE(ALLTRIM(cDsn)),cUser,cPass,cType, cPath,cServer,cDataBase,cDriver,cDescription,nPort,cName,cEngine})
   ENDIF

RETURN aDsn

EXIT PROCEDURE CERRAR_BD()

 MACROEJE("DPCLOSEMYSQL()")   // DPLLAVE.HRB

RETURN

/*
// Crear la BD desde master
*/

FUNCTION MS_CREATEDATABASE(cDb)
   LOCAL oODbc:=TODBC():New("master", oDp:cLogin, oDp:cPass  , .F. )
   LOCAL cSql :="CREATE DATABASE "+cDb

   ? oOdbc:Execute(cSql),cSql

   oOdbc:End()

RETURN .T.

/*
// Revisa si la Clave Esta Indexada
*/
FUNCTION CHKINDEX(cTable,cField,cTabRef)
  LOCAL lIndex:=.T.
  LOCAL cCommand,oIndice,cIndice

  cCommand:=SQLGET("DPCAMPOS","CAM_COMMAN","CAM_TABLE"+GetWhere("=",cTable)+;
            " AND CAM_NAME"+GetWhere("=",cField))

  IF !EMPTY(cCommand).AND."PRIMARY"$UPPE(cCommand)
     RETURN .T.
  ENDIF

  oIndice:=OpenTable("SELECT * FROM DPINDEX WHERE IND_TABLA"+GetWhere("=",cTable)+;
                     " AND IND_CLAVE"+GetWhere("=",cField)+;
                     " ORDER BY IND_INDICE",.T.)

  IF oIndice:RecCount()=0

//   cIndice:=DPSQLGETMAX("DPINDEX","IND_INDICE","IND_TABLA"+GetWhere("=",cTable))
     cIndice:=SQLINCREMENTAL("DPINDEX","IND_INDICE","IND_TABLA"+GetWhere("=",cTable))


     IF EMPTY(cIndice)

        cIndice:=ALLTRIM(cTable)+"1"

     ELSE

        cIndice:=DPINCREMENTAL32(cIndice,.F.)

        IF ISALLDIGIT(cIndice)
           cIndice:=cTable+"_"+cIndice
        ENDIF

//      cIndice:=ALLTRIM(cIndice)
//      cIndice:=cTable+ALLTRIM(STR(VAL(RIGHT(cIndice,1)+1),2))

     ENDIF

     oIndice:Append()
     oIndice:lAuditar:=.F.
     oIndice:REPLACE("IND_TABLA" ,cTable )
     oIndice:REPLACE("IND_CLAVE" ,cField )
     oIndice:REPLACE("IND_INDICE",cIndice)
     oIndice:REPLACE("IND_DESCRI","Int/Ref:"+cTabRef)
     oIndice:REPLACE("IND_NUMTAB",SQLGET("DPTABLAS","TAB_NUMERO","TAB_NOMBRE"+GetWhere("=",cTable)))
     oIndice:COMMIT()
     lIndex:=.F.

  ENDIF

  oIndice:End()

  EJECUTAR("BUILDINDEX",cTable) // Solicita la generacion del Indice

RETURN .T.


// EOF
