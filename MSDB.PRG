//---------------------------------------------------------------------------//
// Antiguo: MSDATABASE.PRG
//  AUTOR.....: Manu Expósito Suárez       Soft4U                            //
//  e-Mail....: maex14@dipusevilla.es                                        //
//  CLASE.....: TMSDataBase                                                  //
//  FECHA MOD.: 20/01/2005                                                   //
//  VERSION...: 3.00                                                         //
//  PROPOSITO.: Gestión de Bases de datos en MySQL                           //
//---------------------------------------------------------------------------//

#include "\EAGLE1V300\include\MSUtil.ch"

#DEFINE CRLF CHR(10)+CHR(13)

MEMVAR oDp
//---------------------------------------------------------------------------//

CLASS TMSDataBase

	DATA cName INIT ""
	DATA aTables INIT {}
	DATA lOpen INIT .f.
	DATA hConnect INIT 0
	DATA oConnect

   // JN
   DATA lSuccess INIT .T.
   DATA cType INIT "MYSQL"
   DATA cPath,cQuery,cEngine,lDateAsStr,cDsn
   DATA cVersion INIT ""  // Version
   DATA lAbort   INIT .T. // JN
   DATA lCsrDyn  INIT .T. // JN
   DATA lTableLower  INIT oDp:lTablelower // Indica si las tablas se denominan en Mayúscula
   DATA lDateNull    INIT .F.             // Fecha puede ser 00-00-0000 en el caso lDateNull sera NULL
   DATA lRemote      INIT .F.             // Servidor Remoto no Valida Tablas

	METHOD New CONSTRUCTOR
	METHOD Create

	METHOD Close

#ifdef __C3__
	METHOD Free
	METHOD Destroy
#else
	MESSAGE Free METHOD Close
	MESSAGE Destroy METHOD Close
#endif

	METHOD GetTables
	METHOD ListTables
	METHOD ExistTable
   METHOD File(cTbName) INLINE ::ExistTable( cTbName )

	METHOD Used

#ifdef __C3__
	METHOD Select
	METHOD Use
	METHOD Default
#else
	MESSAGE Select METHOD _Select
	MESSAGE Use METHOD _Select
	MESSAGE Default METHOD _Select
#endif

	METHOD ExecSQL

	// Gestion de transacciones:
	METHOD Begin
	METHOD Start
	METHOD Commit
	METHOD RollBack

	// Utilidates para tablas de la Base de Datos:
	METHOD CreateTable
	METHOD DropTable
	METHOD OptimizeTable
	METHOD CheckTable
	METHOD RepairTable

   METHOD Query(cSql)  // JN
   METHOD QueryData(cSql) // JN
   METHOD QueryAdata(cSql,cTable) // JN 17/07/2017
   METHOD Execute(cSql,cMsg,lMsgErr) INLINE ::ExecSQL(cSql,lMsgErr,cMsg)
   METHOD End()         INLINE ::Close()
   METHOD AllocStatement() INLINE ::hConnect
   METHOD QueryRow(cSql,cTable)
   METHOD GetIndex(cTable)
   METHOD IndexKey(cTable,nOrder)
   METHOD IsPrimary(cTable,cField)
   METHOD IsIndex(cTable,cIndex)
   METHOD CreateTableDp( cFileName, aFieldsInfo , cFileInd )
   METHOD GetTablesDp()
   METHOD HandleEvent( nMsg, nWParam, nLParam ) INLINE 1=1
   METHOD GetVersion()  // JN 01/01/2019

ENDCLASS

// ---------------------------------------------------------------------------//
// Constructor de la clase si ya existe la Base de Datos en el servidor

METHOD New( oConn, cName, lSelect ) CLASS TMSDataBase

 BYNAME cName

	if upper( oConn:ClassName ) == "TMSCONNECT"

     IF !oConn:ExistDb( cName )
         oConn:CreateDb(cName)
     ENDIF

		if oConn:ExistDb( cName )
			::lOpen := .t.
			::oConnect := oConn
			::hConnect := oConn:hConnect
			::GetTables()
			if ValType( lSelect ) == "L" .and. lSelect
				::Select()
			endif
		else
         oConn:oError:Say( MI("No existe la Base de Datos ",1025)+cName+CHR(10)+CHR(13)+GetProce(), .t. )
		endif
	else
		::hConnect := 0
	endif

   ::lSuccess:=::lOpen //JN
   ::cDsn    :=cName
   ::cType   :="MYSQL"


   IF !Empty(::aTables)
      ::cVersion:=::QueryRow("SELECT VERSION()",::aTables[1])[1]
   ENDIF

   IF ::cVersion>="5.7.24"
      ::lDateNull:=.T.
   ENDIF

return( Self )

//---------------------------------------------------------------------------//
// Constructor de la clase. Si no existe la Base de Datos la crea

METHOD Create( oConn, cName, lSelect ) CLASS TMSDataBase

	BYNAME cName

	if upper( oConn:ClassName ) == "TMSCONNECT"
		::oConnect := oConn
		::hConnect := oConn:hConnect
	endif

	if ::hConnect > 0
		if !empty( ::cName )
			::oConnect:CreateDB( ::cName )
        	::lOpen := ::oConnect:ExistDb( cName )
        	if ::lOpen
				::GetTables()
        		if ValType( lSelect ) == "L" .and. lSelect
					::Select()
				endif
			else
				::oConnect:oError:Say( MI("No se pudo abrir la Base de Datos",1026), .t. )
			endif
		else
			::oConnect:oError:Say( MI("Nombre de Base de Datos incorrecto",1027), .t. )
		endif
    else
    	TMSError():Say( MI("Objeto conexion no esta iniciado",1028) )
	endif

return( Self )

//---------------------------------------------------------------------------//
// Comprueba si existe una tabla en la Base de Datos

METHOD ExistTable( cTbName   ) CLASS TMSDataBase
 local cMemo:=""
	local lRet := .f.

	if ValType( cTbName ) == "C"

		cTbName := upper( AllTrim( cTbName ) )

      //IF Empty(::aTables)
        ::aTables:=::GetTables()
      //ENDIF

  // AScan( ::aTables, { | e | cMemo:=cMemo + IF (Empty(cMemo),",",",")+ e  })
  // DpWrite("atables",cMemo+" Buscar a "+cTbName)

		lRet := ( AScan( ::aTables, { | e | upper( Alltrim(e) ) == cTbName } ) > 0 )

	endif

return( lRet )

//---------------------------------------------------------------------------//
// IMPORTANTE ejecuta sentecias SQL que no devuelven registros.

METHOD ExecSQL( cStatement ,lMsgErr ) CLASS TMSDataBase
   local lTracer:=oDp:lTracer
	local lRet := .f.
   LOCAL cDpxBase:= oDp:cDpXbaseLine

   DEFAULT lMsgErr:=.T.

   IF ValType(lMsgErr)<>"L"
      lMsgErr:=.T.
   ENDIF

   //oDp:oFrameDp:SetText(cStatement+" ENMSDB")
   ::Use() // JN 13/11/2016
   ::Select()

	if ValType( cStatement ) == "C"

      if !( lRet := MMQuery( ::hConnect, cStatement ) )

        oDp:lTracer:=.F.

        IF lMsgErr
          RUNNEW("SQLMSGERR",cStatement,nil,::cName,nil,nil,cDpXbase,NIL,Self,::oConnect:oError:GetError()) // oDp:cDpXbaseLine)
        ENDIF

        oDp:lTracer:=lTracer

		endif

   endif

return( lRet )

//---------------------------------------------------------------------------//
// Optimiza una tabla

METHOD OptimizeTable( cTbName ) CLASS TMSDataBase

	local lRet := .f.

	if ::ExistTable( cTbName )
		lRet := ::ExecSql( "OPTIMIZE TABLE " + cTbName )
	endif

return( lRet )

//---------------------------------------------------------------------------//
// Revisa una tabla

METHOD CheckTable( cTbName ) CLASS TMSDataBase

	local lRet := .f.

	if ::ExistTable( cTbName )
		lRet := ::ExecSql( "CHECK TABLE " + cTbName )
	endif

return( lRet )

//---------------------------------------------------------------------------//
// Repara una tabla

METHOD RepairTable( cTbName ) CLASS TMSDataBase

	local lRet := .f.

	if ::ExistTable( cTbName )
		lRet := ::ExecSql( "REPAIR TABLE " + cTbName )
	endif

return( lRet )

//---------------------------------------------------------------------------//

#ifdef __C3__
	METHOD Select() CLASS TMSDataBase
	return( ::oConnect:SelectDb( Self ) )

	METHOD Use() CLASS TMSDataBase
	return( ::oConnect:SelectDb( Self ) )

	METHOD Default() CLASS TMSDataBase
	return( ::oConnect:SelectDb( Self ) )
#else
	METHOD _Select() CLASS TMSDataBase
	return( ::oConnect:SelectDb( Self ) )
#endif

//---------------------------------------------------------------------------//

METHOD Used() CLASS TMSDataBase
return( ( Self == ::oConnect:oDataBase ) )

//---------------------------------------------------------------------------//

METHOD GetTables() CLASS TMSDataBase
return( ::aTables := MMListTables( ::hConnect, ::cName ) )

//---------------------------------------------------------------------------//

METHOD ListTables() CLASS TMSDataBase
return( ::aTables := MMListTables( ::hConnect, ::cName ) )

//---------------------------------------------------------------------------//

METHOD Close() CLASS TMSDataBase
	::lOpen := .f.

//   ? "AQUI CIERRA LA BD",GETPROCE()

return( _DestroyVar( Self ) )

#ifdef __C3__
	METHOD Destroy() CLASS TMSDataBase
		::lOpen := .f.
	return( _DestroyVar( Self ) )

	METHOD Free() CLASS TMSDataBase
		::lOpen := .f.
	return( _DestroyVar( Self ) )
#endif

//---------------------------------------------------------------------------//

METHOD Begin() CLASS TMSDataBase
return( MMBegin( ::hConnect ) )

//---------------------------------------------------------------------------//

METHOD Start() CLASS TMSDataBase
return( MMStart( ::hConnect ) )

//---------------------------------------------------------------------------//

METHOD Commit() CLASS TMSDataBase
return( MMCommit( ::hConnect ) )

//---------------------------------------------------------------------------//

METHOD RollBack() CLASS TMSDataBase
return( MMRollBack( ::hConnect ) )

//---------------------------------------------------------------------------//
// Borra una tabla si existe

METHOD DropTable( cTable ) CLASS TMSDataBase

	local lRet := .f.

	if ::ExistTable( cTable )
		if lRet := ::ExecSQL( "DROP TABLE " + cTable )
			::GetTables()
		endif
	endif

return( lRet )

//---------------------------------------------------------------------------//
// Crea una tabla a partir de una estructura tipo xBase

METHOD CreateTable( cTable, aStruct, cType ) CLASS TMSDataBase

	local lRet := .f.
	local oTb

   ::Select()

	if ValType( cTable ) != "C"
		::oConnect:oError:Say( MI("Debe entrar un nombre de tabla correcto",1029), .t. )
	elseif ValType( aStruct ) != "A"
		::oConnect:oError:Say( MI("Debe entrar la estructura de tabla",1030), .t. )
	elseif ::ExistTable( cTable )
		::oConnect:oError:Say( MI("La tabla ",1031) + cTable + MI(" ya existe...",1032), .t. )
	else
		oTb := TMSTable():Create( Self, cTable, aStruct, cType )
		oTb:Free()
      ::GetTables()
		lRet := ::ExistTable( cTable )

      IF !lRet

      ENDIF

	endif

return( lRet )


// jn
METHOD Query(cSql,cTable) CLASS TMSDataBase
   LOCAL oCursor,aData,oTable
   LOCAL nAt

   IF cTable=NIL
     cTable   :=cTable(cSql)
   ENDIF

   oTable:=TMSTable():New( Self , cTable )

   oCursor:= TMSACursor():New( oTable ) // Sin datafields
   oCursor:Open()
   aData:=ACLONE(oCursor:Cursor)
   oCursor:Free()
   oTable:free()

RETURN aData

METHOD QueryData(cSql) CLASS TMSDataBase
  LOCAL uValue,oTable

  oTable:=MyTable(cSql,.F.,NIL,.F.,SELF)
  uValue:=oTable:FieldGet(1)
  oTable:End()

RETURN uValue

METHOD QueryAdata(cSql,cTable,aFields) CLASS TMSDataBase
   LOCAL oDb,oTb,aData:={}

   IF Empty(::aTables)
      ::GetTables()
   ENDIF

   IF Empty(::aTables)
      RETURN {}
   ENDIF

   DEFAULT cTable:=::GetTables()[1]

   ::Select()
   oTb := TMSTable():New( Self, cTable )

   oTb:Open(cSql)
   oTb:GoTop():Read()
   oTb:xLoad()

   WHILE !oTb:Eof
      AADD(aData,oTb:GetRow())
      oTb:Skip( 1 )
      oTb:Read()
    ENDDO

    aFields :=ACLONE(oTb:aStruct) // Copia la estructura
    oDp:aFields:=aFields

    oTb:Destroy()

RETURN aData

// jn
METHOD QueryRow(cSql,cTable)  CLASS TMSDataBase
    LOCAL oTb,aData

    IF Empty(cTable)
       EJECUTAR("MSGMEMO","QueryRow(cSql,cTable)"+CRLF+cSql,MI("Falta cTable",1033))
    ENDIF

// oDp:oFrameDp:SetText("queryrow:" +cTable+" "+cSql+" "+oDp:cDpXbaseLine)

    ::Select()
    oTb := TMSTable():New( Self, cTable )

    oTb:Open(cSql)
    oTb:GoTop():Read()
    oTb:xLoad()

    aData:=oTb:GetRow()

    oTb:Destroy()

RETURN aData

// jn
METHOD GetIndex(cTable,lPrimary) CLASS TMSDataBase
  LOCAL aData,aIndex:={},I,cFind

  DEFAULT lPrimary:=.F.

  cFind:=IIF(!lPrimary,"KEY","PRIMARY KEY")

  aData:=::QueryRow("SHOW CREATE TABLE "+cTable,cTable)

  aData:=_VECTOR(aData[2],CHR(10))

  FOR I=1 TO LEN(aData)

     aData[I]:=ALLTRIM(aData[I])

     IF LEFT(aData[I],7)="PRIMARY"// "KEY"
       AADD(aIndex,"PRIMARY")
       LOOP
     ENDIF

     IF LEFT(aData[I],LEN(cFind))==cFind

       AADD(aIndex,_VECTOR(aData[I],"`")[2])

     ENDIF

  NEXT

RETURN aIndex

// jn
METHOD IndexKey(cTable,nOrder,lPrimary) CLASS TMSDataBase
  LOCAL aData,aIndex:={},I,cIndex,cKey,cFind:="KEY"

  DEFAULT lPrimary:=.F.

  cFind:=IIF(lPrimary,"KEY","PRIMARY KEY")

  aData:=::QueryRow("SHOW CREATE TABLE "+cTable,cTable)
  aData:=_VECTOR(aData[2],CHR(10))

  FOR I=1 TO LEN(aData)

     aData[I]:=ALLTRIM(aData[I])

     IF LEFT(aData[I],3)=cFind .OR. LEFT(aData[I],7)="PRIMARY"// "KEY"

       cIndex:=_VECTOR(aData[I],"`")[2]
       cKey  :=_VECTOR(aData[I],"(")[2]
       cKey  :=_VECTOR(cKey,")")[1]
       cKey  :=STRTRAN(cKey,"`","")
       cKey  :=STRTRAN(cKey,","," + ")

       IF LEFT(aData[I],7)="PRIMARY"// "KEY"
          cKey:="PRIMARY"
       ENDIF

       AADD(aIndex,{cIndex,cKey})

     ENDIF

  NEXT

RETURN IIF( nOrder=0 .OR. nOrder>len(aIndex) , "" ,aIndex[nOrder,2])

// jn
METHOD IsPrimary(cTable,cField) CLASS TMSDataBase
 LOCAL aIndex:=::GetIndex(cTable,.T.)
RETURN ASCAN(aIndex,cField)>0

// jn
METHOD IsIndex(cTable,cIndex) CLASS TMSDataBase
 LOCAL aIndex:=::GetIndex(cTable,.F.)
RETURN ASCAN(aIndex,cIndex)>0


// jn

METHOD CreateTableDp( cFileName, aFieldsInfo , cFileInd , lLower ) CLASS TMSDataBase

   local cSQL,lResp,aLine:={}
   local n, cType,i,nT1

   DEFAULT lLower:=.T.

   if cFilename == Nil
      MsgStop( "Fieldname invalid", "TMSDataBase:Create)"  )
      return .f.
   endif

   IF ::lTableLower .OR. lLower
      cFileName:=Lower(cFileName)
   ENDIF

   // JN 06/10/2016
   cFileName:=alltrim(cFileName)

   IF ::FILE(lower(cFileName)) .OR. ::FILE(UPPER(cFileName))
      RETURN .F.
   ENDIF

// ViewArray(aFieldsInfo)
//   if " "$Alltrim(cFileName)
//      cSQL := "CREATE TABLE [" + cFileName + "] ( "
//   else
    cSQL := "CREATE TABLE IF NOT EXISTS " + cFileName + " ( "
//   endif

   if aFieldsInfo == Nil
      MsgStop( "Fields info array invalid", "TMSDataBase:CreateTableDp)" )
      return .f.
   endif

   for n = 1 to Len( aFieldsInfo )
      if n > 1
         cSQL += ", "
      endif

      cType := Upper( aFieldsInfo[ n ][ 2 ] )

      do case
         case cType == "C"
              cSQL += aFieldsInfo[ n ][ 1 ] +  " Char(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ")"

         case cType == "L"

              cSQL += aFieldsInfo[ n ][ 1 ] +  " Numeric(1)"
              // JN 20/03/2016  Campo Logico sera Bit
              // cSQL += aFieldsInfo[ n ][ 1 ] +  " BIT(1)"

         case cType == "N" .AND. "AUTO_INCREMENT"$aFieldsInfo[ n ][ 5 ]

              cSQL += aFieldsInfo[ n ][ 1 ] +  " INT(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ")"

         case cType == "I"

              cSQL += aFieldsInfo[ n ][ 1 ] +  " INT(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ")"

         case cType == "N" .AND. !"AUTO_INCREMENT"$aFieldsInfo[ n ][ 5 ]

              cSQL += aFieldsInfo[ n ][ 1 ] +  " Numeric(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ", " +;
                      cValToChar( aFieldsInfo[ n ][ 4 ] ) + ")"

         case cType == "M"

              // jn 19/12/09, text por longtext
              cSQL += aFieldsInfo[ n ][ 1 ] + " longText"

         case cType == "B"

              // jn 19/12/09, text por longtext
              cSQL += aFieldsInfo[ n ][ 1 ] + " longblob"


         case cType == "D"


              cSQL += aFieldsInfo[ n ][ 1 ] + " Date"

         other

            MensajeErr("Campo "+ aFieldsInfo[ n ][ 1 ]+" Tipo "+cType+" Inválido" )
            cSQL :=LEFT(cSql,LEN(cSql)-2)

            IF ::File("DPCAMPOS")
               ::Execute("UPDATE DPCAMPOS SET CAM_TYPE"+GetWhere("=","C")+" WHERE CAM_TYPE"+GetWhere("=","1"))
            ENDIF

            LOOP

      endcase

      if len(aFieldsInfo[ n ]) > 4 .AND. !EMPTY(aFieldsInfo[ n ][ 5 ])

         aFieldsInfo[ n ][ 5 ]:=STRTRAN(aFieldsInfo[ n ][ 5 ],CRLF,"")

         cSQL += " " + alltrim(aFieldsInfo[ n ][ 5 ])

      endif

   next

//   MensajeErr(cSql)

   cSQL += " )" // + IIF( ValType(::cEngine)="C",::cEngine,"") // JN Motor de la Base de Datos

   // Solo agrega si esta permitido en LDBENGINE
   IF Empty(oDp:cDbEngine) .AND. oDp:lDbEngineSet
     oDp:cDbEngine:="ENGINE=InnoDB DEFAULT CHARSET=latin1"
   ENDIF

   /*
   // Solicita la Version de MySql
   */
   IF Empty(oDp:cMySQLVersion)
     EJECUTAR("MYSQLVERSION")
   ENDIF

   // Version 5, no requiere INNODB
   IF "5"=LEFT(oDp:cMySQLVersion,1)
//    JN 15/05/2016 Permitira al Integrador defenir el Tipo de Datos en Datapro.ini
//      oDp:cDbEngine:=""
   ENDIF

   // cSql:=cSql+" TYPE = "+oDp:cDbEngine // InnoDB // "
   // solo asigna el CHARSET
   IF oDp:lDbEngineSet
     cSql:=cSql+" "+oDp:cDbEngine // InnoDB // "
   ENDIF

// JN 06/02/2011
//   IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
//      MensajeErr(cSQL)
//   ENDIF

// MensajeErr(CLPCOPY(cSQL) , "CREAR TABLA "+oDp:cDbEngine)

   nT1  :=SECONDS()

// oDp:oFrameDp:SetText(cSql)

   DPWRITE("TEMP\CREATE_"+cFileName+".SQL"+CRLF+GETPROCE(),cSQL)

   lResp:=::Execute( cSQL, "oOdbc:Create()" ) // JN

   oDp:cSql:=cSql

   IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
// JN 13/12/2013
      EJECUTAR("DPTRAZASQL",cSql,cFileName,::cDsn,SECONDS()-nT1)
//MensajeErr(cSQL)
   ENDIF

   IF !lResp
      EJECUTAR("MSGMEMO",MI("Tabla ",1035)+cFileName+CRLF+cSQL,MI("No pudo ser Creada [DB = ",1034)+::cDsn+"]")
      MemoWrit("create.sql",cSql)
   ELSEIF FILE(cFileInd)
      // Debe Crear los Indices o Comandos
      aLine:=_VECTOR(MEMOREAD(cFileInd),";")
      // aLine:=_VECTOR(aLine,";")

      FOR I := 1 TO LEN(aLine)
         IF !EMPTY(aLine[I])
            DPWRITE("TEMP\CREATE_"+cFileName+"_INDEX"+LSTR(I)+".SQL",aLine[I])
            ::Execute( aLine[I] , "oOdbc:CreateIndex()" ) // JN
         ENDIF
      NEXT

   ENDIF

   // ::GetTables()

   SysRefresh(.t.)

RETURN lResp


// jn
/*
METHOD CreateTableDp( cFileName, aFieldsInfo , cFileInd ) CLASS TMSDataBase

   local cSQL:="",lResp,aLine:={}
   local n, cType,i,nT1,cField

   if cFilename == Nil
      MsgStop( "Fieldname invalid", "TMSDataBase:Create)"  )
      return .f.
   endif

   IF ::lTableLower
      cFileName:=Lower(cFileName)
   ENDIF

   // JN 06/10/2016
   IF ::FILE(lower(cFileName)) .OR. ::FILE(UPPER(cFileName))
      RETURN .F.
   ENDIF

// ViewArray(aFieldsInfo)
//   if " "$Alltrim(cFileName)
//      cSQL := "CREATE TABLE [" + cFileName + "] ( "
//   else

//    cSQL := "CREATE TABLE " + cFileName + " ( "

//   endif

   if aFieldsInfo == Nil
      MsgStop( "Fields info array invalid", "TMSDataBase:CreateTableDp)" )
      return .f.
   endif

   adepura(  aFieldsInfo ,{|a,n| Empty(a) .OR. ASC(LEFT(a,1))<30})

   for n = 1 to Len( aFieldsInfo )

      if n > 1
        // cSQL += ", "
      endif

      cType := Upper( aFieldsInfo[ n ][ 2 ] )

      do case
         case cType == "C"
              cField:= aFieldsInfo[ n ][ 1 ] +  " Char(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ")"

         case cType == "L"

              cField:= aFieldsInfo[ n ][ 1 ] +  " Numeric(1)"
              // JN 20/03/2016  Campo Logico sera Bit
              // cSQL += aFieldsInfo[ n ][ 1 ] +  " BIT(1)"

         case cType == "N" .AND. "AUTO_INCREMENT"$aFieldsInfo[ n ][ 5 ]

              cField:= aFieldsInfo[ n ][ 1 ] +  " INT(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ")"

         case cType == "I"

              cField:= aFieldsInfo[ n ][ 1 ] +  " INT(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ")"

         case cType == "N" .AND. !"AUTO_INCREMENT"$aFieldsInfo[ n ][ 5 ]

              cField:= aFieldsInfo[ n ][ 1 ] +  " Numeric(" + ;
                      cValToChar( aFieldsInfo[ n ][ 3 ] ) + ", " +;
                      cValToChar( aFieldsInfo[ n ][ 4 ] ) + ")"

         case cType == "M"

              // jn 19/12/09, text por longtext
              cField:=aFieldsInfo[ n ][ 1 ] + " longText"

         case cType == "B"

              // jn 19/12/09, text por longtext
              cField:= aFieldsInfo[ n ][ 1 ] + " longblob"


         case cType == "D"


              cField:= aFieldsInfo[ n ][ 1 ] + " Date"

      OTHER

          cField:= aFieldsInfo[ n ][ 1 ]

          MensajeErr("Campo "+cField+" Type "+cType+" Tabla "+cFileName+" no Implementado en AdaptaPro")
          cField:=""

      endcase

      if len(aFieldsInfo[ n ]) > 4 .AND. !EMPTY(aFieldsInfo[ n ][ 5 ])
         cField:=cField+" "+aFieldsInfo[ n ][ 5 ]   // Comando
      endif

      IF !Empty(cField)
        cSql:=cSql + IF(Empty(cSql),"",",")+cField
      ENDIF

   next

//   MensajeErr(cSql)

   cSQL:=STRTRAN(cSQL,", ,",",")

   cSQL := "CREATE TABLE " + cFileName + " ( "+cSql


   cSQL += " )" // + IIF( ValType(::cEngine)="C",::cEngine,"") // JN Motor de la Base de Datos

   IF Empty(oDp:cDbEngine)
     oDp:cDbEngine:="ENGINE=InnoDB DEFAULT CHARSET=latin1"
   ENDIF

   IF Empty(oDp:cMySQLVersion)
     EJECUTAR("MYSQLVERSION")
   ENDIF

   // Version 5, no requiere INNODB
   IF "5"=LEFT(oDp:cMySQLVersion,1)
//    JN 15/05/2016 Permitira al Integrador defenir el Tipo de Datos en Datapro.ini
//      oDp:cDbEngine:=""
   ENDIF

   // cSql:=cSql+" TYPE = "+oDp:cDbEngine // InnoDB // "
   cSql:=cSql+" "+oDp:cDbEngine // InnoDB // "

// JN 06/02/2011
//   IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
//      MensajeErr(cSQL)
//   ENDIF

// MensajeErr(CLPCOPY(cSQL) , "CREAR TABLA "+oDp:cDbEngine)

   nT1  :=SECONDS()

// oDp:oFrameDp:SetText(cSql)
   lResp:=::Execute( cSQL, "oOdbc:Create()" ) // JN

   IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
// JN 13/12/2013
      EJECUTAR("DPTRAZASQL",cSql,cFileName,::cDsn,SECONDS()-nT1)
//MensajeErr(cSQL)
   ENDIF

   IF !lResp
      EJECUTAR("MSGMEMO",MI("Tabla ",1035)+cFileName+CRLF+cSQL,MI("No pudo ser Creada [DB = ",1034)+::cDsn+"]")
      MemoWrit("create.sql",cSql)
   ELSEIF FILE(cFileInd)
      // Debe Crear los Indices o Comandos
      aLine:=_VECTOR(MEMOREAD(cFileInd),";")
      // aLine:=_VECTOR(aLine,";")

      FOR I := 1 TO LEN(aLine)
         IF !EMPTY(aLine[I])
            ::Execute( aLine[I] , "oOdbc:CreateIndex()" ) // JN
         ENDIF
      NEXT

   ENDIF

   ::GetTables()

   SysRefresh(.t.)

RETURN lResp
*/

// jn
METHOD GetTablesDp( cMask ) CLASS TMSDataBase
    LOCAL aData

RETURN ::GetTables()
//    aData:=::QueryData("SHOW TABLE STATUS ")
//RETURN aData

METHOD GetVersion() CLASS TMSDataBase

 IF !Empty(::aTables)
    ::cVersion:=::QueryRow("SELECT VERSION()",::aTables[1])[1]
 ENDIF

 IF ::cVersion>="5.7.24"
    ::lDateNull:=.T.
 ENDIF

RETURN ::cVersion
