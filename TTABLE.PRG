// oDp:bSqlDate:={|nD,nM,nA|STRZERO(nA,4)+""+STRZERO(nM,2)+""+STRZERO(nD,2)}
//----------------------------------------------------------------------------//
// Permite Agregar y Modificar nuevos Valores sobre un Cursor de Tipo TBDODBCD
// CLASS Ttable
// JN 26/12/2002
//

#INCLUDE "FIVEWIN.CH"
#INCLUDE "XBROWSE.CH"

STATIC aRuta
STATIC nTime

// ,aTablas,nTablas
STATIC aMemos,aTableOpen,nHandler
STATIC aTableStruct // Registros Exluidos
STATIC aLimit

MEMVAR oDp,aData


CLASS Ttable

   DATA   oCursor
   DATA   oOdbc
   DATA   oScript

   DATA   nOption
   DATA   nArrayAt
   DATA   nRecord
   DATA   nPos        // Posición del Ultimo Campo
   DATA   nRecno      // Posicion del Registro
   DATA   nOrder      // Orden de los Datos
   DATA   nCountIns   // Cantidad de Registros Contados
   DATA   nInsert     // Cantidad de Registros que Seran Incluidos
   DATA   nSeconds
   DATA   nHandler

   DATA   aDataFill   INIT {} // Data Completa de la tabla (Original)
   DATA   aBodyLink   // Campos de enlace
   DATA   aHeadLink   // Campos de la Tabla enlazadora
   DATA   aFieldsNew
   DATA   aFieldsFrom
   DATA   aFields
   DATA   aAddFields  // Campos Agregados en ADDFIELDS
   DATA   aBuffers
   DATA   aDataOrg    // Data Completa de la taba
   DATA   aVars       // Control de los Campos
   DATA   aRecord     // Control del Registro
   DATA   aFieldSpeed
   DATA   aFieldTable
   DATA   aIntRef  INIT {} // JN 12/03/2014 Contiene Datos de la Integridad Referencial
   DATA   aRecClone INIT {} // JN 01/11/2014 Copia del Registro de Integridad Referencial
   DATA   aFieldsTable INIT {} // Campos exclusivos de la Tabla
   DATA   aTotales INIT {}

   DATA   aDefault INIT {}

   DATA   cTable
   DATA   cFileName
   DATA   cWhere    // Condicion del Enlace entre tablas
   DATA   cSql
   DATA   cPrimary
   DATA   cFieldOrder // permite establecer Orden
   DATA   cField_Order INIT "" // Orden segun ::Sort()
   DATA   cKeyAudita   INIT "" // Clave para Generar las Pistas de Auditoria, por defecto Asume ORDER BY

   DATA   cName       // Nombre del Campo
   DATA   cError
   DATA   cSingular   // Nombre Singular de la Tabla
   DATA   cType       // Tipo de Data en ODBC
   DATA   cInsert     // Indica el Super INSERT INTO de varios registros
   DATA   cMsgInt       INIT "Requiere Ejecutar metodo IsChkIntRef()"
   DATA   cWhereRecord  INIT "" // Where con todos los campos del Registros
   DATA   cDpXbaseLine  INIT "DpXbase: "+oDp:cScript+CRLF+" Función: "+oDp:cScrFunc+" Línea: "+oDp:cScrLine // oDp:cDpXbaseLine // Programa Linea de Ejecución
   DATA   cFieldAud     INIT "" // Campo de Auditoria Segun formulario tdocgrid

   DATA   lFromCursor
   DATA   lDesc         INIT .F. // Orden según ::Sort()

   DATA   lDicc         INIT .F. // 28/04/2023 Depende del Diccionario de Datos

   DATA   lAppend
   DATA   lAutomatic  // Modo de Asignación de Valores
   DATA   lFound
   DATA   lOnly       // Solo campos de la tabla
   DATA   lEof        // Control de Navegación
   DATA   lBof
   DATA   lMemo       // Indica que Hay Campo Memo
   DATA   lIsDef
   DATA   lMySqlNativo
   DATA   lReloadData // Luego que Incluye requiere Recargar
   DATA   lLock          INIT .F.
   DATA   lSetFieldDef   INIT .T.
   DATA   lLower         INIT .F.
   DATA   lCreaRegIntRef INIT .F. // Crear Registros de Integridad Referencial en Commit, clase TDPEDIT debe ejecutarlo en LOAD y por esto debe asigar .f. a DATA lCreaRegIntRef
   DATA   lInsertReplace INIT .F. // Realiza Insert INTO Mediante Sentencia REPLACE
   DATA   lUpdate        INIT .F. // Si append() es modificar su valor sera .t. y agrega Where en COMMIT
   DATA   lRemote        INIT .F. // Indica la Tabla tiene conexion Remota
   DATA   lMsgError      INIT .F. // Mostrar Error en ONEROR
   DATA   lAuditar       INIT .T. // Aplica Pista de Auditoria, importar datos en excel generará incidencias  // programa auditaelimod
   DATA   lFileLog       INIT .T. // Guardar LOG en archivo en TEMP\ mediante programa COMMIT

   DATA   lSET_FOREIGN   INIT .T.
   // DATA   lSetForeignkeyOff INIT .F.


   // METHOD  New(oCursor , cSql , oOdbc )
   METHOD  New( oCursor , cSql , oOdbc , cTable , nOption , cPrimary , lCheck , lLimit ,lLower ) CONSTRUCTOR
   METHOD  Replace(cField, cValor , nLen, nDec )
   METHOD  Replaceifempty(cField, cValor , nLen, nDec ) INLINE IIF(Empty(::Get(cField)), ::Replace(cField, cValor , nLen, nDec ),NIL)
   METHOD  DbStruct() INLINE ::aFields

   METHOD  ReplaceSpeed(cField, cValor , lMsg )
   METHOD  CommitSpeed(lSave)
   METHOD  Commit(cWhere)
   METHOD  Append()  INLINE IF(::RecCount()=0,(::lUpdate:=.F.,::cWhere:="",::AppendBlank()),::lUpdate:=.T.) //  JN 01/07/2017, Agrega si el registro esta Vacio ::lAppend:=.T.
   METHOD  SetAuditar(cFieldAud) // Requiere oTable:cPrimary:="Clave"
//   METHOD  Commit()
   METHOD  InitFields()
   METHOD  AddRecord()
   METHOD  GetExcluye() INLINE GetExcluye(::cTable)[2]
   METHOD  ATOTAL(cField)

   METHOD  LockTable()
   METHOD  UnLockTable()

//INLINE (IIF(::lLock,::oOdbc:Execute("UNLOCK TABLES "),NIL),;
//                                  ::lLock:=.F.)

   METHOD  SetArray(aData,aFields)
   METHOD  Count(cWhere) INLINE Count(::cTable,cWhere)
   METHOD  CopiaReg(oOrigen)
   METHOD  GetPicture(nField,lMiles) // Genera el picture del Campo
   METHOD  Load(nRecord)
   METHOD  SaveSpeed()
   METHOD  SeekBin(uValue,cField,lSoft)
   METHOD  CTONUM(cField)   // Convierte en Numerico
   METHOD  CTOLOG(cField)   // Convierte en Logico
   METHOD  CTOASLOG()       // Convierte todos los Campos en Lógicos Cuando Viene de SELECT 0 AS LOGICO

   METHOD  SetDefault()     INLINE EJECUTAR("DPTABLESETDEF",SELF)
   //METHOD  SetForeignkeyOff() INLINE  ::EXECUTE("SET FOREIGN_KEY_CHECKS = 0"), ::lSET_FOREIGN:=.F.


   METHOD  CTOD(cField)   // Convierte en Numerico
   METHOD  ChkDsn()       // Revisa Dsn


   METHOD FieldName(cField)
   METHOD ChkIntRef(cField) // Revisa Valores Faltantes para la Integridad Ref
   METHOD IsChkIntRef(lSay) INLINE (oDp:lChkInt:=EJECUTAR("CHKINTREF",SELF,lSay)   ,;
                                ::aIntRef:=ACLONE(oDp:aLinks),;
                                ::cMsgInt:=oDp:cMsgChkInt,;
                                oDp:lChkInt)

   METHOD ViewIntRef() INLINE (::IsChkIntRef(),ViewArray(::aIntRef))

   METHOD FieldGet(nPos)
   METHOD NewRecord()   // Realiza un Nuevo Registro
   METHOD FieldPut(nPos,uValue) INLINE ::Replace(::aFields[nPos,1],uValue)
   METHOD ADEL(nPos)            INLINE ADEL(::aDataFill,nPos)

   METHOD _FieldGet(nPos) // Lee desde la Data Original
   METHOD FieldPos(cField)
   METHOD RecCount()        INLINE LEN(::aDataFill) // Cantidad de Registros
   METHOD nRecCount()       INLINE LEN(::aDataFill)
   METHOD Fieldtype(nField) INLINE (nField:=::FieldPos(nField),IF(nField>0,::aFields[nField,2],""))
   METHOD FieldLen(nField)  INLINE (nField:=::FieldPos(nField),IF(nField>0,::aFields[nField,3],""))
   METHOD FieldDec(nField)  INLINE (nField:=::FieldPos(nField),IF(nField>0,::aFields[nField,4],""))
   METHOD Recno()           INLINE (::nRecno)


   METHOD GoTop()           INLINE (::nRecno:=1,::UpDateFields())
   METHOD GoTo(nRecno)      INLINE (::nRecno:=nRecno,::nArrayAt:=nRecno,::UpDateFields())
   METHOD GoBottom()        INLINE (::Goto(LEN(::aDataFill)))
   METHOD Eof()             INLINE (::nRecno>LEN(::aDataFill).OR. Empty(::aDataFill)) // ::nRecno=0)
   METHOD Bof()             INLINE (::nRecno<1)
   METHOD Skip(nSkip,lView)      // INLINE (nSkip:=IIF( nSkip=NIL, 1 ,nSkip ),::nRecno:=MAX(1,::nRecno),::nRecno:=MIN(LEN(::aDataFill),::nRecno), ::Goto(::nRecno+nSkip))
   METHOD DbSkip(nSkip,lView)   INLINE (::Skip(nSkip,lView))
   METHOD DbEval(bAction,bWhile,bFor)
   METHOD Fcount()          INLINE len(::aFields)
   METHOD nFields()         INLINE len(::aFields)
   METHOD GotoId(nRecord)   INLINE ::Goto(nRecord)
   METHOD Execute(cSql,cCommand)
   METHOD GetTotal(cField)
   METHOD GetMax(cField,cWhere,nLimit) INLINE  SqlGetMax(::cTable,cField,cWhere,nLimit,NIL,::oOdbc)
   METHOD TotalField(cField)
   METHOD CreaRegIntRef(lRun) INLINE (RUNNEW("DPCREAREGINTREF",SELF,lRun),;
                                      ::lCreaRegIntRef:=.F.)
   METHOD ISFIELD(cField)
   METHOD CLOSE() INLINE ::End() // 22/03/2022
   // METHOD Use()   // Sólo para Nativo

   METHOD SetInsert(nRecord) INLINE (nRecord:=IIF(nRecord=NIL,100,nRecord),::nInsert:=nRecord,::nCountIns:=0)

   // Metodo para el Desplazamiento en Documentos
   METHOD GotoMax(cField,cWhere,lMax)
   METHOD GotoMin(cField,cWhere,lMax) INLINE ::GotoMax(cField,cWhere,.F.)

   // Determina la Siguiente/Anterior Clave
   METHOD GetNext(cField,uValue,lMax)
   METHOD GetPrev(cField,uValue,lMax) INLINE ::GetNext(cField,uValue,.F.)
   METHOD CTODBF(cFileDbf,cRdd,oMeter,oSay,cError,lCreate,cFieldIndex)
   METHOD CTOHTML(cFile) INLINE EJECUTAR("TABLETOHTML",Self,cFile)
   METHOD GETWHERERECORD(cFields,lAll) INLINE (::cWhereRecord:=EJECUTAR("GETWHERERECORD",SELF,cFields,lAll))

   METHOD CTOTXT(cFile)
   METHOD CLONE(oTable)
   METHOD USE()  INLINE NIL // Compatible MyTable Nativo
   METHOD Browse(lOrder,cTitle,  lMdi)
   METHOD BRW(lOrder) INLINE EJECUTAR("BRWDINAMICO",SELF,NIL,lOrder)


   METHOD GotoSkip(nStep,cField,cWhere)

    /*IF ::nMode=1 // Debe Buscar el Registro Mayor
       oTable  :=OpenTable("SELECT MAX(CONCAT("+oTable:cPrimary+")) AS ULTIMO FROM "+::oTable:cTable,.T.)
       ? oTable:FieldGet(1)
    ENDIF */


   METHOD UpDateFields()
   METHOD GetValKey(cFieldKey)
   METHOD GetWhereKey(cFieldKey)
   METHOD IsOk() INLINE (ValType(::aFields)="A" .AND. !Empty(::aFields))
   METHOD DeleteAll(lIni) // Borra todos los Registros
   METHOD Delete(cWhere)
   METHOD ExportBD(oOdbc,cTable,lDelete,oSay)  INLINE EJECUTAR("DBTABLEXPORT",Self,oOdbc,cTable,lDelete,oSay)

   METHOD  aFill()    INLINE (::aDataFill:=::oCursor:aFill()) // ,::Gotop()) // ,::GoTop())
/*
   METHOD  Goto(nPos) INLINE ::nArrayAt:=nPos
   METHOD  GoTop()    INLINE ::nArrayAt:=1
   METHOD  GoBottom() INLINE ::nArrayAt:=LEN(::aDataFill)
   METHOD  Fcount()   INLINE len(::aBuffers)
*/

   METHOD  DbSeek(uValue,lSoftSeek,cField)
   METHOD  AppendBlank()

   METHOD  AddFields(cField,uValue)
   METHOD  Reload()   // Recarga el Query
   METHOD  Refresh()

   METHOD  GetIndex(lViews) INLINE ::oOdbc:GetIndex(::cTable,lViews)            // JN 29/12/2002
   METHOD  IndexKey(nOrder) INLINE ::oOdbc:IndexKey(::cTable,nOrder)              // JN 29/12/2002
   METHOD  isIndex(cIndex)  INLINE ::oObdc:IsIndex(::cTable,cIndex )
   METHOD  GetDataKey(nOrder)
   METHOD  GetValue(cField)  //INLINE ::FieldGet(::FieldPos(cField))
   METHOD  GenSeek(cKey,nOrder)
   METHOD  SeekTable(cField,oGet,cWhere,oSay)
   METHOD  ShowError(cTitle)
   METHOD  MsgError(cTitle) INLINE ::ShowError(cTitle)
   METHOD  DbSeek2(cKey,lSoftSeek,nOrder) INLINE ::oOdbc:DbSeek(::cTable,cKey,lSoftSeek,nOrder,::GenSeek(cKey,nOrder))
   METHOD  AddField(cField,cType,nLen,nDec) // Agregar un Campo Virtual
   METHOD  GetWhere() // Devuelve Where Segun Valor de todos sus Campos

   // METHOD  GetWhere()  // Devuelve la Clausula Where de la Expresión ::cSql

   // Metodos para los campos //
   METHOD Add( cName, xValue )
   METHOD Get( cName )
   METHOD Set( cName, xValue )
   METHOD GetPos( cName )
   METHOD IsDef( cName )

   METHOD SetOrder(cField,lDesc) INLINE ::Sort(cField,lDesc)
   METHOD Sort(cFields,lDesc)
   METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)

   METHOD ViewArray() INLINE ViewArray(::aDataFill,::cTable)

      METHOD  SetForeignkeyOff() INLINE  ::EXECUTE("SET FOREIGN_KEY_CHECKS = 0"), ::lSET_FOREIGN:=.F.
   METHOD  SetForeignkeyOn()  INLINE  ::EXECUTE("SET FOREIGN_KEY_CHECKS = 1"), ::lSET_FOREIGN:=.T.

   METHOD HandleEvent(nMsg, nWParam, nLParam ) EXTERN ;
                      WndHandleEvent( Self, nMsg, nWParam, nLParam )

   METHOD  End()

   ERROR HANDLER OnError( cMsg, nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oCursor,cSql,oOdbc,cTable,nOption,cPrimary,lCheck,lLimit,lLower  ) CLASS Ttable
  LOCAL I,nLen,uValue,lLoad:=.F.,aIndex,oFile,nAt,nAtX:=0 ,nLimit
  LOCAL oSelect,cName

  DEFAULT cSql     :="",;
          nOption  :=0 ,;
          lLimit   :=.T.,;
          lLower   :=oDp:lTableLower:=.F. ,;
          oDp:lLock:=.T.

  cSql:=STRTRAN(cSql,CRLF," ")

  DEFAULT oDp:aFunction:={} // Funciones de la Base de Datos

  ::lFromCursor:=ValType(oCursor)="O"
  ::lMemo      :=.F.
  ::nRecord    :=1
  ::oOdbc      :=oOdbc
  ::lReloadData:=.T.
  ::aDataFill  :={}
  ::lAutomatic :=.T.
  ::cError     :=""
  ::lOnly      := .T. // 03/07/2006 (" INNER "$UPPE(cSql)) // Indica que solo puede Aceptar INSERT/UPDATE de Campos Propios
  ::lLower     := lLower // Mayúsculas/Minúsculas

  ::lSET_FOREIGN:=.T.
  // ? ::lOnly,cSql

  ::cInsert    :=""  // Indica Alta Cantidad de Registros
  ::nInsert    :=0   // Cantidad de Registros
  ::nCountIns  :=0   // Registros Incluidos
  ::lIsDef     :=.F.
  ::lMySqlNativo:=.F.


  IF Valtype(oCursor)="C" .AND. ValType(cSql)!="C"
     cTable :=oCursor
     cSql   :="SELECT * FROM "+cTable
     oCursor:=NIL
  ENDIF

  IF EMPTY(cTable).AND.VALTYPE(cSql)="C".AND.!"SELECT"$cSql

     ::lFromCursor:=.F.
     ::cTable:=cSql

     cSql:="SELECT * FROM "+cSql

  ENDIF

  cSql  :=IIF( EMPTY(cSql) , "SELECT * FROM "+cTable , cSql)


  IF lLimit
    nLimit:=GETLIMITREC(cSql)
  ENDIF

  IF nLimit>0 .AND. !"LIMIT"$cSql
     cSql:=cSql+" "+CRLF+" LIMIT "+LSTR(nLimit)
  ENDIF

  oDp:lMySqlNativo:=.F.

  IF !Empty(oDp:cFileSql)

   oFile:=TFile():New( oDp:cFileSql )
   oFile:PutStr(cSql)
   oFile:Close()

  ENDIF


//? "AQUI ES OPENTABLE",cSql,GETPROCE()


  // JN 21/04/2016 TODBC

  ::cType:=oOdbc:cType

  // Requiere Version, Caso de MySql5.7, Requiere fecha ::lDateNull , para caso de fechas 0000-00-00

  IF Empty(oOdbc:cVersion)
     oOdbc:GetVersion()
  ENDIF

  DEFAULT ::cType:=oDp:cTypeBD

  // JN 06/01/2017 Alineación de MYSQL-MSSQL SELECT..GROUP BY

  DEFAULT oDp:lSqlSelectClass:=GETINI("DATAPRO.INI","SQLSELECTCLASS",.F.)

//? VALTYPE(oDp:lSqlSelectClass),oDp:lSqlSelectClass

  IF oDp:lSqlSelectClass

    oSelect      :=EJECUTAR("DPSQLSELECTCLASS",cSql,::cTable,::cType,SELF,oDp:cDpXbaseLine,oDp:cDpXbaseLine)

    IF ValType(oSelect)="O"
      cSql:=oSelect:cSqlNew
    ENDIF

  ENDIF

  oDp:cDpXbaseLine:=::cDpXbaseLine

  ::cTable   :=IIF( EMPTY(::cTable),ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql))),::cTable)
  ::cTable   :=IIF(" " $::cTable,LEFT(::cTable,AT(" " ,::cTable)),::cTable)
  ::cTable   :=IIF(CRLF$::cTable,LEFT(::cTable,AT(CRLF,::cTable)),::cTable)
  ::cTable   :=STRTRAN(::cTable,"(","")
  ::cTable   :=ALLTRIM(::cTable) // 25/08/2023


  // 25/08/2023, Evitar caida por recursividad
  IF !Empty(oDp:aDefault)
     ::aDefault:=EJECUTAR("GETDEFAULT",::cTable) //25/08/2023
  ENDIF

  IF oOdbc:ClassName()="TODBC"

    oOdbc:lDateAsStr:=.T.


    DEFAULT oCursor  :=oOdbc:Query(cSql)

  ELSE


    // Si es MySql
    //  03/01/2017 IF (oDp:cTypeBD="MYSQL" .AND. oDp:lNativo) .OR. oOdbc:ClassName()<>"TODBC"

    IF oOdbc:ClassName()<>"TODBC"

       // no se puede aplicar con Conexiones remotas
       IF !oOdbc:oConnect:lRemote .AND. (::cTable=="DPPROGRA" .OR. ::cTable=="DPTABLAS") //  .OR. "DPPROGRA"$cSql
 // ? ::cTable,cSql,"::cTable",cSql,::cTable=="DPPROGRA"
          oOdbc:=OPENODBC(oDp:cDsnConfig)
       ENDIF

       // 08/09/2013 Para conexiones Con otros Servidores
       oCursor:=MyTable(cSql,.T.,oOdbc:cDsn,NIL,oOdbc)
       ::aFields:=ACLONE(oCursor:aFields)
       ::aFieldsTable:=ACLONE(::aFields)

       // oCursor:End(.T.) // jn 21/06/2021, evaluar que no se bloquee cuando se procesos

/*
    ELSE

       ::aFields     :=ACLONE(oCursor:aFields)
       ::aFieldsTable:=ACLONE(::aFields)

// ? ValType(::aFields),LEN(::aFields)
*/
    ENDIF

/*
    DEFAULT oOdbc  :=GetOdbc(cTable),;
            oCursor:=oOdbc:Query(cSql)

    ::aFields:=ACLONE(oCursor:aFields)
    ::aFieldsTable:=ACLONE(::aFields)

? "AQUI ES CAMPOS"
*/
/*
  ELSE

    // ? "DESDE MYSQL",oDp:lMySql

    oCursor:=MyTable(cSql, .T. )
    oOdbc  :=oCursor:oDataBase
    ::cType:=oDp:cTypeBD
    ::lMySqlNativo:=.T.

    //      ? "AQUI LEE LOS DATOS"
*/

  ENDIF

  oDp:cMsgOdbc:=""
  ::nSeconds:=SECONDS()


  ::lAppend  :=.F.
  ::oCursor  :=oCursor
  ::oOdbc    :=oOdbc
  ::cSql     :=cSql

  DEFAULT ::aDataFill:={}

  ::lFound   :=.F.

  ::aVars    :={}
  ::nPos     :=0
  ::cName    :=""

  ::cTable   :=IIF( EMPTY(::cTable),ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql))),::cTable)
  ::cTable   :=IIF(" " $::cTable,LEFT(::cTable,AT(" " ,::cTable)),::cTable)
  ::cTable   :=IIF(CRLF$::cTable,LEFT(::cTable,AT(CRLF,::cTable)),::cTable)
  ::cTable   :=STRTRAN(::cTable,"(","")

//  IF "UNION"$cSql
//? ::cTable,cSql
//  ENDIF

  IF lLower .OR. ::oOdbc:lTableLower
     cTable:=LOWER(::cTable)
     //::cSql  :=STRTRAN(::cSql,"FROM "+UPPER(cTable),"FROM "+LOWER(cTable))
     ::cSql  :=TABLELOWER(::oOdbc,::cSql)
  ENDIF

  ::cFileName:=::cTable
  ::cWhere     :=GetSqlWhere(::cSql)
  oDp:cWhere   :=::cWhere

  IF nOption=1

     // ::cSql:=STRTRAN(::cSql,::cWhere," 1=0 ")
     //
     // ? ::cWhere,::cSql,GetProce()

      ::cSql:=::cSql + IIF( oDp:cTypeDb="MYSQL" .AND. !" LIMIT "$::cSql , " LIMIT 0 " , "" )

   //  IF !" 1=0"$cSql
   //    ? ::cSql,GetProce()
   //  ENDIF

  ENDIF

  DEFAULT ::cType      :=::oOdbc:cType

  IF !EMPTY(oDp:cMsgOdbc)
     ::cError:=oDp:cMsgOdbc
     RETURN SELF
  ENDIF

//  ? cSql

//  IF UPPE(::oCursor:ClassName())=UPPE("TDbOdbcDirect") // Originado directo

     IF !oDp:lNativo .AND. Empty(::aFields) // ::oOdbc:cType="MYSQL"
       DEFAULT ::aFields :=oCursor:CursorFields( oCursor:hStmt )
     ENDIF

     // ? ValType(::aFields),LEN(::aFields), oDp:lNativo
     // ViewArray(::aFields,NIL,.F.)

     IF !oDp:lNativo .AND. ASCAN(::aFields,{|a,n|a[3]=0})>0 .AND. Empty(::aFields) // Problemas del ODBC
        ::aFields:=MyStruct(::cSql,::aFields)
     ENDIF
     //::axStruct:= MMxStruct( ::hMySt )

     DEFAULT ::aFields:={}

     AEVAL(::aFields,{|a,n|::aFields[n,1]:=ALLTRIM(UPPE(a[1]))})

     // ? LEN(::aFields),"::aFields",GetProce()

     ::lMemo               :=ASCAN(::aFields,{|a|a[2]="M"})>0
     nLen                  := len(::aFields)
     ::lMemo               :=ASCAN(::aFields,{|a|a[2]="M"})>0


     IF !(::cType="MYSQL" .AND. oDp:lNativo) .OR. oOdbc:ClassName()="TODBC"
     // IF !::oOdbc:cType="MYSQL"

        ::oCursor:aBuffer     := Array(nLen)
        ::oCursor:aIsNull     := Array(nLen)
        ::oCursor:aScanFields := Array(nLen)
        ::oCursor:Load() // Carga los Datos

//        ::aFields     :=ACLONE(oCursor:aFields)
//        ::aFieldsTable:=ACLONE(::aFields)
//
// ? "AQUI ES MSSQL",oDp:cTypeBD,LEN(::aFields),ValType(::aFields),oCursor:ClassName)

     ENDIF

     lLoad  :=.T.

     DEFAULT ::aDataFill:={}

     IF nOption!=1
       ::aFill()
     ENDIF

     // TODBC LEEE LOS CAMPOS LUEGO DE ::aFILL
     // ? oOdbc:ClassName(),"AQUI ES MSSQL"

     IF oOdbc:ClassName()="TODBC"

        If Empty(oCursor:aFields)
          ::aFill()
        ENDIF

        ::aFields     :=ACLONE(oCursor:aFields)
        ::aFieldsTable:=ACLONE(::aFields)

     ENDIF

     ::nArrayAt:=1
     ::nRecno  :=1

     DEFAULT ::aDataFill:={}
/*
     IF oOdbc:ClassName()="TODBC"

       oOdbc:lDateAsStr:=.T

       DEFAULT oCursor  :=oOdbc:Query(cSql)

       ::aDataFill:=ACLONE(oCursor:aFill())
       ::aFields  :=ACLONE(oCursor:aFields)

ViewArray(::aDataFill)
ViewArray(::aFields)

     ENDIF
*/
     ::lAppend :=LEN(::aDataFill)=0 // Si está Vacio Indica Agregar
     ::lUpdate :=!::lAppend // 21/06/2018

/*  ELSE

     ::oOdbc   :=oCursor:oOdbc
     ::cTable  :=oCursor:cFileName
     ::aFields :=oCursor:aFields

     ? "ESTA FORMA ESTA DEROGADA"

  ENDIF */

  ::nSeconds:=SECONDS()-::nSeconds // Cantidad de Segundos

  DEFAULT ::oOdbc   :=oCursor:oOdbc

  ::aBuffers:={}
  oDp:nRecCount:=::RecCount()

  // __objAddData( Self, "NUEVOCAMPO","NUEVO" )

  FOR I := 1 TO LEN(::aFields)

     IF ValType(::aFields[I,1])="C"

        IF lLoad
           uValue:=::oCursor:aBuffer[i]
        ELSE
           uValue:=::oCursor:FieldGet(i)
        ENDIF

//      uValue:=IIF( nOption!=1,::_FieldGet(i),uValue)

        // uValue:=IIF( ValType(uValue)="U" , CTOEMPTY(uValue,::aFields[I,2] , ::aFields[I,3]) , ::_FieldGet(i) )
        uValue:= CTOEMPTY( nil  ,::aFields[I,2] , ::aFields[I,3] )

//if ValType(uValue)="U"
//   ? "INDEFINIDO, UVALUE ",GetProce()
// ENDIF

        AADD(::aBuffers,{uValue,ALLTRIM(UPPE(::aFields[I,1])),::aFields[I,2],::aFields[I,3],uValue})

     ENDIF

  NEXT
/*
  IF "DPINV"$::cSql
     MensajeErr("este es")
     ViewArray(::aFields,,,.f.)
     ?  ValType( ::aBuffers ),  ErrorSys(.t.) , ValType(::aBuffers[1,1])
     ViewArray(::aBuffers,,.f.,.F.  )
  ENDIF
*/

  IF !Empty(cPrimary)

    ::cPrimary:=cPrimary
    ::nOrder  :=1

  ELSE

    /*
    aIndex:=::oOdbc:GetIndex(UPPE(ALLTRIM(::cTable)))

    DEFAULT aIndex:={}

    IF Len(aIndex)>1 .AND. "PRIMARY"=UPPE(ALLTRIM(aIndex[1]))
       ::cPrimary:=aIndex[2]
    ELSEIF LEN(aIndex)>1
       ::cPrimary:=::oOdbc:IndexKey(::cTable,1)                     // JN 29/12/2002
    ENDIF

//    ? ::cPrimary,"::cPrimary,TTABLE",;
//      ::cPrimary:=::oOdbc:IndexKey(::cTable,1),"LEN(aIndex)",LEN(aIndex),::cTable                     // JN 29/12/2002

    IF EMPTY(::cPrimary)
       ::cPrimary:=::oOdbc:IndexKey(::cTable,1)                     // JN 29/12/2002
    ENDIF

    IF EMPTY(::cPrimary) .AND. !EMPTY(::aFields)
       ::cPrimary:=::aFields[1,1] // Asume el primer campo con Primary
    ENDIF

    */
    ::nOrder:=1

  ENDIF

  ::cFieldOrder:=::cPrimary

  ::InitFields() // Inicializa los Campos para poder ser Leidos

  ::aRecord:=ARRAY(LEN(::aFields))

  IF Empty(::aDataFill) // no hay Campos, hay que crear campos Vacios

     ::lFound:=.F.
     ::nRecno:=0 // Es EOF()

     FOR I:= 1 TO LEN(::aFields)
        ::ADD(::aFields[I,1],CTOEMPTY(NIL,::aFields[I,2],::aFields[I,3]))
     NEXT

  ELSE
     // Asigna todos sus Valores como Variables
     ::lFound:=.T.
     FOR I:= 1 TO LEN(::aFields)
        ::ADD(::aFields[I,1],::aDataFill[1,i])
     NEXT

     ::GoTop()

  ENDIF

  ::CTOASLOG()

/* // JN 20/08/2013
  // Convierte un Campo Lógico (SELECT 1 AS LOGICO, 0 AS LOGICO2...)
  WHILE .T.

    nAt:=ASCAN(::aFields,{|a,n| n>nAtX .AND. LEFT(a[1],5)="LOGIC" .AND. a[2]="N" .AND. a[3]=1})

    IF nAt>0
       ::CTOLOG(::aFields[nAt])
       AEVAL(::aDataFill,{|a,n| ::aDataFill[n,nAt]:=(a[nAt]=1 ) })
       ::aFields[nAt,3]:="L"
    ELSE
       EXIT
    ENDIF

    nAtX:=nAt

   ENDDO
*/
  //? "TTABLE",cSQL,::cTable
  //::nHandler:=nTablas++
  //
  // AADD(aTablas,{::nHandler,Self})

//  ::oCursor:End()
  oOdbc  :=NIL
//  ::oCursor:=NIL

Return Self

METHOD ISFIELD(cField) CLASS Ttable
//
//   cField:=UPPER(ALLTRIM(cField))
//RETURN ASCAN(::aFields,{|a,n|a[1]==cField})>0
RETURN ::FIELDPOS(cField)>0

METHOD NewRecord() CLASS Ttable

   LOCAL aData:={}

   IF ::RecCount()>0
      aData:=::aDataFill[1]
   ENDIF

   ::Append()

   IF !Empty(aData)
      AEVAL(::aFields,{|a,n| ::Replace( a[1], aData[n]) })
   ENDIF

RETURN .T.

/*
// Totalizar Columna del Browse
*/
METHOD TotalField(cField)  CLASS Ttable

  LOCAL nTotal:=0,nField:=::FieldPos(cField)

  IF nField>0 .AND. ::aFields[nField,2]="N"
     AEVAL(::aDataFill,{|a,n|nTotal:=nTotal+a[nField]})
  ENDIF

RETURN nTotal

/*
// Salto de Registro
*/
METHOD  Skip(nSkip,lView) CLASS tTable

   DEFAULT nSkip:=1,lView:=.F.

//IF lView
//   ? ::nRecno,"ANTES"
   ::nRecno:=MAX(1,::nRecno)
//   ? ::nRecno,"MAX(1"
   ::nRecno:=MIN(LEN(::aDataFill),::nRecno)
//   ? ::nRecno,"MIN"
   ::Goto(::nRecno+nSkip)
//   ? ::nRecno,"LISTO"
//ELSE
//   ::Goto(::nRecno+nSkip)
//ENDIF
RETURN .T.

/*
// Retorna Totales
*/
METHOD ATOTAL(cField) CLASS tTable
    LOCAL nField:=::FieldPos(cField)

    // ::aTotales:=ATOTALES(::aArrayData)
    ::aTotales:=RunMacro("ATOTALES(oPar1)",::aArrayData)  // Funcion dpwin32.prg
    //     ATOTALES(aData,bBlq,nColIf)

RETURN IF(nField>0,::aTotales[nField],0)

METHOD SeekBin(uValue,cField,lSoftSeek) CLASS tTable
RETURN ::DbSeek(uValue,lSoftSeek,cField)

METHOD  AddRecord(lEmpty) CLASS tTable
   LOCAL aData:={}

   DEFAULT lEmpty:=.F.

   AEVAL(::aFields,{|a,n|AADD(aData,::Get(a[1]))})

   IF lEmpty
      AEVAL(aData,{|a,n|aData[n]:=CTOEMPTY(a)})
   ENDIF

   AADD(::aDataFill,ACLONE(aData))

//   IF lEmpty JN 06/12/2013
      ::GoBottom()
//   ENDIF
   // ::nRecno:=LEN(::aDataFill)

RETURN NIL // aData

METHOD CLONE(lMSg)
  LOCAL oTable

  DEFAULT lMsg:=.T.

  IF !("*"$::cSql)
     IF lMsg
       MensajeErr("Clonar Registro requiere SELECT * FROM "+::cTable)
     ENDIF
     RETURN NIL
  ENDIF

  IF ::RecCount()=0
     IF lMsg
       MensajeErr("No hay Registros para clonar")
     ENDIF
     RETURN NIL
  ENDIF

  oTable:=OpenTable("SELECT * FROM "+::cTable,.F.)
  oTable:AppendBlank() // Indica que va a Incluir
  AEVAL(oTable:aFields,{|a,n| ::oTable:FieldPut(n,oTable:FieldGet(n))})

RETURN oTable
/*
// Revisa el Dsn, En Ads puede Ser Cerrado
*/
METHOD ChkDsn()

   IF oDp:lNativo // oDp:lMySqlNativo
      RETURN .T.
   ENDIF

   IF ValType(::oOdbc)!="O" .OR. ::oOdbc:lAbort .OR. ::oOdbc:AllocStatement()=0
      ::oOdbc:=GetOdbc(::cTable)
      // ::oOdbc:=OpenOdbc(::oOdbc:cDsn)
   ENDIF

RETURN .T.

/*
// Agrega Campo Virtual
*/
METHOD  AddField(cField,cType,nLen,nDec,uValue) CLASS tTable // Agregar un Campo Virtual
   LOCAL nAt
// ,uValue  JN 10/04/2014

   cField:=ALLTRIM(UPPE(cField))

   DEFAULT cType:="C",nLen:=0,nDec:=0

   DEFAULT uValue:=CTOEMPTY(uValue,cType,nLen)

RETURN ::AddFields(cField,uValue,cType,nLen,nDec)
/*

   nAt:=ASCAN(::aFields,{|a|a[1]=cField})

   IF nAt=0
      AADD(::aFields ,{cField,cType,nLen,nDec})
      AADD(::aBuffers,{NIL,cField,cType,nLen})
      AEVAL(::aDataFill,{|a,n|AADD(::aDataFill[n],uValue)})
      RETURN .T.
   ENDIF
*/
RETURN .F.


// Genera el picture del Campo
METHOD  GetPicture(nField,lMiles) CLASS tTable
   LOCAL cPicture:=NIL,cNew:="",aChar
   LOCAL nLen,nDec,I,nTres:=0

   DEFAULT lMiles:=.F.

   IF ValType(nField)="C"
      nField:=::FieldPos(nField)
   ENDIF

   nLen:=::aFields[nField,3]
   nDec:=::aFields[nField,4]

RETURN BuildPicture(nLen,nDec,lMiles)

/*

   cPicture:=REPLI("9",nLen-nDec+IIF( nDec>0,1 ,0 ))+;
             IIF( nDec>0, "." + REPLI("9",nDec),"")


   IF lMiles

      nDec:=AT(".",cPicture)
      nDec:=IIF( nDec=0,LEN(cPicture) ,nDec-1 )

      FOR I := nDec TO 1 STEP -1
        cNew:="9"+IIF( nTres>2,(nTres:=0,","),"")+cNew
        nTres++
      NEXT

      IF (nDec :=AT(".",cPicture),nDec>0)
         cNew:=cNew+SUBS(cPicture,nDec,LEN(cPicture))
      ENDIF

      RETURN cNew

   ENDIF
//   ? nLen,nDec,nField,"nField",cPicture,"Picture"

RETURN cPicture
/

/*
// Obtiene el Total de una Columna
*/
METHOD GetTotal(cField) CLASS tTable
   LOCAL nTotal:=0,cSql,oTable,nAt
   nAt:=AT(" FROM ",::cSql)

   IF nAt>0
     cSql:="SELECT SUM("+cField+")"+SUBS(::cSql,nAt,LEN(::cSql))
     oTable:=OpenTable(cSql,.T.)
     nTotal:=oTable:FieldGet(1)
     oTable:End()
   ENDIF

RETURN nTotal

/*
// Busca el Mayor Valor del Registro
METHOD GotoMax(cField,cWhere,lMax) CLASS tTable
     LOCAL cSql,oTable,uValue,cWhere2,cFieldOrg:=cField //,oTableO:=SELF
     LOCAL aData,cSqlRun,nAt

     DEFAULT cField:=BUILDCONCAT(::cPrimary) // "CONCAT("+::cPrimary+")"
     DEFAULT lMax  :=.T.
     DEFAULT cWhere:=""

     ::oCursor:End() // EN SQL-SERVER SE REQUIERE

     uValue:=SqlGetMax(::cTable,cField,cWhere,NIL,lMax)


     ::cSql   :=LEFT(::cSql,AT(" FROM ",::cSql)+5)+" "+::cTable+;
                IIF(Empty(cWhere),""," WHERE "+cWhere) +;
              " ORDER BY "+cField+ IF(lMax," DESC ","")+" LIMIT 1 "

     ::Reload()
     ::GoTop()

     IF lMax
        ::lEof:=.T.
        ::lBof:=.F.
     ELSE
        ::lEof:=.F.
        ::lBof:=.T.
     ENDIF

RETURN !EMPTY(uValue)
*/


/*
// Busca el Mayor Valor del Registro
*/
METHOD GotoMax(cField,cWhere,lMax) CLASS tTable
     LOCAL cSql,oTable,uValue,cWhere2,cFieldOrg:=cField //,oTableO:=SELF
     LOCAL aData

     DEFAULT cField:=BUILDCONCAT(::cPrimary) // "CONCAT("+::cPrimary+")"
     DEFAULT lMax  :=.T.
     DEFAULT cWhere:=""

     IF !Empty(cWhere) .AND. !(" WHERE "$" "+cWhere)
        cWhere:=" WHERE "+cWhere
     ENDIF

     // cSql  :=SUBS(::cSql,AT(" FROM ",::cSql)+1)
//     cSql  :=" FROM "+::cTable+IIF(Empty(cWhere) , "" ,cWhere)
// JN 17/09/2014     cSql  :="SELECT "+IIF( lMax , "MAX(" , "MIN(" )+cField+") AS "+cField+" "+cSql

     cSql  :=" FROM "+::cTable+IIF(Empty(cWhere) , "" ,cWhere)
     cSql  :="SELECT "+IIF( lMax , "MAX(" , "MIN(" )+cField+") AS cField "+cSql


     ::oCursor:End() // EN SQL-SERVER SE REQUIERE

     oTable:=OpenTable(cSql,.T.,::oOdbc)

     uValue:=oTable:FieldGet(1)

     DEFAULT uValue:=CTOEMPTY(uValue,oTable:aFields[1,2],oTable:aFields[1,3])

     oTable:End()

     cWhere2:=cField+GetWhere("=",uValue)

     IF ALLTRIM(cWhere2)=ALLTRIM(STRTRAN(cWhere," WHERE ",""))
        cWhere:=""
     ENDIF

     cSql   :=LEFT(::cSql,AT(" FROM ",::cSql)+5)+" "+::cTable+;
              IIF(" WHERE "$cWhere, "" , " WHERE ")+cWhere+;
              IIF( EMPTY(cWhere) , "" , " AND " ) + cWhere2+;
              "" // " LIMIT 1"

     ::cSql:=cSql

// ErrorSys(.T.)
// MensajeErr(CLPCOPY(cSql),"GOTOMAX")

     ::Reload()
     ::GoTop()

    // ViewArray(::oTable:aArrayDataFill)

     IF lMax
        ::lEof:=.T.
        ::lBof:=.F.
     ELSE
        ::lEof:=.F.
        ::lBof:=.T.
     ENDIF

//  oTableO:=oTable // Copia el Objeto */
//  ? uValue,"uValue"

RETURN !EMPTY(uValue)

/*
// Busca el Siguiente/Anterior Partiendo de la Clave de cPrimary
*/
METHOD GotoSkip(nSkip,cField,cWhere) CLASS tTable
   LOCAL cWhere2,cSql,lMax,oTable,uValue

   DEFAULT cField:=::cPrimary
   DEFAULT cWhere:=""

   cSql   :=" FROM "+::cTable
   lMax   :=(nSkip<0)
   cWhere2:=BuildConcat(cField)+GetWhere( IIF( lMax, "<" , ">" ) ,::GetValKey(cField))

   cSql   :="SELECT "+IIF( lMax , "MAX(" , "MIN(" )+cField+") "+cSql+" WHERE  "+cWhere2
   oTable :=OpenTable(cSql,.T.)
   uValue :=oTable:FieldGet(1)

   oTable:End()

   cWhere2:=cField+GetWhere("=",uValue)

   IF !EMPTY(uValue)

     cSql   :=LEFT(::cSql,AT(" FROM ",::cSql)+5)+" "+::cTable+" WHERE "+cWhere+;
              IIF( EMPTY(cWhere),"" , " AND " ) + cWhere2

            //  +" LIMIT 1"

     ::cSql:=cSql
     ::Reload()
     ::GoTop()
     ::lBof:=.F.
     ::lEof:=.F.

   ELSE // Esta Vacio
     IF nSkip>0
        ::lEof:=.T.
        ::lBof:=.F.
     ELSE
        ::lEof:=.F.
        ::lBof:=.T.
     ENDIF
   ENDIF

RETURN .T.

/*
// Convierte todos los Campos 0 AS LOGICO en Logico
*/
METHOD CTOASLOG() CLASS tTable
  LOCAL nAt:=0,nAtx:= 0

  // Convierte un Campo Lógico (SELECT 1 AS LOGICO, 0 AS LOGICO2...)
  WHILE .T.

    nAt:=ASCAN(::aFields,{|a,n| n>nAtX .AND. LEFT(a[1],5)=="LOGIC" .AND. a[2]=="N" .AND. a[3]==1})

    IF nAt>0

// ? ::aFields[nAt,1], "CTOASLOG"
       ::CTOLOG(::aFields[nAt,1])
       /// AEVAL(::aDataFill,{|a,n| ::aDataFill[n,nAt]:=(a[nAt]=1 ) })
       // ::aFields[nAt,3]:="L"
    ELSE
       EXIT
    ENDIF

    nAtX:=nAt

   ENDDO

RETURN NIL

/*
// Determina la Clave Siguiente
*/
METHOD GetNext(cField,uValue,lMax) CLASS tTable
     LOCAL cSql,oTable

     DEFAULT cField:=BUILDCONCAT(::cPrimary) // "CONCAT("+::cPrimary+")"
     DEFAULT lMax  :=.T.
     DEFAULT uValue:=::GetValKey(cField)

     cSql  :="SELECT "+IIF( lMax, "MIN(" ,"MAX(" )+cField+") FROM "+::cTable+;
             " WHERE "+cField+GetWhere(IIF( lMax , ">" , "<" ),uValue)

     oTable:=OpenTable(cSql,.T.)
     uValue:=oTable:FieldGet(1)

     oTable:End()

RETURN uValue

/*
// Muestra mensajes de Error del Query
*/

METHOD ShowError(cTitle) CLASS tTable

   IF EMPTY(::cError)
      RETURN .F.
   ENDIF

   DEFAULT cTitle:=""

   MsgAlert(::cError+CRLF+"SENTENCIA:"+CRLF+::cSql,cTitle+" Error TABLE/SQL")

RETURN .T.
/*
// Realiza la Búsqueda en una Tabla, ya existente, asociada al Formulario
// Utilizada en Los Formulario y Control BMPGET
*/
METHOD SeekTable(cField,oGet,cWhere,oSay) CLASS tTable
    LOCAL cSql,oTable,oObj,cFieldWhere
    LOCAL uValue:="" // Eval(oGet:bSetGet)

    IF ValType(oGet)="O"
       // oGet:KeyBoard(13)
       uValue:=EVAL(oGet:bSetGet)
    ELSE
       uValue:=oGet
    ENDIF

//  ? oGet:nLastKey, "oGet:nLastKey"

    IF !(oGet:nLastKey=13 .OR. oGet:nLastKey=9) // Presionado CANCEL
       RETURN .T.
    ENDIF

    cSql:=LEFT(::cSql,AT(" WHERE ",UPPE(::cSql))+6)

    // ? cWhere,"cWhere"

    // Si envia el Where
    IF ValType(cField)="C" .AND. (","$cField .OR. "+"$cField) .AND. !"CONCAT("$UPPE(cField)
      cField:= STRTRAN(cField,"+",",")
      cField:=BuildConcat(cField) // " CONCAT("+cField+")"
    ENDIF

    // ? cSql,cWhere

    IF cWhere=NIL
       cSql+=cField+GetWhere("=",uValue)  // +" LIMIT 1" // Optimiza y Busca un Solo Registro
    ELSE
       cSql+=" "+cField+GetWhere("=",cWhere) //+" LIMIT 1" // Optimiza y Busca un Solo Registro
    ENDIF


//    ? cSql

    oTable:=OpenTable(cSql,.T.) // Nueva Consulta

    // ? oTable:cSql,"SEEKTABLE"

    ::lFound   :=oTable:RecCount()>0
   //  ? ::lFound,oTable:RecCount(),oTable:cSql
    ::aDataFill:=oTable:aDataFill
    ::GoTop()

    IF ValType(oSay)="O"
       oSay:Refresh()
    ENDIF

    oTable:End()

    // oGet:oWnd:nLastKey:=0
    // oGet:nLastKey     :=0

    IF !::lFound .AND. oGet:ClassName()="TBMPGET" // Ejecutará el Action del Boton

       oGet:oJump:=oGet:oBmp
       // oObj:=EVAL(oGet:bAction)
       oGet:KeyBoard(VK_F6)
       oGet:oWnd:nLastKey:=0
       oGet:nLastKey     :=0

       IF oObj:ClassName()="TDPLBX"
         oObj:=oObj:oBrw // Tengo el Browse
         DpFocus(oObj)   // Forza el Focus del Nuevo browse
         oGet:oJump:=oObj
       ENDIF

       RETURN .T. // Es un Cambio de Foco, y No Una Negación

    ENDIF

RETURN ::lFound

/*
// Borra todos los Registros de la Tabla
*/

METHOD DeleteAll(lIni) CLASS tTable
   LOCAL cWhere

   DEFAULT lIni:=.T.

   ::GoTop()
   WHILE !::Eof()
      cWhere:=::GetWhereKey(::cPrimary,.T.)
      // ? cWhere,"DESDE DELETEALL"
      ::Delete(cWhere,lIni,.T.)
      ::DbSkip(1)
   ENDDO
RETURN .T.



/*
METHOD DbSeek2(cKey,uValue,oTable)
    LOCAL cSql   :="SELECT "+cKey+" FROM "+::cTable+" WHERE "+cKey+" = '"+uValue+"'"
    LOCAL lFound :=.F.
    LOCAL oTable :=::oOdbc:Query(cSql)
    LOCAL aData  :=oCursor:Afill()

    ? cSql,oCursor:ClassName(),LEN(aData)

RETURN LEN(aData)>0
*/

/*
// Elimina un Registro de la Tabla
*/
METHOD Delete(cWhere,lIni,lReload) CLASS tTable
   LOCAL cSql,lResp:=.F. ,nT1

   aRuta:={}

   DEFAULT cWhere :=::GetWhereKey(NIL,.T.)
   DEFAULT lReload:=.F.
   DEFAULT lIni   :=.T.

   cWhere:=IIF("WHERE "$(" "+cWhere) ,"" , " WHERE ")+cWhere

   IF DeleteJoin(::cTable,lIni,cWhere)
   // ? "BORRO EL GRUPO ASOCIADO"
   // Return .T.
   ENDIF

   cSql:="DELETE FROM "+::cTable+" "+cWhere

   // ? cSql

   oDp:cMsgOdbc:=""

   IF oDp:lTracerSql
      ? cSql,"Method Delete "
   ENDIF

   // ? "PARA BORRAR ",cSql
   IF ValType(::oOdbc)!="O"
     ::oOdbc:=OpenOdbc(::cTable)
   ENDIF

   nT1:=SECONDS()

   IF oDp:lMySqlNativo
     lResp:=::oOdbc:ExecSQL(cSql,"DELETE")
   ELSE
     lResp:=::oOdbc:Execute(cSql,"DELETE")
   ENDIF

   IF !lResp // !EMPTY(oDp:cMsgOdbc)

      MemoWrit("ERROR.SQL",cSql)
      MemoWrit("ERROR.TXT",GetProce())

      MensajeErr("Posiblemente posee datos Vinculados con Otras Tablas","Registro no pudo ser Borrado")

      lResp:=.F.

//      MsgErrSql(cSql,"No se Puede Borrar el Registro")
/*
      MensajeErr(cSql,"No se puede Borrar el Registro") //uede borrar ? oDp:cMsgOdbc,cSql
      CLPCOPY(cSql)
      MemoWrit("DELETE.SQL",cSql)
*/
   ELSEIF lReload
      ::Reload()
      ::GoTop()
   ENDIF

RETURN lResp

/*
// Obtiene uno o varios campos de la tabla
*/
METHOD  GetValue(cField)  //INLINE ::FieldGet(::FieldPos(cField))
  local aFields:=_VECTOR(cField,IIF("+"$cField,"+",",")),I
  local uValue :=""

  IF Len(aFields)=1
     return ::FieldGet(cField)
  ELSE // Debe ser Lista de Cadenas
     AEVAL(aFields,{|a,i|uValue:=uValue+CTOO(::FieldGet(a),"C")})
  ENDIF

RETURN uValue

/*
// Obtiene el Valor de la Clave, según la Tabla Actual
*/
METHOD  GetValKey(cFieldKey) CLASS tTable  // Recarga nuevamente el Query
   LOCAL uValue:="",aFields,cSigno:="+",cData:="",I
   LOCAL cSql,oTable

   DEFAULT cFieldKey:=::cPrimary

   // cFieldKey:=STRTRAN(cFieldKey,"+",",")
   // cSigno:=IIF( ","$cFieldKey,"," ,cSigno )
   // Version Concatena
   /*
   IF ("+"$cFieldKey .OR. ","$cFieldKey) .AND. !"CONCAT("$UPPE(cFieldKey)
      cFieldKey:=STRTRAN(cFieldKey,"+",",")
      cFieldKey:="SELECT CONCAT("+cFieldKey+") AS UVALUE "
      cSql:=cFieldkey+Subs(::cSql,AT("FROM",UPPE(::cSql)),LEN(::cSql))
      oTable:=OpenTable(cSql,.T.)
      RETURN oTable:FieldGet(1)
   ENDIF */

   // ? cFieldKey,"GetValKey"

   aFields:=OnLyFields(cFieldKey)

   IF !::IsDef(aFields[1])
       ? GetProce()
       ? cFieldKey,"Campo: "+aFields[1]+" No Existe en Tabla "+::cTable
       RETURN NIL
   ENDIF

   // MsgList(aFields)

   uValue :=::Get(aFields[1])

   IF LEN(aFields)>1
      uValue:=STRSQL(uValue)
   ENDIF

//   ? cFieldKey,::cSql

   IF ValType(uValue)="U"
      ? "Campo ",aFields[1],uValue,cFieldKey
   ENDIF

   // MsgList(aFields)

   FOR I := 2 TO LEN(aFields)
     //? I,aFields[I],uValue,ValType(uValue)
     cData:=::Get(aFields[I])
     cData:=STRSQL(cData) // Convierte en SQL
     // ? "cData",ValType(cData)
     IF ValType(cData)=Valtype(uValue) .AND. ValType(uValue)!="U"
        uValue+=cData
     ELSE
        ? "Campo:",aFields[I],cData,cFieldKey
     ENDIF
   NEXT

//   ? "VALOR FINAL",uValue,cFieldKey
//   AEVAL(aFields,{|a,i|uValue:=uValue+::Get(aFields[I])})

RETURN uValue

/*
// Genera Where Segun los Campos y Claves
*/
METHOD  GetWhereKey(cFieldKey,lTable) CLASS tTable  // Recarga nuevamente el Query
   LOCAL uValue:="",aFields,cSigno:="+",cWhere:="",I,cResult

   DEFAULT cFieldKey:=::cPrimary
   DEFAULT lTable   :=.F.

   // cSigno:=IIF( ","$cFieldKey,"," ,cSigno )
   cFieldKey:=STRTRAN(cFieldKey,"+",",")
   aFields  :=_VECTOR(cFieldKey) // ,cSigno)

   // ? cFieldKey,"GETWHEREKEY",len(aFields)

   FOR I:= 1 TO LEN(aFields)
       IF !::IsDef(aFields[I])
         MensajeErr("Campo:"+aFields[I]+" No existe, en :  "+::cTable+CRLF+GetProce(),"GetWhereKey() "+cFieldKey)
       ELSE
         uValue:=::Get(aFields[I])

//         ? uValue,aFields[I],Valtype(uValue),cWhere,i,ValType(cWhere),::cTable,;
//           aFields[I]+GetWhere("=",uValue)

        cResult:=aFields[I]+GetWhere("=",uValue)

   //     ? cResult,"cResult"

         cWhere:=cWhere+ IIF( EMPTY(cWhere),"" , " AND " )+;
                 IIF(lTable,ALLTRIM(::cTable)+".","")+;
                 cResult // aFields[I]+GetWhere("=",uValue) // " = '"+CTOO(uValue,"C")+"'"
       ENDIF
   NEXT

RETURN " WHERE "+cWhere

METHOD Reload() CLASS tTable  // Recarga nuevamente el Query
  LOCAL aData,I,oTable:=Self,oNew,uValue

  DEFAULT ::aAddFields:={}

  ::cSql:=BuildLimit(::cSql,::cType)

  //IF ValType(::oOdbc)!="O"
  //   ::oOdbc:=OPENODBC(::cTable) // Apertura del DSN
  //ENDIF

  ::oCursor:End() // Cierra el que estaba Antes

  DEFAULT ::oOdbc:=GetOdbc(::cTable)

  IF ::oOdbc:cType="MYSQL" // ::lMySqlNativo

    ::aDataFill:=ASQL(::cSql,::oODbc)

    // ? "DESDE RELOAD"
    // ViewArray(::aDataFill)
    // ::oCursor:=MyTable(::cSql)
    ::aFields :=::oCursor:CursorFields( ::oCursor:hStmt )
// ? LEN(::aFields),"::aFields en Reload"

  ELSE

    ::ChkDsn()
    // ::oCursor:=::oOdbc:Query(::cSql)

    IF ::oOdbc:cType="MYSQL"
       // ::aFields:=::oCursor:aFields
       ::oCursor:aDataFill:=::oOdbc:Query(::cSql)
    ELSE
       ::oCursor:=::oOdbc:Query(::cSql)
       ::aFields :=::oCursor:CursorFields( ::oCursor:hStmt )
       ::aFill()
    ENDIF

    ::oCursor:End()  // Cierra nuevamente la ConexioN

  ENDIF

  // Rehace el Buffers y Campos 27/08/2003
  ::aBuffers:={}
  FOR I := 1 TO LEN(::aFields)
    uValue:=CTOEMPTY(NIL , ::aFields[I,2] , ::aFields[I,3] )
    __objSendMsg(Self, ::aFields[I,1] , uValue)
  //? uValue,ValType(uValue),"RELOAD"
    AADD(::aBuffers,{uValue,ALLTRIM(UPPE(::aFields[I,1])),::aFields[I,2],::aFields[I,3]})
  NEXT I

  AEVAL(::aAddFields,{|cField,n|::AddFields(cField,NIL)}) // Reasigna los Campos Nuevos
  ::aRecord:=ARRAY(LEN(::aFields))
  ::GoTop()

RETURN ::aDataFill


/*
* Tranfiere el Valor de Cada Linea en los Valores de los Campos
*/

METHOD UpdateFields() CLASS tTable

   DEFAULT ::aFields:={}

   // ? LEN(::aFields),ValType(::aFields)

   IF EMPTY(::aDataFill) .OR. ::nRecno<1 .OR. ::nRecno>LEN(::aDataFill) .AND. ValType(::aFields)="A"
      // Debe blanquear los valores como en DBF
      // ? ValType(::aFields)
      // ? LEN(::aDataFill),::nRecno,LEN(::aFields),"VS",LEN(::aRecord)
      ::aRecord:=ARRAY(LEN(::aFields))
      AEVAL(::aFields,{|a,i|::Set(a[1],CTOEMPTY(NIL,a[2],a[3]),;
                            ::aRecord[I]:=CTOEMPTY(NIL,a[2],a[3]);
                        )})

      RETURN NIL
   ENDIF

//   IF oDp:lMySqlNativo
//     ? LEN(::aDataFill),LEN(::aBuffers)
//   ENDIF
//   ? LEN(::aDataFill),"LEN DE aDataFill",::nRecno,
//   ? "aDataFill[1]",LEN(::aDataFill[1]),"::aFields",LEN(::aFields)

// oDp:oFrameDp:SetText(LSTR(LEN( ::aDataFill ))+ " LEN UPDATEFIELD " +::cTable)

   // ::aRecord:={}
   // JN 24/09/2015
   // IF !Empty(::aDataFill) .AND. ::nRecno>0

     AEVAL(::aDataFill[1],{|a,i| ::Set(::aFields[i,1],::aDataFill[::nRecno,I])})
     ::aRecord:={}
     AEVAL(::aDataFill[1],{|a,i|AADD(::aRecord,::aDataFill[::nRecno,I]),::aBuffers[I,1]:=::aDataFill[::nRecno,I]})

   // ENDIF
//  AADD(::aBuffers,{uValue,ALLTRIM(UPPE(::aFields[I,1])),::aFields[I,2],::aFields[I,3]})

RETURN NIL


METHOD DbSeek(uValue,lSoft,cField) CLASS TTable
   LOCAL nField,aKeys,aData:={},nLen,aFields:={},I
   LOCAL nAt:=0,cData,lFound:=.F.

   DEFAULT cField:=::cPrimary // FieldOrder

   IF  "+"$cField  .OR. ","$cField // Busca por Varios Campos

      cField:=STRTRAN(cField, "+",",")
      aKeys :=_VECTOR(cField, ",") // STRTRAN(cKey,"+","','")

      FOR I := 1 TO LEN(aKeys)

         nField:=::FieldPos(aKeys[I]) // Primer Campo

         IF nField>0 .AND. ValType(uValue)="C"
           nLen  :=::aFields[nField,3]
           AADD(aData  ,LEFT(uValue,nLen))
           AADD(aFields,nField)
           uValue:=SUBS(uValue,nLen+1,Len(uValue))
         ENDIF

      NEXT

      IF Empty(aData)
         nField:=::FieldPos(aKeys[1])
         AADD(aData  ,uValue)
         AADD(aFields,nField)
      ENDIF

      uValue:=IIF( ValType(uValue)="C",ALLTRIM(uValue),uValue)

      AEVAL(aData,{|a,i|uValue:=uValue+a}) // Rehace uData
      nAt   :=ASCAN(::aDataFill,{|aVal| aVal[aFields[1]] == aData[1] }) // Agiliza la Búsqueda

      IF nAt>0 .AND. ValType(uValue)<>"C"
         ::Goto(nAt)
         lFound:=.T.
         RETURN .T.
      ENDIF

// ? nAt,nAt,"posicion",ValType(uValue),uValue,cField," EN DBSEEK"

      nAt   :=MAX(1,nAt)

      FOR I := nAt TO LEN(::aDataFill)

         cData:=""
         AEVAL(aFields,{|nField,u|cData:=cData+CTOO(::aDataFill[I,nField],"C")})
         cData:=IIF(ValType(uValue)="C",ALLTRIM(cData),cData)

// ? cData,uValue,"AQUI ES DBSEEK"

         IF cData==uValue
            ::Goto(I)
            lFound:=.T.
            EXIT
         ENDIF
      NEXT

      RETURN lFound

   ENDIF

   nField:=::FieldPos(cField)

   IF ValType(uValue)="C" .AND. nField>0

     uValue:=ALLTRIM(uValue)

     nAt   :=ASCAN(::aDataFill,{|aVal| ALLTRIM(CTOO(aVal[nField],"C")) = uValue })

     IF nAt=0
       uValue:=ALLTRIM(uValue)
       nAt   :=ASCAN(::aDataFill,{|aVal| UPPE(ALLTRIM(CTOO(aVal[nField],"C"))) == uValue })
     ENDIF

     IF nAt=0 // .OR. .T.
       uValue:=ALLTRIM(uValue)
       nLen  :=LEN(uValue)
       nAt   :=ASCAN(::aDataFill,{|aVal| UPPE(LEFT(CTOO(aVal[nField],"C"),nLen)) == uValue })
     ENDIF

   ENDIF

   IF nAt>0
      ::Goto(nAT)
   ELSE
      // Debe mover el cursor hacia final del Archivo
      ::GoBottom()
      // Debe blanquear los valores como en DBF

      AEVAL(::aFields,{|a,i|::Set(a[1],CTOEMPTY(NIL,a[2],a[3]))})

   ENDIF

RETURN nAt>0


/*
// Genera el Registro Vacio en aRecord
*/
METHOD AppendBlank() CLASS TTable

   LOCAL I,uValue

   ::lAppend:=.t.
   ::lUpdate:=.F. // JN 21/06/2018

//   IF ValType(::aFields)!="A"
//      ::Reload()
//   ENDIF
//  DEFAULT ::aFields :=::oCursor:CursorFields( ::oCursor:hStmt )
//  ? ValType(::aFields),::cSql
//  ::aRecord:=ARRAY(Len(::aFields))
  AEVAL(::aFields,{|a,i|::aRecord[I]:=CTOEMPTY(NIL,::aFields[I,2],::aFields[I,3])})

  // 25/08/2023
  FOR I=1 TO LEN(::aDefault)

    uValue:=::aDefault[I,2]

    IF LEFT(uValue,1)="&"
      uValue:=MACROEJE(uValue)
    ENDIF

    ::Replace(::aDefault[I,1],uValue)

  NEXT I

RETURN NIL

/*
// Ejecuta Comando SQL sobre el Dsn
*/
METHOD Execute(cSql,cCommand) CLASS Ttable
   LOCAL lResp

   DEFAULT cCommand:="Execute"

   IF oDp:lMySqlNativo
     lResp:=::oOdbc:ExecSQL(cSql,cCommand)
   ELSE
     lResp:=::oOdbc:Execute(cSql,cCommand)
   ENDIF

RETURN lResp

METHOD Refresh() CLASS Ttable

   ::Reload()

// ? "AQUI ESTA APAGADO EL CURSOR"
/*
   if ::oCursor:hStmt != 0
      ::oCursor:DropStatement( ::oCursor:hStmt )
   endif

   ::oCursor:hStmt := ::oCursor:AllocStatement()

   if ::oCursor:Execute( ::oCursor:cSelect, "TDbOdbcDirect:Refresh()", ::oCursor:hStmt )
      ::aFill()
   ENDIF
*/

RETURN NIL

METHOD DBEVAL(bAction,bWhile,bFor) CLASS Ttable

   DEFAULT bAction:={||NIL},;
           bWhile :={||!::Eof()},;
           bFor   :={||.T.}

   ::Gotop()
   WHILE EVAL(bWhile)
      IIF( Eval(bFor), EVAL(bAction) , NIL )
      ::Skip(1)
   ENDDO

RETURN .T.


METHOD AddFields(cField,uValue,cType,nLen,nDec) CLASS Ttable
    LOCAL nAt  :=::FieldPos(UPPE(ALLTRIM(cField)))
    LOCAL nField

    DEFAULT cType     :=ValType(uValue)
    DEFAULT nLen      :=IIF(cType="C",LEN(uValue) ,0 )
    DEFAULT nDec      :=0

    DEFAULT ::aAddFields:={}  // Campos Agregados en ADDFIELDS
    DEFAULT ::aBuffers  :={}

    IF nAt=0

       // ? "NUEVO CAMPO",uValue,ALLTRIM(UPPE(cField)),cType,nLen,nDec

       AADD(::aAddFields,cField)

       AADD(::aBuffers,{uValue,ALLTRIM(UPPE(cField)),cType,nLen,nDec})
       AADD(::aFields ,{ALLTRIM(UPPE(cField)),cType,nLen,nDec})
       AADD(::aRecord,uValue)

       ::Add(cField,uValue)
       ::Set(cField,uValue)

       AEVAL(::aDataFill,{|a,n|AADD(::aDataFill[n],uValue)})

    ELSE

       // ? "AQUI ASIGNA",nAt,::aBuffers[nAt,2],"DEBE BUSCAR POR BUFFERS",uValue

       ::Set(cField,uValue)
       ::aBuffers[nAt,1]:=uValue

    ENDIF

RETURN nAt>0

METHOD FieldName(nPos) CLASS Ttable
    // LOCAL nAt:=ASCAN(::aBuffers,{|aVal| ALLTRIM(aVal[2]) == ALLTRIM(cField) })
RETURN IIF( nPos>0 , ::aBuffers[nPos,2] , NIL )

METHOD FieldGet(nPos) CLASS Ttable
   LOCAL cField:=nPos

   nPos:=IIF( ValType(nPos)="C",::FieldPos(nPos) ,nPos )

   IF ValType(cField)="C" .AND. nPos=0
      CLPCOPY(::cSql)
      MensajeErr(cField+" no Existe en "+::cTable+CRLF+::cSql+CRLF+GetProce(),"CLASS TTABLE")
      RETURN NIL
   ENDIF

   IF nPos>0 .AND. nPos>LEN(::aBuffers)

      DEFAULT ::cDpXbaseLine:=""

      MensajeErr("Posicion "+LSTR(nPos)+" supera Posición Method FieldGet CLASS TTABLE"+CRLF+GETPROCE(),"DpXbase:" +::cDpXbaseLine)

      RETURN NIL
   ENDIF

RETURN IIF( nPos>0 , ::aBuffers[nPos,1] , NIL )

METHOD _FieldGet(nPos) CLASS Ttable // LEE DESDE LA TABLA ORIGINAL
RETURN IIF( nPos>0 , ::aDataFill[::nArrayAt,nPos] , NIL )

METHOD FieldPos(cField) CLASS Ttable
     IF Valtype(cField)="N"
        RETURN cField
     ENDIF
     cField:=ALLTRIM(UPPE(cField))
RETURN ASCAN(::aBuffers,{|aVal| ALLTRIM(UPPE(aVal[2])) == cField })

METHOD GetDataKey(nOrder,cKey) CLASS TTable // JN
      LOCAL uValue:="",aKeys,nAt,I //,uData

      DEFAULT cKey:=::IndexKey(nOrder)

      IF EMPTY(cKey)
         RETURN NIL
      ENDIF

      IIF( EMPTY(::aDataFill) ,  ::aFill() , NIL )

      IF ::nRecCount()=0
         ::Gotop() // No hay Registros
      ENDIF

      IF EMPTY(::aDataFill)
         RETURN NIL
      ENDIF

      cKey   :=STRTRAN(cKey, "+",",")
      aKeys  :=_VECTOR(cKey, ",")

      FOR I := 1 TO LEN(aKeys)
        nAt   :=::FieldPos(aKeys[I])
        IF nAt=0
          MensajeErr("Campo "+aKeys[I]+" No existe")
          RETURN uValue
        ENDIF
        // ? nAt,aKeys[I]
        IF LEN(aKeys)=1
         // ? nAt,aKeys[I],::nRecno,LEN(::aDataFill)
          uValue:=::aDataFill[::nRecno,nAT]
          EXIT
        ENDIF
        //? uValue,aKeys[i]
        uValue:=uValue+CTOO(::aDataFill[::nRecno,nAT],"C")
      NEXT

RETURN uValue

METHOD GenSeek(xText,nOrder) CLASS TTABLE

   local cString,cKey
   local nFor, nField, nType, cValue, nLen,aIndex

   DEFAULT nOrder:=1

   if len(::GetIndex()) == 0
      return ""
   endif

 //  nField := ::aIndex[1][1]

   cKey:=::IndexKey(nOrder)

   aIndex:=_vector(cKey,"+")

   do case
      case Valtype(xText) == "N"
         cString := ::FieldName(nField) + " = " + ;
                    ltrim(str(xText, ::FieldDec(nField)))

      case Valtype(xText) == "D"
         cString := ::FieldName(nField) + " = {d '" +Str(Year(xText), 4)+ "-" +;
                    StrZero(Month(xText),2) + "-" + StrZero(Day(xText), 2) + "'}"

      case Valtype(xText) == "L"
         cString := ::FieldName(nField) + " = " + iif(xText, "1", "0")

      case Valtype(xText) == "C"

         cString := ""
//         cString := cKey + " = '" + xText + "'" // + iif(xText, "1", "0")


         for nFor = 1 to len(aIndex)

            // nField := ::aIndex[nFor][1]
            // ? nFor,aIndex[nFor]

            nField := ::Fieldpos(aIndex[nFor])
            nType  := ::Fieldtype(nField)
            nLen   := ::FieldLen(nField)

            cValue := Left(xText, nLen)

            do case
            case nType == "N"
                  cString += ::FieldName(nField) + " = " + Alltrim(cValue)
            case nType == "D"
                  cString += ::FieldName(nField) + " = {d '" +Str(Year(cValue), 4)+ "-" +;
                             StrZero(Month(cValue),2) + "-" + StrZero(Day(cValue), 2) + "'}"
            case nType == "L"
                  cString += ::FieldName(nField) + " = " + iif( "T"$Upper(cValue), "1", "0")
            otherwise
                  if Len(cValue) < nLen
                     cString += ::FieldName(nField) + " LIKE  '" + cValue +  "%'"
                  else
                     cString += ::FieldName(nField) + " = '" + cValue + "'"
                  endif
            endcase

            if !Empty(xText).AND.nFor>1
               cString += " AND "
            else
               exit
            endif

         next

   endcase

   // ? cString

return cString

//----------------------------------------------------------------------------//
// Prepara la Data para Ser Almacenada en la Base de Datos
//
METHOD ReplaceSpeed(cField,uValue, lMsg) CLASS Ttable
   LOCAL nAt:=::FieldPos(cField)

//   IF ValType(uValue)="C" .AND. (CHR(13)$uValue .OR. CHR(10)$uValue)
//      uValue:="PONCHAO"
//   ENDIF

   DEFAULT lMsg:=.T.

   IF nAt=0

      IF lMsg
        MensajeErr("Campo "+cField+" no Existe","Method ReplaceSpeed Ttable: "+::cTable)
        MensajeErr(GetProce(),"Procedimientos, Ejecutados")
      ENDIF

      RETURN NIL
   ENDIF

   IF ::cType$"ADS,DBF,MSSQL" .OR. ::lMemo
      ::nCountIns:=0
      ::nInsert  :=0
   ENDIF

   ::Replace(cField,uValue)

RETURN NIL

//----------------------------------------------------------------------------//
// Prepara la Data para Ser Almacenada en la Base de Datos
//

METHOD Replace(cField,uValue,nLen,nDec,uValueX,lSay) CLASS Ttable
   LOCAL lResult:=.F.
   LOCAL nAt:=::FieldPos(cField) // ASCAN(::aBuffers,{|aVal| UPPE(ALLTRIM(aVal[2])) == UPPE(ALLTRIM(cField)) })

   DEFAULT lSay:=.F.

   IF Empty(uValue) .AND. !Empty(uValueX)
      uValue:=uValueX
   ENDIF

   IF ValType(uValue)="C"
      nLen:=LEN(uValue)
      uValue:=ALLTRIM(uValue)
   ENDIF

   IF nAt>0
      ::aBuffers[nAt,1]:=uValue

      IF ValType(::aRecord)="A" .AND. nAt<=LEN(::aRecord)
         ::aRecord[nAt] :=uValue
      ELSEIF ValType(::aRecord)="A"
         MensajeErr("Campo "+cField+" no Existe en "+::cTable,"Clase "+::ClassName())
      ENDIF

      IF ValType(::aBuffers[nAt,3])!="C" .OR. ::aBuffers[nAt,3]="U" // No tiene Tipo
         ::aBuffers[nAt,3]:=ValType(uValue)
         IF nAt>Len(::aFields)
            ? "ERROR TTABLE",::aBuffers[nAt,1],::aBuffers[nAt,2],::aBuffers[nAt,3],;
            "ERROR EN ",LEN(::aFields),uValue
         ENDIF
         ::aFields[nAt,2] :=ValType(uValue)
      ENDIF

      ::Set(cField,uValue)

   ELSE

      IF ValType(uValue)="N" .AND. nLen=NIL
         nLen:=19
         nDec:=2
      ENDIF

      ::AddFields(cField,uValue,ValType(uValue),nLen,nDec)

      nAt:=LEN(::aFields)

   ENDIF

   ::Set(cField,uValue)

   IF ::Recno()>0 .AND. len(::aDataFill)>=::Recno()
      ::aDataFill[::Recno(),nAt]:=uValue
   ENDIF

RETURN nAt>0 // lResult

/*
// Grabar Registro
*/

METHOD Commit( cWhere , lReload , lJoin , lIni , lCreaIntRef) CLASS Ttable
   LOCAL cSql:="",uValue,cType,I,lRet:=.F.,oOdbc,cField,nCuantos:=0,cListFields:=""
   LOCAL oTable,aFields:={},nAt,oUpDate,nLen,nDec
   LOCAL oTableOld:=NIL // Tabla Anterior
   LOCAL cClave   :=""
   LOCAL cXSql
   LOCAL nT1:=SECONDS(),lTracer:=oDp:lTracer
   LOCAL cDpXbase:=::cDpXbaseLine
   LOCAL aBuffers:={}

   // Methodo APPEND si recno() es vacio es lUpdate:=.f., lAppend:=.t.,  jn 01/07/2017
   // ?  ::lUpdate , Empty(cWhere),Empty(::cWhere)," ::lUpdate .AND. Empty(cWhere) .AND. !Empty(::cWhere)"   ,::lAppend,"::lAppend",::cWhere,"::cWhere",cWhere,"cWhere"

   IF !Empty(cWhere)
      ::lUpdate:=.T.
      ::lAppend:=.F.
      ::cWhere :=cWhere
   ENDIF

   IF ::lUpdate .AND. Empty(cWhere) .AND. !Empty(::cWhere)
      cWhere:=::cWhere
   ENDIF

   DEFAULT oDp:oRunLine:=NIL

   IF oDp:oRunLine<>NIL
      cDpXbase:=oDp:oRunLine:oFunction:oScript:cProgram+" Function "+oDp:oRunLine:oFunction:cName+oDp:oRunLine:cLine
   ENDIF

//  DEFAULT lCreaIntRef:=.F.

   //DEFAULT oDp:lCreaIntRef:=GETINI("DATAPRO.INI","CREAINTREF",.T.),;
   //        lCreaIntRef    :=oDp:lCreaIntRef

   // oDp:lChkIntRef:=oDp:lCreaIntRef
   // Crea Integridad Referencial
   // 04/02/2016, Llamado desde la clase TDPEDIT Method save genera incidencia en UNLOCK DPLINK


   // Metod load debe ejecutarlo Previamente
//   IF ::lCreaRegIntRef
//     ::CreaRegIntRef()
//   ENDIF


//IF "DPPERSONAL"$::cTable
//? cDpXbase,"COMMIT" ,oDp:oRunLine:cLine
//   cDpXbase:=oDp:cDpXbaseLine // oDp:oRunLine:cLine // oDp:oRunLine:oFunction:oScript:cProgram+":"+oDp:oRunLine:oFunction:cName+"("+oDp:oRunLine:cLine+")"
//   ? cDpXbase,"COMMIT" ,oDp:oRunLine:cLine,oDp:cDpXbaseLine
// ENDIF

   aMemos:={} // Campos Memos DBF

   DEFAULT lReload:=.F.
   DEFAULT lJoin  :=.F. // .T.
   DEFAULT lIni   :=.F.

   IF ::lOnly  // Sólo campos de la tabla

//     IF .F. // oDp:cTypeBD="MYSQL" .AND. !oDp:lNativo
//         aFields:=MyStruct("SELECT * FROM "+::cTable+" WHERE 1=0 ")
//     ELSE
      ::oCursor:End() // MS-SQL
      // aFields:=OdbcStruct(::cTable) // JN 11/03/2015 Considerado Innecesario
      aFields:=::aFieldsTable // ::aFields


// ? LEN(aFields)
/*         oTable :=OpenTable("SELECT * FROM "+::cTable+" WHERE 1=0 ",.F.)
         aFields:=ACLONE(oTable:aFields)
         oTable:oCursor:End()
         oTable:End()
*/
//   ENDIF

   ENDIF

   // IF !::lAppend .AND. cWhere=NIL

   IF !::lUpdate
      ::lAppend:=.T.   // JN 22/06/2018
   ENDIF

   // IF ::lUpdate .AND. cWhere=NIL
   IF !::lAppend .AND. cWhere=NIL

 //? ::lAppend,"::lAppend",::lUpdate,"::lUpdate"

      IF Empty(::cPrimary)
         MensajeErr("Es necesario Indicar Where o ClassData [::cPrimary]")
         RETURN .F.
      ENDIF

      I     :=::FieldPos(::cPrimary)
      uValue:=::aRecord[I]
      cWhere:=" WHERE "+::cPrimary+GetWhere("=",uValue) //  = '"+uValue+"'"

   ENDIF

   // ? ::oOdbc:cType,cWhere,"ANTES DE GRABAR"
   // BuildLimit(::cSql,::cType)

   /*
   // Verifica la Integridad Referencial Asigna mediante oDp:aInsertDef
   */

   DEFAULT oDp:aInsertDef:={}

   // Asigna Valores por Defecto
   // JN 20/08/2014  POR AHORA SE SUSPENDE,
   // JN 30/12/2014 Si los datos se generan desde un Formulario no es necesario ejecutar este Programa Debido a que los
   // los genera el formulario

   // ? ::lSetFieldDef,"VALORES POR DEFECTO",::cTable
   // JN 26/09/2019 no debe ejecutar DPTABLESETDEF, si no  tiene definiciones


   // DEFAULT  oDp:aDefault:={}

// ? "AQUI ES"

   IF ::lSetFieldDef  .AND. "DPPROGRA"<>::cTable .AND. ASCAN(oDp:aDefault,{|a,n|a[1]==::cTable})>0
     EJECUTAR("DPTABLESETDEF",SELF) // 25/08/2023  Inactivado y aplica en appendblank()
   ENDIF

  // Metod load debe ejecutarlo Previamente
   IF ::lCreaRegIntRef
     ::CreaRegIntRef()
   ENDIF

   // ::cTable:=ALLTRIM(::cTable)

/*
27/04/2023 innecesario
   IF !Empty(::aRecClone) .AND.   !Empty(::cWhere)
      EJECUTAR("DPAUDELIMODTAB",Self)
      ::aRecClone:={}
   ENDIF
*/
   /*
   // registro de Auditoria para incluir y modificar 27/04/2023
   // No puede ser Auditoría
   */

   IF !"AUD"$UPPER(::cTable)
     EJECUTAR("DPAUDELIMODTAB",Self)
   ENDIF

   IF ::lAppend

      nAt:=ASCAN(oDp:aInsertDef,{|a,n| a[1]=ALLTRIM(::cTable)})

      //? ::cTable,"COMMIT",nAt,LEN(oDp:aInsertDef),IF(LEN( oDp:aInsertDef)>0,oDp:aInsertDef[1,1]=::cTable,"")

      IF nAt>0
        EJECUTAR("TTABLEINSERT",SELF) // 25/08/2023 Valores por defecto son Asignados en Append Blank
      ENDIF

      // JN 13/03/2015
      // Los campos que no estan en la tabla seran removidos
      aBuffers:={}


      FOR i := 1 TO LEN( ::aBuffers )
         nAt:=ASCAN(::aFieldsTable,{|a,n|a[1]==::aBuffers[I,2]})
         IF nAt>0
           AADD(aBuffers,::aBuffers[I]) // ::aBuffers[nAt])
         ENDIF
      NEXT I

//   IF LEN(aBuffers)<>LEN(::aBuffers)
     ::aBuffers:=aBuffers
//? "CAMPO NO EXISTE",LEN(aBuffers),LEN(::aBuffers),"COMMIT() CLASS TTABLE "
//    ENDIF

      IF Empty(::aBuffers[1,1])
         // Buffer Vacio
      ENDIF

      FOR i := 1 TO LEN( ::aBuffers )

         // Aqui permite hacer INSERT INTO EN CAMPOS QUE NO ESTAN EN LA TABLA
         nAt:=ASCAN(aFields,{|a,n|a[1]==::aBuffers[I,2]})

         IF nAt=0 .AND. !EMPTY(aFields) // Campos Unicos
            ::aBuffers[I,1]=NIL
         ENDIF


         IF !::aBuffers[I,2]+CHR(1)$cListFields .AND. !::aBuffers[I,1]=NIL
           cListFields:=cListFields + IIF( EMPTY(cListFields),"",",") + ::aBuffers[I,2]+CHR(1)
         ELSE
           // Repetido
           ::aBuffers[I,1]:=NIL
         ENDIF
      NEXT

      // Caso de Append con apertura de una Instancia, pierde en Insert INTO
//      IF Empty(::cListFields)
//         ::cListFields:=cListFields
//      ENDIF

      cSql := IF(::lInsertReplace,"REPLACE INTO ","INSERT INTO ") + ::cTable + " ("+STRTRAN(cListFields,CHR(1),"")+") VALUES ("

      IF ::nInsert>0 .AND. !Empty(::nCountIns)
         cSql:=CRLF+"(" // Continuidad en la Inclusión
      Endif

   ELSE

      IF EMPTY(cWhere) // no puede Grabar Where Vacio
         MensajeErr(GETPROCE(),"NO Puede Grabar sin Condición Where")
         RETURN .T.
      ENDIF

      cWhere:=IIF( !"WHERE "$cWhere, " WHERE " ,"" )+cWhere
      cSql  := "UPDATE " + ::cTable +" SET "  // + ::GenUpdate()+ " WHERE " + ::GenWhere()

   ENDIF


   FOR i := 1 TO LEN( ::aBuffers )

         uValue := ::aBuffers[I,1]
         cField := IIF( ::lAppend,"",::aBuffers[I,2]+"=")
         cType  := ::aBuffers[I,3]
         nLen   := ::aBuffers[I,4]
         nDec   := ::FieldDec(::aBuffers[I,2])

         // Remueve los Residuos del Formato

         IF cType="M" .AND. ::cType="MSSQL" .AND. ValType(uValue)="C"
            uValue:=STRTRAN(uValue,CHR(28),"/")
            uValue:=STRTRAN(uValue,CHR(29),CHR(250))
         ENDIF

         nAt:=ASCAN(aFields,{|a,n|a[1]=::aBuffers[I,2]})

         IF nAt=0 .AND. !EMPTY(aFields) // Campos Unicos
            ::aBuffers[I,1]=NIL
            LOOP
         ENDIF

         IF ::aBuffers[I,2]=::cPrimary .AND. ::lAppend
            cClave=uValue
         ENDIF

//         IF ::lAppend
//            cListFields:=cListFields + IIF( EMPTY(cListFields),"",",") + ::aBuffers[I,2]
//         ENDIF
//      IF !EMPTY(::aDataFill).AND. .F. // OJO
//         ::aDataFill[::nArrayAt,I]:=uValue // Actualiza la copia
//      ENDIF

         IF uValue=NIL // .AND. !::lAppend
            // uValue="NULL"
            LOOP
         ENDIF

//         IF ValType(uValue)="C" .AND. "'"$uValue
//            uValue:=STRSQL(uValue)
//         ENDIF
//       IF "DPPROGR"$::cTable
//          ? uValue,cField
//       ENDIF

         // Valida que el Campo no se Repita
         //? "Campo"+cField,"cSQL"+cSQL,::aBuffers[I,2]
         // IF ::aBuffers[I,2]$cSQL // El campo ya está Indicado
         //   LOOP
         // ENDIF
         // ? i,uValue,cType,ValType(uValue)

         IF cType=="C" .AND. ValType(uValue)="C"
//? uValue,"uValue, AQUI ES 2026"
            uValue:=STRTRAN(uValue,CHR(10),"") // Remueve CHR(10)
            uValue:=STRTRAN(uValue,CHR(13),"") // Remueve CHR(13)
         ENDIF

         IF cType $ "CM"

            uValue := ALLTRIM(IIF( valtype(uValue)!="C","" ,uValue))
            uValue := STRSQL(uValue)

            // Insertar Campo BLOB
//            IF cType="M" .AND. oDp:cTypeBD="MYSQL"  .AND. LEFT(ALLTRIM(::aBuffers[I,1]),10)="LOAD_FILE("
//               uValue := ALLTRIM(::aBuffers[I,1])
//             ? "CAMPO BLOB",cType,cField,uValue
//            ENDIF

            IF cType="M"

              IF !::cType$"ADS,DBF"
                // uValue := STRSQL(uValue)
                IF RIGHT(uValue,1)!=CHR(10) .AND. !::cType="MSSQL"
                  uValue+=CHR(10)
                ENDIF
                //  ? uValue,LEN(uValue)
              ELSE

                // Campos Memos en ADS/DBF no pueden Ser Grabados
                AADD(aMemos,{::aBuffers[I,2],uValue})

                IF !::lAppend // Modifica Requiere NIL
                   LOOP
                ELSE
                   uValue:="" // Incluye Requiere Vacio
                ENDIF

              ENDIF
              // uValue :="memo"
              // ? "memo",uValue
            ELSE

              uValue :=IIF(nLen=0 .OR. CHR(13)$uValue , ALLTRIM(uValue) , ALLTRIM(LEFT(uValue,nLen)))

// OJO esto se resuelve en SQLSTR()
//              IF "'"$uValue
//                 uValue:=STRTRAN(uValue,"'",["])
//              ENDIF
//              uValue :=STRTRAN(uValue,"\","\\")

              // ? uValue,"CHAR"
            ENDIF

			ENDIF

         IF cType="D".AND.ValType(uValue)="C"
            uValue :=CTOD(uValue)
         ENDIF

         // IF cType="D" .AND. oDp:cTypeBD="MSSQL"
            // IF EMPTY(uValue)
            //    uValue:="NULL"
            // ENDIF
            // uValue:="01/01/2004"
            //? "MSSQL NO ACEPTA FECHAS VACIAS",CTOO(uValue,"D")
         // ENDIF

         IF uValue=NIL // Valores Vacios

           DO CASE
              CASE cType="C" .OR. cType="M"
                 uValue:=""
              CASE cType="N"
                 uValue:=0
              CASE cType="D"
                 uValue:=CTOD("")
              CASE cType="L"
                 uValue:=.F.
           ENDCASE

         ENDIF
//         uValue:=STRSQL(uValue) // Quita el slash por Chr(28)
         if cType!=ValType(uValue) .AND. ::aBuffers[I,3]<>"M" // JN 10/06/2016
            cType:=ValType(uValue)
            // ? ::aBuffers[I,2],;
            //  "cField",cField,"uValue",uValue,"cType",cType,"ValType",ValType(uValue)
         ENDIF

//IF "DPMAIL"$::cTable
  //? ::aBuffers[I,3],cType,oDp:cTypeBD,::aBuffers[I,1],"AQUI ES"
//ENDIF

			DO CASE

            CASE cType == "C"

                nAt:=ASCAN(oDp:aFunction,{|a,n| a=LEFT(ALLTRIM(::aBuffers[I,1]),LEN(a))})

                IF ::cType="MYSQL"  .AND. nAt>0
                  uValue := ALLTRIM(::aBuffers[I,1])
                  cSql += cField+uValue // Uso de Funciones en MySQL
                ELSE
                 cSql += cField+"'" + uValue + "'"
                ENDIF

            // CASE cType == "C" ; cSql += GetWhere(cField,uValue)

            CASE cType == "L" ; cSql += cField+IIF( uValue, "1", "0" )

            CASE cType == "N"

                   // JN 26/05/2018
                   cSql += cField+ LSTR(uValue,nLen+nDec,nDec)

                   // cSql += cField+ALLTRIM( STR( uValue ,40) ) // Truncaba los decimales
//IF INT(uValue)<>uValue
//?  "valor numerico",nLen,LSTR(uValue,nLen+nDec,nDec),uValue,"<-uValue",STR( uValue ,40),TRAN(uValue,REPLI("9",30)+"."+REPLI("9",nDec)) ,nDec
//ENDIF


// cSql += cField+LSTR(uValue ,nLen,nLen) )

            CASE cType == "M"

              nAt:=ASCAN(oDp:aFunction,{|a,n| a=LEFT(ALLTRIM(::aBuffers[I,1]),LEN(a))})

              IF ::cType="MYSQL"  .AND. nAt>0
                 // LEFT(ALLTRIM(::aBuffers[I,1]),10)="LOAD_FILE("
                 uValue := ALLTRIM(::aBuffers[I,1])   //  ? "CAMPO BLOB",cType,cField,uValue
                 cSql += cField+uValue // Uso de Funciones en MySQL
              ELSE
                 cSql += cField+"'" + uValue + "'"
              ENDIF

            CASE cType == "D"

              IF ::oOdbc:lDateNull

// ? SQLDATE(uValue,NIL,::oOdbc:lDateNull),"SQLDATE(uValue,NIL,::oOdbc:lDateNull)",ValType(SQLDATE(uValue,NIL,::oOdbc:lDateNull))

                uValue:=SQLDATE(uValue,NIL,::oOdbc:lDateNull)

                IF uValue<>"NULL" .AND. LEFT(uValue,1)<>"'"
                   uValue:="'"+uValue+"'"
                ENDIF

// ? uValue,"FECHA, DEBE ESTAR INCLUIDA ENTRE COMILLAS, EXCEL NULL"

                cSql += cField + uValue // SQLDATE(uValue,NIL,::oOdbc:lDateNull)

              ELSE
                cSql += cField+CTOSQL(uValue)
              ENDIF


            // CASE cType == "D" ; cSql += "'"+DTOS(uValue)+"'"
/*
            CASE cType == "D" .AND. !::cType$"ADS,DBF"; cSql += cField+"'" +StrZero(Year(uValue) ,4)+ "-" +;
                                                    StrZero(Month(uValue),2)+ "-" +;
                                                    StrZero(Day(uValue)  ,2)+ "'"
            CASE cType == "D" .AND. ::cType$"ADS,DBF"; cSql += cField+"'" +StrZero(Month(uValue) ,2)+ "/" +;
                                                    StrZero(Day(uValue)   ,2)+ "/" +;
                                                    StrZero(Year(uValue)  ,4)+ "'"
*/
			ENDCASE

         IF i # LEN( ::aBuffers )
            cSql += ", "
			ELSE
            cSql += IIF( ::lAppend," )","")
			ENDIF

         cXSQL:=cSql

        // IF "DPPROGRA"$::cTable .AND. "DPLINK"$cSql
        //    ? "EN CREACION",ALLTRIM(LEFT(cSql,400)),ALLTRIM(RIGHT(cSql,200))
        //  ENDIF

         nCuantos++

         IF ::nInsert=0
            ::aBuffers[I,1]:=NIL // Debe Ser Limpiado
         ENDIF

    NEXT

    cSql:=ALLTRIM(cSql)

//    IF RIGHT(cSql,1)=","
//       cSql:=LEFT(cSql,Len(cSql)-1)+IIF( ::lAppend,")","")
//    ENDIF

    IF !::lAppend // Agregar Where
       cSql:=ALLTRIM(cSql)
       cSql:=IIF( RIGHT(cSql,1)="," , LEFT(cSql,LEN(cSql)-1) , cSql )
       cSql:=cSql+" "+cWhere
//     ? cSql,cWhere,::cWhere
    ELSE

//    ? oOdbc:cType,cWhere

    IF ::nInsert>0  .AND. !::cType$"ADS,DBF"

          cSql:=STRTRAN(cSql," VALUES ",CRLF+" VALUES "+CRLF)

//         IF ::nCountIns>0
//             cSql:=SUBS(cSql,AT(" VALUES ",cSql)+8,LEN(cSql))
//           ? cSql,"cSql 2DO"
//         ENDIF

          ::cInsert:=::cInsert+IIF(::nCountIns>0 , CRLF+","+CRLF , "" )+cSql

          ::nCountIns++

//          IF "DPTABLAS"$UPPE(cSql)
//            ? "Incluir",cSql , ::nInsert , ::nCountIns
//          ENDIF

//          ? ::cInsert,"Definitivo",LEN(::cInsert)

          IF ::nCountIns>=::nInsert
             ::nCountIns:=0 // Empezamos de Nuevo
             cSql:=::cInsert
             // oDp:cInsert:=::cInsert
             oDp:cSql   :=::cInsert
//           ? "GRABAR",LEN(cSql)
//           MemoWrit("C:\Sql.SQL",cSql)
             ::cInsert:=""
          ELSE
             RETURN .T.  // Sigue Agregando
          ENDIF

       ENDIF


    ENDIF

    // ? cSql,::oOdbc:ClassName() // ::oCursor:oOdbc:ClassName()
    // lRet:=::oCursor:oOdbc:Execute(cSql, "Actualizando "+::cTable)
    //    ? cSql,::oOdbc:ClassName()
/*
    IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
       IF oDp:lCGI  // Ejecuta SGI
         EJECUTAR("CGISQLMSG",cSql)
       ELSE
         ? cSql
       ENDIF
    ENDIF
*/
    oDp:cMsgOdbc:=""

    // Modificar la Tabla y Actualizar Relacionados
    // ? ::cSql,"cSql"
/*
    IF lJoin .AND. " WHERE "$UPPE(::cSql) .AND. .F. // Esta Modificando

       // Data Anterior de la Modificación
       cWhere   :=::cSql
       cWhere   :=ALLTRIM(SUBS(cWhere,AT(" WHERE ",cWhere)),LEN(cWhere))
    //   ? cWhere, "ACTUALIZAR, EN CADENA, oTableOld",cWhere
       oTableOld:=Opentable("SELECT * FROM "+::cTable+" "+cWhere,.T.) // Registro Anterior

    //   ? oTableOld:cSql,"cSql"

       oTableOld:lOnly:=.F. // Sólo campos de la tabla
    //   ? oTableOld:RecCount(),"RecCount()"
    ENDIF
*/
    cSql:=STRTRAN(cSql,"WHERE WHERE","WHERE")

    IF "WHERE"$cSql // MSSQL no necesita LIMIT
       oDp:lStrSql:=.T.
       cSql:=BuildLimit(cSql)
       oDp:lStrSql:=.F.
    ELSE

      // ? "A AQUI INSERTA",cSql
      oDp:cInsert:=cSql

    ENDIF

    // ? "ANTES DE MODIFICAR"

    oDp:cMsgOdbc:=""

//  ? ::cTable,::oOdbc:ClassName()

    IIF( ::lMySqlNativo, NIL ,  ::ChkDsn() )

//  IF ValType(::oOdbc)!="O"
//    ::oOdbc:=OPENODBC(::cTable) // Apertura del DSN
//  ENDIF

//  ? ::cTable,::oOdbc:ClassName(),cSql

/*    IF ::nInsert>0
      ::nCountIns++
      ::cInsert:=::cInsert+cSql
    ENDIF */

//    IF "DPPROGR"$::cTable
//       MemoWrit("SQL.SQL",cSql)
//       ? cSql
//    ENDIF


    IF ::lAppend
      cSql:=cXSQL
      oDp:cInsert:=cSql
    ENDIF

    IF Empty(cSql)
       MensajeErr("No fue construida la Sentencia SQL"+CRLF+CTOO(cSql,"C"))
       RETURN .F.
    ENDIF

    cSql:=ALLTRIM(cSql)

    IF RIGHT(cSql,1)=","
      cSql:=LEFT(cSql,LEN(cSql)-1)+")"
    ENDIF

// ?  SUBS(cSql,LEN(cSql),1)=",",cSql,"AQUI BUSCAR COMA QUE ESTA DEMAS"

    IF SUBS(cSql,LEN(cSql),1)="," // Esta sobrando
       cSql:=LEFT(cSql,LEN(cSql)-1)
    ENDIF


// ? cSql,"AQUI ES LA COMA DEMAS"
/*
    IF CHR(29)$cSql // oDp:cTypeBD="MSSQL" .AND. CHR(29)$cSql // MSSQL No Acepta CHR(29)
       cSql:=STRTRAN(cSql,CHR(29),CHR(255))
       ? "este si ",cSql,"TIENE CHAR(29)"
       ? "QUEDA ASI,AHORA ES ",cSql
    ENDIF

    FOR i := 1 TO len(cSQL)
       uValue:=subs(cSQL,I,1)
       IF ASC(uValue)<30 .AND. !(ASC(uValue)=13 .OR. ASC(uValue)=10 .OR. ASC(uValue)=28)
          cSql:=STRTRAN(cSql,CHR(29),CHR(28))
          ? uValue,ASC(uValue),oDp:cTypeBD
       ENDIF
    NEXT
*/

//    ::oOdbc:lShowError:=.F.
//    ? "AQUI EJECUTARA",cSql

    oDp:cSqlCommit:=cSql
    oDp:cSql:=cSql

    //IF !Empty(::cInsert)
    //  oDp:cInsert:=::cInsert
    //   ENDIF
    // ? CLPCOPY(cSql),GETPROCE()

    nT1:=SECONDS()

    IF oDp:lNativo // lMySqlNativo // .OR.  oDp:cTypeBD="MYSQL"

//     IF ::lLock
//        ::oOdbc:Execute("LOCK TABLES "+::cTable+" WRITE ")
//    ENDIF
//      IF "DPPERSONAL"$::cTable
//        ? cDpXbase,"LUEGO, DPTABLESETDEF:,MYEXECUTE(::cTable,cSql,::oOdbc) "
//      ENDIF

      oDp:cDpXbaseLine:=cDpXbase // Necesario en clase TMSDB:Execute()


      cSql:=EJECUTAR("TTABLE_COMMIT",cSql,::oOdbc,::cType,SELF)

      oDp:cSql:=cSql

      lRet:=MYEXECUTE(::cTable,cSql,::oOdbc,.F.) // El mensaje se Muestra Abajo (EJECUTAR("SQLMSGERR"

      IF ::lLock
         ::UnLockTable()
      ENDIF

      // lRet:=::oOdbc:ExecSQL(cSql, "Actualizando "+::cTable)

      IF !lRet
        oDp:lTracer:=.F. // No debe Mostrar la Traza en este Programa

//      DEFAULT oDp:cDpXbaseLine:=""
//      cDpXbase:=IIF(oDp:oRunLine=NIL,cDpXbase,oDp:oRunLine:oFunction:oScript:cProgram+":"+oDp:oRunLine:oFunction:cName+"("+oDp:oRunLine:cLine+")")
//? "aqui es",getproce(),oDp:oRunLine:oFunction:oScript:cProgram+":"+oDp:oRunLine:oFunction:cName+"("+oDp:oRunLine:cLine+")"
//      cDpXbase:=IIF(oDp:oRunLine=NIL,cDpXbase,)

// oDp:cFileToScr:="C:\X\COMMIT.TXT"
//
 // ? cSql,::cTable,::oOdbc:cDsn,SECONDS()-nT1,nil   ,cDpXbase,NIL      ,::oOdbc,::oOdbc:oConnect:oError:GetError(),::lRemote

        RUNNEW("SQLMSGERR",cSql,::cTable,::oOdbc:cDsn,SECONDS()-nT1,nil   ,cDpXbase,NIL      ,::oOdbc,::oOdbc:oConnect:oError:GetError(),::lRemote)

       //                  cSql ,cTable  , cDb        ,nTime        ,cProce,cDpXbase,lNoInsert,oDb   ,cError,lRemote
//     RUNNEW("SQLMSGERR",cStatement,nil,::cName,nil,nil,cDpXbase,NIL,Self,::oConnect:oError:GetError()) // oDp:cDpXbaseLine)

      ENDIF

    ELSE

      cSql:=EJECUTAR("TTABLE_COMMIT",cSql,::oOdbc,::cType,SELF)

      oDp:cSql:=cSql

      lRet:=::oOdbc:Execute(cSql, "Actualizando ["+oDp:cScript+"]DSN="+::oOdbc:cDsn+" "+::cTable)

    ENDIF

    IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL

// ? ::cTable,::oOdbc:cDsn,SECONDS()-nT1,cSql
      oDp:lTracer:=.F.
      //cDpXbase:=IIF(oDp:oRunLine=NIL,cDpXbase,oDp:oRunLine:oFunction:oScript:cProgram+":"+oDp:oRunLine:oFunction:cName+"("+oDp:oRunLine:cLine+")")
      EJECUTAR("DPTRAZASQL",cSql,::cTable,::oOdbc:cDsn,SECONDS()-nT1,NIL,GETPROCE(),cDpXbase) // oDp:cDpXbaseLine)
      oDp:lTracer:=lTracer

    ENDIF


//    IF "DPTABLAS"$UPPE(cSql)
//       ? cSql
//    ENDIF

    IF !EMPTY(oDp:cMsgOdbc)
       MsgErrSql(CLPCOPY(cSql),"Class Ttable:Commit()")
//       IF !" DATASET "$cSql
//         ? oDp:cMsgOdbc,CLPCOPY(cSql)
//       ENDIF
    ENDIF

    // Modificar la Tabla y Actualizar Relacionados

    IF " SET "$UPPE(cSql) .AND. " WHERE "$UPPE(cSql) .AND. ValType(oTableOld)="O" // Esta Modificando

       DEFAULT aRuta:={}

       IF lIni
         aRuta:={}  // Ruta Recursiva
       ENDIF

       ReplaceJoin(::cTable,Self,oTableOld)
       oTableOld:End()
       // UpdateJoin(::cTable) // Reemplaza todas las tablas relacionadas

    ENDIF

    /* IF " SET "$UPPE(cSql) .AND. " WHERE "$UPPE(cSql) .AND. lJoin

      ? "ASIGNA VALORES COMUNES"
      UpdateJoin(::cTable) // Reemplaza todas las tablas relacionadas

    ENDIF */

    IF !empty(oDp:cMsgOdbc)
      ? oDp:cMsgOdbc,"oDp:cMsgOdbc"
    ENDIF

    // ? "lRet",cSql,"LISTO"
    // lRet := ::oCursor:oConnect:Execute(cSql, "Actualizando "+::oCursor:TableName())

    IF .F. .AND. ValType(lRet)="L".AND.!lRet
        ::oCursor:oConnect:RollBack()
    ENDIF

    IF ::lMySqlNativo
       // ? ::oCursor:ClassName()
       ::oCursor:Refresh()
    ENDIF

    IF !UPPE(::oCursor:ClassName())=UPPE("MySqlTable") .AND. !UPPE(::oCursor:ClassName())=UPPE("TDbOdbcDirect") // Originado directo
      ::oCursor:Refresh()
    ENDIF

    // ? ::cPrimary,"Primary",lReload

    IF !Empty(aMemos) // Solo para DBF/ADS
       SaveMemo(aMemos,::cTable,::oOdbc:cPath,cWhere,cClave,::cPrimary)
    ENDIF

    // ? LEN(aMemos)

    IF lReload
      ::Reload()
    ENDIF

    IF ::ShowError(IIF(::lAppend,"Incluir","Modificar"))
       lRet:=.F.
    ELSE
       ::lAppend:=.F.
    ENDIF

    aMemos:={}

    IF ::lFileLog
      DPWRITE("TEMP\commit_"+ALLTRIM(::cTable)+LSTR(SECONDS())+".SQL",cSql)
    ENDIF

    IF !lRet

      oDp:cError:=oDp:oMySqlCon:oError:GetError()

      DEFAULT oDp:cError:=""

      DPWRITE("TEMP\SQLERR_"+ALLTRIM(::cTable)+".SQL",cSql+CRLF+oDp:cError)

    ENDIF

RETURN lRet

/*
// Solo para REPLACESPEED(), para Incluir
*/
METHOD CommitSpeed(lSave) CLASS Ttable
   LOCAL I,cListFields:="",cSql,uValue,nFields:=0,cError

   IF ::cType$"ADS,DBF,MSSQL"  //.AND. !lSave
      // ::lAppend:=.T.
      // ? "AQUI DEBE SER"
      IF (!Empty(::cInsert) .AND. lSave ) .OR. ::lMemo
         ::Commit()
         RETURN .T.
      ENDIF
   ENDIF

   IF ::lMemo
      RETURN ::Commit()
   ENDIF

   IF lSave

      IF !Empty(::cInsert)

        DpWrite("temp\commit_speed_"+::cTable+".sql",::cInsert)

        IF !::oOdbc:Execute(::cInsert, "Actualizando "+::cTable)
           MsgErrSql(::cInsert,"Class Table:Insert()")
        ENDIF

        ::nCountIns:=0 // Empezamos de Nuevo
        ::cInsert  :=""

      ENDIF

      Return .T.

   ENDIF

   IF ::nCountIns=0

     ::aFieldSpeed:={}

     AEVAL(::aBuffers,{|a,i,uValue|IIF( !::aBuffers[I,2]$cListFields .AND. !::aBuffers[I,1]=NIL,;
                                   (AADD(::aFieldSpeed,I),cListFields:=cListFields + IIF( EMPTY(cListFields),"",",") + ::aBuffers[I,2]),NIL)})

// ViewArray(::aBuffers)

     ::cInsert:=IF(::lInsertReplace,"REPLACE INTO ","INSERT INTO ") + ::cTable + " ("+cListFields+") VALUES "

   ENDIF

   ::cInsert+=IIF(::nCountIns=0 , "" , "," ) + "("

   DEFAULT ::aFieldSpeed:={}

   AEVAL(::aFieldSpeed,{|a,i|nFields++,::cInsert+=IIF(nFields=1,"",",")+CTOSQL(::aBuffers[a,1])})

   ::cInsert+=")"+CRLF
   ::nCountIns++

   oDp:cInsert:=::cInsert

   IF ::nCountIns>=::nInsert .OR. lSave

      //  ? memowrit("sql.sql",::cInsert),"INSERT"

      IF !::oOdbc:Execute(::cInsert, "Actualizando "+::cTable)

         cError   :=oDp:oMySqlCon:oError:GetError()

         MsgErrSql(::cInsert+CRLF+cError,"Registro no Grabado "+;
                   "["+oDp:cScript+"]DSN="+::oOdbc:cDsn+" "+::cTable)

         MemoWrit("temp\insert_error.sql",::cInsert)
//         MensajeErr("Error:"+::cInsert,"Registro no Grabado "+;
//                    "["+oDp:cScript+"]DSN="+::oOdbc:cDsn+" "+::cTable)
      ELSE

       DpWrite("temp\commit_speed_"+::cTable+".sql",::cInsert)

      ENDIF

      ::nCountIns:=0 // Empezamos de Nuevo
      ::cInsert  :=""

   ENDIF

RETURN .T.

METHOD CTOLOG(cField)
   LOCAL nField:=::FieldPos(cField)

   IF nField=0
      MensajeErr(cField+"  no Existe",::ClassName())
      RETURN .F.
   ENDIF

   IF Empty(::aDataFill)
      ::REPLACE(cField,.F.)
      RETURN .T.
   ENDIF

   // JN 20/09/2018   .AND. ::FieldLen(cField)=1
   IF ValType(::aDataFill[1,nField])="C" .AND. ::FieldLen(nField)=1
      AEVAL(::aDataFill,{|a,n|::aDataFill[n,nField]:=(a[nField]="1")})
   ENDIF

   // Numericos JN 19/08/2013
   // JN 20/09/2018  ::FieldLen(cField)=1
   IF ValType(::aDataFill[1,nField])="N" .AND. ::FieldLen(nField)=1
      AEVAL(::aDataFill,{|a,n|::aDataFill[n,nField]:=(a[nField]=1)})
   ENDIF

   ::aFields[nField,2]:="L" // JN 19/08/2013
   ::aFields[nField,3]:=1   // JN 19/08/2013

RETURN .T.


METHOD CTONUM(cField)
   LOCAL nField:=::FieldPos(cField)

   IF nField=0
      MensajeErr(cField+"  no Existe",::ClassName())
      RETURN .F.
   ENDIF

   IF Empty(::aDataFill)
      ::REPLACE(cField,0)
      RETURN .T.
   ENDIF

   IF ValType(::aDataFill[1,nField])="C"
      AEVAL(::aDataFill,{|a,n|::aDataFill[n,nField]:=VAL(a[nField])})
   ENDIF

RETURN .T.

METHOD CTOD(cField)
   LOCAL nField:=::FieldPos(cField)

   IF nField=0
      MensajeErr(cField+"  no Existe",::ClassName())
      RETURN .F.
   ENDIF

   IF Empty(::aDataFill)
      ::REPLACE(cField,CTOD(""))
      RETURN .T.
   ENDIF

   IF ValType(::aDataFill[1,nField])="C"
      // ? ::aDataFill[1,nField],"FECHA"    SQLTODATE(cFecha)
      AEVAL(::aDataFill,{|a,n|::aDataFill[n,nField]:=SQLTODATE(a[nField])})
   ENDIF

RETURN .T.

/*
// Convertir en TXT
*/
#INCLUDE "FILEIO.CH"

METHOD CTOTXT(cFileTxt,lHead,cSep) CLASS TTABLE
   LOCAL cFile:=cTempFile() ,nH,cLine:="",I,aFiles:={}

   aFiles:=ARRAY(ADIR("__*.*"))
   ADIR("__*.*",aFiles)
   AEVAL(aFiles,{|a,n|FERASE(a)})

   FERASE(cFileTxt)

   nH:=fcreate(cFile,FC_NORMAL)

   cLine:=""

   DEFAULT lHead:=.F.,;
           cSep :=CHR(9)

   IF lHead

     AEVAL(::aFields,{|a,n|cLine:=cLine+IIF(Empty(cLine),"",cSep)+;
                                  a[1]})

     fwrite(nH,cLine+CRLF)

   ENDIF


   FOR I=1 TO LEN(::aDataFill)

      cLine:=""

      AEVAL(::aDataFill[1],{|a,n|cLine:=cLine+IIF(Empty(cLine),"",cSep)+;
                                 ALLTRIM(cValToChar(::aDataFill[I,n]))})

      fwrite(nH,cLine+CRLF)

   NEXT I

   fclose(nH)

   COPY FILE (cFile) TO (cFileTxt)

   ferase(cFile)

RETURN File(cFileTxt)

/*
// Convierte en Dbf
*/
METHOD CTODBF(cFileDbf,cRdd,oMeter,oSay,cError,lCreate,cFieldIndex,nRefresh) CLASS tTable
RETURN EJECUTAR("TTABLETODBF",Self,cFileDbf,cRdd,oMeter,oSay,cError,lCreate,cFieldIndex,nRefresh)
/*
    LOCAL cAlias:=ALIAS(),nContar:=0,I,cTotal,cText:="",aFields:={}
    LOCAL cDir // :=cFilePath(cFileDbf)
    LOCAL cFileSql,cIndex

    cError:=""

    DEFAULT cRdd   :="DBFCDX",;
            lCreate:=.T. ,;
            cFileDbf:=::cTable+".DBF"

    cDir :=cFilePath(cFileDbf)

    IF !Empty(cDir)
      lMkDir(cDir)
    ENDIF

    cText:=IIF( ValType(oSay)="O" , EVAL(oSay:bSetGet) , "" )

    DPWRITE("TEMP\"+STRTRAN(cFileDbf,".DBF",".SQL"),::cSql)

    cFileSql:=cFileNoExt(cFileDbf)+".SQL"

    CursorWait()

    IIF( ValType(oSay)="O" , oSay:SetText("Creando: "+cFileDbf) , NIL )


    cFileDbf:=cFileDbf+IIF("."$cFileDbf,"",".DBF")

    IF lCreate

      cIndex:=STRTRAN(UPPE(cFileDbf),".DBF",".CDX")

      FERASE(cFileDbf)
      FERASE(cIndex) // STRTRAN(UPPE(cFileDbf),".DBF",".CDX"))
      FERASE(STRTRAN(UPPE(cFileDbf),".DBF",".FPT"))
    ENDIF

    AEVAL(::aFields,{|a,n|AADD(aFields,{a[1],a[2],a[3],a[4]})})

    // ? ::CLI_NOMBRE,"CLI_NOMBRE",LEN(::CLI_NOMBRE)

    FOR I := 1 TO LEN(aFields)

       aFields[I,1]:=ALLTRIM(aFields[I,1])
       aFields[I,3]:=IIF( aFields[I,2] ="N" , 19 , aFields[I,3] )
       aFields[I,4]:=IIF( aFields[I,2]!="N" , 0  , aFields[I,4] )
       aFields[I,3]:=IIF( aFields[I,2] ="D" , 8  , aFields[I,3] )
       aFields[I,2]:=IIF( aFields[I,2] ="U" , "C", aFields[I,2] )
       aFields[I,3]:=IIF( aFields[I,3] =0   , 1  , aFields[I,3] )

       // ? aFields[I,1],aFields[I,2],aFields[I,3],aFields[I,4]
       //IF aFields[I,2]="C" .AND. aFields[I,3]>7000 // LEN(::FieldGet(aFields[I,1]))
       //  ? "EXCEDE",aFields[I,3]
       // ENDIF

    NEXT

//    ? cFileDbf
//    ViewArray(aFields)

SysRefresh(.T.)

USE

    IF !FILE(cFileDbf)
      DBCREATE(cFileDbf,aFields,cRdd)
    ENDIF

// ::BROWSE()
// ErrorSys(.T.)

    IF !FILE(cFileDBf)
      cError:="Archivo : "+cFileDbf+" No pudo Ser Creado"
      RETURN .F.
    ENDIF

// IF MacroEje("ISPCPRG()")
//  ? cRdd,"cRdd",oDp:cRdd,"Solo en PC DESARROLLO",oDp:cScript,"Programa",cFileDBf
// ENDIF

SysRefresh(.T.)

    USE

    USE (cFileDbf) NEW EXCLU VIA cRdd

// oDp:oFrameDp:SetText(cFileDbf)
// ErrorSys(.T.)

    IIF( ValType(oMeter)="O",oMeter:SetTotal(Len(::aDataFill)),NIL)

    FOR I := 1 TO LEN(::aDataFill)

       // IIF( ValType(oMeter)="O",oMeter:Set(I),NIL)

       cTotal     :=cText+"["+GetNumRel(I,::RecCount())+"]"

       // IIF( ValType(oSay)="O" , oSay:SetText(cTotal) , NIL )

       IF ++nContar>500

         IIF( ValType(oMeter)="O",oMeter:Set(I),NIL)
         IIF( ValType(oSay)="O" , oSay:SetText(cTotal) , NIL )

         SysRefresh(.T.)
         COMMIT
         nContar:=0
         CursorWait()
       ENDIF

       APPEND BLANK

      //  uValue:= IIF( ValType(uValue)="C" , ANSITOOEM(uValue) , uValue ),;
      // ErrorSys(.T.)

       AEVAL(::aFields,{|a,n,uValue|uValue:=::aDataFill[I,n],;
                                    uValue:= IIF( aFields[n,2]=="C", Left(cValToChar(uValue),aFields[n,3]) , uValue ),;
                                    uValue:= IIF( ValType(uValue)="C" .AND. CHR(13)$uValue , STRSQLOFF(uValue) , uValue ),;
                                    uValue:= IIF( ValType(uValue)=a[2], uValue , CTOO(uValue,a[2])),;
                                    FieldPut(n,uValue)})


    NEXT

    IIF( ValType(oMeter)="O" , oMeter:Set(Len(::aDataFill)) , NIL)
    IIF( ValType(oSay)  ="O" , EVAL(oSay:bSetGet,cText)    , NIL )

    DPWRITE("TEMP\"+cFileSql,::cSql)

    COMMIT

    IF !Empty(cFieldIndex)
       USE
       USE (cFileDbf) NEW EXCLU VIA cRdd
       INDEX ON (cFieldIndex) TAG "INDEXKEY" TO (cIndex)
    ENDIF

    USE

    DPSELECT(cAlias)

    SysRefresh(.T.)


RETURN .T.
*/

// Revisa Valores Faltantes para la Integridad Ref
METHOD ChkIntRef(cField) CLASS tTable
    LOCAL aData:=GetFieldIntRef(::cTable)
RETURN .T.

/*
// Grabar, Commit
*/
METHOD SaveSpeed() CLASS Ttable

    IF !Empty(::cInsert)
      ::oOdbc:Execute(::cInsert, "Actualizando "+::cTable)
      ::nCountIns:=0 // Empezamos de Nuevo
    ENDIF

    ::cInsert:=""

RETURN NIL

//----------------------------------------------------------------------------//
METHOD End(lDel) CLASS Ttable
    LOCAL nAt

    DEFAULT lDel:=.F.
    // IIF(!::lFromCursor,(::oCursor:oConnect:Commit(),::oCursor:End()), NIL ) // Cursor Creado en la Clase
    // ::oCursor:oConnect:Commit()
    // ::oCursor:End()

    ::SaveSpeed()
/*
    IF !Empty(::cInsert)
       ::oOdbc:Execute(::cInsert, "Actualizando "+::cTable)
       ::nCountIns:=0 // Empezamos de Nuevo
    ENDIF

    ::cInsert:=""
*/

    nAt:=ASCAN(aTableOpen,{|a,n|a[1]:nHandler=::nHandler})

    IF nAt>0
      aTableOpen:=ARREDUCE(aTableOpen,nAt)
//    AEVAL(aTableOpen,{|a,n|MensajeErr(a[3],a[1]:cTable)})
    ENDIF

//  ViewArray(aTableOpen)

    // ? nAt,::cTable,::cSql,LEN(aTableOpen)
    // ? ::oCursor:hStmt,"sql"
    // ::oCursor:End()

    IF ValType(::oCursor)="O" .AND. ValType(::oCursor:hStmt)="N"
      ::oCursor:End(.T.)
      ::oOdbc  :=NIL
      // ? "SI CERRO OK"
    ENDIF

/*    nAt:=ASCAN(aTablas,{|a,n|a[1]=::nHandler})

    IF nAt>0
       ADEL(aTablas,nAt)
       ASIZE(aTablas,LEN(aTablas)-1)
    ENDIF
*/

  IF lDel .AND. .F.
   ::aDataFill  :=NIL   // Data Completa de la tabla (Original)
   ::aBodyLink  :=NIL  // Campos de enlace
   ::aHeadLink  :=NIL   // Campos de la Tabla enlazadora
   ::aFieldsNew :=NIL
   ::aFieldsFrom:=NIL
   ::aFields    :=NIL
   ::aAddFields :=NIL // Campos Agregados en ADDFIELDS
   ::aBuffers   :=NIL
   ::aDataOrg   :=NIL // Data Completa de la taba
   ::aVars      :=NIL // Control de los Campos
   ::aRecord    :=NIL // Control del Registro
   ::aFieldSpeed:=NIL
   ::aFieldTable:=NIL
  ENDIF

  IF !::lSET_FOREIGN
     ::oOdbc:Execute("SET FOREIGN_KEY_CHECKS=1")
  ENDIF

  ::UnLockTable( )

RETURN NIL

//----------------------------------------------------------------------------//

METHOD CopiaReg(oOrigen) CLASS Ttable

   local n:=0,i,u,nPos

   IF EMPTY(::aFieldsNew) // Campos Iguales entre las dos estructuras
      ::aFieldsNew:={}
      FOR I := 1 TO LEN(::oCursor:aFields)
           FOR U := 1 TO LEN(oOrigen:oCursor:aFields)
              IF ::oCursor:aFields[I,1]=oOrigen:oCursor:aFields[U,1] // Mismo Nombre
                 AADD(::aFieldsNew,oOrigen:oCursor:aFields[U]) // aStruct[U]) // Campo con el Mismo Nombre
                 EXIT
              ENDIF
           NEXT
        NEXT
  ENDIF

  WHILE !(n++,oOrigen:oCursor:IsComplete())
    oOrigen:Load(1)
    ::Append()
    FOR i := 1 TO len(::aFieldsNew)
       nPos:=oOrigen:FieldPos(::aFieldsNew[I,1])
       ::Replace(::aFieldsNew[I,1],oOrigen:FieldGet(nPos))
    NEXT
    ::Commit() // Graba el Registro
  ENDDO

   ? n,"copiados"

return .t.

METHOD  UnLockTable()

     IF !oDp:lLock
         RETURN .F.
     ENDIF

     DEFAULT ::oOdbc:=GetOdbc(::cTable)

     (IIF(::lLock,::oOdbc:Execute("UNLOCK TABLES "),NIL),;
                                  ::lLock:=.F.)

RETURN ::lLock
/*
// JN 10/01/2017
*/
METHOD  LockTable()  CLASS Ttable

    IF !oDp:lLock
       RETURN .F.
    ENDIF

    DEFAULT ::oOdbc:=GetOdbc(::cTable)

    (IIF(!::lLock,::oOdbc:Execute("LOCK TABLES "+::cTable+" WRITE "),NIL),;
    ::lLock:=.T.)

RETURN ::lLock
/*
// Edicion del Browse
*/
METHOD Browse(lOrder,cTitle,lMdi) CLASS Ttable
   LOCAL oDatRdd

   DEFAULT lOrder:=.F.,;
           cTitle:="Title: "+GetFromVar("{oDp:"+::cTable+"}")

   oDatRdd:=TRDDARRAY():New()
   oDatRdd:FromCursor(Self)
   oDatRdd:Browse(cTitle,lOrder)

RETURN oDatRdd

METHOD GetWhere()  CLASS Ttable
   LOCAL cWhere:=""
   AEVAL(::aFields,{|a,n| cWhere:=cWhere + IIF( Empty(cWhere),""," AND ")+;
                         a[1]+GetWhere("=",::FieldGet(n)) })

RETURN cWhere

/*
   LOCAL oBrw,oWnd,oFont,oCol,I,oIco

   IF EMPTY(::aDataFill)
      MensajeErr("Consulta Vacia")
      RETURN .T.
   ENDIF

   DEFINE FONT oFont NAME "Verdana" SIZE 0,-10 // -14

   DEFINE WINDOW oWnd MDICHILD;
      TITLE "Browse "+GetFromVar("{oDp:"+::cTable+"}") ;
      OF DpWndMain()

   DEFINE ICON oIco RESNAME "ICON"

   oWnd:SetIcon(oIco)

   oIco :=NIL
   oBrw := TXBrowse():New( oWnd )

   IF Empty(::aDataFill)
     oBrw:SetArray({{"Vacio"}})
   ELSE
     oBrw:SetArray(::aDataFill,.T.)
   ENDIF

   FOR I:= 1 to Len(oBrw:aCols)
      oBrw:aCols[I]:cHeader  := ::FieldName(i)
      IF ValType(::aDataFill[1,I])="N"
        oBrw:aCols[I]:nDataStrAlign:= AL_RIGHT
        oBrw:aCols[I]:nHeadStrAlign:= AL_RIGHT
      ENDIF
   NEXT

   oBrw:CreateFromCode()
   oBrw:SetFont(oFont)
*/
/*
   FOR I:= 1 to Len(oBrw:aCols)
      oCol:=oBrw:aCols[I]
      IF ValType(::aDataFill[1,I])="N"
        oBrw:aCols[I]:nDataStrAlign:= AL_RIGHT
        oBrw:aCols[I]:nHeadStrAlign:= AL_RIGHT
      ENDIF
   NEXT
*/
/*
   oWnd:oClient := oBrw
   oWnd:SetControl( oBrw )

   ACTIVATE WINDOW oWnd VALID (oFont:End(),.T.)

RETURN oBrw
*/

METHOD InitFields() CLASS Ttable

     AEVAL(::aFields,{|a,n| __objAddData( Self, a[1] , CTOEMPTY(NIL,a[2],a[3] )  ) })

//   local aFields,aData
//   local nLen, n

//   ::aBuffers:=ARRAY(LEN(::aFields))
//   ? ::aBuffers

   // Debe copiar los registros en el buffers

/*

  // ::oCursor:aFields    := ::oCursor:CursorFields( ::oCursor:hStmt )
   nLen                 := len(::aFields)
   aFields              := Array(nLen)
   //::oCursor:aBuffer    := Array(nLen)
   //::oCursor:aIsNull    := Array(nLen)
   //::oCursor:aScanFields:= Array(nLen)

   ? "LISTO "

   For n := 1 to nLen

      #ifndef __HARBOUR__
         ::oCursor:aScanFields[n] := StrTran(Upper(Left(::oCursor:aFields[ n ][ SQLNAME ], 9)), " ", "_")
      #else
         ::oCursor:aScanFields[n] := StrTran(Upper(::oCursor:aFields[ n ][ SQLNAME ]), " ", "_")
      #endif

   Next
*/
Return .t.

METHOD Load( nRecs ) CLASS TTable

   local aBuffer, aIsNull
   local cIsNull, cAlias
   local n, nLen
   local lAll

//   ? "este " ,::oCursor:hStmt

   if ::oCursor:hStmt == 0
      return .f.
   endif

   DEFAULT nRecs := 1

   aBuffer := ::oCursor:aBuffer
   aIsNull := ::oCursor:aIsNull
   lAll    := (nRecs == 0)
   nLen    := len(::oCursor:aFields)
   // SysRefresh()

   Do While (lAll .or. nRecs > 0)  .and. ::oCursor:hStmt != 0

      SysRefresh()

      if ::oCursor:Fetch()
         cIsNull := ""
         For n := 1 To nLen
            ::aBuffers[n,1]:=aBuffer[n]
         Next
      endif
      nRecs --
      EXIT
   Enddo
return (nRecs == 0)

*
*  Ttable:Add()
*  Control de Campos
*/
METHOD Add( cName, xValue ) CLASS Ttable

      __objAddData( Self, cName )
      __objSendMsg(Self , cName , xValue)
      __objSendMsg(Self , "_"+cName , xValue)

/*

      cName:=UPPE(ALLTRIM(cName))

      if (::nPos := aScan( ::aVars, { |e,n| e[1] == cName } )) != 0
         ::aVars[::nPos,2] := xValue
      else
         aAdd( ::aVars, { cName, xValue } )
         ::nPos := Len(::aVars)
      endif

      ::cName  := cName

*/
RETURN nil

/*
 *  Ttable:Get()
 *  Obtiene el valor de un campo
 */
METHOD Get( cName ) CLASS Ttable
RETURN  __objSendMsg(Self,cName)

METHOD SORT(cFields,lDesc) CLASS Ttable

  LOCAL aData:={},aFields:=_VECTOR(cFields),I,cField,cType,nField
  LOCAL bBlqX:="",bBlqY:="",bBlq

  DEFAULT lDesc:=.F.

  FOR I=1 TO LEN(aFields)

     cField:=aFields[I]
     cType :=ValType(::FieldGet(cField))
     nField:=::FieldPos(cField)
     cField:="x["+LSTR(nField)+"]"
     IF cType="D"
       cField:="DTOS("+cField+")"
     ENDIF
     bBlqX :=bBlqX + IIF( I>1 , "+", "" ) + cField
     cField:=STRTRAN(cField,"x[","y[")
     bBlqY :=bBlqY + IIF( I>1 , "+", "" ) + cField
     // ? cField,cType,nField
  NEXT I


  ::lDesc       :=lDesc
  ::cField_Order:=cFields

  bBlq:=" { |x, y| "+ bBlqX + IIF(lDesc," > " , " < " ) + bBlqY + "} "

  // ? bBlqX,bBlqY,bBlq

  bBlq:=MacroEje(bBlq)

  aData:=ASORT( ::aDataFill,,, bBlq )

  // ? LEN(aData),"SORT",ValType(bBlq)

  ::aDataFill:=ACLONE(aData)

//  ASORT(oTable:aDataFill,,, { |x, y| x[nCodigo]+DTOS(x[nFecha])+x[nHora] < y[nCodigo]+DTOS(y[nFecha])+y[nHora] })

RETURN aData

/*
   if !(cName==::cName)
      cName:=UPPE(ALLTRIM(cName))
      ::nPos   := aScan( ::aVars, { |e,n| e[1] == cName } )
      ::cName  := cName
   endif

RETURN ::aVars[::nPos,2]
*/

/*
 *  Ttable:Set()
 *  Asigna el Valor de un Campo
 */
METHOD Set( cName, xValue ) CLASS Ttable

  __objSendMsg(Self , cName , xValue)
  __objSendMsg(Self , "_"+cName , xValue)

RETURN Self

METHOD SETAUDITAR(cFieldAud) CLASS Ttable
   ::aRecClone:={}
   ::cFieldAud:=cFieldAud
   AEVAL(::aFields,{|a,n|AADD(::aRecClone,{::FieldName(n),::FieldGet(n)})})
RETURN NIL

/*
 *  Ttable:GetPos()                         // [by ER]
 */
METHOD GetPos( cName ) CLASS Ttable

   MensajeErr(GetProce(),"Ttable:Getpos(), Method Obsoleto")

RETURN 0
//   cName:=UPPE(ALLTRIM(cName))
//   ::cName  := cName
//
//RETURN ::nPos := aScan( ::aVars, { |e,n| e[1] == cName } )

/*
 *  Ttable:IsDef()
 *  Determina si un campo existe
 */

METHOD IsDef( cName ) CLASS Ttable
   // RETURN ::FieldPos(cName)
RETURN __objHasMsg( self, cName )
/*
  LOCAL lResp:=.F.



  ::lIsDef:=.t.

  lResp   :=(__objSendMsg(Self,cName)!=NIL)

  ::lIsDef:=.f.

RETURN lResp
*/

/*
   local lOk := .F.

//   if cName != NIL
      cName:=UPPE(ALLTRIM(cName))
      if (::nPos := aScan( ::aVars, { |e,n| e[1] == cName } )) != 0
         ::cName := cName
         lOk := .T.
      endif
//   endif

RETURN lOk
*/
/*
// Asigna Arreglo, como Data
*/
METHOD  SetArray(aData,aFields) CLASS Ttable

     IF !Empty(aFields)
        ::InitFields()
     ENDIF

RETURN .T.
/*
//
*/
//METHOD OnError( uValue, cMsg, nError ) CLASS Ttable
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11) CLASS Ttable

  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N //,oFrameDp
  LOCAL lResp:=.F.

  cMsg   := ALLTRIM(UPPE(__GetMessage()))

  PUBLICO("oTable",SELF)

  if Left( cMsg, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2,Len(cMsg))

      // ::add(cMsg,uValue)
      __objAddData( Self, cMsg )
      //  oSend(Self ,cMsg , uValue)
      __objSendMsg(Self,cMsg,uValue) // 16/03/2023
   ELSE

     IF !ValType(::oScript)="O"
        ::oScript:=XCOMPILA("TTABLE")
     ENDIF

     IF !::oScript:IsFunction(cMsg)
         MensajeErr("FUNCTION "+cMsg+CRLF+GETPROCE(),"Funcion no existe ")
         RETURN NIL
      ENDIF

      ::oScript:lNoError:=!::lMsgError
      lResp:=::oScript:RUN(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)

      IF !Empty(::oScript:cError) .AND. ::lMsgError
         MensajeErr(::oScript:cError,"Programa"+::cScript+CRLF+"Function: "+cMsg)
      ENDIF

      RETURN lResp

   Endif

RETURN NIL

/*

   cMsg   := UPPE(ALLTRIM(__GetMessage()))

   if Left( cMsg, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2)

      __objAddData( Self, cMsg )
      __objSendMsg(Self,cMsg,uValue)

      RETURN uValue

   ELSE

     IF ::lIsDef   // Llamada por ::IsDef()
       RETURN NIL
     ENDIF

     IF !ValType(::oScript)="O"
        EJECUTAR("TTABLE",Self)
        ::oScript:=GetScript()
     ENDIF

     IF !ValType(::oScript)="O" .OR. !::oScript:IsFunction(cMsg)
          COMPILA(::cScript,.T.)
          ::oScript:=GetScript()
          MensajeErr("REQUIERE COMPILAR "+::cScript+CRLF+"Funcion "+cMsg,GetProce())
          MensajeErr("FUNCTION "+cMsg+CRLF+GETPROCE(),"Funcion no existe: "+::oScript:IsFunction(cMsg))
      ENDIF

      IF ValType(::oScript)="O" // busca la función en el Script

          RestoreEdit(::nNumEdit,Self)
          lScript:=::oScript:IsFunction(cMsg)

          IF lScript

             ::oScript:lNoError:=!::lMsgError
             lResp:=::oScript:RUN(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)
             PUBLICO(::cVarPublic,Self)

             IF !Empty(::oScript:cError) .AND. ::lMsgError
                 MensajeErr(::oScript:cError,"Programa"+::cScript+CRLF+"Function: "+cMsg)
             ENDIF

            RETURN lResp

          ENDIF

     _ClsSetError( _GenError( nError, ::ClassName(), cMsg ) )

   ENDIF

RETURN NIL
*/

/*
// Abre un Cursor
*/
FUNCTION OpenTable(cTable,lFill,oOdbc,lChecktable,lExcluye,lDicc,lLimit,lLower)
   LOCAL oTable,cSql,nAt:=0,aTablas,aData:={},cScrRun:="",lNativo:=.F.
   LOCAL nT1:=SECONDS()
   LOCAL cDsn
   LOCAL lTracer:=oDp:lTracer

   oDp:nTimerRun:=SECONDS() // JN Obtener el Tiempo de uso

   // No Revisa la Tabla
   IF Valtype(oOdbc)="O" .AND. lChecktable=NIL
      lChecktable:=.F.
   ENDIF

   // No Valida Diccionario de Datos
   IF Valtype(oOdbc)="O" .AND. lDicc=NIL
      lDicc:=.F.
   ENDIF

   DEFAULT lFill       :=.T.    // Todos los Registros
   DEFAULT lCheckTable :=.T.    // Revisa las tablas en DPTABLES
   DEFAULT nHandler    :=0
   DEFAULT aTableOpen  :={}
   DEFAULT lExcluye    :=oDp:lExcluye   // Verifica Exclusión de Where
   DEFAULT oDp:aExcluye:={}
   DEFAULT lDicc       :=.T.    // Buscar en el Diccionario de Datos
   DEFAULT lLimit      :=.T.    // Valida Limitaciones
   DEFAULT lLower      :=.F.    // Nombre de la tabla en minuscula

   // DEFAULT lStrSql   :=.F. // No esta Grabando Memos

   DEFAULT oDp:nMySqlT1:=SECONDS() // Toma el Tiempo de la Ultima Ejecución de MySql

   oDp:nMySqlT2:=SECONDS()

   // 1/2 hora sin usar tablas

   DEFAULT oDp:nMySqlTime:=SECONDS(),;
           oDp:nMySeconds:=130*30



//IF "VIEW_"$cTable .AND. !oOdbc=NIL
//   ? oOdbc:cName,cSql,"AQUI ES CUANDO SE INICIA TTABLE",GETPROCE()
//ENDIF

   // oDp:oFrameDp:SetText(LSTR(Seconds()-oDp:nMySqlTime))

/*
   // Verifica el Tiempo de MySql sin Utilización
   IF ABS(Seconds()-oDp:nMySqlTime) >= (oDp:nMySeconds) .OR. .T.
      EJECUTAR("MYSQLRESTART",oOdbc,cTable)
   ENDIF
*/


   oDp:lStrSql:=.F.

   IF !"SELECT "$UPPE(cTable) .AND. !"SHOW"$UPPER(cTable)
      cSql   :="SELECT * FROM "+cTable
   ELSE
      cSql   :=cTable
   ENDIF

   cSql:=STRTRAN(cSql,CHR(10)," ")
   IF ASC(RIGHT(cSQL,1))=10 .OR. ASC(RIGHT(cSQL,1))=0
      cSQL:=LEFT(cSQL,LEN(cSQL)-1)
   ENDIF

   cTable :=cTable(cSql)

   /*
   // Verifica el Tiempo de MySql sin Utilización
   */

   IF ABS(Seconds()-oDp:nMySqlTime) >= (oDp:nMySeconds) .AND. oDp:nMySeconds>0
      //  jn 09/09/2016 EJECUTAR("MYSQLRESTART",oOdbc,cTable)
   ENDIF

   // Tiempo Dormido
   oDp:nMySqlTime:=Seconds()

   // JN 03/12/2015

   IF !"VIEW_"$UPPER(LEFT(cTable,5)) .AND. lDicc

     cDsn   :=GETDSN(cTable)


     IF Empty(oOdbc) .AND. !Empty(cDsn) .AND. cDsn=oDp:cDsnData .AND. !Empty(GETDBSERVER())

// IF "USUARIO"$cSql
//  ? cDsn,"EN OPENTABLE 1",cSql,"ANTES DE GETDBSERVER"
// ENDIF
        oOdbc:=GETDBSERVER() // ? cDsn,oDp:cDsnData,"AQUI DEBEMOS INDICAR EL DSN"
        SETDBSERVER() // Remueve Asignación
     ENDIF

// ? cDsn,"EN OPENTABLE 2",cSql

   ENDIF

/*
   cTable :=ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql)))
   cTable :=ALLTRIM(IIF(" "$cTable ,LEFT(cTable,AT(" ",cTable)),cTable))
   cTable :=ALLTRIM(cTable)
*/

   // Revisa las tablas en DPTABLES
/*
   DEFAULT oDp:nTimesMysql:=Seconds()

   IF ValType(oDp:oMySqlCon)="O" .AND. ABS(Seconds()-oDp:nTimesMysql) > (60*30)
      oDp:oMySqlCon:ReConnect()
      oDp:nTimesMysql:=Seconds()
   ENDIF
*/

  IF ValType(oOdbc)!="O" //.AND. !oDp:lCgi

//IF "USUARIO"$cSql
//  ? cDsn,"EN OPENTABLE 2",cSql,"ANTES DE OPENTABLE",oDp:lOpenTableChk,"oDp:lOpenTableChk",lCheckTable,"lCheckTable",cTable,"<-cTable"
//ENDIF


      IF lCheckTable

        IF oDp:lOpenTableChk .AND. .F.  // Solo se Activa en Programa DPINI
           // 13/05/2021 GENERA INCIDENCIA NOMINA cTable:=EJECUTAR("OPENTABLECHK",cTable,oOdbc)
           EJECUTAR("OPENTABLECHK",cTable,oOdbc)
        ENDIF

        aTablas:=GetTables()

        IF EMPTY(aTablas)
          LOADTABLAS()
          aTablas:=GetTables()
        ENDIF

        cTable := ALLTRIM(UPPER(ALLTRIM(cTable)))
        nAt    :=ASCAN(aTablas,{|aVal| aVal[2] == cTable })

        IF nAt=0 .AND. !("VIEW_"$UPPER(cTable))

          IF FILE("STRUCT\"+cTable+".TXT")
            MsgRun("Importante Tabla "+cTable)
            EJECUTAR("DPFILSTRTAB",cTable,.T.)
            LOADTABLAS()
            aTablas:=GetTables()
          ENDIF

          nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTable) })

        ENDIF

/*
jn 01/05/2014, si es una vista y no esta en dptablas, tratara de importarla
        IF nAt=0
           EJECUTAR("DPTABLAIMPORT",cTable,cSql)
           aTablas:=GetTables()
           nAt    :=ASCAN(aTablas,{|aVal| aVal[2] == cTable })
        ENDIF
*/
        IF nAt=0 // Busca por la Tabla Compatible

          nAt:=ASCAN(oDp:aVistas,{|aVal| UPPE(aVal[2])== UPPE(cTable) })

          IF nAt>0
            cDsn   :=oDp:aVistas[nAt,3] // Apertura del DSN
          ENDIF

          // Importa la tabla desde Otra aplicación
          IF nAt=0
            EJECUTAR("DPTABLAIMPORT",cTable,cSql)
            aTablas:=GetTables()
            nAt    :=ASCAN(aTablas,{|aVal| aVal[2] == cTable })
          ENDIF


        ELSE

          IF Empty(aTablas[nAt,3])
             oDp:cDsnData:=oDp:aDsn[2,7] // GETINI(oDp:cPathExe+"DATAPRO.INI","DSNNAME" )    // DSN de la Empresa Principal
             aTablas[nAt,3]:=oDp:cDsnData
             cDsn   :=oDp:cDsnData
          ELSE
             cDsn   :=aTablas[nAt,3] // Apertura del DSN
          ENDIF

          IF LEN(aTablas[nAt])>=9 .AND.  aTablas[nAt,9]!=NIL
            cSql:=STRTRAN(cSql+" "," "+ALLTRIM(UPPE(cTable))+" "," "+ALLTRIM(aTablas[nAt,9])+" ")
            cSql:=SETFIELDAS(cSql,aTablas[nAt,11])
          ENDIF

        ENDIF

         IF nAt=0 .AND. lDicc

           MensajeErr("Tabla "+cTable+" no está Incluida en el Diccionario de Datos ","Tablas: "+LSTR(LEN(aTablas)))

           RETURN NIL

        ENDIF

        DPWRITE("TEMP\FILE.TXT","TABLE:"+cTable+" DSN "+cDsn)

        IF !Empty(cDsn) //Empty(aTablas[nAt,3])

          // Cuando viene por Parametro no requiere abrir la Base de datos
           DEFAULT oOdbc  :=OPENODBC(cDsn) // aTablas[nAt,3]) // Apertura del DSN

         ENDIF

       ENDIF

      // nAt   :=IIF( nAt=0 , ASCAN(aTablas,{|aVal| aVal[2] == cTable }) ,nAt)
/*
       IF nAt>0 .AND. LEN(aTablas[nAt])>=9 .AND.  aTablas[nAt,9]!=NIL

         cSql:=STRTRAN(cSql+" "," "+ALLTRIM(UPPE(cTable))+" "," "+ALLTRIM(aTablas[nAt,9])+" ")
         cSql:=SETFIELDAS(cSql,aTablas[nAt,11])

         oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN

       ELSEIF nAt>0

         oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN

       ENDIF
*/

       IF !ValType(oOdbc)="O"

/*
         IF cDsn=oDp:cDsnData
            oOdbc:=GETDBSERVER() // ? cDsn,oDp:cDsnData,"AQUI DEBEMOS INDICAR EL DSN"
? "AQUI ASUME GETDBSERVER",cDsn,oDp:cDsnData,"AQUI DEBEMOS INDICAR EL DSN"

         ENDIF
*/
         // JN 16/12/2015, no es necesario hacer

//IF "USUARIO"$cSql
  //? cDsn,"EN OPENTABLE 2",cSql,"BUSCAR cDsn EN OPENODBC",ErrorSys(.T.),ValType(cDsn),CLPCOPY(cDsn)
//ENDIF

         oOdbc  :=OPENODBC(cDsn) // Apertura del DSN

       ENDIF

     ELSE

       lDicc:=.F. // No requiere buscar en el Diccionario de Datos

   ENDIF

   IF !lFill .AND. !" WHERE "$cSql .AND. !" LIMIT "$cSql  // Solo los Campos
// JN 30/04/2014
/*
      IF " ORDER "$cSql
        cSql=STRTRAN(cSql," ORDER BY "," WHERE 1=0 ORDER BY ")+" LIMIT 0"
      ELSE
        cSql:=cSql+" WHERE 1=0 LIMIT 0"
      ENDIF
*/
      IF " ORDER "$cSql
        cSql=STRTRAN(cSql," ORDER BY "," ORDER BY ")+" LIMIT 0"
      ELSE
        cSql:=cSql+" LIMIT 0"
      ENDIF


   ENDIF

   oOdbc:cType:=IIF(Empty(oOdbc:cType),oDp:cTypeBD, oOdbc:cType)


   DEFAULT oDp:cSqlAlert:=""

   IF !Empty(oDp:cSqlAlert) .AND. oDp:cSqlAlert$cSql
       MensajeErr(cSql+CRLF+oDp:cSqlAlert+GETPROCE(),"SQLALERT")
   ENDIF

//   IF "DPCLIENTES"$cTable
//      MensajeErr(cSql+GETPROCE())
//   ENDIF


   cSql  :=BuildLimit(cSql,oOdbc:cType)

   IF lFill .AND. lExcluye .AND. (nAt:=ASCAN(oDp:aExcluye,{|a,n|cTable==a[1]}),nAt>0)
      cSql:=BuildExcluye(cSql,oDp:aExcluye[nAt,2],oDp:aExcluye[nAt,3])
   ENDIF

   lNativo:=.F.

   oOdbc:Use()

//  METHOD New( oCursor,cSql,oOdbc,cTable,nOption,cPrimary,lCheck,lLimit,lLower  ) CLASS Ttable
//  oTable:=TTable():New( NIL, cSql , oOdbc ,NIL , IIF(lFill , 0 , 1 ) , NIL, lLower)


// IF "VIEW_"$cTable
//   ? oOdbc:cName,cSql,GETPROCE()
// ENDIF


// ? GETPROCE(),"BUSCANDO MENSAJE LIMIT ANTES DE TTABLE"
   oTable:=TTable():New( NIL, cSql , oOdbc ,NIL , IIF(lFill , 0 , 1 ) , NIL,NIL,NIL, lLower)

   oTable:lDicc:=lDicc   // 28/04/2023

// ? GETPROCE(),"BUSCANDO MENSAJE LIMIT LUEGO DE TTABLE"
// ,lCheckTable,lLimit)

   IF Empty(oTable:aFields)

      EJECUTAR("CHKSQL",cSql,NIL,NIL,oOdbc)
      oTable:=TTable():New( NIL, cSql , oOdbc ,NIL , IIF(lFill , 0 , 1 ) , NIL,NIL,NIL, lLower)

      IF Empty(oTable:aFields)

        cScrRun:=VP("SCRPROGRAM")
        cScrRun:=IIF( ValType(cScrRun)<>"C" , "" , "DpxBase/Run:"+cScrRun )

        IF oDp:lSqlSayErr
          ClpCopy(oTable:cSql)
          MensajeErr(oTable:cSql + CRLF + "Conexion:"+oOdbc:cDSN+"/"+oOdbc:ClassName()+CRLF+GetProce(),"Expresión Inválida sin Estructura "+cScrRun)
        ENDIF

        oTable:lError:=.T.

      ENDIF

   ENDIF

   oTable:lFound:=oTable:RecCount()>0

   // JN 08/09/2013
   // IF nAt>0.AND.LEN(aTablas[nAt])>7
   //
   IF lDicc .AND. nAt>0.AND.LEN(aTablas[nAt])>7
     oTable:cSingular:=aTablas[nAt,8] // Nombre Singular
   ELSE
     oTable:cSingular:=""
   ENDIF


   IF oTable:lMySqlNativo

      // Verifica los Valores Numericos
      AEVAL(oTable:aFields,{|a,n| IIF( a[2]="N" , oTable:CTONUM(a[1]) , NIL ) ,;
                                  IIF( a[2]="D" , oTable:CTOD(a[1])   , NIL ) ,;
                                  IIF( a[2]="L" , oTable:CTOLOG(a[1]) , NIL ) })
   ELSE

     oTable:oCursor:End() // MS-SQL lo requiere

   ENDIF

   nHandler++
   oTable:nHandler:=nHandler
   AADD(aTableOpen,{oTable,nHandler}) // ,GetProce()})

   oDp:cSql:=cSql

   oTable:nSeconds:=SECONDS()-nT1

   IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
     oDp:lTracer:=.F.
     EJECUTAR("DPTRAZASQL",cSql,cTable,oOdbc:cDsn,oTable:nSeconds,NIL,GETPROCE(),oDp:cDpXbaseLine)
     oDp:lTracer:=lTracer
   ENDIF

   oDp:nMySqlTime:=SECONDS() // Siempre se inicia el tiempo, toda vez que realiza un Query
   oDp:nSeconds  :=nT1-SECONDS()

   oDp:lExcluye  := .T. // 16/04/2014 Retorna la exclusión

RETURN oTable

/*
// Convierte el Contenido de una Tabla en Array, ideal Para ComboBox
*/
FUNCTION aTable(cSql,lSort,oDb)
  LOCAL oTable,aArray:={},I,U,cLine:=""

  DEFAULT lSort:=.F.

  // ="SELECT MNU_MODULO,MNU_TITULO FROM DPMENU WHERE MNU_VERTIC='A' ORDER BY MNU_MODULO"
  IF !ChkSql(cSql)
     MensajeErr(cSql+ CRLF + GetProce() , "Error Function aTable")
     RETURN {}
  ENDIF

  oTable :=OpenTable(cSql,.T.,oDb)

  FOR I=1 TO LEN(oTable:aDataFill)
     cLine:=""
     FOR U := 1 TO LEN(oTable:aDataFill[I])
       cLine:=cLine+IIF( EMPTY(cLine),"" ," ")+CTOO(oTable:aDataFill[I,U],"C")
     NEXT U
     AADD(aArray,cLine)
  NEXT
  oTable:End()

  IF lSort
    ASORT(aArray)
  ENDIF

RETURN aArray
/*
// Quita el slash
*/
FUNCTION STRSQL(uValue,cTypeBD) // Quita el slash por Chr(28)

   DEFAULT cTypeBD:=oDp:cTypeBD

   IF ValType(uValue)="D" // Fecha en SQL
     RETURN SQLDATE(uValue)
   ENDIF

   IF ValType(uValue)="L"
      RETURN IIF(uValue,"1","0")
   ENDIF

   IF ValType(uValue)="N"
      uValue:=ALLTRIM(STR(uValue))
   ENDIF

/*   IF ValType(uValue)="C"
      uValue:=STRTRAN(uValue,"'",CHR(29)) // "CUALQUIER COSA SDFSDFSDFD "+CRLF //+MEMOREAD("\DWH\PRG\WINDOW.PRG")
      uValue:=REPLI("X",200)
      ? GetProce(),uValue
   ENDIF
*/

  oDp:lStrSql:=.T.

//  IF ValType(uValue)="C".AND. oDp:cTypeBD="MYSQL"
//     lStrSql:=.F.
//     RETURN ALLTRIM(uValue)
//  ENDIF

  IF ValType(uValue)="C" .AND. cTypeBD="MSSQL" .AND. (CHR(28)$uValue .OR. "'"$uValue)
      uValue:=STRTRAN(uValue,CHR(28),"/")
      uValue:=STRTRAN(uValue,"'",CHR(250))
      oDp:lStrSql:=.F.
      RETURN uValue
   ENDIF

   IF ValType(uValue)="C" .AND. "\"$uValue
     uValue:=STRTRAN(uValue,"\","\\")
     uValue:=ALLTRIM(uValue)
    // ? "AQUI ES",GetProce(),uValue
   ENDIF

   IF ValType(uValue)="C" .AND. [']$uValue
      uValue:=STRTRAN(uValue,"'",[\'])
      uValue:=ALLTRIM(uValue)
   ENDIF


/* Queda Reemplazado por "/'"
//   IF ValType(uValue)="C".AND.("\"$uValue .OR. "'"$uValue)
//      uValue:=STRTRAN(uValue,"\",CHR(28)) // "CUALQUIER COSA SDFSDFSDFD "+CRLF //+MEMOREAD("\DWH\PRG\WINDOW.PRG")
//      uValue:=STRTRAN(uValue,"'",CHR(29)) // "CUALQUIER COSA SDFSDFSDFD "+CRLF //+MEMOREAD("\DWH\PRG\WINDOW.PRG")
//   ENDIF
*/

  oDp:lStrSql:=.F.

RETURN uValue

FUNCTION STRSQLOFF(uValue,cTypeBD) // Quita el slash por Chr(28)

   DEFAULT cTypeBD:=oDp:cTypeBD

   oDp:lStrSql:=.T.

   IF ValType(uValue)="C" .AND. cTypeBD="MSSQL" .AND. CHR(250)$uValue
      uValue:=STRTRAN(uValue,CHR(250),"'")
      oDp:lStrSql:=.F.
      RETURN uValue
  ENDIF


  IF ValType(uValue)="C".AND. CHR(29)$uValue
     uValue:=STRTRAN(uValue,CHR(29),"'") // "CUALQUIER COSA SDFSDFSDFD "+CRLF //+MEMOREAD("\DWH\PRG\WINDOW.PRG")
  ENDIF

  IF ValType(uValue)="C" .AND. (CHR(28)$uValue .OR. CHR(29)$uValue)
     uValue:=STRTRAN(uValue,CHR(28),"\") // "CUALQUIER COSA SDFSDFSDFD "+CRLF //+MEMOREAD("\DWH\PRG\WINDOW.PRG")
     uValue:=STRTRAN(uValue,CHR(29),"'") // "CUALQUIER COSA SDFSDFSDFD "+CRLF //+MEMOREAD("\DWH\PRG\WINDOW.PRG")
   ENDIF

   oDp:lStrSql:=.F.

RETURN uValue

FUNCTION cTokenWhere(cField,cStrSql,cTable,lSQL)
   LOCAL oTable,cWhere:="",aLen:={}
   LOCAL aFields,nAt,I,nLen:=1,nLenField,xValue,cValue
   LOCAL aStrSql:={cStrSql}

   IF ValType(cStrSql)="C"
     aStrSql:=_VECTOR(cStrSql,",") // Valores con Tipos de datos Diferentes
   ENDIF

   DEFAULT lSql:=.T.

   cField :=STRTRAN(cField,"+",",")

   IF !","$cField // Un Solo Campo
      RETURN cField+GetWhere("=",cStrSql) // cField+"='"+CTOO(lStrSql,"C")+"'"
   ENDIF

   aFields:=_VECTOR(STRTRAN(cField,"+",","))
   oTable :=OpenTable(cTable,.F.)

   FOR I=1 TO LEN(aFields)

      nAt:=oTable:FieldPos(ALLTRIM(UPPE(aFields[I])))

      IF nAt>0

         nLen   :=oTable:FieldLen(nAt)

         IF oTable:aFields[nAt,2]="C"
            xValue :=LEFT(cStrSql,nLen)
            cStrSql:=SUBS(cStrSql,nLen+1,LEN(cStrSql))
         ELSE
            xValue:=CTOO(aStrSql[I],oTable:aFields[nAt,2])
         ENDIF

         cValue:=xValue

         IF oTable:aFields[nAt,2]="D"  .AND. !lSql

           cValue:='CTOD("'+xValue+'")'

           cWhere:=cWhere+IIF( !EMPTY(cWhere), " AND " , "") + ;
                   aFields[I]+"="+cValue

         ELSE

           cWhere:=cWhere+IIF( !EMPTY(cWhere), " AND " , "") + ;
                   aFields[I]+GetWhere("=",cValue)

         ENDIF

      ENDIF

   NEXT

   oTable:End()

// ? cWhere

RETURN cWhere

/*
// Convierte el Contenido de una Tabla en Array, ideal Para ComboBox
*/
FUNCTION TABLEDELETE(cTable,cWhere,lDepende)
   LOCAL cSql,aTablas,nAt,oOdbc

   DEFAULT lDepende:=.T. // Borra todos los Dependientes

   IF EMPTY(cWhere)
     MsgAlert("TABLEDELETE()) Requiere, Clausula WHERE ")
     Return .F.
   ENDIF

   cTable :=ALLTRIM(UPPER(cTable))
   aTablas:=GetTables()
   nAt    :=ASCAN(aTablas,{|aVal| aVal[2] == cTable })

   IF nAt=0
      MensajeErr("Tabla "+cTable +" no está Incluida en el Diccionario de Datos","FUNCION TABLEDELETE")
      RETURN .T.
   ENDIF

   oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN
   cSql  :="DELETE FROM "+ALLTRIM(cTable)+" WHERE "+cWhere

   IF oDp:lTracerSql
      ? cSql
   ENDIF

   oOdbc:Execute(cSql)

RETURN .T.

/*
// Obtiene una Lista Limpio de los Campos
*/
FUNCTION OnlyFields(cFields)
   // QUITAR COMANDOS
   LOCAL aFields,I,nAt

   cFields:=STRTRAN(cFields,"+",",")
   aFields:=_VECTOR(cFields,",")

   FOR  I:= 1 TO LEN(aFields)
      aFields[i]:=alltrim(aFields[i])
      // ? i,aFields[i]
      nAt       :=AT(" ",aFields[I])
      aFields[I]:=ALLTRIM(IIF(nAt>0,LEFT(aFields[I],nAt),aFields[I]))
   NEXT

RETURN aFields

//
// Actualiza las Tablas Relacionadas
//
/*
CLASS TUpdate

   DATA   aFields
   DATA   aValues
   DATA   aTables
   DATA   aFieldsI // Campos del Inner Join
   DATA   aWhere

   DATA   cWhere
   DATA   cTable

   DATA   oTable

   METHOD  New( cTable ) CONSTRUCTOR
   METHOD  InnerJoin(cTable2, cCampo2, cCampo  )
   METHOD  Replace(cField, cValor )
   METHOD  Commit(cWhere)
   METHOD  Where(cWhere) INLINE AADD(::aWhere,cWhere)
   METHOD  End()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( cTable )  CLASS TUpDate

   ::cTable  :=cTable
   ::aFields :={}
   ::aValues :={}
   ::aFieldsI:={} // Campos del Inner Join
   ::aTables :={}
   ::aWhere  :={} // Where General

   // ::oTable:=OpenTable(::cTable,.F.) // Solo el Cursor

RETURN SELF

METHOD Replace(cField, lStrSql ) CLASS TUpdate
  LOCAL nAt:=0,cLink

  IF ValType(lStrSql)="C"
    nAt  :=ASCAN(::aFieldsI,{|a,n|a[1]=ALLTRIM(lStrSql)})
  ENDIF

  cLink:=IIF( nAt=0,"" ,::aFieldsI[nAt,2] )

  AADD(::aFields,{cField,lStrSql,cLink})

RETURN NIL

// Enlace con la Tabla
// cTable Join Indica la Posición de la Tabla en el UpDate

METHOD  InnerJoin(cTable, cField, cLink, cTableJoin  ) CLASS TUpdate
   LOCAL aFields,oTable,I,nAt

   DEFAULT cTableJoin:=cTable

   oTable:=OpenTable(cTable,.F.)
   // Agrega los Campos de las Tablas Indexadas
   FOR I := 1 TO LEN(oTable:aFields)
      nAt:=ASCAN(::aFieldsI,{|a,n|a[1]=ALLTRIM(oTable:aFields[I,1])})
      IF nAt=0
        AADD(::aFieldsI,{ALLTRIM(oTable:aFields[I,1]),cTable})
      ENDIF
   NEXT

   AADD(::aTables,{cTable,cField,cLink,cTableJoin})

   oTable:End()

RETURN NIL

METHOD Commit() CLASS TUpDate
   LOCAL cSql :="",I,oTable,cInner:="",cWhere,cSet:="",cTables:=::cTable
   LOCAL oOdbc:=GetOdbc(::cTable),aFields:={},aSets:={},aSetd:={},cNewTable:=""
//   LOCAL oTable

   oTable:=OpenTable("SELECT LNK_FIELDS,LNK_FIELDD,LNK_TABLES,LNK_TABLED,LNK_SETS,LNK_SETD FROM DPLINK WHERE LNK_TABLES"+GetWhere("=",::cTable)+" AND LNK_UPDATE=1",.T.)

   oTable:GoTop()

   ? oTable:LNK_TABLES,oTable:LNK_TABLED
   IF !oTable:Eof() // Encontró, Asumen Como Principal al Primer Relacionado
      cTables:=ALLTRIM(oTable:LNK_TABLED)
   ENDIF

   ? cTables
   // Tambien las Claves
   aSets:=_VECTOR(ALLTRIM(oTable:LNK_FIELDS),",")
   aSetd:=_VECTOR(ALLTRIM(oTable:LNK_FIELDD),",")

   FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
     // ::InnerJoin(ALLTRIM(oTable:LNK_TABLED),ALLTRIM(oTable:LNK_FIELDD), ALLTRIM(oTable:LNK_FIELDS),ALLTRIM(oTable:LNK_TABLES))
      AADD(::aFields,{ALLTRIM(oTable:LNK_TABLED)+"."+aSetd[I],aSets[I],ALLTRIM(oTable:LNK_TABLES)})
   NEXT

   // Campos Comunes
   aSets:=_VECTOR(ALLTRIM(oTable:LNK_SETS),",")
   aSetd:=_VECTOR(ALLTRIM(oTable:LNK_SETD),",")

   FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
      ::InnerJoin(ALLTRIM(oTable:LNK_TABLED),ALLTRIM(oTable:LNK_FIELDD), ALLTRIM(oTable:LNK_FIELDS),ALLTRIM(oTable:LNK_TABLES))
      AADD(::aFields,{ALLTRIM(oTable:LNK_TABLED)+"."+aSetd[I],aSets[I],ALLTRIM(oTable:LNK_TABLES)})
   NEXT

   oTable:End()

   //? oTable:Reccount(),"cantidad",::cTable
   // Lista de Tablas
   FOR I:=1 TO LEN(::aTables)
      IF !::aTables[I,4]$cTables
        cTables := cTables + IIF(EMPTY(cTables),"" ,",")+ ::aTables[I,4]
      ENDIF
   NEXT

   FOR I := 1 TO  LEN(::aFields)
      cSql+= IIF( Empty(cSql) , "" , "," ) + ::aFields[i,1]
   NEXT

   // Campos de Otras Tablas
   FOR  I:= 1 TO LEN(::aFields)
      IF !EMPTY(::aFields[I,3])
        cSql+= IIF( Empty(cSql) , "" , "," ) + ::aFields[i,3]+"."+::aFields[I,2]+" AS "+::aFields[I,2]
      ENDIF
   NEXT

   FOR I:= 1 TO LEN(::aTables)
      cInner:=cInner+::aTables[I,1]+"."+::aTables[I,2]+"="+::cTable+"."+::aTables[I,3]
      cWhere:=::aTables[I,2]+"="+::aTables[I,3]
   NEXT

   FOR I := 1 TO LEN(::aWhere)
      cInner+=IIF( Empty(cInner),"" ," AND " )+::aWhere[I]
   NEXT

   cInner:=IIF( !EMPTY(cInner)," WHERE "+cInner , "" )

   cSet:=""
   FOR I := 1 TO LEN(::aFields)
     cSet:= cSet + IIF( Empty(cSet) , "" , "," ) + IIF( "."$::aFields[I,1],"",::cTable+".")+::aFields[I,1] + ;
            IIF( Empty(::aFields[I,3]) ,;
                 GetWhere("=",::aFields[I,2]),;
                 "="+::aFields[I,3]+"."+::aFields[I,2])
    // oTable:FieldGet(::aFields[I,3]))
    // oTable:Replace(::aFields[I,1],::aFields[I,2])
   NEXT

   cSQL:="UPDATE "+cTables+" SET "+cSet+" "+cInner

   ? cSql

   oOdbc:Execute(cSql)

   IF !EMPTY(oDp:cMsgOdbc)
     ? oDp:cMsgOdbc
     RETURN SELF
   ENDIF

   //cWhere:=::aTables[1,2]+GetWhere("=",oTable:FieldGet(::aTables[1,3]))
   //   oDp:lTracerSql:=.T.
   //   oTable:Commit(cWhere)
   //   ? oTable:DEP_CODIGO,"DEP_CODIGO"
   //   ? oTable:cSql
   //   oTable:DbSkip()
   // ENDDO

RETURN NIL


METHOD End()  CLASS TUpdate

//   oTable:End()

RETURN NIL
*/

/*
// Realiza la Actualización de las Tablas Asociadas
*/
#DEFINE TABLE 1
#DEFINE FIELD 3
#DEFINE NEW   5
#DEFINE OLD   6

FUNCTION ReplaceJoin(cTable,oTable,oTableOld)
   LOCAL aSets,aSetd,I,aFields:={},oLink,lStrSql,lStrSqlOld,nContar:=0
   LOCAL cTableUpdate:="",oTableUpDate,cWhere,aData,cFields:=""

   IF oTableOld:RecCount()=0 // No esta en condiciones de hacer reemplazos
      return .F.
   ENDIF

//   ? oTableOld:cTable,"OLD",oTableOld:RecCount()
   oTableOld:GoTop()

//   ? "ReplaceJoin: ",cTable

   oLink:=OpenTable("SELECT LNK_FIELDS,LNK_FIELDD,LNK_TABLES,LNK_TABLED,LNK_SETS,LNK_SETD FROM DPLINK WHERE LNK_TABLES "+GetWhere("=",cTable)+" AND LNK_UPDATE=1 ORDER BY LNK_TABLED",.T.)
   oLink:GoTop()

   // Tambien las Claves
   WHILE !oLink:Eof()
      // Campos de Relación
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_FIELDS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_FIELDD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
         lStrSql   :=IIF(oTable:IsDef(aSets[I]   ),oTable:Get(aSets[I])   ,NIL)
         lStrSqlOld:=IIF(oTableOld:IsDef(aSets[I]),oTableOld:Get(aSets[I]),NIL)
         // ? oLink:LNK_TABLED,"RepaceJoin",i,aSets[I],aSetd[I],lStrSql,lStrSqlOld,oTableOld:Reccount()
         AADD(aFields,{ALLTRIM(oLink:LNK_TABLED),cTable,aSets[I],aSetd[I],lStrSql,lStrSqlOld,.t.})
      NEXT
      // Ahora los Campos, Comunes
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_SETS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_SETD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
        lStrSql   :=IIF(oTable:IsDef(aSets[I]   ),oTable:Get(aSets[I])   ,NIL)
        lStrSqlOld:=IIF(oTableOld:IsDef(aSets[I]),oTableOld:Get(aSets[I]),NIL)
        // ? oLink:LNK_TABLED,"RepaceJoin",i,aSets[I],aSetd[I],lStrSql,lStrSqlOld,oTableOld:Reccount()
        AADD(aFields,{ALLTRIM(oLink:LNK_TABLED),cTable,aSets[I],aSetd[I],lStrSql,lStrSqlOld,.f.})
      NEXT
      oLink:DbSkip(1)
   ENDDO

   // Vamos hacer un Update por Tabla
   nContar:=1
   cWhere :=""
   WHILE nContar<=LEN(aFields)
      cTableUpDate:=aFields[nContar,1]
      aData       :={}
      cFields     :=""
      cWhere      :=""
      WHILE nContar<=LEN(aFields) .AND. cTableUpDate=aFields[nContar,1]

         // ? aFields[nContar,4],cWhere," Generando WHERE"

         IF aFields[nContar,7] .AND. !aFields[nContar,4]$cWhere
           cWhere:=cWhere+IIF( EMPTY(cWhere),"" ," AND " )+aFields[nContar,4]+GetWhere("=",aFields[nContar,OLD])
         ENDIF

         IF aFields[nContar,5]=aFields[nContar,6] // Comparación de Cambios
            nContar++
            LOOP
         ENDIF

         AADD(aData,{aFields[nContar,4],aFields[nContar,NEW]})
         cFields:=cFields + IIF( Empty(cFields) , "" , "," ) + aFields[nContar,4]
         nContar++

      ENDDO

      IF !EMPTY(aData)
        cWhere:=IIF( "WHERE "$cWhere,"" ," WHERE ")+cWhere
        oTableUpdate:=OpenTable("SELECT "+cFields+" FROM "+cTableUpDate,.F.)
        oTableUpdate:lAppend:=.F.

        FOR I:= 1 TO LEN(aData)
          oTableUpdate:Replace(aData[I,1],aData[I,2])
        NEXT

        // oDp:lTracerSql:=.T.
        oTableUpdate:cSql:=oTableUpdate:cSql+" "+cWhere // Puede Actualizar Las Dependientes de él
        oTableUpdate:lOnly:=.F. // Sólo campos de la tabla
        oTableUpdate:Commit(cWhere,.F.,.F.,.F.)
        // oDp:lTracerSql:=.F.
        oTableUpdate:End()

      ENDIF

   ENDDO


RETURN .T.

/*
// Actualiza las tablas de Manera Relacional
*/
FUNCTION UpdateJoin(cTable)
   LOCAL aSets,aSetd,I,aFields:={},oLink,nContar:=0
   LOCAL cWhere,aData,cFields:="",cTableUpdate,cSet,oOdbc,cSql,aTables:={},cWhereOr

   oLink:=OpenTable("SELECT LNK_FIELDS,LNK_FIELDD,LNK_TABLES,LNK_TABLED,LNK_SETS,LNK_SETD FROM DPLINK WHERE LNK_TABLES "+GetWhere("=",cTable)+" AND LNK_UPDATE=1 ORDER BY LNK_TABLED",.T.)
   oLink:GoTop()

   DEFAULT aRuta:={}

   IF ASCAN(aRuta,cTable)>0
      // ? "YA REPITIO",cTable
      Return .F.
   ENDIF

   AADD(aRuta,cTable)

   // Tambien las Claves
   WHILE !oLink:Eof()
      // Campos de Relación
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_FIELDS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_FIELDD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
         AADD(aFields,{ALLTRIM(oLink:LNK_TABLED),cTable,aSets[I],aSetd[I],.t.})
      NEXT
      // Ahora los Campos, Comunes
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_SETS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_SETD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
        AADD(aFields,{ALLTRIM(oLink:LNK_TABLED),cTable,aSets[I],aSetd[I],.f.})
      NEXT
      oLink:DbSkip(1)
   ENDDO

   // Vamos hacer un Update por Tabla
   nContar:=1
   cWhere :=""
   WHILE nContar<=LEN(aFields)
      cTableUpDate:=aFields[nContar,1]
      aData       :={}
      cFields     :=""
      cSet        :=""
      cWhere      :=""
      cWhereOr    :=""
      WHILE nContar<=LEN(aFields) .AND. cTableUpDate=aFields[nContar,1]

         IF aFields[nContar,5] // Campos del Enlace
           cWhere:=cWhere+IIF( EMPTY(cWhere),"" ," AND " )+ALLTRIM(cTable)+"."+aFields[nContar,3]+"="+aFields[nContar,1]+"."+aFields[nContar,4]
         ELSE
           // Campos
           AADD(aData,{aFields[nContar,3],aFields[nContar,4]})
           // Este Where debe Indicar que los Campos debe Tener Valores Diferentes
           cWhereOr:=cWhereOr+IIF( EMPTY(cWhereOr),"" ," OR " )+ALLTRIM(cTable)+"."+aFields[nContar,3]+"<>"+aFields[nContar,1]+"."+aFields[nContar,4]
         ENDIF

         cFields:=cFields + IIF( Empty(cFields) , "" , "," ) + aFields[nContar,4]
         nContar++

      ENDDO

      IF !EMPTY(aData)

        cWhere:= " WHERE ("+cWhere+")"+IIF( Empty(cWhereOr),"" , " AND ("+cWhereOr+")" )

        //oTableUpdate:=OpenTable("SELECT "+cFields+" FROM "+cTableUpDate,.F.)
        //oTableUpdate:lAppend:=.F.
        //
        FOR I:= 1 TO LEN(aData)
          cSet:=cSet + IIF( Empty(cSet) , "" , "," )+ALLTRIM(cTableUpDate)+"."+aData[I,2]+"="+ALLTRIM(cTable)+"."+aData[I,1]
        NEXT

        cSql:="UPDATE "+cTableUpDate+","+cTable+" SET "+cSet +" "+cWhere

        oOdbc  :=GetOdbc(cTableUpDate)

//         ? cSql,cWhereOr
         oDp:cMsgOdbc:=""

         ? cSql,"updatejoin"

         oOdbc:Execute(cSql)
         IF !EMPTY(oDp:cMsgOdbc)
           ? oDp:cMsgOdbc,cSql
         ENDIF

         oOdbc:=NIL

         // ? cSet
        // oDp:lTracerSql:=.T.
        // oTableUpdate:cSql:=oTableUpdate:cSql+" "+cWhere // Puede Actualizar Las Dependientes de él
        // oTableUpdate:Commit(cWhere,.F.)
        // oDp:lTracerSql:=.F.
        // oTableUpdate:End()

      ENDIF
      AADD( aTables,cTableUpDate)
   ENDDO

   AEVAL(aTables,{|a,i|UpdateJoin(a)})

   // Ahora debe Procesar todas las demas tablas relacionadas

RETURN .T.

/*
// Genera Lista de las tablas asociadas que pueden Eliminar
*/

FUNCTION DeleteJoin(cTable,lIni,cWhereMain)
   LOCAL cSql   :="",cTableDelete:="",cWhere:="",cLista:="",cNewWhere:="",cListTable:=""
   LOCAL nContar:=0,I
   LOCAL aData  :={},aSets:={},aSetd:={},aFields:={},aTables:={}
   LOCAL oLink,oTable

   IF lIni
      aRuta:={}
   ENDIF

   oLink:=OpenTable("SELECT LNK_FIELDS,LNK_FIELDD,LNK_TABLES,LNK_TABLED FROM DPLINK WHERE LNK_TABLES "+GetWhere("=",cTable)+" AND LNK_DELETE=1 ORDER BY LNK_TABLED",.T.)
   oLink:GoTop()

   IF ASCAN(aRuta,cTable)>0
      Return .F.
   ENDIF

   AADD(aRuta,cTable)

   WHILE !oLink:Eof()
      // Campos de Relación
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_FIELDS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_FIELDD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
         AADD(aFields,{ALLTRIM(oLink:LNK_TABLED),cTable,aSets[I],aSetd[I]})
      NEXT
      oLink:DbSkip(1)
   ENDDO
   cListTable:="" // Lista de Tablas que Seran Borradas
   nContar   :=1
   cWhere    :=""
   WHILE nContar<=LEN(aFields)
      cTableDelete:=aFields[nContar,1]
      cWhere      :=""
      cLista      :=""
      // cWhere :=cWhere+IIF( EMPTY(cWhere),"" ,"(" )
      WHILE nContar<=LEN(aFields) .AND. cTableDelete=aFields[nContar,1]
        cLista:=cLista+IIF( EMPTY(cLista),"" ,"," )    +aFields[nContar,4]
        cWhere:=cWhere+IIF( EMPTY(cWhere),"" ," AND " )+ALLTRIM(cTable)+"."+aFields[nContar,3]+"="+aFields[nContar,1]+"."+aFields[nContar,4]
        nContar++
      ENDDO

      cSql:="SELECT "+cLista+" FROM "+cTableDelete+" INNER JOIN "+cTable+" ON "+cWhere+" "+cWhereMain
      // ? cTableDelete,cLista,cWhere,"AQUI DEBE BORRAR CON ESTO",cSql
      // ? cSql,"Sera Borrado"
      oTable:=OpenTable(cSql,.T.)
      IF oTable:RecCount()>0
        oTable:cPrimary:=cLista
        // ? "ESTE Si Tiene "+cSql
        CursorWait()
        SysRefresh()
        oTable:DeleteAll(.f.)
      ENDIF
      oTable:End()

      // Lista de Tablas
      // cWhere+=")"
//      AADD(aTables,cTableDelete)
//      cListTable:=cListTable+IIF(Empty(cListTable),"",",")+cTableDelete
   ENDDO

//   FOR I := 1 TO LEN(aTables)
//      cLista    :=""
//      cNewWhere :=DeleteJoin(aTables[I],.F.)
//      IF !EMPTY(cNewWhere)
//         cWhere    :=cWhere+" .AND. ("+cNewWhere+")"
//         cListTable+=","+cLista
//      ENDIF
//   NEXT

/*   IF lIni
      cListTable:=cTable+","+cListTable
      cWhere := "DELETE FROM "+cListTable+" USING "+cListTable+" "+cWhereMain+ " AND "+cWhere
   ENDIF */

RETURN .T.

//  DELETE FROM dptablas,dpcampos using dptablas,dpcampos  WHERE dptablas.tab_numero="2114" AND dptablas.tab_numero=dpcampos.cam_numtab
/*
// Determina los Campos Comunes Entre Dos Tablas
*/
FUNCTION GetFieldCommon(cTableS,cTableD)
   LOCAL aFields:={},oLink,aSets,aSetd,i

   oLink:=OpenTable("SELECT LNK_FIELDS,LNK_FIELDD,LNK_SETS,LNK_SETD FROM DPLINK WHERE LNK_TABLES "+GetWhere("=",cTableS)+" AND LNK_TABLED"+GETWHERE("=",cTableD)+;
                    " AND LNK_DELETE=1 ORDER BY LNK_UPDATE",.T.)

   oLink:GoTop()

   WHILE !oLink:Eof()
      // Campos de Relación
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_FIELDS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_FIELDD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
         AADD(aFields,{aSets[I],aSetd[I]})
      NEXT
      aSets:=_VECTOR(ALLTRIM(oLink:LNK_SETS),",")
      aSetd:=_VECTOR(ALLTRIM(oLink:LNK_SETD),",")
      FOR I := 1 TO MIN(LEN(aSets),Len(aSetd))
         AADD(aFields,{aSets[I],aSetd[I]})
      NEXT
      oLink:DbSkip(1)
   ENDDO
   oLink:End()

RETURN aFields

/*
// Obtiene la Clausula WHERE de la sentencia SQL
*/
FUNCTION GetSqlWhere(cSql,lLimit)
   LOCAL cWhere:=cSql,aQuitar:={" ORDER "," GROUP "},nAt,I

   DEFAULT lLimit:=.F. // Quitar Limit

   IF ValType(cSql)!="C" .OR. !" WHERE "$cSql
      RETURN ""
   ENDIF

   cWhere :=" "+cWhere

   cWhere :=Subs(cWhere,AT(" WHERE ",cWhere),LEN(cWhere))

   // ? cWhere,cSql,"GETSQLWHERE()"

   IF lLimit .AND. " LIMIT "$cWhere // Quita el Valor Limit
      cWhere:=LEFT(cWhere,AT(" LIMIT ",cWhere))
   ENDIF

   FOR I := 1 TO LEN(aQuitar)
     nAt:=AT(aQuitar[I],cWhere)

     IF nAt>0
       cWhere:=LEFT(cWhere,nAt)
     ENDIF

   NEXT

RETURN cWhere

/*
// Obtiene la Clausula Select de la sentencia SQL
*/
FUNCTION GetSqlSelect(cSql,lFromWhere)
   LOCAL cSelect

   DEFAULT cSql:=""
   DEFAULT lFromWhere:=.F.

   IF lFromWhere
      cSelect:=Left(cSql,AT( " WHERE ",cSql)-1)
   ELSE
      cSelect:=Left(cSql,AT( " FROM ",cSql)-1)
   ENDIF

   // ? cSelect , "GetSqlSelect"

RETURN cSelect

/*
// Obtiene el Valor menor de un Campo
*/
FUNCTION SqlGetMin(cTabla,cField,cWhere,nLimit,oDb)
   oDp:lExcluye:=.F.
RETURN SqlGetMax(cTabla,cField,cWhere,nLimit,.F.,oDb)

FUNCTION ASQL(cSql,oDb,lView,lChecktable,lExcluye,lDicc)
  LOCAL oTable,cError,aData

  DEFAULT lView:=.F.

  oDp:lErrorSql:=.F.

  CursorWait()

// ? "AQUI ES ASQL",cSql,GETPROCE()

  oTable:=OpenTable(cSql,.T.,oDb,lChecktable,lExcluye,lDicc) // lFill,oOdbc,lChecktable,lExcluye,lDicc)

  oDp:oTable:=oTable
  oDp:cSql  :=cSql
  oDp:cWhere:=oTable:cWhere

  IF lView
    ? oTable:ClassName()
    ViewArray(oTable:aFields)
  ENDIF

  IF Empty(oTable:aFields)

     oTable:End(.T.)

     cError   :=oDp:oMySqlCon:oError:GetError()

     IF !Empty(cError)
       MensajeErr(cError+CRLF+cSql)
     ENDIF

     oDp:lErrorSql:=.T.

     RETURN {}

  ENDIF

  aData:=ACLONE(oTable:aDataFill)

  oTable:End(.T.)

RETURN aData // oTable:aDataFill

/*
// Obtiene el Valor Mayor de un Campo
*/
FUNCTION SqlGetMax(cTabla,cField,cWhere,nLimit,lMax,oDb)
   LOCAL oTable,lStrSql,cOrder,cSql
   LOCAL cDsn

   DEFAULT lMax  :=.T.,;
           cWhere:="" ,;
           nLimit:=0

//,;
//         oDb   :=GETDBSERVER()


   oDp:lExcluye:=.F.

   cDsn  :=GETDSN(cTabla)

   IF cDsn=oDp:cDsnData
      DEFAULT oDb   :=GETDBSERVER()
   ENDIF

   cOrder :=" ORDER BY "+cField+ IF(lMax," DESC ","")+" LIMIT 1 "

   IF Empty(cWhere)
      cWhere:=" WHERE 1=1 "
   ENDIF

   lStrSql:=SQLGET(cTabla,cField,cWhere+" "+cOrder,NIL,oDb)

//? lStrSql,cTabla,cField,cWhere,nLimit,lMax,"lStrSql,cTabla,cField,cWhere,nLimit,lMax,oDb" , GETPROCE()

   oDp:cSqlMax:=oDp:cSql

   oDp:cSql:=cSql

   IF !EMPTY(nLimit) .AND. !EMPTY(lStrSql)

      cSql:="SELECT "+cField+" FROM "+cTabla+" WHERE "+cField+GetWhere("<",lStrSql)+;
            " ORDER BY "+cField+ " DESC LIMIT "+STR(nLimit+0)

      oTable:=OpenTable(cSql,.T.,oDb)
      oTable:GoBottom()


      lStrSql:=oTable:FieldGet(1)
      oTable:End()

      oDp:cSqlMax:=oDp:cSql

   ENDIF

   oDp:cSql:=cSql

RETURN lStrSql

/*
// QUITAR LIMIT y Convertir IF de MySQL hacia MSSQL CASE
*/
FUNCTION BuildLimit(cSql,cType)

    IF cType="MYSQL"
       RETURN cSql
    ENDIF

    IF oDp:lMySqlToMsql

       oDp:lBuildLimitSql:=.T. // Requerido por  MYSQLTOMSQL

// oDp:lTracer:=.T.

       cSql:=EJECUTAR("MYSQLTOMSQL",cSql,cType)

       RETURN cSql

    ENDIF

RETURN cSql

/*
   LOCAL cLimit,nAt,cIf,cIf2,nPar:=0,lOn:=.T.,cChar:=""

   DEFAULT cType:=oDp:cTypeBD

   IF !oDp:lBuildLimitSql .OR. !cType="MSSQL"
      RETURN cSql
   ENDIF

   cSql:=CIFTOMSQL(cSql,cType)

   DEFAULT lStrSql:=.F.

   IF cType$"MSSQL" .AND. "CONCAT("$cSql .AND. !lStrSql // NO MEMO

      nAt :=AT("CONCAT(",cSql)
      cSql:=STRTRAN(cSql,[','],[']+CHR(2)+['])
      nPar:=1 // Tengo el primer parentesis

      WHILE nAt<=LEN(cSql)

         cChar:=Subs(cSql,nAt,1)

         IF cChar=['] .OR. cChar=["]
            lOn:=!lOn
         ENDIF

         IF cChar=")" // Finaliza Concat

            nPar--

            IF nPar<2
               EXIT
            ENDIF

         ENDIF

         IF cChar="(" // Finaliza Concat
            nPar++
         ENDIF

         IF cChar=","  .AND. lOn
            cSql:=LEFT(cSql,nAt-1)+"+"+SUBS(cSql,nAt+1,LEN(cSql))
         ENDIF

         nAt++

      ENDDO

      cSql:=STRTRAN(cSql,[']+CHR(2)+['],[','])
      cSql:=STRTRAN(cSql,"CONCAT(","(")

   ENDIF

   IF !cType$"ADS.DBF.MSSQL" .OR. !" LIMIT "$cSql
      RETURN cSql
   ENDIF

   nAt   :=AT(" LIMIT ",cSql)
   cLimit:=ALLTRIM(SUBS(cSql,nAt,100))
   cSql  :=LEFT(cSql,nAt)

   IF (" WHERE "$(" "+cSql)) // WHERE NO NECESITA LIMIT
      RETURN cSQL
   ENDIF

   cLimit:=STRTRAN(cLimit,"LIMIT ","TOP ")
   cSql  :=STRTRAN(cSql,"SELECT ","SELECT "+cLimit+" ")


RETURN cSql
*/


/*
// Contruye Concat
*/
FUNCTION BUILDCONCAT(cField)
   IF ","$cField
      RETURN " CONCAT("+cField+")"
   ENDIF
RETURN cField

FUNCTION SQLDATE(dFecha,cTypeBD,lDateNull)
   LOCAL nD,nM,nA

   DEFAULT cTypeBD  :=oDp:cTypeBD,;
           lDateNull:=.F.

/*   IF oDp:cTypeBD="MSSQL"
      RETURN DTOS(dFecha)
   ENDIF
*/

   IF EMPTY(dFecha) .AND. ValType(oDp:bSqlDate)="B"

     IF cTypeBD="MSSQL" .OR. lDateNull
         RETURN 'NULL'
      ENDIF

      RETURN EVAL(oDp:bSqlDate,0,0,0)

   ENDIF

   // ? ValType(oDp:bSqlDate),"EN SQLDATE"

   IF !EMPTY(oDp:bSqlDate)
     dFecha:=IIF(ValType(dFecha)="D",dFecha,CTOO(dFecha,"D"))
     nD:=DAY(dFecha)
     nM:=MONTH(dFecha)
     nA:=YEAR(dFecha)
// ? nD,nM,nA,"nD,nM,nA"
     dFecha:=EVAL(oDp:bSqlDate,nD,nM,nA)
     RETURN dFecha
   ENDIF

RETURN DTOS(dFecha)

/*
// Elimina la Palabra Where
*/
FUNCTION KillWhere(cWhere)
  cWhere:=ALLTRIM(cWhere)
  WHILE UPPE(LEFT(cWhere,5))="WHERE"
    cWhere:=ALLTRIM(SUBS(cWhere,6,LEN(cWhere)))
  ENDDO
RETURN cWhere

/*
// Obtiene la Longitud de Tabla
*/
FUNCTION SQLFIELDLEN(cTabla,cField,cType,nLen,nDec)
   LOCAL oTable
   // cType,nLen,nDec Pasan por Referencia
   oTable:=OpenTable("SELECT "+cField+" FROM "+cTabla,.F.)

   cType :=oTable:aFields[1,2] // Pasa Por Ref
   nLen  :=oTable:aFields[1,3] // Pasa por Ref
   nDec  :=oTable:aFields[1,4] // Pasa por Ref
   oTable:End()

RETURN nLen

/*
// Revisa la Sintaxis del Código SQL
// @cError Valor por Referencia
*/
FUNCTION ChkSql(cSql,cError)
   LOCAL oTable,cTable:=CTABLE(cSql)
   LOCAL lResp

   IF Empty(cTable)
      cError:="Sentencia SQL requiere Tabla "
      RETURN .F.
   ENDIF

   IF !oDp:lChkSql // No revisa Sintaxis
      RETURN .T.
   ENDIF

   DPWRITE("TEMP\SQL"+LSTR(SECONDS())+".SQL",cSql)

   oTable:=OpenTable(cSql,.F.)
   lResp :=oTable:IsOk()

   IF !lResp
       cError:=oTable:cError
   ENDIF
   oTable:End()

RETURN lResp
/*
// Verifica si no Tiene Where
*/
FUNCTION ADDWHERE(cOld,cExp,cAnd,lPutWhere)

    DEFAULT cOld:=""
    DEFAULT cAnd:=" AND "

    IF ALLTRIM(UPPE(cExp))="WHERE" // Requiere el Where
      IF LEFT(ALLTRIM(UPPE(cOld)),5)!="WHERE"
          cOld:=" WHERE "+cOld
      ENDIF
      RETURN cOld
    ENDIF

    cExp:=cOld + IIF( Empty(cOld) , "" , cAnd  )+cExp

RETURN cExp

/*
// Determina la Mascara Numerica soportada por un campo
*/
FUNCTION FIELDPICTURE(cTable,cField,lMiles)
  LOCAL oTable,cPicture,nAt

  oDp:lSqlNativo:=.T.

  DEFAULT oDp:aFieldPict:={}

  nAt:=ASCAN(oDp:aFieldPict,{|a,n| a[1]=cTable .AND. a[2]=cField})

  IF nAt>0
     RETURN oDp:aFieldPict[nAt,3]
  ENDIF

  IF EJECUTAR("ISFIELDMYSQL",NIL,cTable,cField)

    oTable  :=OPENTABLE("SELECT "+cField+" FROM "+cTable+" WHERE 1=0",.F.)
    cPicture:=oTable:GetPicture(cField,lMiles)

    oTable:End()
    oDp:lSqlNativo:=.F.

    AADD(oDp:aFieldPict,{cTable,cField,cPicture})

  ENDIF

RETURN cPicture

/*
// Determina la Etiqueta de un Campo
*/
FUNCTION FIELDLABEL(cTable,cField)
  LOCAL oTable,cLabel,nAt

  DEFAULT oDp:aFieldLabel:={}

  nAt:=ASCAN(oDp:aFieldLabel,{|a,n| a[1]=cTable .AND. a[2]=cField})

  IF nAt>0
     RETURN oDp:aFieldLabel[nAt,3]
  ENDIF

  cLabel:=SQLGET("DPCAMPOS","CAM_DESCRI","CAM_TABLE"+GetWhere("=",cTable)+" AND CAM_NAME"+GetWhere("=",cField))
  cLabel:=STRTRAN(ALLTRIM(cLabel),";",CRLF)

  AADD(oDp:aFieldLabel,{cTable,cField,cLabel})

RETURN cLabel



/*
// Cierra Todas las Tablas
FUNCTION CLOSEALLTABLAS()
   LOCAL I

   FOR I := 1 TO LEN(aTablas)
      aTablas[I,2]:End()
      I=1
   NEXT

RETURN .T.
*/

FUNCTION SQLSEEK(cTable,cWhere,lMsg,oDb)
RETURN COUNT(cTable,cWhere,oDb)>0
//RETURN SQLGET(cTable,"COUNT(*) AS CUANTOS ",cWhere)!=0

/*
// Obtiene el Valor del Campo de una Tabla

FUNCTION XSQLGET(cTable,cField,cWhere,lMsg)
  LOCAL oTable,aData:={},aRow:={},cSql,lStrSql

  IF !Empty(cWhere)
     cWhere:=IIF( " WHERE "$cWhere , " " , " WHERE " )+cWhere
  ENDIF

  cSql      :="SELECT "+cField+" FROM "+cTable+cWhere

  oDp:aQuery:={}
  oDp:aRow  :={}

  IF "DPDOCCLI"$cSql
    ? cSql
  ENDIF

  oTable:=MyTable(cSql)
  lStrSql:=oTable:FieldGet(1)

  AEVAL(oTable:aFields,{|a,n| AADD(aRow,oTable:FieldGet(n)) })

  oDp:aRow:=aRow
  AADD(oDp:aQuery,{aRow})

  // ? lStrSql,LEN(aRow)

  oTable:End()

RETURN lStrSql
*/

/*
FUNCTION SQLGET(cTable,cField,cWhere,lMsg)
   LOCAL cSql,oTable,lStrSql

   DEFAULT lMsg:=.F.,cWhere:=""

   IF !Empty(cWhere)
      cWhere:=IIF( " WHERE "$cWhere , " " , " WHERE " )+cWhere
   ENDIF

   cSql      :="SELECT "+cField+" FROM "+cTable+cWhere

   oDp:aQuery:={}
   oDp:aRow  :={}

   // ? cSql,"SQLGET"
   // oDp:lMySqlNativo:=.T.
   // ?  CLPCOPY(cSql)

   oDp:lMySqlNativo:=.F.

   IF oDp:lMySqlNativo .AND. .F.

     oTable:=MyTable(cSql)
     lStrSql:=oTable:FieldGet(1)
     IF Empty(oTable:aFields)
       lMsg:=.F.
     END

     Aeval(oTable:aFields,{|a,n| AADD(oDp:aRow,oTable:FieldGet(n))})
     AADD(oDp:aQuery,{oDp:aRow})

   ELSE

     oTable:=OpenTable(cSql,.T.)

   ENDIF

   IF !Empty(oTable:aFields) .AND. !oDp:lMySqlNativo // ChkSql(cSql)

     // ? cSql,"aqui debe ser"

     lStrSql    :=oTable:Fieldget(1)

     IF LEN(oTable:aFields)=1 .AND. "SUM("$cSql
        lStrSql:=CTOO(lStrSql,"N")
     ENDIF

     // ? oTable:aFields[1,2]
     oDp:aQuery:=ACLONE(oTable:aDataFill)

     // ? cSql,"AQUI DEBE SER",lStrSql,LEN(oTable:aDataFill),ValType(lStrSql)

     IF !Empty(oTable:aDataFill)
        oDp:aRow  :=ACLONE(oTable:aDataFill[1])
     ENDIF

   ELSEIF !oDp:lMySqlNativo

      lMsg:=.T.

   ENDIF

   IF lMsg
     CLPCOPY(cSql)
     MemoWrit("ERROR.SQL",cSql)
     MensajeErr(cSql+CRLF+GetProce(),"SQLGET()")
   ENDIF

   oTable:End()

RETURN lStrSql
*/

/*
// Obtiene el Valor del Campo de una Tabla
*/
FUNCTION SQLGET(cTable,cField,cWhere,lMsg,oOdbc)
   LOCAL cSql,oTable,lStrSql

   DEFAULT lMsg:=.F.,cWhere:=""

   DEFAULT oOdbc:=GETDBSERVER() // Servidor Remoto

   IF !Empty(cWhere)
      // JN 07/03/2014

//      IF !("ORDER"$UPPE(cWhere) .OR. "GROUP"$UPPE(cWhere))
        cWhere:=IIF( " WHERE "$cWhere , " " , " WHERE " )+cWhere
   //      ENDIF

// 13/12/2013
//   ELSE
//      cWhere:=" LIMIT 1" // Trae un Solo Registro
   ELSE
      cWhere:=" WHERE 1=1 ORDER BY "+cField+" LIMIT 1"
   ENDIF

   oDp:lExcluye:=.F. // JN 25/05/2017

   cSql      :="SELECT "+cField+" FROM "+cTable+cWhere

   oDp:cTable   :=cTable
   oDp:aQuery   :={}
   oDp:aRow     :={}
   oDp:nRecCount:=0
   oDp:aRowEmpty:={} // JN 23/10/2014


   //? cSql,"SQLGET"
   oTable:=OpenTable(cSql,.T.,oOdbc)

   IF oTable=NIL
      RETURN NIL
   ENDIF

   // ? cSql,GetProce()

   IF !Empty(oTable:aFields) // ChkSql(cSql)
     // ? cSql,"aqui debe ser"

     lStrSql      :=oTable:FieldGet(1)
     oDp:aQuery   :=ACLONE(oTable:aDataFill)

     IF !Empty(oTable:aDataFill)
        oDp:aRow  :=ACLONE(oTable:aDataFill[1])
     ELSE
        AEVAL(oTable:aFields,{|a,n| AADD( oDp:aRowEmpty,oTable:FieldGet(n))})
     ENDIF

   ELSE
     lMsg:=.T.
   ENDIF

   IF lMsg
     CLPCOPY(cSql)
     MemoWrit("ERROR.SQL",cSql)
     MensajeErr(cSql+CRLF+GetProce(),"SQLGET()")
   ENDIF

   oTable:End()

RETURN lStrSql

/*
// Devuelve Todos los Campos de la Tabla
*/
FUNCTION SELECTFROM(cTable,lFrom,cExcluye,oDb,lChecktable)
   LOCAL cFields:=""
   LOCAL oTable:=OpenTable("SELECT * FROM "+cTable+" WHERE 1=0",.F.,oDb,lChecktable)

   DEFAULT lFrom:=.T.,cExcluye:=""

   AEVAL(oTable:aFields,{|a,n|IIF( a[1]$cExcluye , NIL , ;
                              cFields:=cFields + IIF( Empty(cFields) , "" , ",") + UPPE(ALLTRIM(cTAble))+"."+a[1])})
   oTable:End()

RETURN IIF( lFrom ,  "SELECT "+cFields+" FROM "+cTable , cFields )

/*
// Cuenta la Cantidad de Registros
*/
FUNCTION COUNT(cTable,cWhere,oDb,lSay)
   // LOCAL cTableName:=TableName(cTable),nCount:=0

   LOCAL nCount:=0

   DEFAULT cWhere:="",;
           lSay  :=.F.

   // Caso de las Vistas no estan en el Diccionario de Datos
   IF ValType(oDb)="C"
      oDb:=OPENODBC(oDb)
   ENDIF

   // oOdbc :=GetOdbc(cTableName)
   cWhere:=" "+cWhere

   IF !Empty(cWhere) .AND. !" WHERE "$cWhere
      cWhere:=" WHERE "+cWhere
   ENDIF

   oDp:cSql:="SELECT COUNT(*) AS CUANTOS FROM "+UPPE(cTable) + cWhere

   IF lSay
     ? oDp:cSql,"FUNCTION COUNT()",GETPROCE()
   ENDIF

   DEFAULT oDb:=GetOdbc(cTable) // Name)

   oDp:lExcluye:=.F.

   nCount  :=oDb:QueryData(oDp:cSql)
   nCount  :=CTOO(nCount,"N")

RETURN nCount

/*
// Actualiza un Campo Específico
*/
FUNCTION SQLPUT(cTable,cField,lStrSql,cWhere)
    LOCAL oTable,lResp:=.T.

    DEFAULT cWhere:=""

    cWhere:=" "+cWhere

    IF !Empty(cWhere) .AND. !" WHERE "$cWhere
       cWhere:=" WHERE "+cWhere
    ENDIF

    oTable:=OpenTable("SELECT "+cField+" FROM "+cTable + cWhere , .T.)

    IF !ChkSql(oTable:cSql)
       MensajeErr(oTable:cSql,"FUNCTION SQLPUT()")
       oTable:End()
       RETURN .F.
    ENDIF

    IF oTable:RecCount()>0
       oTable:Replace(cField,lStrSql)
       IF !oTable:Commit(cWhere)
           MensajeErr("No puede Grabar: Tabla "+cTable+" Campo:"+cField+ " Valor:"+CTOO(lStrSql,"C")+;
                      CRLF+cWhere,"SQLPUT()")
       ENDIF
    ELSE
       lResp:=.F.
    ENDIF

    oTable:End()

RETURN lResp

/*
// Actualiza un Campo Específico
*/
FUNCTION SQLDELETE(cTable,cWhere,lChkInt,oOdbc,lSay)
    LOCAL oTable,lResp:=.T.,oIntRef,cClave,cSql,cTableO:=cTable,lRet:=.T.
    LOCAL nT1 ,lTracer:=oDp:lTracer

    DEFAULT cWhere:="",lChkInt:=.F.   , lSay:=.T.

    IF ","$cTable
      cTable:=_VECTOR(cTable,",")[1]
    ENDIF

    cWhere:=" "+cWhere

    IF !Empty(cWhere) .AND. !" WHERE "$cWhere
       cWhere:=" WHERE "+cWhere
    ENDIF

    IF ","$cTableO
      cSql    :="DELETE "+cTable+" FROM "+cTableO+ cWhere
    ELSE
      cSql    :="DELETE FROM "+cTableO+ cWhere
    ENDIF

    oDp:cSql:=cSql

/*
    IF "DPDOCPRORTI"$cSql
       MensajeErr(CLPCOPY(cSql),"Borra en DPDOCPRORTI")
    ENDIF
*/
    // ? CLPCOPY(oDp:cSql)

    DPWRITE("TEMP\SQLDELETE_"+ALLTRIM(cTable)+".SQL",cSql)

    IF !lChkInt

      nT1:=SECONDS()

      DEFAULT oOdbc:=GetOdbc(cTable)

      IF oOdbc:cType="MYSQL" // Antes 3/1/2017 oDp:cTypeBD

        lRet:=oOdbc:Execute(cSql)

//MYEXECUTE(cTable,cSql,oOdbc)

      ELSE

        lRet:=oOdbc:Execute(cSql)

      ENDIF

      oDp:cSql:=cSql

      IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
        oDp:lTracer:=.F.
        EJECUTAR("DPTRAZASQL",cSql,cTable,oOdbc:cDsn,SECONDS()-nT1,NIL,GETPROCE(),oDp:cDpXbaseLine)
        oDp:ltracer:=lTracer
      ENDIF


      // oTable:=OpenTable(cTable,.F.)
      // IF !oTable:Execute(cSql)
      IF !lRet
        MensajeErr("No es posible Borrar "+cSql,"FUNCTION SQLDELETE()")
      ENDIF

      RETURN lRet

    ENDIF
    // ? cSql , "SELECT * FROM "+cTable+cWhere

    IF lChkInt .AND. !ChkSql("SELECT * FROM "+cTable+cWhere)
       MensajeErr(cSql,"FUNCTION SQLDELETE()")
       oTable:End()
       RETURN .F.
    ENDIF

    // ? cSql

    IF !IsIntRef(cTable) .OR. !lChkInt // No tiene Referencias

       oTable:=OpenTable("SELECT * FROM " + cTable + cWhere , .F. ,oOdbc)

       IF !oTable:Execute(cSql)
           oTable:End()
           // MensajeErr("No es posible Borrar "+cSql,"FUNCTION SQLDELETE()")
           RETURN .F.
       ENDIF

       oTable:End()

       RETURN .T.

    ENDIF

    oTable:=OpenTable("SELECT * FROM " + cTable + cWhere , .T.,oOdbc)

    IF oTable:RecCount()>0

        if IsIntRef(cTable) // Verifica Integridad Referencial

           oIntRef:=IntRef(oTable) // Solo Para Modificar
           oIntRef:Run(oTable)     // Guarda todos los Valores

           IF oIntRef:IsDelete()

              oIntRef:Delete() // Borra todo lo Referenciado

           ELSEIF !oIntRef:IsTableRel()

               cClave:=oTable:FieldName(1)

               EJECUTAR("DELETEMSG",cClave,NIL,oTable:cTable,oIntRef:aList)

               oTable:End()
               oIntRef:End()

               RETURN .F.

            ENDIF

            oIntRef:End()

         ELSE

            lResp:=.T.

         ENDIF

     ENDIF

     // ? lResp,"DELETE FROM "+cTable+" "+cWhere

     IF lResp

        IF !oTable:Execute(cSql)

           MensajeErr("No es posible Eliminar "+cSql,"FUNCTION SQLDELETE()")

        ELSE

           nT1:=SECONDS()

           IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
             oDp:lTracer:=.F.
             EJECUTAR("DPTRAZASQL",cSql,cTable,oOdbc:cDsn,SECONDS()-nT1,NIL,GETPROCE(),oDp:cDpXbaseLine)
             oDp:lTracer:=lTracer
           ENDIF

        ENDIF

     ENDIF

     oTable:End()

RETURN lResp

FUNCTION TableName(cSql)
   LOCAL cTable,nAt

   cSql:=STRTRAN(cSql,"FROM"," FROM ")

   nAt:=AT(" FROM ",UPPE(cSql))

   IF nAt=0
      nAt   :=AT(" INNER ",UPPE(cSql))
      IF nAt>0
         cTable:=LEFT(cTable,nAt)
         RETURN cTable
      ENDIF
   ENDIF

   IF nAt>0
     cTable:=ALLTRIM(SUBS(cSql, nAt+5,LEN(cSql)))+" "
     nAt   :=AT(" ",cTable)
     cTable:=IIF(nAt>0 , LEFT(cTable, nAt -1 ) , cTable)
   ENDIF

RETURN cTable

/*
// Actualiza el Valor de un Campo
*/
FUNCTION SQLUPDATE(cTable,cField,cUpdate,cWhere,lView,oOdbc)
   LOCAL oTable,lRet:=.T.,cSql ,I,cTableO:=cTable
   LOCAL nT1 ,lTracer:=oDp:lTracer

   DEFAULT cWhere:=""

   oDp:lExcluye:=.F.

   IIF(ValType(oDp:oSay)="O",oDp:oSay:SetText("Actualizando Table "+cTable),NIL)

   IF Empty(cField)
     MensajeErr("Requiere parámetro cField, SQLUPDATE(cTable,cField,cUpdate,cWhere,lView,oOdbc) ")
     RETURN .F.
   ENDIF

   IF Valtype(cField)!="A"
      cField:={cField}
   ENDIF

   IF Valtype(cUpdate)!="A"
      cUpdate:={cUpdate}
   ENDIF

   DEFAULT lView:=.F.

   IF ","$cTable
      cTable:=_VECTOR(cTable,",")[1]
   ENDIF

   cWhere:=" "+IIF( Empty(cWhere), "" , cWhere)+" "

   IF !Empty(cWhere) .AND. !" WHERE "$cWhere
      cWhere:=" WHERE "+cWhere
   ENDIF

   cSql:="UPDATE "+cTableO+" SET "

   FOR I=1 TO LEN(cUpdate)
      cSql:=cSql + IIF(I>1 , "," , "" ) +;
            cField[I]+GetWhere("=",cUpdate[I])
   NEXT I

   cSql:=cSql + " "+cWhere

   oDp:cSql:=cSql

   DEFAULT oOdbc   :=GETODBC(cTable)

   nT1:=SECONDS()

   DEFAULT oDp:lUpdateMsgRun:=.F.

   IF !oDp:lUpdateMsgRun .OR. .T.

      oOdbc:Execute(oDp:cSql)

   ELSE
      MsgRun("Actualizando Tabla "+cTable,"Espere, Registros "+LSTR(COUNT(cTable)),{||oOdbc:Execute(oDp:cSql) })
      SysRefresh(.T.)
   ENDIF


   IF oDp:lTracerSql  // Monitore los Comandos Actualizar de SQL
     oDp:lTracer:=.F.
     EJECUTAR("DPTRAZASQL",oDp:cSql,cTable,oOdbc:cDsn,SECONDS()-nT1,NIL,GETPROCE(),oDp:cDpXbaseLine)
     oDp:lTracer:=lTracer
   ENDIF

   IF lView
     ? oDp:cSqL,cWhere
   ENDIF

RETURN lRet
/*
// GetWhere Construye DESDE Y HASTA
*/
FUNCTION GetWhereAnd(cField,uDesde,uHasta,lEmpty)
     LOCAL cWhere

     DEFAULT lEmpty:=.T.

     IF Empty(uDesde) .AND. Empty(uHasta) .AND. lEmpty
        RETURN " 1=1 "  // Si devuelve Vacio genera incidencia
     ENDIF

     IF uDesde=uHasta
        RETURN cField+GetWhere("=",uDesde)
     ENDIF

     IF Empty(uDesde) .AND. !Empty(uHasta) .AND. ValType(uDesde)<>"N"
        RETURN cField+GetWhere("=",uHasta)
     ENDIF

     IF !Empty(uDesde) .AND. Empty(uHasta) .AND. ValType(uHasta)<>"N"
        RETURN cField+GetWhere("=",uDesde)
     ENDIF

     cWhere:="("+cField+GetWhere(">=",uDesde)+ " AND "+;
                 cField+GetWhere("<=",uHasta)+")"

RETURN cWhere

/*
// Cierra todos los Cursores
*/
FUNCTION CLOSEALLTABLE()

   IF !Valtype(aTableOpen)="A"
      aTableOpen:={}
   ENDIF

   AEVAL(aTableOpen,{|a,n| if( ValType(a)="A" .AND. LEN(a)>0 , a[1]:End() , nil) })

   // AEVAL(aTableOpen , { |a,n| IF( len(a)>0 , a[1]:End() ,NIL ) })

RETURN .T.

/*
// Permite Excluir Registros por Tabla
*/
FUNCTION SETEXCLUYE(cTabla,cWhere,cNot)
   LOCAL nAt:=0

   DEFAULT oDp:aExcluye:={},cNot:=" NOT "

   cTabla:=alltrim(uppe(cTabla))

   nAt:=ASCAN(oDp:aExcluye,{|a,n|a[1]=cTabla})

   IF nAt=0

      AADD(oDp:aExcluye,{cTabla,cWhere,cNot})
      nAt:=LEN(oDp:aExcluye)

   ELSE

      oDp:aExcluye[nAt,1]:=cTabla
      oDp:aExcluye[nAt,2]:=cWhere
      oDp:aExcluye[nAt,3]:=cNot

   ENDIF

   // ViewArray(oDp:aExcluye)

RETURN .T.

/*
// Devuelve el Valor de Exclusión de las Tablas
FUNCTION GETEXCLUYE(cTabla)
   LOCAL cExcluye:=""
   LOCAL nAt:=ASCAN(oDp:aExcluye,{|a,n|a[1]=cTabla})

   IF nAt>0
      RETURN oDp:aExcluye[nAt,2]
   ENDIF

RETURN NIL
*/

FUNCTION BuildExcluye(cSql,cWhere,cNot)
   LOCAL aLexico:={" GROUP BY "," HAVING "," ORDER BY "},I,nAt:=0,cLimit,aData

   DEFAULT cNot:=" NOT "

   IF Empty(cWhere)
      RETURN cSql
   ENDIF

   IF " WHERE "$cSql

      IF cWhere$cSql
         RETURN cSql
      ENDIF

      cSql:=STRTRAN(cSql," WHERE "," WHERE ("+cNot+cWhere+") AND ")

   ELSE

      // ? " AQUI SE DEBE AGREGAR NOY HAY WHERE ",cSql,GETPROCE()

      FOR I := 1 TO LEN(aLexico)
         IF aLexico[I]$cSql
            nAt:=I
            EXIT
         ENDIF
      NEXT

      IF nAt>0
         cSql:=STRTRAN(cSql,aLexico[nAt]," WHERE ("+cNot+cWhere+") "+aLexico[nAt])
         // ? " CAMBIAR EN ",aLexico[nAt]
      ELSE
         cSql:=cSql+" WHERE "+cNot+cWhere
      ENDIF

   ENDIF

   IF " LIMIT "$cSql

      nAt   :=AT(" LIMIT ",cSql)
      cLimit:=ALLTRIM(SUBS(cSql,nAt,LEN(cSql)))
      aData :=_VECTOR(cLimit," ")

      IF nAt>0 .AND. !Empty(aData)
         cLimit:=aData[1]+" "+aData[2]
         cSql := STRTRAN(cSql,cLimit," ")+" "+cLimit
         // ? cSql,"final"
      ENDIF

      // ? nAt,cLimit,LEN(aData)

   ENDIF

RETURN cSql

/*
// Obtiene la Lista de Excluidos
*/
FUNCTION GetExcluye(cTabla)
   LOCAL nAt

   IF cTabla=NIL
      RETURN oDp:aExcluye
   ENDIF

   nAt:=ASCAN(oDp:aExcluye,{|a,n|a[1]=cTabla})

   IF nAt>0
      RETURN oDp:aExcluye[nAt]
   ENDIF

RETURN {"","",""}

FUNCTION SetFieldAs(cSql,aFields)
  LOCAL nAt:=0,aLexico:={},cSql1:="",cSql2:="",i

  AADD(aLexico," FROM ")
  //AADD(aLexico," WHERE ")

  FOR I := 1 TO LEN(aLexico)
     IF AT(aLexico[I],cSql)>0
        nAt:=I
     ENDIF
  NEXT I

  IF nAt>0
     I    :=AT(aLexico[nAt],cSql)
     cSql1:=LEFT(cSql,I-1)
     cSql2:=SUBS(cSql,I+0)
  ELSE
     cSql1:=cSql
  ENDIF

//  ? aFields[1,1],aFields[1,2],cSql1

  FOR I=1 TO LEN(aFields)
    IF !aFields[I,1]$cSql1
      cSql1:=STRTRAN(cSql1,aFields[I,2],aFields[I,1]+" AS "+aFields[I,2])
    ENDIF
  NEXT

  AEVAL(aFields,{|a,i|cSql2:=STRTRAN(cSql2,a[2],a[1])})

  cSql:=cSql1+cSql2

  // ? cSql1,cSql2,cSql

RETURN cSql
/*
FUNCTION ISFIELD(cTable,cField)
   LOCAL lFound:=.F.
   LOCAL oTable:=OpenTable("SELECT * FROM "+cTable,.F.)
   cField:=UPPE(ALLTRIM(cField))
   lFound:=ASCAN(oTable:aFields,{|a,n|Uppe(AllTrim(a[1]))=cField})>0
   oTable:End()
RETURN lFound
*/

// Transacciones SQL
FUNCTION DpSqlBegin(cExp,cDsn,cTable,lLock)

  DEFAULT lLock:=.T.

  CursorWait()

  // Aqui bloque la tabla para evitar que otro Usuario Escriba
  IF !Empty(cTable)  .AND. lLock

     oDp:lLockTable:=.T.
     cDsn  :=GETDSN(cTable)
     cTable:="LOCK TABLES "+cTable
     SQLEJECUTAR(cTable,cDsn) // 29/06/2013
  ELSE
     cTable:=""
  ENDIF

  // ? "cDsn,DpSqlBegin",cDsn," CON ESTE DSN SERA EJECUTADO EL BEGIN ",cExp,cDsn,cTable,lLock,GETPROCE()

  SQLEJECUTAR("BEGIN ",cDsn)

  IF !Empty(cTable)
    SQLEJECUTAR(cTable,cDsn)
    // ? "BLOQUEAR",cTable
  ENDIF

RETURN NIL

FUNCTION DpSqlCommit(cExp,cDsn)
     LOCAL cUnlock:=""

// ? "DpSqlCommit(cExp,cDsn)",cExp,cDsn

     SQLEJECUTAR("COMMIT",cDsn)

     IF oDp:lLockTable
        oDp:lLockTable:=.F.
        cUnlock:="UNLOCK TABLES"
        SQLEJECUTAR(cUnlock)
     ENDIF

RETURN NIL

FUNCTION DpSqlRollBack(cExp,cDsn)
     LOCAL cUnLock:=""

     SQLEJECUTAR("ROLLBACK",cDsn)

     IF oDp:lLockTable
       oDp:lLockTable:=.F.
       cUnlock:="UNLOCK TABLES"
       SQLEJECUTAR(cUnlock,cDsn)
     ENDIF

RETURN NIL

FUNCTION SQLEJECUTAR(cExp,cDsn)
   LOCAL oOdbc

   DEFAULT cDsn:=oDp:cDsnData,;
           oDp:nSqlBegin:=.F.

   IF oDp:lNativo
      RETURN .T.
   ENDIF

   IF "BEG"$cExp

      IF oDp:nSqlBegin
         RETURN .T.
      ENDIF

   ELSEIF !oDp:nSqlBegin

      RETURN .T.

   ENDIF

   oOdbc:=OpenOdbc(cDsn)

   IF "BEG"$cExp
      oDp:nSqlBegin:=.T.
   ELSE
      oDp:nSqlBegin:=.F.
   ENDIF

RETURN oOdbc:Execute(cExp)
//N oOdbc:Execute(cExp)

/*
// Muestra las Tablas Abiertas
*/
FUNCTION ViewOpenTable()
   LOCAL aData:={}
   AEVAL(aTableOpen,{|o,n|AADD(aData,o:cTable)})
RETURN ViewArray(aTableOpen) // ,GetProce()})

FUNCTION SqlSetWhere(cSql,cWhere)

  IF " WHERE "$cSql
     cSql:=STRTRAN(cSql," WHERE "," WHERE "+cWhere)
  ELSE
     cSql:=cSql+" WHERE "+cWhere
  ENDIF

RETURN cSql

FUNCTION ISSQLGET(cTable,cField,uValue,bRun,cWhere,oDb)

  LOCAL uResp

  oDp:aRow:={}

  uResp:=SQLGET(cTable,cField,cField+GetWhere("=",uValue)+ IIF( Empty(cWhere), "" ," AND "+cWhere),NIL,oDb)

  DEFAULT bRun:={||NIL}

  bRun:=BloqueCod(bRun)

  IF ValType(uValue)="C"
     uValue:=ALLTRIM(uValue)
     uResp :=ALLTRIM(uResp)
  ENDIF

  EVAL(bRun,uResp==uValue)

RETURN (uResp==uValue) .AND. !Empty(oDp:aRow)
/*
// Lee la Estructura de las Tablas
*/
FUNCTION OdbcStruct(cTable)
   LOCAL aStruct,nAt,oTable

   DEFAULT aTableStruct:={}

   nAt:=ASCAN(aTableStruct,{|a,n|a[1]==cTable})

   IF nAt=0
      oTable:=OpenTable("SELECT * FROM "+cTable+" WHERE 1=0 ",.F.)
      AADD(aTableStruct,{cTable,oTable:aFields})
      nAt:=Len(aTableStruct)
      oTable:End()
   ENDIF

RETURN aTableStruct[nAt,2]

FUNCTION MsgErrSql(cSql,cTitle)

 DEFAULT cTitle:="Mensaje SQL"

 cTitle:=cTitle+" Revisión["+oDp:cExeRev

 CLPCOPY(cSql)
 MemoWrit("ERROR.SQL",cSql)
 MemoWrit("ERROR.TXT",GetProce())
 MensajeErr(cSql+CRLF+"Copiado en ClipBoard",cTitle+" Error.SQL/Error.TXT")

RETURN .T.
/*
// Convierte IF en CASE/THEN/ELSE/END
*/
FUNCTION CIFTOMSQL(cSql)
RETURN   EJECUTAR("SQLIFTOCASE",cSql)
/*
LOCAL aSum:={},cSqlMs:="",I,oTable


   aSum:=_VECTOR(cSql , "SUM(")

   IF LEN(aSum)=1 .AND. "IF("$aSum[1]
     aSum:=_VECTOR(aSum[1],"IF(")
     AEVAL(aSum,{|a,n| IIF( LEFT(a,2)="F(", aSum[n]:="I"+a , NIL ) })
   ENDIF

   aSum:=IIF( Empty(aSum), {cSql} , aSum )

   FOR I=1 TO LEN(aSum)
     cSqlMs:=cSqlMs + CRLF + CTOCASEMS(aSum[I])
   NEXT I

RETURN cSqlMs

STATIC FUNCTION CTOCASEMS(cSum)
   LOCAL cExp:=cSum,cSumSql:="",lAs:=.F.,cAs,cOrg
   LOCAL I,nContar,nIni,nAbrir:=0,nCerrar:=0,nAt,nVeces:=0

   IF UPPE(LEFT(ALLTRIM(cSum),2))="UM"
      cSum:=" S"+ALLTRIM(cSum)
   ENDIF

   // Ahora Separamos solo el SUM(
   nIni:=AT( "SUM(" , UPPE(cSum) ) // Detectamos el Inicio

   IF nIni=0 .AND. !("IF("$cSum)
     RETURN cSum
   ENDIF

   cSum:=HacerCaseMs(cSum)

RETURN cSum

STATIC FUNCTION HacerCaseMs(cSql)
   LOCAL cLimit,nAt,cIf,cIf2,nPar:=0,lOn:=.T.,cChar:=""
   LOCAL aIf:={}
   LOCAL I,nAbrir:=0,nCerrar:=0,nContar,lComa,cC,lIf

   cSql  :=STRTRAN(cSql,"IF(",CRLF+"  (CASE WHEN "+CRLF)
   nAbrir:=1

   nContar:=AT("CASE WHEN ", cSql )
   lComa  :=.F.
   lIf    :=.T.

   IF nContar=0
      RETURN cSql
   ENDIF

   WHILE nContar<LEN(cSql)
      // Buscamos la Coma
      cC:=SUBS(cSql,nContar,1)

      // Primera Coma THEN
      IF cC="," .AND. !lComa .AND. lIf
          lComa:=.T.
          cSql:=LEFT(cSql,nContar-1)+CRLF+" THEN "+CRLF+SUBS(cSql,nContar+1,LEN(cSql))
          nContar++
          LOOP
      ENDIF

      // Primera Coma OTHER
      IF cC="," .AND. lComa .AND. lIf
         cSql:=LEFT(cSql,nContar-1)+CRLF+" ELSE "+CRLF+SUBS(cSql,nContar+1,LEN(cSql))
         nContar++
         LOOP
      ENDIF

      IF cC="(" .AND. lIf
         nAbrir++
      ENDIF

      IF cC=")" .AND. lIf
         nCerrar++
      ENDIF

      IF nCerrar=nAbrir .AND. lIf .AND. nCerrar>0
         lComa  :=.F.
         cSql:=LEFT(cSql,nContar-1)+CRLF+" END ) "+CRLF+SUBS(cSql,nContar+1,LEN(cSql))
         lIf:=.F.
      ENDIF

      nContar++

      IF AT("CASE WHEN ", SUBS(cSql,nContar,LEN(cSql)) )>0
         nCerrar:=0
         nAbrir :=0
         lIf    :=.T.
         lComa  :=.F.
      ENDIF

   ENDDO

RETURN cSqL
*/


FUNCTION SQLTABLE(cSql)
  LOCAL cTable

  cTable   :=ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql)))


RETURN cTable

/*
// Incrementador Caracter
*/
FUNCTION DPINCREMENTAL(cNumero,lZero,nMas)
   LOCAL nAt  :=0,cChar:="",nLen:=0,lOk:=.F.
   LOCAL aMask:={"-",".","*"},nPos:=0,nLenOrg:=0

   DEFAULT cNumero:="000-001",;
           lZero  :=.T.,;
           nMas   :=1

//? cNumero,lZero,nMas,"cNumero,lZero,nMas"

   nPos   :=ASCAN(aMask,{|a,n|nAt:=RAT(a,cNumero),nAt>0 })
   nLenOrg:=LEN(cNumero)

   IF nPos>0
     cChar  :=aMask[nPos]
     cNumero:=ALLTRIM(cNumero)
   ENDIF

   nLen:=LEN(cNumero)

   WHILE nAt<=nLen

     nAt++
     cChar:=SUBS(cNumero,nAt,1)

     IF cChar>="0" .AND. cChar<="9"
        lOk:=.T.
        EXIT
     ENDIF

   ENDDO

   IF lOk

     cChar  :=LEFT(cNumero,nAt-1)
     nLen   :=LEN(cNumero)-(nAt-1)

     IF lZero
       cNumero:=cChar+STRZERO(VAL(SUBS(cNumero,nAt,LEN(cNumero)))+nMas,nLen)
     ELSE
       cNumero:=cChar+PADR(LSTR(VAL(SUBS(cNumero,nAt,LEN(cNumero)))+nMas),nLen)
     ENDIF

   ELSE

     IF lZero .AND. (Empty(cNumero) .OR. ISDIGIT(cNumero))
       cNumero:=STRZERO(VAL(cNumero)+nMas,LEN(cNumero))
     ELSE
       cNumero:=ALLTRIM(STR(VAL(cNumero)+nMas))
     ENDIF

   ENDIF

   IF ValType(cNumero)="C" .AND. ISDIGIT(cNumero) .AND. lZero
     cNumero:=STRZERO(VAL(cNumero),LEN(cNumero))
   ENDIF

   cNumero:=PADR(cNumero,nLenOrg)

// ? cNumero,LEN(cNumero),nLenOrg,lZero,GETPROCE(),"DPINCREMENTAL"

RETURN cNumero
// EOF

FUNCTION DPMAX(lStrSql1,lStrSql2)

   IF lStrSql2=NIL
      lStrSql2:=lStrSql1
   ENDIF

   IF lStrSql1=NIL
      lStrSql1:=lStrSql2
   ENDIF

RETURN IF(lStrSql1>lStrSql2,lStrSql1,lStrSql2)

/*
// Incremental
*/

FUNCTION SQLINCREMENTAL(cTable,cField,cWhere,oDb,cMax,lZero,nLen)
   LOCAL cNumero,oTable // ,nLen 29/02/2016 DPDOCCXP

// LOCAL oOdbc:=GetOdbc(cTable)
// LOCAL oDdbc:=MyOpenDataBase(cTable)

   oDp:lMySqlNativo:=.F.

   DEFAULT cWhere:=""

   DEFAULT lZero:=ISFIELDZERO(cTable,cField)

   IF !oDp:lMySqlNativo
     DEFAULT oDb:=GetOdbc(cTable)
   ENDIF

   DEFAULT cWhere:=""

   IF !Empty(cWhere) .AND. !" WHERE "$(" "+cWhere)
      cWhere:=" WHERE "+cWhere
   ENDIF

   cWhere:=" "+cWhere

/*
JN 13/12/2013
   oDp:cSql:= "SELECT MAX("+cField+") FROM "+cTable+" "+cWhere

   cNumero:=oOdbc:QueryData( oDp:cSql) // "SELECT MAX("+cField+") FROM "+cTable+" "+cWhere )

   IF ValType(cNumero)="U"
      cNumero:=SQLGET(cTable,"MAX("+cField+")",cWhere)
   ENDIF
*/
   // JN 13/12/2013
   cNumero:=SQLGETMAX(cTable,cField,cWhere,NIL,NIL,oDb)

//? oDp:cSql,cTable,cField,cWhere,cMax,"oDp:cSql,cTable,cField,cWhere,cMax",cNumero,"Número mas alto encontrado"
// ? cTable,cField,cWhere,NIL,NIL,oDb,cMax,"cTable,cField,cWhere,NIL,NIL,oDb,cMax"
// ? "AQUI ES SQLINCREMENTAL ESTE",cNumero,CLPCOPY(oDp:cSql),oDb:ClassName(),"<-ESTE DEBE SER OBJETO",GETPROCE()

   IF ValType(cNumero)="C"

//      nLen  :=LEN(SQLGET(cTable,cField)) AQui lee toda la tabla, por esto era muy lento
      DEFAULT nLen  :=SqlFieldLen(cTable,cField)

      IF lZero .AND. ISALLDIGIT(ALLTRIM(cNumero))
         cNumero:=STRZERO(VAL(cNumero),nLen)
      ENDIF

      IF !Empty(cMax)
        cNumero:=DPMAX(DPINCREMENTAL(cNumero,lZero),cMax)
      ELSE
        cNumero:=DPINCREMENTAL(cNumero,lZero)
      ENDIF

      IF lZero .AND. ISALLDIGIT(ALLTRIM(cNumero))
         cNumero:=STRZERO(VAL(cNumero),nLen)
      ENDIF

//? cNumero,"Incrementado, ahora va verificar que no existe en SQLINCREMENTALBIN"


      cNumero:=EJECUTAR("SQLINCREMENTALBIN",cTable,cField,cWhere,oDb,cNumero)

//      WHILE ISSQLFIND(cTable,cWhere)
//        cNumero:=DPINCREMENTAL(cNumero)
//      ENDDO

      RETURN cNumero

      // cNumero:=STRZERO(VAL(cNumero)+1,LEN(cNumero))
   ENDIF

   IF ValType(cNumero)="N"
      cNumero++
   ENDIF

// cMax:=IF(Empty(cMax),cNumero,cMax)
// oDp:lSqlNativo:=.F.

RETURN DPMAX(cNumero,cMax)

FUNCTION ISFIELDZERO(cTable,cField)

  LOCAL nAt

  DEFAULT cTable:="DPPROVEEDOR",;
          cField:="PRO_CODIGO"

  IF oDp:aZero=NIL

    oDp:aZero:=ASQL("SELECT CAM_TABLE,CAM_NAME FROM DPCAMPOS WHERE CAM_ZERO"+GetWhere("=",.T.))

    AEVAL(oDp:aZero,{|a,n| oDp:aZero[n,1]:=ALLTRIM(oDp:aZero[n,1]),;
                           oDp:aZero[n,2]:=ALLTRIM(oDp:aZero[n,2]) })

  ENDIF

  cTable:=ALLTRIM(cTable)
  cField:=ALLTRIM(cField)

  nAt:=ASCAN(oDp:aZero,{|a,n| a[1]=cTable .AND. a[2]=cField })

RETURN nAt>0

FUNCTION CTABLE(cSql)
  LOCAL aData,cTable

// ? "aquie es ctable32",cSql

  IF !(" FROM"$UPPE(cSql))
     RETURN ""
  ENDIF

  cSql :=ALLTRIM(cSql)+" "
  cSql :=STRTRAN(cSql,CHR(10)," ")
  cSql :=STRTRAN(cSql,CHR(13)," ")

  aData:=_VECTOR(UPPER(cSql)," FROM ")

  IF LEN(aData)>1

    cTable:=aData[2]
    cTable:=ALLTRIM(STRTRAN(cTable,"FROM",""))

    aData :=_VECTOR(cTable," ")

    IF !Empty(aData)
      cTable:=ALLTRIM(aData[1])
    ENDIF

  ELSEIF LEN(aData)=1
? aData[1],"CTABLE32"
  ENDIF

  cTable:=STRTRAN(cTable,"(","")

RETURN cTable
// RETURN EVAL(GenBlock("cTable32(oPar1)",cSql))  // Function Ubicada en

// EJECUTAR("CTABLE",cSql)
/*
   LOCAL cTable:="",nAt:=0,nAdd:=5

   nAt:=AT(" FROM ",UPPE(cSql))

   IF nAt=0
     nAt :=AT(" TABLE ",UPPE(cSql))
     nAdd:=6
   ENDIF

   cTable :=ALLTRIM(SUBS(cSql,nAt+nAdd,LEN(cSql)))
   cTable :=ALLTRIM(IIF(" "$cTable ,LEFT(cTable,AT(" ",cTable)),cTable))
   cTable :=ALLTRIM(cTable)

RETURN cTable
*/
FUNCTION GETLIMITREC(cSql)

  LOCAL nAt,cTable:=CTABLE(cSql)

  DEFAULT aLimit:={}

  cTable:=ALLTRIM(UPPER(cTable))
  nAt   :=ASCAN(aLimit,{|a,n|a[1]=cTable})

  IF nAt>0
     RETURN aLimit[nAt,2]
  ENDIF

RETURN 0

FUNCTION SETLIMITREC(cTable,nLimit)
   LOCAL nAt:=0

   DEFAULT aLimit:={}

   cTable:=ALLTRIM(UPPER(cTable))
   nAt   :=ASCAN(aLimit,{|a,n|a[1]=cTable})

   IF nAt>0
      aLimit[nAt,2]:=nLimit
   ELSE
      AADD(aLimit,{cTable,nLimit})
   ENDIF

   oDp:aLimit:=aLimit

RETURN .T.

FUNCTION TABLELOWER(oDb,cSql)

  DEFAULT oDb:=OpenOdbc(oDp:cDsnData),;
          cSql:=" SELECT INV_GRUPO,GRU_DESCRI,COUNT(*) AS CUANTOS "+;
                "  FROM DPINV "+;
                "  LEFT JOIN DPGRU ON INV_GRUPO=GRU_CODIGO "+;
                "  WHERE INV_ESTADO='A' AND INV_METCOS='S' "+;
                "  GROUP BY INV_GRUPO "

  AEVAL(oDb:aTables,{|a,n| cSql:=STRTRAN(cSql,UPPER(a),LOWER(a))})

RETURN cSql
// EOF


/*
STATIC FUNCTION xData()

   PARAM aData,bBlq

   AEVAL(aData,bBlq)

RETURN NIL
*/
// EOF

// Asigna oDb por Defecto, sera Asumido por DPLBXRUN
FUNCTION SETDBSERVER(oDb)
   oDp:oDbServer:=oDb
RETURN NIL

FUNCTION GETDBSERVER(cTable)
RETURN oDp:oDbServer
