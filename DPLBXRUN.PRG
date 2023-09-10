//                               ?n
// Definición y Ejecución de Listas DpListBox
// JN 03/01/2003
//
#INCLUDE "FIVEWIN.CH"
#INCLUDE "XBROWSE.CH"
#INCLUDE "\DWH\LIBS\TSBUTTON.CH"
#INCLUDE "C5MENU.CH"


STATIC aLines :={}
STATIC aDpLbx :={}
STATIC nNumLbx:=0,nWidth,nHeight,aMemos

MEMVAR oDp,oBrw,oWnd,oCursor,oDpLbx,nOption

FUNCTION DpLbx(cFileLbx,cTitle,cWhere,lPage,cField,uValue,aParam,lDialog,oDb,oGet,oWndMain,bInit)
   LOCAL oDpLbx

   DEFAULT lPage  :=.F.,;
           lDialog:=oDp:lSetDialog  // Generado por SETDIALOG

   oDp:lSetDialog:=.F.

   oDp:lLbxRun:=.T.

   // MsgDemo()

   // Obtiene datos del Servidor Remoto
   IF !oDp:oDbServer=NIL .AND. oDb=NIL
      oDb:=oDp:oDbServer
      SETDBSERVER(NIL)
   ENDIF

   CursorWait()

   cFileLbx+=IIF( !"."$cFileLbx,".lbx" , "")

   oDp:aRowSql:={} // Lista de Campos Seleccionados

   IF Empty(uValue) .AND. ValType(oGet)="O"
      uValue:=oGet:GetText()
   ENDIF

   Macroeje("ISPCPRG()")

   oDpLbx:=TDpLbx():New(cFileLbx,cTitle,cWhere,lPage,cField,uValue,aParam,lDialog,oDb,oGet,oWndMain,bInit)

   oDp:oLbx:=oDpLbx

   IF !oDpLbx:lSqlOk

      oDpLbx:End()

   ELSE

     oDpLbx:Activate()

     EJECUTAR("LBXINIRUN",oDpLbx) // Donde se Inician Todos los LBX

     // DpFocus(oDpLbx:oWnd)
   ENDIF

   // CLEA TYPE

   oDp:oLbx:=oDpLbx

RETURN oDpLbx

/*
 * TDpLbx
 * Pasa los valores del cursor de la clase TDBODBCD, hacia arreglos
 * facilitando su manejo de manera similar a RDD
 * Juan Navas 19/04/2003
 */

CLASS TDpLbx FROM TDpClass

// DATA  aVars       // Control de los Campos
   DATA  aBtnRun     // Ejecuciones de los Botones
   DATA  aBtnWhen    // Condicione de los Botones
   DATA  aBtns       // Lista de Botones
   DATA  aZero       // Nombre de los Campos
   DATA  aFind       // Lista de Búsquedas por Campo, Almacena las Ultimas Búsquedas
   DATA  aCargo      // Almacena Cualquier Valor
//   DATA  aData       // Contiene los Datos del Cursor
   DATA  aFields     // Campos de la Columna
   DATA  aLineEmpty  // Línea Vacia
   DATA  aParam      // Valores para los parametros ::uData1,::uData2
   DATA  aColColor   INIT {} // Colores para buscar
   DATA  aColEdit    INIT {} // {cField,nCol} Columnas Editables

   DATA  nNumLbx     // Identificador del DpLbx
   DATA  nStyle      // Stilo del Browse
   DATA  nKey        // Tecla Presionada
   DATA  nBtnDblClic INIT 0

   DATA nWidth
   DATA nHeight

   DATA nClrPane1 INIT oDp:nClrPane1
   DATA nClrPane2 INIT oDp:nClrPane2

   DATA nColSel INIT 0

   DATA  nItemLen    INIT 100
   DATA  nItemLen1   INIT 100
   DATA  nItemLen2   INIT 0

   DATA  cFileLbx    // Nombre del Campo
   DATA  cTable      // Nombre de la Tabla
   DATA  cSql        // Comando que Crea el Cursor
   DATA  cOrderBy    // Está Representado
   DATA  cPrimary    // Clave Primaria
   DATA  cOrder      // Ordenado por
   DATA  cPreDelete
   DATA  cPostDelete
   DATA  cTopic      // Nombre de la Tabla
   DATA  cFileChm    // Nombre del Archivo de Ayuda
   DATA  uFind       // Valor que Está Buscando
   DATA  cFieldFound
   DATA  cIniSql     // Ejecución Inicial SQL
   DATA  cCargo      // Almacena Cualquier Valor
   DATA  cScope      // Condicion de la Tabla
   DATA  cTitle      // Titulo
   DATA  cWhere      // Clausula Where
   DATA  cFieldFind  // Campo de Búsqueda
   DATA  cFileRtf    INIT "" // Archivo de Ayuda RTF
   DATA  cFileRtfOld INIT oDp:cHelpRtf
   DATA  cIniFilter  INIT "" // Inicia como buscador
   DATA  cMessage    INIT ""
   DATA  cFileSql    INIT ""
   DATA  cPrgIni     INIT ""

   DATA  cFilter_Log  INIT ""
   DATA  cFilter_text INIT "Activo"


   DATA  cItem    INIT "" // Cantidad de Registros
   DATA  cItem1   INIT ""
   DATA  cItem2   INIT ""

   DATA  uValueFind  // Valor de Búsqueda
   DATA  uValue1,uValue2,uData1,uData2
   DATA  cSqlFilter // Necesario para rehacer la búsqueda


    DATA nClrText  INIT oDp:nGris // CLR_BLUE
    DATA nClrText1 INIT 0
    DATA nClrText2 INIT 0
    DATA nClrText3 INIT 0
    DATA nClrText4 INIT 0
    DATA nClrText5 INIT 0

    DATA nFrezer   INIT 0


    DATA cClrText  INIT ""
    DATA cClrText1 INIT ""
    DATA cClrText2 INIT ""
    DATA cClrText3 INIT ""
    DATA cClrText4 INIT ""
    DATA cClrText5 INIT ""

    // 09/09/2023 SubConsultas
    DATA cSub01 INIT ""
    DATA cSub02 INIT ""
    DATA cSub03 INIT ""
    DATA cSub04 INIT ""
    DATA cSub05 INIT ""
    DATA cSub06 INIT ""
    DATA cSub07 INIT ""
    DATA cSub08 INIT ""
    DATA cSub09 INIT ""
    DATA cSub10 INIT ""
    DATA cSub11 INIT ""
    DATA cSub12 INIT ""
    DATA cSub13 INIT ""
    DATA cSub14 INIT ""
    DATA cSub15 INIT ""
    DATA cSub16 INIT ""
    DATA cSub17 INIT ""
    DATA cSub18 INIT ""
    DATA cSub19 INIT ""
    DATA cSub20 INIT ""

    // DATA nClrPane1 INIT oDp:nGris
    // DATA nClrPane2 INIT oDp:nGris2

    DATA nClrPane3 INIT 0


/*
   DATA cSelect,;
        cInner,;
        cWhere,;
        cWhere_,;
        cGroup,;
        cHaving,;
        cOrderBy
*/

   // DATA  cIniGet  // Get de Búsqueda

   DATA  oCursor     // Nombre del Cursor
   DATA  oBrw        // Browse editando la Tabla
   DATA  oBar
   DATA  oWnd        // Ventana Mdi
   DATA  oMsgBar     // Mensajes
   DATA  oFocusOld   // Foco Anterior
   DATA  oDb
   DATA  oGet
   DATA  oWndMain
   DATA  oLbx
   DATA  oFilterLog
//INIT oLbx:=SELF


   DATA  oItem,oItem1,oItem2

   DATA  bLDblClick,;  // Accion que debe Ejecutar el Browse
         bRClicked ,;
         bStart    ,;
         bFor          // Localizador de Registro

   DATA  bValid INIT {||.T.}


   DATA  lAutomatic  // Modo de Asignación de Valores
   DATA  lFind
//   DATA  lSetFilter  INIT .F.
   DATA  lPage       // Si trabaja por Paginado Según Lineas del Browse
   DATA  lDeleteRef  // Indica si Borra Data Referencial
   DATA  lDialog
   DATA  lStart
   DATA  lExport     // Expotar
// DATA  lChkSql     // Chequear SQL
   DATA  lSqlOk      // Si ok; Sigue
   DATA  lTablaAcc INIT .T.  // Permisos de Acceso dede la Tabla
   DATA  lGoBottom INIT .F.
   DATA  lFullSize INIT .F.
   DATA  lFullHight INIT .F.
   DATA  lIniFilter INIT .F.
   DATA  lFullWidht INIT .F.

   DATA  lDesc      INIT .F. // lDesc en la Clausula ORDER BY (CAMPOS DESC) genera Incidencai


   DATA  lResized   INIT .F.

   DATA lFilterLog INIT .F.

   DATA  uData1,uData2,uData3

/* DATA  lIncluye    // Según Mapa
   DATA  lConsulta
   DATA  lModifica
   DATA  lElimina
   DATA  lImprime */

   METHOD New( cFileLbx, cTitle , cWhere , lPage , uValue)
   METHOD SetBar(oWnd,oBar,oBrw)

   METHOD Set(cName,xValue) INLINE  (__objAddData( Self, cName),__objSendMsg(Self,cName,xValue))

   METHOD IsDef( cName )

   METHOD Gotop() INLINE (::oBrw:GoTop(),::oBrw:Refresh(.T.))


   METHOD RunAction(nButton,nOption,lRun)
   METHOD RunKey(nKey)
   METHOD GetValue(cField,oGet)
   METHOD GotFocus() INLINE (oDp:oLbx:=SELF,oDp:cHelpRtf:=::cFileRtf,::oBrw:GotFocus())

   METHOD Query()    INLINE EJECUTAR("BRWMAKERRUN",::cTable)
   METHOD Find()
   METHOD SetFilter() INLINE  EJECUTAR("LBXSETFILTER",SELF)
// ::Find(.F.)

   METHOD Reporte(cCodRep)
   METHOD Locate(bFor,lNext,nRecord)
// METHOD RunKey()
   METHOD FindMas()
   METHOD GetRowSql()
   METHOD SetOrder(nCol)

   METHOD Delete(cMsg,lAsk,nOrder,cClave,cField,lDirect,cPosDel)
   METHOD Close() // HIDDEN
   // METHOD   DbSeek(oCol)
   METHOD DbSeek(oCol,uValue,nLastKey)
   METHOD DbSeek2 (oCol,uValue,nLastKey)
   METHOD ClassName() INLINE "TDPLBX"
   METHOD Activate()
   METHOD ChkData()
   METHOD Preview() INLINE EJECUTAR("BRWPREVIEW",::oBrw,NIL)
   METHOD Print()
   METHOD CTOHTML(cFile) INLINE ::oCursor:CTOHTML(cFile)
   METHOD SetMsg(cMsg)  INLINE (::oWnd:oMsgBar:SetMsg(cMsg),::oWnd:oMsgBar:Refresh())
   METHOD LbxChange()


// EJECUTAR("BRWPREVIEW",::oBrw,::cTitle)
   METHOD Reload()
   METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)
   METHOD OpenTable()
   METHOD CancelEdit()
   METHOD CancelFind()

   METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                             WndHandleEvent( Self, nMsg, nWParam, nLParam )


   ERROR HANDLER OnError( cMsg,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )


//   METHOD SetMsg(cMsg)  INLINE (::oMsgBar:SetMsg(cMsg),::oMsgBar:Refresh())
   METHOD Refresh()

//   METHOD Add( cName, xValue )
//   METHOD Get( cName )
//   METHOD Set( cName, xValue )
//   METHOD GetPos( cName )
//   METHOD IsDef( cName )

   METHOD End()

ENDCLASS

/*
 *  TDpLbx:New()
 */

METHOD New( cFileLbx , cTitle , cParWhere , lPage , cField , uValue , aParam,lDialog,oDb,oGet,oWndMain,bInit) CLASS TDpLbx

  LOCAL cTable,cSql,cHeader,oIco,oLbx,oOdbc,cPict,cMemo,aDelCol:={},aLine:={},aHeader:={}
  LOCAL cSelect,cWhere,cView,cInner,cData,cIniSql,cGroup:=""
  LOCAL cOrderBy,cHaving
  LOCAL cGroupBy,nAling,cIniGet,cWhereGet,cLimit:=""
  LOCAL cInner2:="",cInner3:="",cInner4:="",cInner5:=""
  LOCAL nClrCol,cFont,cClrGra

  LOCAL I,nFor,nAt,nWidthC,nBtnRun
  LOCAL lDelRef:=.F.,lChkSql:=.F.,lTablaAcc

  LOCAL oFont
  LOCAL oCol,oBrw,oDpLbx:=SELF,oWnd
  // LOCAL bStart:={||.T.} // Ejecución Inicial

  LOCAL nClrText   :=0
  LOCAL nClrPane1  :=0
  LOCAL nClrPane2  :=0
  LOCAL nStyle     :=1
  LOCAL nFreeze    :=0
  LOCAL lHScroll   :=.T.
  LOCAL lFullSize  :=.T.
  LOCAL lFullHight :=.F.
  LOCAL lFullWidht :=.F.
  LOCAL lTotal     :=.F.
  LOCAL lHayTotales:=.F.
  LOCAL aTotales   :={}
  LOCAL lEditar    :={}

//LOCAL lDialog:=.F. // !oDp:lMenuXp

  oDp:bLbxRun:={||NIL} // Utilizado por Scrollget para Asignar el Valor a la Campo ::PutData

  nNumLbx++

  DEFAULT lPage   :=.F.,;
          aMemos  :={},;
          lDialog :=.F.,;
          oWndMain:=DpWndMain()

  oDp:oLbx    :=SELF

  ::oWndMain  :=DpWndMain()  //  oWndMain
  ::aParam    :=aParam

//? lDialog,"lDialog, que es dplbxrun"

  IF Valtype(::aParam)="A" .AND. !Empty(::aParam)
     ::uData1:=::aParam[1]
     ::uData2:=IIF(Len(::aParam)>1,::aParam[2],::uData2)
     ::uData3:=IIF(Len(::aParam)>2,::aParam[3],::uData3)
     ::aParam:=aClone(aParam)
  ELSE
     ::aParam    :=aParam
  ENDIF

  ::oFocusOld :=oDp:oFocus
  ::lAutomatic:=.T.      // Campo no declarado lo asume por defecto
  ::bStart    :={||.F.}
  ::aVars     :={}
  ::nPos      :=0
  ::nKey      :=0
  ::cName     :=""
  ::nNumLbx   :=nNumLbx
  ::aBts      :={}      // Lista de las acciones que se ejecutará
  ::aBtnRun   :={}
  ::aBtnWhen  :={}
  ::lDialog   :=lDialog
  ::lStart    :=.T.
  ::lExport   :=.F.
  ::lSqlOk    :=.T.
  // Campo de Búsqueda
  ::cFieldFind:=cField
  ::uFieldFind:=uValue

  ::bLDblClick:={||nil}  // Accion que debe Ejecutar el Browse
  ::bRClicked :={||nil}
  ::lFind     :=.F.
  ::oDb       :=oDb
  ::oGet      :=oGet
  ::oLbx      :=SELF

  ::bValid   :={|| EJECUTAR("LBXSAVEPAR", oDpLbx)}

  PUBLICO("oDpLbx",SELF)

  DEFAULT bInit:={||NIL}

  EVAL(bInit)  // Utilizado para Declarar Variables

  // ::lDeleteRef:=.F.      // No Puede Borrar Referencial

  AADD(aDpLbx,Self) // Permite determinar el número del Objeto

  aLines      :="" // Contiene las Lineas de las Definiciones del Memo
  cFileLbx    :=IIF("\"$cFileLbx,cFileLbx,oDp:cPathLbx+cFileLbx) //  oDp:cLbxPath
  ::cFileLbx  :=cFileLbx
  ::cFileSql  :="temp\"+ALLTRIM(cFileNoExt(cFileNoPath(cFileLbx)))+".SQL"

// ? ::cFileSql,"cFileSql"

  IF !FILE(cFileLbx) .AND. !Empty(oDp:cFileSource) .AND. FILE(oDp:cFileSource+::cFileLbx )
    COPY FILE (oDp:cFileSource+::cFileLbx) TO (cFileLbx)
    EJECUTAR("DPSCRIMPORT",cFileLbx)
  ENDIF
  // I           :=ASCAN(aMemos,{|a,n|a[1]==cFileLbx}) // Busca Memos
  I           :=0

  IF I>0 .AND. !Empty(aMemos[I,2])
     IniGetLbx(aMemos[I,2])
  ELSE

    // ? cFileLbx
    // Utilizando DPLBXRUN  06/08/2013
    // IniGetLbx(GETLBXRUN(NIL,cFileLbx))

    IF !ISDPSTD() .AND. !FILE(cFileLbx)
      EJECUTAR("DPDIRAPLDOWN",cFileNoPath(cFileLbx),cFilePath(cFileLbx),.F.,.T.)
    ENDIF

    IF !ISDPSTD() .AND. !FILE(cFileLbx)
     EJECUTAR("DPREGSOPORTEAUTO","EDT",cFileLbx,"Archivo "+cFileNoPath(cFileNoExt(cFileLbx))+" no Existe")
    ENDIF

       IF !FILE(cFileLbx) .AND. !ISDPSTD()
       MensajeErr("Archivo "+cFileLbx+" no Existe")
       cFileLbx:="FORMS\LBXNOEXISTE.LBX"
    ENDIF




    IF FILE(cFileLbx) .OR. ISDPSTD()

       IF ISDPSTD()
          IniGetLbx(GETFILESTD(cFileLbx))
       ELSE
          IniGetLbx(MEMOREAD(cFileLbx))
       ENDIF

    ELSE // Busca en la Base de datos, en caso de Estar Creada

       IF !BuildLbx(cFileLbx)
         MsgAlert("Archivo "+cFileLbx+" no Existe")
         cFileLbx:="FORMS\LBXNOEXISTE.LBX"
       ENDIF

       IniGetLbx(MEMOREAD(cFileLbx))

    ENDIF

    AADD(aMemos,{cFileLbx,aLines})  // Solo Graba el Memo

  ENDIF

  IF Empty(oDp:cModeVideo)

    DEFINE FONT oFont NAME "Tahoma" SIZE 0,-10 // -14

  ELSE

    DEFINE FONT oFont NAME "Tahoma" SIZE 0,-12 // -14

  ENDIF

  DEFAULT cTitle  :=GetLbx("TITLE"   ) // lo Obtiene de la Lista

  ::cTopic   :=GetLbx("TOPIC"  )
  ::cFileChm :=GetLbx("FILECHM")
  ::cTitle   :=cTitle

  cTable       :=GetLbx("TABLE"  )

  /*
  // Caso de Tabla oDp:cDpProgra,
  */
  IF LEFT(cTable,1)="&"
     cTable:=SUBS(cTable,2,LEN(cTable))
     cTable:=MACROEJE(cTable)
  ENDIF

// ? cTable,"cTable"

  IF Empty(cTable)
     MensajeErr("Llamada DPLBX "+cFileLbx)
     RETURN NIL
  ENDIF

  IF ValType(cTable)="C" .AND. ("&"$cTable)
     cTable:=STRTRAN(cTable,"&","")
     cTable:=MACROEJE(cTable)
  ENDIF

  // 09/09/2023
  ::cSub01    :=GetLbx("SUB01",NIL,"C")
  ::cSub02    :=GetLbx("SUB02",NIL,"C")
  ::cSub03    :=GetLbx("SUB03",NIL,"C")
  ::cSub04    :=GetLbx("SUB04",NIL,"C")
  ::cSub05    :=GetLbx("SUB05",NIL,"C")
  ::cSub06    :=GetLbx("SUB06",NIL,"C")
  ::cSub07    :=GetLbx("SUB07",NIL,"C")
  ::cSub08    :=GetLbx("SUB08",NIL,"C")
  ::cSub09    :=GetLbx("SUB09",NIL,"C")
  ::cSub10    :=GetLbx("SUB10",NIL,"C")


  cSelect      :=GetLbx("SELECT" )
  cInner       :=GetLbx("INNER"  )
  cInner2      :=GetLbx("INNER2",NIL,"C")
  cInner3      :=GetLbx("INNER3",NIL,"C")
  cInner4      :=GetLbx("INNER4",NIL,"C")
  cInner5      :=GetLbx("INNER5",NIL,"C")

  DEFAULT cInner :="",;
          cInner2:="",;
          cInner3:="",;
          cInner4:="",;
          cInner5:=""


  cWhere       :=GetLbx("WHERE"  )
  aDelCol      :=GetLbx("DELETECOL")
  cIniGet      :=GetLbx("INIGET"  )

  ::cIniFilter :=GetLbx("INIFILTER")   // Inicio de Filtro para evitar leer todos los registros
  ::lIniFilter :=CTOO(::cIniFilter,"L")


  // Verifica si la tabla tiene Buscador Inicial
  oDp:lLbxIniFind:=CTOO(oDp:lLbxIniFind,"L")

  IF oDp:lLbxIniFind  .AND. oDp:IsDef("l"+cTable) .AND. oDp:Get("l"+cTable)
     // Activa el Buscador Inical en el LBX
     ::cIniFilter:=.T.
  ENDIF

  // cTable       :=GetLbx("TABLE"  )
  lTablaAcc    :=GetLbx("ISTABLAACC")  // Indica si la tabla obtiene permiso desde las funcones ISTABINC(),ISTABMOD()
  ::nbtnDblClic:=GetLbx("BTNDBLCLIC") // Boton que Ejecuta el Doble Clic
  ::nbtnDblClic:=CTOO(::nbtnDblClic,"N")
  ::cFileRtf   :=GetLbx("FILERTF")

  oDp:cHelpRtf:=::cFileRtf

  IF !Empty(lTablaAcc)
    ::lTablaAcc:=(UPPE(lTablaAcc)=".T.")
  ENDIF

 IF !Empty(cIniGet)

    oDp:oGetLbx:=oDp:oControl

    cWhereGet:=Eval(BloqueCod(cIniGet))

    IF cWhereGet!=NIL .AND. Empty(cWhereGet)
      ::lSqlOk:=.F.
      RETURN SELF
    ENDIF

  ENDIF


  IF !Empty(aDelCol)
    aDelCol:=_VECTOR(aDelCol)
  ELSE
    aDelCol:={}
  ENDIF

  IF !Empty(cWhere) .AND. Left(cWhere,1)='"'
     cWhere:=MacroEje(cWhere)
  ENDIF

  IF !Empty(cInner) .AND. Left(cInner,1)='"'
     cInner:=MacroEje(cInner)
  ENDIF

  IF !Empty(cInner2) .AND. Left(cInner2,1)='"'
     cInner2:=MacroEje(cInner2)
  ENDIF

  IF !Empty(cInner3) .AND. Left(cInner3,1)='"'
     cInner3:=MacroEje(cInner3)
  ENDIF

  IF !Empty(cInner4) .AND. Left(cInner4,1)='"'
     cInner4:=MacroEje(cInner4)
  ENDIF

  IF !Empty(cInner5) .AND. Left(cInner5,1)='"'
     cInner5:=MacroEje(cInner5)
  ENDIF

 //? cInner,cInner2,cInner3,cInner4,cInner5,ValType(cInner),ValType(cInner2),ValType(cInner3),ValType(cInner4),ValType(cInner5)
 // ? cInner

  cInner       :=cInner+" "+cInner2+" "+cInner3+" "+cInner4+" "+cInner5

  cGroup    :=GetLbx("GROUP BY")

  cIniSql   :=GetLbx("INISQL"  )     // Ejecuta al Iniciar SQL

  cOrderBy  :=GetLbx("ORDER BY"   ,.T.) // si esta NIL, devuelve ""
  cGroupBy  :=GetLbx("GROUP BY"   ,.T.)
  cHaving   :=GetLbx("HAVING"     ,.T.)
  ::cPrimary:=GetLbx("PRIMARYKEY" ,.T.) // Indicar Clave Primaria

  // 29/06/2017
  IF Empty(::cPrimary)
     ::cPrimary:= cOrderBy
  ENDIF

  // Quitarlo de la Sentencia SQL
  IF " DESC"$::cPrimary
     ::cPrimary:=ALLTRIM(STRTRAN(::cPrimary," DESC",""))
  ENDIF

  nWidth   :=GetLbx("WIDTH"      ,NIL,"N")
  nHeight  :=GetLbx("HEIGHT"     ,NIL,"N")
  nStyle   :=GetLbx("STYLE"      ,NIL,"N")
  nClrText :=GetLbx("COLORTEXT"  ,NIL,"N")
  nClrPane1:=GetLbx("COLORPANE1" ,NIL,"N")
  nClrPane2:=GetLbx("COLORPANE2" ,NIL,"N")
  nFreeze  :=GetLbx("FREEZE"     ,NIL,"N")
  lDelRef  :=GetLbx("DELETEREF"  ,NIL,"L",,,.F.)
  lChkSql  :=GetLbx("LCHKSQL"    ,NIL) // ,"L",,,.T.) // Solo se excluye si esta declarado

  ::cPrgIni  :=GetLbx("PRGINI"     ,NIL,"C",,,"") // 02/08/2023 programa de Inicio

  ::nClrPane1:=nClrPane1
  ::nClrPane2:=nClrPane2

//? lChkSql,"aqui es lChkSql"

  IF Empty(lChkSql)
     lChkSql:=.T.
  ELSE
     lChkSql  :=GetLbx("LCHKSQL"    ,NIL,"L")
  ENDIF

//? lChkSql,ValType(lChkSql),"aqui es lChkSql"

  ::nHeaderLines:=GetLbx("HEADERLINES"   ,NIL,"N")

  IF Empty(::nHeaderLines)
     ::nHeaderLines:=NIL
  ENDIF

  lHScroll  :=GetLbx("LHSCROLL"   ,.T.,"L")
  lFullSize :=GetLbx("LFULLSIZE"  ,.F.,"L")  // Abarca todo el Video Disponible
  lFullHight:=GetLbx("LFULLHIGHT" ,.F.,"L")  // Abarca todo el Alto Disponible
  lFullWidht:=GetLbx("LFULLWIDHT" ,.F.,"L")  // Abarca todo el Ancho

  // 03/05/2023, Ficheros empresas no debe excluirlas

  IF !Empty(GetLbx("LEXCLUYE"))
    oDp:lExcluye  :=GetLbx("LEXCLUYE" ,oDp:lExcluye,"L")  // 03/05/2023
  ENDIF

  ::cFilter_Log :=GetLbx("FILTER_LOG"  ,NIL,"C")  // Filtro para campos Logicos, removerlos, su valor se guarda en el DATASEY
  ::cFilter_Text:=GetLbx("FILTER_TEXT" ,NIL,"C")  // Filtro para campos Logicos, removerlos, su valor se guarda en el DATASEY

  IF Empty(::cFilter_Text)
     ::cFilter_Text:="Activos"
  ENDIF

  ::cMessage :=GetLbx("MESSAGE" ,NIL,"C")  // Mensaje en la parte inferior del LBX

  IF !Empty(::cMessage) .AND. "&"=LEFT(::cMessage,1)
     ::cMessage:=SUBS(::cMessage,2,LEN(::cMessage))
     ::cMessage:=MacroEje(::cMessage)
  ENDIF

  ::nItemLen1:=GetLbx("NITEMLEN1" ,NIL,"N")
  ::nItemLen2:=GetLbx("NITEMLEN2" ,NIL,"N")

  ::cItem1   :=GetLbx("ITEM1" ,NIL,"C")
  ::cItem2   :=GetLbx("ITEM2" ,NIL,"C")

  IF !Empty(::cItem1) .AND. "&"=LEFT(::cItem1,1)
      ::cItem1:=SUBS(::cItem1,2,LEN(::cItem1))
      ::cItem1:=MacroEje(::cItem1)
  ENDIF

  IF !Empty(::cItem2) .AND. "&"=LEFT(::cItem2,1)
      ::cItem2:=SUBS(::cItem2,2,LEN(::cItem2))
      ::cItem2:=MacroEje(::cItem2)
  ENDIF

  ::lGoBottom:=GetLbx("GOBOTTOM"   ,NIL,"L",,,.F.)

  cSelect  :=IIF( Empty(cSelect ),"*" ,cSelect   )
  // Aqui anida los where

  IF !Empty(cWhere) .AND. !Empty(cParWhere)

     IF " JOIN "$UPPER(cParWhere)
       cWhere:=cParWhere+" AND "+cWhere
     ELSE
       cWhere:=cWhere+ " AND "+cParWhere
     ENDIF

     cParWhere:=""

// ? "AQUI ES WHERE 1",cWhere

  ENDIF

  IF Empty(cWhere) .AND. !Empty(cParWhere)
     cWhere:=cParWhere
  ENDIF

  // ? cParWhere,"cParWhere"

  IF !Empty(cParWhere) .AND. !Empty(cWhere)
    //? cWhere,cParWhere
    cWhere   :=ADDWHERE(cParWhere,cWhere)
    cParWhere:=""  // 24/05/2018

// ? "AQUI ES WHERE2",cWhere
// ? "cWhere,",cWhere
  ELSE

    cWhere   :=IIF( Empty(cWhere  ), IIF( Empty(cParWhere),"" , cParWhere ), cWhere )

// ? "AQUI ES WHERE 3",cWhere

  ENDIF

//? cParWhere,"cParWhere",cWhere,"cWhere"
//? cWhere,"WHERE 4"
//? cWhereGet,"cWhereGet",cWhere,"cWhere","AQUI WHERE 5"

  IF !Empty(cWhereGet) .AND. !Empty(cWhere)
     //  cWhere:=ADDWHERE("("+cWhereGet+")","("+cWhere+")") 24/05/2016

     IF "JOIN"$cWhereGet
         cWhere:=ADDWHERE(cWhereGet,cWhere)
     ELSE
         cWhere:=ADDWHERE(cWhere   ,cWhereGet)
     ENDIF

// ? "ASI QUEDO",cWhere

  ELSEIF !Empty(::cScope)
     cWhere:=cWhereGet
  ENDIF

  IF Empty(cWhere) .AND. !Empty(cWhereGet)
     cWhere:=cWhereGet
  ENDIF

// ? cWhere,"cWhere 5"

  IF EMPTY(cInner)
     cInner:=""
  ELSE
     cInner:=" "+cInner+" "
  ENDIF

  IF !" WHERE "$UPPE(cWhere) .AND. !EMPTY(cWhere)
     cWhere:= " WHERE "+cWhere+" "
  ENDIF

  cOrderBy:=IIF(!Empty(cOrderBy)," ORDER BY ","")+cOrderBy

  IF !Empty(cGroup)
     cGroup:=" "+cGroup+" "
     cGroup:=IIF( " GROUP BY "$cGroup , "" , " GROUP BY " )+cGroup
  ENDIF

  ::cWhereLog :=""
  ::lFilterLog:=.F. //

// cFilter_Text
  IF !Empty(::cFilter_Log)
     ::cTable:=cTable
     EJECUTAR("LBXFILTERLOG",Self)  // Lee su Valor
  ENDIF

  cWhere:=cWhere+CHR(28) // Necesario para Realizar Filtrado en el oCursor

  DEFAULT ::cWhereLog:=""

  cSql    :="SELECT "+cSelect+" FROM "+UPPE(cTable)+;
            " "+cInner  +;
            " "+::cWhereLog + IIF(Empty(::cWhereLog) .AND. !Empty(cWhere) ,""," AND ")+cWhere +;
            " "+cGroup  +;
            " "+cHaving +;
            " "+cOrderBy+" "

  ::cSub01    :=BUILDSUBC(::cSub01,"SUB01")
  ::cSub02    :=BUILDSUBC(::cSub02,"SUB02")
  ::cSub03    :=BUILDSUBC(::cSub03,"SUB03")
  ::cSub04    :=BUILDSUBC(::cSub04,"SUB04")
  ::cSub05    :=BUILDSUBC(::cSub05,"SUB05")
  ::cSub06    :=BUILDSUBC(::cSub06,"SUB06")
  ::cSub07    :=BUILDSUBC(::cSub07,"SUB07")
  ::cSub08    :=BUILDSUBC(::cSub08,"SUB08")
  ::cSub09    :=BUILDSUBC(::cSub09,"SUB09")
  ::cSub10    :=BUILDSUBC(::cSub10,"SUB10")
  ::cSub11    :=BUILDSUBC(::cSub01,"SUB11")
  ::cSub12    :=BUILDSUBC(::cSub02,"SUB12")
  ::cSub13    :=BUILDSUBC(::cSub03,"SUB13")
  ::cSub14    :=BUILDSUBC(::cSub04,"SUB14")
  ::cSub15    :=BUILDSUBC(::cSub05,"SUB15")
  ::cSub16    :=BUILDSUBC(::cSub06,"SUB16")
  ::cSub17    :=BUILDSUBC(::cSub07,"SUB17")
  ::cSub18    :=BUILDSUBC(::cSub08,"SUB18")
  ::cSub19    :=BUILDSUBC(::cSub09,"SUB19")
  ::cSub20    :=BUILDSUBC(::cSub10,"SUB20")

  cSql:=STRTRAN(cSql,"&SUB01",::cSub01)
  cSql:=STRTRAN(cSql,"&SUB02",::cSub02)
  cSql:=STRTRAN(cSql,"&SUB03",::cSub03)
  cSql:=STRTRAN(cSql,"&SUB04",::cSub04)
  cSql:=STRTRAN(cSql,"&SUB05",::cSub05)
  cSql:=STRTRAN(cSql,"&SUB06",::cSub06)
  cSql:=STRTRAN(cSql,"&SUB07",::cSub07)
  cSql:=STRTRAN(cSql,"&SUB08",::cSub08)
  cSql:=STRTRAN(cSql,"&SUB09",::cSub09)
  cSql:=STRTRAN(cSql,"&SUB10",::cSub10)
  cSql:=STRTRAN(cSql,"&SUB11",::cSub11)
  cSql:=STRTRAN(cSql,"&SUB12",::cSub12)
  cSql:=STRTRAN(cSql,"&SUB13",::cSub13)
  cSql:=STRTRAN(cSql,"&SUB14",::cSub14)
  cSql:=STRTRAN(cSql,"&SUB15",::cSub15)
  cSql:=STRTRAN(cSql,"&SUB16",::cSub16)
  cSql:=STRTRAN(cSql,"&SUB17",::cSub17)
  cSql:=STRTRAN(cSql,"&SUB18",::cSub18)
  cSql:=STRTRAN(cSql,"&SUB19",::cSub19)
  cSql:=STRTRAN(cSql,"&SUB20",::cSub20)

  cSql:=EJECUTAR("WHERE_VAR",cSql)


  ::cSqlFilter:=cSql // Utilizado para Buscar

  cSql :=STRTRAN(cSql,CHR(28),"")

  IF !Empty(::cIniFilter) .AND. (ValType(::cIniFilter)="C" .AND. !(".F."$::cIniFilter))
      // Obtiene un Solo Registro y sera Blanqueado
      cSql:=cSql+" LIMIT 1"
   ENDIF

   ::cWhere:=STRTRAN(cWhere,CHR(28),"") // Sin Char 28

   cLimit:=GETLIMITREC(cTable)

   IF !Empty(cLimit)
     cLimit:=" [Límite:"+LSTR(cLimit)+"/"+LSTR(COUNT(cTable))+" Rec.]"
   ELSE
     cLimit:=""
  ENDIF

  nAt:=ASCAN( aDpLbx , {|o| o:cSql=cSql} )

  //oDp:oFrameDp:SetText(STR(nAt))

  IF nAt>0 .AND. !oDp:lMdiLbx
    ::End()
    RETURN aDpLbx[nAt]
  ENDIF

  IF lChkSql .AND. !ChkSql(cSql,NIL,.T.)
     EJECUTAR("CHKSQL",cSql,.F.,.T.)
  ENDIF

  // ? lChkSql,ChkSql(cSql,NIL,.T.,.T.),"lChkSql,ChkSql(cSql,NIL,.T.,.T.)"
  IF lChkSql .AND. !ChkSql(cSql,NIL,.T.)
     MensajeErr(cSql,"Expresión SQL no Válido")
     CLPCOPY(cSql)
     EJECUTAR("LBXEDITLBX",cFileLbx)
     ::lSqlOk:=.F.
     RETURN SELF
  ENDIF

  CursorWait()

  ::cTable    :=UPPE(ALLTRIM(cTable))

  ::cTopic   :=IIF( EMPTY(::cTopic  ) , cTable             , ::cTopic   ) // Nombre del Topico
  ::cFileChm :=IIF( EMPTY(::cFileChm) , GetFileChm(cTable) , ::cFileChm ) // Archivo de Ayuda
  ::cIniSql  :=::cIniSql

  oDp:cHelpTopic:=::cTopic
  oDp:cFileChm  :=::cFileChm

  CursorWait()

  IF ::oDb=NIL
    oOdbc:=GetOdbc(cTable)
  ELSE
    oOdbc:=::oDb
  ENDIF

  oDp:cSqlErr:=""

  IF (oOdbc:cType=="MSSQL" .OR. oOdbc:cType="ADS") .OR. "COUNT(*"$cSql
//.OR.;
//     (!oOdbc:cType=="MYSQL")

     oOdbc:lCsrDyn:= .T.
     ::oCursor    :=OpenTable(cSQL,.T.,::oDb)
     cSql:=::oCursor:cSql

  ELSE

//oDp:oFrameDp:SetText(cSql)

     oDp:lSqlSayErr_:=oDp:lSqlSayErr
     oDp:lSqlSayErr :=.F.

// ? ::oDb:cName,"::oDb"

     ::oCursor    :=OpenTable(cSQL,.T.,::oDb)

// ? oDp:cSqlErr,"oDp:cSqlErr"

     IF Empty(::oCursor:aFields) .AND. !Empty(oDp:cSqlErr)
       // nuevamente
       ::oCursor    :=OpenTable(cSQL,.T.,::oDb)
     ENDIF

//   oDp:lSqlSayErr_:=oDp:lSqlSayErr
     oDp:lSqlSayErr :=oDp:lSqlSayErr_

// ? ::oCursor:ClassName(),oDp:oMySqlCon:oError:GetError(),LEN(::oCursor:aFields), oDp:cSqlErr
//? ::oCursor:nSeconds

  ENDIF

/*
  IF !Empty(::cIniFilter) .AND. ::oCursor:Recno()=1
     // Debe Blanquear el Contenido
     AEVAL(::oCursor:aFields,{|a,n| ::oCursor:Replace(a[1],CTOEMPTY(::oCursor:Fieldget(n)))})
  ENDIF
*/

//::aFields:=ACLONE(::oCursor:aFields)
  ::aFields:=::oCursor:aFields
  ::cWhere_:=::oCursor:cWhere // Copiar del Where

  IF .F. // ya esta revisando anteriormente cSelect$cSql .AND. !ChkSql(cSql,NIL,.T.) .OR. Empty(::oCursor:aFields)
     ::oCursor:End()
     MensajeErr(cSql,"Expresión SQL no Válido")
     CLPCOPY(cSql)
     EJECUTAR("LBXEDITLBX",cFileLbx)
     ::lSqlOk:=.F.
     RETURN SELF
  ENDIF

  // Lectura de Pocetas
  FOR I := 1 TO 40

      lTotal:=GetLbx("COL"+STRZERO(I,2)+"_TOTAL",NIL,"L",NIL,.F.)

       // etLbx(cName , lEmpty , cType , lFound , lDefault)
      lTotal:=IF(ValType(lTotal)="L",lTotal,.F.)

      IF lTotal
        lHayTotales:=.T.
      ENDIF

  NEXT I

// ? lHayTotales,"lHayTotales"

  FOR I=1 TO LEN(::oCursor:aFields)

     IF !Empty(GetOptions(::oCursor:cTable,::oCursor:aFields[I,1])) // Campos Con Opciones
       AADD(::aColColor,{I,::oCursor:cTable,::oCursor:aFields[I,1]})
     ENDIF

   NEXT I

  aTotales:=ATOTALES(::oCursor:aDataFill)

// ViewArray(aTotales)

  ::cSql      :=cSql
  ::aZero     :={}
  ::cOrderBy  :=ALLTRIM(StrTran(cOrderBy,"ORDER BY",""))
  ::cOrder    :=::cOrderBy

  DpWrite(::cFileSql,cSql)

  ::lPage     :=lPage
  ::lDeleteRef:=lDelRef
  ::lFullSize :=lFullSize
  ::lFullHight:=lFullHight
  ::lFullWidht:=lFullWidht

  IF ValType(::oCursor)!="O"
     MsgAlert("Cursor "+cSql)
     RETURN .F.
  ENDIF

  IF lDialog

      DEFINE DIALOG oWnd;
             TITLE GetFromVar(cTitle);
             FROM 100,100 TO nWidth,nHeight;
             PIXEL;
             OF DpWndMain()

//IF(ValType(::oGet)="O",::oGet,DpWndMain())

    ELSE


      IF lFullSize

        DEFINE WINDOW oWnd MDICHILD;
               TITLE GetFromVar(cTitle) ;
               FROM NIL,NIL TO nWidth,nHeight;
               OF ::oWndMain

      ELSE

        DEFINE WINDOW oWnd MDICHILD;
               TITLE GetFromVar(cTitle) ;
               NOMAXIMIZE;
               FROM NIL,NIL TO nWidth,nHeight;
               OF ::oWndMain

      ENDIF

      oWnd:Hide()

      DEFINE ICON oIco RESNAME "ICON"

      oWnd:SetIcon(oIco)
      SendMessage( oWnd:hWnd, 128, 1, oIco:hICON )

      oIco:=NIL

      ::cItem:="R:1/"+LSTR(::oCursor:RecCount()) // ALLTRIM(GetFromVar(cTitle)+cLimit)

      SET MESSAGE OF oWnd TO ::cMessage FONT oFont

      //SET MESSAGE OF oWnd FONT oFont

      // DEFINE MSGITEM ::oItem  OF oWnd:oMsgBar;
      //       PROMPT ::cItem SIZE ::nItemLen;
      //       ACTION 1=1

      IF !Empty(::cItem1)

        ::nItemLen1:=IF(Empty(::nItemLen2),LEN(::cItem2),::nItemLen1)

        DEFINE MSGITEM ::oItem1  OF oWnd:oMsgBar;
               PROMPT ::cItem1 SIZE ::nItemLen1;
               ACTION 1=1

      ENDIF

      IF !Empty(::cItem2)

        ::nItemLen2:=IF(Empty(::nItemLen2),LEN(::cItem2),::nItemLen2)

        DEFINE MSGITEM ::oItem2  OF oWnd:oMsgBar;
               PROMPT ::cItem2 SIZE ::nItemLen2;
               ACTION 1=1

      ENDIF

      IF !Empty(::cItem)

        DEFINE MSGITEM ::oItem  OF oWnd:oMsgBar;
               PROMPT ::cItem SIZE 100;
               ACTION 1=1

      ENDIF



   ENDIF

  // JN 13/07/2018

   ::oWnd:=oWnd

/*
   IF !oItem=NIL
     ::cItem     :="1/"+LSTR(::oCursor:Recno()) // Cantidad de Registros
     ::oItem:SetText(::cItem)
   ENDIF
*/

   oBrw := TXBrowse():New( oWnd )

   oBrw:nHeaderLines:=1

   IF nStyle=1
     oBrw:nMarqueeStyle       := MARQSTYLE_HIGHLCELL
     oBrw:nColDividerStyle    := LINESTYLE_BLACK
     oBrw:nRowDividerStyle    := LINESTYLE_BLACK
     oBrw:lColDividerComplete := .t.
     oBrw:nMarqueeStyle       := MARQSTYLE_HIGHLROW
   ENDIF

   ::nStyle:=nStyle

   IF lHayTotales
      oBrw:lFooter     := .T.
      oBrw:nFooterLines:= 1
      oBrw:bClrHeader  := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
      oBrw:bClrFooter  := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
   ENDIF


  IF !EMPTY(nClrPane1) .AND. !EMPTY(nClrPane2)

     IF !::lDialog
       oBrw:SetColor(nClrText,nClrPane1)
     ENDIF

     oBrw:bClrStd := {|| {nClrText, iif( oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }

  ELSEIF !EMPTY(nClrPane1)

     IF !::lDialog
       oBrw:SetColor(nClrText,nClrPane1)
     ENDIF
     oBrw:bClrStd := {|| {nClrText, nClrPane1 } }
  ENDIF

   oBrw:bClrFooter         := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
   oBrw:bClrHeader          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}

  oBrw:SetFont(oFont)

  IIF( !Empty(nFreeze),oBrw:nFreeze:= nFreeze,NIL)


  ::oLbx:=SELF

  oBrw:bGoTop    := {|| ::oCursor:Gotop(),oBrw:nArrayAt := 1,oBrw:nRowSel := 1}
  oBrw:bGoBottom := {|| oBrw:nArrayAt := ::oCursor:nRecCount()}
  oBrw:bBookMark := {| n | iif( n == nil, oBrw:nArrayAt, oBrw:nArrayAt := n)}
  oBrw:bKeyNo    := {|| ::oCursor:nRecno}
  oBrw:bKeyCount := {|| IIF(Empty(::oCursor:nRecCount()),0,::oCursor:nRecCount())}
  oBrw:bBof      := {|| oBrw:nArrayAt == 1 }
  oBrw:bEof      := {|| oBrw:nArrayAt == ::oCursor:nRecCount()}
  oBrw:cAlias    := ""
  oBrw:bSkip     := { | nSkip, nOld | ;
                        iif(nSkip == nil, nSkip := 1, ),;
                        nOld       := oBrw:nArrayAt,;
                        oBrw:nArrayAt += nSkip,;
                        oBrw:nArrayAt := Min( Max( oBrw:nArrayAt, 1 ), ::oCursor:nRecCount()),;
                        ::oCursor:GotoId(oBrw:nArrayAt),;
                        oBrw:nArrayAt - nOld }

  oBrw:bChange := {|| oBrw:DrawLine(.T.), ::LbxChange()} // 13/12/2016

  oBrw:bResized:= {|| EJECUTAR("LBXRESIZED",SELF,oWnd,oBrw)} // DoBrw:SetSize(oWnd:nWidth(),oWnd:nHeight()-125),oDp:oFrameDp:SetText("ME MOVE")}
//oBrw:bMoved  := {|| EJECUTAR("LBXRESIZED",SELF,oWnd,oBrw)} //


// JN 11/04/2016
// oWnd:bGotFocus :={|| oBrw:Refresh(.F.)} // JN 11/04/2016

  oBrw:nArrayAt := 1

   IF EMPTY(::oCursor:nRecCount())
      ::oCursor:nRecno:=0
      oBrw:nArrayAt:=0
   ENDIF

   IF .T. // lFields // Coloca los Campos de Manera Automática

      FOR nFor := 1 to ::oCursor:nFields

        nAt:=ASCAN(aDelCol,{|a,n|a=::oCursor:aFields[nFor,1]})

        IF nAt>0
           LOOP
        ENDIF

        oCol:=oBrw:AddCol()
        oCol:cHeader      := ::oCursor:FieldName( nFor )
        oCol:bLClickHeader:= GenHeadBlock(Self,nFor)
        oCol:bStrData     := GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(ValType(::oCursor)="O" .AND. oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL),SELF)

        IF ValType(::oCursor:Fieldget(nFor))="N" // Numérico
           oCol:nDataStrAlign:= AL_RIGHT
           oCol:nHeadStrAlign:= AL_RIGHT
        ENDIF

        IF ValType(::oCursor:Fieldget(nFor))="D" // Fecha
           oCol:cEditPicture:= "99/99/9999"
        ENDIF

      NEXT

   ENDIF

   oCol:=NIL
   aLine:={}

   FOR I := 1 TO Len(oBrw:aCols)

      cHeader:=GetLbx("COL"+STRZERO(I,2)+"_HEADER")

      IF ValType(cHeader)="C" .AND. ";"$cHeader

         aHeader:=_VECTOR(cHeader,";")
         cHeader:=""
//ViewArray(aHeader)
         AEVAL(aHeader,{|a,n| a      :=IF("&"$a,MacroEje(a),a),;
                              cHeader:=cHeader+IF(Empty(cHeader),"",CRLF)+a })

         cHeader:=STRTRAN(cHeader,";",CRLF)

         DEFAULT ::nHeaderLines:=2

         oBrw:nHeaderLines:=::nHeaderLines

      ENDIF

      cView   :=GetLbx("COL"+STRZERO(I,2)+"_VIEW")
      nWidthC :=GetLbx("COL"+STRZERO(I,2)+"_WIDTH",NIL,"N")
      cData   :=GetLbx("COL"+STRZERO(I,2)+"_DATA")
      cPict   :=GetLbx("COL"+STRZERO(I,2)+"_PICTURE")
      nAling  :=GetLbx("COL"+STRZERO(I,2)+"_ALING")
      cFont   :=GetLbx("COL"+STRZERO(I,2)+"_FONT")
      nClrCol :=GetLbx("COL"+STRZERO(I,2)+"_CLRTEXT")
      cClrGra :=GetLbx("COL"+STRZERO(I,2)+"_COLOR")
      // lTotal  :=GetLbx("COL"+STRZERO(I,2)+"_TOTAL")
      lTotal  :=GetLbx("COL"+STRZERO(I,2)+"_TOTAL",NIL,"L",NIL,.F.)
      lEditar :=GetLbx("COL"+STRZERO(I,2)+"_EDIT" ,NIL,"L",NIL,.F.)

      // 14/05/2023
      IF lEditar
         AADD(::aColEdit,{I,::oCursor:FieldName(I)})
      ENDIF

      nClrCol:=CTOO(nClrCol,"N")
      lTotal :=IF(ValType(lTotal)="L",lTotal,.F.)

//      IF lTotal
//        lHayTotales:=.T.
//      ENDIF

      oCol   :=oBrw:aCols[I]

      IF !EMPTY(cHeader)
         //cHeader:=IF("&"$cHeader,MacroEje(cHeader),cHeader) // JN 03/07/2018
         oBrw:aCols[I]:cHeader:=cHeader
      ENDIF

      IF !Empty(nAling)
        oBrw:aCols[I]:nDataStrAlign:= AL_RIGHT
        oBrw:aCols[I]:nHeadStrAlign:= AL_RIGHT
      ENDIF

      IF ValType(cView)="C" .AND. cView="2" .AND. ValType(::oCursor:FieldGet(I))="L" // Visualiza Logicos con Bitmaps

          oCol:AddBmpFile(DPBMP("xCheckOn.bmp"))
          oCol:AddBmpFile(DPBMP("xCheckOff.bmp"))
          oCol:bBmpData:=GetFromLogic( ::oCursor, I )
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    := {||""} // GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL))

      ENDIF

      IF ValType(cView)="C" .AND. cView="3" .AND. ValType(::oCursor:FieldGet(I))="L" // Visualiza Logicos con Bitmaps
          oCol:AddBmpFile(DPBMP("ledverde.bmp"))
          oCol:AddBmpFile(DPBMP("ledrojo.bmp"))
          oCol:bBmpData:=GetFromLogic( ::oCursor, I )
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    := {||""} // GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL))
      ENDIF


      IF ValType(cView)="C" .AND. cView="4" .AND. ValType(::oCursor:FieldGet(I))="L" // Visualiza Logicos con Bitmaps
          oCol:AddBmpFile(DPBMP("checkverde.bmp"))
          oCol:AddBmpFile(DPBMP("checkrojo.bmp"))
          oCol:bBmpData:=GetFromLogic( ::oCursor, I )
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    := {||""} // GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL))
      ENDIF


      IF ValType(nWidthC)="N" .AND. nWidthC>0
            oCol:nWidth:= nWidthC
      ENDIF

      IF !Empty(cPict) .AND. EMPTY(cData)
         cPict:=STRTRAN(cPict,'"',"")
         cData:="TRAN(oCursor:"+::oCursor:FieldName(I)+",'"+cPict+"')"
      ENDIF

      IF !Empty(cData)
         oCol:bStrData:=LbxGetData(cData,::oCursor,cPict)
      ENDIF

      IF lTotal .AND. !Empty(cPict)
         cPict:=STRTRAN(cPict,["],"")
         cPict:=ALLTRIM(cPict)
         oCol:cEditPicture:=cPict
         oCol:cFooter:=TRAN(aTotales[I],cPict)
         //oCol:nFooterStrAlign:= AL_RIGHT
         oCol:nFootStrAlign:= AL_RIGHT
      ENDIF

      // IF !Empty(cPict)
      //   oCol:cEditPicture:=cPict
      // ENDIF

      IF nClrCol<>0

// ? nClrPane1,nClrPane2,"nClrPane1,nClrPane2,BRWCOLSETCOLOR"
         oCol:bClrStd:=BRWCOLSETCOLOR(oBrw,nClrCol,nClrPane1,nClrPane2)

      ELSE

        // JN 12/07/2020
        nAt:=ASCAN(::aColColor,{|a,n| a[1]=I})

        IF nAt>0

// ? nClrPane1,nClrPane2,"nClrPane1,nClrPane2,LBXCOLSETCOLORDEF"

          oBrw:aCols[I]:bClrStd:=LBXCOLSETCOLORDEF(::oCursor,oBrw,I,::aColColor[nAt,2],::aColColor[nAt,3],nClrPane1,nClrPane2)
        ENDIF

     ENDIF

/*
// jn 23/07/2018
*/
      IF !Empty(cClrGra)

         nClrCol:=::oCursor:FieldPos(cClrGra)
         nClrCol:=IF(nClrCol=0,I,nClrCol)

// ? nClrPane1,nClrPane2,"nClrPane1,nClrPane2,BRWCOLSETCOLORFIELD"

         oCol:bClrStd:=BRWCOLSETCOLORFIELD(Self,nClrCol,nClrPane1,nClrPane2)

      ENDIF

      IF !Empty(cFont)
         oCol:oDataFont:=oFont(cFont)
      ENDIF

      IF !Empty(cPict)
         oCol:cEditPicture:=cPict
      ENDIF

   NEXT

   ::aDataEmpty:={}
   AEVAL( oBrw:aCols,{|o,n| AADD(aLine,Eval(o:bStrData)) })
   AADD(::aDataEmpty,aLine)

   oBrw:lHScroll        := lHScroll
   oBrw:lVScroll        := .t.
   oBrw:nRowSel         := 1
   oBrw:bKeyChar        := { | nKey | ::RunKey(nKey) }


//   IF lHayTotales
//      oBrw:lFooter     := .T.
//      oBrw:bClrHeader  := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
//      oBrw:bClrFooter  := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
//   ENDIF

   oBrw:bRClicked :={||eval(::bRClicked )}
   oBrw:bLDblClick:={|| IF(EJECUTAR("LBXRUNCLIC",SELF),NIL,eval(::bLDblClick))} // JN 14/05/2023

//? ::lDialog,oGet:ClassName()

   IF ::lDialog .AND. ValType(oGet)="O"

      oBrw:bLDblClick:={|uValue,nField|uValue:=GetValue(cField,::oCursor,NIL),;
                                       oGet:VarPut(uValue,.T.),;
                                       oGet:SetFocus()  ,;
                                       DpFocus(oGet)    ,;
                                       oGet:KeyBoard(13),;
                                       ::oWnd:End()       }

   ENDIF


   EJECUTAR("BRWCALTOTALES",oBrw,NIL,aTotales)

   oBrw:CreateFromCode()

   // oBrw:aArrayData:=oCursor:aDataFill
   // EJECUTAR("BRWCALTOTALES",oBrw,NIL,aTotales)

   oBrw:aData:={} // Utilizado para Filtrar y Opciones

   IF lFullSize

   ENDIF


   oBrw:aData     :=ACLONE(::oCursor:aDataFill) // Copia de los Datos
   oBrw:lSetFilter:=.F.

   oBrw:oLbx      :=SELF
   oBrw:GoTop()

   IF !lDialog

     oWnd:oClient := oBrw
     oWnd:SetControl( oBrw )

     ::oBar:=::SetBar(oWnd,NIL)

     oWnd:oClient:bKeyDown := {| nKey | if( nKey == 13   ,::RunKey(nKey)  , nil ) ,;
                                        if( nKey == 107  ,::Locate(::bFor , .T. ) , NIL ),;
                                        if( nKey == 27   ,::RunKey(nKey)  , .T. ) }

   ELSE

     oBrw:bKeyDown := {| nKey | if( nKey == 13   ,::RunKey(nKey)  , nil ) ,;
                                if( nKey == 107  ,::Locate(::bFor , .T. ) , NIL ),;
                                if( nKey == 27   ,::RunKey(nKey)  , .T. ) }
   ENDIF


   ::oWnd:=oWnd
   ::oBrw:=oBrw

   oLbx:=Self

   IF ::oCursor:Eof() // Si esta Vacio Busca el Boton Incluir

     // BTN01_TYPE   :=NEW
     oDp:nNumLbx    :=::nNumLbx
     nAt:=ASCAN(::aBtns,{|a|a[7]="NEW"})

     IF nAt>0
        IF ::RunAction(nAt,1,.F.) // Debe Tener Permiso
          nBtnRun:=nAt
          ::bStart:={||oDpLbx:=SELF,::RunAction(nBtnRun,1,.T.)}
        ELSE
          ::bStart:={||MensajeErr("Tabla Vacia, Sin Permiso para Incluir"),.F.}
        ENDIF

     ENDIF

   ENDIF

   nAt:=ASCAN(::aBtns,{|a|a[7]="OPEN"})

   IF nAt>0 .AND. EVAL(::aBtns[nAt,8]:bWhen)
      ::bLDblClick:=::aBtns[nAt,8]:bAction // Modificar
   ENDIF

   IF ::nBtnDblClic>0  .AND. ValType(::aBtns)="A"
      ::bLDblClick:=::aBtns[::nBtnDblClic,8]:bAction
   ENDIF

   IF ::oCursor:RecCount()=0 .AND. ValType(::aBtns)="A"
      AEVAL(::aBtns,{|o,n|IIF( n<len(::aBtns) .AND. ValType(o)="O",o[8]:Disable(),nil)})
   ENDIF

   oFont:=NIL
   oLbx :=NIL
   oDp:oFocus:=oBrw

// 02/02/2021
//   IF !Empty(cWhereGet)
//   EVAL(oBrw:bGoTop)
    //oBrw:Gotop()
//   oBrw:Refresh(.F.)
//   ENDIF

   //IF lHayTotales
   //  EJECUTAR("BRWCALTOTALES",oBrw)
   // ENDIF

   DpFocus(oBrw) // :SetFocus()

RETURN SELF

// Method Change
METHOD LbxChange()

   ::cItem:="R:"+LSTR(::oCursor:RecNo())+"/"+LSTR(::oCursor:RecCount()) // Muestra Posición del Regitro
   ::oItem:SetText( ::cItem)
   ::oItem:Refresh(.T.)

   ::CancelFind()  // no puede seguir buscando
   //::SetMsg("R:"+LSTR(::oCursor:RecNo())+"/"+LSTR(::oCursor:RecCount())) // Muestra Posición del Regitro

RETURN .T.

METHOD OpenTable(cSql) CLASS TDpLbx
  LOCAL oCursor:=OpenTable(cSql,.T.,::oDb)

  ::oCursor:aDataFill:=ACLONE(oCursor:aDataFill)
  ::oBrw:Refresh(.T.)

// ? "AQUI ES OPEN TABLE"
/*
  ::oCursor:Browse()
  ::oCursor:=OpenTable(cSql,.t.)
  ::oCursor:Browse()
*/

/*
  LOCAL cSql:="SELECT "+::cSelect+" FROM "+UPPE(::cTable)+;
              " "+::cInner  +;
              " "+::cWhere  +;
              " "+::cGroup  +;
              " "+::cHaving +;
              " "+::cOrderBy

 //::oCursor    :=OpenTable(cSQL,.T.)

 ? CLPCOPY(cSql)
*/
RETURN SELF

METHOD IsDef( cName ) CLASS TDpLbx
RETURN __objHasMsg( self, cName )

METHOD SetOrder(nCol) CLASS TDpLbx
  LOCAL cField,lDesc:=.F.

  cField:=::aFields[nCol,1]

  IF "MY"$::oCursor:ClassName() .AND. !ISFIELD(::cTable,cField)
     RETURN NIL
  ENDIF

  CursorWait()

  // Invertir el Orden de la Columna jn 20/02/2018
  IF cField=::oCursor:cField_Order
     lDesc:=!::oCursor:lDesc
  ENDIF

  ::oCursor:SetOrder(cField,lDesc)

  ::oBrw:KeyBoard(VK_HOME)
  ::oBrw:nColSel:=nCol
  ::oBrw:Refresh(.T.)

  // ? cField

  ::cOrder:=cField

RETURN NIL

/*
// Activar Browse
*/
METHOD ACTIVATE() CLASS TDpLbx

   LOCAL oDpLbx:=Self,oBrw:=::oBrw,oWnd:=::oWnd,oBar:=::oBar,aPoint
   LOCAL nClrPane1,nClrPane2
   LOCAL bStart:=::bStart
   LOCAL nAt,nBtnRun,cRun:=""
   LOCAL nTop :=oWnd:nTop
   LOCAL nLeft:=oWnd:nLeft
   LOCAL aCoors

//  LOCAL nClrPane:=::nClrPane1
// ? ::cFieldFind,"<-::cFieldFind",::uFieldFind,"<-::uFieldFind"

  ErrorSys(.T.)

   IF !Empty(::cFieldFind)
      //  JN 18/11/2020 ::GetValue(::cFieldFind,::uFieldFind)
      ::GetValye(::cFieldFind,::oGet)
   ENDIF

   IF ValType(::oGet)="O" .AND. !"TDOC"$::oGet:ClassName()
      aPoint:= AdjustWnd(::oGet,oWnd:nWidth(),oWnd:nHeight())
      nTop  :=aPoint[1]+5
      nLeft :=aPoint[2]-0
      // DPFOCUS(::oGet) 02/02/2021
      // ? "REPOSICIONA LBX",nTop,nLeft
   ENDIF

   ::nWidth :=nWidth
   ::nHeight:=nHeight

   // 02/08/2023 Facilita intervenir en la Barra de Botones
   IF !Empty(::cPrgIni)
      EJECUTAR(::cPrgIni,SELF)
   ENDIF

   IF ::lDialog

//     LOCAL nTop :=oWnd:nTop
//     LOCAL nLeft:=oWnd:nLeft

     nClrPane1:=GetLbx("COLORPANE1" ,NIL,"N")
     nClrPane2:=GetLbx("COLORPANE2" ,NIL,"N")

     oDpLbx:=Self

     nTop:=MAX(130,oWnd:nTop) // JN 03/12/2015

//     IF ValType(::oGet)="O"
//        aPoint:= AdjustWnd(::oGet,oWnd:nWidth(),oWnd:nHeight())
//        nTop  :=aPoint[1]+5
//        nLeft :=aPoint[2]-8
//     ENDIF

/*
     ACTIVATE DIALOG oWnd ON INIT  (oBrw:SetFocus(),;
                                    Ejecutar("LBXSYSMENU",oWnd,oDpLbx),;
                                    IF(oDpLbx:lGoBottom,oDpLbx:oBrw:Gobottom(),NIL),;
                                    oDpLbx:SetBar(oWnd,@oBar,oBrw),;
                                    oWnd:Move(oWnd:nTop,oWnd:nLeft,nWidth,nHeight,.T.),;
                                    oBrw:Move(oBar:nHeight(),0,oWnd:nWidth()-5,oWnd:nHeight()-oBar:nHeight()-35,.T.)  ,;
                                    oBrw:SetColor(0,nClrPane1),;
                                    Eval(bStart));
                                    VALID (killFind(oBrw) ,.T.)

     ACTIVATE DIALOG oWnd ON INIT  (oBrw:SetFocus(),;
                                    Ejecutar("LBXSYSMENU",oWnd,oDpLbx),;
                                    IF(oDpLbx:lGoBottom,oDpLbx:oBrw:Gobottom(),NIL),;
                                    oDpLbx:SetBar(oWnd,@oBar,oBrw),;
                                    oWnd:Move(nTop,nLeft,nWidth,nHeight,.T.),;   // oWnd:Move(oWnd:nTop,oWnd:nLeft,nWidth,nHeight,.T.),;
                                    oBrw:Move(oBar:nHeight(),0,oWnd:nWidth()-5,oWnd:nHeight()-oBar:nHeight()-35,.T.)  ,;
                                    oBrw:SetColor(0,nClrPane1),;
                                    .T.);
                                    VALID (killFind(oBrw),oDp:lSetDialog:=.F.,oDp:lLbxDialog:=.F.,.T.)


*/


     ACTIVATE DIALOG oWnd ON INIT  (oBrw:SetFocus(),;
                                    EJECUTAR("LBXSYSMENU",oWnd,oDpLbx),;
                                    EJECUTAR("LBXRESTOREPAR",oDpLbx)  ,;
                                    IF(oDpLbx:lGoBottom,oDpLbx:oBrw:Gobottom(),NIL),;
                                    oDpLbx:SetBar(oWnd,@oBar,oBrw),;
                                    oWnd:Move(nTop,nLeft,nWidth,nHeight,.T.),;   // oWnd:Move(oWnd:nTop,oWnd:nLeft,nWidth,nHeight,.T.),;
                                    oBrw:Move(oBar:nHeight(),0,oWnd:nWidth()-5,oWnd:nHeight()-oBar:nHeight()-35,.T.)  ,;
                                    oBrw:SetColor(0,nClrPane1),;
                                    .T.);
                                    VALID (EVAL(oDpLbx:bValid),;
                                           killFind(oBrw),oDp:lSetDialog:=.F.,oDp:lLbxDialog:=.F.,.T.)

   ELSE

     DEFAULT oDp:nWndTop:=90

     ACTIVATE WINDOW oWnd ON INIT  (oWnd:Hide(),;
                                    oBrw:SetFocus(),;
                                    EJECUTAR("LBXSYSMENU",oWnd,oDpLbx),;
                                    EJECUTAR("LBXRESTOREPAR",oDpLbx)  ,;
                                    oWnd:oMsgBar:SetColor(NIL,oDp:nGris),; // 15724527),;
                                    oDpLbx:oMsgBar:=oWnd:oMsgBar,;
                                    oDpLbx:lStart:=.F.,;
                                    Eval(bStart));
                             VALID (EVAL(oDpLbx:bValid),;
                                    killFind(oBrw) ,;
                                    oDpLbx:Close() ,;
                                    oDpLbx  :=NIL  ,;
                                    oBar     :=NIL  ,;
                                    oBrw     :=NIL  ,;
                                    .T.);
                             ON MOVE EJECUTAR("LBXMOVED",oDpLbx,oWnd,oBrw,"MOVE");
                             ON DOWN EJECUTAR("LBXMOVED",oDpLbx,oWnd,oBrw,"DOWN")


//? ::oWndMain:nTop,"::oWndMain:nTop"
//? nTop,nLeft,"nTop,nLeft"
// ::oWnd:oMsgBar:SetText(::cItem+" aqui es refrescar" )

   aCoors :=GetCoors( GetDesktopWindow() )

   IF ::oGet<>NIL

     // 04/02/2020 Amplia el Alto hasta el final del video
     IF ::lFullHight
       ::nHeight:=(aCoors[3]-(nTop-oDp:nWndTop))-160
     ENDIF

     oWnd:Move(nTop-oDp:nWndTop,nLeft,::nWidth,::nHeight,.T.)

   ELSE

     // aCoors :=GetCoors( GetDesktopWindow() )
     // FullHight Solo si no Existe Parámetro OGET

     IF ::lFullHight .AND. ::oGet=NIL
        nTop   :=0
//     aCoors :=GetCoors( GetDesktopWindow() )
       ::nHeight:=aCoors[3]-160
     ENDIF

//? ::lFullHight,"::lFullHight"

     IF ::lFullHight .AND. ValType(::oGet)="O"
        // Debe llegar hasta el Final
        ::nHeight:=aCoors[3]-160
//? "AQUI DEBE SER MAS ALTO"
        ::lResized:=.T. // Requiere Autoajustarse con bResized
     ENDIF

     IF ::lFullWidht .AND. ::oGet=NIL
//      aCoors :=GetCoors( GetDesktopWindow() )
        ::nWidth:=MAX(::nWidth,aCoors[4]-20)
        nLeft:=0
     ENDIF

     oWnd:Move(nTop,nLeft,::nWidth,::nHeight,.T.)

     IF ::lFullSize
        oWnd:Maximize()
     ENDIF

// ? nWidth,nHeight,"nWidth,nHeight",::lFullSize


   ENDIF

   oWnd:Show()

/*
    ACTIVATE WINDOW oWnd ON INIT  (oBrw:SetFocus(),; //  IIF( nWidth!=nil,oWnd:SetSize(nWidth,nHeight),nil),;
                                   Ejecutar("LBXSYSMENU",oWnd,oDpLbx),;
                                   oWnd:oMsgBar:SetColor(NIL,oDp:nGris),; // 15724527),;
                                   oDpLbx:lStart:=.F.,;
                                   oWnd:Move(oWnd:nTop,oWnd:nLeft,nWidth,nHeight,.T.),;
                                   oWnd:Refresh(.T.),Eval(bStart));
                            VALID (killFind(oBrw) ,;
                                   oDpLbx:Close() ,;
                                   oDpLbx  :=NIL  ,;
                                   oBar     :=NIL  ,;
                                   oBrw     :=NIL  ,;
                                   .T.)
*/

//     oWnd:bGotFocus:={||IIF(!EMPTY(oDp:cMapaTab),oDpLbx:oWnd:oBar:aControls[1]:ForWhen(),NIL),oDpLbx:ChkData()}
// 11/04/2016     oWnd:bGotFocus:={|| IIF(!EMPTY(oDp:cMapaTab), IF(ValType(oWnd)="O",oWnd:oBar:aControls[1]:ForWhen() , NIL) , NIL ), iif(ValType(oWnd)="O",oDpLbx:ChkData(),NIL)}

     oWnd:bGotFocus :={||   PUBLICO("oDpLbx",oDpLbx), IIF(!EMPTY(oDp:cMapaTab), IF(ValType(oWnd)="O",oWnd:oBar:aControls[1]:ForWhen() , NIL) , NIL ), iif(ValType(oWnd)="O",oDpLbx:ChkData(),NIL)}
//   oWnd:bLostFocus:={|| oDpLbx:CancelEdit()}

     oWnd:bLostFocus:={|| KILLFIND(::oBrw,NIL)}

     IF !oDp:lMenuXp

       oWnd:bGotFocus:={||IIF(!EMPTY(oDp:cMapaTab), IF(ValType(oWnd)="O",oWnd:oBar:aControls[1]:ForWhen() , NIL) , NIL ), iif(ValType(oWnd)="O", NIL ,NIL)}
       oDp:oFrameDp:Cascade()

     ENDIF

     IF ::lGoBottom
       ::oBrw:Gobottom(.T.)
     ELSE
       ::oBrw:GoTop(.T.)
     ENDIF

     ::oBrw:Refresh(.f.)

     IF !Empty(cRun)
        oDpLbx:=SELF
     ENDIF

     IF ::oGet<>NIL
        EJECUTAR("LBXINIFIND",Self)
     ENDIF

   ENDIF

   PUBLICO("oDpLbx",oDpLbx)

RETURN Self

/*
// Culminar la Clase
*/
METHOD End() CLASS TDpLbx

   IF ValType(::oWnd)="O" .AND. ::oWnd:hWnd>0
     ::oWnd:End()
   ENDIF

   oDp:cHelpRtf:=::cFileRtfOld

RETURN NIL

/*
// Revisa si al Recuperar el Foco Existe Data, sino Cierra el Browse
// Llamado por GotFocus
*/
METHOD GetRowSql() CLASS TDpLbx
   LOCAL aData:={}

   AEVAL(::oCursor:aFields,{|a,i| AADD(aData,::oCursor:FieldGet(i))})

RETURN aData

/*
// Revisa si al Recuperar el Foco Existe Data, sino Cierra el Browse
// Llamado por GotFocus
*/
METHOD ChkData()


   IF ::oCursor=NIL .OR. Empty(::oCursor:RecCount())
     IF ValType(::oWnd)="O"
        ::oWnd:End()
     ENDIF
   ENDIF

RETURN .T.

/*
// Refresca el Browse, ejecutado desde DPEDIT:SAVE()
*/
METHOD Reporte(cCodRep) CLASS TDpLbx
   LOCAL oTable,lFound:=.F.

   DEFAULT cCodRep:=::cTable

   //lFound:=SQLGET("DPREPORTES","REP_CODIGO","REP_CODIGO"+GetWhere("=",cCodRep))!=cCodRep

   //IF lFound
   IF !Empty(SQLGET("DPREPORTES","REP_CODIGO","REP_CODIGO"+GetWhere("=",cCodRep)))
      REPORTE(cCodRep)
   ELSE
      MensajeErr("No Existe Reporte para "+cCodRep)
   ENDIF

RETURN .F.
/*
// Refresca el Browse, ejecutado desde DPEDIT:SAVE()
*/
METHOD Refresh(lAppend,uValue) CLASS TDpLbx
   LOCAL nRecno:=0

   DEFAULT lAppend:=.T.

   IF ValType(::oWnd)="O" .AND. ::oWnd:hWnd=0
      RETURN .F.
   ENDIF

   IF ValType(::oCursor)!="O"
      RETURN .F.
   ENDIF


// ? GETPROCE(),"GETPROCE()"

   nRecno:=::oCursor:nRecno
   ::oCursor:Refresh()

   IF ValType(uValue)!="U" .AND. ::oCursor:RecCount()>1 .AND. ::oCursor:SeekBin(uValue,::cPrimary) // ::cOrder) 29/06/2017
      nRecno:=::oCursor:nRecno
   ENDIF

   IF ::oCursor:RecCount()>1

     nRecno:=MIN(::oCursor:nRecCount(),nRecno)
     ::oCursor:GotoId(nRecno)
     ::oBrw:Refresh(.T.)
     ::oCursor:GotoId(nRecno)
     ::oBrw:nArrayAt:=nRecno


   ELSE // IF ValType(::oBrw)="O"

     ::oBrw:nArrayAt:=1
     ::oBrw:Refresh(.T.)

   ENDIF

RETURN NIL

/*
// Cerrar la Clase y la Excluye de la Lista
*/
METHOD Close() CLASS TDpLbx
  LOCAL aData:={}
  LOCAL nPos:= aScan( aDpLbx, { |a,i|  ValType(a)="O" .AND. a:nNumLbx == ::nNumLbx} )

  ::oCursor:_lClosed:=.T.

  IF ValType(::oBrw)="O"

    ::oBrw:bGoTop    := {|| 1}
    ::oBrw:bGoBottom := {|| 1}
    ::oBrw:bBookMark := {|| 1}
    ::oBrw:bKeyNo    := {|| 1}
    ::oBrw:bKeyCount := {|| 1}
    ::oBrw:bBof      := {|| ::oBrw:nArrayAt == 1 }
    ::oBrw:bEof      := {|| ::oBrw:nArrayAt == 1 }
    ::oBrw:bSkip     := { | |1 }

    AEVAL(::oBrw:aCols,{|oCol,n|oCol:bBmpData:=BloqueCod("1"),oCol:bStrData:=BloqueCod("'X'")})
    ::oBrw:nArrayAt:=1
    ::oBrw:nRowSel :=1

  ENDIF

  ::oCursor:End()

  IIF( nPos> 0 ,  arreduce(aDpLbx, nPos) , nil)

  IF Empty(aDpLbx)
     MyCloseAll()
  ENDIF

  oDp:cHelpRtf:=::cFileRtfOld

RETURN NIL

METHOD SetBar(oWnd,oBar,oBrw) CLASS TDpLbx
   LOCAL oCursor,I,aBtn:={},cBtnBmp,bBtnAction,bBtnWhen,cBtnTool,oBtn,cMessage,nOption
   LOCAL nAt,nBtnRun
   LOCAL oFont,oFontG,cAction,cWhen,cType //,cValMapa
   LOCAL cBtnOff,lBtnOff:=.T.,bWhen,cBuscar
   LOCAL oPopFind
   LOCAL oDpLbx:=Self
   LOCAL nBtnHeight :=GetLbx("BTNHEIGHT"   ,NIL,"N")
   LOCAL nBtnWidth  :=GetLbx("BTNWITH"     ,NIL,"N")
   LOCAL nBtnClrPane:=GetLbx("BTNCLRPANE"  ,NIL,"N")
   LOCAL nBtnClrText:=GetLbx("BTNCLRTEXT"  ,NIL,"N")
   LOCAL aBuscar    :=EJECUTAR("LBXRESTFIND",SELF,"FIND")
   LOCAL nLin:=0

//   nBtnHeight :=IIF( EMPTY(nBtnHeight), 35-1 , nBtnHeight )
//   nBtnWidth  :=IIF( EMPTY(nBtnWidth ), 35-1 , nBtnWidth  )


   nBtnHeight :=IIF( EMPTY(nBtnHeight), 40 , nBtnHeight )
   nBtnWidth  :=IIF( EMPTY(nBtnWidth ), 40    , nBtnWidth  )

   nBtnHeight :=40
   nBtnWidth  :=40

   nBtnClrPane:=IIF(ValType(nBtnClrPane)!="N",NIL,nBtnClrPane)
   nBtnClrText:=IIF(ValType(nBtnClrText)!="N",NIL,nBtnClrText)

   DEFINE FONT oFont NAME "Courier New" SIZE 0,-9

   DEFINE FONT oFontG NAME "Tahoma" SIZE 0,-10 BOLD


   // Jn Buscar Registro
//   IF ::lDialog
//      oBrw:SetColor(::nClrPane1,::nClrPane2)
//   ENDIF

IF !Empty(aBuscar)

   C5MENU oPopFind POPUP;
          COLOR    oDp:nMenuItemClrText,oDp:nMenuItemClrPane;
          COLORSEL oDp:nMenuItemSelText,oDp:nMenuItemSelPane;
          COLORBOX oDp:nMenuBoxClrText;
          HEIGHT   oDp:nMenuHeight;
          FONT     oDp:oFontMenu;
          LOGOCOLOR oDp:nMenuMainClrText

          FOR I=1 TO LEN(aBuscar)

            cBuscar   :=STRTRAN(ALLTRIM(aBuscar[I]),"\'","'") // Recupera Comilla Simple
            aBuscar[I]:=cBuscar

            DO CASE

             CASE "["$aBuscar[I]
               cBuscar:=["]+cBuscar+["]
             CASE ["]$aBuscar[I]
               cBuscar:="["+cBuscar+"]"
            OTHERWISE
               cBuscar:=["]+cBuscar+["]
            ENDCASE

            bWhen:=[EJECUTAR("LBXRUNFIND",]+cBuscar+[)]

// ?  bWhen,"bWhen"
// oDp:oFrameDp:SetText("bWhen "+ bWhen) // Quitar cuando funcione

            bWhen:=BloqueCod(bWhen)
            C5MenuAddItem(aBuscar[I],,.F.,,bWhen,,,,,,,.F.,,,.F.,,,,,,,,.F.,)

          NEXT I

   C5ENDMENU

ENDIF

   FOR I := 1 TO 30

     cBtnBmp:=GetLbx("BTN"+STRZERO(I,2)+"_BMP")
     cBtnBmp:=IIF( ValType(cBtnBmp)="C",DPBMP(cBtnBmp),cBtnBmp)

//IF !Empty(cBtnBmp)
//? I,cBtnBmp
//ENDIF

     IF !EMPTY(cBtnBmp) // Existe Boton
        cType  :=GetLbx("BTN"+STRZERO(I,2)+"_TYPE")

        cAction:=GetLbx("BTN"+STRZERO(I,2)+"_ACTION")
        cAction:=IIF( Empty(cAction),".T." ,cAction )

        cWhen  :=GetLbx("BTN"+STRZERO(I,2)+"_WHEN")
        cWhen  :=IIF( EMPTY(cWhen  ),".T." ,cWhen )

        IF cType="NEW"  .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABINC("+CTOSQL(::cTable)+")"
        ENDIF

        IF cType="VIEW" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABCON("+CTOSQL(::cTable)+")"
        ENDIF

        IF cType="OPEN" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABMOD("+CTOSQL(::cTable)+")"
        ENDIF

        IF cType="DELETE" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABELI("+CTOSQL(::cTable)+")"
        ENDIF

        IF cType="PRINT" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABPRN("+CTOSQL(::cTable)+")"
        ENDIF

        IF cType="QUERY" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABQRY("+CTOSQL(::cTable)+")"
        ENDIF

        IF cType="MENU" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABMNU("+CTOSQL(::cTable)+")"
        ENDIF

        // Previsualizacion Requiere el Mismo Proceso que PRINT
        IF cType="VIEW" .AND. ::lTablaAcc
           cWhen:=cWhen+" .AND. ISTABPRN("+CTOSQL(::cTable)+")"
        ENDIF

        cAction:=STRTRAN(cAction,CRLF,"")

IF oDp:lTracer
  oDp:oFrameDp:SetText(cAction)
ENDIF
        AADD(::aBtnRun  ,{BloqueCod(cAction),cAction})

IF oDp:lTracer
  oDp:oFrameDp:SetText(cWhen)
ENDIF

        AADD(::aBtnWhen ,{BloqueCod(cWhen  ),cWhen  })

        cBtnTool  :=GetLbx("BTN"+STRZERO(I,2)+"_TOOLTIP")
        cMessage  :=GetLbx("BTN"+STRZERO(I,2)+"_MESSAGE")
        nOption   :=VAL(GetLbx("BTN"+STRZERO(I,2)+"_OPTION",.T.))

        DEFAULT cMessage:=cBtnTool
        DEFAULT cBtnTool:=cMessage

        AADD(aBtn,{cBtnBmp,cAction,cWhen,cBtnTool,cMessage,nOption,cType,NIL})

     ENDIF

   NEXT

// ? "VER LISTA DE BOTONES"
// ViewArray(aBtn)

   DEFINE CURSOR oCursor HAND

   DEFINE BUTTONBAR oBar SIZE nBtnWidth,nBtnHeight OF oWnd 3D CURSOR oCursor

   // DEFINE BUTTONBAR oBar SIZE 50,50 OF oWnd 3D CURSOR oCursor

   oBar:SetColor(NIL,oDp:nGris)

   ::oBar:=oBar

// oDp:oFrameDp:Settext("FOR I := 1 TO LEN(aBtn)")

   FOR I := 1 TO LEN(aBtn)

      lBtnOff:=.T.
      cBtnOff:=STRTRAN(UPPE(aBtn[I,1]),".BMP","G.BMP")

      IF !File(cBtnOff)
         lBtnOff:=oDp:lBtnOff
         cBtnOff:=aBtn[i,1]
      ENDIF

      bWhen   :=GenBlqAction(Self,I,aBtn[I,6],.F.) // Evalua Condiciones

      IF !lBtnOff .AND.  aBtn[I,7]="OTHER" .AND. !EVAL(bWhen)
         LOOP
      ENDIF

      IF (nBtnClrPane!=NIL .AND. nBtnClrText!=NIL) .AND. ;
         (nBtnClrPane=0 .AND. nBtnClrText=0)

      IF aBtn[I,7]="FIND" .AND. !oPopFind=NIL

        DEFINE BUTTON oBtn;
               OF oBar;
               NOBORDER;
               FONT oFont;
               FILENAME aBtn[I,1],NIL,cBtnOff;
               MENU oPopFind;
               ACTION (1=1)

        oBtn:cToolTip:="Visualización 3D"

      ELSE

//         DEFINE XBUTTON oBtn ;
//                FILE aBtn[I,1],NIL,cBtnOff;
//                TOOLTIP aBtn[I,5];
//                OF oBar;
//                SIZE nBtnWidth,nBtnHeight

 DEFINE BUTTON oBtn;
               OF oBar;
               NOBORDER;
               FONT oFont;
               FILENAME aBtn[I,1],NIL,cBtnOff;
               ACTION (1=1)


      ENDIF


      ELSE

        DEFINE XBUTTON oBtn ;
               FILE aBtn[I,1],,cBtnOff;
               TOOLTIP aBtn[I,5];
               SIZE nBtnWidth,nBtnHeight;
               COLORS CLR_WHITE, { nRGB(243,250,200), CLR_BLACK, 5 } OF oBar   // degraded background color            ROUNDRECT   // elliptic button

// ? "AQUI",oBtn:ClassName()

      ENDIF

      Publico("oCursor",::oCursor)

      oBtn:bAction :=GenBlqAction(Self,I,aBtn[I,6],.T.)
      oBtn:bWhen   :=GenBlqAction(Self,I,aBtn[I,6],.F.) // Evalua Condiciones
      oBtn:cToolTip:=aBtn[I,4] // + " AQUI ES "+aBtn[I,6]
      oBtn:cMsg    :=aBtn[I,5]

      aBtn[I,8]:=oBtn

// ? I,aBtn[I,6],aBtn[I,4]

   NEXT I

   oBar:bRClicked := {|| NIL }

// oDp:oFrameDp:Settext("oBtn:ForWhen()")
// oBtn:ForWhen()
// oBar:SetColor(NIL,oDp:nGris) // 15724527)
// oDp:oFrameDp:Settext("::aBtns:=ACLONE(aBtn)")
// ::aBtns:=ACLONE(aBtn) // Todos los Botones


   ::aBtns:=aBtn // Todos los Botones

   IF ::oCursor:Eof() // Si esta Vacio Busca el Boton Incluir

      nAt:=ASCAN(::aBtns,{|a|a[7]="NEW"})

      IF nAt>0

        IF ::RunAction(nAt,1,.F.) // Debe Tener Permiso

          nBtnRun:=nAt
          ::bStart:={||oDpLbx:=SELF,::RunAction(nBtnRun,1,.T.)}

          IF ::lDialog
            ::oWnd:bStart:=::bStart
          ENDIF

        ELSE

          ::bStart:={||MensajeErr("Tabla Vacia, Sin Permiso para Incluir"),.F.}

        ENDIF


     ENDIF

    ENDIF

    IF ::lDialog
       // Solo en Dialogo
       ::oBrw:Move(oBar:nHeight(),0,::oWnd:nWidth()-5,::oWnd:nHeight()-oBar:nHeight()-35,.T.)
    ELSE
 // OJO 15/04/2016
       Aeval(oBar:aControls,{|o|o:SetColor(NIL,oDp:nGris)})

       nLin:=32
       AEVAL(oBar:aControls,{|o,n|nLin:=nLin+o:nWidth() })

       IF !Empty(::cFilter_Log)

          @ 1,nLin+10 CHECKBOX ::oFilterLog VAR ::lFilterLog PROMPT ::cFilter_Text OF oBar SIZE 100,20 PIXEL;
             ON CHANGE EJECUTAR("LBXFILTERLOGRUN",Self) FONT oFontG  // Lee su Valor

       ENDIF

     ENDIF

//  oCursor:=NIL
// oDp:oFrameDp:Settext("Return oBar")

RETURN oBar

/*
// Ejecución desde los Botones
*/
METHOD RunAction(nButton,nOpc,lAction)  CLASS TDpLbx
  LOCAL uValue:=.T.,i,nAt,uKey,aKey

  PUBLICO("oDpLbx",SELF)
  PRIVATE oBrw   :=::oBrw
  PRIVATE oCursor:=::oCursor
  PRIVATE oWnd   :=::oWnd
  PRIVATE nOption:=nOpc

  oDp:nNumLbx    :=::nNumLbx

  PUBLICO("oDpLbx",SELF) // JN 02/04/2013

  oDp:oLbx:=SELF

  SETDBSERVER(::oDb)

  DO CASE

     CASE !lAction .AND. ValType(nButton)="N" // Evalue When

        uValue:=.T.

        IF ValType(::aBtnWhen[nButton,1])="B"
          uValue:=EVAL(::aBtnWhen[nButton,1])
        ELSE
          uValue:=.T.
        ENDIF

     CASE lAction.AND.ValType(nButton)="N"


// ? nOpc,"nOpc",nButton
// nAt :=IF(nOpc>0 .AND. ::aBtns[nOpc,7]="VIEW", 0 , nOpc ) // ASCAN(::aBtns,{|a|a[7]="VIEW"}) // .OR. a[7]="MOD"})

        IF !Empty(::cPrimary) // ::cOrderBy)

          uKey:=""

          IF ","$::cPrimary // ::cOrderBy
            akey:=_VECTOR(::cPrimary,",") //_VECTOR(::cOrderBy,",")
            uKey:=""
            AEVAL(aKey, {|a,n| uKey:=uKey + CTOO(MacroEje("oCursor"+":"+a),"C") })
          ELSE
//          uKey:=MacroEje(STRTRAN("oCursor"+":"+::cOrderBy,",","+oCursor:"))
            uKey:=MacroEje(STRTRAN("oCursor"+":"+::cPrimary,",","+oCursor:"))
          ENDIF

          IF nButton=2 // Consultar
            AUDITAR("DCON" , NIL ,::cTable , uKey , NIL, SELF )
          ENDIF

        ENDIF

        uValue:=EVAL(::aBtnRun[nButton,1])

        IF ValType(uValue)="O".AND."DPEDIT"$uValue:ClassName() // Le dice a la clase que asocie este ListBox
           uValue:nNumLbx:=::nNumLbx // Permite Refrescar el Browse
        ENDIF

     CASE ValType(nButton)="C" // Comando Directo

   ENDCASE

   SETDBSERVER()

   STORE NIL TO oDpLbx,oCursor,oWnd,oBrw

   oDp:nLbxRun    :=0 // asociación entre el Editor y el ListBox

RETURN uValue

/*
//  Bucar Más
*/
METHOD FindMas() CLASS TDpLbx
   LOCAL nRecord,nLen,bFind,uFind:=::uFind
   LOCAL lIndex,lFound:=.F.
   LOCAL nFieldPos,nRecno:=::oCursor:Recno()

   IF EMPTY(uFind) .OR. ::oCursor:Eof()
      RETURN .T.
   ENDIF

   // lIndex   :=(::cFieldFound==::cOrder) // Si es Indice
   lIndex   :=(::cFieldFound==::cPrimary) // Si es Indice
   nFieldPos:=::oCursor:FieldPos(::cFieldFound)

   //? ::cFieldFound,nFieldPos,::uFind,"BUSCAR MAS"

   CURSORWAIT()

   IF ValType(::oCursor:FieldGet(nFieldPos))="C"
     uFind:=ALLTRIM(UPPE(uFind))
     nLen :=LEN(uFind)
     IF lIndex
       bFind:={||LEFT(UPPE(::oCursor:FieldGet(nFieldPos)),nLen)==::uFind}
     ELSE
       bFind:={|uValue|uValue:=ALLTRIM(UPPE(::oCursor:FieldGet(nFieldPos))),::oCursor:GotoId(nRecord),(Left(uValue,nLen)==uFind) .OR. uFind$uValue}
     ENDIF
   ELSE
     uFind:=CTOO(uFind,ValType(::oCursor:FieldGet(nFieldPos)))
     bFind:={||::oCursor:FieldGet(nFieldPos)==uFind}
   ENDIF

   nRecord  := ::oCursor:Recno()+1

   WHILE nRecord<=::oCursor:RecCount()
       ::oCursor:GotoId(nRecord)
       IF EVAL(bFind)
          lFound:=.t.
          EXIT
       ENDIF
       nRecord++
   ENDDO

   IF !lFound

      ::oCursor:GotoId(nRecno)

   ELSE

      ::oCursor:GotoId(nRecord)
      ::oBrw:GoTop()
      ::oBrw:nArrayAt:=nRecord
      ::oBrw:DrawLine(.t.)
      DpFocus(::oBrw)

   ENDIF

   CURSORARROW()

RETURN .T.

/*
// Prepara el Browse para Importar el Valor de un Campo
// uAdd:= Agrega el Valor a la Selección
*/
METHOD GetValue(cField,oGet,bBlq,uAdd,uValue) CLASS TDpLbx
   LOCAL nPos,aData,I,lFound:=.F.,nArrayAt,nRecno:=0
   LOCAL aPoint
   LOCAL x   :=ErrorSys(.T.)
   LOCAL nAt :=0
   LOCAL nRow:=0,nAtPos

   DEFAULT bBlq:={||.T.}

   DEFAULT ::oGet:=oGet

   IF !ValType(::oBrw)="O"
      RETURN .F.
   ENDIF

   nAt :=::oBrw:nArrayAt
   nRow:=::oBrw:nRowSel

   // Sin parametros LBX, el oGet lo obtiene en este metodo y reajusta la columna

   IF ::oGet=NIL .AND. ValType(oGet)="O"
      ::oGet:=oGet
   ENDIF
/*
   IF ValType(::oGet)="O" .AND. !"TDOC"$::oGet:ClassName()

       aPoint:= AdjustWnd(::oGet,::oWnd:nWidth(),::oWnd:nHeight())
       // nTop  :=aPoint[1]+5
       // nLeft :=aPoint[2]-8
       ::oWnd:Move(aPoint[1],aPoint[2],NIL,NIL,.T.)

   ENDIF
*/
   IF ValType(uAdd)="C" .AND. "GET"$oGet:ClassName() // Debe Sumar el Valor Anterior
      uAdd:=ALLTRIM(EVAL(oGet:bSetget))+uAdd
      oGet:nLastKey:=0
   ENDIF

   DEFAULT cField:=::cOrder

   // ? ::oCursor,"CURSOR"

   IF !ValType(::oCursor)="O"
      RETURN .F. // NIL
   ENDIF

   IF Valtype(cField)="C" .AND. (","$cField)
      // el campo viene en lista
      cField:=_VECTOR(cField,",")[1]
   ENDIF

//  OJO   SysRefresh(.T.)

   nPos:=::oCursor:FieldPos(cField)

   IF nPos=0 //::oCursor:FieldPos(cField)=0

      MsgAlert("Campo: "+cField+" No está en la Lista")

      RETURN .F.  // NIL

   ENDIF

   IF oGet:ClassName()!="TDOCGRIDCOL"

     IF ValType(oGet)="O"
       uValue:=IIF( oGet:ClassName()="TXBRWCOLUMN",uValue,EVAL(oGet:bSetGet))
     ELSE
       uValue:=oGet
     ENDIF

   ENDIF

   // QUITAR VACIOS

   IF !Empty(uValue) .AND. !"WHERE"$::cSql

      IF ValType(uValue)="C"

        aData:={uValue,UPPE(uValue)}

        FOR I=1 TO LEN(uValue)

          uValue:=aData[I]

          WHILE .T.

            IF ValType(::oCursor)="O" .AND. ::oCursor:RecCount()>0 .AND. ::oWnd:hWnd>0 .AND. ::DbSeek(::oBrw:aCols[nPos],uValue,13) .OR. EMPTY(uValue)
               nRecno:=::oCursor:RecNo()
               lFound:=.T.
               EXIT
            ENDIF

            uValue:=LEFT(uValue,LEN(uValue)-1)

           ENDDO

           IF lFound
              EXIT
           ENDIF

        NEXT I

      ELSE

        IF ::DbSeek(::oBrw:aCols[nPos],uValue,13)
           // ::oBrw:Refresh(.T.)
           // ? "LISTO"
        ENDIF

      ENDIF

//      ErrorSys(.t.)
//      MsgAlert(str(::oCursor:Recno()),"Recnoaqui es")
//      ::oBrw:ColAtPos( nPos )
//      ? "buscado",,"recno"

    ENDIF

    IF ValType(oGet)="O"

        ::bLDblClick:={|uValue,nField|uValue:=GetValue(cField,::oCursor,uAdd),::oWnd:End(),oGet:VarPut(uValue,.T.),oGet:SetFocus(),Eval(bBlq,uValue,oGet),;
                        DpFocus(oGet),oGet:KeyBoard(13),NIL} /* )oGet:Keyboard(13)} ,;
                        oGet:oWnd:GoNextCtrl( oGet:hWnd )} */



        IF oGet:ClassName()="TXBRWCOLUMN"

            nArrayAt:=oGet:oBrw:nArrayAt

            ::bLDblClick:={|uValue,nField|uValue:=GetValue(cField,::oCursor,uAdd),;
                                          ::oWnd:End(),;
                                          oGet:lButton:=.F.,;
                                          oGet:VarPut(uValue,.T.,nArrayAt),;
                                          Eval(bBlq,uValue,oGet),;
                                          Eval(oGet:bOnPostEdit,oGet,uValue)}

        ENDIF

        ::lExport   := .T. // Debe Expotar Con Enter o Doble Click

        IF oGet:ClassName()="TDOCGRIDCOL"

           ::bLDblClick:={|uValue,nField|oDp:aRowSql:=::GetRowSql(),uValue:=GetValue(cField,::oCursor,uAdd),::oWnd:End(),;
                           IF(::nKey!=27,(oGet:GotFocus(),oGet:VarPut(uValue,.T.),DpFocus(oGet:oWnd)), oGet:GotFocus())}

        ENDIF


   ELSE // Colocar Valor en BloqueCodigo

      ::bLDblClick:={|uValue,nField|uValue:=GetValue(cField,::oCursor),::oWnd:End(),Eval(bBlq,uValue,oGet),EVAL(oDp:bLbxRun,uValue)}
      ::lExport   := .T.

   ENDIF

// OJO   SysRefresh(.T.)
// ? lFound,"lFound",uValue,"uValue",::cSql

   IF lFound
      // ? "AQUI ENCONTRADO",::oBrw:nArrayAt,nRecno
      // ::oBrw:Gotop()
      // ::oBrw:nArrayAt:=
      // ::oCursor:RecNo()
      // ::oBrw:Refresh(.F.)
       oDp:nRecord:=nRecno

       ::oBrw:GoTop()
       ::oCursor:GotoId(nRecno)
       ::oBrw:nArrayAt:=nRecno
       ::oBrw:DrawLine(.t.)

   ELSE
       ::oBrw:DrawLine(.T.) // JN 22/04/2016 Refresh(.F.)
       ::oBrw:nArrayAt:=nAt
       ::oBrw:nRowSel :=nRow
   ENDIF

   ::CancelFind()
   ::oBrw:Refresh(.F.)

RETURN lFound // NIL


/*
 *  OnError()
 */
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TDpLbx

  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N //,oFrameDp
  LOCAL lResp:=.F.

  cMsg   := ALLTRIM(UPPE(__GetMessage()))

  if Left( cMsg, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2,Len(cMsg))
      __objAddData( Self, cMsg )
      __objSendMsg(Self ,cMsg , uValue)

   ENDIF

RETURN NIL


STATIC FUNCTION GetValue(cField,oCursor,uAdd)
    LOCAL uValue,I,cWhere:="",cSql,oTable

    FOR I := 1 TO oCursor:nFields
      IF oCursor:FieldGet(I)!=NIL
        IF oCursor:FieldName(I)=cField
          uValue:=oCursor:FieldGet(i)
        ENDIF
        cWhere:=cWhere+IIF( EMPTY(cWhere),"" , " AND " )+oCursor:FieldName(i)+GetWhere("=",oCursor:FieldGet(i))
      ENDIF
    NEXT

    //? cWhere,uValue
    // ? oCursor:cFileName

    IF uValue=NIL // Hace otro Query para Buscar el Valor de Campo
      cSql  :="SELECT "+cField+" FROM "+oCursor:cFileName+" WHERE "+cWhere
      oTable:=OpenTable(cSql,.T.,oCursor:oOdbc)
      uValue:=oTable:FieldGet(cField)
      oTable:End()
    ENDIF

    IF ValType(uAdd)="C"
       uValue:=uAdd+uValue
    ENDIF

RETURN uValue

/*
// Cuando se Pierde el Focus, debe Apagar las Búsquedas
*/

METHOD CancelEdit() CLASS TDpLbx
/*
  LOCAL nLen,nFor,oCol

  nLen := len( ::oBrw:aCols )

  for nFor := 1 to nlen

     oCol := ::oBrw:aCols[ nFor ]

     if oCol:oEditGet != nil
        oCol:oEditGet:End()
     endif

     oCol:nEditType:=0

   NEXT
*/
// ErrorSys(.T.)
// ::oBrw:CancelEdit()

RETURN NIL

METHOD CancelFind() CLASS TDpLbx

  LOCAL nLen,nFor,oCol

  nLen := len( ::oBrw:aCols )

  ::oBrw:CancelEdit()

  for nFor := 1 to nlen

     oCol := ::oBrw:aCols[ nFor ]

     if oCol:oEditGet != nil
        oCol:oEditGet:End()
     endif

     oCol:nEditType:=0

   NEXT

RETURN .T.


/*
// Preparar busqueda
*/
METHOD Find(lFind) CLASS TDpLbx
  LOCAL I,oCol,oTable,cWhere:="",nAt,cField

  DEFAULT lFind:=.T.


  RETURN EJECUTAR("LBXSETFIND",SELF,lFind)

/*
  ::oBrw:CancelEdit()

  IF EMPTY(::aZero)

    cWhere:="CAM_TABLE = '"+::oCursor:cFileName+"'"
    oTable:=OPENTABLE("SELECT * FROM DPCAMPOS WHERE "+ cWhere , .T. )

    WHILE !oTable:Eof()
       FOR I := 1 TO LEN(::oCursor:aFields)
         IF ALLTRIM(UPPE(::oCursor:aFields[I,1]))=ALLTRIM(UPPE(oTable:CAM_NAME))
            AADD(::aZero,CTOLOG(oTable:CAM_ZERO))
            // ? oTable:CAM_ZERO,"ZERO"
         ENDIF
       NEXT
       oTable:DbSkip()
    ENDDO
    oTable:End()

  ENDIF

  IF ::lFind
     ::lFind:=.f.
     ::oBrw:CancelEdit()
     Return killfind(::oBrw)
  ENDIF

  ::lFind:=.T.

  FOR I=1 TO LEN(::oBrw:aCols)
     ::oBrw:aCols[I]:nEditType:=1

     IF lFind
       ::oBrw:aCols[I]:bOnPostEdit:={|oCol,uValue,nLastKey,nCol|::DbSeek(oCol,uValue,nLastKey),::lFind:=.F.,KillFind(::oBrw,::oBrw:nColSel)}
     ELSE
       // Localizar
       ::oBrw:aCols[I]:bOnPostEdit:={|oCol,uValue,nLastKey,nCol|EJECUTAR("LBXFILTER",Self,oCol,uValue,nLastKey),::lFind:=.F.,KillFind(::oBrw,::oBrw:nColSel)}
     ENDIF

  NEXT

  oCol := ::oBrw:SelectedCol()
  oCol:Edit()
*/

RETURN NIL

/*
// Preparar busqueda
// Hay que Buscar si este Campo tiene Cero hacia la Izquierda
*/
METHOD DbSeek(oCol,uValue,nLastKey) CLASS TDpLbx
    LOCAL lFound:=.F.
    LOCAL aData,I

    IF ValType(uValue)="C" // Busqueda Incremental

       aData:={uValue,UPPE(uValue)}

       FOR I := 1 TO LEN(aData)

          uValue:=aData[I]

          WHILE LEN(uValue)>0 .AND. !lFound
             lFound:=::DbSeek2(oCol,uValue,nLastKey)
             uValue:=LEFT(uValue,LEN(uValue)-1)
          ENDDO

          IF lFound
            EXIT
          ENDIF

       NEXT

    ELSE

       lFound:=::DbSeek2(oCol,uValue,nLastKey)

    ENDIF

RETURN lFound

/*
// Preparar busqueda
// Hay que Buscar si este Campo tiene Cero hacia la Izquierda
*/
METHOD DbSeek2(oCol,uValue,nLastKey) CLASS TDpLbx
    LOCAL nField:=oCol:nPos,cField:=::oCursor:FieldName(nField),nNext,lZero
    LOCAL lIndex:=.F.,lFound:=.F.,nFieldPos,uFind,nLen
    LOCAL bFor,bFind,nRecord:=1

    oDp:nRecord:=0

    IF Empty(cField)
      RETURN .F.
    ENDIF

    CURSORWAIT()

    IF nField>0 .AND. FIELDZERO(::cTable,cField)
      uValue:=CTOZERO(uValue)
    ENDIF

    ::uFind      :=uValue // Para Buscar con +
    ::cFieldFound:=cField

    lIndex   :=!EMPTY(::cOrder) .AND. cField=::cOrder
    nFieldPos:=::oCursor:FieldPos(cField)

//  ? nFieldPos,cField,::oCursor:FieldGet(nFieldPos)
//  ? ValType(::oCursor:FieldGet(nFieldPos)),ValType(uValue),uValue,nField

    IF nFieldPos>0  .AND. !lIndex .AND. ValType(::oCursor:FieldGet(nFieldPos))="C" // Busca en un solo Campo

       uFind  :=ALLTRIM(UPPE(uValue))
       ::uFind:=uFind

       WHILE ValType(uFind)="C" .AND. LEN(uFind)>0 .AND. !lFound

         nLen :=LEN(uFind)

//         IF lIndex
//            bFind:={||LEFT(UPPE(::oCursor:FieldGet(nFieldPos)),nLen)==uFind}
//         ELSE

         bFind:={|uValue|uValue:=UPPE(::oCursor:FieldGet(nFieldPos)),Left(uValue,nLen)==uFind .OR. uFind$uValue}

//       ENDIF

         nRecord  := 1
         ::oCursor:GotoId(nRecord)

         WHILE nRecord<=::oCursor:RecCount()
           ::oCursor:GotoId(nRecord)
           IF EVAL(bFind)
             lFound:=.t.
             EXIT
           ENDIF
           nRecord++
         ENDDO

         IF !lFound
           uFind:=ALLTRIM(LEFT(uFind,nLen-1))
           LOOP
         ENDIF

         EXIT

      ENDDO

      IF !lFound
        ::oCursor:GoTop()
      ENDIF

    ENDIF

    // ? lIndex,"lIndex       ",lFound

    IF lIndex .AND. !lFound

      // ? "lIndex",uValue,cField,"BUSQUEDA BIN"

      ::oCursor:aFldPrimary:={cField}

      // ErrorSys(.T.)

      lFound:=::oCursor:SeekBin(uValue,cField,.F.,@nRecord)

  //    IF nRecord<>::oCursor:Recno()
  //       ::oCursor:GotoId(nRecord)
  //    ENDIF

      // lFound:=.F.

    ELSEIF !lFound

      IF ValType(::oCursor:FieldGet(nField))="C"
         uValue:=ALLTRIM(uValue)
         bFor:={||UPPE(uValue)$UPPE(::oCursor:FieldGet(nField))}
      ELSE
         uValue:=CTOO(uValue,ValType(::oCursor:FieldGet(nField)))
         bFor:={||uValue=::oCursor:FieldGet(nField)}
      ENDIF

      lFound:=::Locate(bFor,.f.,@nRecord)

    ENDIF

    IF lFound

       nNext:=::oCursor:nRecno()

       // MsgAlert("ENCONTRADO "+uValue+" nRecord "+STR(nRecord),"AQUI ES")

       oDp:nRecord:=nNext


       IF !ValType(::oBrw)="O"
           ::Close()
           RETURN .F.
       ENDIF

       ::oBrw:GoTop()
       ::oCursor:GotoId(nNext)
       ::oBrw:nArrayAt:=nNext
       ::oBrw:DrawLine(.t.)
       // ::oBrw:GotoCol( nField ) // JN 11/04/2016 Posicionar Columna

       // MsgAlert(STR(nNext),"AQUI ES")

       DpFocus(::oBrw)

    ENDIF

RETURN lFound

/*
// Ejecucion segun Tecla
*/
METHOD RunKey(nKey) CLASS TDpLbx
    LOCAL nAt
    LOCAL bBlq:={||.T.}

    // JN esta en busqueda inicial , Presionar C genera "Concultar"
    IF !Empty(::cIniFilter) .AND. ::oCursor:Recno()=1
        RETURN .T.
    ENDIF

    DO CASE

      CASE nKey=43 // Bucar Más

        ::FindMas()

      CASE nKey=ASC("B") .OR. nKey=ASC("b")

        nAt:=ASCAN(::aBtns,{|a|a[7]="FIND" .OR. "XFIND"$UPPE(a[1])})

        IF nAt>0
           bBlq:=::aBtns[nAt,8]:bAction  // Buscar
        ENDIF

      CASE (nKey=ASC("I") .OR. nKey=ASC("i"))

        nAt:=ASCAN(::aBtns,{|a|a[7]="NEW"})

        IF nAt>0 .AND. EVAL(::aBtns[nAt,8]:bWhen)
          bBlq:=::aBtns[nAt,8]:bAction  // Buscar
        ENDIF

      CASE nKey=ASC("C") .OR. nKey=ASC("c")

        nAt:=ASCAN(::aBtns,{|a|a[7]="VIEW"})

        IF nAt>0 .AND. EVAL(::aBtns[nAt,8]:bWhen)
          bBlq:=::aBtns[nAt,8]:bAction  // Buscar
        ENDIF

      CASE nKey=ASC("M") .OR. nKey=ASC("m") .OR. (nkey=13 .AND. !::lExport)

        nAt:=ASCAN(::aBtns,{|a|a[7]="OPEN"})

        IF nAt>0 .AND. EVAL(::aBtns[nAt,8]:bWhen)
          bBlq:=::aBtns[nAt,8]:bAction  // Buscar
        ENDIF

      CASE (nkey=13 .AND. ::lExport)

        // bBlq:=::aBtns[nAt,8]:bAction  // EXPORTAR
        bBlq:=::bLDblClick // :={|uValue,nField|uValue:=GetValue(cField,::oCursor,uAdd),::oWnd:End(),oGet:VarPut(uValue,.T.)}

     CASE  nKey == 107  // Seguir Buscando

        ::Locate(::bFor , .T. )

     CASE nkey=27

        ::nKey:=27
        ::Close()
        ::End()

        RETURN .T.

     OTHE

    ENDCASE

    EVAL(bBlq)

RETURN .T.
/*
// Genera Búsqueda Sequenciales
*/
METHOD Locate(bFor,lNext,nRecord) CLASS TDpLbx
   LOCAL lFound:=.F.,nRecno:=::oCursor:Recno(),nNext

   DEFAULT lNext:=.F.

   IF ValType(bFor)!="B"
      RETURN .F.
   ENDIF

   IF lNext
      ::oCursor:Skip(1)
   ENDIF

   CursorWait()

   IF ValType(::oMsgBar)="O"
     ::oMsgBar:SetMsg("Localizando Registro....")
   ENDIF

   WHILE !::oCursor:Eof() // .AND. ValType(::bFor)="B"
      lFound:=EVAL(bFor)
      IF lFound
         EXIT
      ENDIF
      ::oCursor:Skip(1)
   ENDDO

   IF !lFound
      ::oCursor:GotoId(nRecno)
      IF ValType(::oMsgBar)="O"
         ::oMsgBar:SetMsg("Registro no Localizado")
      ENDIF
   ELSEIF lNext
      nNext:=::oCursor:nRecno()
      ::oBrw:GoTop()
      ::oCursor:GotoId(nNext)
      ::oBrw:nArrayAt:=nNext
      ::oBrw:DrawLine(.t.)
      DpFocus(::oBrw)
   ENDIF

   IF lFound .AND. ValType(::oMsgBar)="O"
     ::oMsgBar:SetMsg("[+] Seguir Localizando")
   ENDIF

   IF lFound
     nRecord:=::oCursor:Recno()
   ENDIF

   ::bFor:=bFor
   CursorArrow()

   SysRefresh()


RETURN lFound

METHOD RELOAD()

   ::oCursor:Refresh(.T.)
   ::oBrw:Refresh(.T.)
// OpenTable(::cSQL,.T.)
// ::oCursor:Browse()


/*
   ::oCursor:GoTop()
   ::oBrw:Gotop(.T.)
   ::oBrw:Refresh(.T.)
     ::oCursor:Refresh()
*/

RETURN NIL


/*
// Ejecuta el Borrado de Registros
*/
METHOD DELETE(cMsg,lAsk,nOrder,cClave,cField,lDirect,cPrgPosDel) CLASS TDpLbx
     LOCAL cSql,nAtPos,oCuerpo,I,U,uValue,cWhere:="",oOdbc
     LOCAL nArrayAt,aLinks,aFieldsHead,aFieldsBody,cLinked,nAt,bStart:={||.T.}
     LOCAL aClave:={},aPos:={},oTable,oIntRef //,aData:={} //,uValue
     LOCAL lResp

     DEFAULT lDirect:=.T.

     nOrder:=IIF( nOrder=NIL,  1   ,nOrder )
     lAsk  :=IIF( lAsk  =NIL, .T.  ,lAsk )

     IF EMPTY(cField)
        cField:=::cPrimary // ::cOrderBy // Cursor:
     ENDIF

     IF EMPTY(cField)
        cField:=::oCursor:FieldName(1)
     ENDIF

     IF cClave=NIL // La Otiene de la Suma de Cadenas

        aClave:=_VECTOR(cField)

        IF EMPTY(aClave)
           aClave:={cField}
        ENDIF

        Aeval(aClave,{|a,n|AADD(aPos,::oCursor:FieldPos(a))})

        cClave:=""
        FOR I := 1 TO LEN(aPos)

           uValue:=aPos[I]
           IF ::oCursor:FieldPos(uValue)=0
             cClave:=cClave+IF(!Empty(cClave),",","")+CTOO(uValue,"C")
           ENDIF

        NEXT I

        IF !Empty(cClave)
           MensajeErr("Requiere campos "+cClave)
           RETURN .F.
        ENDIF

        cClave:=""
        FOR I := 1 TO LEN(aPos)

           uValue:=::oCursor:FieldGet(aPos[I]) // Ojo Con las Fechas
           // uValue:=CTOO(uValue,"C")
           // uValue:=CTOSQL(uValue) //,"C")

           IF EMPTY(cClave) .AND. I=1
              cClave:=CTOO(uValue,"C") // Esta es Para Mostrar
           ELSE // IF  ValType(cClave)="C"
              cClave:=cClave+IF(!Empty(cClave),",","")+CTOO(uValue,"C")
           ENDIF

           cWhere:=cWhere + IIF( Empty(cWhere), "" , " AND " ) + aClave[I]+GetWhere("=",uValue)

        NEXT

     ELSE

        cWhere:=cField+GetWhere("=",cClave)

     ENDIF

     cMsg    :=IIF( ValType(cMsg)="U", "Clave "+CTOO(cClave,"C") , cMsg)

     IF lAsk.AND.!MsgNoYes(cMsg+CRLF+"De "+ALLTRIM(::oWnd:cTitle)," Borrar Registro ") // de "+ALLTRIM(::oWnd:cTitle))
        RETURN .F.
     ENDIF

//   oIntRef:=IntRef(::oTable) // Solo Para Modificar
/*
     aLinks:=GetLinks(::oCursor:cFileName) // Obtiene todas la tablas relacionadas

     FOR I := 1 TO LEN(aLinks)
        aFieldsHead:=_VECTOR(aLinks[I,2])
        aFieldsBody:=_VECTOR(aLinks[I,3])
        cWhere:=""
        FOR U := 1 TO LEN(aFieldsHead)
           uValue:=::oCursor:Fieldpos(aFieldsHead[U])
           uValue:=::oCursor:FieldGet(uValue)
           cWhere:=cWhere+IIF( EMPTY(cWhere),"" , " AND " )+aFieldsBody[u]+"='"+uValue+"'"
        NEXT

        // Borra el Archivo Asociado
        cLinked:=aLinks[I,1]
        cLinked:=SUBS(cLinked,AT(".",cLinked)+1,20)
        // Debe hacer un Query y borrar tambien a sus Asociados //
        cSql   :="DELETE FROM "+cLinked+" WHERE "+cWhere
        oOdbc  :=GetOdbc(cLinked) // Requiere el Odbc de esta tabla
        // OJO ESTE BORRA LOS RELACIONADOS oOdbc:Execute(cSql)
        oOdbc:=NIL

     NEXT

     nArrayAt:=::oBrw:nArrayAt

     cField:=::oCursor:IndexKey(nOrder) // +GetWhere("=",cClave)
*/
     nArrayAt:=::oBrw:nArrayAt

     ::oCursor:GoTop()

     IF !Empty(cWhere) .AND. !Empty(::cScope)
        cWhere:=ADDWHERE(cWhere,::cScope)
     ELSEIF !Empty(::cScope)
        cWhere:=::cScope
     ENDIF

     // ? cWhere,"cWhere"

     cSql  :="SELECT * FROM "+::cTable+" WHERE "+cWhere+" LIMIT 1"

     IF lDirect

        IF oDp:cTypeBD="MYSQL"
          cWhere:=cWhere + " LIMIT 1"
        ENDIF

        SQLDELETE(::cTable,cWhere,.F.,::oDb)

        DPWRITE("TEMP\"+::cTable+"_DELETE.SQL",oDp:cSql)

// ? oDp:cSql,"REMOVER"
// ? "AQUI ES, DELETE ",::oDb:ClassName()

        // ? cField,"cField",uValue

        AUDITAR("DELI" , NIL , ::cTable , uValue , NIL , SELF ) // ::oTable:GetDataKey(NIL,cField))

        IF UPPE(::cTable)=="DPTABLAS"
          LOADTABLAS(.T.) // Debe Cargar en Memoria todas las tablas
        ENDIF

     ELSE

     oTable:=OpenTable(cSql,.T.,::oDb)

//? cSql

     if !::lDeleteRef .AND. IsIntRef(::cTable) // Verifica Integridad Referencial

        oIntRef:=IntRef(oTable) // Solo Para Modificar
        oIntRef:Run(oTable)     // Guarda todos los Valores

        IF oIntRef:IsDelete()

           oIntRef:Delete() // Borra todo lo Referenciado

           // RETURN .T.
           //

        ELSEIF !oIntRef:IsTableRel()

           EJECUTAR("DELETEMSG",cClave,NIL,oTable:cTable,oIntRef:aList)
           oIntRef:End()

           RETURN .F.

        ENDIF

        oIntRef:End()

       ENDIF


       oTable:cPrimary:=cField

       AUDITAR("DELI" , NIL , oTable:cTable , oTable:GetDataKey(NIL,cField) ,;
               NIL, SELF)

       oTable:DeleteAll(.T.)

     ENDIF

     IF oDp:lTracerSql
        ? cSql
     ENDIF

     ::oCursor:Refresh()

     IF EMPTY(::oCursor:nRecCount())
        ::oCursor:nRecno:=0
        oBrw:nArrayAt:=0
        oBrw:Refresh()
        AEVAL(::aBtns,{|o,n|IIF( n<len(::aBtns),o[8]:Disable(),nil)})

        // Debe Enviar para Incluir
        nAt:=ASCAN(::aBtns,{|a|a[7]="NEW"})
        IF nAt>0
          bStart:=::aBtns[nAt,8]:bAction // ]{||MsgAlert("Inicio")}
        ENDIF
        EVAL(bStart)

     ELSE
        ::oBrw:Refresh()
        ::oBrw:nArrayAt:=nArrayAt-1
        nArrayAt:=MIN(::oCursor:nRecCount(),nArrayAt)
        ::oBrw:nArrayAt:=nArrayAt
     ENDIF

     EJECUTAR("LBXPOSTDELETE",SELF)

     // Personalizar Programa post-Eliminar
     IF !Empty(cPrgPosDel)
        EJECUTAR(cPrgPosDel,SELF)
     ENDIF

RETURN .T.


/*
// Busca desde archivo LBX, los valores
*/
// lFound, pasa como Referencia
FUNCTION GetLbx(cName , lEmpty , cType , lFound , lDefault)
   Local uValue,nAT

   DEFAULT lEmpty:=.F.

   // ErrorSys(.t.)

   IF Empty(cName )
      RETURN ""
   ENDIF

   IF ValType(aLines)="C" // Hay que Convertirno en Cadenas

       aLines:=STRTRAN(aLines,CHR(13),"")
       aLines:=STRTRAN(aLines,":=",CHR(02))
       aLines:=_VECTOR(aLines,CHR(10))

       FOR nAt := 1 TO LEN(aLines)

         aLines[nAt]:=IIF( "/"+"/"$aLines[nAt],LEFT(aLines[nAt],AT("/"+"/",aLines[nAt])-1),aLines[nAt])

         IF !EMPTY(aLines[nAt])

           uValue:=_VECTOR(aLines[nAt],CHR(02))
           uValue[1]:=UPPE(ALLTRIM(uValue[1]))

           IF len(uValue)<3

             IF "["$aLines[nAt]
                AADD(uValue,STRTRAN(aLines[nAt],CHR(02),""))
             ELSE
                AADD(uValue,"")
             ENDIF

           ENDIF

           AADD(uValue,NIL) // 3RA Columna de Valores

           aLines[nAT]:=uValue

         ELSE

           aLines[nAt]:={"","",nil}

         ENDIF

       NEXT

       aMemos[Len(aMemos),2]:=aLines  // Asigna el Memo

   ENDIF

   // Ahora Realiza las búsquedas
   cName :=ALLTRIM(UPPE(cName))
   nAt   :=ASCAN(aLines,{|aVar|aVar[1]==cName}) // ESTA DEBE SER PUBLICA
   uValue:=IIF(nAt=0,NIL, aLines[nAt,2] )
   lFound:=(nAt>0)

// ViewArray(aLines)

   IF lFound .AND. cType="C"
      RETURN uValue
   ENDIF

//   IF nAT>0 .AND. aLines[nAt,3]<>NIL
//      RETURN aLines[nAt,3]
//   ENDIF

   IF lEmpty
      DEFAULT uValue:=""
   ENDIF

   IF cType="N"
      uValue:=CTOO(uValue,cType)
   ENDIF

   IF cType="L"

     IF ValType(uValue)="C"

       uValue:=(ALLTRIM(UPPE(uValue))=".T.")

     ELSE

       DEFAULT lDefault:=.F.
       uValue=lDefault

     ENDIF

   ENDIF

//   IF nAT>0
//      aLines[nAt,3]:=uValue
//   ENDIF

RETURN uValue

FUNCTION LBXGETLINES()
RETURN aLines

/*
// Iniciación de Lectura de Valores de GetLbx()
*/
FUNCTION IniGetLbx(cValue,cFileLbx)
   LOCAL I

   DEFAULT cValue:=""
   aLines:=cValue


   IF !Empty(cFileLbx)

      DEFAULT aMemos:={}

      I:=ASCAN(aMemos,{|a,n|a[1]==cFileLbx}) // Busca Memos

      IF I=0
        AADD(aMemos,{cFileLbx,cValue})
      ENDIF

   ENDIF

RETURN NIL

/*
// Devuelve lógico 1 o 2
*/
STATIC FUNCTION GetFromLogic( oCursor, nField )
RETURN {|uValue|uValue:= oCursor:FieldGet( nField ) , IIF( uValue , 1 , 2 )}

// Genera un valor Logico de un campo Numerico, el campo debe tener el nombre LOGICO
STATIC FUNCTION GetFromLogic_Num( oCursor, nField )
RETURN {|uValue|uValue:= oCursor:FieldGet( nField )>0 , IIF( uValue , 1 , 2 )}


/*
// Genera las columnas como Bloques de Código
*/
FUNCTION GenCursorBlock( oCursor, nField, bBlq,oDpLbx)
     LOCAL nInt,nDec,cPicture:="",aValues
     LOCAL cField:=oCursor:FieldName(nField)
     LOCAL nLen  :=oCursor:FieldLen(nField)
     LOCAL cType :=oCursor:FieldType(nField)
     LOCAL cTable:=oDpLbx:cTable
     LOCAL uValue:=oCursor:FieldGet( nField )

     DEFAULT bBlq:={||.T.}

     // Verifica si el Campo Tiene Opciones

     IF ValType(oCursor:FieldGet(nField))="N"
        cPicture:=GetPicture(oCursor:aFields[nField])
        RETURN {|| EVAL(bBlq),TRAN(oCursor:FieldGet( nField ),cPicture) }
     ENDIF

     IF ValType(oCursor:FieldGet(nField))="L"
        RETURN {|| EVAL(bBlq),IIF(oCursor:FieldGet( nField ),"Si","No") }
     ENDIF

     // ? oCursor:FieldGet(nField),VALTYPE(oCursor:FieldGet(nField))

     // IF ValType(oCursor:FieldGet(nField))="D"
      //  ? "FECHA"
      //  RETURN {|| EVAL(bBlq),DTOC(oCursor:FieldGet( nField )) }
     // ENDIF

     aValues:=GetOptions(cTable,cField) // Campos Con Opciones

     IF !EMPTY(aValues)                               // Tiene Opción

// uValue:=SAYOPTIONS(cTable,cField,uValue,NIL,aValues)
// ? cTable,cField,oCursor:FieldGet( nField ),LEN(aValues), uValue)
// SAYOPTIONS(cTable,cField,uValue,NIL,aValues)

        RETURN {|uValue| EVAL(bBlq) , uValue:=oCursor:FieldGet( nField ), SAYOPTIONS(cTable,cField,uValue,NIL,aValues)}


//SAYOPTIONS(cTable,cField,uValue,NIL,aValues) }
     ENDIF

// JN 16/04/2018 , Debe devolver el Campo con su espacio completo para lograr buscar
RETURN {|| EVAL(bBlq),IF(cType="C", oCursor:FieldGet( nField ),cValToChar( oCursor:FieldGet( nField ))) }

/*
// Devuelve el Número del Objeto GetDpLbx
*/
FUNCTION GetDpLbx(nNumLbx)
  LOCAL nPos

  IF nNumLbx=NIL // Envia todos los LBX
     RETURN aDpLbx
  ENDIF

  nPos:=aScan( aDpLbx, { |a,i|  ValType(a)=="O" .AND. a:nNumLbx == nNumLbx} )

RETURN IIF( nPos>0 , aDpLbx[nPos] , NIL )

/*
// Genera el BloqueCodigo para evaluar y Ejecutar las funcione de cada Boton
*/
FUNCTION GenBlqAction(oDpLbx,I,nOption,lRun)
RETURN {|| oDp:oLbx:=oDpLbx,oDpLbx:RunAction(I,nOption,lRun)}

FUNCTION KILLFIND(oBrw,nCol)
  LOCAL I,uValue,nRowSel:=oBrw:nRowSel,nAt:=oBrw:nArrayAt

  DEFAULT nCol:=1


  // JN 11/04/2016 No realiza Búsqueda
/*
  FOR I=1 TO LEN(oBrw:aCols)

     IF !oBrw:aCols[I]:oEditGet=NIL
         uValue:=CTOEMPTY(EVAL(oBrw:aCols[I]:oEditGet:bSetGet))
         oBrw:aCols[I]:oEditGet:VarPut(uValue)
     ENDIF

  NEXT I
*/
//  AEVAL(oBrw:aCols,{|a,i|a:nEditType:=0,a:bOnPostEdit:=NIL})
//  oBrw:SelectCol( nCol ) // JN 11/04/2016 (Blanque las Columnas)
//  oBrw:ColAtPos( nCol )

  // 01/09/2018
  AEVAL(oBrw:aCols,{|a,i|a:bOnPostEdit:={||NIL}})

  oBrw:CancelEdit()
  oBrw:nRowSel:=nRowSel
  oBrw:nArrayAt:=nAt
  AEVAL(oBrw:aCols,{|a,i|a:nEditType:=0})


RETURN NIL

/*
// Genera el ListBox desde DPFILES Cuando este no Existe
*/
STATIC FUNCTION BuildLbx(cFileLbx)
   LOCAL cFile,oTable,cMemo,cSql

   cFile :=cFileName(cFileLbx)
   cSql  :="SELECT FIL_TEXTO FROM DPFILES WHERE FIL_FILE"+GetWhere("=",cFile)
   oTable:=OpenTable(cSql,.T.)
   cMemo :=ALLTRIM(oTable:FIL_TEXTO)
   cMemo :=STRSQLOFF(cMemo)
   oTable:End()

   MemoWrit(cFileLbx,cMemo)

RETURN !EMPTY(cMemo)

/*
// Genera el ListBox desde DPFILES Cuando este no Existe
*/
METHOD PRINT(cCodRep,cWhere)

   DEFAULT cCodRep:=cFileNoPath(cFileNoExt(::cFileLbx))

   DEFAULT cWhere:=::cWhere

   IF !ISREPORTE(cCodRep)
      EJECUTAR("LBXTOREPORT",Self,::cFileLbx,cCodRep)
   ENDIF

RETURN REPORTE(cCodRep,NIL,cWhere)

/*
// Cierra Todos los DPLBX
*/
Function CloseAllDpLbx()
   LOCAL I

   WHILE !EMPTY(aDpLbx)

     IF ValType(ATAIL(aDpLbx):oWnd)="O"
        // aDpLbx[Len(aDpLbx)]:oWnd:End()
        ATAIL(aDpLbx):oWnd:End()
     ELSE
        ARREDUCE(aDpLbx,Len(aDpLbx))
     ENDIF

   ENDDO

   aDpLbx:={}

Return .T.

STATIC FUNCTION LbxGetData(cExp,oCursor)
   cExp:=MacroEje("{|oCursor|"+cExp+"}")
RETURN {||Eval(cExp,oCursor)}

FUNCTION LbxRelease()
    aMemos:={}
RETURN NIL

STATIC FUNCTION GenHeadBlock(oLbx,nFor)
RETURN {|| oLbx:SetOrder(nFor) }

/*
// Color Indicado en un campo
*/
FUNCTION BRWCOLSETCOLORFIELD(oLbx,nCol,nClrPane1,nClrPane2)
  // oDp:oFrameDp:SetText("COLORFIELD "+LSTR(nClrPane1)+","+LSTR(nClrPane2))
RETURN {|| {oLbx:oCursor:FieldGet(nCol), iif( oLbx:oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }

/*
// Devuelve la Direccion del Archivo *.BMP, JN 06/04/2020
*/
FUNCTION DPBMP(cFile)

  cFile:=oDp:cPathBitMaps+cFile

  IF !Empty(oDp:cFileToScr)

     DEFAULT oDp:oFileBmp:=TFile():New("temp\filebmp.txt")

     oDp:oFileBmp:AppStr( cFile+CRLF )

     SysRefresh(.T.)

  ENDIF

RETURN cFile


STATIC FUNCTION LBXCOLSETCOLORDEF(oCursor,oBrw,nField,cTable,cField,nClrPane1,nClrPane2)
  LOCAL nClrText:=0
  // oDp:oFrameDp:SetText("LXCOLSETDEF "+LSTR(nClrPane1)+","+LSTR(nClrPane2))

RETURN {|| {nClrText:=LBXGETCOLOR_OPC(oCursor,nField,cTable,cField,oBrw), iif( oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }

STATIC FUNCTION LBXGETCOLOR_OPC(oCursor,nField,cTable,cField,oBrw)
 // LOCAL uValue:=oCursor:FieldGet( nField )
 // LOCAL cSay    :=SAYOPTIONS(cTable,cField,uValue)

   LOCAL uValue:=oCursor:FieldGet( nField )
   LOCAL cSay    :=SAYOPTIONS(cTable,cField,uValue)

//   DEFAULT bBlq:={||.T.}
/*
   oDp:nClrOptions:=0

   IF oBrw:nArrayAt>0 .AND. oBrw:nArrayAt>=LEN(oBrw:aArrayData) .AND. LEN(oBrw:aArrayData[oBrw:nArrayAt])>=nField

     uValue:=oBrw:aArrayData[oBrw:nArrayAt,nField]
     cSay  :=SAYOPTIONS(cTable,cField,uValue)

   ENDIF
*/
  // nClrText:=oDp:nClrOptions
RETURN oDp:nClrOptions


/*
// Totaliza un Array
*/
STATIC FUNCTION ATOTALES(aData,bBlq,nColIf)
   LOCAL aTotal,I,aNum:={},U,bColIf:={||.T. }

   DEFAULT bBlq  :={||.T.},;
           nColIf:=0

   IF nColIf>0
      bColIf:={|n| aData[n,nColIf] }
   ENDIF

   IF Empty(aData) .OR. ValType(aData[1])<>"A"
       RETURN {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
   ENDIF

   aTotal:=ARRAY(LEN(aData[1]))

   Aeval( aTotal,{ |a,n| aTotal[n]:=0 , AADD(aNum , ValType(aData[1,n]) ="N" ) })

   FOR I=1 TO LEN(aData)

     // Evalua si la Columna cumple Condiciï¿½n para Totalizar
     IF EVAL(bColIf,I,aData[I])

       FOR U=1 TO LEN(aData[I])

         // IF aNum[U] .AND. Eval(bBlq,aData[I],U)
         IF Eval(bBlq,aData[I],U) .AND. ValType(aData[I,U])="N" .AND.  LEN(aTotal)>=U
            aTotal[U]:=aTotal[U]+aData[I,U]
         ENDIF

       NEXT U

     ENDIF

   NEXT I

RETURN aTotal

/*
// CREAR LA SUBCONSULTA
*/
FUNCTION BUILDSUBC(cSubC,cCol)

   IF Empty(cSubC)
      RETURN .F.
   ENDIF

   cSubC:=CRLF+"("+cSubC+" LIMIT 1) "

   // ? cSubC,"BUILDSUBC"
   // cSubC:=CRLF+"("+cSubC+" LIMIT 1) AS  "+cCol+CRLF

RETURN cSubC

/*
// JN 28/08/2014
STATIC FUNCTION GETLBXRUN(cDir,cFile,cOut)

  LOCAL nLen:=0,nAt1,nAt2,cIni,cFin,cMemo:=""

  DEFAULT cDir :="FORMS\",;
          cFile:="DPVENDEDOR.LBX",;
          cOut :="FORMS\DPLBX.RUN"

  cFile:=cFileName(cFile)+"."+cFileExt(cFile)

  DEFAULT oDp:cMemoRun:=NIL

  IF Empty(oDp:cMemoRun)
     oDp:cMemoRun:=MEMOREAD(cOut)
     nLen        :=LEN(oDp:cMemoRun)
     oDp:cMemoRun:=HB_UNCOMPRESS(1024*1024,oDp:cMemoRun,nLen)
  ENDIF

  IF Valtype(oDp:cMemoRun)<>"C"
     MensajeErr("No es posible leer "+cOut)
     RETURN ""
  ENDIF

  cIni:="["+UPPE(cFile)+"]"
  cFin:="[EOF:"+UPPE(cFile)+"]"

  nAt1 :=AT(cIni,oDp:cMemoRun)
  nAt2 :=AT(cFin,oDp:cMemoRun)
  cMemo:=SUBS(oDp:cMemoRun,nAt1,nAt2-nAt1)
  cMemo:=STRTRAN(cMemo,cIni,"")
  cMemo:=STRTRAN(cMemo,cFin,"")
  cMemo:=ENCRIPT(cMemo,.F.)

RETURN cMemo
//         select * from dpcampos where cam_numtab='2115' order by cam_numtab
*/
