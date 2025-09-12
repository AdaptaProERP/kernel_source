/*
// Generador de Informes [Declarativo]
*/

#INCLUDE "\DPWIN32\INCLUDE\DPXBASE.CH"
#INCLUDE "\DPWIN32\INCLUDE\RichEdit.ch"

MEMVAR oDp

REQUEST TREPSELECT,FRENAME // Seleccionar Reporte

REQUEST DpRptBegin,DpRptAddGroup,DpRptAddColumn

/*
 * Tnomina
*/
CLASS TGENREP FROM TDpClass

   DATA cCodigo
   DATA cScript
   DATA cCodTab
   DATA cScope       // Filtro enviado por Parámetros
   DATA cNameRep     // Nombre del Reporte o Query
   DATA cFileIni
   DATA cMemo
   DATA cParameter   // Parámetros del Reporte
   DATA cSql
   DATA cSelect
   DATA cSqlWhere
   DATA cSqlOrderBy
   DATA cSqlInnerJoin
   DATA cSqlGroupBy
   DATA cInnerJoin
   DATA cGroupBy
   DATA cCodigoOld   // Número Anterior
   DATA cOrderRupt   // Ordenado por Rupturas de Control
   DATA cCodOrg      // Código Original de Donde será Importada
   DATA cHelp
   DATA cFileChm
   DATA cTitle       // Titulo Recibido como parámetro para Imprimir
   DATA cSqlRun
   DATA cPrgBrowse   // Nombre del Programa Browse que ejecutara el Browse

   DATA REP_CODIGO
   DATA REP_DESCRI
   DATA REP_TABLA
   DATA REP_APLICA
   DATA REP_GRUPO
   DATA REP_FECHA
   DATA REP_HORA
   DATA REP_CODADD


   DATA oTable                             // Aqui Se Genera la Prenómina
   DATA oREP_CODIGO
   DATA oREP_DESCRI
   DATA oREP_TABLA
   DATA oREP_APLICA
   DATA oREP_GRUPO
   DATA oBtnLink
   DATA oMemo
   DATA oRango
   DATA oCriterio
   DATA oRun

   DATA aLinks
   DATA aAplica
   DATA aAddOn
   DATA aTablas                           // Lista de las Tablas
   DATA aBtn
   DATA aSelect                           // Campos Seleccionados
   DATA aCols                             // Lista de Columnas
   DATA aOrderBy                          // Ordenado por
   DATA aRango                            // Valor Rango
   DATA aCriterio                         // Criterio
   DATA aRuptura                          // Rupturas o Grupos
   DATA aFilesRpt                         // Lista de Archivos Rpt
   DATA aSet
   DATA aTitle
   DATA aCargo                            // Otros Valores
   DATA aDevice // necesario para reejecutar reporte 20/2/2025
   // DATA aRgo                           // Columnas del Rango

   DATA nOption    // Duplicar
   DATA nNumLbx
   DATA nFijar     // Modo de Fijar 1=POR PC 2, POR USUARIO 3=TODOS  , 4 Ninguno
   DATA nOut INIT 1

   DATA oDpEdit
   DATA oTabDescri
   DATA oGruDescri
   DATA oWnd
   DATA oRepRun
   DATA oFrmRun // 26/06/2016 Objeto Formulario de Ejecución

   DATA lTable // La tabla esta Ok
   DATA lGroupBy
   DATA lSum
   DATA lSave
   DATA lAppend
   DATA lLinks   // Indica si Tiene Enlaces
   DATA lDescend // Indica si Order By es DESCEN
   DATA lMenu
   DATA lQuery INIT .F. // Clase TCQUERY
   DATA lBrowse INIT .F.
   DATA lCrystalDesign INIT .F.
   DATA lOnlyPreview INIT .F. // solo previsualización para uso fiscal

   DATA lPdfFile INIT .F. // Salida PDF Archivo
   DATA lPdfView INIT .F. // Salida PDF View

   DATA bPostPrint // Ejecuta luego de Imprimir por Crystal

   // Dispositivos de Salida
   DATA  lPreview
   DATA  lPrinter
   DATA  lVentana
   DATA  lTxtWnd
   DATA  lExcel
   DATA  lDbf
   DATA  lHtml
   DATA  lCrystalP
   DATA  lCrystalW

   DATA  bPostRun // Ejecución post-emisión por impresora

   DATA uValue1,uValue2,uValue3,uValue4

   METHOD New() CONSTRUCTOR
   METHOD Load()
   METHOD IniReport()
   METHOD EditReport()
   METHOD EditHead()
   METHOD Links()    INLINE TREPLINK():New(Self)

   METHOD SETCREXPORT() INLINE EJECUTAR("REPFRMRUNSETRPTPDF",::oFrmRun)

                     // INLINE EJECUTAR("REPLINKS",SELF) // Esta en DPGENREP.HRB
   METHOD Close()
   METHOD Select()    INLINE TREPSELECT():New(Self) // EJECUTAR("REPSELECT",SELF)
   METHOD Save()
   METHOD OrderBy()
   METHOD Rango()                INLINE TREPRGO():New(Self)
   METHOD Criterio()             INLINE TREPCRI():New(Self)
   METHOD Ruptura(lAuto)         INLINE TREPRUP():New(Self,lAuto)
   METHOD OutPut(lBegin,oCursor) INLINE (oDp:lResp:=.F.     ,;
                                         ::nOut:=::oRun:nOut,;
                                         oDp:lResp:=EJECUTAR("REPOUTPUT",lBegin,::oRun,oCursor),;
                                         ::oRun:nOut:=::nOut,;
                                         oDp:lResp)

   METHOD SetRango(nPos,uValue,uHasta,lRefresh)
   METHOD SetCriterio(nPos,uValue,cOper,cLogico)

   METHOD Cols()

   METHOD ClassName()   INLINE "TGENREP"
   METHOD Write()       INLINE EJECUTAR("REPWRITE"      ,Self)
   METHOD Read()        INLINE EJECUTAR("REPREAD"       ,Self)
   METHOD BuildPrg()    INLINE EJECUTAR("REPBUILDPRG"   ,Self)
   METHOD BuildSql()    INLINE EJECUTAR("REPBUILDSQL"   ,Self)
   METHOD BuildWhere()  INLINE EJECUTAR("REPBUILDWHERE" ,Self)
   METHOD GetSql()      INLINE EJECUTAR("REPGETSQL"     ,Self)
   METHOD PutBar()
   METHOD Compila()      INLINE EJECUTAR("REPCOMPILA"    ,Self)
   METHOD Delete()       INLINE EJECUTAR("REPDELETE"     ,Self)
   METHOD Depura()       INLINE EJECUTAR("REPDEPURA"     ,Self)
   METHOD RunWnd(cTitle) INLINE ::oRun:=TREPRUN():New(Self)
   METHOD Run()          INLINE EJECUTAR("REPRUN",Self)
   METHOD PutPar(cVarName,uValue)
   METHOD GetPar(cVarName,uValue)
   METHOD GetPosParam(cVarName)
   METHOD BarPutFont(cVarName)
   METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)
   METHOD Set( cName, xValue )
   METHOD SetDevice(oReport) INLINE EJECUTAR("REPDEVICE",oReport,::oRun)

   METHOD GetTablas()

   METHOD HandleEvent( nMsg, nWParam, nLParam ) INLINE 1=1

   //EXTERN ;
   //                       WndHandleEvent( Self, nMsg, nWParam, nLParam )

   METHOD End()

   ERROR HANDLER OnError( cMsg, nError )

    // ERROR HANDLER OnError( cMsg,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )
   // ERROR HANDLER OnError( cMsg,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )

ENDCLASS

METHOD New(cCodigo) CLASS TGENREP

   ::Super:New()


   oDp:cIdTras:="GENREP"

   HRBLOAD("DPGENREP.HRB")

   // COMPILA("REPLINKS,REPPUTBAR,REPSELECT")

   ::cScript   :="GENREP"            // "NOMINAINI"
   ::oScript   :=XCOMPILA(::cScript) // Programa Xbase
   ::REP_CODIGO:=cCodigo
   ::REP_GRUPO :=SPACE(08)
   ::REP_FECHA :=DATE()
   ::REP_HORA  :=TIME()

   ::oScript   :=NIL // busca la función en el Script

   ::aLinks    :={}
   ::aSelect   :={}                         // Campos Seleccionados
   ::aCols     :={}
   ::aOrderBy  :={}
   ::aRuptura  :={}
   ::aFilesRpt :={}
   ::aSet      :={}
   ::aTitle    :={}  // Titulos de Rango y Criterio
   ::lMenu     :=.F.

   ::nOption   :=0
   ::lTable    :=.F.
   ::lGroupBy  :=.F.
   ::lSum      :=.F.
   ::aBtn      :=Array(10)
   ::lDescend  :=.F.
   // ::aLinks   :={}

   ::aTablas  :={}
   ::aRango   :={}                      // Valor Rango
   ::aCriterio:={}                      // Criterio

   ::cNameRep :="REPORTE"
   ::cMemo    :=""
   ::lAppend  :=.F.
   ::lSave    :=.F. // no ha Grabado
   ::lLinks   :=.T.
   ::nNumLbx  :=oDp:nNumLbx    // asociación entre el Editor y el ListBox
   ::dFecha   :=oDp:dFecha

   ::cScript   :="DPREPORTES"
   ::oScript   :=XCOMPILA(::cScript) // Programa Xbase

   // Comandos Sql
   ::cSelect      :=""
   ::cWhere    :=""
   ::cOrderBy  :=""
   ::cInnerJoin:=""
   ::cGroupBy  :=""

   ::cSqlWhere     :=""
   ::cSqlOrderBy   :=""
   ::cSqlInnerJoin :=""
   ::cSqlGroupBy   :=""

   // Modo de Fijar
   ::nFijar        :=3

   // Dispositivos de Salida
   ::lPreview      :=.T.
   ::lPrinter      :=.T.
   ::lVentana      :=.T.
   ::lTxtWnd       :=.T.
   ::lExcel        :=.F.
   ::lDbf          :=.T.
   ::lHtml         :=.F.
   ::lCrystalP     :=.T.
   ::lCrystalW     :=.T.

   IF !Empty(::REP_CODIGO)

   ENDIF

RETURN SELF

/*
// Asigna Parámetros
*/
METHOD GetPosParam(cVarName) CLASS TGENREP
   DEFAULT ::aSet:={}
//   cVarName:=ALLTRIM(UPPE(cVarName))
RETURN ASCAN(::aSet,{|a|a[1]==cVarName})


 /*
 *  TGENREP:Set()
 */
METHOD Set( cName, xValue ) CLASS TGENREP             // [by ER]

   IF oSend(Self,cName)=NIL
      __objAddData( Self, cName )
   ENDIF

   oSend(Self,cName,xValue)

RETURN Self

/*
// Reestablece FONT de las Barras
*/
METHOD BarPutFont() CLASS TGENREP

   LOCAL oFont

   IF ValType(::oWnd:oBar)="O"

     DEFINE FONT oFont     NAME "Arial"   SIZE 0, -10

     AEVAL(::oWnd:oBar:aControls,{|o,i|o:SetFont(oFont),o:Refresh()})

     oFont:End()

   ENDIF

   STORE NIL TO oFont

RETURN .T.


/*
// Asigna Parámetros
*/
METHOD PutPar(cVarName,uValue) CLASS TGENREP
   LOCAL nAt

   cVarName:=UPPE(ALLTRIM(cVarName))
   nAt     :=::GetPosParam(cVarName)

   IF nAt=0
      AADD(::aSet,{cVarName,uValue})
   ENDIF

RETURN uValue

METHOD GetPar(cVarName,uValue ) CLASS TGENREP
   LOCAL nAt

   cVarName:=UPPE(ALLTRIM(cVarName))
   nAt     :=::GetPosParam(cVarName)

   IF nAt>0
      uValue:=::aSet[nAt,2]
   ENDIF

RETURN uValue


/*
// Carga los Valores del Informe
*/
METHOD Load() CLASS TGENREP
RETURN NIL

/*
// Carga los Valores del Informe
*/
METHOD Close() CLASS TGENREP

    IF !::lSave .AND. MsgYesNo("Desea Grabar","Grabar Reporte "+::REP_CODIGO)

       ::Save()
       ::oWnd:End()

    ENDI

    IF !::lSave .AND. MsgNoYes("Desea Cancelar","Reporte "+::REP_CODIGO)

      IF ::nOption=1
        ::Delete()
//        Ferase(::cFileIni)
//        Ferase(StrTran(::cFileIni,".REP",".SCR"))
//        Ferase(StrTran(::cFileIni,".REP",".DXB"))
      ENDIF

      ::oWnd:End()

    ELSEIF ::lSave

      ::oWnd:End()

    ENDIF

RETURN NIL

/*
// Grabar Informe
*/
METHOD Save() CLASS TGENREP
   LOCAL oTable,cWhere,oDpLbx,lAppend:=.F.

   ::REP_HORA :=TIME()
   ::REP_FECHA:=oDp:dFecha
   ::dFecha   :=oDp:dFecha // Fecha para Grabar
   ::cMemo    :=::oMemo:GetText()
   ::Write()

   // ? ::cMemo,"MEMO"
   oDp:lBuildLimitSql:=.F.

   oTable:=OpenTable("SELECT * FROM DPREPORTES WHERE REP_CODIGO"+GetWhere("=",::cCodigoOld),.T.)

   IF oTable:RecCount()=0
      cWhere:=""
      oTable:Append()
      lAppend:=.T.
   ELSE
      cWhere:=GetSqlWhere(oTable:cSql)
   ENDIF

   oTable:Replace("REP_CODIGO",::REP_CODIGO)
   oTable:Replace("REP_DESCRI",::REP_DESCRI)
   oTable:Replace("REP_TABLA" ,::REP_TABLA )
   oTable:Replace("REP_APLICA",::REP_APLICA)
   oTable:Replace("REP_GRUPO" ,::REP_GRUPO )
   oTable:Replace("REP_FECHA" ,::REP_FECHA )
   oTable:Replace("REP_ALTERA",.T.         )
   oTable:Replace("REP_FUENTE",::cMemo     )
   oTable:Replace("REP_PARAM" ,::cParameter)
   oTable:Replace("REP_CODADD",::REP_CODADD)

   IF oTable:IsDef("REP_HORA")
      oTable:Replace("REP_HORA",::REP_HORA)
   ENDIF

   oTable:Commit(cWhere)

   FERASE(::cFileIni)
   FERASE(STRTRAN(UPPE(::cFileIni),".REP",".DXB"))

/*   IF ::cCodigoOld!=::REP_CODIGO
      MsgAlert("DEBE BORRAR LOS CODIGOS ANTERIORES")
      FERASE()
   ENDIF
*/
   ::cCodigoOld:=::REP_CODIGO
   ::lSave:=.T.

   oTable:End()

   oDp:lBuildLimitSql:=.T.

   oDpLbx:=GetDpLbx(::nNumLbx)

   IF ValType(oDpLbx)="O"
      IF oDpLbx:oCursor:RecCount()=0 // Ya Tiene el Primer Registro
         AEVAL(oDpLbx:aBtns,{|o|o[8]:Enable()})
      ENDIF
      oDpLbx:Refresh(lAppend,::REP_CODIGO)
   ENDIF

//   ? ::cParameter,"PARAMETROS"

RETURN NIL

// Carga los Valores del Informe
// lRun: Cuando Corre no necesita cargar todo, la info esta en los .REP
*/
METHOD IniReport(nOption,cCodigo,lRun) CLASS TGENREP
   LOCAL cFileScr,lSave:=.F.
   LOCAL oTable
   LOCAL bInit

   DEFAULT nOption:=1,cCodigo:="",lRun:=.f.

   IF nOption=1
      cCodigo:=""
   ENDIF

   ::nOption   :=nOption
   ::cMemo     :=""
   ::cParameter:=""

//   ? ErrorSys(.t.),lRun

   IF lRun

//      oTable:=OpenTable("SELECT REP_CODIGO,REP_DESCRI,REP_HORA,REP_FECHA FROM DPREPORTES "+;
//                        "WHERE REP_CODIGO"+GetWhere("=",cCodigo),.T.)

      oTable:=OpenTable("SELECT REP_CODIGO,REP_DESCRI,REP_TABLA,REP_APLICA,REP_GRUPO,REP_HORA,REP_FECHA FROM DPREPORTES "+;
                     "WHERE REP_CODIGO"+GetWhere("=",cCodigo),.T.)

   ELSE

      oTable:=OpenTable("SELECT REP_CODIGO,REP_DESCRI,REP_TABLA,REP_APLICA,REP_FUENTE,REP_PARAM,REP_GRUPO,REP_HORA,REP_FECHA FROM DPREPORTES "+;
                     "WHERE REP_CODIGO"+GetWhere("=",cCodigo),.T.)

   ENDIF

   //oTable:Browse()

   ::nOption:=nOption

   ::REP_CODIGO:=oTable:REP_CODIGO
   ::REP_DESCRI:=oTable:REP_DESCRI

  // IF !lRun
     ::REP_TABLA :=oTable:REP_TABLA
     ::REP_APLICA:=oTable:REP_APLICA
     ::REP_GRUPO :=oTable:REP_GRUPO
 //  ENDIF

   ::REP_FECHA :=oTable:REP_FECHA
   ::REP_HORA  :=TIME()

//   MensajeErr(::REP_HORA ,"HORA")
//   MensajeErr(::REP_FECHA,"HORA")

   IF oTable:FieldPos("REP_HORA")>0
     ::REP_HORA  :=oTable:REP_HORA
    //   ? "SI EXISTE, REP:HORA"
   ENDIF

   ::cFileIni:="Report\"+ALLTRIM(::REP_CODIGO)+".REP"
   ::cCodigoOld:=::REP_CODIGO

   IF !lRun

     ::cMemo     :=STRSQLOFF(oTable:REP_FUENTE)
     ::cParameter:=STRSQLOFF(oTable:REP_PARAM )  // Parámetros

     cFileScr:=STRTRAN(::cFileIni,".REP",".SCR")

     /* Recupera desde el Disco */
//     ? FILE(::cFileIni),::cFileIni,::cParameter

     IF Empty(::cParameter) .AND. FILE(::cFileIni)
        ::cParameter:=STRSQLOFF(MemoRead(::cFileIni))
        lSave:=.T.
   //     ? ::cParameter,"ESTE ES"
     ENDIF

     IF Empty(::cMemo) .AND. FILE(cFileScr)
        ::cMemo:=MemoRead(cFileScr)
        lSave:=.T.
     ENDIF

   ENDIF

   IF ::nOption=1
      ::cMemo      :=""
      ::cParameters:=""
      ::REP_APLICA :="  "
   ENDIF

   IF lSave
      oTable:Replace("REP_FUENTE",::cMemo)
      oTable:Replace("REP_PARAM" ,::cParameter)
      oTable:Commit(oTable:cWhere)
   ENDIF

   oTable:End()

   IF nOption=4 .OR. nOption=0 // Delete y Run
      IIF( nOption=0,::Read() , NIL ) // Para Ejecutar Requiere Lectura
      RETURN NIL
   ENDIF

   // Si la Tabla Tiene Enlaces, Envia a Links
   bInit:={||::Links()}

   ::EditReport( IIF( ::nOption=1 .OR. ::nOption=5, {||::EditHead(bInit)} , {||.T.}) )

RETURN NIL
/*
// Asignación de Rango
*/
METHOD SetRango(nPos,uDesde,uHasta,lRefresh) CLASS TGENREP
   LOCAL nAt:=nPos

   IF ::oRepRun=NIL
      RETURN .F.
   ENDIF

   DEFAULT lRefresh:=.F.

// ? LEN(::oRepRun:aRango)," aRangos "

   ::oRepRun:aRango[nPos,2]:=uDesde
   ::oRepRun:oBrwR:aArrayData[nPos,2]:=uDesde

   IF !uHasta=NIL
      ::oRepRun:aRango[nPos,3]:=uHasta
   ENDIF

   IF lRefresh
     ::oRepRun:oBrwR:Refresh(.T.)
   ENDIF

RETURN .T.
/*
// Set Criterio
*/
METHOD SetCriterio(nPos,uValue,cOper,cLogico) CLASS TGENREP
   LOCAL nAt:=nPos

   IF ::oRepRun=NIL .OR. nPos>LEN(::oRepRun:aCriterio)
      RETURN .F.
   ENDIF

   ::oRepRun:aCriterio[nPos,3]       :=uValue
   ::oRepRun:oBrwC:aArrayData[nPos,3]:=uValue

   IF !Empty(cOper)
      ::oRepRun:aCriterio[nPos,2]:=cOper
   ENDIF

   IF !Empty(cLogico)
      ::oRepRun:aCriterio[nPos,4]:=cLogico
   ENDIF

   ::oRepRun:oBrwC:DrawLine(.T.) // Refresh(.T.)

RETURN NIL

/*
// Edición del Informe
*/
METHOD EditReport(bInit) CLASS TGENREP

     LOCAL oWnd,oGenRep:=Self // ,cMemo:=""
     LOCAL oRow,oCol,oLen,oIco,oBar,lResp,oFont,oFontBtn,nClrText:=CLR_BLACK
//   LOCAL aLinks,I
//   LOCAL oLink


     oDp:cFileChm  :=GetFileChm("DPREPORTES")
     oDp:cHelpTopic:="EDITOR"

     IF Empty(oDp:cModeVideo)

       DEFINE FONT oFont NAME "Courier New" BOLD SIZE 0,-14

     ELSE

       DEFINE FONT oFont NAME "Courier New" BOLD SIZE 0,-16

     ENDIF

     DEFINE FONT oFontBtn NAME "Arial"   SIZE 0, -10

     ::cFileIni:="Report\"+ALLTRIM(::REP_CODIGO)+".REP"

     IF ::nOption!=1 .OR. ::nOption=5
       ::Read()
       ::lLinks:=nGetLinks(ALLTRIM(::REP_TABLA))>0
     ELSE
       ::cMemo:=""
     ENDIF

//     IF nOption=5
//       ::REP_CODIGO:=CTOEMPTY(::REP_CODIGO)
//     ENDIF

     IF Empty(oDp:hDllRtf) // Carga RTF
       oDp:hDLLRtf := LoadLibrary( "Riched20.dll" )
     ENDIF

     IF !oDp:IsDef("RtfcFind") // Inicio de búsqueda del RTF
       oDp:RTFcFind:=SPACE(50) // Carga RTF
       oDp:RTFcRepl:=SPACE(50) // Reemplazar
       oDp:RTFnDir :=1
       oDp:RTFlCase:=.F.
       oDp:RTFlWord:=.T.
     ENDIF

     IF ::nOption=5
       ::REP_CODIGO:=CTOEMPTY(::REP_CODIGO)
       ::cCodigoOld:=""
     ENDIF

//   ErrorSys(.T.)

     DEFINE WINDOW oWnd MDICHILD;
           TITLE IIF( ::nOption=1 .OR. ::nOption=5 ,"Nuevo Diseño de Reporte ","Modificar Reporte "+::cCodigoOld);
           OF DpWndMain() PIXEL

     DEFINE ICON oIco RESNAME "ICON"

     SET MESSAGE OF oWnd TO "Generador de Reportes"

     ::oWnd:=oWnd

     DEFINE MSGITEM oLen PROMPT "0 bytes" SIZE 80 OF oWnd:oMsgBar ;
            TOOLTIP "Size of the file"

     DEFINE MSGITEM oRow PROMPT "Lin: 0" SIZE 60 OF oWnd:oMsgBar ;
            TOOLTIP "Line"

     DEFINE MSGITEM oCol PROMPT "Col: 0" SIZE 60 OF oWnd:oMsgBar ;
            TOOLTIP "Column"

     oWnd:SetIcon(oIco)

// JN 01/05/2017, No compatible con XHB
     IF !oDp:lMenuXp .AND. .F.

       // @ 0,0 GET ::oMemo Var ::cMemo FONT oFont HSCROLL FONT oFontB MULTILINE

     ELSE

      IF ::nOption=2 // .OR. !ISGENREP() .OR. ISDEMO()

         @ 0,0 RICHEDIT  ::oMemo VAR ::cMemo OF oWnd HSCROLL READONLY

         nClrText:=CLR_GRAY

      ELSE

         @ 0,0 RICHEDIT  ::oMemo VAR ::cMemo OF oWnd HSCROLL // NOBORDER
//      ::oMemo:SetColor(0,14087148)

      ENDIF

     ENDIF

     ::oMemo:SetFont(oFont)


     ::oMemo:bChange := { || oLen:SetText( Transform( ::oMemo:Len(), "@E 999,999 bytes" ) ), ;
                             oRow:SetText( "Lin: " + LTrim( Str( ::oMemo:GetRow() ) ) ), ;
                             oCol:SetText( "Col: " + LTrim( Str( ::oMemo:GetCol() ) ) ), ;
                             RTFStatus(::oMemo) }

     oWnd:oClient    := ::oMemo

     ::oWnd:=oWnd

    //  ACTIVATE WINDOW oWnd ON INIT (lResp:=oGenRep:EditHead(oWnd),IIF( !lResp,oGenRep:Close(),EJECUTAR("REPPUTBAR",oGenRep)),.F.) // Edita el Encabezado
     ACTIVATE WINDOW oWnd ON INIT (oGenRep:oMemo:SetColor(nClrText,NIL),;
                                   lResp:=EVAL(bInit),IIF( !lResp,oGenRep:Close(),oGenRep:PutBar()),.F.) // Edita el Encabezado
//    oWnd:bGotFocus:={||oGenRep:PutBar(oWnd:oBar)}
      oWnd:bGotFocus:={||oDp:cFileChm :=GetFileChm("DPREPORTES"),oDp:cHelpTopic:="EDITOR"}

      IF !oDp:lMenuXp
        oDp:oFrameDp:Cascade()
     ENDIF


//    BarPutFont(oBar,oFont)}

RETURN NIL

/*
// Edición del Encabezado
*/
METHOD EditHead(bInit) CLASS TGENREP
    LOCAL cSql,oDpLbx,oTable,lCerrar:=.F.,nGrupos:=1,lValGrupo:=.F.
    LOCAL oGenRep:=SELF
    LOCAL oFontB,oBtn,oFont,oFontC
    LOCAL oDlg //, aAplica
    LOCAL lAceptar :=.F.,cTabNombre:=SPACE(40),cGruNombre:=SPACE(40)
    LOCAL lValTable:=!::nOption=1

    ::lMenu    :=::nOption=1 .OR. ::nOption=5 // Solo para Incluir

    nGrupos:=MYCOUNT("DPGRUREP")
    //oTable :=OpenTable("SELECT COUNT(*) FROM DPGRUREP",.T.)
    //nGrupos:=oTable:FieldGet(1)
    //oTable:End()

    DEFAULT bInit:={||.T.}

    DEFINE FONT oFontB NAME "Tahoma"   SIZE 0, -12 BOLD // BOLD ITALIC
    DEFINE FONT oFont  NAME "Tahoma"   SIZE 0, -12 // BOLD ITALIC

    DEFINE FONT oFontC NAME "Courier New"   SIZE 0, -12  BOLD  // ITALIC

    IF !Empty(::REP_TABLA)

       oTable:=OpenTable("SELECT TAB_DESCRI FROM DPTABLAS WHERE TAB_NOMBRE"+;
               GetWhere("=",::REP_TABLA),.T.)

       cTabNombre:=oTable:TAB_DESCRI

       oTable:End()

    ENDIF

    IF !Empty(::REP_GRUPO)

       oTable:=OpenTable("SELECT GRR_DESCRI FROM DPGRUREP WHERE GRR_CODIGO"+;
               GetWhere("=",::REP_GRUPO),.T.)

       cGruNombre:=oTable:GRR_DESCRI

       oTable:End()

    ENDIF

/*
    IF ::nOption=1 // Incluir
       // cSql     :=[SELECT * FROM DPGENREP WHERE ]+BuildConcat("INF_CODIGO")+GetWhere("=",cCodigo)+[]
    ELSE

    ENDIF
*/
    ::aAplica:=aTable("SELECT MNU_MODULO,MNU_TITULO FROM DPMENU  WHERE MNU_VERTIC='A'",.T.)
    ::aAddOn :=aTable("SELECT ADD_CODIGO,ADD_DESCRI FROM DPADDON WHERE ADD_ACTIVO=1",.T.)

//  ::oDpEdit:=DPEDIT():New("Generador de Informes","forms\GENREP.EDT","oGenRep",.T.)
//  ::oDpEdit:ViewTable("DPTABLAS","TAB_DESCRI","TAB_NUMERO","REP_NUMTAB")

    oDp:cFileChm  :=GetFileChm("DPREPORTES")
    oDp:cHelpTopic:="CREAR"

    DEFINE DIALOG oDlg TITLE "Identificación del Informe " FROM 0,0 TO 18+4,74 OF ::oWnd;
           COLOR CLR_BLACK,oDp:nGris2  FONT oFont

    @ 1,.5 GET    oGenRep:oREP_CODIGO VAR oGenRep:REP_CODIGO;
                  PICT "@!";
                  FONT oFont;
                  SIZE 60,NIL;
                  VALID VALGENREP(oGenRep:REP_CODIGO)

    oGenRep:cToolTip:="Código que Indentifica al Reporte"

    @ oGenRep:oREP_CODIGO:nTop-9,oGenRep:oREP_CODIGO:nLeft SAY "Código:" PIXEL SIZE 40,08 ;
                   FONT oFontB;
                   COLOR CLR_BLACK,oDp:nGris2

    @ 3,.5 GET    oGenRep:oREP_DESCRI VAR oGenRep:REP_DESCRI;
                  FONT oFont

    @ oGenRep:oREP_DESCRI:nTop-9,oGenRep:oREP_DESCRI:nLeft SAY "Descripción:";
                  PIXEL SIZE 70,08;
                  FONT oFontB;
                  COLOR CLR_BLACK,oDp:nGris2

    @ 5,.5 BMPGET oGenRep:oREP_TABLA  VAR oGenRep:REP_TABLA ;
                  PICT "@!" ;
                  FONT oFont;
                  VALID (lValTable:=VALTABLE(oGenRep:oREP_TABLA,oGenRep:oTABDESCRI,oGenRep),lValTable);
                  NAME "BITMAPS\FIND.BMP";
                  WHEN oGenRep:nOption=1; // Solo Cuando Incluye puede colocar la Tabla
                  ACTION LISTTABLAS(oGenRep:oREP_TABLA)

    @ oGenRep:oREP_TABLA:nTop-8,oGenRep:oREP_TABLA:nLeft SAY "Nombre de Tabla:" PIXEL;
                  SIZE 210,08;
                  FONT oFontB;
                  COLOR CLR_BLACK,oDp:nGris2

    @ oGenRep:oREP_TABLA:nTop,oGenRep:oREP_TABLA:nRight+5 SAY oGenRep:oTABDESCRI PROMPT cTabNombre BORDER PIXEL SIZE 200,12;
                                                          COLOR oDp:nClrLabelText,oDp:nClrLabelPane;
                                                          FONT oFontB

    @ 7.0,.5 BMPGET oGenRep:oREP_GRUPO VAR oGenRep:REP_GRUPO ;
                  FONT oFont;
                  SIZE 50,NIL;
                  VALID (lValGrupo:=VALGRUPO(oGenRep:oREP_GRUPO,oGenRep:oGRUDESCRI,oGenRep),lValGrupo);
                  NAME "BITMAPS\FIND.BMP";
                  WHEN nGrupos>0; // Solo Cuando Incluye puede colocar la Tabla
                  ACTION LISTGRUPOS(oGenRep:oREP_GRUPO,oGenRep)

    @ oGenRep:oREP_GRUPO:nTop,oGenRep:oREP_GRUPO:nRight+5 SAY oGenRep:oGRUDESCRI PROMPT cGruNombre PIXEL SIZE 200,12;
                      COLOR oDp:nClrLabelText,oDp:nClrLabelPane;
                      SIZE 270,NIL;
                      FONT oFontB

    @ oGenRep:oREP_GRUPO:nTop-8,oGenRep:oREP_GRUPO:nLeft SAY "Grupo:" PIXEL;
                      SIZE 210,08;
                      FONT oFontB;
                      COLOR CLR_BLACK,oDp:nGris2

    @ 8.6,.5 COMBOBOX oGenRep:oREP_APLICA VAR oGenRep:REP_APLICA ITEMS ::aAplica;
                      FONT oFont;
                      SIZE 100,08 OF oDlg
                      // COLOR CLR_BLACK,oDp:nGris2

    ComboIni(oGenRep:oREP_APLICA)

    @ oGenRep:oREP_APLICA:nTop-8,oGenRep:oREP_APLICA:nLeft SAY "Aplicación:" PIXEL;
                  SIZE 210,08;
                  FONT oFontB;
                  COLOR CLR_BLACK,oDp:nGris2

    @ 10.6,.5 COMBOBOX oGenRep:oREP_CODADD VAR oGenRep:REP_CODADD ITEMS ::aAddOn;
                      FONT oFont;
                      SIZE 100+60,08 OF oDlg

                 // COLOR CLR_BLACK,oDp:nGris2

    ComboIni(oGenRep:oREP_CODADD )

    @ oGenRep:oREP_CODADD:nTop-8,oGenRep:oREP_CODADD:nLeft SAY "Add-On:" PIXEL;
                  SIZE 210,08;
                  FONT oFontB;
                  COLOR CLR_BLACK,oDp:nGris2


    @ 8.6,.5+15 CHECKBOX ::lMenu PROMPT ANSITOOEM("Agregar en Menú");
               WHEN ::nOption=1;
               FONT oFontB;
               COLOR CLR_BLACK,oDp:nGris2

    @120, 215 BTNBMP oBtn ;
              SIZE 25, 32 ; //  NOBORDER;
              TOP PROMPT "Seguir";
              FONT oFont;
              FILENAME "BITMAPS\XNEXT.BMP";
              WHEN lValTable;
              ACTION (lCerrar:=.T.,lAceptar:=.T.,oDlg:End())

    oBtn:SetColor(CLR_BLUE,oDp:nGris)
    oBtn:lCancel :=.T.
    oBtn:cToolTip:="Continuar"

   @120, 245 BTNBMP oBtn ;
             SIZE 25, 32 ; //  BORDER;
             TOP PROMPT "Cerrar";
             FONT oFont;
             FILENAME "BITMAPS\XSALIR.BMP";
             ACTION (lCerrar:=IIF(oGenRep:nOption=1,MsgNoYes("Desea Abandonar la Creación de un Nuevo Reporte","Cerrar"), .T.),;
                     IIF( lCerrar,(lAceptar:=.F.,oDlg:End()),DpFocus(oGenRep:oREP_CODIGO)))

    oBtn:SetColor(CLR_BLUE,oDp:nGris)
    oBtn:lCancel :=.T.
    oBtn:cToolTip:="Salir"

/*    @ 09,33 BUTTON oGenRep:oBtnLink;
            PROMPT " Enlaces ";
            WHEN oGenRep:lTable;
            ACTION oGenRep:Links()

    oGenRep:oBtnLink:cToolTip:="Relación Entre Tablas"
*/

    ACTIVATE DIALOG oDlg CENTERED ;
       ON INIT .F.;
       VALID lCerrar

//  IF ::nOption=1
    ::lLinks:=LEN(GetLinks(::REP_TABLA))>1 // Indica si la tabla tiene posibles enlaces

    IF !::lLinks // No Hay Enlaces

      IF ::nOption=1
         bInit:={||oGenRep:Select()}
      ENDIF

      IF EMPTY(oGenRep:aLinks)
         AADD(oGenRep:aLinks,{oGenRep:REP_TABLA,GetTableName(oGenRep:REP_TABLA),SPACE(LEN(oGenRep:REP_TABLA)),"Ninguno"}) //GetTableName(aLista[1])})
      ENDIF

    ENDIF

    IF lAceptar // Va para Links

       EVAL(bInit)

    ELSEIF ::nOption=1 .OR. ::nOption=5

      ::lSave:=.T. // Para que End No solicite Nada
      ::oWnd:End()
      ::End()

    ENDIF

   //  (oDlg:Show(),DpFocus(oGenRep:oREP_CODIGO),.F.) // ::oDpEdit:Activate()
   // ::Links()

RETURN lAceptar

/*
// Selecciona las Columnas
*/
METHOD Cols() CLASS TGENREP

   IF EMPTY(::aSelect)
      MsgInfo("Es Necesario Seleccionar Campos","Importante")
      ::Select()
   ENDIF

   IF !EMPTY(::aSelect)
      TREPCOLS():New(Self)   // Selecciona Columnas
   ENDIF

RETURN .T.

/*
// Order by
*/
METHOD OrderBy()   CLASS TGENREP

//   IF !EMPTY(::aSelect)
    oDp:cFileChm  :=GetFileChm("DPREPORTES")
    oDp:cHelpTopic:="ORDERBY"

    TREPORDERBY():New(Self)   // Selecciona Columnas
//   ENDIF

RETURN .T.

/*
// Write, Genera Archivo *.ini

METHOD Write() CLASS TGENREP

   ? "AQUI GRABA EL REPORTE"

RETURN .T.
*/

/*
// Determina la Lista de Tablas, Según Links
*/
METHOD GetTablas() CLASS TGENREP
   LOCAL nAt,I,cTable

   ::aTablas:={}

   AADD(::aTablas,UPPE(ALLTRIM(::REP_TABLA)))

   FOR  I:= 1 TO LEN(::aLinks)

      cTable:=uppe(alltrim(::aLinks[i,1]))

      // ? cTable
      // nAt:=ASCAN(::aTablas,{|a|a==ALLTRIM(::aLinks[I,1])})
      nAt:=ASCAN(::aTablas,{|a|a==cTable})

//    ? cTable,I,"I,1"

      IF nAt=0 .AND. !("NINGUNO"==cTable) //  CALLTRIM(UPPE(::aLinks[I,1]))
         AADD(::aTablas,cTable)
      ENDIF

      cTable:=uppe(alltrim(::aLinks[i,3]))
      nAt:=ASCAN(::aTablas,{|a|a==cTable})

//    ? cTable,I,"I,3",nAt
//    nAt:=ASCAN(::aTablas,{|a|a==ALLTRIM(::aLinks[I,3])})

      IF nAt=0 .AND. !("NINGUNO"==cTable) // ALLTRIM(UPPE(::aLinks[I,3]))
         AADD(::aTablas,cTable)
      ENDIF

   NEXT

//   ? LEN(::aTablas)

RETURN NIL

/*
// Barra y Botones
*/
METHOD PutBar() CLASS TGENREP

   LOCAL oFont,oBtn,I,oBar
   LOCAL oGenRep:=Self

   DEFINE FONT oFont NAME "Arial"   SIZE 0, -10

   DEFINE BUTTONBAR oBar SIZE 52,60 OF oGenRep:oWnd  3D
          //COLOR NIL,oDp:nGris2

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          TOP PROMPT "Datos";
          FILENAME "BITMAPS\VIEW.BMP";
          ACTION oGenRep:EditHead()

   //oBtn:bAction :=BlqParam({|oPar1|oPar1:EditHead()},oGenRep)
   oBtn:cToolTip:="Datos del Informe"

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          TOP PROMPT "Enlace";
          FILENAME "BITMAPS\LINK.BMP","BITMAPS\LINKG.BMP";
          WHEN oGenRep:lLinks ;
          ACTION (oGenRep:Links(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Relación Entre Tablas [INNER JOIN]"
    oBtn:ForWhen()

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Select";
           FILENAME "BITMAPS\QUERY.BMP";
           ACTION (oGenRep:Select(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Campos del Query [SELECT]"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Order By";
           FILENAME "BITMAPS\INDEX.BMP";
           ACTION (oGenRep:OrderBy(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Order By [Query]"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Rango";
           FILENAME "BITMAPS\RANGO.BMP";
           ACTION (oGenRep:Rango(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Rango del Reporte [WHERE LOGICO]"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Criterio";
           FILENAME "BITMAPS\CRITERIO.BMP";
           ACTION (oGenRep:Criterio(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Criterios  [WHERE RELACIONAL]"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           GROUP;
           FONT oFont;
           TOP PROMPT "Grupos ";
           FILENAME "BITMAPS\GROUPBY.BMP";
           ACTION (oGenRep:Ruptura(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Seleccionar Grupos (Rupturas de Control)"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Columnas ";
           ACTION (oGenRep:Cols(),oGenRep:BarPutFont());
           FILENAME "BITMAPS\COLUMNAS.BMP"

    oBtn:cToolTip:="Seleccionar Columnas del Informe"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Crear";
           FILENAME "BITMAPS\PRG.BMP";
           ACTION (oGenRep:BuildPrg(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Generar Código PRG del Reporte"

    // oBtn:bDropOver:={||MsgAlert("LISTO")}

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           GROUP;
           FONT oFont;
           TOP PROMPT "Ejecutar";
           FILENAME "BITMAPS\RUN.BMP";
           ACTION (oGenRep:cMemo:=oGenRep:oMemo:GetText(),;
                   EJECUTAR("REPWRITE",oGenRep    ),;
                   EJECUTAR("REPREAD" ,oGenRep,.T.),;
                   REPORTE(oGenRep:REP_CODIGO))

/*
OJO
           ACTION (oGenRep:cMemo:=oGenRep:oMemo:GetText(),;
                   EJECUTAR("REPWRITE",oGenRep    ),;
                   EJECUTAR("REPREAD" ,oGenRep,.T.),;
                   oGenRep:RunWnd()           ,;
                   oGenRep:BarPutFont())
*/

    oBtn:cToolTip:="Ejecutar Reporte "

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Crytal";
           FILENAME "BITMAPS\CRYSTAL.BMP";
           ACTION (RunMacro("CrpBrowse(oPar1)",Self),oGenRep:BarPutFont())

   oBtn:cToolTip:="Asociar Archivos RPT de Crystal"

   DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Fijar";
           FILENAME "BITMAPS\FIJAR.BMP";
           ACTION (EJECUTAR("REPPARAM",Self))

   oBtn:cToolTip:="Asociar Archivos RPT de Crystal"

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Grabar";
           FILENAME "BITMAPS\XSAVE.BMP";
           ACTION (oGenRep:Save(),oGenRep:BarPutFont())

    oBtn:cToolTip:="Grabar y Continuar "

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFont;
           TOP PROMPT "Salir";
           FILENAME "BITMAPS\XSALIR.BMP";
           ACTION oGenRep:Close()

    oBtn:cToolTip:="Salir del Informe"

    oBar:SetColor(NIL,oDp:nGris2)

    Aeval(oBar:aControls,{|o|o:SetColor(NIL,oDp:nGris2)})

    oGenRep:oWnd:Maximize()

    oFont:End()

//    ::oWnd:bGotFocus:={||BarPutFont(oBar,oFont)}

    STORE NIL TO oFont

RETURN nil

// RETURN oBar

/*
// Finalizar Clase
*/
METHOD End() CLASS TGENREP

   LOCAL I

   IF ValType(::aRango)="A"

      FOR I := 1 TO LEN(::aRango)
         ::aRango[I,6]:End()
         ::aRango[I,6]:=NIL
      NEXT I
   ENDIF

   IF ValType(::aCriterio)="A"
      FOR I := 1 TO LEN(::aCriterio)
         ::aCriterio[I,6]:End()
         ::aCriterio[I,6]:=NIL
      NEXT
   ENDIF

   IIF( ValType(::oRango)   ="O" , ::oRango:End()    , NIL )
   IIF( ValType(::oCriterio)="O" , ::oCriterio:End() , NIL )

   STORE NIL TO ::oRango,::oCriterio,::aRango,::aCriterio

   EJECUTAR("REPORTEND",Self)

RETURN NIL

/*
 *  OnError()
 */
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TGENREP

  LOCAL cMsg   := UPPE(ALLTRIM(__GetMessage()))

  if SubStr( cMsg, 1, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2)
      __objAddData( Self, cMsg )
      oSend(Self,cMsg,uValue)

      RETURN uValue

  ENDIF

RETURN NIL

/*
METHOD OnError(uValue,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError ) CLASS TGENREP

  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N //,oFrameDp
  LOCAL lResp:=.F.

  #ifdef __HARBOUR__  // JN
      cMsg   := __GetMessage()
  #endif

  cMsg := Upper( AllTrim( cMsg ))

  //? cMsg

  if SubStr( cMsg, 1, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2)

      if cMsg == Upper(::cName)
         ::aVars[::nPos,2] := uValue

      elseif ( ::nPos := aScan( ::aVars, { |e,n| e[1] == cMsg } ) ) != 0
         ::cName  := cMsg
         ::aVars[::nPos,2] := uValue // ( 1, 1 )

      else

         if !::lAutomatic	// [WA]
            _ClsSetError( _GenError( nError, ::ClassName(), cMsg ) )
            ::cName  := ""
            ::nPos   := 0
         else
            ::add(cMsg)
            ::aVars[::nPos,2] := uValue //  GetParam( 1, 1 )
         endif
      endif
   else

      if cMsg == Upper(::cName)           // [by ER]

         RETURN ::aVars[::nPos,2]

      elseif ( ::nPos := aScan( ::aVars, { |e,n| e[1] == cMsg } ) ) != 0
         ::cName  := cMsg
         RETURN ::aVars[::nPos,2]

      else

         // Busca si tiene alguna función oScript
         ::cName  := ""
         ::nPos   := 0

         ? "BUSCA FUNCION ",cMsg

      endif
   endif

RETURN NIL //   ::Super:OnError(cMsg,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )

*/

STATIC FUNCTION VALTABLE(oGet,oSay,oGenRep)
   LOCAL oTable,lFound,cTable:=oGet:VarGet(),nAt,cAplica

   IF !(oGet:nLastKey=13 .OR. oGet:nLastKey=9)
       RETURN .T.
   ENDIF

   oTable:=OpenTable("SELECT TAB_NOMBRE,TAB_DESCRI,TAB_APLICA FROM DPTABLAS WHERE TAB_NOMBRE"+;
           GetWhere("=",cTable),.T.)

   lFound         :=oTable:RecCount()>0
   oGenRep:lTable :=lFound
   cAplica        :=oTable:TAB_APLICA

   oSay:SetText(oTable:TAB_DESCRI)
   oTable:End()

   IF !lFound

      EVAL(oGet:bAction)
      RETURN .F.

   ELSEIF EMPTY(oGenRep:oREP_APLICA:VarGet())

      nAt:=ASCAN(oGenRep:aAplica,{|c,i|Left(c,2)==cAplica})

      IF nAt>0
         oGenRep:REP_APLICA :=oGenRep:aAplica[nAt]
         oGenRep:oREP_APLICA:VarPut(oGenRep:aAplica[nAt],.T.)
         oGenRep:oREP_APLICA:nAt:=nAt
      ENDIF

   ENDIF

RETURN .T.

/*
// Solicita la Lista de Tablas
*/
STATIC FUNCTION LISTTABLAS(oGet)
     //LOCAL uValue,lResp,oGet,cWhere:=""
     LOCAL cTable:=oGet:VarGet(),lResp,cWhere:="",cOldTable:=cTable

     lResp:=DPBRWPAG("DPTABLASEL.BRW",0,@cTable,NIL,.T.,cWhere)

     IF cTable!=cOldTable
       oGet:VarPut(cTable,.T.)
       oGet:SetFocus()
       oGet:Keyboard(13)
       lResp:=.T.
     ENDIF

RETURN lResp


/*
// Demo de Gráfico
*/

#Include "G_Graph.ch"

FUNCTION Grafico()
  Local aValues, aTitCol,cTitle:=""
  LOCAL oFontT, oFontX, oFontY, oFont
  LOCAL oGraph, oColumn, ii,oGWnd
  LOCAL aData:={}
  LOCAL cSql,I
  LOCAL nDivide:=1,nTotal:=0
  LOCAL oTable

  cSql  :="SELECT GRUPO,COUNT(*) AS CUANTOS FROM NMTRABAJADOR GROUP BY GRUPO"
  oTable:=OpenTable(cSql,.T.)
  aData :=oTable:aDataFill
  oTable:End()

  DEFINE FONT oFont  NAME "MS Sans Serif" SIZE 0,-10
  DEFINE FONT oFontT NAME "Times New Roman" SIZE 0,-18 BOLD ITALIC
  DEFINE FONT oFontX NAME "Times New Roman" SIZE 0,-10
  DEFINE FONT oFontY NAME "Times New Roman" SIZE 0,-10

  aValues:={}
  nDivide:=MAX(LEN(aData),10)
  FOR I := 1 TO LEN(aData)
     AADD(aValues,{aData[I,1],aData[I,2],1})
     nTotal:=nTotal+Val(aData[I,2])
  NEXT

  cTitle:="["+ALLTRIM(STR(nTotal))+"]"

/*
  aValues := { { "Viernes", 10000, 12000 }, ;
               { "Sabado" , 09000, 12000 }, ;
               { "Domingo", 08000, 12000 }, ;
               { "Lunes"  , 02000, 12000 };
               }
*/
/*               { "May", 13600, 16000 }, ;
               { "Jun",   500,  8000 }, ;
               { "Jul", 12000,  3000 }, ;
               { "Aug", 14500, 11000 }, ;
               { "Sep", 14600, 18600 }  ;
           } */


 aTitCol := { { "Grupo 1" , RGB(150,150,150) }, ;
              { "Grupo 2" , RGB(  0,200,100) }  ;
            }

 oGWnd := GraWnd():New( , , , , ;
          "Distribución de Trabajadores por Grupo", GraServer():New(aValues), ;
          .F. )

 oGWnd:oWnd:oFont := oFont

//Sin ToolBar
 oGWnd:bToolBar := { || nil }
//Tomar grafica
 oGraph := oGWnd:oGraph

 oGraph:lPopUp := .T.
 oGraph:oTitle:cText   := "Trabajadores por Grupo"
// oGraph:oTitle:cText   := cTitle+" Firmas Recogidas en "+ALLTRIM(str(nDivide))+" Horas "
 //Tipo Lineas
 oGraph:GTypeLine()
 //Titulo a la izquierda
 oGraph:oTitle:AliLeft()
 //Asignar Fonts
 oGraph:oTitle:oFontT:=oFontT
 oGraph:oAxisX:oFontL:=oFontX
 oGraph:oAxisYL:oFontL:=oFontY
 //Asignar colores
 oGraph:oTitle:nClrT   :=RGB( 55, 55, 55)
 oGraph:oAxisX:nClrL   :=CLR_RED
 oGraph:oAxisYL:nClrL   :=CLR_BLUE
 //Presentacion de etiquetas AxisYL
 oGraph:oAxisYL:bToLabel := { |nParam| TRANSFORM(nParam,"999,999") }
 oGraph:cBitmap:= "beige2.bmp"

 //Sin linea punteada en Grid
 oGraph:oAxisX:lDottedGrid := .F.
 oGraph:oAxisYL:lDottedGrid := .F.
 //Intervalo de AxisYL
 oGraph:oAxisYL:nStepOne := 6000
 //Color gris en oAxisYL:nAxisBase
 oGraph:oAxisYL:nClrZ := CLR_GRAY
 //Anular escalamiento automatico en AxisYL
 oGraph:oAxisYL:bToScale := { | nVal | nVal }

 //Asignar automaticamente las Columnas del servidor de datos
 oGraph:AutoData()

 //Asignar titulos y colores a Columnas (la primer columna son valores de X)
 FOR ii = 1 TO oGraph:nColGraph()
   oColumn:= oGraph:GetColGraph(ii)
   oColumn:cTitle   := aTitCol[ii,1]
   oColumn:nClrFill := aTitCol[ii,2]
 NEXT ii

oGWnd:Activate()

//oGWnd:oWnd:bGotFocus:={||cFileBmp:="firmasxhora.bmp",oWndx:=oGWnd:oWnd}
//Save2Bmp(oGWnd:oWnd,"firmasxhora.bmp")
// Eval(oGWnd:oWnd:bGotFocus)
// oWndX:=oGWnd:oWnd
// oGWnd:oWnd:Maximize()
// Fotografiar()

oFontT:End()
oFontX:End()
oFontY:End()
oFont:End()

RETURN (NIL)

FUNCTION REPORTE(cCodigo,cWhere,cHelp,cFileChm,cTitle,cSqlRun)
   LOCAL oTable,nLen:=SQLFIELDLEN("DPREPORTES","REP_CODIGO")
   LOCAL oRep
   LOCAL oDb:=OpenOdbc(oDp:cDsnConfig)

   DEFAULT cCodigo:=""

   cCodigo:=IIF( ValType(cCodigo)="N",STRZERO(cCodigo,nLen) , cCodigo )

   // cCodigo:=CTOZERO(PADR(cCodigo,nLen))

   // Si el Reporte no Existe lo importa desde Otro Diccionario de Datos
   IF Empty(SQLGET("DPREPORTES","REP_CODIGO","REP_CODIGO"+GetWhere("=",cCodigo),NIL,oDb))
      EJECUTAR("DPREPORTIMPORT")
   ENDIF

   IF Empty(SQLGET("DPREPORTES","REP_CODIGO","REP_CODIGO"+GetWhere("=",cCodigo),NIL,oDb))
      MensajeErr("Reporte "+cCodigo+" No existe")
      RETURN .F.
   ENDIF

   oRep:=EJECUTAR("DPREPORTES",0,cCodigo,cWhere,cHelp,cFileChm,cTitle)

RETURN oRep

/*
// Visualización de Archivos RTF
*/
// #INCLUDE "\FW24\INCLUDE\RichEdit.ch"
#define COLOR_BTNFACE    15

#define FW_BOLD          700

#define ID_YES           6
#define ID_NO            7

#define MB_YESNOCANCEL   3
#define MB_ICONQUESTION  32

#define RTF_VER          "RichPad 1.3"
#define RTF_CREDITS      "RichPad 1.3  © 2003  -  http://www.ozs.com"

FUNCTION ViewRTF(cFile,cTitle,oFont,lHscroll)

   LOCAL cRTF   := "",nFormat
   LOCAL oWnd, oRTF, oMsgBar, oLen, oRow, oCol
   LOCAL oBar,oBtn,oIco
   LOCAL oFontBtn

   DEFAULT cFile :="docs\capitulo1.rtf",;
           cTitle:=cFile,;
           lHscroll:=.T.

   cFile:=STRTRAN(cFile,"/","\")

   IF !File(cFile)
      MensajeErr("Archivo :"+cFile+" no Exite","Precaución")
      RETURN .F.
   ENDIF

   CursorWait()

//   ErrorSys(.T.) // QUITAR ESTO

   IF oFont=NIL

    DEFINE FONT oFont     NAME "Courier"   SIZE 0, -10

   ENDIF

   DEFINE FONT oFontBtn NAME "Tahoma" SIZE 0, -10 BOLD


   IF Empty(oDp:hDllRtf) // Carga RTF
      oDp:hDLLRtf := LoadLibrary( "Riched20.dll" )
   ENDIF

   IF !oDp:IsDef("RtfcFind") // Inicio de búsqueda del RTF
     oDp:RTFcFind:=SPACE(50) // Carga RTF
     oDp:RTFcRepl:=SPACE(50) // Reemplazar
     oDp:RTFnDir :=1
     oDp:RTFlCase:=.F.
     oDp:RTFlWord:=.T.
   ENDIF

   DEFINE WINDOW oWnd FROM 0, 0 TO 300, 650 PIXEL ;
          MDICHILD OF DpWndMain() ;
          TITLE cTitle;
          COLOR CLR_BLACK, NIL

   DEFINE ICON oIco RESNAME "ICON"

   oWnd:SetIcon(oIco)

   DEFINE BUTTONBAR oBar SIZE 45+15,45+15 OF oWnd  3D
   oBar:bRClicked := NIL

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFontBtn;
          FILENAME "BITMAPS\XSAVE.BMP";
          TOP PROMPT "Grabar";
          ACTION oRtf:SaveFile(cFile )

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFontBtn;
          FILENAME "BITMAPS\PASTE.BMP";
          TOP PROMPT "Pegar";
          ACTION oRtf:Paste()

    DEFINE BUTTON oBtn;
           OF oBar;
           NOBORDER;
           FONT oFontBtn;
           FILENAME "BITMAPS\XPRINT.BMP";
           TOP PROMPT "Imprimir";
           ACTION oRtf:Print( cFileNoPath( cFile ))

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFontBtn;
          TOP PROMPT "Cerrar";
          FILENAME "BITMAPS\XSALIR.BMP";
          ACTION (oWnd:End())

//   oWnd:SetIcon( TIcon():New( ,, "RTFNew" ) )
//   oWnd:SetFont( ::oFont )
//   oWnd:SetText( If( Empty( cFile ), cTitle, cFileNoPath( cTitle ) ) )

   IF lHscroll

     @ 0, 0 RICHEDIT oRTF VAR cRTF OF oWnd ;
            FILE cFile FORMAT nFormat FONT oFont HSCROLL
   ELSE

     @ 0, 0 RICHEDIT oRTF VAR cRTF OF oWnd ;
            FILE cFile FORMAT nFormat FONT oFont HSCROLL

   ENDIF

   DEFINE MSGBAR oMsgBar OF oWnd PROMPT "File: " + cFile ;
          COLOR CLR_HBLUE, GetSysColor( COLOR_BTNFACE )

   DEFINE MSGITEM oLen PROMPT "0 bytes" SIZE 80 OF oWnd:oMsgBar ;
          TOOLTIP "Size of the file"

   DEFINE MSGITEM oRow PROMPT "Lin: 0" SIZE 60 OF oWnd:oMsgBar ;
          TOOLTIP "Line"

   DEFINE MSGITEM oCol PROMPT "Col: 0" SIZE 60 OF oWnd:oMsgBar ;
          TOOLTIP "Column"

   oRTF:bChange := { || oLen:SetText( Transform( oRTF:Len(), "@E 999,999 bytes" ) ), ;
                        oRow:SetText( "Lin: " + LTrim( Str( oRTF:GetRow() ) ) ), ;
                        oCol:SetText( "Col: " + LTrim( Str( oRTF:GetCol() ) ) ), ;
                        NIL }


   oWnd:SetControl( oRTF )

   oWnd:bValid := { || oRtf:End() , .T. }

   oBar:SetColor(NIL,oDp:nGris)
   Aeval(oBar:aControls,{|a,n|a:SetColor(nil,oDp:nGris)})

   ACTIVATE WINDOW oWnd MAXIMIZED

RETURN oRTF

STATIC FUNCTION VALGRUPO(oGet,oSay,oGenRep)
  LOCAL oTable,lFound,cGrupo:=CTOZERO(oGet:VarGet()),nAt,cAplica

  oGet:SetText(cGrupo)
  oGenRep:REP_GRUPO:=cGrupo

  oTable:=OpenTable("SELECT GRR_CODIGO,GRR_DESCRI FROM DPGRUREP WHERE GRR_CODIGO"+;
          GetWhere("=",cGrupo),.T.)

  lFound         :=oTable:RecCount()>0
  oGenRep:lTable :=lFound

  oSay:SetText(oTable:GRR_DESCRI)
  oTable:End()

  IF !lFound
     EVAL(oGet:bAction)
     RETURN .F.
  ENDIF

RETURN .T.

STATIC FUNCTION LISTGRUPOS(oGet,oGenRep)
//  LOCAL cGrupo

   LOCAL cGrupo:=EJECUTAR("REPBDLIST","DPGRUREP",{"GRR_CODIGO","GRR_DESCRI"},NIL,NIL,NIL,NIL,NIL,NIL,NIL,NIL,oGet)

   IF !Empty(cGrupo)
     oGet:VarPut(cGrupo,.T.)
     // oGet:SetText(cGrupo)
     oGenRep:REP_GRUPO:=cGrupo
     oGet:KeyBoard(13)
   ENDIF

RETURN .T.

STATIC FUNCTION RTFSTATUS(oMemo)
   LOCAL nAlign
   LOCAL aChar  := oMemo:GetCharFormat()
   LOCAL aChar2 := oMemo:GetCharFormat2()

   nAlign := oMemo:GetAlign()

RETURN .T.

/*
// Valida el Codigo del Reporte
*/
FUNCTION VALGENREP()
  // )CERO(oGenRep:REP_CODIGO,.T.)
RETURN .T.
