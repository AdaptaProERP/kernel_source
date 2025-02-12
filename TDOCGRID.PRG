/*
// TDOCGRID
// Detalle del Encabezado del Documento
*/
#include "FiveWin.ch"
#include "InKey.ch"
#include "xbrowse.ch"
#include "\DWH\LIBS\TSBUTTON.CH"
#include "Obj2Hb.ch"

MEMVAR oDp

CLASS TDOCGRID

   DATA oTable
   DATA oDb
   DATA oBrw
   DATA oScript
   DATA oHead
   DATA oFont
   DATA oFontH     // Fuente para los Encabezados
   DATA oFontF     // Fuente para Footer (totales)
   DATA oFontBtn
   DATA oSayOpc
   DATA oEditMemo
   DATA oFocus    // Despues del PostEdit
   DATA oDlg      // Ubicacion DIALOG

   DATA uEdit // Valor por Defecto en la Edición

   DATA nOption
   DATA nHeaderLines
   DATA nOptionOld
   DATA nBtnWidth
   DATA nBtnHeight
   DATA nCuerpo
   DATA nBtnStyle  // Tipo de Boton: 0=Nada, 1=Bmp 2=Text
   DATA nColEdit   // Columna Que Esta en Edición
   DATA nLineGet   // Linea de Get
   DATA nRowSel    // Linea que Edita
   DATA nLastKey   // Enviado por la Columna
   DATA nTotalWidth INIT 0 // Total de las Columnas



   DATA nItemAdd
   DATA nGris      // Color Gris del Borde del Browse
   DATA nClrPane1,nClrPane2,nClrText,nClrFind
   DATA nRecSelColor // Color del Selector
   DATA nArrayAt
   DATA nFileMain
   DATA nMaxItems   INIT 0
   DATA nTime       INIT 0
   DATA nAtData     INIT 1
   DATA nStyle      INIT 1


   DATA nMarqueeStyle    INIT MARQSTYLE_SOLIDCELL  // MARQSTYLE_HIGHLROW
   DATA nColDividerStyle INIT 0 // LINESTYLE_BLACK
   DATA nRowDividerStyle INIT LINESTYLE_BLACK
   DATA lColDividerComplete INIT .F.

   DATA lAuditaEliMod INIT .F. // JN 10/08/2017 (Pistas de Auditoria, clase TDOCGRID)
   DATA nMargin       INIT 60
   DATA nId           INIT 0 // Numero del Grid dentro de la clase TDOCGRID, lograr personalizar Varios Grid dentro de un documento

   // Color del header
   DATA nClrPaneH
   DATA nClrTextH

   // Color del Footer
   DATA nClrPaneF
   DATA nClrTextF

   DATA aBtn
   DATA aMemo
   DATA aSize
   DATA aCols
   DATA aData
   DATA aLineBack  // Recupera una Linea Cancelada
   DATA aFields
   DATA aDataQuery
   DATA aBtnSize   // Tamaño de los Botones
   DATA aBtnEdit   // Botones de Edición
   DATA aBtnNew    // Nuevos Botones
   DATA aColsSave  //
   DATA aFieldGrid // Campos del Grid
   DATA aCargo    INIT {} // Datos Financieras
   DATA aDataGet  INIT {} // Valor en Cada Columna
// DATA aLine     INIT {} // Valor de la Línea Editada
// DATA aEmpty

   DATA cScript
   DATA cLinkHead  // Campo de la Cabeza que realiza en Link
   DATA cLinkGrid  // Campos del Cuerpo que Realizan en Link
   DATA cSql       // Comando SQL
   DATA cSqlData   // Sql que Muestra los Datos
   DATA cOrderBy
   DATA cTable     // Nombre de la Tabla
   DATA cWhere     // Condición de Cada Registro para ser Modificado
   DATA cScope     // Scope
   DATA cPrimary   // Clave de Identificación Unica, se Valida Conjuntamente con la Cabeza
   DATA cFieldChar // Campos Caracter
   DATA cPostSave  // Programa que se Ejecuta Cuando PosTSave
   DATA cLoad      // Programa de Carga
   DATA cTotal     // Totaliza
   DATA cItem      // Campo Item
   DATA cItem_A    // Campo Item, Asociados, DPMOVINV indica los componentes asociados
   DATA cDelete    // Procedimiento de Borrado
   DATA cPreDelete // Prodecimiento de Eliminación
   DATA cPostDelete // Luego de Borrar
   DATA cPreSave   // Pregrabar
   DATA cMemo      // Contenido del Campo Memo
   DATA cView      // Consultar
   DATA cSqlDataQuery // Comando SQL de DataQuery
   DATA cKey        INIT ""  // Para detectar combinacion de teclas con Ctrl o Alt.
   DATA cFieldFile
   DATA cFieldAud   INIT ""  // Nombre del Campo para el Registro de Pistas de Auditoría
   DATA cCommit     INIT "GRIDCOMMIT" // Antes de Guadar el Registro
   DATA cBrwWidth   INIT ""           // Estado del Browse al Iniciar
   DATA cPrimaryItem INIT ""

   DATA cIncluir,cModificar,cConsular,cEliminar,cRegistro
   DATA cDeleteUpdate // Actualizar por Anulación
   DATA cPrint        // Indica Programa para Imprimir
   DATA cDataAudita   // Contiene la Clave de los Campos que Cambiaron
   DATA cDataHead     // Data del Encabezado
   DATA cFileEdit     // Personalizacion de colores
   DATA cKeyAudita    // Clave para la Auditoria

   DATA bWhen      // Condición de Edición
   DATA bValid     // Condición Para Grabar
   DATA bChange    // Cuando Cambia el Registro
   DATA bDataDel   // Muestra los Datos del Cliente que se desear Borrar
   DATA bClrHeader // Color Encazados
   DATA bClrFooter // Color Pie de Página
   DATA bClrText   // INIT {||::nClrText} // Bloque de codigo del Color Par e Impar,

   DATA lEdit      // Editar Modo Edición
   DATA lView      // Acceso a Consultar
   DATA lFind      // Buscar
   DATA lMsgError
   DATA lEof
   DATA lBof
   DATA lAskDelete
   DATA lBar
   DATA lBtnBrow  // Botones de Navegación
   DATA lTotal    // Indica los Totales en la Barra Inferior
   DATA lIsDef    // Validador
   DATA lPrint    // puede Imprimir
   DATA lShowMsgErr
   DATA lLockHead
   DATA lInc INIT .T.
   DATA lMod INIT .T.
   DATA lEli INIT .T.
   DATA lCon INIT .T.
   DATA lGetRowData  INIT .T. // Lo Realiza en Presave, de lo Contrario lo hace en PostSave
   DATA lInc_Auditar INIT .T. // Hace Auditoria de la Inclusión del Registro
   DATA lShow        INIT .F. // Muestra los Datos
   DATA lEnd         INIT .T. // No ha Concluido
   DATA lViewArray   INIT .F. // Para Visualizar el Arreglo
   DATA lSwapLine    INIT .F.
   DATA lHScroll     INIT .F.  // Hscroll
   DATA lSwapCol     INIT .F.
   DATA lLostFocus   INIT .T.  // Ejecuta Lost-Focus


    DATA nClrText  INIT oDp:nGris // CLR_BLUE
    DATA nClrText1 INIT 0
    DATA nClrText2 INIT 0
    DATA nClrText3 INIT 0
    DATA nClrText4 INIT 0
    DATA nClrText5 INIT 0


    DATA cClrText  INIT ""
    DATA cClrText1 INIT ""
    DATA cClrText2 INIT ""
    DATA cClrText3 INIT ""
    DATA cClrText4 INIT ""
    DATA cClrText5 INIT ""

    DATA nClrPane1 INIT oDp:nClrPane1
    DATA nClrPane2 INIT oDp:nClrPane2
    DATA nClrPane3 INIT 0


   DATA lCreaRegIntRef INIT .T.
   DATA NOTIFY,COMMAND

   // DATA lBotton

    METHOD New( oHead , cTable , cLinkHead , cLinkGrid , cScope , cOrderBy ,oDlg, cGroupBy)
    METHOD AddCol(cField,cNameAs,nPos)
    METHOD InsertCol(cField,cNameAs,nAt,cFieldP)
    METHOD GetPosAt(cField)

    METHOD SetSize() INLINE ::oBrw:Move(::aSize[1],::aSize[2],::aSize[3],::aSize[4],.T.)
    METHOD ColorFind()
    METHOD Set(cField,uValue,lBrowse)
    // Manejo de Campos direccionados por Variables
    METHOD SetV(cVarName,uValue,lBrowse) INLINE ::Set(::Get(cVarName),uValue,lBrowse)
    METHOD GetV(cVarName)                INLINE ::Get(::Get(cVarName))

    METHOD SetScript(cScript)
//  METHOD ChkIntRef(cField)
    METHOD CancelEdit()
    METHOD Append()
    METHOD NewMemo(cNumMemo) // Asigna un Nuevo Memo
    METHOD Edit(lEdit)
    METHOD BrwGotFocus()
    METHOD BrwLostFocus()
    METHOD DocGotFocus()
    METHOD DocLostFocus()
    METHOD GridColorText()
    METHOD BRWREFRESCAR() INLINE ::oBrw:Refresh(.F.)
    METHOD GRIDGETSIZE(nTop,nLeft,nWidth,nHeight,cFileBrwZ) INLINE ::aSize:=EJECUTAR("GRIDGETSIZE",SELF,nTop,nLeft,nWidth,nHeight,cFileBrwZ),::aSize


    METHOD GetCol(cName,lMsg)
    METHOD BtnSave()
    METHOD PreSave()
    METHOD Find()
    METHOD SetFind(lOn) // Activa la Búsqueda
    METHOD DbSeek()
    METHOD Print() INLINE ::RunScript(::cPrint)
    METHOD View()  INLINE ::RunScript(::cView)
    METHOD SetColPos(cField)  // Posicionarse en la Columna
    METHOD GetTotalWidth()
    METHOD CalcWidth(nWidth,nReserva) // Devuelve si Puedes Agregar Columnas

//    METHOD SayOpc()
    METHOD Save()
    METHOD PostSave()
    METHOD Activate()
    METHOD Prepare()
    METHOD Cancel()
    METHOD Change(lChange)
    METHOD RunDblClick()
    METHOD NewJob(nOption,lClear)
    METHOD IsFinish() // Si no has Columnas editables
    METHOD NewItem()
    METHOD PostDelete()
    METHOD Delete(cSql)
    METHOD PreDelete(lAuto)
    METHOD PostDelete(lAuto)
    METHOD MensajeErr(cMsg,cTitle) INLINE (::lMsgError:=.T.,EJECUTAR("XSCGMSGERR",::oBrw,cMsg,cTitle))
    METHOD SwapLineUp()   INLINE EJECUTAR("GRIDSWAPLINE",SELF,.T.)
    METHOD SwapLineDown() INLINE EJECUTAR("GRIDSWAPLINE",SELF,.F.)

// MensajeErr(cMsg)

    METHOD OpcSay()
    METHOD GridRefresca() INLINE EJECUTAR("GRIDREFRESCA",SELF)
    METHOD GotFocus()     INLINE (oDp:lTracer_:=oDp:lTracer,;
                                  oDp:lTracer:=.F.,;
                                  ::oHead:oBrw :=::oBrw,;
                                  IIF(::lLostFocus,EJECUTAR("DOCGRIDGOTFOCUS",SELF),NIL),;
                                  oDp:lTracer:=oDp:lTracer_)

// //::oBrw:GotFocus() // MensajeErr(GETPROCE(),"METODO POR IMPLEMENTAR")

    METHOD RunOff()      INLINE (::LoadRecord(),;
                                 ::aCols[::oBrw:nGetColSel()]:RunOff())   // JN 21/11/2024 nColSel->:nGetColSel()
// oCol:nPos
// INLINE (::LoadRecord(),;
//                                  ::aCols[::oBrw:nGetColSel()]:RunOff())

    METHOD AddBtn(cFile,cText,cWhen,cAction,cFind,cKey)

    // 06/10/2023 Incluye Valor de cItem para indicar los campos del Where facilitando guardar la factura de compra
    // en su defecto puede ser LEN(oGrid:oBrw:aArrayData)-1
    METHOD COUNT(cField) INLINE COUNT(::cTable,::GetWhere(cField),::oDb)  // jn 25/05/2023 debe ser cambiado por el conjunto completo de los registros del grid, esta tomando en cuenta mov_item no graba la factura
    METHOD LoadRecord()
    METHOD GetColEdit() INLINE ::oBrw:nGetColSel() // Current columns based on current display
    METHOD Open()     // Modificar
    METHOD RunKey()
    METHOD ColCalc(cField)
    METHOD ShowTotal() INLINE AEVAL(::aCols,{|oCol,n|oCol:ShowTotal()}),;
                              ::oBrw:RefreshFooters() // ::oBrw:Refresh(.F.)
//,MensajeErr(GETPROCE(),"SHOWTOTAL") // JN 03/12/2015
    METHOD SetFocus()  INLINE 1=1 /*(MsgAlert(::oFocus:ClassName(),"oGrid:SetFocus"),;
                               DpFocus(::oFocus),::oFocus:=nil) */


    METHOD SetFontHead(oFont)   INLINE ::oFontH:=oFont
    METHOD SetFontFooter(oFont) INLINE ::oFontF:=oFont
    METHOD SetColor()

    METHOD FieldPos(cField)

//  METHOD Find()
    METHOD CalCols() // Calcula las Columnas
    METHOD KeyBoard(nKey)
    METHOD GetWhere(cFields)
    METHOD Get(cVarName) INLINE __objSendMsg(Self,cVarName)
    METHOD ChkRecord()
    METHOD Mensaje(cMensaje)
    METHOD RunScript(cFunction,nPar1,nPar2,nPar3,nPar4)
    METHOD LoadData() // Carga la Data
    METHOD AdjustBtn(lForWhen)
    METHOD PutBar()
    METHOD SetScope(cScope)
    METHOD CalculeRow(bExp) INLINE ::aCols[1]:CalCuleRow(bExp)
    METHOD GetTotal(cField,lAll)
    METHOD SetColorHead(nClrPane,nClrText,oFont)
    METHOD IsDef( cName , lResp ) INLINE __objHasMsg( self, cName )
//      INLINE (::lIsDef:=.T.,lResp:=__objSendMsg(Self,cName)=NIL,::lIsDef:=.F.,lResp)
//     __objHasMsg( self, cName )

    // Movilizando el Grid
    METHOD Gotop()       INLINE (::oBrw:GoTop()   ,::oBrw:nArrayAt:=1,::Skip(0), ::oBrw:Refresh(.t.))
    METHOD GoBottom()    INLINE (::oBrw:GoBottom(),::Skip(0), ::oBrw:Refresh(.t.))
    METHOD Recno()       INLINE ::oBrw:nArrayAt
    METHOD Goto(nRecord) INLINE (::oBrw:nArrayAt:=nRecord,::Skip(0))
    METHOD RecCount()    INLINE LEN(::oBrw:aArrayData)
    METHOD Skip(nSkip)
    METHOD Eof()         INLINE ::lEof .OR. Empty(::oBrw:aArrayData)
    METHOD Bof()         INLINE ::lBof .OR. Empty(::oBrw:aArrayData)
    METHOD Replace(cField,uValue)
    METHOD CreateBtn(oDlg)
    METHOD SetMemo(cField,cTitle,nTop,nLeft,nWidth,nHeight)
    METHOD SetAdjuntos(cField,cTitle)
    METHOD ItemSelect()

    METHOD RestorePar() INLINE EJECUTAR("GRIDRESTORE",SELF)

    METHOD GetRowData()

    METHOD Browse()  INLINE ::oTable:Browse()
    METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)

METHOD End()

METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                               WndHandleEvent( Self, nMsg, nWParam, nLParam )


ERROR HANDLER OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 )

ENDCLASS

METHOD New(oHead , cTable , cLinkHead , cLinkGrid , cSql , cScope , cOrderBy, oDlg, cGroupBy ) CLASS TDOCGRID

  LOCAL oTable,nT1:=Seconds()

  DEFAULT cSql:="SELECT * FROM "+cTable,cOrderBy:=""

  PUBLICO("oGrid",Self)

  ::oDb:=oHead:oDb // Conector de la Base de Datos

  cOrderBy:=" "+cOrderBy

  IF !Empty(cOrderBy) .AND. !"ORDER BY"$cOrderBy
     cOrderBy:=" ORDER BY "+cOrderBy
  ENDIF

  // ::nNumEdit    :=nNumEdit
  ::cTable      :=cTable
  ::oHead       :=oHead
  ::cOrderBy    :=cOrderBy
  ::cLinkHead   :=cLinkHead
  ::cLinkGrid   :=cLinkGrid
  ::cView       :="GRIDVIEW"
  ::lTotal      :=.F.
  ::cScript     :=""
  ::aSize       :={60,10,400,200}
  ::aBtnNew     :={}
  ::aCols       :={}
  ::aBtnEdit    :={}
  ::aFieldGrid  :={}
  ::cSql        :=cSql
  ::cSqlData    :=""
  ::lEdit       :=.F.
  ::lEof        :=.T. // Siempre es Fin de Archivo
  ::lBof        :=.T. // Siempre es Ultimo
  ::lBar        :=.F. // No tiene Barra de Botones
  ::lView       :=.T. // Acceso a Consultar
  ::lFind       :=.T. // Siempre Busca
  ::lIfDef      :=.F.
  ::lPrint      :=.F.
  ::lShowMsgErr :=.F.
  ::cFileSound  :="SOUNDS\msgerror.wav"// ::lBotton     :=.T. // Posee Botones
  ::nId         :=0

  //::nMarqueeStyle :=0 // NIL // JN  4/06/2017 Windows8, color negro cuado se pierde foco

  ::nBtnStyle   :=oHead:nBtnStyle
  ::nOption     :=0
  ::nColEdit    :=1 // Siempre Empieza de la Columna 1
  ::lBtnBrow    :=.T. // Botones de Navegacion
  ::nLineGet    :=0
  ::nLastKey    :=0
  ::lAskDelete  :=.T.
  ::bWhen       :={||.T.} // Condición General del Get
  ::bValid      :={||.T.} // Condición para Grabar
  ::bChange     :={||.T.} // Movimiento de Registro
  ::aBtn        :={}
  ::lMsgError   :=.F.
  ::cWhereRecord:=""
  ::nItemAdd    :=1       // Incremento de Items
  ::aDataQuery  :={}      // Contiene toda la Data del Query
  ::cPreDelete  :=""      // Pre-Eliminar
  ::cPosDelete  :=""      // Post-Eliminar
  ::cScope      :=cScope
  //::aBtnSize    :={17,17}
  ::aBtnSize    :={16,16}
  ::nOptionOld  :=-1      // Option Anterior
  ::nHeaderLines:=1
  ::nGris       :=oDp:nGris
  ::nClrText    :=CLR_BLACK
  ::nClrFind    :=CLR_HBLUE // HGRAY

  ::bClrText    :={||::nClrText} // Bloque de codigo del Color Par e Impar,

  ::cIncluir    :=MI("Incluir"  ,601)
  ::cModificar  :=MI("Modificar",602)
  ::cConsular   :=MI("Consular" ,603)
  ::cEliminar   :=MI("Eliminar" ,604)
  ::cRegistro   :=MI("Registro" ,613)

// ? oDlg:ClassName(),oDlg, oHead:oDlg,"oHead:oDlg"

  DEFAULT oDlg:=oHead:oDlg

  ::oDlg:=oDlg

  oDp:lExcluye:=.F.  // 05/04/2021  evitar no leer cuerpo filtrar sucursal x Usuario
  oTable  :=OpenTable( cSql , .F. ,::oDb )

  DPWRITE("TEMP\GRID"+alltrim(oTable:cTable)+"_.SQL",cSql)

  ::cTable    :=oTable:cTable

  ::lInc:=MacroEje([ISTABINC("]+::cTable+[")])
  ::lMod:=MacroEje([ISTABMOD("]+::cTable+[")])
  ::lEli:=MacroEje([ISTABELI("]+::cTable+[")])
  ::lCon:=MacroEje([ISTABCON("]+::cTable+[")])

  ::cFieldChar:=""

   // Campos Virtuales
  AEVAL(oTable:aFields,{|a,n|a[1]:=ALLTRIM(a[1]),;
                             __objAddData( Self, a[1]    )          ,;
                             __objAddData( Self, a[1]+"_")          ,;
                             AADD(::aFieldGrid,a[1])                ,;
                             __objSendMsg(Self,a[1],oTable:FieldGet(n))    ,;
                             IIF( ValType(oTable:FieldGet(n))="C"   ,;
                             ::cFieldChar:=::cFieldChar + IIF( Empty(::cFieldChar)," " , "," ) + oTable:FieldName(n), NIL ) })

  ::aFields:=ACLONE(oTable:aFields)

  // ? "aqui 3"
  // MensajeErr(CLPCOPY(oTable:cSql),oTable:nSeconds)

  oTable:End()

  AADD(::aBtn,{1,"xNew2.bmp "   ,"Incluir        "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0 .AND. oGrid:lInc","oGrid:RunKey(73)","Inc",STR(DP_CTRL_I)})
  AADD(::aBtn,{2,"xView2.bmp"   ,"Consultar      "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0 .AND. oGrid:lCon","oGrid:View()"   ,"Con",STR(DP_CTRL_V)})

  AADD(::aBtn,{3,"xEdit2.bmp"   ,"Modificar      "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0 .AND. oGrid:lMod","oGrid:Open()"    ,"Mod",STR(DP_CTRL_M)})
  AADD(::aBtn,{4,"xdelete2.bmp" ,"Eliminar       "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0 .AND. oGrid:lEli","oGrid:RunKey(69)","Eli",STR(DP_CTRL_X)})
  AADD(::aBtn,{5,"xFind2.bmp"   ,"Buscar         "   ,"oGrid:nOption=0","oGrid:SetFind(.T.)"                    ,"Bus",""})
  AADD(::aBtn,{9,"xPrint.bmp"   ,"Imprimir       "   ,"oGrid:nOption=0.AND.oGrid:lPrint","oGrid:Print()"        ,">> ",""})

//  IF ::lSwapLine
  AADD(::aBtn,{5,"UP.BMP"   ,"Subir"   ,"oHead:nOption=3 .AND. oGrid:nOption=0 .AND. oGrid:lSwapLine .AND. oGrid:RecCount()>1","oGrid:SwapLineUp()"    ,"Sub",""})
  AADD(::aBtn,{9,"DOWN.BMP" ,"Bajar"   ,"oHead:nOption=3 .AND. oGrid:nOption=0 .AND. oGrid:lSwapLine .AND. oGrid:RecCount()>1","oGrid:SwapLineDown()"  ,"Baj",""})

//  AADD(::aBtn,{5,"UP.BMP"   ,"Subir"   ,"oGrid:nOption=0 .AND. oGrid:lSwapLine","oGrid:SwapLineUp()"    ,"Sub",""})
//  AADD(::aBtn,{9,"DOWN.BMP" ,"Bajar"   ,"oGrid:nOption=0 .AND. oGrid:lSwapLine","oGrid:SwapLineDown()"  ,"Baj",""})

//  ENDIF

  AADD(::aBtn,{6,"xTop2.bmp"    ,"Primer         "   ,"oGrid:nOption=0 ","oGrid:oBrw:GoTop(.t.)"                                   ,"<< ",""})
  AADD(::aBtn,{7,"xAnt2.bmp"    ,"Pagina Abajo   "   ,"oGrid:nOption=0 ","oGrid:oBrw:GoUp(.t.)"                                   ," < ",""})
  AADD(::aBtn,{8,"xSig2.BMP"    ,"Pagina Arriba  "   ,"oGrid:nOption=0 ","oGrid:oBrw:GoDown(.t.)"                                   ," > ",""})
  AADD(::aBtn,{9,"xFin2.bmp"    ,"Ultimo Registro"   ,"oGrid:nOption=0 ","oGrid:oBrw:GoBottom(.t.)"                                   ,">> ",""})

  AADD(::aBtn,{11,"xSave2.bmp"   ,"Salvar         "   , "oGrid:nOption!=0 " , "oGrid:BtnSave()"   ,"Gra",STR(DP_CTRL_G)})

  ::oTable:=oTable

RETURN SELF

METHOD SetColor() CLASS TDOCGRID

    IF !EMPTY(::nClrPane1) .AND. !EMPTY(::nClrPane2)
      ::oBrw:SetColor(::GridColorText(),::nClrPane1)
      ::oBrw:bClrStd := {|| { IIF( ::ColorFind(), ::nClrFind , ::GridColorText() ), iif( ::oBrw:nArrayAt%2=0, ::nClrPane1  ,   ::nClrPane2 ) } }
    ELSEIF !EMPTY(::nClrPane1)
// OJO GRIDCOLOR
//     ::oBrw:SetColor(::nClrText,::nClrPane1)
      ::oBrw:SetColor(::GridColorText(),::nClrPane1)
      ::oBrw:bClrStd := {|| { IIF( ::ColorFind(), ::nClrFind , ::GridColorText() ), ::nClrPane1 } }
    ENDIF

RETURN .T.

/*
// Agregar Nuevos Botones
*/
METHOD AddBtn(cFile,cText,cWhen,cAction,cFind,cKey) CLASS TDOCGRID

/*
  5
  ,"xFind.bmp"
  ,"Buscar         "
  ,"oGrid:nOption=0"
  ,"1=1"
  ,"Bus"
  ,"1776"})
*/

   DEFAULT cFind:="OTHER",cText:="",cWhen:="oGrid:nOption=0",cAction:="MsgAlert('OK')",cKey:="cKey"

   IF ValType(cKey)="N"
      cKey:=STR(cKey)
   ENDIF

   AADD(::aBtnNew,{0,cFile,cText,cWhen,cAction,cFind,cKey})

RETURN NIL

METHOD ITEMSELECT()
   LOCAL nCol   :=::oBrw:nGetColSel(),oCol:=::aCols[nCol]
   LOCAL lSelect:=!(::oBrw:aArrayData[::oBrw:nArrayAt,nCol])
   LOCAL lResp

   IF ::nOption=0
      RETURN NIL
   ENDIF

   ::Set(oCol:cField,lSelect,.T.) // Asigna el Valor en la Variable, 02/08/2013

   ::oBrw:aArrayData[::oBrw:nArrayAt,nCol]:=lSelect
   ::oBrw:DrawLine(.T.)

   oCol:uValue:=lSelect

   // JN 08/05/2014
   lResp:=EVAL(oCol:bValid)

   // Debe Recuperar el Valor Anterior
   IF !lResp
      ::Set(oCol:cField,!lSelect,.T.) // Asigna el Valor en la Variable, 02/08/2013
   ENDIF

RETURN NIL

METHOD GridColorText() CLASS TDOCGRID
   LOCAL aLine   :=::oBrw:aArrayData[::oBrw:nArrayAt]
   LOCAL nClrText:=::nClrText

// oDp:oFrameDp:SetText(ValType(::bClrText))

   IF ValType(::bClrText)="C"
      ::bClrText:=BLOQUECOD(::bClrText)
   ENDIF

   IF !::bClrText=NIL
     nClrText:=EVAL(::bClrText,aLine,::oBrw:nArrayAt,Self)
   ENDIF

//   oDp:oFrameDp:SetText(ValType(::bClrText)+" "+LSTR(nClrText)+" "+LSTR(EVAL(::bClrText,aLine,::oBrw:nArrayAt,Self)))

RETURN nClrText

/*
// Posicionar en la Columna del Browse
*/
METHOD SetColPos(cField)  CLASS TDOCGRID
   LOCAL oCol:=::GetCol(cField)

   IF ValType(oCol)="O"
      ::oBrw:nColSel:=oCol:nCol
   ENDIF

RETURN .T.

/*
// Mostrar Opción
*/
METHOD NewMemo(nNumMemo) CLASS TDOCGRID
     LOCAL cMemo:=""

     IF Empty(nNumMemo)
        RETURN ""
     ENDIF

     cMemo:=SQLGET(oDp:cDpMemo,"MEM_MEMO","MEM_NUMERO"+GetWhere("=",nNumMemo))

//     IF Empty(cMemo)
//        cMemo:=SQLGET(oDp:cDpMemo,"MEM_MEMO","MEM_NUMERO"+GetWhere("=",nNumMemo))
//     ENDIF

     IF !Empty(cMemo)
       ::aMemo[7]:=0
       ::aMemo[8]:=cMemo
       ::aMemo[9]:=""
     ENDIF

RETURN cMemo

METHOD GetTotalWidth()  CLASS TDOCGRID
   ::nTotalWidth:=0
   AEVAL(::aCols,{|oCol,n| ::nTotalWidth:=::nTotalWidth+oCol:nWidth})
RETURN ::nTotalWidth


 METHOD GetPosAt(cField) CLASS TDOCGRID
   LOCAL nCol:=ASCAN(::aCols,{|o|o:cField==cField})
RETURN nCol

// Indica el Espacio Disponible
METHOD CalcWidth(nWidth,nReserva)
   LOCAL nDif:=0,oGrid:=SELF

   DEFAULT nReserva:=0 ,;
           nWidth  :=0

   ::GetTotalWidth() // Siempre requiere Calcular el Total

   // Ancho  50 Barra y SCROLL
   // nDif:=(oGrid:aSize[3]-::nMargin)-(nReserva+nWidth)-::nTotalWidth
   nDif:=(oGrid:aSize[3]-(::nMargin+LEN(::aCols)))-(nReserva+nWidth)-::nTotalWidth

RETURN nDif
/*
// Mostrar Opción
*/
METHOD OpcSay() CLASS TDOCGRID

  LOCAL cText:=::cIncluir

  IF ValType(::oSayOpc)!="O"
     RETURN NIL
  ENDIF

  DO CASE

     CASE ::nOption=2
          cText:=::cConsultar
     CASE ::nOption=3
          cText:=::cModificar
     CASE ::nOption=4
          cText:=::cEliminar

  ENDCASE

  ::oSayOpc:SetText(cText+" "+::cRegistro)

RETURN NIL

/*
// Mostrar Opción
*/
METHOD Find() CLASS TDOCGRID
   LOCAL I

   // Busca las Find Iniciales, Solicitados por Otro Programa
   FOR I := 1 TO LEN(::aCols)
       IF ValType(::aCols[I]:bFind)="B"
         EVAL(::aCols[I]:bFind)
         ::aCols[I]:bFind:=NIL
       ENDIF
   NEXT

RETURN .T.

/*
// Cancela Edición
*/
METHOD CancelEdit(lBtn) CLASS TDOCGRID

   DEFAULT lBtn:=.T.

   ::oBrw:CancelEdit()
   Aeval(::oBrw:aCols,{|oCol|oCol:KillGet(),oCol:nEditType:=0})
   ::oBrw:DrawLine(.F.)
   ::nOption:=0
   // ? Len(::oBrw:aArrayData),"MAXIMO"
   ::Change(.F.)
   IIF( lBtn, ::AdjustBtn(.T.) , NIL )

RETURN .T.

/*
// Establece Buscar
*/

//METHOD FIND() CLASS TDOCGRID
//   ? "AQUI DEBE BUSCAR"
// RETURN .T.

/*
// Busca el Total por Columna
*/
METHOD GetTotal(cField) CLASS TDOCGRID
    LOCAL oCol:=::GetCol(cField)
    ::CalCols()
RETURN IIF( ValType(oCol)="O" , oCol:nTotal ,0 )

/*
// Emite Mensaje de Error, Typo ToolTip
*/
METHOD Mensaje(cMensaje) CLASS TDOCGRID
   MensajeErr(cMensaje)
RETURN .T.


/*
// Mueve los Registros
*/
METHOD Skip(nSkip)  CLASS TDOCGRID
   ::lEof:=.F.
   ::lBof:=.F.

   DO CASE

      CASE nSkip=1

          IF ::oBrw:nArrayAt=LEN(::oBrw:aArrayData)
             ::lEof:=.T.
           ELSE
             ::oBrw:GoDown()
           ENDIF

      CASE nSkip=-1

           IF ::oBrw:nArrayAt=1
             ::lBof:=.T.
           ELSE
             ::oBrw:GoUp()
           ENDIF

   ENDCASE

   IF ::oBrw:nArrayAt<=LEN(::oBrw:aArrayData) .AND. ::oBrw:nArrayAt<=LEN(::aDataQuery) .AND. !Empty(::aDataQuery)

      AEVAL(::aFields,{|a ,n , uValue | uValue:=::aDataQuery[::oBrw:nArrayAt,n],;
            ::Set(a[1]             ,uValue),;
            ::Set(ALLTRIM(a[1])+"_",uValue) })

   ENDIF

   IF !Empty(::cLoad)
     ::RunScript(::cLoad)
   ENDIF

RETURN nSkip

/*
// Asigna los Valores desde el Browse
// No los Pinta en Línea
*/
METHOD Replace(cField,uValue,lSet)  CLASS TDOCGRID
      LOCAL oCol:=::GetCol(cField)

      DEFAULT lSet:=.F.

      IF ValType(oCol)="O"
         ::oBrw:aArrayData[::oBrw:nArrayAt,oCol:nCol]:=uValue
      ENDIF

      IF lSet
         ::Set(cField,uValue,.T.)
      ENDIF

RETURN uValue

/*
// Inicia Una Nueva Actividad
// lCrear, Borra todas las Variables
*/
METHOD NewJob(nOption,lClear) CLASS TDOCGRID
   LOCAL oTable,I,aLine:={},nAt,nRecno,nCol:=1,cSql // ,nRecno
   LOCAL nArrayAt:=::oBrw:nArrayAt
   LOCAL nRowSel :=::oBrw:nRowSel

  //  ? "NewJob",GETPROCE()

   DEFAULT nOption:=::oHead:nOption,lClear:=.F.

   ::aData:={}
   ::Prepare()

   //IF nOption<>1 // JN 7/9/09
   // ::CalCols() // Muestra Total Items, 7/9/09
   //ENDIF
   // ::ShowTotal()  // JN 29/07

   // JN 02/10/2011
   IF Empty(::cOrderBy) .AND. !Empty(::cItem)
      ::cOrderBy:=" ORDER BY "+::cItem
   ENDIF

   cSql:=::cSql


   IF !Empty(::cLinkGrid)
      cSql:=cSql+ " WHERE " +::GetWhere(::cLinkGrid)
   ENDIF

   cSql:=cSql + " " + ::cOrderBy

   ::cSqlDataQuery:=cSql

   // FIN-JN 02/10/2011


   IF nOption=1


     IF lClear
        AEVAL(::aFields,{|a,n,uValue|uValue:=::Get(a[1]),uValue:=CTOEMPTY(uValue),::Set(a[1],uValue)})
     ENDIF

     ::nOption        :=1
     ::nFileMain      :=0

     // ::Append()
     // ::oBrw:nArrayAt  :=1
     // ::oBrw:nRowSel   :=1
     ::oBrw:aArrayData:={}
     // AADD(::oBrw:aArrayData,::Append())
     // ::oBrw:Refresh(.T.)
     //  ? LEN(::oBrw:aArrayData)
     // ? "NEWJOB",GetProce()
     // ::nOption        :=1
     ::aDataQuery     :={}
     ::NewItem(.T.)

     // ::oBrw:GoBottom()
     // Refresh(.T.)
     // ? LEN(::oBrw:aArrayData),"::aArrayData"
     // ::ShowTotal()  // JN 29/07 , 7/09/09

   ELSE


     IF !Empty(::cFieldFile)
        ::nFileMain :=::Get(::cFieldFile)
     ENDIF

     // ? ::cSql,"ESTE DEBE SER EL QUERY PARA LEER TODO"
     // oTable:=OpenTable(::cSqlData+" WHERE "+::GetWhere(::cLinkGrid) ,.T. )
     // ::aData:=ACLONE(oTable:aDataFill)

/*
     IF Empty(::cOrderBy) .AND. !Empty(::cItem)
        ::cOrderBy:=" ORDER BY "+::cItem
     ENDIF

     cSql:=::cSql

     IF !Empty(::cLinkGrid)
        cSql:=cSql+ " WHERE " +::GetWhere(::cLinkGrid)
     ENDIF

     cSql:=cSql + " " + ::cOrderBy

     ::cSqlDataQuery:=cSql
*/

     oDp:lExcluye:=.F.  // 05/04/2021
     oTable:=OpenTable(cSql , .T. ,::oDb )

     DPWRITE("TEMP\GRID_NEWLOAD"+ALLTRIM(oTable:cTable)+".SQL",cSql)

     // ::cSql+" WHERE "+::GetWhere(::cLinkGrid) + ;
     //                  " "+::cOrderBy , .T. )
     // ? CLPCOPY(oTable:cSql)

     ::aDataQuery:=ACLONE(oTable:aDataFill) // jn 13/12/2013
     //::aFieldGrid:=ACLONE(oTable:aField)
     // AEVAL(::)
     ::aData     :={}

     WHILE !oTable:Eof()
        aLine:={}
        Aeval(::aCols,{|oCol,n|AADD(aLine,oTable:FieldGet(oCol:cField))})
        AADD(::aData,aLine)
        oTable:DbSkip()
     ENDDO

     IF Empty(::aData)
   //     ::NewItem(.T.)
   //      ::oBrw:Disable()
      //? "::aData",len(::aData)
      //  aLine:={}
      //  Aeval(::aCols,{|oCol,n|AADD(aLine,CtoEmpty(oTable:FieldGet(oCol:cField)))})
      //  AADD(::aData,aLine)
     ENDIF

     oTable:End()
     ::nOption:=0

     IF Empty(::aData) .AND. ::oHead:nOption!=0
        // AADD(::aData,::Append())
         ::oBrw:aArrayData:={}
         ::NewItem(.T.)
//       ::nOption:=1

     ELSE

         // ErrorSys(.T.)

         ::oBrw:aArrayData:=ACLONE(::aData)

         IF !Empty(::aData)
        //  ::oBrw:Refresh(.T.)
            ::oBrw:nArrayAt  :=MIN(nArrayAt,LEN(::aData))
            ::oBrw:nRowSel   :=MAX(nRowSel,1)
            ::oBrw:DrawLine(.T.)
            // ::oBrw:Refresh(.T.) // jn 22/04/2016
// ? "qui 1"
         ELSE

            ::oBrw:aArrayData:={}
            ::NewItem(.T.)
            ::nOption:=0
            // ::Cancel()
            ::CancelEdit()
//          AEVAL(::oBrw:aCols,{|a,n|::oBrw:aArrayData[1,n]:=""})
//          ::NewItem(.T.)
//          ::nOption:=0
//          ::oBrw:Disable()
        ENDIF

     ENDIF

     // Aqui debe pintar las columnas con Lista
     // ? len(::oBrw:aArrayData),LEN(::oBrw:aArrayData),"ojo",::nOption,ErrorSys(.T.)

     // nRecno:=::Recno()
     IIF( ::nOption!=1 , ::Gotop() , NIL )

     FOR nRecno:= 1 TO LEN(::oBrw:aArrayData)

          FOR I := 1 TO LEN(::aCols)

          IF ValType(::aCols[I]:aItems)="A" .AND. ::aCols[I]:cType="C"

             nAt:=ASCAN(::aCols[I]:aItems,{|cLine,uValue|uValue:=ALLTRIM(::oBrw:aArrayData[nRecno,I]),;
                                                         LEFT(cLine,Len(uValue))=uValue})
             IF nAt>0
               ::oBrw:aArrayData[nRecno,I]:=::aCols[I]:aItems[nAt]
             ENDIF

             // ? nAt, nRecno,::oBrw:aArrayData[nRecno,1]

          ENDIF

//          IF !::aCols[i]:When()
//             nCol++
//          ENDIF

       NEXT I

     NEXT
/*
     WHILE !::Eof() // .AND. ::oHead:nOption=0

        FOR I := 1 TO LEN(::aCols)

          IF ValType(::aCols[I]:aItems)="A" .AND. ::aCols[I]:cType="C"

             nAt:=ASCAN(::aCols[I]:aItems,{|aLine,uValue|uValue:=ALLTRIM(::Get(::aCols[I]:cField)),;
                                                         LEFT(aLine,Len(uValue))=::Get(::aCols[I]:cField)})

             ? ::Get(::aCols[I]:cField),::aCols[I]:cField,I

             IF nAt>0
                ::Replace(::aCols[I]:cField,::aCols[I]:aItems[nAt])
             ENDIF

          ENDIF

        NEXT

        ::Skip(1)

      ENDDO
*/
      // IIF( ::nOption!=1 , ::Goto(nRecno) , NIL )

   ENDIF

   IIF( !Empty(::cTotal) , ::RunScript(::cTotal)  , NIL )

   ::showtotal()

   ::AdjustBtn()

RETURN .T.

/*
// Carga la Data del Grid
*/
METHOD LoadData() CLASS TDOCGRID

  ::Prepare()

//"AQUI ES LOADDATA"

  IF ::nOption=0
     ::AdjustBtn(.T.)
  ENDIF

  // JN 8/10/09
  IF ::nOption>1
     ::skip(0)
  ENDIF

  ::GetWhere(::cLinkGrid)

  PUBLICO("oGrid",Self)

  IF !Empty(::cLoad)
     ::RunScript(::cLoad)
  ENDIF

  // ? "AQUI ES CUANDO LEEE",::cSqlDataQuery,::cSql,::cWhere

RETURN .T.

/*
// Establece Programa Script
*/
METHOD SetScript(cScript) CLASS TDOCGRID

   DEFAULT cScript:=::cScript

//   IF IsType(cScript,::cScript) .AND. !cScript=::cScript
//      ::oScript:=NIL
//   ENDIF

//   IF Empty(::cScript)
//    ::oScript:=GetScript()
//     ::cScript:=VP("SCRPROGRAM")
//   ELSE
   ::cScript:=cScript
//   ENDIF
//   COMPILA(::cScript)
//   ::oScript:=GetScript()
 // ::oScript:=XCOMPILA(::cScript)

   ::oScript:=GETRUNSCRIP(::cScript)

RETURN ::oScript

METHOD InsertCol(cField,cNameAs,nAt,cFieldPos) CLASS TDOCGRID

    LOCAL oCol:=::GetCol(cField,.F.)

    /*
    // Si Existe, se Devuelve
    */

    IF !oCol=NIL
       RETURN oCol
    ENDIF

    oCol:=TDOCGRIDCOL():New( cField , NIL, Self )

    // Posición del Campo Donde será Insertado
    IF !Empty(cFieldpos)
       nAt:=::GetPosAt(cFieldPos)+1 // Inserta Luego del campo
    ENDIF

    AADD(::aCols,NIL)
    AINS(::aCols,nAt)
    ::aCols[nAt]:=oCol

RETURN oCol

/*
// Agrega Cada Columna
*/
METHOD AddCol(cField,cNameAs,nPos) CLASS TDOCGRID
    LOCAL oCol

//  mensajeErr(cField,cNameAs) // ? cField,"cField"
//  IF !Empty(cNameAs)
//     MensajeErr(cNameAs)
//  ENDIF
// Insertar Columna

   IF ValType(nPos)="N"

     oCol:=TDOCGRIDCOL():New( cField , cNameAs, Self )

     AADD(::aCols,NIL)
     AINS(::aCols,nPos)

     ::aCols[nPos]:=oCol

    ELSE

       AADD(::aCols,TDOCGRIDCOL():New( cField , cNameAs, Self ))

       ::cSqlData:=::cSqlData+ IIF( Empty(::cSqlData),"", ",") + cField

    ENDIF

    ::GetTotalWidth() // Calcula Total de las Columnas

RETURN aTail(::aCols)

/*
// Revisa que el Registro no se Repita
*/
METHOD ChkRecord()
   LOCAL lResp:=.T.,oTable,cSql,cRecord,nAt

   cSql  :="SELECT "+::cPrimary+" FROM "+::cTable+" WHERE "+::GetWhere()

   cRecord:=STRTRAN(SUBS(cSql,AT(" WHERE ", cSql )+1,LEN(cSql))," AND   ", " Y ")

   oDp:lExcluye:=.F.  // 05/04/2021
   oTable:=OpenTable(cSql , .T. ,::oDb) // "SELECT "+::cPrimary+" FROM "+::cTable+" WHERE "+::GetWhere(),.T.)

   DPWRITE("TEMP\GRID"+oTable:cTable+".SQL",cSql)

  //  ? ::GetWhere()

   IF oTable:RecCount()>0 .AND. ::nOption=1
       lResp:=.F.
   ENDIF

   oTable:End()

   IF !lResp
      ::Mensaje("Registro "+cRecord+" ya Existe")
   ENDIF

RETURN lResp

/*
// Asigna Valores al Grid
*/
METHOD Set(cField,uValue,lBrowse)  CLASS TDOCGRID
    LOCAL oCol

    DEFAULT lBrowse:=.F.

    IF !::IsDef(cField) // __objSendMsg(Self,cField)=NIL    // Agrega la ClassData
       __objAddData( Self, cField , uValue )
    ENDIF

    __objSendMsg(Self,cField,uValue)

//    IF cField="MOV_DESCUE"
//      ? cField,uValue,GetProce()
//    ENDIF

    // Debe Pintar el Browse
    // oCol:=::GetCol(cField)
    IF lBrowse .AND.(oCol:=::GetCol(cField,.F.),oCol<>NIL)
    //  ::IsDef(cField) .AND. !Empty(::oBrw:aArrayData)
       oCol:uValue:=uValue

       IF oCol:cType<>ValType(uValue)
          MensajeErr("Tipo de Dato Incorrecto "+CTOO(uValue,"C")+" En Campo "+ oCol:cField)
          uValue:=CTOO(uValue,oCol:cType) // Evitar Salida del Sistema
       ENDIF

       ::oBrw:aArrayData[::oBrw:nArrayAt,oCol:nCol]:=uValue
       ::oBrw:DrawLine(.F.) // JN 07/04/2016 (Pestañea)

// MensajeErr(GETPROCE(),"AQUI DIBUJA EL BROWSE")
    ENDIF

RETURN uValue

/*
// Modificar Registro
*/
METHOD Open()
   // LOCAL I,nCol:=1
   LOCAL nCol:=1,I,nAt

   ::nArrayAt:=::oBrw:nArrayAt
   ::Goto(::oBrw:nArrayAt)
   // ::Skip(0) // Debe leer el Registro
   ::aLineBack:=ACLONE(::oBrw:aArrayData[::oBrw:nArrayAt])
   ::nOption  :=3
   ::cWhere   :=""

   ::adjustBtn(.T.)

// ::oHead:oDlg:Refresh(.t.)
// MensajeErr(lstr(::nOption),"listo")

   ::nLineGet :=::oBrw:nArrayAt
   ::nRowSel  :=::oBrw:nRowSel

//   ? ::cWhere,"Open"

   ::oBrw:SelectCol( 0+1 ) // jn 8/2/2017
   ::oBrw:DrawLine()
   ::oBrw:Select(0) // para seleccionar columnas

//   FOR I := 1 TO  LEN(::aCols)
//      IF !::aCols[i]:When()
//        nCol++
//    ENDIF
// NEXT I

   ::oBrw:nColSel:=nCol
   ::oBrw:SelectCol( nCol )

//   ? nCol,"nCol es la col con permiso"
//   FOR I=1 TO LEN(::aCols)
//       ? ::Get(::aCols[I]:cField) , ::aCols[I]:cField  // ,::oBrw:aArrayData[::nLineGet,::aCols[I]:nCol])
//   NEXT I
   // Debe Pasar los Valores del Array como Campos

   // ::Change()
   // IF ::nOption=3 .AND. !Empty(::cSqlDataQuery) // Modificar

//   ? ::cSqlDataQuery,::nOption ,::GetWhere()

   DEFAULT ::cSqlDataQuery:=""

   IF (::nOption=3 .AND. !Empty(::cSqlDataQuery)) .OR. (::nOption=3 .AND. " LIMIT "$::cSqlDataQuery) // Modificar

     ::Prepare(.T.)
     nAt:=AT(" WHERE ",::cSqlDataQuery)

     // ? ::cSqlDataQuery," method open",nAt
     // Debe Restaurar WHERE

     IF nAt>0

       ::cSqlDataQuery:=LEFT(::cSqlDataQuery,nAt)
       ::cSqlDataQuery:=::cSqlDataQuery+" WHERE "+::GetWhere(::cLinkGrid,.F.)

       // JN 15/01/2015, Repuestos la avenida san cristobal, al modificar documento 0000003, no ordena los items
       IF !Empty(::cOrderBy) .AND. !(::cOrderBy$::cSqlDataQuery)
          ::cSqlDataQuery:=::cSqlDataQuery+::cOrderBy
       ENDIF

     ENDIF

     //? "PORQUE DEBE RECARGAR"
     oDp:lExcluye:=.F.  // JN 05/04/2021 no debe filtrar DPMOVINV x Sucursal (El filtro debe ser por Encabezado)

     ::aDataQuery:=ASQL(::cSqlDataQuery,::oDb)

     IF Empty(::aDataQuery)
        RETURN .F.
     ENDIF

   ENDIF

   ::Change(.T.)
   ::LoadData() // JN 21/05/2007
   ::Skip(0)

   IF ::oBrw:nArrayAt>LEN(::aDataQuery)
     MensajeErr("Error en lectura de Query "+CLPCOPY(::cSqlDataQuery))
     RETURN .F.
   ENDIF

   AEVAL(::aFields,{|a ,n , uValue | uValue:=::aDataQuery[::oBrw:nArrayAt,n],::Set(a[1],uValue)})

   ::cWhere   :=::GetWhere(NIL,.T.)  // Necesario Para Modificar

   IF !EMPTY(::aMemo)
       ::aMemo[7]:=IIF( ::nOption=1 , 0 , ::Get(::aMemo[1]))
       ::aMemo[8]:=""
       ::aMemo[9]:=""

       IF ::nOption<>1  .AND. !Empty(::aMemo[7])

         SQLGET(oDp:cDpMemo,"MEM_MEMO,MEM_DESCRI","MEM_NUMERO"+GetWhere("=",::aMemo[7]))

         IF !Empty(oDp:aRow)
           ::aMemo[8]:=oDp:aRow[1]
           ::aMemo[9]:=oDp:aRow[2]
         ENDIF

          // ? "debe leer el memo",::Get(::aMemo[1]),::aMemo[1]

       ENDIF

   ENDIF

   IF ::nOption<>1 .AND. !Empty(::cFieldFile)
     ::nFileMain :=::Get(::cFieldFile)
   ENDIF

   ::RunScript(::cLoad)
   ::Edit(.T.)
   ::OpcSay()

   ::oBrw:SetFocus()

RETURN NIL

/*
// Calcula el Valor de una Columna
*/
METHOD ColCalc(cField)
   LOCAL nCol

   nCol:=ASCAN(::aCols,{|o|o:cField=cField})

   IF nCol>0
      ::aCols[nCol]:RunCalc()
   ELSE
      MensajeErr("Columna "+cField,"No Existe, GRID:ColCal(")
   ENDIF

RETURN .T.

/*
// Teclado KeyBoard
*/
METHOD KeyBoard(nKey)  CLASS TDOCGRID
   LOCAL I,nCol,uValue

   // ? nKey,"TDOCGRID"
   IF ::oHead:nOption=0

      ::CancelEdit(.T.)
      EVAL(::oHead:oDlg:bKeyDown , nKey , 0 )
      RETURN .T.

   ELSE

//      ::oHead:RunKey(nKey ,0)

   ENDIF

   // Puede Haber Campos Logicos

   IF ::nOption<>0 .AND. ValType(::oBrw:aArrayData[::oBrw:nArrayAt,::oBrw:nGetColSel()])="L"

//   ? "AQUI DEBE HABER LOGICO",::oBrw:nGetColSel()
      IF nKey=83 .OR. nKey=78  // Presiono SI o NO
         EVAL(::oBrw:aCols[::oBrw:nGetColSel()]:bLDClickData)
         RETURN .T.
      ENDIF

      ::aCols[::oBrw:nGetColSel()]:Valid(::oBrw:aArrayData[::oBrw:nArrayAt,::oBrw:nGetColSel()],13,::oBrw:aCols[::oBrw:nGetColSel()])
      ::IsFinish(.T.)
//   ? "DEBE FINALIZAR"

      RETURN .T.

   ENDIF

   DO CASE

      CASE nKey=13 .AND. ::nOption=0 .AND. !Empty(::oHead:oExport)

         // ? ::oHead:oExport:ClassName()

/*         IF ::oHead:oExport:ClassName()="TDOCGRIDCOL"
            ? "AQUI DEBE EXPORTAR"
            ::oGrid:oHead:Close()
            ::RunDblClick()
         ELSE */
         ::RunDblClick()
//       ENDIF

      CASE nKey=VK_F6 // Editar ListBox


         nCol:=ASCAN(::aCols,{|oCol|!Empty(oCol:cListBox)})
         nCol:=::oBrw:nGetColSel()

         IF nCol>0 .AND. !Empty(::aCols[nCol]:cListBox)

            uValue:=::aCols[nCol]:ListBox()

            IF !Empty(uValue)
               // Ejecuta postEdit
               ::aCols[nCol]:PostEdit( ::oBrw:aCols[nCol] , uValue )
               ::aCols[nCol]:GoNextCol(nCol)
            ENDIF

            // ::oBrw:aCols[nCol]:Edit(.T.)
         ENDIF

       OTHER

         ::Runkey(nKey)

   ENDCASE

RETURN NIL

/*
// Obtiene Número de Columna
*/
METHOD GetCol(cField,lMsg) CLASS TDOCGRID
   LOCAL oCol,nAt

   DEFAULT lMsg:=.T.

   cField:=UPPE(cField)

   nAt:=ASCAN(::aCols , {|oCol|oCol:cField=cField} )

   IF nAt=0

      IF( lMsg ,  MensajeErr("Campo "+cField+" no Existe") , NIL )
      RETURN NIL

   ELSE

      ::aCols[nAt]:oCol:=::oBrw:aCols[nAt] // 07/04/2024 Obtengo la columna del browse

   ENDIF

RETURN ::aCols[nAt]

/*
// Activa el Modo Edición
*/
METHOD Edit(lEdit)  CLASS TDOCGRID
   LOCAL I,nCol:=1

   DEFAULT lEdit:=.T.

   ::nArrayAt:=::oBrw:nArrayAt // JN 09/03/2017

   Aeval(::oBrw:aCols,{|oCol|oCol:KillGet(),oCol:nEditType:=0})

   AEVAL(::aCols,{|oCol,n|::oBrw:aCols[n]:bOnPostEdit  :=PutOnEdit(oCol,Self,n)})

   // ::Find(.f.) // quitar buscar
   // ? "aqui debe reactivarse el postedit"

   ::Prepare()

   ::bWhen:=BloqueCod(::bWhen)

   AEVAL(::aCols,{|oCol,n|oCol:lRunValid:=.F.,oCol:Edit(lEdit)  })

   ::lEdit:=lEdit

   IF !::lEdit
     ::oBrw:nMarqueeStyle :=::nMarqueeStyle // MARQSTYLE_HIGHLROW
     ::oBrw:Select( 1 )
     ::oBrw:bSelect   :={||IIF( ::nOption=0 ,::Open() ,NIL )} // Modifica con Enter
   ELSE
     // ::oBrw:KeyBoard(13)
     // ::oBrw:Drawline()
     // ::adjustBtn()
     ::oBrw:bSelect        :=NIL
     ::oBrw:nMarqueeStyle  := ::nMarqueeStyle // 2 //MARQSTYLE_HIGHLROW

//   ? ValType(::oSayOpc),"::oSayOpc"

   ENDIF

   FOR I := 1 TO LEN(::aCols)

     IF !::aCols[i]:When()
        nCol++
     ELSE
        EXIT
     ENDIF

   NEXT

//   IF ::nOption=3
//     oDp:oFrameDp:SetText(::oBrw:aCols[1]:oEditGet:ClassName())
//   ENDIF
//   AEVAL(::oBrw:aCols, {|oCol,n| oCol:bKeyAction:={|nKey| ::runkey(nkey)} })

   IF !lEdit
     AEVAL(::oBrw:aCols, {|oCol,n| oCol:bKeyAction:={|nKey| ::runkey(nkey)} })
   ELSE
     AEVAL(::oBrw:aCols, {|oCol,n| oCol:bKeyAction:=NIL })
   ENDIF

RETURN .T. //RRETUR N .T.

/*
// Agrega un Nuevo Item
*/
METHOD NewItem(lCrear)  CLASS TDOCGRID
  // LOCAL aLine

  DEFAULT lCrear:=.F.  // Hace Append

//  ? "newitem",getproce()
//  ::Prepare()
//  IF !Empty(::cLoad)
//     ::RunScript(::cLoad)
//  ENDIF
  ::nOption:=1
  ::cDataAudita:="" // Clave para detecar Cambios

  IF lCrear .OR. Empty(::oBrw:aArrayData)

     ::lEdit  :=.F.
     ::aData:=ACLONE(::oBrw:aArrayData)
     AADD(::oBrw:aArrayData,::Append())
     // ::oBrw:Drow
     ::oBrw:Refresh(.T.) // 22/04/2016 innecesario
     // ? LEN(::oBrw:aArrayData),"NUEVO"
     // ::oBrw:nGetColSel():=1
     //::oBrw:aArrayData:=ACLONE(::aData)
     // ::oBrw:DrawLine(.T.)
     // ? "AQUI ES 2"

  ENDIF

  ::oBrw:GoBottom() // 22/09/2016 revisar
  ::oBrw:nColSel:=1 // JN 08/02/2017
  ::oBrw:DrawLine(.T.)

  ::aLineBack   :={}
  ::oBrw:nColSel:=1
  ::nlineGet:=LEN(::oBrw:aArrayData)
  ::nRowSel :=::oBrw:nRowSel
  //::nOption :=1
  ::oBrw:SetFocus()

   IF !EMPTY(::aMemo)
       ::aMemo[7]:=IIF( ::nOption=1 , 0 , ::Get(::aMemo[1]))
       ::aMemo[8]:=""
       ::aMemo[9]:=""
   ENDIF

   IIF( !Empty(::cItem) , ::Set(::cItem," "), NIL )

  //? "AQUI DEBE ESTAR VACIO",::Get(::cItem),::cItem

  IIF( !Empty(::cLoad)  , ::RunScript(::cLoad)   , NIL )

  ::OpcSay()
  ::Edit(.T.)

// ? "newitem",getproce()

  //  ? "LISTO PORQUE DOS?",LEN(::oBrw:aArrayData)
  // ::oBrw:Disable()

RETURN .T.

/*
// Activa el Browse
*/
METHOD Activate() CLASS TDOCGRID

  LOCAL oTable,I,aBtnTip:={},nAt ,oDoc:=SELF,oCol
  LOCAL cCol:="",oColGrid

/*
  IF ::lSwapLine
     ::AddBtn("UP.BMP","Subir","oGrid:nOption=0  .AND. oGrid:RecCount()>1",;
                  [EJECUTAR("GRIDSWAPLINE",oGrid,.T.)],"SUB",STR(0))
     ::AddBtn("DOWN.BMP","Subir","oGrid:nOption=0 .AND. oGrid:RecCount()>1 ",;
                  [EJECUTAR("GRIDSWAPLINE",oGrid,.F.)],"SUB",STR(0))
  ENDIF
*/

//  IF !Empty(::cItem)
//     ::cSqlData:=::cSqlData+","+::cItem // Agrega el Item
//  ENDIF
//  ? "ACTIVATE",ERRORSYS(.T.   ) ,::cSql
//  ::cSqlData:="SELECT "+::cSqlData+" FROM "+::cTable
//  oTable    :=OpenTable(::cSqlData , .F.)
//  ::aEmpty  :={}
//
//  AEVAL(oTable:aFields,{|a,n|AADD(::aEmpty,CtoEmpty(oTable:FieldGet(n)))})
//
//  oTable:End()

  DEFAULT ::aData:={}

//  IF !Empty(::oHead:uData)
/*
     ::Prepare()
     oTable:=OpenTable(::cSqlData+" WHERE "+::GetWhere(::cLinkGrid) ,.T. )
     ::aData:=ACLONE(oTable:aDataFill)
     oTable:End()
     ::nOption:=0
*/
//  ENDIF
//   ErrorSys(.T.)


//   ? ::aData,LEN(::aData)

//   IF Empty(::aData)

   ::nOption:=1
   AADD(::aData,::Append())

   IF !Empty(::cScript)
      ::SetScript(::cScript)
   ENDIF

   // ? len(::aData)
   // ? LEN(::aData),::aData[1],::cPrimary ,"A",::aData[1,1]
   // IF Empty(::aData)
   //    ? "Error"
   // ENDIF

   ::oBrw := TXBrowse():New( ::oDlg ) // ::oHead:oDlg )

   // ::oBrw:nColDividerStyle    := LINESTYLE_BLACK
   // ::oBrw:nRowDividerStyle    := LINESTYLE_BLACK
   // ::oBrw:lColDividerComplete := .t.
   // ::oBrw:nMarqueeStyle       := MARQSTYLE_HIGHLROW

   DEFAULT ::bClrHeader  :={||{NIL,::nGris}},;
           ::bClrFooter  :={||{NIL,::nGris}}

   IF !Empty(::nClrPaneH)
      ::bClrHeader  :={||{::nClrTextH,::nClrPaneH}}
   ENDIF

   IF !Empty(::nClrPaneF)
      ::bClrFooter  :={||{::nClrTextF,::nClrPaneF}}
   ENDIF

   IF ::nStyle=0
     ::oBrw:nColDividerStyle   :=::nColDividerStyle // LINESTYLE_BLACK
     ::oBrw:nRowDividerStyle   :=::nRowDividerStyle // LINESTYLE_BLACK
     ::oBrw:lColDividerComplete:=::lColDividerComplete // División completa columnas
   ENDIF

   IF ::nStyle=1
     ::oBrw:nMarqueeStyle       := MARQSTYLE_HIGHLCELL
     ::oBrw:nColDividerStyle    := LINESTYLE_BLACK
     ::oBrw:nRowDividerStyle    := LINESTYLE_BLACK
     ::oBrw:lColDividerComplete := ::lColDividerComplete
     ::oBrw:nMarqueeStyle       := MARQSTYLE_HIGHLROW
   ENDIF


 //   DATA nMarqueeStyle    INIT MARQSTYLE_SOLIDCELL  // MARQSTYLE_HIGHLROW
//   DATA nColDividerStyle INIT LINESTYLE_BLACK
//   DATA nRowDividerStyle INIT LINESTYLE_BLACK

   ::oBrw:lHScroll         := ::lHScroll
   ::oBrw:lFastEdit        := .F.
   ::oBrw:lEditMode        :=.T.
   ::oBrw:oFont            :=::oFont
   ::oBrw:lAllowRowSizing  :=.F.
   ::oBrw:lAllowColSwapping:=.F.
   ::oBrw:nHeaderLines     :=::nHeaderLines
   ::oBrw:lFooter          :=::lTotal
   ::oBrw:bClrHeader       :=::bClrHeader
   ::oBrw:bClrFooter       :=::bClrFooter

//   AEVAL(::aCols,{|oCol,n|::oBrw:aCols[n]:cHeader:=oCol:cTitle})
/*
 :: oBrw:lHScroll        := ::lHScroll
 oBrw:lVScroll        := ::lVScroll
 oBrw:nRowSel         := 1
 oBrw:lRecordSelector := .F.
*/
    ::oBrw:SetArray(::aData,.F.)

//    IF !Empty(::cItem)  // Debe Borrar la Ultima Columna del Get
//       ? LEN(::aData),LEN(::aData[1])
//       ::oBrw:DelCol( LEN(::aData[1] ) )
//    ENDIF

    // Fuentes para los Encabezados
IF .T.

    AEVAL(::aCols,{|oCol,n|oCol:nCol:=n,;  // Establece Numero de Columna
                           oCol:cTitle:=STRTRAN(oCol:cTitle,";",CRLF),;
                           PutValid(::oBrw:aCols[n] , oCol , SELF) ,;
                           ::oBrw:aCols[n]:cSayPicture  :=oCol:BuildPicture()   ,;
                           ::oBrw:aCols[n]:cHeader      :=oCol:cTitle           ,;
                           ::oBrw:aCols[n]:nWidth       :=oCol:nWidth           ,;
                           ::oBrw:aCols[n]:nDataStrAlign:=oCol:nDataStrAlign    ,;
                           ::oBrw:aCols[n]:nHeadStrAlign:=oCol:nHeadStrAlign    ,;
                           ::oBrw:aCols[n]:bOnPostEdit  :=PutOnEdit(oCol,Self,n),;
                           ::oBrw:aCols[n]:cEditPicture :=oCol:cPicture         ,;
                           ::oBrw:aCols[n]:bStrData     :=BrwViewData(::oBrw,n,oCol) ,;
                           ::oBrw:aCols[n]:bClrStd      :=BrwColorData(::oBrw,n,Self,oCol),;
                            NIL})
    /*
    // Si la Columna tiene ; , es convertida en CRLF y el encabezado será dos lineas
    */
    IF ASCAN( ::aCols,{|oCol,n| CRLF$oCol:cTitle})>0
      ::nHeaderLines:=2
    ENDIF

    ::oBrw:bGotFocus  :={||::BrwGotFocus()}
    ::oBrw:bLostFocus :={||::BrwLostFocus()}
    ::oBrw:bChange    :={||oDoc:nAtData:=::oBrw:nArrayAt,::prepare(), IIF( oDoc:nOption!=0, oDoc:Cancel(.F.) , oDoc:Change(.T.) )}
    // Aqui Agregar persimo para modificar OJO
    ::oBrw:bLDblClick:={||IIF( ::nOption=0 .AND. ::oHead:nOption>0 .AND. ::lMod, ::RunDblClick(),NIL),;
                          IIF( ::oHead:nOption=0 , ::RunOff() , NIL) }
    ::oBrw:bKeyChar  :={| nKey | ::RunKey(nKey) }
    //  oBrw:bKeyChar
    ::oBrw:nFreeze   :=0 // LEN(::aCols)
    ::oBrw:bKeyDown  :={ |nKey| ::KeyBoard(nKey)  }
   // ::oBrw:nRecSelColor:=::nGris
   // ::oBrw:bClrHeader  :={||{NIL,::nGris}}

    IF !EMPTY(::nClrPane1) .AND. !EMPTY(::nClrPane2)
//      ::oBrw:SetColor(CLR_GRAY,CLR_YELLOW) // ::nClrText,::nClrPane2)
      ::oBrw:bClrStd := {|| { ::GridColorText(), iif( ::oBrw:nArrayAt%2=0, ::nClrPane1  ,   ::nClrPane2 ) } }
    ELSEIF !EMPTY(::nClrPane1)
//      ::oBrw:SetColor(::nClrText,::nClrPane1)
      // ::oBrw:bClrStd := {|| {::nClrText, ::nClrPane1 } }
      ::oBrw:bClrStd := {|| {::GridColorText(), ::nClrPane1 } }
    ENDIF

    // Busca los Campos Lógicos
    FOR I=1 TO LEN(::aCols)


       IF ValType(::aData[1,I])="L" // Lógico

          oCol:=::oBrw:aCols[I]

//          oCol:AddBmpFile("BITMAPS\xCheckOn.bmp")
//          oCol:AddBmpFile("BITMAPS\xCheckOff.bmp")

          oCol:AddBmpFile("BITMAPS\checkverde.bmp")
          oCol:AddBmpFile("BITMAPS\checkrojo.bmp")

          oCol:bBmpData:=PutBmpData(::oBrw,I,oCol,Self)
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    :={||""}
          oCol:bLDClickData:={|| ::ItemSelect()}
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)

       ELSE

          ::oBrw:aCols[I]:bClrStd     :=GetBlq(SELF,::aCols[I]:bClrStd     )
          ::oBrw:aCols[I]:bClrFooter  :=GetBlq(SELF,::aCols[I]:bClrFooter  )
          ::oBrw:aCols[I]:bClrHeader  :=GetBlq(SELF,::aCols[I]:bClrHeader  )
          ::oBrw:aCols[I]:bClrSel     :=GetBlq(SELF,::aCols[I]:bClrSel     )
          ::oBrw:aCols[I]:bClrSelFocus:=GetBlq(SELF,::aCols[I]:bClrSelFocus)
          ::oBrw:aCols[I]:bClrEdit    :=GetBlq(SELF,::aCols[I]:bClrEdit    )

       ENDIF

    NEXT I

ENDIF

    ::ShowTotal()
    AEVAL(::oBrw:aCols,{|oCol,n|oCol:oHeaderFont:=::oFontH,oCol:oFooterFont:=::oFontF,oCol:nPos:=n}) // 21/11/2024 nPos:=n Para ser reemplazada oCol:nGetColSel()

    IF ::nRecSelColor=NIL
      ::nRecSelColor:=::nGris
    ENDIF

    ::oBrw:nRecSelColor := ::nRecSelColor

    ::oBrw:lAllowColSwapping:=.F.


    ::oBrw:CreateFromCode()

    ::oBrw:lAllowRowSizing  :=.F.
    ::oBrw:lAllowColSwapping:=::lSwapCol // Usuario no podrá intercambiar las columnas

    // ? ::nOption,"OPCTION"
    // ::oBrw:Hide()
    // ::oBrw:nWidth:=0
    // ::oBrw:Move(0,0,0,0) // SetSize(0,0)

    // AEVAL(::oBrw:aCols,{|oCol|oCol:oHeaderFont:=::oFontH})

    // ::oBrw:Refresh(.t.)
    // Crea las Claves Primaria

    IF Empty(::cPrimary)

      ::cPrimary:=""

      AEVAL(::aCols,{|oCol,n| IIF( oCol:lPrimary , (::cPrimary:=::cPrimary + ;
                              IIF( Empty(::cPrimary),"" ,"," ) + oCol:cField) , NIL )})

      ::cPrimary:=::cLinkGrid + IIF( Empty(::cPrimary ) , " ", "," ) + ::cPrimary

    ENDIF

    // ::CreateBtn()
    // ::oBrw:SetFocus() // EVAL(::oBrw:bChange) // Necesita Mostrar los Datos de la Cuenta

    IF !::lBtnBrow // No Incluye los Botones de Navegacion

       aBtnTip:={"<< "," < "," > ",">> "}

//       ErrorSys(.T.)
       i:=0
       AEVAL(aBtnTip,{||i++,nAt:=ASCAN(::aBtn,{|aBtn|aBtn[6]=aBtnTip[i]}),;
                        IIF( nAt>0 , ARREDUCE(::aBtn,nAt) , NIL )})

    ENDIF

/*
? "aqui"

    ::AddBtn("xsave2.BMP","Guardar (Ctrl-G)","oGrid:nOption!=0",;
                                             "oGrid:BtnSave()" ,;
                                             "Gra",STR(DP_CTRL_G))
*/

    // Debe ser el Primer Boton, JN 22/11/2012
//    AADD(::aBtn,{11,"xSave2.bmp"   ,"Salvar         "   , "oGrid:nOption!=0 " , "oGrid:BtnSave()"   ,"Gra",STR(DP_CTRL_G)})

    IF !Empty(::cFieldAud)

      ::AddBtn("AUDITORIA2.BMP","Visualizar Auditoria","oGrid:nOption=0",;
                     [EJECUTAR("GRIDVIEW_AUDITOR",oGrid)],"AUD",NIL)

    ENDIF

    IF ::lBar
       AADD(::aBtn,{10,"xSalir.bmp"   ,"Cerrar"            ,"oGrid:nOption=0 "   ,"oGrid:oHead:Close()","Fin",STR(DP_CTRL_Q)})
    ENDIF

//  AADD(::aBtn,{11,"xSave2.bmp"   ,"Salvar         "   , "oGrid:nOption!=0 " , "oGrid:BtnSave()"   ,"Gra",STR(DP_CTRL_G)})

    // Botones de Edicion
    FOR I := 1 TO LEN(::aBtnEdit)

        AADD(::aBtn,{I,::aBtnEdit[I,1],; // Bmp
                       ::aBtnEdit[I,2],; // ToolTip
                       ::aBtnEdit[I,3],; // Condicion
                       ::aBtnEdit[I,4],; // Ejecución
                       ::aBtnEdit[I,5],; // Tipo de Boton
                       ::aBtnEdit[I,6]}) // Combinacion de Teclas


    NEXT

    AADD(::aBtn,{I+12,"xCancel2.bmp" ,"Cancelar       "   , "oGrid:nOption!=0 " , "oGrid:Cancel(.T.)" ,"Esc",STR(DP_CTRL_C)})
//  AADD(::aBtn,{11,"xSave2.bmp"   ,"Salvar         "   , "oGrid:nOption!=0 " , "oGrid:BtnSave()"   ,"Gra",STR(DP_CTRL_G)})

    ::Prepare()

//    IIF( !::lBar , ::CreateBtn(::oHead:oDlg) , NIL )
    // Crea los Botones Adicionales
   IIF( !::lBar , ::CreateBtn(::oDlg) , NIL )

/*
    FOR I := 1 TO LEN(::aBtn)

      IF ::nBtnStyle=0
         ::aBtn:={}
         EXIT
      ENDIF

      IF ::nBtnStyle=1

      @ ::aSize[1]+::aSize[3]+11 , (::aBtnSize[2]+1)*(i-1)  XBUTTON ::aBtn[i,1] PIXEL;
          FILE oDp:cPathBitMaps+::aBtn[i,2];
          NOBORDER;
          SIZE ::aBtnSize[1],::aBtnSize[2];
          MESSAGE ::aBtn[i,3];
          TOOLTIP ::aBtn[i,3];
          COLORS oDp:nGris,NIL

      ELSE

         @ ::aSize[1]+::aSize[3]+11 , (::aBtnSize[2]+1)*(i-1)  BUTTON ::aBtn[i,1];
             PROMPT ::aBtn[I,6];
             FONT ::oFontBtn;
             SIZE ::aBtnSize[1],::aBtnSize[2]


         // ::aBtn[I,1]:cMessage:=::aBtn[i,3]
         ::aBtn[I,1]:cToolTip:=::aBtn[i,3]

      ENDIF

      ::aBtn[I,1]:bWhen  :=GenBlock("oPar1:Prepare(),"+::aBtn[I,4],Self)
      ::aBtn[I,1]:bAction:=GenBlock("oPar1:Prepare(),"+::aBtn[I,5],Self)

    NEXT
*/
    // ::aBtn[1,1]:ForWhen()

    ::nOption:=1

    ::cPrimary:=IIF( Left(::cPrimary,1)="," , Subs(::cPrimary,2,LEN(::cPrimary)),::cPrimary )

    EJECUTAR("TDOCGRIDACTIVATE",Self)

//    ::cBrwState:=::oBrw:SaveState()
    // AEVAL(::aCols,{|oCol,n|oCol:ShowTotal()})

RETURN NIL

/*
// Creación de Botones
*/
//METHOD CreateBtn()
//RETURN .T.

/*
// Ejecucion segun Tecla
*/
METHOD RunKey(nKey,oGet) CLASS TDOCGRID
    LOCAL nAt
    LOCAL bBlq:={||.T.}

    IF ::oHead:nOption=0 .AND. ::nOption=0
       RETURN NIL
    ENDIF

    IF !::oHead:nOption=0  .AND. nKey=VK_F10
      MensajeErr("GRABAR DOC")
    ENDIF

//   oDp:oFrameDp:SetText(nKey,"RUNKEY")
// +;
// ::oBrw:aCols[::nColEdit]:oEditGet:ClassName())

    DO CASE

       CASE (nKey = 17 .OR. nKey = 18)  // --- primera vez (debe agregar)

          ::cKey := LSTR(nKey)
          ::nTime:= SECONDS()

          // oDp:oFrameDp:SetText("Empezamos:"+LSTR(SECONDS() - ::nTime))

          RETURN .T.

       // --- Otra tecla... y tenia presionada ctrl o alt (debe agregar) y menos de un segundo en tiempo
      CASE (nKey <> 17 .AND. nKey <> 18) .AND. LEN(::cKey)=2

          IF ABS( SECONDS() - ::nTime ) < 1
             ::cKey := ::cKey + ALLTRIM(STR(nKey))
             //::Cancel()  // Aqui realmente debe ejecutarse una busqueda entre los botones disponibles... (RIGC)
          ENDIF
          //? ::cKey
          //? ::aBtn[1,7]
          nAt:=ASCAN( ::aBtn  , {|a,n| LEFT(ALLTRIM( a[7] ),4) = ::cKey })

          IF nAt>0

             ::nColEdit:=::oBrw:nGetColSel()
             ::aCols[::nColEdit]:lValid:=.F. // no debe validar
             Eval(::aBtn[nAt,1]:bAction)
             ::aCols[::nColEdit]:lValid:=.T. // debe validar

          ENDIF

          ::cKey:=""

          RETURN .T.

    ENDCASE

//?? nKey, "GRID ", ::cKey

    ::cKey:=""
    // return .T.

     IF nKey==VK_F9
// ?? "pppppp"
        ::oHead:PreSave(.T.)
     ENDIF
// ?? "oGrid ",nKey

     DO CASE

       CASE (nKey=73 .OR. nKey=105)  .AND. ::nOption=0 // Incluir

         ::NewItem(.T.)
         ::AdjustBtn(.T.)

       CASE (nKey=69 .OR. nKey=101) .AND. ::nOption=0  // Delete

         ::PreDelete(.F.)

       CASE (nKey=77 .OR. nKey=109) .AND. ::nOption=0 // Modificar

         ::Open()

       CASE (nKey=66 .OR. nKey=98)  .AND. ::nOption=0 // Buscar

         ::SetFind(.t.)

       CASE nKey=43 .AND. ::nOption=0  // Buscar Más

         ::aCols[::oBrw:nGetColSel()]:Find(::aCols[::oBrw:nGetColSel()]:uValueFind,.T.)

    OTHER

      IF ::oHead:nOption=0 .AND. ::nOption=0
        RETURN .T.
      ENDIF

      // ? nKey,"nKey",::nOption,::oHead:nOption

       // ::oBrw:SetFocus()
    ENDCASE

RETURN .T.
/*
// Prepara el Grid
*/
METHOD Prepare(lFocus,lView) CLASS TDOCGRID
   LOCAL aVarHead:={},I,cMemo:="",aLine
   LOCAL aVarGrid:={},oGrid:=SELF

   DEFAULT lFocus:=.T.,;
           lView :=.F.

   // IF lFocus .AND. ::oHead:oGridFocus<>NIL
   //    oGrid:=::oHead:oGridFocus
   // ENDIF

   // ::oBrw:bLostFocus:=NIL
   // Debe Pasar los Valores de la Cabeza hacia el Grid
   aVarHead:=_Vector(STRTRAN(::cLinkHead,"+",","))
   aVarGrid:=_Vector(STRTRAN(::cLinkGrid,"+",","))

   // IF LEN(aVarHead)<>LEN(aVarGrid)
   // IF LEN(aVarGrid)<LEN(aVarHead)
   //   MensajeErr(::cLinkHead+CRLF+CRLF+"Vs"+CRLF+::cLinkGrid,"Vinculo Documento Grid Requiere Campos Homogéneos")
   // ENDIF

// ? ErrorSys(.T.),"PREPARE"
// ? ErrorSys(.T.),LEN(aVarHead),LEN(aVarGrid   ),::cLinkHead,::cLinkGrid
// ViewArray(aVarHead)

   ::cDataHead:=""
   FOR I := 1 TO LEN(aVarGrid)
      ::Set(aVarGrid[I],::oHead:Get(aVarHead[I]))
     // ::cDataHead:=::cDataHead+CTOO(::oHead:Get(aVarHead[I]),"C")
   NEXT

//   IIF( ::oHead=NIL, NIL , ::oHead:Prepare() )

   __MVPUBLIC("OGRID")   // ::aPublics[nFor][VAR_NAME])
   __MVPUT("OGRID",oGrid) // ::aPublics[nFor][VAR_NAME], ::aPublics[nFor][VAR_VALUE])

   __MVPUBLIC("OHEAD")       // ::aPublics[nFor][VAR_NAME])
   __MVPUT("OHEAD",::oHead)  // ::aPublics[nFor][VAR_NAME], ::aPublics[nFor][VAR_VALUE])
   __MVPUT("ODOC" ,::oHead)  // ::aPublics[nFor][VAR_NAME], ::aPublics[nFor][VAR_VALUE])

   __MVPUBLIC(::oHead:cVarName)      // ::aPublics[nFor][VAR_NAME])
   __MVPUT(::oHead:cVarName,::oHead) // ::aPublics[nFor][VAR_NAME], ::aPublics[nFor][VAR_VALUE])

   PUBLICO("oGrid",oGrid)
   MOVER(oGrid  ,"oGrid")
   MOVER(::oHead,::oHead:cVarName)

RETURN .T.

/*
// Ejecutar Doble/Click
*/
METHOD RunDblClick() CLASS TDOCGRID

    IF !Empty(::oHead:oExport)
       AEVAL(::aFields,{|a ,n , uValue | uValue:=::aDataQuery[::oBrw:nArrayAt,n],::Set(a[1],uValue)})
       ::oHead:RunExport()
       RETURN NIL
    ENDIF

    IF ::oHead:nOption<>0
       ::Open()
//    ELSE
//       ::CancelEdit()
    ENDIF

//    IF ::nOption=0
//       ? "::nOption,OFF"
//    ENDIF

RETURN NIL

/*
// Se mueve cada Vez que se Cambia el Browse
*/
METHOD Change(lChange) CLASS TDOCGRID

    DEFAULT lChange:=.T.

//   AEVAL(::aCols  ,{|oCol,n,uValue|uValue:=::oBrw:aArrayData[::oBrw:nArrayAt,oCol:nCol],::Set(oCol:cField,uValue)})

   ::Prepare()

// ? ::bChange,"::bChange"

   ::bChange      :=BloqueCod(::bChange)

   ::oBrw:nArrayAt:=MAX(1,::oBrw:nArrayAt)
   ::oBrw:nArrayAt:=MIN(Len(::oBrw:aArrayData),::oBrw:nArrayAt)

   IF lChange .AND. ::nOption<>1 .AND. !Empty(::aDataQuery) .AND. ::oBrw:nArrayAt<=Len(::aDataQuery) // No es Necesario Cuando incluye
      ::lEdit:=.F.
      AEVAL(::aFields,{|a ,n , uValue | uValue:=::aDataQuery[::oBrw:nArrayAt,n],::Set(a[1],uValue)})
      EVAL(::bChange,Self)
   ENDIF

RETURN .T.

/*
// Trata de Cancelar la Transacción
*/
METHOD Cancel(lAuto)

    LOCAL nRowSel :=::nRowSel,nArrayAt:=::oBrw:nArrayAt

    DEFAULT lAuto:=.F.

    // ? ::lEdit,"::lEdit"
    // Esta Incluyendo y Presiona Cancelar, Solo existe una Línea
    IF LEN(::oBrw:aArrayData)=1 .AND. ::nOption=1 .AND. ::lBar  .AND. !::oHead:lBar .AND. lAuto
  //      ? "AQUI 1"
        ::oHead:Cancel()
        RETURN .T.
    ENDIF

    IF !::lEdit .OR. (LEN(::oBrw:aArrayData)=1 .AND. ::nOption=1)
//      ? "AQUI OJO 2"
       RETURN NIL
    ENDIF

    // Modifica
    IF ::nLineGet==::oBrw:nArrayAt .AND. (::nOption=3.OR.::nOption=1) .AND. !lAuto
//       ? "AQUI 3"
       RETURN NIL
    ENDIF

    // ::AdjustBtn()
    //? ::nLineGet,::oBrw:nArrayAt,::nRowSel
    ::oBrw:SetFocus()

    IF (LEN(::oBrw:aArrayData)=1 .AND. ::nOption=1 .AND.  !::nLineGet==::oBrw:nArrayAt) .OR. (::nOption!=1 .AND. !::nLineGet==::oBrw:nArrayAt) // Es el Primero
       // ? "aqui EDITA NO SE PUEDE IR" , ::nLineGet==::oBrw:nArrayAt
       ::CancelEdit()
       RETURN .T.
    ENDIF
/*
    IF ASCAN(::aCols,{|oCol,n|oCol:lChange})>0 .AND. ;
       !MsgNoYes("Desea Cancelar")
       ::oBrw:nArrayAt:=::nLineGet
       ::oBrw:nRowSel :=::nRowSel
       ::nOption:=0
       ::CancelEdit()
       // ::AdjustBtn(.T.)
       ::oBrw:Refresh()
       ::oBrw:SetFocus()
//       ::Change()
       RETURN .F.
    ENDIF
*/

    IF ::nOption=1

       ADEL(::oBrw:aArrayData,LEN(::oBrw:aArrayData)   )
       ASIZE(::oBrw:aArrayData,LEN(::oBrw:aArrayData)-1)
       ::oBrw:nArrayAt:=LEN(::oBrw:aArrayData)
       ::oBrw:Refresh() // JN 22/04/2016

//     ? "aqui estaba incluyendo"
//     ? LEN(::aDataQuery),LEN(::oBrw:aArrayData),"len de ambos"

    ENDIF

    IF ::nOption<>1 .AND. !Empty(::aLineBack)
      // ::oBrw:nArrayAt:=::nLineGet
      // ::oBrw:nRowSel :=::nRowSel
         ::oBrw:aArrayData[::nLineGet]:=ACLONE(::aLineBack)
      // ::oBrw:Refresh() // Drawline(.T.)
      // ? "AQUI DIBULA LA LINEA ANTERIOR"
      // ::oBrw:nRowSel :=nRowSel
      // ::oBrw:nArrayAt:=nArrayAt
      // JN 22/04/2016 ::oBrw:Refresh() // Drawline(.T.)
      ::oBrw:SetFocus()
    ENDIF

    ::nOption :=0
    // ::nLineGet:=0
    ::Edit(.F.)
    ::CancelEdit()
    // ::AdjustBtn(.T.)
    ::oBrw:SetFocus()

RETURN .T.

/*
// Pregrabar
*/
METHOD PreSave() CLASS TDOCGRID
   LOCAL oTable,cSql,cItem

   ::oHead:Prepare()

   ::Prepare()
   ::bValid:=BLOQUECOD(::bValid)
   ::CalCols()

   // Debe Asignar valores en todas las columnas

   IF !IIF( !Empty(::cPreSave) , ::RunScript(::cPreSave) , .T.  )
      RETURN .F.
   ENDIF

   IF !EVAL(::bValid)
       RETURN .F.
   ENDIF

   IF !Empty(::cFieldFile)
      ::Set(::cFieldFile,::nFileMain)
   ENDIF

   IF !Empty(::cItem) .AND. ::nOption=1 .AND. Empty(::Get(::cItem))

     cItem:=SQLINCREMENTAL(::cTable,::cItem,::GetWhere(::cLinkGrid))
     ::Set(::cItem,cItem)

   ENDIF

   IF !::oHead:lSaved .AND. ::oHead:nOption=1

      IF ::oHead:PreSave() .AND. !::oHead:Save()
        ::Mensaje("No es Posible Grabar el Documento")
        RETURN .F.
      ENDIF

   ENDIF

   // Ahora Graba el Memo

   IF !EMPTY(::aMemo)

       ::aMemo[7]:=DPMEMOSAVE(::aMemo[7],::aMemo[8],::aMemo[9])
       ::Set(::aMemo[1],::aMemo[7]) // Asigna el Valor

       IF ValType(::oEditMemo)="O"   // Edición de Campo Memo
         ::oEditMemo:oWnd:End()
         ::oEditMemo:=NIL
       ENDIF

    ENDIF

    ::ShowTotal()


RETURN .T.

/*
// Grabar
*/
METHOD Save() CLASS TDOCGRID
   LOCAL cWhere:="",cSql,oCol,oTable_
   LOCAL oTable,I,lAppend:=.F.,lResp:=.F.,aLine:={},aRecord:={}

   CursorWait()

   IF ::nOption=0
      // ? GETPROCE(),"NO PUEDE GRABAR"
      RETURN .F.
   ENDIF

   IF ::nOption!=1

      DEFAULT ::cWhere:=::GetWhere(NIL,.T.)

      //::cWhere:=::GetWhere(NIL,.T.)
      // ? CLPCOPY(::cWhere)," PARA MODIFICAR"
      // ? CLPCOPY(::GetWhere()), "ESTE DEBE SER"

   ENDIF

   IF !Empty(::cWhere) .AND. ::nOption<>1
     cWhere:=" WHERE "+::cWhere
 //? "este el que graba",::cWhere
   ENDIF

   // 19/04/2023
   IF !Empty(::cPrimaryItem) .AND. !" ORDER BY "$cWhere
      cWhere:=cWhere+" ORDER BY "+::cPrimaryItem
   ENDIF

// ? cWhere,"antes de modificar, debe tener el vinculoa con oHead mas cItem",::cPrimaryItem

   cSql:="SELECT * FROM "+::cTable+cWhere+IF(!Empty(::cKeyAudita)," ORDER BY "+::cKeyAudita,"")+" LIMIT 1"

   // oTable:=OpenTable("SELECT * FROM "+::cTable+cWhere+" LIMIT 1"," WHERE "$cWhere)
   oDp:lExcluye:=.F.  // 05/04/2021
   oTable:=OpenTable(cSql," WHERE "$cWhere,::oDb)

   // JN 19/04/2023
   //IF !Empty(::cPrimaryItem)
   IF !Empty(::cKeyAudita)
     oTable:cPrimary:=::cKeyAudita
   ENDIF

// ? ::cKeyAudita,"::cKeyAudita para la auditoria"
   // ENDIF

//   IF !Empty(::cPrimaryItem)
//      oTable:cPrimaryKey:=cPrimaryItem
//   ENDIF

   DPWRITE("TEMP\GRID"+oTable:cTable+".SQL",cSql)

   IF Empty(oTable:aFields)
      MensajeErr(ClpCopy(oTable:cSql),"TDOCGRID Method Save()")
      RETURN .F.
   ENDIF

   IF ::nOption=1

      oTable:Append()
      lAppend:=.T.

   ELSEIF !Empty(::cFieldAud)

      oTable:SETAUDITAR(::cFieldAud)

      // 27/04/2023
      IF !Empty(::cKeyAudita)
        oTable:cPrimary:=::cKeyAudita
      ENDIF

      //  27/04/2023 oTable:cPrimary:=::cWhere // Genera incidencia

   ENDIF

   FOR I:= 1 TO LEN(oTable:aFields)

      IF ::IsDef(oTable:aFields[I,1])

        oCol:=::GetCol(oTable:aFields[I,1],.F.)

        oTable:Replace(oTable:aFields[I,1],::Get(oTable:aFields[I,1]))
        AADD(aLine,::Get(oTable:aFields[I,1]))
      ENDIF

   NEXT

   // Campos Segun Grid
   AEVAL(::aFieldGrid , {|cField,n| AADD(aRecord,::Get(cField)) })

   //ViewArray({aRecord})


//   ? ::oScript:IsFunction(::cCommit),::cCommit

   IF ::oScript:IsFunction(::cCommit)
      ::RunScript(::cCommit,oTable)
   ENDIF

   IF lAppend

      lResp:=oTable:Commit() // Nuevo

      DPWRITE("TEMP\GRID"+ALLTRIM(oTable:cTable)+"_INSERT.SQL",oDp:cSql)

      IF lResp
        oTable:End()
      ELSE
        MensajeErr(oDp:cSql," No se puede Incluir Registro en Tabla "+oTable:cTable)
      ENDIF

      // por este no totalizaba, jn 13/08/2008
      IF( ::lGetRowData ,  :: GetRowData() , NIL )

      ::nLineGet :=::oBrw:nArrayAt
      ::nRowSel  :=::oBrw:nRowSel

    ELSE

     ::oBrw:nArrayAt:=::nArrayAt

     // ? "MODIFICAR",::cWhere ,::oBrw:nArrayAt,::nArrayAt

      lResp:=oTable:Commit(::cWhere+" LIMIT 1 ") // Actualiza un solo registro  jn 05/06/2017

      IF lResp

         IF ::lViewArray   // JN 17/05/2017
           ViewArray(aRecord)
? ::oBrw:nArrayAt,"::oBrw:nArrayAt",LEN(::aDataQuery),"len(::aDataQuery)",CLPCOPY(oDp:cSql)
         ENDIF

         IF LEN(::aDataQuery)<::oBrw:nArrayAt
            AADD(::aDataQuery,aRecord)
         ENDIF

         ::aDataQuery[::oBrw:nArrayAt]:=ACLONE(aRecord)

         IF ::lViewArray
             ViewArray(::aDataQuery)
         ENDIF

      ENDIF

      ::Change()

   ENDIF

   ::ShowTotal()

   oTable:End()

RETURN lResp

/*
// Evalua Elimina el Registro
*/

METHOD PreDelete(lAuto) CLASS TDOCGRID
   LOCAL cItem
   LOCAL nAt:=::oBrw:nArrayAt,cSql,oOdbc,uKey,cPrimary:="",uData:="",lResp:=.F.

   DEFAULT lAuto:=.F.

   ::oBrw:nArrayAt:=MAX(::oBrw:nArrayAt,1)
   ::oBrw:nArrayAt:=MIN(::oBrw:nArrayAt,LEN(::oBrw:aArrayData))

// ? ::oBrw:nArrayAt,LEN(::aFields)
// ViewArray(::aDataQuery)

   AEVAL(::aFields,{|a ,n , uValue | uValue:=::aDataQuery[::oBrw:nArrayAt,n],::Set(a[1],uValue)})

   IF !lAuto

      ::Skip(0)

      IF !Empty(::cItem)
         uData:=::Get(::cItem)
      ENDIF

      AEVAL(::aCols,{|oCol,n| IIF( oCol:lPrimary ,;
                                    uData:= uData + IIF( Empty(uData), "" , "," ) + CTOO(::Get(oCol:cField),"C") , NIL )})

      ::nOption:=4

      IF !EMPTY(::aMemo)
        ::aMemo[7]:=IIF( ::nOption=1 , 0 , ::Get(::aMemo[1]))
        ::aMemo[8]:=""
        ::aMemo[9]:=""
      ENDIF

//    ::RunScript(::cLoad)
      ::OpcSay()
      ::nOption:=0

      IF ValType(::bDataDel)="B"   // Muestra los Datos del Cliente que se desear Borrar
         uData:=EVAL(::bDataDel)
         uData:=CTOO(uData,"C")
      ENDIF

      IF !MsgNoYes(::cRegistro+": "+uData,"Eliminar")
         DpFocus(::oBrw)
         RETURN .F.
      ENDIF

   ENDIF

   ::nOption:=4
   ::Skip(0)  // JN 7/9/09

   cItem:= IIF( Empty(::cItem), "" , " AND "+::cItem+GetWhere("=",::Get(::cItem)))

   cSql:="DELETE FROM "+::cTable+" WHERE "+::GetWhere() +  cItem  + " LIMIT 1" // JN 08/07/2018 (Solo debe eliminar un solo item


   IF !Empty(::cDeleteUpdate)
      cSql:="UPDATE "+::cTable+" SET "+::cDeleteUpdate+" WHERE "+::GetWhere() +cItem
   ENDIF

   uKey:=::GetWhere()

//   ? ::cPredelete,"",::RunScript(::cPreDelete , cSql )

   IF !::RunScript(::cPreDelete , cSql )
      ::nOption:=0
      RETURN .F.
   ENDIF

  //  ? "AQUI BEGIN, SI ELIMINA"

   IF ::Delete(cSql)

      IF ::PostDelete(lAuto,uKey)
         lResp:=.T.
      ENDIF

   ENDIF

//   ::ShowTotal()
//   IF lResp
//      ? "COMMIT"
//   ELSE
//      ? "ROLLBAK"
//   ENDIF

RETURN lResp

/*
// Luego de Borrar
*/
METHOD PostDelete(lAuto,uKey) CLASS TDOCGRID
   LOCAL nAt    :=::oBrw:nArrayAt,lResp:=.F.
   LOCAL nRowSel:=::oBrw:nRowSel

/*
   // Movido para la Parte Inferior, ya borró no nada que hacer, POR ELLO está abajo
   IF !::RunScript(::cPostDelete)
      RETURN .F.
   ENDIF
*/

//   ? "SI BORRO"

   IF !Empty(::aMemo) .AND. !Empty(::aMemo[7]) // Elimina el Campo Memo
      SQLDELETE(oDp:cDpMemo,"MEM_NUMERO"+GetWhere("=",::aMemo[7]),::oDb)
      AUDITAR("DELI" , NIL ,oDp:cDpMemo ,::aMemo[7] ,NIL,SELF)
   ENDIF
   // ::aMemo[7]:=DPMEMOSAVE(::aMemo[7],::aMemo[8],::aMemo[9])

   AUDITAR("DELI" , NIL ,::cTable , uKey ,NIL, SELF)

   ADEL(::oBrw:aArrayData ,nAt)
   ASIZE(::oBrw:aArrayData,LEN(::oBrw:aArrayData)-1)

   ADEL(::aDataQuery      ,nAt)
   ASIZE(::aDataQuery     ,LEN(::aDataQuery     )-1)

   ::oBrw:nArrayAt:=MIN(LEN(::oBrw:aArrayData),nAt)
   ::oBrw:nRowSel :=MIN(LEN(::oBrw:aArrayData),::oBrw:nRowSel)

IF .F.
// 08/06/2016
   ::CalCols()

   IIF( !Empty(::cTotal   ) , ::RunScript(::cTotal   ) , NIL )

   ::ShowTotal()

ENDIF

// JN 28/9/9

   IF !::RunScript(::cPostDelete)
      RETURN .F.
   ENDIF

   IF Empty(::oBrw:aArrayData)

      ::NewItem(.T.)
      ::nOption:=1
      ::adjustBtn(.T.)

   ELSE

      ::nOption:=0
      ::Change()
// MAYO 2020 DpFocus(::oBrw)

      nRowSel:=::oBrw:nRowSel
      nAt    :=::oBrw:nArrayAt

      ::oBrw:Refresh(.F.)

      ::oBrw:nArrayAt:=nAt
      ::oBrw:nRowSel :=nRowSel


   ENDIF

// 08/06/2016

   ::CalCols()

   IIF( !Empty(::cTotal   ) , ::RunScript(::cTotal   ) , NIL )

   ::ShowTotal()
// FIN


   ::oHead:BtnCancelOff()  // Apaga el Boton Cancelar

RETURN .T.

/*
// Elimina el Registro
*/
METHOD Delete(cSql) CLASS TDOCGRID
   LOCAL oOdbc:=::oDb,lResp:=.T.

   IF !Empty(::cDelete)

      lResp:=::RunScript(::cDelete , cSql )
//      ::ShowTotal()

      RETURN lResp

   ELSE

      DEFAULT oOdbc:=GetOdbc(::cTable)

      ::nOption:=0

      IF !oOdbc:Execute(cSql)

         lResp:=.F.
         MensajeErr("Registro no Pudo ser Eliminado")

      ELSE

        ::oHead:BtnCancelOff()  // Apaga el Boton Cancelar

      ENDIF

   ENDIF

RETURN lResp

/*
// PostGrabar
*/
METHOD PostSave() CLASS TDOCGRID
   LOCAL nAt,uValue

   // LOCAL nAt:=::oBrw:nArrayAt,nRow:=::oBrw:nRowSel
   //? "quien te llamo",getproce()

   ::CalCols()

   IF Empty(::cDataAudita)

      ::cDataHead:=""

      AEVAL(_VECTOR(STRTRAN(::cLinkGrid ,"+",",")),;
             {|a,n|::cDataHead:= ::cDataHead + ALLTRIM(CTOO(::Get(a),"C")) })

      ::cDataAudita:=""
      nAt:=ASCAN(::aCols,{|oCol,n|    oCol:lPrimary = .T. })

      IF nAt>0
         // Obtiene el Valor del Campo Clave
        //? nAt,::aCols[nAt]:cField,"CLAVE"

//       ? ::cDataHead,"::cDataHead",::aCols[nAt]:cField,"::aCols[nAt]:cField"

        uValue=::Get(::aCols[nAt]:cField)

        IF !(ValType(uValue)=ValType(::cDataHead))
           uValue:=CTOO(uValue,ValType(::cDataHead))
        ENDIF

        ::cDataHead:=::cDataHead+","+uValue // ::Get(::aCols[nAt]:cField)

      ELSE
        ::cDataHead  := ::cDataHead+":"
      ENDIF

// ? "aqui esta asi", ::cDataHead

     //::cDataAudita:=::cDataHead

/*
      AEVAL(::aFields,{|a ,n,uValueA,uValueB|       ;
                        uValueA:=ALLTRIM(CTOO(::Get(a[1]),"C"    )) ,;
                        uValueB:=ALLTRIM(CTOO(::Get(a[1]+"_"),"C")) ,;
                        ::cDataAudita:=::cDataAudita+;
                                      IF(uValueA==uValueB , ""  , ;
                                         ::aFields[n,1]+":"+uValueA+;
                                         " x "+uValueB)})


       ::cDataAudita:=::cDataHead+::cDataAudita
*/

       // Identifica, el item
       IF !Empty(::cDataHead) .AND. !Empty(::cItem)

// +ALLTRIM(::cDataHead)+;

          ::cDataHead:= ::cDataHead +;
                        "ITEM:"+::Get(::cItem)


       ENDIF
//                                        IIF(Empty(::cDataAudita),"",",")+:
       ::cDataAudita:=::cDataHead

   // ? ::cDataAudita,"clave, ::cDataAudita" ,   ::cDataHead

   ENDIF

   IF ::nOption=1



     ::oBrw:nArrayAt:=LEN(::oBrw:aArrayData)
     // ::oBrw:nRowSel :=::nRowSel
     IF ::lInc_Auditar

        ::cDataAudita:=::cDataHead

        AUDITAR("DINC" , NIL ,::cTable , ::cDataAudita, NIL, SELF )

     ENDIF

   ELSE

     ::oBrw:nArrayAt:=::nLineGet
     ::oBrw:nRowSel :=::nRowSel

     AUDITAR("DMOD" , NIL ,::cTable , ::cDataAudita , NIL , SELF)

   ENDIF

   IIF( !Empty(::cPostSave) , ::RunScript(::cPostSave) , NIL )
   IIF( !Empty(::cTotal   ) , ::RunScript(::cTotal   ) , NIL )

   IF( ! ::lGetRowData ,  ::GetRowData() , NIL )

   Aeval(::oBrw:aCols,{|oCol|oCol:KillGet(),oCol:nEditType:=0})

   IF ::nOption=1
      ::NewItem(.T.)
   ELSE
      ::nOption:=0
      ::AdjustBtn(.T.)
//      ::aBtn[1,1]:ForWhen()
      ::oBrw:SelectCol(1)

      // Reposiciona el Video ROW

      // ::Goto(::nLineGet)

      ::oBrw:nArrayAt:=::nLineGet
      ::oBrw:nRowSel :=::nRowSel

      ::oBrw:Drawline(.F.) // Refresh(.F.)
      ::Edit(.F.)

      ::oBrw:nArrayAt:=::nLineGet
      ::oBrw:nRowSel :=::nRowSel

//? "AQUI DEBE ESTAR",nRow,nAt,::nLineGet,GetProce()

   ENDIF

   ::oHead:BtnCancelOff()  // Apaga el Boton Cancelar

RETURN .T.

/*
// Realiza los Cálculos de las Columnas
*/
METHOD CalCols(cField,lAll) CLASS TDOCGRID
   LOCAL nMonto:=0,aData:=ACLONE(::oBrw:aArrayData)
   LOCAL I,U,nItems:=LEN(::oBrw:aArrayData)+;
             IIF( ::nOption=1, 0  ,  0 ),;
         lRefresh:=.F.

/*
  IF !::lShow .AND. ::nOption<>1
    ViewArray(aData)
    ::lShow:=.T.
  ENDIF
*/

// mensajeErr(LEN(aData),"aqui es calcols")
// GETPROCE()
//ViewArray(aData)

   // Debe totalizar las Columnas
   // AEVAL(::aCols,{|o,n|::aCols[n]:nTotal:=0})

   FOR I=1 TO LEN(::aCols)

      ::aCols[I]:nTotal:=0

      IF ::aCols[I]:lTotal
        lRefresh:=.T.

        AEVAL(aData,{|a,n,nMonto|nMonto:=VALSTRTRAN(a[I],::aCols[i]:cPicture),;
                                 ::aCols[I]:nTotal:=::aCols[I]:nTotal+nMonto})

//?  i,::aCols[I]:nTotal

      ENDIF


   NEXT I
/*
   FOR I := 1 TO LEN(aData)

      FOR U := 1 TO LEN(::aCols)

//       ? ::aCols[U]:lTotal,"TOTALIZA",U
         IF ::aCols[U]:lTotal

            //nMonto:=TRAN(::oBrw:aArrayData[I,::aCols[U]:nCol],::aCols[U]:cPicture)
            //nMonto:=VAL(STRTRAN(nMonto,",",""))

            nMonto:=VALSTRTRAN(::oBrw:aArrayData[I,::aCols[U]:nCol],::aCols[U]:cPicture)

// MensajeErr(nMonto,"nMonto")
// MensajeErr(::aCols[U]:cPicture,"cPicture"+nMonto)
// mensajeErr(nMonto,"nMonto")

            lRefresh:=.T.
            ::aCols[U]:nTotal:=::aCols[U]:nTotal+nMonto

// ::oBrw:aArrayData[I,::aCols[U]:nCol]
         ENDIF
      NEXT
   NEXT
*/

/*
// JN: 7/9/09 totaliza dos veces
   FOR U := 1 TO LEN(::aCols)
     IF ::aCols[U]:lTotal
       // ::aCols[U]:nTotal:=VAL(STR(::aCols[U]:nTotal,19,2)) // AQUI intercede en los decimales
       // nMonto:=VALSTRTRAN(::oBrw:aArrayData[I,::aCols[U]:nCol],::aCols[U]:cPicture)
       ///::aCols[U]:nTotal:=VAL(STRTRAN(ALLTRIM(TRAN(::aCols[U]:nTotal,::aCols[U]:cPicture)),",","")) // AQUI intercede en los decimales

     ENDIF
   NEXT
*/
   // Ahora Calcula los Items
/*
   FOR I := 1 TO LEN(::aCols)
     IF ::aCols[I]:lItems
        lRefresh:=.T.
        ::oBrw:aCols[I]:cFooter:=::aCols[I]:cItems+LSTR(nItems)
     ENDIF
   NEXT
*/
/*
// debe refrescar porque ya totalizo
   IF lRefresh
//mensajeErr("aqui debe refrescar")
      ::ShowTotal()
      // ::oBrw:Refresh(.F.)
   ENDIF
*/

   IF lRefresh
      // 22/04/2016 ::oBrw:Refresh(.F.)
      ::oBrw:DrawLine(.T.) // Refresh(.F.)
//? "AQUI REFRESCA"
   ENDIF

RETURN NIL


/*
// Agrega un Nuevo Elemento Vacio para la Inclusión
*/
METHOD Append() CLASS TDOCGRID
    LOCAL I,uValue,aLine:={},oTable

    DEFAULT ::aData:={}

    // ::nOption:=1
    // aLine:=ACLONE(::aEmpty)

    FOR I := 1 TO LEN(::aCols)

       uValue:=__objSendMsg(Self,::aCols[I]:cField) // Lee la Data
//
       IF !::aCols[I]:lRepeat
         uValue:=CTOEMPTY(uValue)             // No Repite el Valor Anterior
         uValue:=::aCols[I]:GetDefault(uValue)
         // __objSendMsg(Self,::aCols[I]:cField,uValue) // Valor Vacio
       ELSE
         uValue:=::aCols[I]:GetDefault(uValue)
       ENDIF

//       ? uValue,"desde append"

       __objSendMsg(Self,::aCols[I]:cField,uValue) // Valor Vacio

       IF ValType(uValue)="N"
         ::aCols[I]:nDataStrAlign:= AL_RIGHT
         ::aCols[I]:nHeadStrAlign:= AL_RIGHT
       ENDIF

       ::aCols[I]:lChange:=.F. // Ninguno Cambiado

       AADD(aLine,uValue)

    NEXT I

    // AADD(::aData,aLine)
    // Ahora en Nuevo Registro

RETURN aLine

/*
// Recupera el Foco
*/
METHOD BrwGotFocus()  CLASS TDOCGRID

   // ::oBrw:GotFocus() // jn 20/02/2014
   ::Prepare()

   // ? ::nOption,"nOption"
   //oDp:oFrameDp:SetText("gotfocus "+IIF( ::lEdit, "SI", "NO" ) + " "+LSTR(SECONDS()))

   IF ::lEdit
      ::Edit(::lEdit)
   ENDIF


RETURN NIL

/*
// Pierde el Foco
*/
METHOD BrwLostFocus()  CLASS TDOCGRID
     ::oBrw:nMarqueeStyle:=::nMarqueeStyle
RETURN NIL

METHOD DocLostFocus()  CLASS TDOCGRID
    oDp:lTracer_:=oDp:lTracer
    oDp:lTracer:=.F.

    IF !::oHead:lDesign
      EJECUTAR("DOCGRIDLOSTFOCUS",Self)
    ENDIF

    oDp:lTracer:=oDp:lTracer_

RETURN .T.

METHOD DocGotFocus()  CLASS TDOCGRID
    oDp:lTracer_:=oDp:lTracer
    oDp:lTracer:=.F.
    EJECUTAR("DOCGRIDGOTFOCUS",Self)
    oDp:lTracer:=oDp:lTracer_
RETURN NIL

/*
// Grabar a Traves del Boton
*/
METHOD BtnSave(lAuto)
   LOCAL lResp:=.T.
   LOCAL I,uValue

   DEFAULT lAuto:=.F.

IF !lAuto  // 09/04/2021

   FOR I := 1 TO LEN(::aCols)

      uValue:=::Get(::aCols[I]:cField)

      IF !::aCols[I]:Valid(uValue , NIL , ::oBrw:aCols[I] )
         lResp:=.F.
         EXIT
      ENDIF
   NEXT

ENDIF

   IF !lResp
      DpFocus(::oBrw)
      RETURN .F.
   ENDIF

   ::IsFinish(.T.)

   // ? "BtnSave",lResp

RETURN .T.

/*
// Verifica si hay mas Columnas Editables
*/
METHOD IsFinish(lAuto)  CLASS TDOCGRID

   LOCAL I,lEnd:=.T.,lResp:=.T.

   DEFAULT lAuto:=.F.

   IF !(::nColEdit=LEN(::aCols)) .AND. !lAuto .AND. LEN(::aCols)>1

     FOR I := ::nColEdit+1 TO LEN(::aCols)

        IF ::aCols[I]:Edit(::lEdit) // Puede Editar
           lEnd:=.F.
           EXIT
        ENDIF

     NEXT

   ENDIF

   IF ::nOption=0 // No puede Grabar
     RETURN .T.
   ENDIF

   IF lEnd

    // ? "AQUI DEBE SER BEGIN SECUENCIA",GETPROCE()

      // ::oBrw:nArrayAt:=::nLineGet
      // ::oBrw:nRowSel :=::nRowSel


      DEFAULT ::lLockHead:=.F.

      // Solo debe Bloquear Una Vez, Cuando se Incluye
      IF ::oHead:nOption<>1
         ::lLockHead:=.T.
      ENDIF

//      IF !::lLockHead
//         ? "BLOQUEAR", ::oHead:cTable
//      ENDIF

      /*
      // Debe revisar la Integridad Referencial Antes del Save
      */

      IF ValType(::oTable)="O"  .AND. ::lCreaRegIntRef

         // ::oTable:CreaRegIntRef() // Suspedido Genera Incidencia
         ::oTable:lCreaRegIntRef:=.F. // Ya lo ejecutó

      ENDIF

      DpSqlBegin(NIL,NIL , IIF(!::lLockHead,::oHead:cTable,NIL))

      ::lLockHead:=.T.

      IF ::Presave()

//? "PRESAVE"

         IF ::Save()
            ::PostSave()
         ELSE
            lResp:=.F.
         ENDIF

      ELSE

        lResp:=.F.

      ENDIF

      IF !lResp

          DpSqlRollBack()

      ELSE

          DpSqlCommit()

      ENDIF

   ENDIF

RETURN lResp //  09/04/2021 lEnd

/*
// Genera la Data que será Mostrada en el Browse
*/
// METHOD BuildData()  CLASS TDOCGRID
//
//RETURN NIL

/*
// Construye el Where del Registro
*/
METHOD GetWhere(cFields,lItem,lFields)  CLASS TDOCGRID
   LOCAL cWhere:="",I,aVar,uValue,oTable,cSql:=::cSql,cVarRep:=""

   DEFAULT cFields:=::cPrimary,lItem:=.F.,lFields:=.F.

   IF Empty(cFields)
     cFields:=::cLinkHead
   ENDIF

   IF Empty(cFields)
      RETURN ""
   ENDIF

   IF (::nOption>1 .AND. !Empty(::cItem) .AND. lItem) .OR. (lItem .AND. !Empty(::cItem))
      cFields:=cFields + "," + ::cItem
   ENDIF

   IF Empty(::cKeyAudita)
      ::cKeyAudita:=cFields // 19/04/2023
   ENDIF

   aVar  :=_VECTOR(STRTRAN(cFields,"+",","))

   FOR I := 1 TO LEN(aVar)

     IF (aVar[I]+","$cVarRep)
        LOOP
     ENDIF

     cVarRep:=cVarRep+aVar[I]+","

     uValue:=::Get(aVar[I])

     // ? i, uValue,aVar[I],"GetWhere",ValType(uValue)

     IF ValType(uValue)="C"

//         ? ::cFieldChar , ValType(uValue)," ES ::cFieldChar"

        IF Empty(cSql)
          cSql:="SELECT "+::cFieldChar+" FROM "+::cTable
        ENDIF

        IF oTable=NIL
          oDp:lExcluye:=.F.  // 05/04/2021
          oTable:=OpenTable(cSql,.F.,::oDb)
        ENDIF

        uValue:=LEFT(uValue,LEN(oTable:FieldGet(aVar[I])))

     ENDIF

     cWhere:=cWhere+;
             IIF( Empty(cWhere) , "" , " AND " )+;
             aVar[I] + GetWhere("=",uValue)



   NEXT I

   IF !Empty(cWhere) .AND. !Empty(::cScope)

      cWhere:=cWhere+ " AND "+::cScope

   ENDIF

// ? cWhere,"cWhere"

   IIF( oTable=NIL , NIL , oTable:End() )

RETURN cWhere

/*
// Carga los Datos hacia las Variables
*/
METHOD RunScript(cFunction,uPar1,uPar2)  CLASS TDOCGRID
  LOCAL lResp

  IF Empty(cFunction)
     RETURN .T.
  ENDIF

  ::Prepare()

  IF !::oScript:IsFunction(cFunction)

     ::Mensaje("Función : "+cFunction+" No Existe en "+::cScript)

  ELSE

     ::oScript:cError:=""

     lResp:=::oScript:Run(cFunction,Self,uPar1,uPar2)

     IF !Empty(::oScript:cError)
         MensajeErr(::oScript:cError,::cScript+ "FUNCTION "+cFunction)
     ENDIF

  ENDIF

RETURN lResp

/*
// Nuevo Scope
*/
METHOD SetScope(cScope , nOption) CLASS TDOCGRID

    DEFAULT nOption:=::oHead:nOption

    ::cScope:=cScope
    ::NewJob(nOption , .T.)
//? "OJO CON ESTE",GETPROCE()

RETURN .T.


/*
// Ajustar Botones
*/
METHOD AdjustBtn(lForWhen) CLASS TDOCGRID
  LOCAL nStep:=0,I,nOk:=0
  LOCAL nHeight,nWidth

  // ::Prepare(.F.) // no Acepta

  IF ::nOptionOld=::nOption .OR. Empty(::aBtn) .OR. ValType(::aBtn[1,1])!="O"
     RETURN .T.
  ENDIF

  IF ::nBtnStyle=0 // No hay Boton
     RETURN .T.
  ENDIF

  nWidth :=::aBtn[1,1]:nWidth
  nHeight:=::aBtn[1,1]:nHeight

  DEFAULT lForWhen:=.F.

  ::Prepare()

  FOR I := 1 TO LEN(::aBtn)

      IF !::aBtn[I,1]:lWhen() // !EVAL(::aBtn[I,1]:bWhen)
         ::aBtn[I,1]:Hide()
      ELSE
//       ::aBtn[I,1]:Move(::aSize[1]+::aSize[3]+11 , (::aBtnSize[2]+1)*(i-1) + nStep ) // , ::aBtnSize[1] , ::aBtnSize[2] , .T. )
         nOk++

         IF ::lBar

           ::aBtn[I,1]:Move(3, (nWidth + 1) * (nOk-1) )


         ELSE

           ::aBtn[I,1]:Move(::oBrw:nTop+::oBrw:nHeight , ::oBrw:nLeft + ((::aBtnSize[2]+1)*(nOk-1) + nStep )) // , ::aBtnSize[1] , ::aBtnSize[2] , .T. )

         ENDIF

         //::aBtn[I]:Move(oBrw:nTop+oBrw:nHeight+5,oBrw:nLeft+nStep,35,35,.T.)
         //::aBtns[i]:SetSize(35,35)

         nStep:=nStep+(::aBtnSize[1] + 1)
         ::aBtn[I,1]:Show()

      ENDIF

      ::aBtn[I,1]:Refresh()
      // :: Refresh()

   NEXT

   IF ::nOption=0
      DpFocus(::oBrw)
   ENDIF

   ::nOptionOld:=::nOptionOld

   IIF( lForWhen  , ::aBtn[1,1]:ForWhen()  , NIL )
   // Refresh()

   // ::aBtn[I,1]:oWnd:Refresh()
//   DpFocus(oBrw)

RETURN NIL

/*
// Cargar Registros
*/
METHOD LoadRecord(nRecord)  CLASS TDOCGRID
   ::Change()
   // ?  ::oBrw:nArrayAt,LEN(::aDataQuery)

   IF Empty(::aDataQuery)
      RETURN .T.
   ENDIF

   DEFAULT nRecord:=::oBrw:nArrayAt

   AEVAL(::aFields,{|a ,n , uValue | uValue:=::aDataQuery[nRecord,n],;
                                     ::Set(a[1]    ,uValue),;
                                     ::Set(alltrim(a[1])+"_",uValue)})

RETURN .T.

/*
// Crear Botones del Grid
*/
METHOD PutBar() CLASS TDOCGRID
   LOCAL I,oBtn

   IF !::lBar
      RETURN NIL
   ENDIF

   ::aBtnSize[1]:=::oHead:nBtnWidth
   ::aBtnSize[2]:=::oHead:nBtnHeight

   DEFINE BUTTONBAR ::oHead:oBar 3D SIZE ::aBtnSize[1],::aBtnSize[2] OF ::oHead:oDlg

   // ::oHead:nBtnWidth,::oHead:nBtnHeight OF ::oHead:oDlg

   ::oHead:oBar:bRClicked :={||.T.}

   ::CreateBtn(::oHead:oBar)

   ::oHead:oBar:SetColor(CLR_BLACK,oDp:nGris)
   AEVAL(::oHead:oBar:aControls,{|o,n|o:SetColor(CLR_BLACK,oDp:nGris)})

RETURN .T.

/*
// Crear Botones
*/
METHOD CreateBtn(oDlg) CLASS TDOCGRID

    LOCAL I,n:=5

    IF !::lView
       I:=ASCAN(::aBtn,{|a,n|a[1]=2})
       IIF(I>0,(::aBtn:=Arreduce(::aBtn,I)),NIL)
    ENDIF

    IF !::lFind
       I:=ASCAN(::aBtn,{|a,n|a[1]=5})
       IIF(I>0,(::aBtn:=Arreduce(::aBtn,I)),NIL)
    ENDIF

/*
   AADD(::aBtn,{1,"xNew2.bmp "   ,"Incluir        "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0","oGrid:RunKey(73)","Inc"})

   IF ::lView
     AADD(::aBtn,{2,"xView2.bmp"   ,"Consultar      "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0","1=1"             ,"Con"})
   ENDIF

   AADD(::aBtn,{3,"xEdit2.bmp"   ,"Modificar      "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0","oGrid:Open()"    ,"Mod"})
   AADD(::aBtn,{4,"xdelete2.bmp" ,"Eliminar       "   ,"oHead:nOption<>0 .AND. oGrid:nOption=0","oGrid:RunKey(69)","Eli"})
   AADD(::aBtn,{5,"xFind.bmp"    ,"Buscar         "   ,"oGrid:nOption=0","1=1"                                    ,"Bus"})

   AADD(::aBtn,{6,"xTop2.bmp"    ,"Primer         "   ,"oGrid:nOption=0 ","1=1"                                   ,"<< "})
   AADD(::aBtn,{7,"xAnt2.bmp"    ,"Pagina Abajo   "   ,"oGrid:nOption=0 ","1=1"                                   ," < "})
   AADD(::aBtn,{8,"xSig2.BMP"    ,"Pagina Arriba  "   ,"oGrid:nOption=0 ","1=1"                                   ," > "})
   AADD(::aBtn,{9,"xFin2.bmp"    ,"Ultimo Registro"   ,"oGrid:nOption=0 ","1=1"                                   ,">> "})
*/
   FOR I := 1 TO LEN(::aBtnNew)
       AADD(::aBtn,NIL)
       AINS(::aBtn,n)
       ::aBtn[n]:=::aBtnNew[I]
       n++
    NEXT

    FOR I := 1 TO LEN(::aBtn)

      IF ::nBtnStyle=0
         ::aBtn:={}
         EXIT
      ENDIF

      DO  CASE

         CASE ::nBtnStyle=1 .AND. !::lBar

           @ ::aSize[1]+::aSize[3]+11 , (::aBtnSize[2]+1)*(i-1)  XBUTTON ::aBtn[i,1] PIXEL;
             FILE oDp:cPathBitMaps+::aBtn[i,2];
             NOBORDER;
             SIZE ::aBtnSize[1],::aBtnSize[2];
             MESSAGE ::aBtn[i,3];
             TOOLTIP ::aBtn[i,3];
             COLORS oDp:nGris,NIL OF oDlg

         // ::aBtn[i,1]:SetColor(NIL,oDp:nGris)

        CASE ::nBtnStyle!=1 .AND. !::lBar

          @ ::aSize[1]+::aSize[3]+11 , (::aBtnSize[2]+1)*(i-1)  BUTTON ::aBtn[i,1];
            PROMPT ::aBtn[I,6];
            FONT ::oFontBtn;
            SIZE ::aBtnSize[1],::aBtnSize[2] OF oDlg

         // ::aBtn[I,1]:cMessage:=::aBtn[i,3]
           ::aBtn[I,1]:cToolTip:=::aBtn[i,3]

        CASE ::nBtnStyle=1 .AND. ::lBar

            DEFINE BUTTON ::aBtn[I,1];
                  OF oDlg ;
                  NOBORDER  ;
                  FILENAME "BITMAPS\"+::aBtn[I,2]

           ::aBtn[I,1]:cToolTip:=::aBtn[i,3]

        CASE ::nBtnStyle!=1 .AND. ::lBar

            DEFINE BUTTON ::aBtn[I,1];
                  OF ::oBar ;
                  PROMPT ::aBtn[I,8];
                  FONT ::oHead:oFontBtn

      ENDCASE

      ::aBtn[I,1]:bWhen  :=GenBlock("oPar1:Prepare(),"+::aBtn[I,4],Self)
      ::aBtn[I,1]:bAction:=GenBlock("oPar1:Prepare(),"+::aBtn[I,5],Self)
      ::aBtn[i,1]:SetColor(nil,oDp:nGris)


      // Cancel
      IF ::aBtn[I,6]="Esc"
         ::aBtn[I,1]:lCancel:=.T.
      ENDIF

// ? ::aBtn[I,2],::aBtn[I,5]
    NEXT

   //  ? "aqui hace los botones",GetProce(),oDlg:ClassName()

RETURN .T.

/*
// Inicia el Proceso de Búsqueda
*/
METHOD SetFind(lOn)
  LOCAL oCol,I

//  IF Empty(::aColsSave) .AND. lOn
//     ::aColsSave:={}
//     AEVAL(::oBrw:aCols,{|a,n|MensajeErr(valType(a:bOnPostEdit)),AADD(::aColsSave,a:bOnPostEdit)})
//  ENDIF

  FOR I=1 TO LEN(::oBrw:aCols)
     ::oBrw:aCols[I]:nEditType:=IIF( lOn, 1 , 0)
     IF lOn
       ::oBrw:aCols[I]:bOnPostEdit:={|oCol,uValue,nLastKey,nCol|::DbSeek(oCol,uValue,nLastKey),::lFind:=.F.,KillFind(::oBrw,::oBrw:nGetColSel())}
     ENDIF
  NEXT

  // IF !lOn

     // AEVAL(::aColsSave,{|a,n|MsgAlert(ValType(a)+"Devuelve "),::oBrw:aCols[n]:bOnPostEdit:=a})

     // AEVAL(::aCols,{|oCol,n|::oBrw:aCols[n]:bOnPostEdit  :=PutOnEdit(oCol,Self,n)})
     // ? "aqui se reactiva el PostEdit"

//     RETURN .T.

  // ENDIF

  oCol := ::oBrw:SelectedCol()
  ::oBrw:lRefreshOnlyData:=.f.
  oCol:Edit()
  DpFocus(oCol:oEditGet)

RETURN .T.

/*
// Pinta los Textos Según las Búsquedas
*/
METHOD ColorFind()
   LOCAL lFound:=.F.,I,uValue

   FOR I := 1 TO LEN(::aCols)

     uValue:=::aCols[I]:uValueFind

     IF !Empty(uValue) .AND. ValType(uValue)="C"
        uValue:=UPPE(ALLTRIM(uValue))
//        ::oBrw:oWnd:oWnd:SetText(uValue)
     ENDIF

     IF ValType(uValue)="C" .AND. ValType(::oBrw:aArrayData[::oBrw:nArrayAt,I])="C" .AND. uValue$UPPE(::oBrw:aArrayData[::oBrw:nArrayAt,I])
       lFound:=.T.
       EXIT
     ENDIF

   NEXT

RETURN lFound

/*
// Reliza la Búsqueda
*/
METHOD DbSeek(oCol,uValue,nLastKey)
   LOCAL nCol

   nCol:=::oBrw:nGetColSel()
   ::aCols[nCol]:Find(uValue)

RETURN .T.

METHOD End() CLASS TDOCGRID

    Aeval(::aBtn,{|aBtn|aBtn[1]:End()})
    ::aBtn:={}

    Aeval(::oBrw:aCols,{|o,n| IIF(o:oBtnElip<>NIL,o:oBtnElip:End(),NIL )})
    ::lEnd:=.T.
    // ::oWnd:hWnd:=0

    EJECUTAR("TDOCGRIDEND",Self)

RETURN NIL

/*
// Coloca el Color el Color del Encabezado
*/
METHOD SetColorHead(nClrPane,nClrText,oFont) CLASS TDOCGRID
     LOCA I

      IF oFont!=NIL
         FOR I := 1 TO LEN(::oBrw:aCols)
           ::oBrw:aCols[I]:oHeaderFont:=oFont
         NEXT
      ENDIF

      oFont:=NIL

RETURN ::oBrw:bClrHeader:= {|| { nClrPane,nClrText}}

/*
// Asigna Valores de Relación con DPMEMO a Nivel de Descripción Amplia
*/

METHOD SetMemo(cField,cTitle,nTop,nLeft,nWidth,nHeight) CLASS TDOCGRID

   LOCAL aData:={},nNumMemo:=0

   DEFAULT cTitle:="Descripción Amplia"

   IF !::IsDef(cField) // Campo no Existe
     MensajeErr("Campo "+cField+" no existe","SetMemo()")
     RETURN .F.
   ENDIF

   nNumMemo:=::Get(cField)

   cTitle:=GetFromVar(cTitle)

   ::aMemo:={cField,cTitle,nTop,nLeft,nWidth,nHeight,nNumMemo,"",""}

   AADD(::aBtnEdit, { "xMemo2.bmp"  ,;
                      cTitle      ,;
                      "oGrid:nOption!=0",;
                      "KillGetCol(oGrid:oBrw),oGrid:oEditMemo:=_DPMEMOEDIT(oGrid,oGrid:oEditMemo,oGrid:oBrw),oGrid:oHead:LinkEdit(oGrid:oEditMemo)",;
                      "Mem",STR(DP_CTRL_W) })

RETURN NIL

/*
// Asigna Valor y Refresca Objeto
*/
METHOD SetAdjuntos(cField,cTitle) CLASS TDOCGRID
   lOCAL cTabla
   LOCAL aData:={},nNumMemo:=0,cFileBmp:="adjuntar.bmp"
   LOCAL oGrid:=SELF

   DEFAULT cTitle:="Asociar Archivos Adjuntos",;
           cTabla:=oDp:cTableDpFile

   IF !::IsDef(cField) // Campo no Existe
     MensajeErr("Campo "+cField+" No Existe")
     RETURN .F.
   ENDIF

   cTitle:=GetFromVar(cTitle)
   // Campo Vinculado con los archivos Adjuntos
   ::cFieldFile:=cField
   ::nFileMain :=::Get(cField)

// ::EditButtons(cTitle , cFileBmp ,{||::nFileMain:=EJECUTAR("DPFILEEMPMAIN",::nFileMain,cTitle,::cTable,::nOption=0 .OR. ::nOption=2)})

   AADD(::aBtnEdit, { cFileBmp    ,;
                      cTitle      ,;
                      "oGrid:nOption!=0",;
                      "oGrid:nFileMain:=EJECUTAR('DPFILEEMPMAIN',oGrid:nFileMain,'"+cTitle+"',oGrid:cTable,(oGrid:nOption=0 .OR. oGrid:nOption=2),nil,nil,nil,oGrid)",;
                      "Adj",STR(DP_CTRL_W) })

RETURN NIL

/*
// Localizacion de Campos
*/
METHOD FieldPos(cField) CLASS TDOCGRID
      cField:=ALLTRIM(UPPE(cField))
RETURN  ASCAN(::aFields,{|a,n| ALLTRIM(a[1])=cField })

/*
// Lee el Ultimo Registro Incluido
*/
METHOD GetRowData() CLASS TDOCGRID
  LOCAL cSql,oTable,aLine,cWhere

  cSql  :=::cSql
  cWhere:=::GetWhere(NIL,.T.)

  // 08/04/2020, lograr que SQL del GRID pueda utilizar GROUP BY, INNER JOIN
  IF !Empty(::GetWhere(NIL,.T.))
//   cSql:=cSql+" WHERE "+::GetWhere(NIL,.T.)
     cSql:=EJECUTAR("SQLINSERTWHERE",cSql," WHERE "+::GetWhere(NIL,.T.))
  ELSE
//   cSql:=cSql+" WHERE "+::GetWhere(::cLinkHead,.T.)
     cSql:=EJECUTAR("SQLINSERTWHERE",cSql,+" WHERE "+::GetWhere(::cLinkHead,.T.))
  ENDIF

/*
  IF !Empty(::GetWhere(NIL,.T.))
     cSql:=cSql+" WHERE "+::GetWhere(NIL,.T.)
  ELSE
      cSql:=cSql+" WHERE "+::GetWhere(::cLinkHead,.T.)
  ENDIF
*/

  cSql:=cSql+" LIMIT 1"

  oDp:lExcluye:=.F.  // 05/04/2021
  oTable:=OpenTable(cSql,.T.,::oDb)

  DPWRITE("TEMP\GRID"+oTable:cTable+".SQL",cSql)

  IF oTable:RecCount()=0
     DPWRITE("TEMP\VACIO.SQL",cSql)
     MensajeErr(ClpCopy(cSql),"Method Save, Cursor Vacio")
  ELSE
     aLine:=oTable:aDataFill[1]
  ENDIF

   AADD(::aDataQuery , aLine)

RETURN NIL


/*
 *  OnError()
 */
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TDOCGRID

LOCAL cErrorLog,cMsg,lScript:=.F.,J,N
LOCAL lResp:=.F.

 cMsg   := UPPE(ALLTRIM(__GetMessage()))

 if SubStr( cMsg, 1, 1 ) == "_"      // Asignar Valor

   cMsg:=Subs(cMsg,2)

   __objAddData( Self, cMsg )
   __objSendMsg(Self,cMsg,uValue)

   RETURN uValue

 ELSE

 //  IF ::lIsDef
//      ::lsDef:=.F.
//      RETURN NIL
//   ENDIF

    ::Prepare()

//  ? cMsg,uValue

    IF !ValType(::oScript)="O"
       ::SetScript(::cScript)
    ENDIF

    IF ::oScript:cProgram<>::cScript
       ::SetScript(::cScript)
    ENDIF

    ::SetScript(::cScript)

    IF !::oScript:IsFunction(cMsg)

        // ? LEN(::oScript:aFunctions),"LEN(::oScript:aFunctions)",cMsg
        // ViewArray(::oScript:aFunctions)
        MensajeErr("REQUIERE COMPILAR "+::oScript:cProgram+CRLF+"SCRIPT"   +::cScript+CRLF+"Función "+cMsg+" no Existe Clase:"+::CLASSNAME()+CRLF+GetProce())
        //MensajeErr("FUNCTION "+cMsg,::oScript:IsFunction(cMsg))
        uValue:=.F. // La mayoria de las Funciones requieren .T. / .F.
    ELSE
       // Ejecuta la Función
       ::oScript:lNoError:=!::lMsgError
       uValue=::oScript:Run(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)
       ::oBrw:SetFocus()
    ENDIF

 ENDIF

RETURN uValue

/*
// Coloca las Validaciones
*/
STATIC FUNCTION PUTVALID(oBrwCol,oCol,oGrid)
  oBrwCol:bEditValid:={|uValue|oGrid:Prepare(),oCol:Valid(uValue)}
  oBrwCol:bEditValid:={||.T.} // Todo pasa por PostEdit y PostEdit Hace el Valid
RETURN .T.

/*
// Establece PostEdit en Cada Columna
*/
STATIC FUNCTION PutOnEdit(oCol,oGrid,nCol)
RETURN {|o,x,lResp| lResp :=oCol:PostEdit(o,x) , ;
                    IIF( lResp ,  oGrid:IsFinish() , NIL ) , oGrid:SetFocus() }

/*
// Bloque de Código, Visualizar Data
*/
STATIC FUNCTION BrwViewData(oBrw,nCol,oColDp)
RETURN { |oCol| IF(Empty(oBrw:aArrayData[oBrw:nArrayAt,nCol]) .AND. oColDp:lViewEmpty ,"" , TRAN(oBrw:aArrayData[oBrw:nArrayAt,nCol],oCol:cSayPicture))}

// ::oBrw,n,Self,oCol
STATIC FUNCTION BrwColorData(oBrw,nCol,oGrid,oColDp)
   LOCAL bClrStd:=oBrw:bClrStd
// RETURN {||IIF(oGrid:nOption<>0 .AND. oGrid:nLineGet=oBrw:nArrayAt .AND. nCol=oBrw:nGetColSel()  , {oDp:Get_nCltText,oDp:Get_nClrPane},Eval(oBrw:bClrStd))}
RETURN {||IIF(oGrid:nOption<>0 .AND. oGrid:nLineGet=oBrw:nArrayAt .AND. nCol=oBrw:nGetColSel()  , {oDp:Get_nCltText,oDp:Get_nClrPane},Eval(bClrStd))}

STATIC FUNCTION PutBmpData(oBrw,nCol,oCol,oGrid)
    oCol:bLDClickData:={||IIF(oGrid:nOption=0,NIL,CambiaLogico(oBrw,nCol))}
RETURN {||IIF(oBrw:aArrayData[oBrw:nArrayAt,nCol],1,2) }

STATIC FUNCTION CambiaLogico(oBrw,nCol)
   oBrw:aArrayData[oBrw:nArrayAt,nCol]:=!oBrw:aArrayData[oBrw:nArrayAt,nCol]
   oBrw:DrawLine(.T.)
RETURN NIL

STATIC FUNCTION GetBlq(oGrid,bBlq)
   LOCAL aArray

   IF !bBlq=NIL

      IF ValType(bBlq)="A"
         aArray:=bBlq
         bBlq:={||aArray}
      ENDIF

      RETURN {||PUBLICO("oGrid",oGrid),EVAL(bBlq)}

   ENDIF

RETURN bBlq




