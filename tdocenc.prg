/*
// TdocEnc
// Encabezado del Documento
*/

#include "FiveWin.ch"
#include "InKey.ch"
#include "xbrowse.ch"
#include "\DWH\LIBS\TSBUTTON.CH"
#include "Obj2Hb.ch"
#include "RichEdit.ch"

// #define SM_CXSCREEN         0

MEMVAR oDp

STATIC nTop,nLeft,nWidth,nHeight,nContar
STATIC nNumEdit,aForms

FUNCTION DOCENC(cTitle,cVarName,cFileEdt)
RETURN TDOCENC():New(cTitle,cVarName,cFileEdt)

CLASS TDOCENC

    DATA aKeys
    DATA aFields
    DATA aBtn
    DATA aGrids
    DATA aDataOrg      // Verifica la Data que sera Modificada
    DATA aIncremental  // Campos AutoIncremental
    DATA aScrollGets
    DATA aDpEdit
    DATA aBtnNew
    DATA aBtnEdit     // Botones de Edición
    DATA aMemo
    DATA aFind        // Controles Find
    DATA aDataTable
    DATA aSayGroup INIT ARRAY(250)
    DATA aFieldsC  INIT {} // 29/12/2025
//    DATA HANDLEEVENT

    DATA oTable
    DATA oDb
    DATA oWnd
    DATA oDlg
    DATA oBar
    DATA oBrw,oBrwO
    DATA oScript
    DATA oControl // Control en Focalizado
    DATA oFocus   // Este el Focus para Incluir y Modificar
    DATA oFontBtn // Fuente del Botón
    DATA oExport  // Control de Exportación
    DATA oScroll
    DATA oEditMemo //
    DATA oFocusFind // Foco Donde se Inicia la Búsqueda
    DATA oSayMsgErr // Mensajes de Error en FLASH
    DATA oGridFocus //  Grid con el Foco
    DATA oSayRecord // Muestra en la Super de la Barra la Posición
    DATA aBtnMnu INIT {} // Botones con funciones del menu


    DATA cBtnList INIT "xBrowse.bmp"

    DATA cTable
    DATA cCancel
    DATA cTitle
    DATA cFileEdit
    DATA cLoad
    DATA cAfterLoad
    DATA cView    // Procedimiento de Consultas
    DATA cSql
    DATA cVarName
    DATA cPrimary
    DATA cWhere
    DATA cScript
    DATA cScope,cScopeF
    DATA cFieldList
    DATA cRepeat
    DATA cReport // Reporte
    DATA cWhereRecord
    DATA cSqlDelete INIT "" // Sql Para Borrar
    DATA uData      // Data del Registro Visualizado
    DATA cIncluir
    DATA cConsultar
    DATA cModificar
    DATA cBuscar
    DATA cEliminar
    DATA cExport    // Campo que será Exportado
    DATA cImport    // Campo que Importa el Valor
    DATA cPreSave   // PreGrabar
    DATA cPostSave
    DATA cList      // Archivo *.BRW para la Edición
    DATA cSingular
    DATA cSayMsgErr  // Mensaje de Error en FLASH
    DATA cWhereOpen  // Where de Apertura
    DATA cScrFind    INIT "DOCFIND"
    DATA cCommit     INIT "DOCCOMMIT" // Antes de Guadar el Registro
    DATA cScopeFind  // Filtro para Buscar
    DATA cSetFind    // cFind
    DATA cKey        INIT ""  // Para detectar combinacion de teclas con Ctrl o Alt.
    DATA cIdFrm      INIT ""  // Identificación del Formulario
    DATA cFieldFile  INIT ""
    DATA cTitleFile  INIT ""  // Titulo
    DATA cFileRtf    INIT "" // Archivo de Ayuda RTF
    DATA cFileRtfOld INIT oDp:cHelpRtf
    DATA cSqlIni,cSqlFin
    DATA cScope_Update INIT NIL // Scope para Where de Update
    DATA cFieldAud     INIT ""  // Pistas de Auditoría
    DATA cFileEdt
    DATA cFileBrwZ    INIT ""
    DATA cTextGroup   INIT ""
    DATA cOnClose     INIT "ONCLOSE"
    DATA cFieldChkSum INIT ""
    DATA cDsnData     INIT oDp:cDsnData

    // DATA INIT AS NUMERIC nFileMain
    DATA nFileMain AS NUMERIC INIT 0
    DATA nTime

    DATA nOption
    DATA nContar
    DATA nBtnWidth
    DATA nBtnHeight
    DATA nCuerpo
    DATA nBtnStyle
    DATA nForms
    DATA nTop,nLeft,nWidth,nHeight
    DATA nGris
    DATA nRecno    INIT 0
    DATA nRecCount INIT 0

    DATA lBar
    DATA lDesign   // Indica si Requiere modo Diseño
    DATA lMsgError
    DATA lActivated
    DATA lSaved
    DATA lIsDef
    DATA lPrint
    DATA lAutoEdit // Solo sirve para Modificar
    DATA lView     // Opción Consultar
    DATA lFind     // Opción Buscar
    DATA lFound    // Encontrado
    DATA lOkJn
    DATA lValUnique // Validar
    DATA lAutoInc
    DATA lSavedDsn      INIT .F.
    DATA lEOF           INIT .T.
    DATA lBOF           INIT .F.
    DATA lCancel        INIT .F. // JN 19/09/2016
    DATA lCancelClic    INIT .F. // JN 22/08/2023 Indica si el boton Cancelar fue Utilizado necesario para validaciones bLostFocus
    DATA lCreaRegIntRef INIT .T.
    DATA lRecordSeltor  INIT .T.
    DATA lLock          INIT .T.
    DATA lMsgErr        INIT .T. // Muestra el mensaje de Error del DPCBTE
    DATA lOPENSIZE      INIT .F. // Auto Size, Auto Ajusta el Browse y Dialog
    DATA lRegFiscal     INIT .F. // Registro Fiscal, debe guardar pista de auditoria enscriptada
    DATA lDsnData       INIT .T.


    DATA lBtnText       INIT .F.  // Texto para los Botones

    DATA lMdiBar        INIT .F. // En caso .T. La Barra de botones sera ampliada

    DATA lBtnIncRun     INIT .F. // Incluir fue activado desde el boton. Difiere de autoincluir luego de finalizar, esto es necesario cuando se factura con impresora fiscal,
                                 // debe validar que la factura fue impresa y aun esta en recibo de ingreso

    DATA lInc INIT .T.
    DATA lCon INIT .T.
    DATA lMod INIT .T.
    DATA lEli INIT .T.
    DATA lPrn INIT .T.
    DATA lAll INIT .F.
    DATA lLock INIT .F.
    DATA lCancelOff INIT .F.

    DATA uValue1,uValue2,uValue3,uValue4

    DATA bPostRun    INIT {||NIL}
    DATA bLostFocus  INIT {||NIL}
    DATA bGotFocus   INIT {||NIL}

    DATA NOTIFY

    METHOD New( cTitle , cVarName , cFileEdit )
    METHOD Windows(nTop,nLeft,nWidth,nHeight)
    METHOD Activate(bInit,bEnd)
    METHOD BtnSetMnu(cBtn,cOption,cFunction,cBtnText) INLINE AADD(::aBtnMnu,{cBtn,cOption,cFunction,cBtnText}) // Opciones para ser Incluidas en los Botones


    METHOD EditFile() INLINE ::nFileMain:=EJECUTAR("DPFILEEMPMAIN",::nFileMain,::cTitleFile,::cTable,::nOption=0,nil,nil,nil,SELF)

    METHOD CTOMEMO(cKey,lHead,cSep) INLINE EJECUTAR("TDOCENCMEMO",Self,lHead,cSep,cKey)
    METHOD LockTable( )
/*
INLINE  (::oTable:=IF(::oTable=NIL,Opentable("SELECT * FROM "+::cTable,.F.,::oDb),::oTable),;
                                 IIF(!::lLock,::oTable:LockTable(),NIL),;
                                 ::lLock:=.T.)
*/

    METHOD UnLockTable( ) INLINE (::oTable:=IF(::oTable=NIL,Opentable("SELECT * FROM "+::cTable,.F.,::oDb),::oTable),;
                                    IIF(::lLock,::oTable:UnLockTable(),NIL),;
                                   ::lLock:=.F.)



    METHOD PutBar()
    METHOD Delete()
    METHOD IsData()
    METHOD RefreshBtn1() // JN 15/09/2016
    METHOD SetIncremental(cField,cWhere,cNumero) INLINE AADD(::aIncremental,{cField,cWhere,cNumero})
    METHOD BuildIncremental(lSave)
// jn 13/12/2013    METHOD Printer() INLINE (::LoadData(0),::RunScript("PRINTER"))
//  ::LoadData() innecesario, va a imprimir lo que esta en LoadData
    METHOD Printer(uPar1,uPar2,uPar3,uPar4,uPar5) INLINE (::RunScript("PRINTER",uPar1,uPar2,uPar3,uPar4,uPar5))

    METHOD RunExport()
    METHOD EditMemo() INLINE ::oEditMemo:=_DPMEMOEDIT(Self,::oEditMemo,::oDlg)
    METHOD SetFocusBtn(cName)

    METHOD SetTable(cTable , cPrimary , cWhere , lAll, cInner,oDb,cOrderBy,lDesc)
    METHOD Close()
    METHOD RunKey(nKey)
    METHOD SeekTable(cTable)
    METHOD Say(nLin,nCol,cText)
    METHOD SetKey(nKey,bAction) INLINE AADD(::aKeys,{nKey,BloqueCod(bAction)})
    METHOD SeekTable(cTable,cField,uValue,cDescri,oSay,cWhere)
    METHOD FindGet()
    METHOD PreDelete()
    METHOD GridEdit(cTable , cLinkdHead , cLinkGrid , cSql , cGroupBy )
    METHOD SetScript(cScript)
    METHOD SetScope(cScope,cScopeF) INLINE (::cScope :=cScope,;
                                            ::cScopeF:=cScopeF)
    METHOD BtnCancelOff()

    METHOD RECCOUNT(lIni)

    METHOD ONCLOSE()

    METHOD SETOPENSIZE() INLINE ::lOPENSIZE:=.T.
    METHOD OPENSIZE()

    METHOD ISDOCMODFISCAL() INLINE EJECUTAR("ISDOCMODFISCAL",SELF) // modificar
    METHOD ISDOCNULFISCAL() INLINE EJECUTAR("ISDOCNULFISCAL",SELF) // Anular Documento Fiscal

//  METHOD BtnWhen()
/*
    METHOD Prepare() INLINE (PUBLICO("oDoc",Self),;
                             PUBLICO(::cVarName,Self),;
                             MOVER(Self,::cVarName),;
                             MOVER(Self,"oDoc"),;
                             AEVAL(::aGrids,{|a,n|a:Prepare()}))
*/

    METHOD Prepare() INLINE ( __MVPUBLIC("oDoc"),;
                              __MVPUT(::cVarName,Self),;
                              __MVPUBLIC(::cVarName),;
                              oDp:cHelpRtf:=::cFileRtf,;
                              MOVER(::cVarName,Self),;
                                AEVAL(::aGrids,{|a,n|a:Prepare()}))

    METHOD GotFocus(lAuto)
    METHOD LostFocus()
    METHOD Cancel(lAsk)
    METHOD Presave(lSave)
    METHOD MensajeErr(cVar,cTitle) INLINE MensajeErr(cVar,cTitle)
    METHOD ListBrw(cFileBrw)
    METHOD Save()
    METHOD GetWhere(cFields)
//    METHOD SetBtnWhere(cOpc,lOpc)
//  METHOD SetScope(cWhere)
    METHOD Repeat(cRepeat)
    METHOD Get(cVarName)
    METHOD Set(cField,uValue)
    METHOD Add(cField,uValue)
    METHOD IsDef(cVarName)
    METHOD LoadData(nOption , nNext , lStart , cWhere)
    METHOD SetColorBar(nClrText,nClrPane) // Color para las Barras
    METHOD RunScript(cFunction,nPar1,nPar2,nPar3,nPar4)
    METHOD IsFunction(cFunction) INLINE ::oScript:IsFunction(cFunction)
    METHOD RunRepeat()
    METHOD BtnPaint()
    METHOD ValUnique(uValue,cField)
    METHOD Mensaje(cMensaje,cTitle)
    METHOD Primero(lPrimero,cWhere)
    METHOD Skip(nSkip)
    METHOD GetWhereMax(cField , cWhere , lMax)
    METHOD GetMax(cField,cWhere)
    METHOD SetExport(oExport,cExport,cImport)
    METHOD SetMsg(cMsg)  INLINE (::oDlg:oMsgBar:SetMsg(cMsg),::oDlg:oMsgBar:Refresh())
    METHOD KillForms()
    METHOD ScrollGet()
    METHOD GetValue(cField)
    METHOD SetValue(cField,uValue)
    METHOD LinkEdit(oDpEdit) INLINE (AADD(::aDpEdit,oDpEdit)) // ,DpFocus(oDpEdit:oDlg)) // Relación Entre Formularios
    METHOD AddBtn(cFile,cText,cWhen,cAction,cFind,nPos,cKey)
    METHOD AddBtnEdit(cFile,cText,cWhen,cAction,cFind,cKey)
    METHOD SetMemo(cField,cTitle,nTop,nLeft,nWidth,nHeight)
    METHOD ChkIntRef(cField)
    METHOD CancelFind()
    METHOD SetAdjuntos(cField,cTitle)
    METHOD Find()
    METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)

    METHOD UpDateFromTable()

    // METHOD BtnForWhen()

    METHOD End()

    METHOD HandleEvent( nMsg, nWParam, nLParam ) INLINE 1=1

      /*
       EXTERN ;
                             WndHandleEvent( Self, nMsg, nWParam, nLParam )
     */
/*
    METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                             WndHandleEvent( Self, nMsg, nWParam, nLParam )
*/

    ERROR HANDLER OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 )

ENDCLASS

METHOD New(cTitle, cVarName , cFileEdit) CLASS TDOCENC

     DEFAULT nNumEdit:=0,aForms:={}

     MsgDemo()

     nNumEdit++

     PUBLICO("oDoc",SELF)
     MOVER(Self,"oDoc")

     ::nForms      :=nNumEdit
     ::lIsDef      :=.F.
     ::cTitle      :=cTitle // +" "+cFileEdit
     ::aFields     :={}
     ::cVarName    :=cVarName
     ::cRepeat     :=""  // Lista de Campos que se Repetiram
     ::nBtnWidth   :=40
     ::nBtnHeight  :=40
     ::lBar        :=.T. // Indica Barra de Botones
     ::lDesign     :=.F. // Modo Diseño Activo
     ::nCuerpo     :=0   // No tiene Cuerpo Activo
     ::nBtnStyle   :=1
     ::nGris       :=oDp:nGris
     ::aGrids      :={}
     ::lMsgError   :=.F.
     ::lPrint      :=.T.
     ::lActivated  :=.F.
     ::lAutoEdit   :=.F.  // Sólo puede Modificar
     ::lSaved      :=.F.
     ::lView       :=.T.  // Siempre Hay Consultar
     ::lFind       :=.F.  // Buscar es Optativo
     ::cScope      :=""   // Limita todo el Query
     ::cScope      :=""
     ::cWhereRecord:=""
     ::cLoad       :="LOAD"
     ::cCancel     :="CANCEL"
     ::aIncremental:={}
     ::cIncluir    :=MI("Incluir"  ,601)
     ::cConsultar  :=MI("Consultar",602)
     ::cModificar  :=MI("Modificar",603)
     ::cEliminar   :=MI("Eliminar" ,604)
     ::cBuscar     :=MI("Buscar"   ,605)


     ::aScrollGets :={}
     ::aDpEdit     :={}
     ::aBtnNew     :={} // Botones Adicionales
     ::aBtnEdit    :={} // Botones de Edición
     ::cList       :=""
     ::cSingular   :="Nombre Singular"
     ::cSayMsgErr  :=SPACE(50)
     ::cFindNoEnter:=""
     ::lOkJn       :=.F.
     ::lValUnique  :=.T.
     ::cWhereOpen  :=""
     ::lAutoInc    :=.F.

     ::lInc:=.T.
     ::lCon:=.T.
     ::lMod:=.T.
     ::lEli:=.T.
     ::lPrn:=.T.

     IF !Empty(cFileEdit)
       ::cFileEdit   :=IIF( "\"$cFileEdit.OR.":"$cFileEdit,"" ,oDp:cPathEdt)+cFileEdit        // Nombre del Archivo donde se Guardan los Documentos
     ELSE
       ::cFileEdit   :=""
     ENDIF

     IF !FILE(::cFileEdit) .AND. !Empty(oDp:cFileSource) .AND. FILE(oDp:cFileSource+::cFileEdit)
       COPY FILE (oDp:cFileSource+::cFileEdit) TO (::cFileEdit)
     ENDIF

     ::cTitle      :=::cTitle // +" "+::cFileEdit

     ISFILESTD(::cFileEdit,.T.)

     DEFAULT nContar:=0

     ::cFileEdt:=cFileEdit

     ::nContar:=nContar++
     ::aKeys  :={}

     PUBLICO(::cVarName,Self)
     MOVER(Self,::cVarName)

     ::SetScript()

     AADD(aForms,{::nForms , cVarName , Self})

RETURN SELF


METHOD LockTable( )

   (::oTable:=IF(::oTable=NIL,Opentable("SELECT * FROM "+::cTable,.F.,::oDb),::oTable),;
   IIF(!::lLock,::oTable:LockTable(),NIL),;
   ::lLock:=.T.)

RETURN .T.


/*
// Realiza la Búsqueda de Controles, solo en Controles Asociados a Variables
*/
METHOD Find() CLASS TDOCENC
    LOCAL I,cVar,oGet,aItems,nAt,uValue,aOptions,cTexto

    ::oWnd:SetText(ALLTRIM(::cTitle) + " ["+::cBuscar+"]")
    ::nOption:=5
    ::BtnPaint()

    ::aFind:={}
    ::lFind:=.F.


    // Grid debe ser reiniciado
    Aeval(::aGrids,{|o,n| o:NewJob(1,.t.),o:nOption:=0,o:AdjustBtn()})


    FOR I := 1 TO LEN(::aFields)

       cVar:="O"+UPPE(::aFields[I,1])

       IF ::IsDef(cVar)

         oGet  :=::Get(cVar)

         IF ValType(oGet)<>"O"
            LOOP
         ENDIF

         uValue:=EVAL(oGet:bSetGet)

         IF "COMBO"$oGet:ClassName()

            aItems  :=ACLONE(oGet:aItems)
            aOptions:=ACLONE(aItems)
            AADD(aOptions,CTOEMPTY(aItems[1]))

            oGet:SetItems(aOptions)
            oGet:Select( Len(aOptions))
            oGet:nAt:=LEN(aOptions)

//  oGet:bChange:=FrmFindBlq(Self,oGet,::aFields[I,1],::cScrFind)
// ? "aquies combo",oGet:ClassName()

         ELSE

            aItems:=NIL
            nAt   :=0
            EVAL(oGet:bSetGet,CtoEmpty(oGet:VarGet())) // Coloca Vacio
            oGet:Refresh(.T.)

         ENDIF

         AADD(::aFind,{::aFields[I,1],cVar,oGet,oGet:bValid,oGet:bWhen,uValue,aItems,nAt,oGet:bKeyDown,oGet:bChange})

         oGet:bWhen   :={||.T.}
         oGet:bValid:=FrmFindBlq(Self,oGet,::aFields[I,1],::cScrFind)

         // 19/09/2016 Refrescar TSAY

       ENDIF
    NEXT

    // 19/09/2016 Refrescar los TSAY
    FOR I := 1 TO LEN(::aFields)

       cVar:="O"+UPPE(::aFields[I,1])

       IF ::IsDef(cVar)

        oGet  :=::Get(cVar)

        IF "TSAY"$oGet:ClassName()
           oGet:Refresh(.T.)
        ENDIF

       ENDIF

    NEXT I


    IF ::oFocusFind!=NIL

       ::oFocusFind:ForWhen()
       DpFocus(::oFocusFind) // Busca por Control

    ELSEIF ValType(::oFocus)="O"

       ::oFocus:ForWhen()
       DpFocus(::oFocus)

    ENDIF

    IF !Empty(::cSetFind)
      ::RunScript(::cSetFind)
    ENDIF

RETURN .T.

/*
// Asigna Valor y Refresca Objeto
*/
METHOD SetAdjuntos(cField,cTitle,cTabla,lRun) CLASS TDOCENC

   LOCAL cFileBmp:="adjuntar.bmp"

   DEFAULT cTitle:=MI("Asociar Archivos Adjuntos",9),;
           cTabla:=oDp:cTableDpFile,;
           lRun  :=.T.

   IF !::IsDef(cField) // Campo no Existe
     MensajeErr("Campo "+cField+" No Existe")
     RETURN .F.
   ENDIF

   cTitle:=GetFromVar(cTitle)
   // Campo Vinculado con los archivos Adjuntos
   ::cFieldFile:=cField
   ::nFileMain :=::Get(cField)
   ::cTitleFile:=cTitle

   // ::EditButtons(cTitle , cFileBmp ,{||::nFileMain:=EJECUTAR("DPFILEEMPMAIN",::nFileMain,cTitle,::cTable,::nOption=0 .OR. ::nOption=2)})

RETURN NIL



//METHOD SetBtnWhere(cOpc,lOpc)
//     ? cOpc,lOpc
//RETURN .T.

METHOD SetFocusBtn(cName) CLASS TDOCENC
   LOCAL nAt:=ASCAN(::aBtn,{|a,n|ALLTRIM(UPPE(a[3]))=ALLTRIM(UPPE(cName))})
   LOCAL oBtn

   IF nAt>0
      oBtn:=::aBtn[nAt,1]
      DpFocus(oBtn)
      // ? nAt,::aBtn[nAt,3]
   ENDIF

RETURN oBtn


/*
// Cancelar buscar
*/
METHOD CancelFind() CLASS TDOCENC
   LOCAL oGet,I,uValue

   DEFAULT ::aFind :={},;
           ::lFound:=.F.

// ? "AQUI ES CANCELFIND" , ::nOption

   FOR I := 1 TO LEN(::aFind)

       oGet         :=::aFind[I,3]
       oGet:bValid  :=::aFind[I,4]
       oGet:bWhen   :=::aFind[I,5] // Este lo hace Lento
       // oGet:bKeyDown:=::aFind[I,8] // AADD(::aFind,{::aFields[I,1],cVar,oGet,oGet:bValid,oGet:bWhen,uValue,aItems,nAt,oGet:bKeyDown})

       IF "COMBO"$oGet:ClassName()
          oGet:SetItems(::aFind[I,7])
          COMBOINI(oGet)
       ENDIF

   NEXT

   IIF( ValType(oGet)="O" , oGet:ForWhen(.T.) , NIL  )

   //? ValType(::aFind),::lFound

   ::nOption:=0

   IF !Empty(::aFind) .AND. !::lFound
      ::LoadData(0)
   ENDIF

   ::aFind:={}
   ::lFind:=.F.

RETURN .T.

METHOD ONCLOSE() CLASS TDOCENC

 IF Empty(::cOnClose)
   RETURN .T.
 ENDIF

 IF !::oScript:IsFunction(::cOnClose)
    RETURN .F.
 ENDIF

RETURN ::RunScript(::cOnClose)

/*
// Borrar Registro
*/
METHOD SetExport(oExport,cExport,cImport) CLASS TDOCENC

   ::oExport:=oExport // Control
   ::cExport:=cExport // Campo para Exportar
   ::cImport:=cImport // Campo para Importar en el Control

   IF ValType(::oExport)="O" .AND. ::oExport:ClassName()="TXSCROLL" .AND. Empty(cImport)
//      ::oExport:oFocus:=::oDlg
//      ? ::oDlg:ClassName()
      ::cImport:=::oExport:aData[::oExport:oBrw:nArrayAt,1] // Busca el Nombre del Campo
   ENDIF


   IF ValType(::oExport)="O" .AND. ::oExport:ClassName()="TDOCGRIDCOL" .AND. Empty(cImport)
      ::cImport:=::oExport:oGrid:oBrw:aArrayData[::oExport:oGrid:oBrw:nArrayAt,::oExport:nCol] // Busca el Nombre del Campo
   ENDIF

RETURN NIL

METHOD FindGet(oDlg,lBrw)
   LOCAL I,oGet,oFocus:=NIL,U
   LOCAL cList:="TGET,TMULTIGET,TCOMBOBOX,TBMPGET,TCHECKBOX"

   DEFAULT oDlg:=::oDlg ,lBrw:=.T.

   IF Empty(oDlg:aControls)
      RETURN NIL
   ENDIF

   oDlg:aControls[1]:ForWhen()

   FOR I := 1 TO LEN(oDlg:aControls)
      oGet:=oDlg:aControls[I]
      IF oGet:ClassName()="TFOLDER"
         FOR U := 1 TO LEN(oDlg:aControls[I]:aDialogs)
            IF (oGet:=::FindGet(oDlg:aControls[I]:aDialogs[u],.F.),!Empty(oGet))
               EXIT
            ENDIF
         NEXT
      ENDIF
      IF oGet:ClassName()$"TGET,TBMPGET,TCOMBOBOX" .AND. ISBLOCK(oGet:bWhen) .AND. Eval(oGet:bWhen)
         oFocus:=oGet
         EXIT
      ENDIF
   NEXT

   IF lBrw .AND. oFocus=NIL .AND. !Empty(::aGrids)
      oFocus:=::aGrids[1]:oBrw
   ENDIF

   IF ValType(oFocus)="O"
     // ? "aqui hace oFocus"
//     MensajeErr(oFocus:ClassName())
     DpFocus(oFocus)
     ::oFocus:=oFocus
   ENDIF

//   IIF( ValType(oFocus)="O" , (DpFocus(oFocus),::oFocus:=oFocus),;
//                              IIF( LEN(::aGrids)>0,::oGrids[1]:NIL,NIL ))

RETURN oFocus


/*
// Asigna Valores de Relación con DPMEMO a Nivel de Descripción Amplia
*/
METHOD SetMemo(cField,cTitle,nTop,nLeft,nWidth,nHeight) CLASS TDOCENC

   LOCAL aData:={},nNumMemo:=0

   DEFAULT cTitle:="Descripción Amplia"

   IF !::IsDef(cField) // Campo no Existe
     MensajeErr("Campo "+cField+" no existe","SetMemo()")
     RETURN .F.
   ENDIF

   nNumMemo:=::Get(cField)

   cTitle:=GetFromVar(cTitle)

   ::aMemo:={cField,cTitle,nTop,nLeft,nWidth,nHeight,nNumMemo,"",""}

//   AADD(::aBtnEdit, { "xMemo2.bmp"  ,;
//                      cTitle      ,;
//                      "oGrid:nOption!=0",;
//                      "KillGetCol(oGrid:oBrw),oGrid:oEditMemo:=_DPMEMOEDIT(oGrid,oGrid:oEditMemo,oGrid:oBrw),oGrid:oHead:LinkEdit(oGrid:oEditMemo)",;
//                      "Mem" })

RETURN NIL




/*
// Botones Adicionales
// nPos:=Indica la Posición
*/
METHOD AddBtn(cFile,cText,cWhen,cAction,cFind,nPos,cKey,cBtnText) CLASS TDOCENC

   DEFAULT cFind:="OTHER",cText:="",cWhen:="oDoc:nOption=0",cAction:="MsgAlert('OK')",cKey:="KEY"

   IF ValType(cKey)="N"
      cKey:=STR(cKey)
   ENDIF

//   ? "CWHEN->",cWhen,"ACTION->",cAction
   AADD(::aBtnNew,{NIL  ,cFile         ,cText            ,cAction                 ,cWhen                        , {||.T.} ,"OTHER",cFind,cKey ,cBtnText})
//   AADD(::aBtn   ,{NIL,"xNew.bmp"    ,"Incluir  "      ,"oDoc:LoadData(1)"      ,"oDoc:nOption =0"  , {||.T.} ,"NEW"  ,"Inc" })

RETURN NIL

METHOD AddBtnEdit(cFile,cText,cWhen,cAction,cFind,nPos,cKey,cBtnText) CLASS TDOCENC

   DEFAULT cFind:="OTHER",cText:="",cWhen:="oDoc:nOption=0",cAction:="MsgAlert('OK')",cKey:="cKey"

   AADD(::aBtnEdit,{NIL  ,cFile         ,cText            ,cAction                 ,cWhen                        , {||.T.} ,"OTHER",cFind,cKey,cBtnText })

RETURN NIL

/*
// Colocar Colores en la Barra de Botones
*/
METHOD SetColorBar(nClrText,nClrPane) // Color para las Barras

   IF !Empty(::aBtn)
     ::aBtn[1,1]:oWnd:SetColor(nClrText,nClrPane)
     AEVAL(::aBtn[1,1]:oWnd:aControls,{|o,n|o:SetColor(nClrText,nClrPane)})
   ENDIF

RETURN NIL

/*
// Carga Datos para el Formulario
*/
METHOD ListBrw(cWhere,cList,cTitle,cOnlyWhere,cJoin) CLASS TDOCENC

    LOCAL lResp,uValue,uValueIni,cSql

    DEFAULT cList:= ::cList

    IF !"."$cList
       cList:=cList+".BRW"
    ENDIF
/*
    IF !FILE("FORMS\"+cList)
       MensajeErr("Archivo : FORMS\"+cList+"no Existe")
    ENDIF
*/
    oDp:cWhereBrw:=""

    SETDBSERVER(::oDb)

// ? cWhere,"cWhere",::cScope,"::cScope",GETPROCE()

IF " JOIN "$::cScope

    cWhere:=::cScope

// ? " TIENE JOIN",::cScope

ELSE

    IF !Empty(cWhere) .AND. !Empty(::cScope)
       cWhere:=::cScope+" AND "+cWhere
    ELSEIF Empty(cWhere) .AND. !Empty(::cScope)
       cWhere:=::cScope
    ENDIF

ENDIF

// ? cWhere,"cWhere",::cScope,"::cScope"

    // lResp:=DPBRWPAG(cList,NIL,@uValue,::cPrimary,.T.,::cScope,cTitle,::oDb,cOnlyWhere)

    cTitle:=CTOO(cTitle,"C")
    cTitle:=ALLTRIM(cTitle)

    lResp:=DPBRWPAG(cList,NIL,@uValue,::cPrimary,.T.,cWhere,cTitle,::oDb,cOnlyWhere,cJoin)

    SETDBSERVER()

//? oDp:cWhereBrw ," VIENE DE BRWPAGO"

    IF !Empty(oDp:cWhereBrw)
      ::cWhere:=" WHERE "+IIF(Empty(::cScope),"",::cScope+" AND ")+oDp:cWhereBrw
      ::LoadData(0)
      ::RECCOUNT(.F.) // JN 16/09/2016 Debe obtene la posicion del registro
    ELSE
      RETURN .F.
    ENDIF

    // ? lResp,uValue,oDp:cWhere,::cScope

RETURN .T.
/*
    uValue   :=::oTable:GetValue(::oTable:cPrimary)
    uValueIni:=uValue

     IF ValType(::cList)="U"

       MsgInfo("Hay que Desarrollar List/Paginado")

     ELSE

       DO CASE

          CASE ValType(::cList)="C" .AND. ".BRW"$UPPE(::cList)

              lResp:=DPBRWPAG(::cList,NIL,@uValue,::oTable:cPrimary,.T.)

              IF uValueIni!=uValue .AND. ValType(uValueIni)==ValType(uValue)// Seleccionó el Registro

                ::oTable:cSql:="SELECT * FROM "+::oTable:cTable+" WHERE "+BuildConcat(::oTable:cPrimary)+;
                               GetWhere("=",uValue) + " LIMIT 1"

                // ::oTable:cSql:=BuildLimit(::oTable:cSql,oDp:cTypeBD)

                ::oTable:Reload()

                // ? ::oTable:cSql

                ::Load(0)

              ENDIF


          CASE  ValType(::cList)="C"

             EJECUTAR(::cList,::oTable)

          CASE ValType(::cList)="B"

             EVAL(::cList,::oTable    )

       ENDCASE

     ENDIF
*/
RETURN NIL




/*
// Ejecución de la Exportación
*/
METHOD RunExport() CLASS TDOCENC
    LOCAL uValue

    IF !ValType(::oExport)="O" // Murio el Control
       RETURN .F.
    ENDIF

    ::Prepare()

    IF ValType(::cExport)="C"
       uValue:=::Get(::cExport)
    ENDIF

    IF ValType(::cExport)="B"
       uValue:=Eval(::cExport)
    ENDIF

    DO CASE

       CASE ::oExport:ClassName()="TXSCROLL" .AND. ::oExport:oForm:oWnd:hWnd>0

           ::oExport:Import( ::cImport , uValue )
           // Aeval(::oExport:oBrw:aCols,{|o,n|o:KillGet()})
           // DpFocus(::oExport:oBrw)

       CASE ::oExport:ClassName()="TDOCGRIDCOL" .AND. ::oExport:oGrid:oBrw:oWnd:hWnd>0

           ::Close()

           ::oExport:Import( uValue )

       CASE "GET"$::oExport:ClassName() .AND. ::oExport:oWnd:hWnd>0

           ::Close()

           ::oExport:VarPut( uValue , .T. )
           DpFocus(::oExport)

   ENDCASE

   ::Close()

RETURN .T.


/*
// Asigan el ScrollGet hacia el Formulario
*/
// METHOD Delete() CLASS TDOCENC
METHOD SCROLLGET(cTable,cFileScg,cExcluye,oWnd,nType,oFont) CLASS TDOCENC // [JN]

   LOCAL oForm:=Self
   LOCAL oScroll

// ? "SCROLLGET"

   DEFAULT cTable  :=::cTable
   DEFAULT cFileScg:=cFileName(::cFileEdit)
   DEFAULT oWnd    :=GetWndDefault() //::oDlg
   DEFAULT nType   :=1 // ScrollGet

   // ::CreateWindow()
   // ? "SCROLLGET",cTable,cFileScg,oForm:nOption

   oScroll:=SCROLLGET(cTable,cFileScg,cExcluye,oWnd,nType,oForm,oFont)

   AADD(::aScrollGets,oScroll)

   oScroll:oForm:=Self  // Asocia ScrollGet con el Formulario

   IF oForm:nOption=0 // No puede Modificar
//? "oScroll:SetEdit(.F.,0)"
     oScroll:SetEdit(.F.,0)
   ENDIF

RETURN  oScroll

/*
// Asigna el Valor y Refresca el Control
*/
METHOD SetValue(cField,uValue)
   LOCAL oGet

//   ErrorSys(.T.)
   IF ValType(cField)="C"
      ::Set(cField,uValue)
   ENDIF

   IF ValType(cField)="O" //.AND. ::lActivated

      IF "SAY"$cField:ClassName()
        cField:SetText(uValue)
        cField:Refresh(.T.)
      ELSE
        cField:VarPut(uValue,.T.)
      ENDIF

   ELSEIF ValType(cField)="C"

      ::Set(cField,uValue)

      Aeval(::aScrollGets,{|o|o:Import(cField,uValue)})

      oGet:=Get("O"+cField)

      IF ValType(oGet)="O" .AND. ::lActivated

//         ErrorSys(.T.)

         IF "SAY"$oGet:ClassName()
           oGet:SetText(uValue)
           oGet:Refresh(.T.)
         ELSE
           oGet:VarPut(uValue,.T.)
         ENDIF

      ENDIF

   ENDIF

RETURN NIL

/*
// Borrar Registro
*/
METHOD Delete() CLASS TDOCENC

   LOCAL oOdbc:=GetOdbc(::cTable),lResp:=.F.


//   DEFAULT ::cSqlDelete:="DELETE FROM "+::cTable+" "+::GetWhere()

   IF !Empty(::cSqlDelete)

      lResp:=oOdbc:Execute( ::cSqlDelete )

   ELSE

      lResp:=SQLDELETE(::cTable,::GetWhere())

   ENDIF

   // ? oDp:cSql,"METHODO DELETE",GETPROCE()

   IF !lResp
      MensajeErr("No es posible Borrar el Registro"+IF(::lMsgErr,CRLF+::cSqlDelete,""))
   ENDIF

RETURN  lResp

/*
// Obtener Maxmimo Valor
*/
METHOD GetMax(cField,cWhere) CLASS TDOCENC
    LOCAL oTable,uValue

    DEFAULT cWhere:=""

    IF !Empty(cWhere) .AND. !(" WHERE "$" "+cWhere)
       cWhere:=" WHERE "+cWhere
    ENDIF

   //  ? ::cWhere,::cScope,"GETMAX"

    oTable:=OpenTable("SELECT MAX("+cField+") FROM " + ::cTable+ "  "+cWhere , .T.,::oDb )
//    ? oTable:cSql
    uValue:=oTable:FieldGet(1)
    oTable:End()

RETURN uValue
/*
// Busca el Where Según el Valor mas alto
*/
METHOD PreDelete() CLASS TDOCENC
    LOCAL cWhere:=::GetWhere()
    LOCAL cKey  :=::cPrimary
    LOCAL uKey  :=::GetValue(cKey)

    ::cSqlDelete:=NIL

    IF ::RunScript("PREDELETE") .AND. !::ISDOCNULFISCAL()

       CursorWait()

//? "AQUI ES VERDADERO"

       IF ::Delete()

         AUDITAR("DELI" , NIL ,::cTable , uKey , NIL, SELF )

         ::RunScript("POSTDELETE")
         ::Skip(-1)

         IF cWhere=::cWhere // no puede más hacia delante
            ::Primero(.t.) // Busca el Primer Registro
         ENDIF

         IF cWhere=::cWhere
            Eval(::aBtn[1,1]:bAction)
         ENDIF

       ENDIF

    ELSE

// ? "AQUI ES FALSO"

    ENDIF

RETURN .T.

/*
// Si no Hay Data Incluye
*/
METHOD ISData()
  LOCAL lResp :=.T.
  LOCAL cWhere:=::GetWhere()

  ::Skip(-1)

  IF cWhere=::cWhere // no puede más hacia delante
    ::Skip(+1) // Busca al Siguiente
  ENDIF

  IF cWhere=::cWhere // no puede más hacia delante
    ::Primero(.t.) // Busca el Primer Registro
  ENDIF

  IF cWhere=::cWhere
    Eval(::aBtn[1,1]:bAction)
    lResp:=.T.
  ENDIF

RETURN lResp


/*
// Busca el Where Según el Valor mas alto
*/
METHOD GetWhereMax(cField , cWhere , lMax) CLASS TDOCENC
   LOCAL cSql
   LOCAL oTable,uValue,aFields:={},I,cList,nCount:=0,nT1:=SECONDS()

   DEFAULT cWhere:="",lMax:=.T.

// DEFAULT cWhere:=::cScope,lMax:=.T.
// ? "3",::cScope,ErrorSys(.T.)
// ? cField,"4",GETPROCE()

   IF !","$cField

      IF !Empty(::cScope) .AND. !Empty(cWhere)

         cWhere:=cWhere +   " " + IIF( Empty(cWhere) , " WHERE ", " AND " ) +::cScope

      ENDIF

      IF !Empty(::cScopeF) .AND. !Empty(cWhere)

// ? "AQUI ES GETWHEREMAX",cWhere,::cScope,"4"

         cWhere:=cWhere + " " + IIF( Empty(cWhere) , " WHERE ", " AND " ) +;
                 ::cScopeF


      ENDIF

      IF Empty(cWhere) .AND. !Empty(::cScope)
         cWhere:=::cScope
      ENDIF

      IF Empty(cWhere) .AND. !Empty(::cScopeF)
         cWhere:=::cScopeF
      ENDIF

      IF Empty(cWhere)
        cWhere:=" WHERE 1=1 "
      ENDIF

 // ? "AQUI OBTIENE GetWhereMax(",::cTable,cField,cWhere+" ORDER BY "+cField+IIF( lMax , " DESC ", "" )+" LIMIT 1"

      uValue:=SQLGET(::cTable,cField,cWhere+" ORDER BY "+cField+IIF( lMax , " DESC ", "" )+" LIMIT 1")

//? "CONCLUYO",oDp:cSql

      IF !Empty(uValue)

        // RETURN " WHERE "+cField+GetWhere( "=" , uValue )
        RETURN cField+GetWhere( "=" , uValue )

      ENDIF

      RETURN ""

   ENDIF

// ? "scope 4",::cTable,::cScope,"::cTable,::cScope"

   IF .T. // oDp:lDpXbase
     DPWRITE("TEMP\DPDOCENC"+::cTable+"_COUNT.SQL",::cScope)
   ENDIF

   nCount:=COUNT(::cTable,::cScope,::oDb) // Antes MYCOUNT

// ? nCount,"nCount",GETPROCE(),ErrorSys(.T.)

   IF nCount=0
      RETURN ""
   ENDIF


/*
   oTable:=OpenTable("SELECT COUNT(*) FROM "+::cTable + IIF( Empty(::cScope ), "" , " WHERE "+::cScope ) ,.T.)

   IF Empty(oTable:FieldGet(1)) // Tabla Vacia
      oTable:End()
      RETURN ""
   ENDIF

   oTable:End()
*/
   aFields:=_VECTOR(cField)

   IF !Empty(::cScope)


     IF " JOIN "$::cScope

// ? cWhere,"<-cWhere",::cScope,"<-::cScope",Empty(cWhere),"Esta vacio o no vacio"

        IF Empty(cWhere)
           cWhere:=::cScope
        ELSE
           cWhere:=::cScope + IF( Empty(cWhere),""," AND ")+cWhere
        ENDIF

// ? "AQUI ES NUEVO WHERE",cWhere,"no puede estar repetido",CLPCOPY(cWhere)

     ELSE

        cWhere:=cWhere + " " + IIF( Empty(cWhere) , " WHERE ", " AND " ) +::cScope

     ENDIF

//? cWhere,"GetWhereMax 2"

   ENDIF

   FOR I := 1 TO LEN(aFields)


     cSql:="SELECT "+IIF( lMax , "MAX", "MIN" )+"("+aFields[I]+") FROM " + ::cTable + cWhere

   // ? I,cWhere,"ANTES DEL OPENTABLE",CLPCOPY(cSql)


     oTable:=OpenTable("SELECT "+IIF( lMax , "MAX", "MIN" )+"("+aFields[I]+") FROM " + ::cTable + cWhere , .T. ,::oDb )

//     ? oTable:cWhere,aFields[i]

     uValue:=oTable:FieldGet(1)

     IF !Empty(uValue)

         cList :=aFields[I]+GetWhere( "=" , uValue )

  //       ? cList,::cScope,cList=::cScope

         IF cList<>::cScope

           cWhere:=cWhere+IIF( Empty(cWhere) , " WHERE " , " AND " ) + ;
                   cList

         ENDIF

     ELSE

         EXIT

     ENDIF

     oTable:End()

   NEXT I

   // 26/06/2024 , cWhere vacio, buscara el ultimo registro
/*
   cWhere:=ALLTRIM(cWhere)
   cWhere:=IF(cWhere=="WHERE","",cWhere)

   IF Empty(cWhere) .AND. !(","$cField)
      cSql  :="SELECT "+cField+" FROM "+::cTable+" ORDER BY "+cField+" DESC LIMIT 1"
      oTable:=OpenTable(cSql, .T. ,::oDb )
      uValue:=oTable:FieldGet(1)

      IF !Empty(uValue)
        cWhere:=" WHERE "+cField+GetWhere( "=" , uValue )
      ENDIF

      oTable:End()
   ENDIF
*/
 // ? "AQUI TERMINO OK",cWhere

RETURN cWhere

/*
// Solicita la Impresión del Documento
*/
// METHOD Imprimir() CLASS TDOCENC
//
// RETURN NIL

/*
// Busca el Valor Más Alto
*/
METHOD Get(cField) CLASS TDOCENC
  LOCAL I,aFields,uValue

  IF !","$cField
     RETURN __objSendMsg(Self,cField)
  ENDIF

  aFields:=_VECTOR(cField)
  uValue :=__objSendMsg(Self,aFields[1])

  FOR I := 2 TO LEN(aFields)
    uValue:=IIF( ValType(uValue)<>"C",CTOO(uValue,"C") , uValue )
    uValue:=uValue+CTOO (__objSendMsg(Self,aFields[I]),"C")
  NEXT I

RETURN uValue

/*
// Salto de Registro
// Busca desde el Ultimo Campo hacia Atras
*/
METHOD Skip(nSkip) CLASS TDOCENC
    // LOCAL aCampos:=_VECTOR(::cPrimary),I,oTable
    LOCAL aCampos:=_VECTOR(::cOrderBy),I,oTable,oTable2  // 13/09/2016
    LOCAL cWhere :="",cSql,uValue,lIgual:=.F.
//    LOCAL nFind  :=LEN(aCampos)
    LOCAL cFieldX:=""
//    LOCAL cFunc  :=IIF( nSkip<0 , "MAX" , "MIN" )
    LOCAL cOper  :=IIF( nSkip<0 , "<"   , ">"   )
    LOCAL nStart:=1,aEof:={}

    CursorWait()

    /*
    // Debe ser replanteado por Order By
    */

    cWhere:=""

    FOR I := 1 TO LEN(aCampos) // nFind-1

      cWhere :=cWhere +IF(Empty(cWhere ),"",",")+CTOSQL(::Get(aCampos[I]))
      cFieldX:=cFieldX+IF(Empty(cFieldX),"",",")+aCampos[I]

    NEXT

    cWhere:="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cWhere+")"

    IF !Empty(cWhere).AND. !Empty(::cScope)
       cWhere:=::cScope+" AND "+cWhere
    ENDIF

    cSql  :="SELECT "+::cOrderBy+" FROM "+::cTable+" WHERE "+cWhere+" ORDER BY CONCAT("+::cOrderBy+") "+IF(nSkip>0,"","DESC")+" LIMIT 1 "


    oTable:=OpenTable( cSql , .T. ,::oDb)

    IF oTable:RecCount()>0

       ::lEof:=.F.
       ::lBof:=.F.

       AEVAL(oTable:aFields,{|a,n|__objSendMsg(Self,a[1],oTable:FieldGet(n))})
       ::cWhere:=::GetWhere() // " WHERE "+cWhere
       ::LoadData(0)

       cWhere :=" "+::GetWhereMax(::cOrderBy , NIL , nSkip>0) // JN 14/04/16

       // Verificación Ultimo Registro

       IF Empty(cWhere)
          cWhere:=::cScope
//? "ASUME SCOPE "+cWhere,CLPCOPY(oDp:cSql)
       ENDIF

// ? ::cOrderBy,cWhere,::GetWhereMax(::cOrderBy , NIL , nSkip>0)

       IF !" WHERE "$cWhere
           cWhere:=" WHERE "+cWhere
       ENDIF

       // ? cWhere,"cWhere"

       // 26/06/2024 Caso de Alicuotas de IVA , eliminar registro generaba incidencia por falta cWhere
       cWhere:=ALLTRIM(cWhere)

       IF LEN(cWhere)=5 .OR. cWhere=="WHERE"

          cSql:="SELECT "+::cOrderBy+" FROM "+::cTable+" ORDER BY "+::cOrderBy+" DESC LIMIT 1"

       ELSE

          cSql:="SELECT "+::cOrderBy+" FROM "+::cTable+" "+cWhere

       ENDIF

       // ? cSql,"cSql",cWhere,"cWhere",LEN(cWhere)
       //  26/06/2024 cSql:="SELECT "+::cOrderBy+" FROM "+::cTable+" "+cWhere

       oTable2:=OpenTable( cSql , .T. ,::oDb)
       oTable2:End()

       lIgual:=.T.

       FOR I=1 TO LEN(oTable:aFields)

          IF lIgual .AND. !(oTable:FieldGet(I)==oTable2:FieldGet(I))
             lIgual:=.F.
          ENDIF

       NEXT I

       ::lEof:=(nSkip>0 .AND. lIgual)
       ::lBof:=(nSkip<0 .AND. lIgual)

       ::nRecno:=::nRecno+(1*nSkip)

    ELSE

       ::lEof:=nSkip>0
       ::lBof:=nSkip<0

    ENDIF

    ::RefreshBtn1()

    oTable:End()

RETURN .T.

/*
JN 15/09/2016

    WHILE nFind>=nStart

       FOR I := 1 TO nFind-1

         cWhere:=cWhere + IIF( Empty(cWhere) , "" , " AND " )+;
                 aCampos[I]+GetWhere("=",::Get(aCampos[I]))

       NEXT

       IF Len(aCampos)=1
         cWhere:=aCampos[I]+GetWhere("=",::Get(aCampos[I]))
       ENDIF

//       ? nFind,cWhere

       IF EMPTY(cWhere)
         // ? "NO HAY WHERE"
         EXIT
       ENDIF

       IF Len(aCampos)>1

         cWhere :=cWhere+" AND "+aCampos[nFind] + ;
                   IIF( nFind=1 ," " ,  GetWhere(cOper,::Get(aCampos[nFind])))
       ELSE

         cWhere :=aCampos[1]+GetWhere(cOper,::Get(aCampos[nFind]))

       ENDIF

//      ? cWhere
       IF !Empty(cWhere).AND.!Empty(::cScope)
         cWhere:=::cScope+" AND "+cWhere
       ENDIF

//     ? cWhere


       cSql  :="SELECT "+::cOrderBy+" FROM "+::cTable+" WHERE "+cWhere+" ORDER BY "+::cOrderBy+" "+IF(nSkip<1,"","DESC")+" LIMIT 1 "

? cSql,CLPCOPY(cSql)

       oTable:=OpenTable( cSql , .T. ,::oDb)
       AEVAL(oTable:aFields,{|a,n|__objSendMsg(Self,a[1],oTable:FieldGet(n))})

oTable:Browse()

? CLPCOPY(cSql)

       oTable:End()


       cSql  :="SELECT "+cFunc+"("+aCampos[nFind]+") FROM "+::cTable+" WHERE "+cWhere

       oTable:=OpenTable( cSql , .T. ,::oDb)
       uValue:=oTable:FieldGet(1)


? CLPCOPY(cSql),::cOrderBy,"cOrderBy",uValue


       IF !Empty(uValue)

          __objSendMsg(Self,aCampos[nFind],uValue)

  //        ? "SI ENCONTRADO",oTable:cSql,uValue,nFind

          cWhere:=""
          FOR I := 1 TO nFind
             cWhere := cWhere + IIF( Empty(cWhere) , " " , " AND " ) + aCampos[I]+GetWhere("=",::Get(aCampos[I]))
             // ? I,aCampos[I]
          NEXT

          IF !Empty(cFieldX)

            cSql:="SELECT "+cFunc+"("+cFieldX+") FROM "+::cTable+  " WHERE "+cWhere

    //       ? cSql,"BUSCA EL MAYOR"

           oTable:End()

           oTable  :=OpenTable(cSql,NIL,::oDb)
           uValue  :=oTable:FieldGet(1)
           __objSendMsg(Self,cFieldX,uValue)

           ::cWhere:=::GetWhere() // " WHERE "+cWhere
           ::LoadData(0)

      //     ? cWhere,"DEBE LOCALIZAR ESTE, LISTO ENCONTRADO"
           oTable:End()

           RETURN .F.

         ENDIF

       ELSE

          cFieldX:=aCampos[nFind]
          nFind--
          cWhere:=""

        //  ? "NO ENCONTRO",nFind,oTable:cSql

          LOOP

       ENDIF

       // ? cSql,uValue,::GetWhere(),oTable:RecCount()

       oTable:End()

       IF !Empty(uValue)
          ::cWhere:=::GetWhere()
          ::LoadData(0)
          EXIT
       ENDIF

    ENDDO
*/


RETURN .T.

/*
// Primer Registro
*/
METHOD Primero(lPrimero,cWhere) CLASS TDOCENC

   LOCAL oTable,cSql

   DEFAULT cWhere:=""

   CursorWait()

   // cWhere :=::GetWhereMax(::cPrimary , cWhere , lPrimero)
   cWhere :=::GetWhereMax(::cOrderBy , cWhere , lPrimero) // JN 14/04/16

//   ? cWhere

   IF !Empty(cWhere)
     ::cWhere:=cWhere // ::cPrimary+GetWhere("=",::uData)
     ::LoadData(0)
   ENDIF

   ::lEof:=lPrimero
   ::lBof:=!lPrimero

   ::RefreshBtn1() // JN 15/09/2016

RETURN .T.

/*
// Ejecutar Repetición de Campos desde Incluir
*/
METHOD RunRepeat(cField) CLASS TDOCENC
   LOCAL I,uValue,oControl

   FOR I := 1 TO LEN(::aFields)

      IF ::aFields[I,1]$::cRepeat
         LOOP
      ENDIF

      uValue:=CTOEMPTY(::Get(::aFields[I,1]))
      __objSendMsg(Self , ::aFields[I,1] , uValue)

      IF ::IsDef("O"+::aFields[I,1])
         oControl:=__objSendMsg(Self , "O"+::aFields[I,1])
         IIF( ValType(oControl)="O" , oControl:Refresh(.T.) , NIL )
      ENDIF

   NEXT

   EJECUTAR("DPEDITSETDEF",SELF)

RETURN .T.

/*
// Cancelar Transacción
*/
METHOD Repeat(cField) CLASS TDOCENC

    ::cRepeat+=IIF( Empty(::cRepeat) , "" , "," ) + cField // Campos que se Repiten

RETURN ::cRepeat


/*
// Cancelar Transacción
*/
METHOD Cancel(lAsk)
   LOCAL oTable,I,nCount

   DEFAULT lAsk:=.T. // Debe Preguntar

   ::lCancelClic:=.T.

//?  "AQUI CANCEL"

   IF ::nOption=5
     ::CancelFind()
   ENDIF

   IF !Empty(::cCancel) .AND. ::IsFunction(::cCancel)

      IF !::RunScript(::cCancel,lAsk)
         RETURN .F.
      ENDIF

   ENDIF

   // Cancela la Edición
   // ::nOption:=0
   Aeval(::aGrids,{|oGrid|oGrid:CancelEdit(.f.)})

   // IIF( ValType(::oEditMemo)="O" , ::oEditMemo:Close() , NIL )
   //    ? "CANCEALR",::oEditMemo:ClassName()

   nCount:=COUNT(::cTable,::cWhere,::oDb)

   IF nCount=0
      ::nOption:=0 // lAutoInc:=.F.
      ::Close()
      RETURN .F.
   ENDIF

   IF ::nOption=3 .AND. ::lAutoEdit
      ::nOption:=0
      ::Close()
      RETURN .T.
   ENDIF

   ::nOption=0
   // Aeval(::aGrids,{|oGrid|oGrid:nOption:=0,oGrid:AdjustBtn()})
   // ? "aqui que pasa???"

   ::LoadData(0)
   // ::nOption:=0
   Aeval(::aGrids,{|oGrid|oGrid:nOption:=0,oGrid:AdjustBtn(.F.)})
   ::BtnPaint()
   ::oDlg:Refresh()

// ? ::cWhere,"si esta vacio va pa afuera"

RETURN .T.


/*
// Perdia de Focus
*/
METHOD LostFocus()

/*
   IF GetControl()!=NIL
     ::oControl:=GetControl()
   ENDIF
*/

   IF !VP("oControl")=NIL // JN
      ::oControl:=VP("oControl")
   ENDIF

   IF !oDp:oControl=NIL
      ::oControl:=oDp:oControl
      // oDp:oFrameDp:SetText(::oControl:ClassName())
   ENDIF

   // 07/11/2016
   IF ::lActivated  .AND. !:: lDesign
     AEVAL(::aGrids,{|o,n| o:DOCLOSTFOCUS()})
     EVAL(::bLostFocus,Self)
   ENDIF

RETURN NIL

/*
// Emite el Mensaje
*/
METHOD Mensaje(cMensaje,cTitle) CLASS TDOCENC
    MensajeErr(cMensaje,cTitle)
RETURN .T.

/*
// Carga los Datos hacia las Variables
*/
METHOD BtnPaint()  CLASS TDOCENC
    LOCAL I,nStep:=3,nWidth,nHeight,aHide:={},aShow:={},nLin:=24

    IF !::lBar
      RETURN .T.
    ENDIF

    nWidth :=::aBtn[1,1]:nWidth
    nHeight:=::aBtn[1,1]:nHeight

    FOR I := 1 TO LEN(::aBtn)

      ::aBtn[I,1]:bWhen :=RUNBTN(Self,BloqueCod(::aBtn[I,6])) // AQUI

      IF !EVAL(::aBtn[I,5]) // ::aBtn[I,1]:bWhen)

         AADD(aHide,I)

      ELSE

        // ::aBtn[I,1]:Enable()
         ::aBtn[I,1]:Move(3,nStep)
         ::aBtn[I,1]:SetSize(nWidth,nHeight)
         AADD(aShow,I)
         nStep:=nStep+(nWidth)+1

//         IF ValType(::aBtns[I,6])="B" .AND. !EVAL(::aBtns[I,6]) // Inactiva
//           ::aBtns[I,1]:Disable()
//         ENDIF

      ENDIF

   NEXT

  // aShow[1]:ForWhen()

   AEVAL(aHide,{|n|::aBtn[n,1]:Hide()})
   AEVAL(aShow,{|n|::aBtn[n,1]:Show(),nLin:=nLin+::aBtn[n,1]:nWidth()})

   // Aqui Replantea la Condición en la API  JN: 04/5/09
   ::aBtn[1,1]:ForWhen(.t.)

// AEVAL(::aBtn,{|a,n| a[1]:ForWhen(.T.)})
// IF ::nOption=0
//    ::lCancel:=.T. // Puede Salir
// ENDIF

   // IIF(lMdi,DpFocus(::oDlg),NIL)
   IF !EMPTY(::oDlg:aControls)
     ::oDlg:aControls[1]:ForWhen()
   ENDIF

   // Reposiciona la Posición de Registros
   // 19/09/2016 no es necesario reubiscarse
   IF ValType(::oSayRecord)="O" .AND. (::nOption=0) .AND. nLin>10
      ::oSayRecord:Move(0,nLin)
      ::oSayRecord:Show()
   ENDIF

   IF ValType(::oSayRecord)="O"
       ::oSayRecord:Refresh(.t.)
   ENDIF

RETURN .T.

/*
// Refresca los Botones de Navagación

*/
METHOD RefreshBtn1(aBtn)
   LOCAL I,nAt

   ::nRecno:=IF(::lEof,::nRecCount,::nRecno)
   ::nRecno:=IF(::lBof,1          ,::nRecno)

   ::oSayRecord:Refresh(.T.)

   DEFAULT aBtn:={"BTN_INI","BTN_SIG","BTN_ANT","BTN_FIN"}

   FOR I=1 TO LEN(aBtn)

     nAt:=ASCAN(::aBtn,{|a,n| a[7]=aBtn[I]})

     IF nAt>0
        ::aBtn[nAt,1]:ForWhen(.t.)
        ::aBtn[nAt,1]:Refresh(.t.)
     ENDIF

   NEXT

// AEVAL(::aBtn,{|a,n| a[1]:ForWhen(.T.)})
// ? "AQUI"
// ::oBar:Refresh(.T.)

RETURN NIL


/*
// Carga los Datos hacia las Variables
*/
METHOD RunScript(cFunction,uPar1,uPar2)  CLASS TDOCENC
   LOCAL lResp:=.t.

  ::Prepare()

  IF !::oScript:IsFunction(cFunction)

     ::Mensaje("Función : "+cFunction+" No Existe ")

  ELSE

     ::oScript:cError:=""

     SETDBSERVER(::oDb)

     lResp:=::oScript:Run(cFunction,Self,uPar1,uPar2)

     SETDBSERVER()

     // ? ::oScript:cError,"Error",oDp:cMsgError

     IF !Empty(::oScript:cError)
         MensajeErr(::oScript:cError,::cScript+ "FUNCTION "+cFunction)
     ENDIF

  ENDIF

RETURN lResp

/*
// Carga los Datos hacia las Variables
*/
METHOD LoadData(nOption , nNext , lStart , cWhere)  CLASS TDOCENC

     LOCAL oTable,oControl,I,cTexto:="",uValue,oGet
     LOCAL nOldOpc:=::nOption

     DEFAULT lStart:=.F.

     ::aDataTable:={}

     ::lCancelClic:=.F.

     IF ::nOption<>1
       ::lAutoInc:=.F.
     ENDIF

     IF nOption=2
        IIF( !Empty(::cView) ,  ::RunScript(::cView)  ,NIL )
        RETURN .F.
     ENDIF

     ::nOption:=nOption

     IF Empty(::cWhere) .OR. nOption=3
       ::cWhere :=::GetWhere()
     ENDIF

     IF ::nOption=1
        ::RunRepeat()           // Repetir Campos
        ::BuildIncremental(.F.) // Calcula los Incremental
        ::lSaved      :=.F.
     ELSE
        ::lSaved      :=.T.    // Cuando Modifica ya esta Grabado
     ENDIF

     IF !EMPTY(::aMemo) // Campo Memo
         ::aMemo[7]:=IIF( ::nOption=1 , 0 , ::Get(::aMemo[1]))
         ::aMemo[8]:=""
         ::aMemo[9]:=""
     ENDIF

     IF !Empty(::cFieldFile)
       ::nFileMain :=::Get(::cFieldFile)
     ENDIF

     IF ::nOption=1
      ::nFileMain :=0
     ENDIF

     IIF( ValType(::oTable)="O" , ::oTable:End() , ::oTable:=NIL )

     IF !::nOption=1

       cWhere:=" "+IIF(Empty(cWhere),::cWhere,cWhere)

       IF !ALLTRIM(UPPE(::cScope))$ALLTRIM(UPPE(cWhere)) .AND. !Empty(::cScope)
         cWhere:=" "+::cScope + IIF( Empty(cWhere)," "," AND "+cWhere)
       ENDIF


       IF " JOIN "$cWhere

// ? "AQUI TIENE WHERE ",cWhere,"cWhere",::cScope,"cScope"

       ELSE

         cWhere:=STRTRAN(cWhere," WHERE ","")

       ENDIF

       cWhere:=" "+cWhere+" "

       cWhere:= IIF( Empty(cWhere) , ::cWhere , cWhere )
       cWhere:= IIF(" WHERE "$cWhere, "", " WHERE " ) + cWhere

       ::oTable:=OpenTable(::cSql + cWhere   ,.T. , ::oDb)

       DPWRITE("TEMP\"+::cTable+"_LOADDATA.SQL",oDp:cSql)

       ::aDataTable:=ACLONE(::oTable:aDataFill)

       FOR I := 1 TO LEN(::aFields)

           uValue:=::oTable:FieldGet(I)

           IF ::oTable:aFields[I,2]="C"
              uValue:=PADR(uValue,::oTable:aFields[I,3])
           ENDIF

           __objSendMsg(Self , ::aFields[I,1]     , uValue) // ::oTable:FieldGet(I))
           __objSendMsg(Self , ALLTRIM(::aFields[I,1])+"_" , uValue) // Copia del Registro

           IF ::IsDef("O"+::aFields[I,1])

             oControl:=__objSendMsg(Self , "O"+::aFields[I,1])

             // ? oControl:ClassName(), ::aFields[I,1], ::oTable:FieldGet(I)
             // oGet:= __objSendMsg(Self , ::aFields[I,1] )
             // ? oControl,oControL:ClassName(),uValue,ValType(uValue)

             IF ValType(uValue)="D" .AND. ValType(oControl)="O"
               oControl:oGet:buffer:=DTOC(uValue)
               oControl:DispText()
             ENDIF

             IIF( ValType(oControl)="O" , oControl:Refresh(.T.) , NIL )

           ENDIF

       NEXT

       // oTable:End()

     ELSE

       ::oTable:=OpenTable(::cSql,.F.,::oDb)
      // EJECUTAR("DPTABLESETDEF",::oTable)

     ENDIF

     // ? ::nOption,"LOADDATA" //,::cLoad
     // QUITAR POR AHORA
     IF ::lAutoEdit
        ::nOption:=3
     ENDIF

// Aeval(::aGrids,{|oGrid|oGrid:AdjustBtn()})
// Posicion Original
// Aeval(::aGrids,{|oGrid|oGrid:NewJob(::nOption,.T.)})
// Aeval(::aScrollGets,{|o|o:UpdateFromForm()})

     IF !IIF(!Empty(::cLoad) , ::RunScript(::cLoad) , .T.  ) // Ejecuta Carga de Datos
        // ::Cancel(.F.)
        ::nOption:=nOldOpc
        RETURN .F.
     ENDIF

     IF ::nOption<>0 // Debe Apagar los Botones

        ::BtnPaint()

        I:=ASCAN(::oDlg:aControls,{|o,n|o:ClassName()="TFOLDER"})

        IF I>0

          AEVAL(::oDlg:aControls[I]:aDialogs,{|a,n|IIF( Len(a:aControls)>0 , ( a:Refresh(.T.) , ;
                                                    IF( "XBROW" $ a:aControls[1]:ClassName() , NIL , a:aControls[1]:ForWhen()) ;
                                                      ) , NIL )})
          // AEVAL( ::oDlg:aControls[I]:aDialogs , { |a,n|;
          //   IIF( Len(a:aControls) >0 , ( a:Refresh(.T.) , a:aControls[1]:ForWhen() ),;
          //        ,NIL })
        ENDIF

     ENDIF

     Aeval(::aGrids,{|oGrid|oGrid:NewJob(::nOption,.T.)})
     Aeval(::aScrollGets,{|o|o:UpdateFromForm()})

     // JN 13/07/2015

     Aeval(::aScrollGets,{|o|o:SetEdit(::nOption=1 .OR. ::nOption=3,::nOption)})

     I:=ASCAN(::oDlg:aControls,{|o,n|o:ClassName()="TFOLDER"})

     IF ::nOption=0 .AND. I>0
       AEVAL(::oDlg:aControls[I]:aDialogs,{|a,n|IIF( Len(a:aControls)>0,a:aControls[1]:ForWhen(),NIL)})
     ENDIF

     ::cWhereOpen:=::GetWhere(NIL,.T.)

      IF !IIF(!Empty(::cAfterLoad) , ::RunScript(::cLoad) , .T.  ) // Ejecuta Carga de Datos
        // ::Cancel(.F.)
        ::nOption:=nOldOpc
        RETURN .F.
     ENDIF


     IF ::lOkJn
        RETURN .F.
     ENDIF

//   Aeval(::oDlg:aControls,{|a,n| IIF( a:ClassName()$"TFOLDER", (a:aDialogs[1]:ForWhen(),MensajeErr("folder")),nil)})

     IF ValType(::oFocus)="O"
        ::oFocus:ForWhen()
        DpFocus(::oFocus)
     ELSEIF !Empty(::oDlg:aControls)
        ::oDlg:aControls[1]:ForWhen()
     ENDIF

/*
     //IF ::uData=NIL // Busca el Mayor Registro
     //   ::uData:=SqlGetWhereMax(::cTable,::cPrimary)
     // ENDIF

     IF Empty(::uData) // No Existe Nada
        ::nOption:=1
        RETURN .F.
     ENDIF

     cWhere:=::cPrimary+GetWhere("=",::uData)

     oTable:=OpenTable("SELECT * FROM "+::cTable+" WHERE "+cWhere,.T.)

     AEVAL(oTable:aFields,{|a,n|__objAddData( Self, a[1] ) , __objSendMsg(Self,a[1],oTable:FieldGet(n))})

     //? ::uData,"Encontrada",oTable:RecCount(),oTable:cSql

     oTable:End()
*/
//      ::RunScript("Load")
     DO CASE

       CASE ::nOption=1
          cTexto:=::cIncluir
          ::FindGet() // Busca el Primer Get
       CASE  ::nOption=2
          cTexto:=::cConsultar
       CASE ::nOption=3
          cTexto:=::cModificar
          ::FindGet() // Busca el Primer Get
       CASE ::nOption=4
          cTexto:=::cEliminar
       CASE ::nOption=5
          cTexto:=::cBuscar

    ENDCASE

// ? "aqui uno"

    cTexto:=IIF( !Empty(cTexto) ,"["+ALLTRIM(cTexto)+"]" , cTexto )

    ::oWnd:SetText(ALLTRIM(::cTitle) + " "+cTexto)

    oControl:=NIL

    IIF(ValType(::oTable)="O", ::oTable:End()  , NIL)
    ::oTable:=NIL

// ? "aqui debe revisar el incrementador"

RETURN .T.

// Revisa Valores Faltantes para la Integridad Ref
METHOD ChkIntRef(cField) CLASS TDOCENC
    LOCAL aData:=GetFieldIntRef(::cTable),i,aResp:={}

    // ? ::cTable

    FOR I := 1 TO LEN(aData)
      IF (!","$aData[I,2]) .AND. Empty(::Get(aData[I,2]))
         AADD(aResp,aData[I,1])
      ENDIF
    NEXT

RETURN aResp

/*
// Revisa las Condiciones de Grabar
*/
METHOD PreSave(lSave) CLASS TDOCENC
   LOCAL lResp:=.T.,aIntRef:={}
   LOCAL I,cVar,oGet,uValue,cKeyAudita

   LOCAL cWhere:=::GetWhere()
   LOCAL cKey  :=::cPrimary
   LOCAL uKey  :=::GetValue(cKey)

   DEFAULT lSave:=.F. // Grabar Automático desde Boton Grabar

// ? ::oTable:ClassName()
// ::oTable:Browse()
// ViewArray(::oTable:aBuffers)

   IF !::ValUnique(NIL,::cPrimary)

      DpFocus(oDp:oGetFocus) // Ultimo Get Activo

      RETURN .F.

   ENDIF

   IF !Empty(::cPreSave) .AND.  !::RunScript(::cPreSave,lSave)
      Return .F.
   ENDIF

   IF !Empty(::cFieldFile)
     ::Set(::cFieldFile,::nFileMain )
   ENDIF

   IF (aIntRef:=::ChkIntRef(),!EMPTY(aIntRef))
      EJECUTAR("DPMSGINTREF",aIntRef,SELF)
      RETURN .F.
   ENDIF

   IF lSave

      IF !::Save()

         DpFocus(oDp:oGetFocus) // Ultimo Get Activo
         Return .F.

      ELSEIF !Empty(::cPostSave)

         ::Prepare()
         ::RunScript(::cPostSave,lSave)

      ENDIF

// ? cKey,::cPrimary,"cKey,::cPrimary",GETPROCE(),uKey
// ViewArray(::aFields)
      oDp:cKeyEncrip:=""
      cKeyAudita    :=""

      IF ::cTable="DPDOCCLI" .AND. ::Get("DOC_DOCORG")="V"
        cKeyAudita:=REGFISCAL(::CTOMEMO(uKey,.T.)) // crear texto del registro
      ENDIF

      // ? cKeyAudita,"cKeyAudita"

      AUDITAR( IF(::nOption=1,"DINC","DMOD") ,!::lDsnData ,::cTable , uKey , NIL, SELF ,NIL,NIL,cKey,NIL,cKeyAudita)

      // Incluye productos , movimiento de Productos
      IF !Empty(cKeyAudita) // ::cTable="DPDOCCLI" .AND. ::DOC_DOCORG="V" // ingresó por DPFACTURAV
         VALIDARDOCFISCAL(NIL,NIL,NIL,NIL,NIL,.F.,uKey) // solo Cuerpo de factura
      ENDIF

      IF ::nOption=1
         ::lOkJn=.T.
         ::LoadData(::nOption) // Sigue Incluyendo
         Memory(-1)
      ELSE
// JN 06/12/2014, Al Modificar factura pregunta si continuo o no         ::Cancel(.T.)
      ::Cancel(.F.)
      ENDIF
   ENDIF

   IF lResp
     ::lAutoInc:=.F.
   ENDIF

RETURN lResp

/*
// Valida Existencia
*/
METHOD ValUnique(uValue,cPrimary,lMsg,cMsg) CLASS TDOCENC
   LOCAL lResp:=.T.,oTable,cWhere,nCount:=0,xWhere1,xWhere2,nCant:=0

   DEFAULT cPrimary:=::cPrimary,;
           uValue  :=::Get(cPrimary),;
           lMsg    :=.T.,;
           cMsg    :="Registro ya Existe"

   IF !::lValUnique
      RETURN .T.
   ENDIF

   //? ::oTable:cWhere

   WHILE .T.

        cWhere:=::GetWhere(NIL,.T.) // Actual

        // cWhere:=IIF( " WHERE "$cWhere .OR. Empty(cWhere) , " " , " WHERE ")+cWhere+;
        //        IIF( Empty(::cScope), "" , " AND "+::cScope)
        // ? ::cScope,"SCOPE",::cWhereOpen
        // ? ::cTable,cWhere
        // oTable:=OpenTable("SELECT "+cPrimary+" FROM "+::cTable + cWhere , .T. )

        nCount:=COUNT(::cTable,cWhere,::oDb)
        DPWRITE("TEMP\"+::cTable+"_INCREMENTAL.SQL",oDp:cSql)

        // ? ::cTable,cWhere,nCount," BUSCA EL INCREMENTADOR"
        // ? ::cTable,cWhere,::nOption
        //  ? nCount,"cWhere "+cWhere,CRLF," ::cWhereOpen    "+::cWhereOpen , CLPCOPY(oDp:cSql)
        // IF ::nOption=1 .AND. oTable:RecCount()>0 .AND. !::lSaved

         IF ::nOption=1 .AND. nCount>0 .AND. !::lSaved

            IF !Empty(::aIncremental)

               // ? "::aIncremental"

               ::BuildIncremental(.F.) // Calcula los Incremental

               IF (nCant++)>10

                  IF EJECUTAR("TDOCENCVALUNIQUE",SELF)

                     RETURN .T. // Quitar
                  // IF MsgNoYes("Desea Continuar con el Incrementador")
                     EXIT
                  ELSE
                     nCant:=0
                  ENDIF
               ENDIF

               LOOP

            ELSE

               lResp:=.F.

            ENDIF

       ENDIF

       IF ::nOption=3 .AND. nCount>0

         xWhere1:=ALLTRIM(::cWhereOpen) // STRTRAN(cWhere  ," WHERE ",""))
         xWhere2:=ALLTRIM(cWhere      ) // STRTRAN(::cWhere," WHERE ",""))

         // ? xWhere1,xWhere2,cPrimary,"VALUNIQUE" ,xWhere1=xWhere2

         lResp  :=(xWhere1=xWhere2)

         // ? cWhere,CRLF, ::cWhereOpen,lResp  // ,::cWhere==::cWhereOpen,"ESTOS SE COMPARAN"
         // ? xWhere1,xWhere2,"AQUI SE COMPARAN",lResp,cPrimary

       ENDIF

       EXIT

   ENDDO

   IF ::nOption<>1
//      ? "ValUnique",::nOption
   ENDIF

// oTable:End()

   IF !lResp .AND. lMsg
      ::Mensaje(cMsg) // "Registro ya Existe") //  ,GetProce())
   ENDIF

RETURN lResp

/*
// Recuperar Focus
*/
METHOD GotFocus(lAuto) CLASS TDOCENC

   DEFAULT lAuto:=.F.

   ::Prepare()

   IF !lAuto
      ::oWnd:GotFocus()
   ENDIF

   oDp:cHelpRtf:=::cFileRtf

   IF ValType(::oControl)="O"
        DpFocus(::oControl)
   ENDIF

   IF !oDp:lMenuXp
      ::oWnd:Move(::nTop,::nLeft,::nWidth,::nHeight,.T.)
      EVAL(::oWnd:bResized)
      ::oDlg:Refresh(.T.)
   ENDIF

   // 07/11/2016
   IF ::lActivated
     AEVAL(::aGrids,{|o,n| o:DOCGOTFOCUS()})
     EVAL(::bGotFocus,Self)
   ENDIF

RETURN NIL

/*
// Mostrar Textos
*/
METHOD GridEdit( cTable , cLinkHead , cLinkGrid ,     cSql , cScope , cOrderBy , cGroupBy ) CLASS TDOCENC

   DEFAULT cLinkHead:=::cPrimary

   AADD(::aGrids,TDOCGRID():New(Self,cTable , cLinkHead , cLinkGrid , cSql , cScope , cOrderBy, cGroupBy ))

RETURN ATAIL(::aGrids)

/*
// Construye el Where del Registro
*/
METHOD GetWhere(cFields,lPrimary) CLASS TDOCENC
   LOCAL cWhere:="",I,aVar

   DEFAULT cFields:=::cPrimary  ,lPrimary:=.F.

   aVar  :=_VECTOR(STRTRAN(cFields,"+",","))
// cWhere:=IIF( lPrimary , "" , " "+::cWhereRecord )
// ? cWhere,"En GETWHERE, "
   cWhere:=""

   FOR I := 1 TO LEN(aVar)

//   ? "GETWHERE",aVar[I],I

     cWhere:=cWhere+;
             IIF( Empty(cWhere) , "" , " AND " )+;
             aVar[I]+GetWhere("=",::Get(aVar[I]))

   NEXT I

   cWhere:= cWhere +;
            IF( !Empty(::cScopeF)," AND "+::cScopeF , "")

   // ? cWhere,"cWhere en Method GetWhere "

RETURN cWhere // " WHERE "+cWhere

// +;
//       IF( !Empty(::cScopeF)," AND "+::cScopeF , "")

/*
// Grabar Encabezado
*/
METHOD Save()
   LOCAL oTable,I,lAppend:=.F.,lResp:=.F.,cWhere,uValue ,oTable_

   // Solo Cuando Modificado
   // ? "AQUI ES SAVE"

   IF ::nOption=3 .AND. ::lAutoEdit
      // ? "NO NECESITA GRABAR"
      ::Close()
      RETURN .T.
   ENDIF

   IF !EMPTY(::aMemo)

       // ? ::aMemo[7],::aMemo[8],::aMemo[9],"::aMEMO"

       ::aMemo[7]:=DPMEMOSAVE(::aMemo[7],::aMemo[8],::aMemo[9])
       ::Set(::aMemo[1],::aMemo[7]) // Asigna el Valor

       IF ValType(::oEditMemo)="O"   // Edición de Campo Memo
         ::oEditMemo:oWnd:End()
         ::oEditMemo:=NIL
       ENDIF

   ENDIF

   oTable:=OpenTable("SELECT * FROM "+::cTable,.F.,::oDb)
   ::oTable:=oTable
   oTable:lAppend:=.F.

   // 29/12/2025
   oTable:aFieldsC    :=::aFieldsC
   oTable:cFieldChkSum:=::cFieldChkSum

   IF ::lCreaRegIntRef
      oTable:lCreaRegIntRef:=.F. // Ya lo ejecutó
   ENDIF


// oTable:Browse()
// ViewArray(oTable:aBuffer)

   DEFAULT ::cWhere:="",;
           ::cScope:=""

// ? ::cWhere,::cScope,"::cWhere,::cScope"

   cWhere:=::cWhere+;
           IIF( Empty(::cScope) .OR. ::cScope$::cWhere , "" , " AND "+::cScope)

   IF ::nOption=1 .AND. !::lSaved

      // ? GetProce()

      IF ::lLock
        ::LockTable()
      ENDIF

      cWhere:=::GetWhere()

// ? cWhere," si tiene WHERE NO LO PUEDE AGREGAR"

      IF " JOIN "$::cScope

         cWhere:=::cScope+" AND "+cWhere

//      IIF( Empty(::cScope) .OR. ::cScope$cWhere , "" , " AND "+::cScope)


      ELSE

         cWhere:=cWhere+;
                  IIF( Empty(::cScope) .OR. ::cScope$cWhere , "" , " AND "+::cScope)

      ENDIF

//    ? cWhere,"BUSCAR",COUNT(::cTable,cWhere),getproce()

      IF COUNT(::cTable,cWhere,::oDb)>0
         ::lSaved:=.T.
         // ? "YA EXISTE",cWhere
         RETURN .T.
      ENDIF

      // ? "GRABAR UNO NUEVO "

      oTable:Append()
      lAppend:=.T.

   ENDIF

   FOR I:= 1 TO LEN(oTable:aFields)

       // 31/12/2016, Evita Update en Campos Innecesarios
       uValue:=::Get(oTable:aFields[I,1])

       IF ::oTable:aFields[I,2]="C"

          IF ValType(uValue)<>::oTable:aFields[I,2]
              MensajeErr("Campo "+::oTable:aFields[I,1]+" de Tipo de Dato "+::oTable:aFields[I,2]+" contiene "+ValType(uValue)+" Dato "+CTOO(uValue))
              uValue:=CTOO(uValue,"C") // Asi Evita Paralización
          ENDIF

          uValue:=LEFT(uValue,::oTable:aFields[I,3])

       ENDIF

      oTable:Replace(oTable:aFields[I,1],uValue) // ::Get(oTable:aFields[I,1]))

   NEXT

   IF lAppend

      IF ::oScript:IsFunction(::cCommit)
         ::RunScript(::cCommit,oTable)
      ENDIF

      lResp:=oTable:Commit() // Nuevo

   ELSE

      DEFAULT ::cScope_Update:=::cScope  // Se coloca luego del Where del Registro

      cWhere:=::cWhere+;
               IIF( Empty(::cScope) .OR. ::cScope$::cWhere , "" , " AND "+::cScope_Update)

// ? cWhere,"Optimizado"

      oTable:lAppend:=.F.

/*
      // JN 31/12/2016 (Necesario para Optimizar la UPDATE
*/

// ViewArray(oTable:aBuffers)
// ViewArray(oTable:aFields)

       //IF !Emtpy(oTable:aBuffers) .AND. LEN(oTable:aBuffers[1])=4 // Necesita la 5ta Columna del valor Anterior
       // AEVAL(oTable:aBuffers,{|a,n| IF(LEN(a)=4,AADD(oTable:aBuffers[n],NIL),NIL),;
       // oTable:aBuffers[n,5]:=__objSendMsg(SELF,a[1]+"_")})
//ENDIF

       AEVAL(oTable:aBuffers,{|a,n| oTable:aBuffers[n,5]:=__objSendMsg(SELF,a[2]+"_")})

       // Campos Caracter (No memos ni Blob), 30/12/2016 , Solo asigna los campos Caracter segun su longitud

// ? ::oScript:IsFunction(::cCommit),::cCommit

      IF ::oScript:IsFunction(::cCommit)
        ::RunScript(::cCommit,oTable)
      ENDIF

      // 27/02/2024
      IF !Empty(cWhere)
         cWhere:=cWhere+" LIMIT 1"
      ENDIF

      lResp:=oTable:Commit(cWhere) // Actualizar

// ? cWhere,"cWhere",oDp:cSql,::nOption,"nOption",lAppend,"lAppend",GETPROCE()

   ENDIF

   oTable:End()

   IF lResp // Fué Efectiva la Grabación
     ::cWhere:=::GetWhere()
     ::lSaved:=.T.

     IF ::nOption=1
        ::nRecCount++
        ::nRecNo++

        IF ValType(::oSayRecord)="O"
           ::oSayRecord:Refresh(.t.)
        ENDIF

     ENDIF
   ENDIF

   IF ::lLock
     ::UnLockTable()
   ENDIF


   // ? "AQUI GRABA"

   // ? "AQUI DEBE GRABAR LA CABEZA ",::cTable,::cPrimary,::cWhere

RETURN lResp

/*
// Mostrar Textos
*/
METHOD SetScript(cScript)

   IF cScript!=NIL
     ::oScript:=NIL
     ::cScript:=cScript
   ENDIF

   IF EMPTY(::cScript)
      ::oScript:=GetScript()
      ::cScript:=VP("SCRPROGRAM")
   ENDIF

   IF ValType(::oScript)!="O" .OR. Empty(::oScript:aFunctions)
     ::oScript:=GETRUNSCRIP(::cScript)
     // ::oScript:=GetScript()
   ENDIF

RETURN ::oScript
/*
// Mostrar Textos
*/
METHOD Say( nRow, nCol, cText, nClrFore, nClrBack, oFont, lPixel,;
            lTransparent, nAlign ) CLASS TDOCENC

   DEFAULT nClrFore := ::oDlg:nClrText,;
           nClrBack := ::oDlg:nClrPane,;
           oFont    := ::oDlg:oFont,;
           lPixel   := .f.,;
           lTransparent := .f.

   if ValType( nClrFore ) == "C"      //  xBase Color string
      nClrBack = nClrFore
      nClrFore = nGetForeRGB( nClrFore )
      nClrBack = nGetBackRGB( nClrBack )
   endif

   IF oFont=nil
      DEFINE FONT oFont NAME GetSysFont() SIZE 0, -8
   ENDIF

   ::oDlg:GetDC()

   DEFAULT nAlign := GetTextAlign( ::oDlg:hDC )

   WSay( ::oDlg:hWnd, ::oDlg:hDC, nRow, nCol, cValToChar( cText ), nClrFore, nClrBack,;
         If( oFont != nil, oFont:hFont, 0 ), lPixel, lTransparent, nAlign )

   ::oDlg:ReleaseDC()

RETURN NIL

/*
// Busca la Tabla y Muestra el Nombre
*/
METHOD SeekTable(cTable,oGet,cField,uValue,cDescri,oSay,cWhere) CLASS TDOCENC
  LOCAL cData,cSql,lFound:=.T.,oTable

  DEFAULT oGet   :=GetControl()
  DEFAULT uValue :=oGet:VarGet()
  DEFAULT cDescri:=cField

  cData:=SQLGET(cTable,cDescri,cField+GetWhere("=",uValue)+;
         IIF( Empty(cWhere) , "" , " AND "+cWhere ))

//  cSql:="SELECT "+cDescri+" FROM "+cTable+" WHERE "+cField+GetWhere("=",uValue)+;
//        IIF( Empty(cWhere) , "" , " AND "+cWhere )

  //? cSql
  // oTable:=OpenTable(cSql,.T.)
  // lFound:=oTable:Recno()>0

  IF (Empty(cData).AND.Empty(uValue)) .OR. (Empty(cData) .AND. !Empty(uValue))
     lFound:=.F.
  ENDIF

  IF ValType(oSay)="O"
     oSay:SetText(cData) // oTable:FieldGet(1))
  ENDIF

  // oTable:End()

//? cSql,oGet:ClassName()

  IF !lFound .AND. "BMP"$oGet:ClassName() // Dispara el Control
     oGet:KeyBoard(VK_F6) // ETHOD KeyDown( nKey, nFlags ) CLASS TBmpGet
     //  EVAL(oGet:bAction)
  ENDIF

RETURN lFound

/*
// Automaticamente Asigna los Campos
*/
METHOD SetTable(cTable , cPrimary , cWhere , lAll, cInner,oDb,cOrderBy,lDesc) CLASS TDOCENC
   LOCAL oTable,cSql
   //,I,cField

   DEFAULT lAll :=.F.,;
           lDesc:=.T.

   ::cTable      :=UPPE(ALLTRIM(cTable))
   ::cPrimary    :=cPrimary
   ::nOption     :=1
   ::cOrderBy    :=cOrderBy

   DEFAULT ::cOrderBy    :=cPrimary // Modo en que presentaran los Documentos

// ? "AQUI ES SETTABLE",GETPROCE()

/*
// JN 13/12/2013
   DEFAULT cWhere  :=::GetWhereMax(::cPrimary),;
           cInner  :=""

   cWhere:=" "+cWhere

? cWhere,"cWhere",::cScope

   IF !Empty(::cScope) .AND. !(ALLTRIM(::cScope)$ALLTRIM(cWhere))
     cWhere:=cWhere +  IIF(Empty(cWhere) , "" , " AND ") + ::cScope
   ENDIF

   IF !Empty(cWhere) .AND. !(" WHERE "$cWhere)
      cWhere:=" WHERE "+cWhere
   ENDIF
*/

// DEFAULT cWhere  :=::GetWhereMax(::cPrimary),;

// ? "SE VA PARA GETWHEREMAX(,1",GETPROCE()

   DEFAULT cWhere  :=::GetWhereMax(::cOrderBy),;
           cInner  :="",;
           oDb     :=oDp:oDbServer // Servidor Remoto

 //? cWhere,::GetWhereMax(::cOrderBy),::cOrderBy,"<-::cOrderBy",ErrorSys(.T.),"1"

  ::oDb:=oDb

   cWhere:=" "+cWhere+" "

// ? "2",ErrorSys(.T.),"GETPROCE()"

   IF " JOIN "$cWhere

//    cWhere:=STRTRAN(cWhere," WHERE "," AND ")
// ? cWhere," REEMPLAZA EL WHERE POR AND "

   ELSE

     cWhere:=STRTRAN(cWhere," WHERE ","")

   ENDIF

 // ? "3",cWhere,"cWhere SIN WHERE, no se le puede quitar con inner join ",::cScope

   IF !Empty(::cScope) .AND. !(ALLTRIM(::cScope)$ALLTRIM(cWhere)) .AND. !Empty(cWhere)


     IF " JOIN "$::cScope

//       ? cWhere,"<-cWhere",::cScope,"::cScope, GENERA INCIDENCIAS"

     ELSE

       cWhere:=::cScope +  IIF(Empty(::cScope) , "" , " AND ") + cWhere

     ENDIF

   ENDIF

   // JN 15/05/2017, asumia completo el Where
   IF Empty(cWhere) .AND. !Empty(::cScope)
      cWhere:=::cScope
   ENDIF

// ? "1"

   IF !Empty(cWhere) .AND. !(" WHERE "$cWhere)
      cWhere:=" WHERE "+cWhere
   ENDIF

// ? cSql,"2"

   cSql    :="SELECT * FROM "+cTable+" "+cInner

   ::cSql  :=cSql

// ? cWhere,"cWhere,TDOCENC",ErrorSys(.T.)

   IF !Empty(cWhere) .AND. COUNT(cTable+cInner,cWhere,::oDb)>0
      cSql:=cSql +" "+ cWhere //  " WHERE "+cPrimary+GetWhere("=",::uData)
      ::nOption:=0 // Ingresa para Editar
   ELSE
      ::lAutoInc:=.T.
   ENDIF

   IF !Empty(::cOrderBy)
      cSql:=cSql+" ORDER BY "+::cOrderBy+IF(lDesc," DESC"," ")
   ENDIF

   // ? ::lAutoInc,"::lAutoInc"
   // ? CLPCOPY(cSql),cWhere,SQLGET(cTable,"COUNT(*)",cWhere),cTable

// ErrorSys(.T.)
// ? CLPCOPY(cSql)

   oTable:=OpenTable(cSql+" LIMIT 1",.T.,::oDb) // " WHERE "$cSql) // jn 09/05/2017 Optimiza la carga del documentos inicial, solo requiere un registro

   DPWRITE("TEMP\"+::cTable+"_SETTABLE.SQL",oDp:cSql)

   // Campos Virtuales
   AEVAL(oTable:aFields,{|a,n|__objAddData( Self, ALLTRIM(a[1]) ) , __objSendMsg(Self,a[1],oTable:FieldGet(n))})
   AEVAL(oTable:aFields,{|a,n|__objAddData( Self, ALLTRIM(a[1])+"_" ) , __objSendMsg(Self,a[1],oTable:FieldGet(n))})

   // IF oTable:RecCount()>0
      // ::nOption:=3
      // Debe ser Modificar
   // ENDIF

   ::aFields:=oTable:aFields
   ::cWhere :=oTable:cWhere

   // JN 27/07/2017, Debe mostrar ultimo registro
   IF lDesc
     ::cWhere :=::GetWhere()
   ENDIF

   oTable:End()

   ::oTable:=oTable

   ::cDsnData:=oTable:oOdbc:cName        // JN 24/01/2024
   ::lDsnData:=(::cDsnData=oDp:cDsnData) // JN 24/01/2024

//? "AQUI TERMINA SETTABLE",oTable:nSeconds

RETURN oTable

METHOD Windows(nTop,nLeft,nHeight,nWidth) CLASS TDOCENC

     LOCAL oIco

     // ::LoadData(0) // Carga los Datos en las Variables

     DEFAULT nTop:=10,nLeft:=20,nWidth:=100,nHeight:=100

     // nWidth :=nWidth *28.464
     // nHeight:=nHeight*04.513

     ::nTop   :=nTop
     ::nLeft  :=nLeft
     ::nWidth :=nWidth
     ::nHeight:=nHeight

     DEFINE ICON oIco RESNAME "ICON"

     DEFINE WINDOW ::oWnd;
            MDICHILD OF oDp:oFrameDp;
            TITLE ::cTitle;
            FROM nTop,nLeft TO 0,170-170; // nWidth,nHeight;
            COLOR ::nGris,NIL;
            NOMAXIMIZE PIXEL

     //? nWidth,::oWnd:nWidth()
     // ? nHeight,::oWnd:nHeight()
     ::oWnd:Hide()

     ::oWnd:SetIcon(oIco)

     DEFINE DIALOG ::oDlg;
           STYLE WS_CHILD OF ::oWnd PIXEL;
           COLOR NIL,::nGris

     // No existe Color Fondo Impar
      IF !__objHasMsg(::oDlg, "lClrPaneDp" )
        __objAddData(::oDlg,"lClrPaneDp")
        ::oDlg:lClrPaneDp:=.F.
      ENDIF

     ::oDlg:Cargo:={nTop,nLeft,nWidth,nHeight}

     SetWndDefault( ::oDlg )

     ::oDlg:bKeyDown := { | nKey, nFlags |::RunKey(nKey,nFlags) }

     MOVER(Self,::cVarName)

RETURN Self

/*
// Ejecución de Teclas
*/
METHOD RunKey( nKey, nFlags)

   LOCAL nAt:=ASCAN(::aKeys,{|aKey,nAt|aKey[1]=nKey}),I

   IF nKey=VK_F9
      ::PreSave(.T.)
   ENDIF

//? "RUNKEY",nKey

   DO CASE
      CASE (nKey = 16 .OR. nKey = 17 .OR. nKey = 18) .AND. ::cKey=""    // --- primera vez (debe agregar)
         ::cKey := ::cKey + ALLTRIM(STR(nKey))
         ::nTime:= VAL(STRTRAN(TIME(),":",""))
      // --- Otra tecla... y tenia presionada ctrl o alt (debe agregar) y menos de un segundo en tiempo
      CASE (nKey <> 16 .AND. nKey <> 17 .AND. nKey <> 18) .AND. LEN(::cKey)=2
         IF ABS( ::nTime - VAL(STRTRAN(TIME(),":",""))) < 1
            ::cKey := ::cKey + ALLTRIM(STR(nKey))
            //::Cancel()  // Aqui realmente debe ejecutarse una busqueda entre los botones disponibles... (RIGC)
         ENDIF

         nAt:=ASCAN( ::aBtn  , {|a,n| ALLTRIM( a[9] ) = ::cKey })

         IF nAt>0  .AND. EVAL(::aBtn[nAt,1]:bWhen)
             Eval(::aBtn[nAt,1]:bAction)
         ENDIF

         ::cKey:=""

         RETURN .T.
         //::cKey := ""
         //?? ::aBtn[nAt,8], "---",::cKey," Linea:",nAt
         //Return .T.

      CASE (nKey = 16 .OR. nKey = 17 .OR. nKey = 18) .AND. LEN(::cKey)=2  // --- ya tenia valor y volvio a teclear ctrl o alt.
         ::cKey := ""
         MsgInfo("inicializado opcion 3")

   ENDCASE

   IF nAt=0

     nAt:=ASCAN( ::aBtn  , {|a,n| ASC(LEFT(ALLTRIM( a[8] ),4)) = nKey })
// msginfo(::cKey)
//     nAt:=ASCAN( ::aBtn  , {|a,n| LEFT(ALLTRIM( a[8] ),4) == ::cKey })

     IF nAt>0
        // JN 11/03/2014
        IIF(::nOption=0 .AND. EVAL(::aBtn[nAt,1]:bWhen) ,Eval(::aBtn[nAt,1]:bAction),NIL)
     ENDIF

     RETURN .T.

   ENDIF

   IF nAt>0

      ::Prepare()
      Eval(::aKeys[nAt,2],Self)

   ENDIF

RETURN .T.

/*
// Activa el Formulario
*/
METHOD ACTIVATE(bInit,bEnd)  CLASS TDOCENC

    LOCAL oDoc:=Self,oDlg,lSalir:=.F.,aPutControls:={},I

    IF ValType(::oWnd)!="O"
       ::Windows()
    ENDIF

    IF ::lAutoEdit .AND. ::nOption=0
       ::nOption:=3
    ENDIF

    oDlg:=::oDlg

    // ::lAutoInc

    DEFAULT bInit:={||.T.},;
            bEnd :={||oDoc:nOption=0 .OR. oDoc:lAutoInc .OR. oDoc:lAutoEdit }

    AEVAL(::aGrids,{|o,n|o:Activate(),;
                         IIF(ValType(::oExport)="O" .AND. "TXSCROLL"=::oExport:ClassName(),;
                             ::oExport:oFocus:=o:oBrw,NIL)})

//    IF ValType(::oExport)="O" .AND. "TXSCROLL"=::oExport:ClassName()
//      ::oExport:oFocus:=::oDlg
//      ? "SI ETE ES",::oDlg:ClassName()
//    ENDIF
//      ? ::oDlg:ClassName()


    oDp:aId       := {}           // inicia el control de nId

    AutoId(::oDlg)

    IF !Empty(::cFileEdit)
      aPutControls:=LoadFileEdt(::cFileEdit,Self) // Carga Valores de los Controles
    ENDIF

    FOR I := 1 TO  LEN(::oDlg:aControls)

        IF ::oDlg:aControls[I]:ClassName()="TFOLDER"

           ::oDlg:aControls[I]:bKeyDown := { | nKey, nFlags |::oDlg:aControls[I]:RunKey(nKey,nFlags)  }

           AEVAL( ::oDlg:aControls[I]:aDialogs , {|oDlg,n| oDlg:bKeyDown := { | nKey, nFlags |oDoc:RunKey(nKey,nFlags) } ,;
                                                           PutSetKey(SELF,oDlg)  })


        ENDIF

       // ::oDlg:bKeyDown := { | nKey, nFlags |::RunKey(nKey,nFlags) }
    NEXT

    // AEVAL(::oDlg:aControls,{|})

    ACTIVATE DIALOG ::oDlg NOWAIT  ;
               ON INIT (oDlg:Move(0,0),;
                        oDoc:PutBar() ,;
                        oDoc:FindGet(),;
                        MOVER(oDoc,oDoc:cVarName),;
                        oDlg:oMsgBar := TMsgBar():New( oDlg, "", .F., .F., .F., .F.,,,, ),;
                        PutControls(oDlg,.t.,.t.,nil,.t.,oDoc),;
                        oDlg:oMsgBar:SetColor(oDlg:nClrText,oDlg:nClrPane),;
                        .T.);
                VALID lSalir

             // PutControls(oDlg,.t.,nil,nil,oDoc:lDesign),;

   ::oDlg:Cargo:={.F.,NIL,NIL} // no ha sido cambiado

   AEVAL(::aGrids,{|oGrid,n|oGrid:nId:=IF(oGrid:nId=0,n,oGrid:nId),oGrid:SetSize(),oGrid:AdjustBtn(),oGrid:RestorePar()})

   ::LoadData(::nOption,0,.T.) // Carga los Datos, ya Envia a Incluir

   AEVAL(::aGrids,{|oGrid,n|oGrid:Find(),oGrid:ShowTotal(),oGrid:SetColor()})

//   ::aGrids[1]:oBrw:SetColor(CLR_GRAY,CLR_YELLOW) // ::nClrText,::nClrPane2)
//                   ::oControl:=GetControl(),;

//  oDoc:nWidth:=300
//  oDoc:nHeight:=400
// ? oDoc:nWidth,oDoc:nHeight

    ACTIVATE WINDOW ::oWnd;
            ON INIT (EVAL(bInit)               ,;
                     oDoc:lActivated:=.T.      ,;
                     oDoc:BtnPaint()           ,;
                     ChangeMenu(oDoc:oWnd,oDoc:oDlg,oDoc:cFileEdit,oDoc),; // oWnd:Move(oDlg:nTop , oDlg:nLeft, oDlg:nWidth , oDlg:nHeight ) ,;
                     oDoc:oWnd:Move(oDoc:nTop,oDoc:nLeft,oDoc:nWidth,oDoc:nHeight,.T.),;
                     oDlg:SetSize(::oWnd:nWidth-7,::oWnd:nHeight-(oDlg:oMsgBar:nHeight*1.5)+2,.T.),;
                     oDoc:oWnd:Show());
            ON RESIZE (oDlg:SetSize(::oWnd:nWidth-7,::oWnd:nHeight-(oDlg:oMsgBar:nHeight*1.5)+2),.t.);
            VALID (IIF(EVAL(bEnd),(lSalir:=.T.,oDoc:End()),.F.));
            ON MOVE EJECUTAR("BRWMOVED",oDoc:oWnd,oDoc);
            ON DOWN EJECUTAR("BRWMOVED",oDoc:oWnd,oDoc)

   IF ::lOPENSIZE
      oDoc:OPENSIZE()
   ENDIF

// ::oWnd:bGotFocus({||PUBLICO(oDoc:cVarName,oDoc)})
//Errorsys(.t.)
// MensajeErr(ValType(oDp:lMenuXp),oDp:lMenuXp)

   IF !oDp:lMenuXp

      oDlg:Refresh(.t.)

   ENDIF


   ::oWnd:bGotFocus :={||oDoc:GotFocus(.T.)}
   ::oWnd:bLostFocus:={||oDoc:LostFocus()}
   ::oDlg:bLostFocus:={||oDoc:LostFocus()}

//   SysRefresh(.T.)

   // ? ::oControl:ClassName()
   // ? ::nOption,"::nOption"
// AQUI CARGA DOS VECES
//   ::LoadData(::nOption,0) // Carga los Datos, ya Envia a Incluir

//   IF ::nOption=1
//      AEVAL(::aGrids,{|oGrid,n|oGrid:NewJob(::nOption)})
//   ENDIF
// OJO ESTE MANEJA EL TAMAÑO

   EVAL(::oWnd:bGotFocus)

   oDp:lKeyOn:=.T. // Programa DPRUNKEY

RETURN Self

/*
// Desactivar el Boton Cancelar, debe ser Ejecutado luego que se modifica, incluye o Elimina Item del Documento
*/
METHOD BtnCancelOff() CLASS TDOCENC
   LOCAL nAt:=ASCAN(::aBtn,{|aBtn,n| aBtn[7]="CANCEL" })

   IF nAt>0
     ::aBtn[nAt,1]:bWhen:={||.F.}
     ::aBtn[nAt,1]:ForWhen(.T.)
     ::oBar:Refresh(.T.)
   ENDIF

RETURN NIL

/*
// IsDef, Verifica la Existencia de Data
*/
METHOD IsDef( cName ) CLASS TDOCENC
//  LOCAL lResp:=.F.
RETURN __objHasMsg( self, cName )
/*

  ::lIsDef:=.t.

  lResp   :=(__objSendMsg(Self,cName)!=NIL)

  ::lIsDef:=.f.

RETURN lResp
*/

/*
// Crear Botones
*/
METHOD PutBar() CLASS TDOCENC
   LOCAL I,oBtn,n:=5,cFileG,u,nAt,oFont,oMenu,oCursor,cText
   LOCAL aOpc:={"Inc","Con","Mod","Eli","Prn"}

   IF !::lBar
      Aeval(::aGrids,{|oGrid|oGrid:PutBar()})
      RETURN NIL
   ENDIF

   ::Prepare()

   DEFINE CURSOR oCursor HAND

   DEFINE FONT oFont NAME "Tahoma" SIZE 0, -09 BOLD

   DEFINE BUTTONBAR ::oBar 3D SIZE ::nBtnWidth,::nBtnHeight OF ::oDlg

   // COLOR NIL,::nGris:

   ::oBar:bRClicked :={||.T.}
   ::oBar:SetColor(NIL,::nGris)

   ::aBtn:={}
/*
// TDPEDIT
   AADD(::aButtons,{NIL,"xNew.bmp"     ,MI("Incluir"  ,601)+CRLF+MI("Tecla",600)+" [I]"   ,"oDpEdit:Load(1)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lInc"  , {||.T.} ,"NEW"    ,73 })
   AADD(::aButtons,{NIL,"view.bmp"     ,MI("Consultar",602)+CRLF+MI("Tecla",600)+" [C]"   ,"oDpEdit:Load(2)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lCon"  , {||.T.} ,"VIEW"   ,67 })
   AADD(::aButtons,{NIL,"xEdit.bmp"    ,MI("Modificar",603)+CRLF+MI("Tecla",600)+" [M]"   ,"oDpEdit:Load(3)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lMod"  , {||.T.} ,"OPEN"   ,77 })
   AADD(::aButtons,{NIL,"xDelete.bmp"  ,MI("Eliminar" ,604)+CRLF+MI("Tecla",600)+" [E]"   ,"oDpEdit:Delete(3) "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lEli"  , {||.T.} ,"DELETE" ,69 } )
   AADD(::aButtons,{NIL,"xFind.bmp"    ,MI("Buscar  " ,605)+CRLF+MI("Tecla",600)+" [B]"   ,"oDpEdit:Find(3)                         "      ,"oDpEdit:nOption =0"  , {||.T.} ,"FIND"   ,66 })
   AADD(::aButtons,{NIL,"xBrowse.bmp"  ,MI("Listar"   ,606)+CRLF+MI("Tecla",600)+" [L]"   ,"oDpEdit:List()                          "      ,"oDpEdit:nOption =0"  , {||.T.} ,"LIST"  ,76 })
   AADD(::aButtons,{NIL,"xPrint.bmp"   ,MI("Imprimir" ,607)+CRLF+MI("Tecla",600)+" [P]"      ,"oDpEdit:RUNPRINT()                      "       ,"oDpEdit:nOption =0"  , {||.T.} ,"PRINT"   , 80})
   AADD(::aButtons,{NIL,"xTop.bmp"     ,MI("Primero"  ,608)+CRLF+MI("Tecla",600)+" [Home]"   ,"oDpEdit:oTable:GotoMin(NIL,oDpEdit:cScope),oDpEdit:Load(0)"       ,"oDpEdit:nOption =0"  , {||.T.}  ,"TOP"    ,  36})
   AADD(::aButtons,{NIL,"xSig.bmp"     ,MI("Siguiente",609)+CRLF+MI("Tecla",600)+" [S] [<-]" ,"oDpEdit:oTable:GotoSkip(+1,NIL,oDpEdit:cScope),oDpEdit:Load(0)"    ,"oDpEdit:nOption =0"  , {||.T.}  ,"NEXT"   ,  83})
   AADD(::aButtons,{NIL,"xAnt.bmp"     ,MI("Anterior ",610)+CRLF+MI("Tecla",600)+" [A] [->]" ,"oDpEdit:oTable:GotoSkip(-1,NIL,oDpEdit:cScope),oDpEdit:Load(0)"    ,"oDpEdit:nOption =0"  , {||.T.}  ,"FORWARD",  65})
   AADD(::aButtons,{NIL,"xFin.bmp"     ,MI("Ultimo   ",611)+CRLF+MI("Tecla",600)+" [End]"    ,"oDpEdit:oTable:GotoMax(NIL,oDpEdit:cScope),oDpEdit:Load(0)   "    ,"oDpEdit:nOption =0"  , {||.T.}  ,"BUTTON" ,  35})
   AADD(::aButtons,{NIL,"xSalir.BMP"   ,MI("Cerrar   ",612)+CRLF+MI("Tecla",600)+" [Esc]"    ,"oDpEdit:nOption:=0,oDpEdit:Close()      "       ,"oDpEdit:nOption =0"  , {||.T.}  ,"CLOSE"  , 27})
   AADD(::aButtons,{NIL,"xSave.BMP"    ,MI("Grabar   ",613)+CRLF+MI("Tecla",600)+" [F10]"    ,"oDpEdit:Save()                          "       ,"oDpEdit:nOption!=0.and.oDpEdit:nOption!=4"  , {||.T.} ,"SAVE" , 0 })
   AADD(::aButtons,{NIL,"paste.BMP"    ,MI("pegar Búsquedas",614)+CRLF        ,"oDpEdit:Paste(),oDpEdit:Load(0)     "       ,"oDpEdit:lPaste .AND. oDpEdit:nOption=4"  , {||.T.} ,"PASTE"   ,27})
   AADD(::aButtons,{NIL,"xCancel.BMP"  ,MI("Cancelar ",615)      +CRLF+MI("Tecla",600)+" [Esc]"    ,"oDpEdit:Cancel(.t.),oDpEdit:Load(0)     "       ,"oDpEdit:nOption!=0"  , {||.T.} ,"CANCEL"   ,27})
*/

   AADD(::aBtn  , {NIL,"xNew.bmp"    ,MI("Incluir"  ,601)+" (I)"      ,"oDoc:lBtnIncRun:=.T.,oDoc:LoadData(1),oDoc:lBtnIncRun:=.F.","oDoc:nOption =0 .AND. oDoc:lInc "  , {||IIF( ::nOption=0,.T.,.F.)} ,"NEW"  ,"Inc",STR(DP_CTRL_I) })

   IF ::lView
     AADD(::aBtn  , {NIL,"View.bmp"  ,MI("Consultar",602)+" (C)"   ,"oDoc:LoadData(2)                     "      ,"oDoc:nOption =0  .AND. oDoc:lView"  , {||.T.} ,"VIEW" ,"Con",STR(DP_CTRL_V) })
   ENDIF

   AADD(::aBtn  , {NIL,"xEdit.bmp"   ,MI("Modificar",603)+" (M)"      ,"oDoc:LoadData(3)                     "  ,"oDoc:nOption =0 .AND. oDoc:lMod .AND. oDoc:ISDOCMODFISCAL()"  , {||.T.} ,"EDIT" ,"Mod",STR(DP_CTRL_M) })

   IF ::lFind
     AADD(::aBtn  , {NIL,"xFind.bmp" ,MI("Buscar",605)+"(B)"      ,"oDoc:Find()                     "      ,"oDoc:nOption =0"  , {||.T.} ,"FIND" ,"Bus",STR(DP_CTRL_B) })
   ENDIF

   AADD(::aBtn  , {NIL,"xDelete.bmp" ,MI("Anular Registro",604)+"(E)","oDoc:PreDelete()                     "      ,"oDoc:nOption =0 .AND. oDoc:lEli"  , {||.T.} ,"DEL"  ,"Eli",STR(DP_CTRL_X) })
   AADD(::aBtn  , {NIL,"xPrint.bmp"  ,MI("Imprimir"       ,607)+"(P)"       ,"oDoc:Printer()                       "      ,"oDoc:nOption =0 .AND. oDoc:lPrn", {||.T.} ,"PRINT","Pri",STR(DP_CTRL_P) })

   IF !Empty(::cList)
      AADD(::aBtn  , {NIL,::cBtnList  ,MI("Listar",606)+" (L)"     ,"oDoc:ListBrw()                       "        ,"oDoc:nOption =0"  , {||.T.} ,"BROWSE","L",STR(DP_CTRL_L) })
   ELSE
      IF ::IsFunction("LIST")
         AADD(::aBtn  , {NIL,::cBtnList ,MI("Listar",606)+"(L)"     ,"oDoc:List()                       "        ,"oDoc:nOption =0"  , {||.T.} ,"BROWSE","L",STR(DP_CTRL_L) })
      ENDIF
   ENDIF


   AADD(::aBtn  , {NIL,"xTOP.BMP"    ,MI("Primer Registro",606)+"(Ctrl-<)" ,"oDoc:Primero(.F.)                    "      ,"oDoc:nOption =0 " , {||!::lBOF} ,"BTN_INI"  ,"<<","17188" })
   AADD(::aBtn  , {NIL,"xANT.BMP"    ,MI("Anterior "      ,610)+"(Shift-<)","oDoc:Skip(-1)                        "      ,"oDoc:nOption =0 " , {||!::lBOF} ,"BTN_SIG"  ,"< ","16188" })
   AADD(::aBtn  , {NIL,"xSIG.BMP"    ,MI("Siguiente"      ,609)+"(Shift->)","oDoc:Skip(+1)                        "      ,"oDoc:nOption =0 " , {||!::lEOF} ,"BTN_ANT"  ,"> ","16190" })
   AADD(::aBtn  , {NIL,"xFIN.BMP"    ,MI("Ultimo Registro",611)+"(Ctrl->)","oDoc:Primero(.T.)                     "      ,"oDoc:nOption =0 " , {||!::lEOF} ,"BTN_FIN"  ,">>","17190" })

   AADD(::aBtn  , {NIL,"xSalir.bmp"  ,MI("Salir",6)+" (X) "          ,"oDoc:Close()                         "      ,"oDoc:nOption =0"  , {||.T.} ,"EXIT" ,"X",STR(DP_CTRL_X) })

   // Botones de Edición
   AADD(::aBtn  , {NIL,"xSave.bmp"     ,MI("Grabar",7)+" (G)"    ,"oDoc:Presave(.T.)                     "      ,"oDoc:nOption=1 .OR. oDoc:nOption=3"  , {||.T.} ,"SAVE"  ,"Gra",STR(DP_CTRL_G) })

   IF !Empty(::aMemo)
     AADD(::aBtn  , {NIL,"xMemo.bmp"     , ALLTRIM(::aMemo[2])+" (W)" , "oDoc:EditMemo()                     "      ,"oDoc:nOption=1 .OR.    oDoc:nOption=3"  , {||.T.} ,"MEMO"  ,"Wri",STR(DP_CTRL_W) })
   ENDIF

   IF !Empty(::cFieldFile)
     AADD(::aBtn  , {NIL,"adjuntar.bmp"     , ALLTRIM(::cTitleFile)+" (W)" , "oDoc:EditFile()                     "      ,"oDoc:nOption=1 .OR.    oDoc:nOption=3"  , {||.T.} ,"DPFILE"  ,"Wri",STR(DP_CTRL_W) })
   ENDIF

   // AADD(::aBtn  , {NIL,"xCancel.bmp"   ,"Cancelar"  ,"oDoc:Cancel()                         "      ,"oDoc:nOption !=0"  , {||.T.} ,"CANCEL","Esc"})

   FOR I := 1 TO LEN(::aBtnNew)
      AADD(::aBtn,NIL) // ::aBtnNew[I])
      AINS(::aBtn,n)
      ::aBtn[n]:=::aBtnNew[I]
      n++
   NEXT

   n:=ASCAN(::aBtn,{|a,n|a[7]="SAVE"})+1

   FOR I := 1 TO LEN(::aBtnEdit)
      AADD(::aBtn,NIL) // ::aBtnNew[I])
      AINS(::aBtn,n)
      ::aBtn[n]:=::aBtnEdit[I]
      n++
   NEXT

   AADD(::aBtn  , {NIL,"xCancel.bmp"   ,"Salir del Proceso (Q)"  ,"oDoc:Cancel()                         "      ,"oDoc:nOption !=0 .AND. !oDoc:lCancelOff"  , {||.T.} ,"CANCEL","Qui",STR(DP_CTRL_C) })

///ViewArray(::aBtn)

   FOR I := 1 TO LEN(::aBtn)

       oMenu:=NIL

       // Crear Menú en Barra de Botones
       IF ASCAN(::aBtnMnu,{|a,n| ::aBtn[I,7]=a[1]})>0
         oMenu:=EJECUTAR("TDOCENCBTNMNU",Self,::aBtn[I,7])
       ENDIF

       IF ::nBtnStyle=1
/*
         DEFINE BUTTON oBtn;
                OF ::oBar ;
                NOBORDER  ;
                FILENAME "BITMAPS\"+::aBtn[I,2]
*/

        cFileG:=STRTRAN(::aBtn[I,2],".","G.")

// ? cFileG,::aBtn[I,2]


      IF oMenu=NIL

        DEFINE BUTTON oBtn OF ::oBar;
               ACTION 1=1;
               FILE "BITMAPS\"+::aBtn[I,2],NIL,"BITMAPS\"+cFileG;
               NOBORDER;
               TOOLTIP ::aBtn[I,3]

      ELSE

        IF ::lBtnText

          cText:=::aBtn[I,3]
          nAt  :=AT(" ",cText)
          cText:=IF(nAt>0,LEFT(cText,nAt),cText)


          DEFINE BUTTON oBtn OF ::oBar;
                 ACTION 1=1;
                 MENU oMenu;
                 NOBORDER;
                 FILE "BITMAPS\"+::aBtn[I,2],NIL,"BITMAPS\"+cFileG;
                 TOP PROMPT cText;
                 FONT oFont;
                 TOOLTIP ::aBtn[I,3]

        ELSE

          DEFINE BUTTON oBtn OF ::oBar;
                 ACTION 1=1;
                 MENU oMenu;
                 NOBORDER;
                 FILE "BITMAPS\"+::aBtn[I,2],NIL,"BITMAPS\"+cFileG;
                 TOOLTIP ::aBtn[I,3]

        ENDIF

        oBtn:SetSize(::nBtnWidth+10,::nBtnHeight,.T.)

      ENDIF

      ELSE


         DEFINE BUTTON oBtn;
                OF ::oBar ;
                PROMPT ::aBtn[I,8];
                FONT ::oFontBtn;
                TOOLTIP ::aBtn[I,3]


      ENDIF

      // ? "ACTION:"+::aBtn[I,4]
      //FOR U=1 TO LEN(aOpc)

         nAt:=ASCAN(aOpc,{|c,n|::aBtn[I,8]=c})

         IF nAt>0

          // ? EVAL(BloqueCod("oDoc:l"+aOpc[nAt]))
          // ::aBtn[I,6]:="oDoc:l"+aOpc[nAt]+" .AND. ISTAB"+aOpc[nAt]+"('"+::cTable+"')"
          ::aBtn[I,6]:="|oDoc|oDoc:l"+aOpc[nAt]+" .AND. ISTAB"+aOpc[nAt]+"('"+::cTable+"') .AND. oDoc:nOption=0"
          // ::aBtn[I,6]:="|oDoc|oDoc:lCon .AND. ISTAB"+aOpc[nAt]+"('"+::cTable+"')"
          // ? ::aBtn[I,6],::cTable,"cTable"

         ENDIF

//       ::aButtons[1,6]:=BLOQUECOD("ISTABINC('"+::cTable+"')")
//       ::aButtons[2,6]:=BLOQUECOD("ISTABCON('"+::cTable+"')")
//       ::aButtons[3,6]:=BLOQUECOD("ISTABMOD('"+::cTable+"')")
//       ::aButtons[4,6]:=BLOQUECOD("ISTABELI('"+::cTable+"')")
//       ::aButtons[7,6]:=BLOQUECOD("ISTABPRN('"+::cTable+"')")
//         ENDIF
//         ? nAt,aOpc[U]
      // NEXT U

      oBtn:bAction :=RUNBTN(Self,BloqueCod(::aBtn[I,4]))
//    oBtn:bWhen   :=RUNBTN(Self,BloqueCod(::aBtn[I,5]))
      oBtn:bWhen   :=RUNBTN(Self,BloqueCod(::aBtn[I,6]))
      ::aBtn[I,5]  :=RUNBTN(Self,BloqueCod(::aBtn[I,5]))
      oBtn:cMsg    :=ALLTRIM(::aBtn[I,3])
      oBtn:cToolTip:=ALLTRIM(::aBtn[I,3])

      oBtn:SetColor(NIL,::nGris)

      ::aBtn[I,1]:=oBtn

      IF ::aBtn[I,7]="CANCEL"
         oBtn:lCancel:=.T.
      ENDIF

 //     AADD(::aBtn  , {NIL,"xNew.bmp"    ,"Incluir  "  ,"oDoc:Load(1)                         "      ,"oDoc:nOption =0"  , {||.T.} ,"NEW"   })

  NEXT I

// ? ::cTable,::cScope,COUNT(::cTable,::cScope)

  IF COUNT(::cTable,::cScope)>0
     ::RECCOUNT(.T.)  // Obtiene la Cantidad de Registros
  ENDIF

  // DEFINE FONT oFont NAME GetSysFont() SIZE 0, -10 BOLD
  DEFINE FONT oFont NAME "Tahoma" SIZE 0, -10 BOLD

// nAt:=32
// AEVAL(::oBar:aControls,{|o,n|nAt:=nAt+o:nWidth() })
// @ .8,nAt SAY ::oSayRecord PROMPT "R:"+LSTR(::nRecNo)+"/"+LSTR(::nRecCount)+"/"+LSTR(LEN(::oBar:aControls)) PIXEL SIZE 100,20 BORDER FONT oFont

  nAt:=60+(LEN(LSTR(::nRecCount))*10)

//@ .1,0 SAY ::oSayText   PROMPT " Registro"                                PIXEL SIZE nAt,18 BORDER FONT oFont
//::oSayText:Hide()

  DEFAULT ::nRecNo:=0,;
          ::nRecCount:=0

  IF ::lRecordSeltor

     @ .8,0 SAY ::oSayRecord PROMPT " Registro "+CRLF+" R:"+LSTR(::nRecNo)+"/"+LSTR(::nRecCount) PIXEL SIZE nAt,18*2 BORDER FONT oFont;
            OF ::oBar

    IF ValType(::oSayRecord)="O"
       ::oSayRecord:Hide()
    ENDIF

  ENDIF

  oBtn:=NIL

RETURN NIL

METHOD OPENSIZE() CLASS TDOCENC

  ::cOnClose:="ONCLOSE"

/*
  oDp:nDif:=(oDp:aCoors[3]-180-::oWnd:nHeight())
  ::oWnd:SetSize(NIL,oDp:aCoors[3]-180,.T.)
  ::aGrids[1]:oBrw:SetSize(NIL,::aGrids[1]:oBrw:nHeight()+oDp:nDif,.T.)
*/

// oGrid:lBar
  ::oWnd:bResized:={|oBrw,oGrid,nDif|oGrid:=::aGrids[1],;
                                     oBrw :=::aGrids[1]:oBrw,;
                                     ::nWidth_Brw:=oBrw:nTop+::oBar:nHeight()-8,;
                                     ::oDlg:Move(0,0,::oWnd:nWidth()-10,::oWnd:nHeight()-10,.T.),;
                                     oBrw:SetSize(::oDlg:nWidth()-10,::oDlg:nHeight()-::nWidth_Brw,.T.)}

   ::cFileBrwZ:="MYFORMS\"+cFileNoPath(cFileNoExt(::cFileEdit))+".BRWZ"

   EJECUTAR("BRWRESTSIZE",::aGrids[1]:oBrw,::cFileBrwZ,.T.)

RETURN NIL

METHOD Close()  CLASS TDOCENC

//::KillForms(::nForms)
//? ::oWnd:ClassName(),::oWnd:hWnd
// ::ONCLOSE()

    ::oWnd:End()

RETURN .T.

/*
// Buscar Get Activo

*/
METHOD BuildIncremental(cField,cWhere,cMax,cVar) CLASS TDOCENC
   LOCAL I,uValue

    // ? "builincremental",cField

   IF !Empty(cField)
      // uValue:=SQLGETMAX(::cTable,cField,cWhere)
      // uValue:=STRZERO(VAL(uValue)+1,LEN(uValue))
      uValue:=SQLINCREMENTAL(::cTable,cField,cWhere,NIL,cMax)
      ::Set(cField,uValue)
      cVar:=uValue

      RETURN .T.

   ENDIF

// ? GetProce(),"GETPROCE()",len(::aIncremental)

   FOR I := 1 TO LEN(::aIncremental)

      cField:=::aIncremental[I,1]
      cWhere:=::aIncremental[I,2]
      // uValue:=SQLGETMAX(::cTable,cField,cWhere)

      uValue:=SQLINCREMENTAL(::cTable,cField,cWhere)

      DPWRITE("TEMP\"+::cTable+"_INCREMENTAL.SQL",oDp:cSql)

      //? ::cTable,cField,cWhere,"::cTable,cField,cWhere"

      IF !Empty(::aIncremental[I,3])
         uValue:=IIF( Empty(uValue) , ::aIncremental[I,3] , uValue )
         uValue:=IIF(::aIncremental[I,3]>uValue,::aIncremental[I,3],uValue)
         uValue:=SQLINCREMENTAL(::cTable,cField,cWhere,NIL,uValue)

      ENDIF

      // uValue:=STRZERO(VAL(uValue)+1,LEN(uValue))
      ::Set(cField,uValue)
      cVar:=uValue


   NEXT

RETURN .T.

METHOD ADD(cField,uValue) CLASS TDOCENC

   IF !::IsDef(cField) // Debe Crear
      __objAddData( Self , cField )
   ENDIF

   __objSendMsg(Self , cField , uValue )

RETURN uValue

/*
// Asigna Valor
*/
METHOD Set(cField,uValue) CLASS TDOCENC
   LOCAL oGet

   __objAddData( Self, cField)

   IF ::IsDef("o"+cField)

     oGet:=::Get("o"+cField)
     __objSendMsg(Self,cField,uValue)

     IF ValType(oGet)="O"
       oGet:VarPut(uValue , .T.)
     ENDIF

   ELSE

     __objSendMsg(Self,cField,uValue)

   ENDIF

RETURN uValue

/*
// Buscar Get Activo

METHOD FindGet(oDlg) CLASS TDOCENC
   LOCAL I,lGet:=.F.

   DEFAULT oDlg:=::oDlg

   FOR I := 1 TO LEN(oDlg:aControls)

      IF ("GET"$oDlg:aControls[I]:ClassName() .OR. "COMBO"$oDlg:aControls[I]:ClassName());
         .AND. oDlg:aControls[I]:lActive

         ::oControl:=oDlg:aControls[I]

         EXIT

      ENDIF

   NEXT

RETURN NIL
*/

/*
// Destruir
*/
METHOD END() CLASS TDOCENC
    LOCAL oEdit:=Self,oWnd,I

    // SaveDlg(::oDlg,.t.,::cFileEdit,::lDesign)

    LOCAL cFile:="MYFORMS\DPCOMPONENTES.BRWZ"

    IF !Empty(::cFileBrwZ)
       EJECUTAR("BRWSAVESIZE",::aGrids[1]:oBrw,::cFileBrwZ,.T.)
    ENDIF

    ::ONCLOSE()

    SaveDlg(::oDlg,.t.,::cFileEdit,::lDesign,NIL,NIL,oEdit)

    ::UnLockTable( )

    IIF( ValType(::oTable)="O" , ::oTable:End() , ::oTable:=NIL )

//  aeval(::aGrids,{|oGrid|oGrid:End()})

   FOR I=1 TO LEN(::aDpEdit)

      oEdit:=::aDpEdit[I]

      IF (ValType(::aDpEdit[I])="O".AND.::aDpEdit[I]:nNumEdit>0)
         oWnd:=oEdit:oWnd
         IIF( ValType(oWnd)="O" , (oWnd:End()) , NIL)
         oWnd:=NIL
      ENDIF

   NEXT I

   AEVAL(::aGrids,{|oGrid,n|oGrid:nId:=IF(oGrid:nId=0,n,oGrid:nId),oGrid:End()})

   ::aDpEdit:={}
   ::aGrids :={}

   // OJO
   ::cIdFrm :=""

   oDp:cHelpRtf:=::cFileRtfOld

/*
   IF ValType(oDp:oFocus)="O"
      ? "SI EXISTE EL FOCO"
      DpFocus(oDp:oFocus)
      oDp:oFocus:=NIL
   ENDIF
*/
RETURN .T.

/*
// Elimina el Formulario Actual de la Lista
*/
METHOD KillForms(nForm) CLASS TDOCENC
/*

  LOCAL nAt

  nAt:=aScan( aForms, { |o,n| ValType(o)="O" .AND. o:nForms == nForm } )

  IF nAt>0
    ADEL(aForms,nAt)
    ASIZE(aForms,LEN(aForms)-1)
  ENDIF

  AEVAL(aForms,{|o|IIF( ValType(o), , )o:Prepare()})

*/

RETURN NIL

/*
// Obtiene uno o varios campos del Formulario
*/
METHOD  GetValue(cField) CLASS TDOCENC
  local aFields:=_VECTOR(cField,IIF("+"$cField,"+",",")),I
  local uValue :=""

  IF Len(aFields)=1
     return ::Get(cField)
  ELSE // Debe ser Lista de Cadenas
     AEVAL(aFields,{|a,i|uValue:=uValue+CTOO(::Get(a),"C")})
  ENDIF

// ViewArray(aFields)
// ? uValue,GETPROCE()

RETURN uValue

/*
// Contador Ejecuta desde el Inicio de la Barra de Botones
*/
METHOD RECCOUNT(lIni) CLASS TDOCENC
 LOCAL cWhere,nCuantos:=0,oTable
 LOCAL aCampos:=_VECTOR(::cOrderBy),cOper:="<="
 LOCAL cDesde:="",cHasta:="",cFieldX:=""

 DEFAULT lIni:=.T. // Inicia y obtiene el primer y Ultimo Registro, Cuando se busca o lista el registro debe obtener la posicion del Registro

 // Hasta es el Registro Actual
 cHasta:=""
 AEVAL(aCampos,{|a,n| cFieldX:=cFieldX+IF(Empty(cFieldX),"",",")+a,;
                      cHasta :=cHasta +IF(Empty(cHasta ),"",",")+CTOSQL(::Get(a)) })

 cHasta:="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cHasta+")"

 cOper:=">="
 cWhere :=::GetWhereMax(::cOrderBy , "", .f.) // JN 14/04/16

//? cWhere,"No puede estar vacia",::cOrderBy,"<-::cOrderBy",getproce(),::cScope

 // jn 11/05/2017, se hace lento, haciendo select sin where
 IF Empty(cWhere) .AND. !Empty(::cScope)
    cWhere:=" WHERE "+::cScope
 ENDIF

 IF !"WHERE "$cWhere .AND. !Empty(cWhere)
    cWhere:=" WHERE "+cWhere
 ENDIF

 oTable :=OpenTable("SELECT "+::cOrderBy+" FROM "+::cTable+" "+cWhere)
 AEVAL(oTable:aFields,{|a,n| cDesde :=cDesde +IF(Empty(cDesde ),"",",")+CTOSQL(oTable:FieldGet(n)) })
 oTable:End()

 //? oDp:cSql,"RECCOUNT()"

  DPWRITE("TEMP\TDOCENC"+ALLTRIM(::cTable)+"RECCOUNT.SQL",oDp:cSql)

 ::cSqlIni:=oDp:cSql

// oTable:Browse()
 cDesde  :="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cDesde+")"
 cWhere  :=::cScope+IF(Empty(::cScope),""," AND ")+"("+cDesde+" AND "+cHasta+")"

 IF lIni
   ::nRecCount:=COUNT(::cTable,cWhere,::oDb)
   ::cSqlFin:=oDp:cSql
   ::nRecno   :=::nRecCount
// ? ::nRecCount,"::nRecCount"
 ELSE
   ::nRecno   :=COUNT(::cTable,cWhere,::oDb)
//? COUNT(::cTable,cWhere),"Encontrado",CLPCOPY(oDp:cSql)
   ::oSayRecord:Refresh(.T.)
 ENDIF

 IF ValType(::oSayRecord)="O"
   ::oSayRecord:Refresh(.T.)
 ENDIF

 //? cDesde,cHasta,::cTable,::cScope,::nRecCount,"aqui finaliza reccount"

RETURN NIL

METHOD UpDateFromTable() CLASS TDOCENC
RETURN AEVAL(::oTable:aFields,{|a,n| ::Set(a[1],::oTable:FieldGet(n))})


/*
// Redefine el When de los Botones
*/
//METHOD BtnForWhen()
//RETURN .T.

/*
 *  OnError()
 */
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TDOCENC

  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N,cScrRun:=""
  LOCAL lResp:=.F.

  IF ::lIsDef
     RETURN NIL
  ENDIF

  cMsg   := UPPE(ALLTRIM(__GetMessage()))


  if Left( cMsg, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2)

      __objAddData( Self, cMsg )
      __objSendMsg(Self,cMsg,uValue)

      RETURN uValue

  ELSE

    ::Prepare()

    IF !ValType(::oScript)="O"
       ::SetScript(::cScript)
    ENDIF

    //IF ::oScript:cProgram<>::cScript .OR. Empty(::oScript:aFunctions)
    ::SetScript(::cScript)
    // ENDIF

    // 19/09/2023

    IF Empty(::oScript:aFunctions)
// ? LEN(::oScript:aFunctions),"LEN(::oScript:aFunctions), Debe compilar ",::oScript:cProgram
       ::oScript:=DPSCRIPTRUN(::oScript:cProgra)
// XCOMPILA(cProgram)
// ? ::oScript:cProgram,"::oScript:cProgram"
    ENDIF

// ? ::cScript,::oScript:cProgram

    IF !::oScript:IsFunction(cMsg)

// ? LEN(::oScript:aFunctions),"LEN(::oScript:aFunctions)"
//  ViewArray(::oScript:aFunctions)

        cScrRun:=VP("SCRPROGRAM")
        cScrRun:=IIF( ValType(cScrRun)<>"C" , ::cScript , cScrRun )

        MensajeErr(cMsg+" sin Declaración "+::oScript:cProgram+;
                   CRLF+"DpXbase:"+::cScript , "DpXbase Run:"+cScrRun+" / "+::ClassName())

        // MensajeErr("REQUIERE COMPILAR "+::cScript+CRLF+"Función o Variable "+cMsg+" no Existe "+::CLASSNAME())
        uValue:=.F. // La mayoria de las Funciones requieren .T. / .F.

    ELSE

       // Ejecuta la Función
       ::oScript:lNoError:=!::lMsgError
       uValue=::oScript:Run(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)

    ENDIF

  ENDIF

RETURN uValue

static function GenDataEdit( oEdit , cName , nPos )
      local nClassH := oEdit:ClassH
/*
      __clsAddMsg( nClassH, cName, ;
         { | oEdit | oEdit:aVars[ nPos ] }, HB_OO_MSG_INLINE )

      __clsAddMsg( nClassH, "_" + cName, ;
         { | oEdit, uValue | oEdit:aVars[nPos]:= uValue }, HB_OO_MSG_INLINE )
*/
return .T.

/*
// Ejecución de los Botones
*/
STATIC FUNCTION RUNBTN(oDoc,bAction)
RETURN {||PUBLICO("oDoc",oDoc),oDoc:Prepare(),Eval(bAction,oDoc)}

//STATIC BLQDOC(oDoc,bAction)
//RETURN EVAL(bAction,oDoc)

/*
// Ejemplo de valores
 aData := __objGetValueList( xTxt )
 nLen  := Len( aData )
 for n := 1 to nLen                     // For each item : Recurse !
    cOut += aData[n][HB_OO_DATA_SYMBOL] + ":" + ;
            ToChar( aData[n][HB_OO_DATA_VALUE], cSeparator, lDebug )

 // asignar méthodos
  __objAddInline( Self,     aField[ MYSQL_FS_NAME ], bBlock )
  __objAddInline( Self, "_"+aField[ MYSQL_FS_NAME ], bB  lock )

 // Verificar Methodo
   IF __objHasMethod( oOwner, "Add" )
     oOwner:Add( Self )
  ENDIF
  // borrar data
     __objDelData( ::Parent, UPPER(::name ))
  // Verificar Data
   __objHasMsg( self, cName ) .
*/

STATIC FUNCTION FrmFindBlq(oFrm,oGet,cField,cScript)

    DEFAULT cScript:="DOCFIND"

// RETURN {|lResp|lResp:=EJECUTAR("DOCFIND",oFrm,oGet,cField),lResp:=IIF( ValType(lResp)!="L" , .F. , lResp )}
RETURN {|lResp|lResp:=EJECUTAR(cScript,oFrm,oGet,cField),lResp:=IIF( ValType(lResp)!="L" , .F. , lResp )}


STATIC FUNCTION PutSetKey(oFrm,oDlg)

    AEVAL(oDlg:aControls, {|oControl,n| IF(oControl:bKeyDown= NIL, NIL , oControl:bKeyDown := { | nKey, nFlags |oFrm:RunKey(nKey,nFlags) } )})

RETURN .T.

FUNCTION ISFRMIDRUN(cId,cVarName,aData)
  LOCAL nAt:=0

  nAt:=ASCAN(aForms,{|a,n| a[2]=cVarName .AND. a[3]:cIdFrm=cId})

  // Si para como @referencia, se pueden leer sus datos

  IF ValType(aData)="A"
     aData:={}
     AEVAL(aForms,{|a,n| AADD(aData,{a[2],a[3]:cIdFrm}) })
  ENDIF

//? nAt,CLPCOPY(cId),cVarName

  IF nAt>0
     aForms[nAt,3]:oWnd:Restore()
  ENDIF

RETURN nAt>0




