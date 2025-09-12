
// Clase TDpEdit
// Edita formularios para el Sistema
// 01/12/2002
//  <oGet>:oGet:Changed  Determina si el dato ha sido cambiado
*/

#define SM_CXSCREEN         0

#define _NTOP      3
#define _NLEFT     4
#define _NBOTTOM   5
#define _NRIGHT    6
#define _NCLRPANE  7
#define _NCLRTEXT  8
#DEFINE _NFONT     9

#define COLOR_BTNFACE     15
// #define CLR_BLACK 0

#include "FiveWin.ch"
#include "InKey.ch"
#include "xbrowse.ch"
#include "\DWH\LIBS\TSBUTTON.CH"
// #include "Obj2Hb.ch"


// STATIC aFonts,aId,aPutControls
// STATIC nTopCopy,nLeftCopy,nClrPaneCopy,nClrTextCopy,nWithCopy,nHeightCopy,oFontCopy

STATIC hToolTip := 0,lToolTip:=.F.,lMdi,oWndMain

STATIC aDpEdit :={}
STATIC nNumEdit:=0
STATIC aEdit   :={} // Lista de Formularios que Seran Eliminados por Timer/Main

// Posibles Variables Privadas
MEMVAR oDp,oBrw,oCursor,oBody,oDpEdit,oCol,oEditGet,oTable
MEMVAR oWnd,oDlg,cFileEdit,cVarName,cFieldName

MEMVAR oFrm // 09/09/2023

REQUEST OCLONE

/*
 * DPEDIT()
 */

CLASS DPEDIT // FROM TDPCLASS

   DATA lAutomatic  AS LOGICAL     INIT .F.
   DATA lSalir       // Condicion de Salida o Cerrar el Control
   DATA lChange      // Indica si Hubo Algún Cambio en el Formulario
   DATA lMultiple    // Indica que la misma Variable tiene multiples ejecuciones
   DATA lAutoInclude // indica si incluye de manera automática
   DATA lCancel      // Cancelación Solicitada Afirmativa
   DATA lDlg         // Indica si es MDI/DLG Controles
   DATA lDesign      // Si ha sido Diseñado o No
   DATA lDialog     INIT .F.   // Si el Formulario no es MDI solo Dialogo
   DATA lEdit
   DATA lFirst INIT .F.
   DATA lSetColorGroup INIT .T.
   DATA lSaveHead    // Indica que el Encabezado ya fué Grabado
   DATA lMdi
   DATA lActivated
   DATA lFound
   DATA lMsgError    // Evita los Mensajes de Error SCRIPT
   DATA lErrorSave  INIT oDp:lErrorSave // Muestra el Error Cuando APLICA COMMIT
   DATA lIsDef
   DATA lEscClose    // Cerrar con ESC
   DATA lMsgBar     INIT .F.
   DATA lWnd        INIT .T. // Con esta Clase no se requiere Ventana
   DATA lPaste      INIT .T. // Activa la Opcion Pegar cuando se presiona Buscar
   DATA lSetDef     INIT .T.
   DATA lSavedDsn   INIT .F.
   DATA lBrwCopy    INIT .F. // Copia lista de los Browse
   DATA lClose      INIT .F. // JN 06/11/2016
   DATA lSqlBegin   INIT .T. // Caso de DPXBASE, Guarda en DpXbase y no en el formulario DPEDIT
   DATA lCreaRegIntRef INIT .T.
   DATA lAutoClose     INIT .F. // indica cerrar el formulario cuando falla scrollget por la falta de campos
   DATA lIntRef        INIT .T. // Valida la Integridad Referencial
   DATA lMdiBar        INIT .F. // En caso .T. La Barra de botones sera ampliada
   DATA lPrint         INIT .T. // Boton es Imprimir
   DATA lFileEdt       INIT .F. // Requiere Archivo EDT, Cuando se crea el formulario podra indicarse la condicion .T.

   // Funciones para ScrollGet
   DATA lVScroll INIT .F.
   DATA lHScroll INIT .F.
   DATA aScrollSize INIT {}

   // Restringir estos Botones
   DATA lInc        INIT .T.
   DATA lCon        INIT .T.
   DATA lMod        INIT .T.
   DATA lEli        INIT .T.
   DATA lLock       INIT .F.
   DATA lEof        INIT .F.
   DATA lBof        INIT .F.

   /* DATA lEof      // Control de Navegación
   DATA lBof */

   DATA aDataKey    AS ARRAY       INIT {}
   DATA aVars       AS ARRAY       INIT NIL
   DATA aIncremental // Campos que se AutoIncrementan
   DATA aSetKey     AS ARRAY  INIT {}

   DATA aSayGroup INIT ARRAY(250)

   // DATA aButtons     // Botones Superiores del Formulario
   DATA aBtnActive   // Permisos en Botones
   DATA aRecord      // Copia del Registro que esta en Edición
   DATA aCopyEdit    // Hace una Copia de las Variables declaradas
   DATA aScrollGets  // Lista de los Scrolls Gets
   DATA aButtons     // Botones de Opciones, tipo Documento
   DATA aFind        // Controles Find
   DATA aControls    // Controles
   DATA aLevel       // Etiqueta de los Campos
   DATA aFieldsTable
   DATA aBrwFocus    INIT {} // Copia de los Browse Copiados en BlostFocus
   DATA aTablesH     INIT {} // requerido DPCLASS para Generar HTML

   // Para Buscar y Consultar
   DATA aValid       // Validación de los Controles (Buscar y Consultar)
   DATA aWhen        // Condicion de los Controles
   DATA aItems       // Itemns de los ComboBox
   DATA aOpcButtons  // Botones de las Opciones
   DATA aEditButtons // Botones de la Edición
   DATA aDpEdit      // Formularios Relacionados
   DATA aTableLink   // Tablas Enlazadas
   DATA aMemo        // Datos del Memo
   DATA aRepeatget
   DATA aDataTable

   DATA aMsgItem INIT {}


   DATA cName       AS CHARACTER   INIT ""  READONLY
   DATA cFile        // Nombre del Archivo de Controls
   DATA cTitle       // Titulo de la Ventana
   DATA cVarPublic   // Nombre de la Instancia
   DATA cFileEdit    // Nombre del Archivo de Controles
   DATA cScript      // Nombre del Programa Script que se Ejecuta
   DATA cTable       // Nombre de la Tabla
   DATA cCancel      // Function que se ejecuta para Cancelar
   DATA cPreSave     // PreGrabar
   DATA cSave        // PostGrabar
   DATA cView        // Programa Consultar
   DATA cLoad        // Programa de Carga de Datos
   DATA cAfterLoad INIT "AFTERLOAD" // ejecutar luego del Load
   DATA cWhere       INIT ""  // Condición del Campo que se Modifica
   DATA cListWhere   INIT ""  // 02/01/2024
   DATA cDataKey     // Clave del registro
   DATA cPrint       // Programa que Ejecuta la Impresión
   DATA cDelete      // Función que Ejecuta Eliminar
   DATA cList        // Programa que hace el ListBox
   DATA cListWhere   // Where para la Lista de Trabajadores
   DATA cFind        // Programa Buscar FRMFIND
   DATA cListTitle   // Titulo para Browse
   DATA cSqlIni      // Query Inicial
   DATA cSingular    // Nombre Singular
   DATA cFileChm
   DATA cTopic
   DATA cFileBmp     // Bmp para el Brush
   DATA cFindNoEnter
   DATA cScope       INIT "" // Filtro
   DATA cScopeF      INIT ""
   DATA cIdFrm       INIT ""
   DATA cFieldFile   INIT "" // Campo Digitalizacion de Documentos VINCULO con DPFILEEMP
   DATA cFileRtf     INIT "" // Archivo de Ayuda RTF
   DATA cFileRtfOld  INIT oDp:cHelpRtf
   DATA cNombre      INIT ""
   DATA cOrderBy,cPrimary,cDesde,cHasta,cActual
   DATA cFieldAud    INIT "" // Pistas de Auditoria
   DATA cMsgBar      INIT "" // Mensaje de Barra Inferior
   DATA cVarName     INIT "" // Necesario  BRRUNNEW/DPXBASE
   DATA cOnClose     INIT "ONCLOSE"
   DATA cTextGroup   INIT ""
   DATA cKeyAudita   INIT "" // Campo clave para el registro de Auditoría
   DATA cFieldRef    INIT "" // CAMPO REFERENCIA INV_DESCRI

   DATA cBtnList     INIT "xbrowse.bmp"

   DATA cScrFind    INIT "DOCFIND"
   DATA cScopeFind,cSqlFin  // Filtro para Buscar

  // DATA cSqlTable    // Sql de la Tabla (Modificar)

   DATA nMode        // Tipo de Formulario =0 Simple
   DATA nMaxButtons  // Cantidad de Botones, Edicion Linea Superior
   DATA nPos         AS NUMERIC     INIT 0   READONLY // Posicion de la ultima Variable
   DATA nOpction     // Número de Opción de Ejecución
   DATA nOrder       AS NUMERIC INIT 1 // Orden del Encabezado
   DATA nClrPane   // Color del Dialogo
   DATA nFileMain    AS NUMERIC INIT 0 // Registro con DPFILEEMP
   //DATA nBtnWidth    AS NUMERIC INIT 42

   DATA nBtnWidth   INIT 40
   DATA nBtnHeight  INIT 40
   DATA nBarHeight  INIT 40+5

   DATA nClrPane1   INIT 0
   DATA nClrPane2   INIT 0


   // DATA nNumMemo     AS NUMERIC INIT 0 // Número de Campo Memo Asociado
   // DATA nType        AS NUMERIC INIT 0 // Formulario Simple/DLG

   DATA oDlg        // Dialogo de Controles
   DATA oWnd        // Ventana del
   DATA oScript     // Programa DpXbase que se Ejecuta con la Clase
   DATA oScroll     //
   DATA oWhere      // Objeto para Asignar los Filtros Mediante Otro Formulario MDI
// DATA oCursor     // Cursor de Datos del Browse de Navegación
   DATA oBrw        // Browse de Navegación
   DATA oTable      // Tabla de Datos
   DATA oToolMsg    // ToolTip
   DATA oBody       // Cuerpo de Documento
   DATA oTmr        // Controlador
   DATA oCtrDoc     // Controlador de Inclusión de Documentos
   DATA oIntRef     // Control de Integridad Referencial <Modificar>
   DATA oEditMemo   // Edición de Campo Memo
   DATA oEditScr    // Edición SCROLL para Campos Adicionales
   DATA oFocus      // Foco del Get
   DATA oControl    // Foco perdido con Lost/Got Focus
   DATA oBrush
   DATA oFrmLnk
   DATA oDb         // Gestor de la Base de datos
   DATA oDpLbx
   DATA oSayRecord // Muestra en la Super de la Barra la Posición
   DATA oAdjustWnd   // Ajusta el Control según
   DATA oBar         // Contiene Controles
   DATA oBtn_Font    // Fuente de la Barra

   DATA nNumLbx     // Número del ListBox Que llamó al Formulario
   DATA nNumEdit

   DATA nRecno    INIT 0
   DATA nRecnoOld INIT 0 // Registro Anterior
   DATA nRecCount INIT 0


   DATA bEnd
   DATA bValid       // Validación de Salida
   DATA bSet         // Actualiza Desde Otro Formulario
   DATA bInit        // Ejecución Cuando se Inicia el Dialogo
   DATA bKeyF10
   DATA bPostRun    init {||NIL}
   DATA bResized    INIT {||NIL} // 13/05/2023 Mover consultas dinámicas

   DATA bLostFocus  // JN 06/11/2016
   DATA bGotFocus   // Recuperación de Focus


   //  MSGRUNVIEW
   DATA lMsgBar  INIT .F.
   DATA nRadio   INIT 1
   DATA cText    INIT ""
   DATA nRecord  INIT 0
   DATA lStop    INIT .F.
   DATA oMeter   INIT NIL
   DATA nData    INIT 0
   DATA lStop_   INIT .F.
   DATA lReset   INIT .F.
   DATA oBtnStop INIT NIL
   DATA oMeter   INIT NIL

   DATA uValue1,uValue2,uValue2,uValue3,uValue4,uValue5


// DATA bTimer     // Bloque de Código del Timer
// ClassData para la Edición de Documentos //
// DATA oVarOld

//   METHOD New( cFile, cVarPublic, cVarPublic, lWindows )
   METHOD NEW(cTitle, cFile , cVarPublic , lWindows , lDialog , oWnd , lModal, lOnlyMdi )
   METHOD End()

   METHOD NOTIFY() INLINE NIL

   METHOD Add( cName, xValue )
   METHOD Del( cName )
   METHOD Get( cName )
   METHOD Set( cName, xValue )
   METHOD VarPut(cName,xValue)
   METHOD NewFilter(nOption,cOption) INLINE EJECUTAR("BRWNEWFILTER",Self,nOption,cOption)

   METHOD SetBotBar()

   METHOD SETMSGBAR_ON()
   METHOD SetMsgItem(cText,nlen,bAction) INLINE AADD(::aMsgItem,{cText,nLen,bAction,NIL}),::lMsgBar:=.T.  // JN 12/07/2018
   METHOD SetMsgItemText(nId,cText)      INLINE IF(nId>0 .AND. nId<=LEN(::aMsgItem), (::aMsgItem[nId,4]:SetText(CTOO(cText,"C")),::aMsgItem[nId,4]:Refresh()),NIL)
   METHOD SetMsg(cMsgBar)                INLINE (::cMsgBar:=cMsgBar,;
                                                 IF(ValType(::oWnd)="O",(::oWnd:oMsgBar:SetMsg(::cMsgBar),::oWnd:oMsgBar:Refresh()),NIL))

   METHOD GetPos( cName )
   METHOD Release()

   METHOD SETCOLORSAY(nClrText,nClrPane)


   METHOD BtnSetMnu(cBtn,cOption,cFunction,cFileBmp,cPrgSubmenu) INLINE AADD(::aBtnMnu,{cBtn,cOption,cFunction,cFileBmp,cPrgSubmenu}) // Opciones para ser Incluidas en los Botones

   // jn 11/03/2019
   METHOD SetScroll(n1,n2,n3,n4) INLINE (::lVScroll:=.T.,;
                                         ::lHScroll:=.T.,;
                                         ::lMsgBar :=.F.,;
                                         ::aScrollSize:={n1,n2,n3,n4})

   METHOD GetWhere(cPrimary)
   METHOD RECCOUNT(lIni)
   // METHOD IsDef( cName )
   METHOD IsDef( cName ) // INLINE (::lIsDef:=.t.,oSend(Self,cName)!=NIL)
          //      INLINE (::GetPos(cName)>0)

// METHOD Clone()          INLINE aClone( ::aClone )
// METHOD nCount()         INLINE Len( ::aVars )
// METHOD Save()           INLINE aClone( ::aVars )
// METHOD Restore( aVars ) INLINE ::aVars := aClone( aVars )
   METHOD Load( nOption , nSkip)
   METHOD CheckBtn()
   METHOD SetColorBar(nClrText,nClrPane) // Color para las Barras
   METHOD CreateVars( oObj )
   METHOD SetTimer( bAction, nInterval )
   METHOD SetTable( oTable , lCreateWindow , oDb )
   METHOD SetEdit(lOn)       // Establece Condicion de Edición
   METHOD GetValue(cField)   // GetValue()
   METHOD ViewTable(cTable,cField,cWhere) // Indica la cantidad de tablas asociadas
   METHOD LbxRefresh()
   METHOD BuildLink()                        // Visualiza las Tablas Asociadas
   METHOD Activate( bInit,bValid )
   METHOD Incremental( cVarName, lZero, cSql , cWhere  )
   METHOD LinkTable(cTable,cSql)
   METHOD CreateWindow()
   METHOD Windows() INLINE CreateWindow()
   METHOD ValUnique( cKey, cField,cMsg,cWhere  ) //jn
   METHOD LostFocus()
   METHOD GotFocus()
   METHOD RunBtn(nPos,nValid)
   METHOD Save() //jn
   METHOD Cancel(lAsk,lFromValid) //jn
   METHOD DestroyMsg()
   METHOD GetWhereMax()
   METHOD MOVETO(oControl) INLINE EJECUTAR("FRMMOVE",SELF,oControl) // Mueve el Control hacia la posición del Control

// METHOD SetBody(oTable,cScript,aFields,cPreFix) //jn
   METHOD Delete() INLINE ::RunScr(::cDelete)
   METHOD RunScript(cFunction,uPar1,uPar2)
   METHOD DelRecord(lIntRef)  // Borra con la Integridad Ref
   METHOD PutVar(cVarName,nValue,oSay) // Asigna Valir y Refresca Objeto
   METHOD SetMemo(cField,cTitle,nTop,nLeft,nWidth,nHeight)
   METHOD SetBmp(cField,cTitle,nTop,nLeft,nWidth,nHeight)
   METHOD SetAdjuntos(cField,cTitle)
   METHOD SetIcon(cFile)
   METHOD RepeatGet(aFiedls,aExcluye)
   METHOD Find()
   METHOD CancelFind()
   METHOD ChkIntRef()
   METHOD RunKey(nKey)  // Opciones I,C,M del Formulario
   METHOD SetKey(nKey,bAction)
   METHOD ValidDlg()
   METHOD ValidWnd(bValid)
   METHOD Prepare()
   METHOD Paste()
   METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)
   METHOD SetScript(cScript)
   METHOD AdjustWnd(oControl) INLINE ::oAdjustWnd:=oControl
   METHOD ViewDef() INLINE EJECUTAR("VIEWDEF",SELF)

   METHOD ONCLOSE()

   METHOD DEFBTN()

   METHOD UpdateFromTable()

   METHOD ApEditMemo() INLINE  (CursorWait(),;
                                KillGetCol(::oBody),;
                                ::oEditMemo:=_DPMEMOEDIT(Self,::oEditMemo),;
                                ::LinkEdit(::oEditMemo))


   METHOD  LockTable( ) INLINE (IIF(!::lLock,::oTable:LockTable(),NIL),;
                                ::lLock:=.T.)

   METHOD  UnLockTable( ) INLINE (IIF(::lLock,::oTable:UnLockTable(),NIL),;
                                  ::lLock:=.F.)

   METHOD EditMemo() INLINE ::SetMemo()

//,;
//                            KillGetCol(::oBody),;
//                            ::oEditMemo:=_DPMEMOEDIT(Self,::oEditMemo),;
//                            ::LinkEdit(::oEditMemo)


   METHOD IsSaveHead(lUpdate,cPrimary)
   // METHOD EndCtrDoc() //Finaliza el Control del Documento
// METHOD InitBody()
   METHOD RunScr(cProgram)
   METHOD SetDefault()
   METHOD SetDefValue() INLINE EJECUTAR("FRMSETDEFAULT",SELF) // JN 8/12/2018, Asigna los Valores por Defecto
   METHOD ONINIT()

   METHOD List(cWhere,cTitle)
   METHOD RUNPRINT()

   // cTable,cFileScr,oWnd)

   METHOD ScrollGet(cTable,cFileScg,cExcluye,oWnd,nType,oFont,nStyle)

   METHOD ScrollField(cTitle)    // Campos AdicionaleS

   METHOD LinkEdit(oDpEdit) INLINE (AADD(::aDpEdit,OCLONE(oDpEdit))) // JN 28/02/2014 ,DpFocus(oDpEdit:oDlg)) // Relación Entre Formularios

   METHOD KillEdit(oDpEdit)

      // INLINE MsgAlert("Kill ESTE")

   METHOD OpcButtons(cOption,cBmp,cRun,cWhen,nKey,cType) INLINE;
                                        nKey:=IIF( nKey=NIL, 0 , nKey ),;
                                        AADD(::aOpcButtons ,{cBmp,GetFromVar(cOption),cRun,cWhen,nKey,cType})

   METHOD CloseAllEdit()

   METHOD EditButtons(cOption,cBmp,cRun,cWhen,nKey) INLINE;
                                              nKey:=IIF( nKey=NIL , 0 , nKey ),;
                                              AADD(::aEditButtons,{cBmp,cOption,cRun,cWhen,nKey})

   METHOD SetMsg(cMsg)  INLINE IIF( ::lMsgBar , (::oDlg:oMsgBar:SetMsg(cMsg),::oDlg:oMsgBar:Refresh()) , NIL)

   METHOD SETBTNBAR(nAlto,nAncho,oBar,nCol)

   METHOD Close()

   // METHOD RunFunction(cFunction,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14);
   //                        INLINE (VP("SCRRUNFUNCTION",cFunction),;
   //                        MsgAlert(::cScript),;
   //                        SCRRUN(::cScript,P1,P2,P3,P4,P5,P6,P7,P8,P9,P10,P11,P12,P13,P14))
// SETSCRIPT(),;  // JN 07/04/2016 Obtiene el Actual Programa DpXbase en Ejecución


/* INLINE (::oScript:=GetScript(cScript),;
//                              ::cScript:=cScript,;
//                              MsgAlert(::oScript:ClassName(),oDp:cScriptRun))
// ,::oScript:cProgram))
// VP("SCRPROGRAM"))
                              // MsgAlert(::oScript:ClassName(),::oScript:cProgram))
*/


   METHOD ShowMsg(cToolTip,oControl,nClrPane,nClrText) // JN CLASS TWindow
   METHOD MensajeErr(cToolTip,oControl,nRow,nCol) INLINE ::ShowMsg(cToolTip,oControl,nRow,nCol)
   METHOD Level(cField)
   METHOD SetLevel(cField,cText)

   // JN 14/12/2019 Migrar Programa MsgRun

METHOD FRMTITLE(cTitle)
METHOD FRMCENTRAR()
METHOD FRMSETTOTAL(nCant)
METHOD FRMSET(n,lSay,bBlq)
METHOD FRMTEXT(cText,bBlq)
METHOD FRMSAY(cText,bBlq)
METHOD FRMHIDE()
METHOD FRMSHOW()
METHOD FRMCLOSE()

   METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                             WndHandleEvent( Self, nMsg, nWParam, nLParam )

   METHOD COMMAND(nValue) INLINE 1=1

   ERROR HANDLER OnError( cMsg,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )

ENDCLASS

/*
 *  DPEDIT:New()
*/

METHOD New(cTitle, cFile , cVarPublic , lWindows , lDialog , oWnd , lModal, lOnlyMdi  ) CLASS DPEDIT // [WA]
   LOCAL lAutomatic:=.t.,i,u // := Las Variables no son True
   LOCAL nClrPane  :=NIL,nClrText:=NIL,nAt
   LOCAL oIco
   LOCAL oDpEdit     :=SELF
   LOCAL oDpLbx

   // IIF(ValType(oDp:oToolMsg)="O",(oDp:oToolMsg:End(),oDp:oToolMsg:=NIL),NIL)

   CursorWait()

   // MsgDemo()

   oDp:oForm:=Self

   IF ValType(cVarPublic)="C"
     ::cVarName:=ALLTRIM(cVarPublic)
   ENDIF

   // JN 03/12/2015
   ::nNumLbx     :=oDp:nNumLbx
   oDpLbx:=GetDpLbx(::nNumLbx)
   ::oDpLbx:=oDpLbx

   IF ValType(::oDpLbx)="O" .AND. lDialog =NIL
      lDialog:=::oDpLbx:lDialog
   ENDIF

//   DEFAULT lDialog:=oDp:lSetDialog // Si es Dialog

   // Errorsys(.t.)

   DEFAULT cTitle    :="DpEdit Class"
   DEFAULT lAutomatic:=.F.
   DEFAULT lWindows  :=.F.
   DEFAULT lDialog   :=.F.
   DEFAULT cVarPublic:="oDpLbx"
   DEFAULT oWndMain  :=DpWndMain()

   DEFAULT lOnlyMdi  :=.F.

   // lDialog:=.F. // !oDp:lMenuXp // QUITAR DIALOGO
  // lDialog:=.T.
 //? Errorsys(.t.),"AQUI ES"

   oDp:lBotar:=.t.

   DEFAULT oDp:aLevel:={} // Contenido de las Etiquetas


   lMdi:=!lDialog

   //  ? lMdi,"lMdi"
   // Controlador de Instancias

   cTitle            :=GetFromVar(cTitle) // Convierte los Datos
   nNumEdit          :=IIF( nNumEdit=NIL , 0 , nNumEdit+1 )
   aDpEdit           :=IIF( aDpEdit =NIL , {}, aDpEdit    )
   oWndMain          :=oWnd

   VP("lRunBtn",.F.) // Utilizado en la Validación del Cuerpo

//   PUBLICO(cVarPublic,NIL)

   oDp:oFocus    :=NIL
   oDp:aId       := {}           // inicia el control de nId

   ::lDialog     :=lDialog
   ::aVars       := {}
   ::lAutomatic  :=lAutomatic   // [WA]
   ::cFileEdit   :=""
   ::cFindNoEnter:=""
   ::cFind       :="FRMFIND"

   ::aBtnMnu     :={} //JN 28/12/2018


   oDp:lLbxDialog:=::lDialog // Utilizado para los LBX Llamados desde el Formulario TDPEDIT 03/12/2015

   IF !Empty(cFile)
     ::cFileEdit   :=IIF( "\"$cFile.OR.":"$cFile,"" ,oDp:cPathEdt)+cFile        // Nombre del Archivo donde se Guardan los Documentos
   ELSE
     cFile:=""
   ENDIF

   ::nMode       :=0            // Formulario Simple
   ::nMaxButtons :=10
   ::aBtnActive  :=ARRAY(::nMaxButtons) // Determina los Permisos de cada Boton
   ::nOption     :=0               // Opción del Formulario
   ::aIncremental:={}
   ::cFile       :=::cFileEdit // cFile          // Archivo del Formulario
   ::cTitle      :=cTitle //+ " "+cFile
   ::lChange     :=.F.            // Cambios en el Formulario
   ::nNumLbx     :=oDp:nNumLbx    // asociación entre el Editor y el ListBox
   ::lMultiple   :=.F.            // Verifica multimes ejecuciones y requiere una nueva instancia del Script
   ::lSaveHead   :=.F.
   ::lAutoInclude:=.t.            // indica si incluye de manera automática
   ::lActivated  :=.F.
   ::lIsDef      :=.F.
   ::lMsgError   :=.T.
   ::aScrollGets :={}
   ::lCancel     :=.F.  // 21/01/2021        // Cancelación no Salicitada
   ::lFound      :=.F.
   ::lDlg        :=.T.
   ::lDesign     :=.F.
   ::lEdit       :=.T. // Modo Edicion
   ::aButtons    :={} // Lista de Botones
   ::aOpcButtons :={}  // Botones en las Opciones
   ::aEditButtons:={}  // Botones Durante la Edición
   ::aDpEdit     :={}  // Formularios Relacionados
   ::aTableLink  :={}
   ::aMemo       :={}  // No Asigna Memo
   ::bEnd        :={||.T.}
   ::bSet        :={||.T.}
   ::bInit       :={||.T.}
   ::lMdi        :=.F.
   ::aRepeatGet  :={}
   ::bKeyF10     :=::SetKey(VK_F10,{||oDpEdit:RunKey(VK_F10)})
   ::lEscClose   :=.T.
   ::cTopic      :=cFileNoExt(cFileNoPath(::cFileEdit))
//   ::lMsgBar     :=.T.
   ::cPostSave   :=""
   ::cCancel     :=""
   ::cPreSave    :=""
   ::cPostSave   :=""
   ::cDelete     :="DELETE"
   ::cPrint      :="PRINT"

   IF lWindows
      ::CreateWindow()
   ENDIF

// ? "LUEGO DE CREATEWINDOWS",ErrorSys(.T.)

   ::SetScript() // JN 07/04/2015  Se ejecuta desde Panel ERP

   DEFAULT cVarPublic:="oDpEdit"

   ::cVarPublic:=ALLTRIM(UPPE(cVarPublic))
   //::cVarName  :=::cVarPublic

   PUBLICO("oFrm",Self) // 09/09/2023

   IF !Empty(::cScript)
      ::oScript   :=GetScript(::cScript)       // obtiene el Script de Ejecución
   ELSE
      ::oScript   :=GetScript()       // obtiene el Script de Ejecución
      ::cScript   :=VP("SCRPROGRAM")  // Nombre del Script de Ejecución
   ENDIF

   //  07/06/2018 Incidencia desde el panel asume nombre del programa del panel ERP ,::cScript   :=VP("SCRPROGRAM")  // Nombre del Script de Ejecución
   ::nNumEdit  :=nNumEdit

   nAt:=aScan( aDpEdit, { |e,n| e[2] == ::cVarPublic } )

/*
   AADD(::aButtons,{NIL,"xNew.bmp"     ,MI("Incluir"  ,601)+CRLF+MI("Tecla",600)+" [I]"   ,"oDpEdit:Load(1)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lInc"  , {||.T.} ,"NEW"    ,73 })
   AADD(::aButtons,{NIL,"view.bmp"     ,MI("Consultar",602)+CRLF+MI("Tecla",600)+" [C]"   ,"oDpEdit:Load(2)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lCon"  , {||.T.} ,"VIEW"   ,67 })
   AADD(::aButtons,{NIL,"xEdit.bmp"    ,MI("Modificar",603)+CRLF+MI("Tecla",600)+" [M]"   ,"oDpEdit:Load(3)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lMod"  , {||.T.} ,"OPEN"   ,77 })
   AADD(::aButtons,{NIL,"xDelete.bmp"  ,MI("Eliminar" ,604)+CRLF+MI("Tecla",600)+" [E]"   ,"oDpEdit:Delete(3) "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lEli"  , {||.T.} ,"DELETE" ,69 } )
   AADD(::aButtons,{NIL,"xFind.bmp"    ,MI("Buscar  " ,605)+CRLF+MI("Tecla",600)+" [B]"   ,"oDpEdit:Find(3)                         "      ,"oDpEdit:nOption =0"  , {||.T.} ,"FIND"   ,66 })
// AADD(::aButtons,{NIL,"xBrowse.bmp"  ,MI("Listar"   ,606)+CRLF+MI("Tecla",600)+" [L]"   ,"oDpEdit:List()                          "      ,"oDpEdit:nOption =0"  , {||.T.} ,"LIST"  ,76 })

   AADD(::aButtons,{NIL,::cBtnList     ,MI("Listar"   ,606)+CRLF+MI("Tecla",600)+" [L]"   ,"oDpEdit:List()                          "      ,"oDpEdit:nOption =0"  , {||.T.} ,"BROWSE"  ,76 })
// cBtnList

   IF lPrint
     AADD(::aButtons,{NIL,"xPrint.bmp"   ,MI("Imprimir" ,607)+CRLF+MI("Tecla",600)+" [P]"      ,"oDpEdit:RUNPRINT()                      "       ,"oDpEdit:nOption =0"  , {||.T.} ,"PRINT"   , 80})
   ENDIF

   AADD(::aButtons,{NIL,"xTop.bmp"     ,MI("Primero"  ,608)+CRLF+MI("Tecla",600)+" [Home]"   ,"oDpEdit:oTable:GotoMin(NIL,oDpEdit:cScope),oDpEdit:Load(0)"       ,"oDpEdit:nOption =0"  , {||!oDpEdit:lBof  }  ,"TOP"    ,  36})
   AADD(::aButtons,{NIL,"xSig.bmp"     ,MI("Siguiente",609)+CRLF+MI("Tecla",600)+" [S] [<-]" ,"oDpEdit:oTable:GotoSkip(+1,NIL,oDpEdit:cScope),oDpEdit:Load(0,+1)"    ,"oDpEdit:nOption =0"  , {||!oDpEdit:lEof }  ,"NEXT"   ,  83})
   AADD(::aButtons,{NIL,"xAnt.bmp"     ,MI("Anterior ",610)+CRLF+MI("Tecla",600)+" [A] [->]" ,"oDpEdit:oTable:GotoSkip(-1,NIL,oDpEdit:cScope),oDpEdit:Load(0,-1)"    ,"oDpEdit:nOption =0"  , {||!oDpEdit:lBof }  ,"FORWARD",  65})

//   AADD(::aButtons,{NIL,"xSig.bmp"     ,MI("Siguiente",609)+CRLF+MI("Tecla",600)+" [S] [<-]" ,"oDpEdit:oTable:GotoSkip(+1,NIL,oDpEdit:cScope),oDpEdit:Skip(+1),oDpEdit:Load(0,+1)"    ,"oDpEdit:nOption =0"  , {||.T.}  ,"NEXT"   ,  83})
//   AADD(::aButtons,{NIL,"xAnt.bmp"     ,MI("Anterior ",610)+CRLF+MI("Tecla",600)+" [A] [->]" ,"oDpEdit:oTable:GotoSkip(-1,NIL,oDpEdit:cScope),oDpEdit:Skip(-1),oDpEdit:Load(0,-1)"    ,"oDpEdit:nOption =0"  , {||.T.}  ,"FORWARD",  65})

   AADD(::aButtons,{NIL,"xFin.bmp"     ,MI("Ultimo   ",611)+CRLF+MI("Tecla",600)+" [End]"    ,"oDpEdit:oTable:GotoMax(NIL,oDpEdit:cScope),oDpEdit:Load(0)   "    ,"oDpEdit:nOption =0"  , {||!oDpEdit:lEof}  ,"BUTTON" ,  35})
   AADD(::aButtons,{NIL,"xSalir.BMP"   ,MI("Cerrar   ",612)+CRLF+MI("Tecla",600)+" [Esc]"    ,"oDpEdit:nOption:=0,oDpEdit:Close()      "       ,"oDpEdit:nOption =0"  , {||.T.}  ,"CLOSE"  , 27})
   AADD(::aButtons,{NIL,"xSave.BMP"    ,MI("Grabar   ",613)+CRLF+MI("Tecla",600)+" [F10]"    ,"oDpEdit:Save()                          "       ,"oDpEdit:nOption!=0.and.oDpEdit:nOption!=4"  , {||.T.} ,"SAVE" , 0 })
   AADD(::aButtons,{NIL,"paste.BMP"    ,MI("pegar Búsquedas",614)+CRLF        ,"oDpEdit:Paste(),oDpEdit:Load(0)     "       ,"oDpEdit:lPaste .AND. oDpEdit:nOption=4"  , {||.T.} ,"PASTE"   ,27})
   AADD(::aButtons,{NIL,"xCancel.BMP"  ,MI("Cancelar ",615)      +CRLF+MI("Tecla",600)+" [Esc]"    ,"oDpEdit:Cancel(.t.),oDpEdit:Load(0)     "       ,"oDpEdit:nOption!=0"  , {||.T.} ,"CANCEL"   ,27})
*/

   DEFAULT aEdit:={}

//   oDpLbx:=GetDpLbx(::nNumLbx)
//
//   ::oDpLbx:=oDpLbx

// Obtiene la BD desde LBX
//   IF ValType(oDpLbx)="O" .AND. !oDpLbx:oDb=NIL .AND.  ::oDb=NIL
//      ::oDb=oDpLbx:oDb
//? "AQUI COPIAMOS LA BD DEL LBX"
//   ENDIF

   AADD(aDpEdit,{nNumEdit,cVarPublic,Self})
   AADD(aEdit,Self)

   PUBLICO(::cVarPublic,oDpEdit)
   PUBLICO("oBody",NIL)

// ? "AQUI TERMINA EL NEW",ERRORSYS(.T.)

RETURN Self

METHOD DEFBTN() CLASS DPEDIT

   AADD(::aButtons,{NIL,"xNew.bmp"     ,MI("Incluir"  ,601)+CRLF+MI("Tecla",600)+" [I]"   ,"oDpEdit:Load(1)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lInc"  , {||.T.} ,"NEW"    ,73 })
   AADD(::aButtons,{NIL,"view.bmp"     ,MI("Consultar",602)+CRLF+MI("Tecla",600)+" [C]"   ,"oDpEdit:Load(2)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lCon"  , {||.T.} ,"VIEW"   ,67 })
   AADD(::aButtons,{NIL,"xEdit.bmp"    ,MI("Modificar",603)+CRLF+MI("Tecla",600)+" [M]"   ,"oDpEdit:Load(3)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lMod"  , {||.T.} ,"OPEN"   ,77 })
   AADD(::aButtons,{NIL,"xDelete.bmp"  ,MI("Eliminar" ,604)+CRLF+MI("Tecla",600)+" [E]"   ,"oDpEdit:Delete(3) "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lEli"  , {||.T.} ,"DELETE" ,69 } )
   AADD(::aButtons,{NIL,"xFind.bmp"    ,MI("Buscar  " ,605)+CRLF+MI("Tecla",600)+" [B]"   ,"oDpEdit:Find(3)                         "      ,"oDpEdit:nOption =0"  , {||.T.} ,"FIND"   ,66 })
// AADD(::aButtons,{NIL,"xBrowse.bmp"  ,MI("Listar"   ,606)+CRLF+MI("Tecla",600)+" [L]"   ,"oDpEdit:List()                          "      ,"oDpEdit:nOption =0"  , {||.T.} ,"LIST"  ,76 })

   IF ValType(::oTable)="O" .AND. COUNT("DPVIEWGRU","VIG_TABLA"+GetWhere("=",::oTable:cTable))>0
         AADD(::aButtons,{NIL,"viewdef.bmp"  ,MI("Consulta Definible",602)+CRLF+MI("Tecla",600)+" [C]"   ,"oDpEdit:ViewDef(0)   "      ,"oDpEdit:nOption =0 .AND. oDpEdit:lCon"  , {||.T.} ,"VIEW"   ,67 })
   ENDIF

   AADD(::aButtons,{NIL,::cBtnList     ,MI("Listar"   ,606)+CRLF+MI("Tecla",600)+" [L]"   ,"oDpEdit:List()                          "      ,"oDpEdit:nOption =0"  , {||.T.} ,"BROWSE"  ,76 })
// cBtnList

   IF ::lPrint
     AADD(::aButtons,{NIL,"xPrint.bmp"   ,MI("Imprimir" ,607)+CRLF+MI("Tecla",600)+" [P]"      ,"oDpEdit:RUNPRINT()                      "       ,"oDpEdit:nOption =0"  , {||.T.} ,"PRINT"   , 80})
   ENDIF

   AADD(::aButtons,{NIL,"xTop.bmp"     ,MI("Primero"  ,608)+CRLF+MI("Tecla",600)+" [Home]"   ,"oDpEdit:oTable:GotoMin(NIL,oDpEdit:cScope),oDpEdit:Load(0)"       ,"oDpEdit:nOption =0"  , {||!oDpEdit:lBof  }  ,"TOP"    ,  36})
   AADD(::aButtons,{NIL,"xSig.bmp"     ,MI("Siguiente",609)+CRLF+MI("Tecla",600)+" [S] [<-]" ,"oDpEdit:oTable:GotoSkip(+1,NIL,oDpEdit:cScope),oDpEdit:Load(0,+1)"    ,"oDpEdit:nOption =0"  , {||!oDpEdit:lEof }  ,"NEXT"   ,  83})
   AADD(::aButtons,{NIL,"xAnt.bmp"     ,MI("Anterior ",610)+CRLF+MI("Tecla",600)+" [A] [->]" ,"oDpEdit:oTable:GotoSkip(-1,NIL,oDpEdit:cScope),oDpEdit:Load(0,-1)"    ,"oDpEdit:nOption =0"  , {||!oDpEdit:lBof }  ,"FORWARD",  65})

//   AADD(::aButtons,{NIL,"xSig.bmp"     ,MI("Siguiente",609)+CRLF+MI("Tecla",600)+" [S] [<-]" ,"oDpEdit:oTable:GotoSkip(+1,NIL,oDpEdit:cScope),oDpEdit:Skip(+1),oDpEdit:Load(0,+1)"    ,"oDpEdit:nOption =0"  , {||.T.}  ,"NEXT"   ,  83})
//   AADD(::aButtons,{NIL,"xAnt.bmp"     ,MI("Anterior ",610)+CRLF+MI("Tecla",600)+" [A] [->]" ,"oDpEdit:oTable:GotoSkip(-1,NIL,oDpEdit:cScope),oDpEdit:Skip(-1),oDpEdit:Load(0,-1)"    ,"oDpEdit:nOption =0"  , {||.T.}  ,"FORWARD",  65})

   AADD(::aButtons,{NIL,"xFin.bmp"     ,MI("Ultimo   ",611)+CRLF+MI("Tecla",600)+" [End]"    ,"oDpEdit:oTable:GotoMax(NIL,oDpEdit:cScope),oDpEdit:Load(0)   "    ,"oDpEdit:nOption =0"  , {||!oDpEdit:lEof}  ,"BUTTON" ,  35})
   AADD(::aButtons,{NIL,"xSalir.BMP"   ,MI("Cerrar   ",612)+CRLF+MI("Tecla",600)+" [Esc]"    ,"oDpEdit:ONCLOSE(),oDpEdit:nOption:=0,oDpEdit:Close()      "       ,"oDpEdit:nOption =0"  , {||.T.}  ,"CLOSE"  , 27})
   AADD(::aButtons,{NIL,"xSave.BMP"    ,MI("Grabar   ",613)+CRLF+MI("Tecla",600)+" [F10]"    ,"oDpEdit:Save()                          "       ,"oDpEdit:nOption!=0.and.oDpEdit:nOption!=4"  , {||.T.} ,"SAVE" , 0 })
   AADD(::aButtons,{NIL,"paste.BMP"    ,MI("pegar Búsquedas",614)+CRLF        ,"oDpEdit:Paste(),oDpEdit:Load(0)     "       ,"oDpEdit:lPaste .AND. oDpEdit:nOption=4"  , {||.T.} ,"PASTE"   ,27})
   AADD(::aButtons,{NIL,"xCancel.BMP"  ,MI("Cancelar ",615)      +CRLF+MI("Tecla",600)+" [Esc]"    ,"CursorWait(),oDpEdit:Cancel(.t.),oDpEdit:Load(0)     "       ,"oDpEdit:nOption!=0"  , {||.T.} ,"CANCEL"   ,27})

RETURN SELF

/*
// Colocar Colores en la Barra de Botones
*/
METHOD SetColorBar(nClrText,nClrPane) // Color para las Barras

   IF !Empty(::aButtons)
     ::aButtons[1,1]:oWnd:SetColor(nClrText,nClrPane)
     AEVAL(::aButtons[1,1]:oWnd:aControls,{|o,n|o:SetColor(nClrText,nClrPane)})
   ENDIF

RETURN NIL

METHOD PREPARE() CLASS DPEDIT

  PUBLICO(::cVarPublic,Self)
  oDp:oForm:=Self
  MOVER(Self,::cVarPublic)

RETURN NIL

METHOD DestroyMsg()

   IF ValType(::oToolMsg)="O"
      ::oToolMsg:End()
   ENDIF

   ::oToolMsg:=NIL

RETURN .T.


METHOD SETSCRIPT(cScript)  CLASS DPEDIT

     IF !Empty(cScript)
        ::oScript:=GetScript(cScript)
        RETURN NIL
     ENDIF

     ::oScript:=oDp:oRunLine:oFunction:oScript
     ::cScript:=cScript

RETURN ::oScript

METHOD SETCOLORSAY(nClrText,nClrPane,oDlg)  CLASS DPEDIT
     LOCAL I,oObj

     DEFAULT oDlg:=::oDlg

     FOR I=1 TO LEN(::oDlg:aControls)

        oObj:=::oDlg:aControls[I]

        IF oObj:Classname()="TSAY"
           oObj:SetColor(nClrText,nClrPane)
        ENDIF

     NEXT I

RETURN NIL

/*
// Iniciación
*/
METHOD ONINIT() CLASS DPEDIT

   IF !Empty(::aButtons) .AND. ::nMode=1
      ::CheckBtn()
   ENDIF

RETURN .T.

METHOD UpDateFromTable(lBrowse) CLASS DPEDIT
  LOCAL I

  DEFAULT lBrowse:=.F.

// ? "AQUI ES  UpDateFromTable"
 IF lBrowse
   // ViewArray(::oTable:aFields)
   ::oTable:Browse()
 ENDIF

 FOR I=1 TO LEN(::oTable:aFields)
  //  AEVAL(::oTable:aFields,{|a,n| ::Set(a[1],::oTable:FieldGet(n))})
   __objSendMsg( Self, ::oTable:aFields[I,1] ,::oTable:FieldGet(I))
 NEXT I

RETURN NIL

METHOD FRMTITLE(cTitle) CLASS DPEDIT

  ::oWnd:SetText(cTitle)
  ::oWnd:Refresh(.T.)

RETURN NIL


METHOD FRMCENTRAR()  CLASS DPEDIT
  LOCAL nAlto :=::oWnd:nHeight()
  LOCAL nAncho:=::oWnd:nWidth()
  LOCAL aCoors:=GetCoors( GetDesktopWindow() )

  nAlto :=(aCoors[3]-nAlto)/2
  nAncho:=(aCoors[4]-nAncho)/2

  ::oWnd:Move(nAlto,nAncho,NIL,NIL,.T.)
  ::oWnd:Refresh(.T.)

RETURN NIL

METHOD SETBOTBAR(nAlto,nAncho) CLASS DPEDIT
   LOCAL oCursor

   IF ::oBar=NIL

     DEFAULT nAlto:=52-15,nAncho:=60-15

     DEFINE CURSOR oCursor HAND
     DEFINE BUTTONBAR ::oBar SIZE nAlto,nAncho OF ::oDlg 3D CURSOR oCursor

   ENDIF

RETURN ::oBar

METHOD FRMSETTOTAL(nCant) CLASS DPEDIT

  LOCAL cTitle:=::cTitle,cText:=::cText,nData:=::nData,lStop:=::lStop_,lReset:=::lReset

  IF ::oMeter=NIL
     ::Close()
     oDp:oMsgRun:=NIL
     MSGRUNVIEW(cTitle,cText,nCant,lStop,lReset)
  ENDIF

  IF ::oMeter=NIL
    RETURN NIL
  ENDIF

  ::oMeter:Hide()

RETURN ::oMeter:SetTotal(nCant)

METHOD FRMSET(n,lSay,bBlq,cText) CLASS DPEDIT


  DEFAULT lSay :=.F.,;
          cText:=""

  IF ::oMeter=NIL
     ::FRMSETTOTAL(1)
  ENDIF

  ::oMeter:Show()

  IF lSay

     cText:=cText+IF(Empty(cText),""," ")+LSTR(n)+"/"+LSTR(::nData)

//? cText,"cText"

     ::FRMTEXT(cText,bBlq)

  ENDIF

RETURN ::oMeter:Set(n)

METHOD FRMTEXT(cText,bBlq) CLASS DPEDIT

  DEFAULT bBlq:={||.T.}

  IF oDp:oMsgRun=NIL  .OR. oDp:oMsgRun:oWnd=NIL
     RETURN .F.
  ENDIF

  IF ::oSay=NIL
     MSGRUNVIEW(NIL,cText)
  ENDIF

  cText:=CTOO(cText,"C")

  IF !::oWnd=NIL
    ::oWnd:Show()
  ENDIF

  ::oSay:SetText(cText)

  IF ValType(bBlq)="B"
     EVAL(bBlq)
  ENDIF

RETURN SysRefresh(.T.)

METHOD FRMSAY(cText,bBlq) CLASS DPEDIT

  IF oDp:oMsgRun=NIL
     RETURN .F.
  ENDIF

  IF ::oSay=NIL
     MSGRUNVIEW(NIL,cText)
  ENDIF

  IF !::oWnd=NIL
    ::oWnd:Show()
  ENDIF

  ::oSay:SetText(cText)

  DEFAULT bBlq:={||.T.}

  EVAL(bBlq)

RETURN SysRefresh(.T.)


METHOD FRMHIDE() CLASS DPEDIT

   IF ::oWnd=NIL
      RETURN NIL
   ENDIF

RETURN ::oWnd:HIDE()

METHOD FRMSHOW() CLASS DPEDIT

   IF ::oWnd=NIL
      RETURN NIL
   ENDIF

RETURN ::oWnd:Show()

METHOD FRMCLOSE() CLASS DPEDIT

  IF ::oWnd=NIL

     oDp:oMsgRun:=NIL

     RETURN NIL

  ENDIF

  ::Close()

  oDp:oMsgRun:=NIL

RETURN NIL

METHOD ValidWnd(bValid)  CLASS DPEDIT

  // IF ValType(::oWnd:nLastKey)="N"
  //  ::oWnd:Settext(LSTR(::oWnd:nLastKey))
  // ENDIF

  // oDp:oFrameDp:SetText(oDp:oToolMsg:ClassName())
  // ? "AQUI VALIDWND" ,::oToolMsg:ClassName(),oDp:oToolMsg:ClassNAme()

  ::ONCLOSE()

  ::DestroyMsg()

  DEFAULT ::bValid:={||.T.}

  IF !Eval(bValid)
     RETURN .F.
  ENDIF

  IF !Eval(::bValid)
     RETURN .F.
  ENDIF


RETURN .T.

/*
// Ejecuta cuando se Cierra el Formulario, caso cuando cierra el Proceso de Reversion, concluye el proceso.
*/
METHOD ONCLOSE() CLASS DPEDIT

 IF Empty(::cOnClose)
   RETURN .T.
 ENDIF

 IF ::oScript=NIL
   ::oScript   :=GetScript(::cScript)
 ENDIF

 IF !ValType(::oScript)="O"
    RETURN .F.
 ENDIF

// ? ::oScript:ClassName(),::cOnClose,GETPROCE(), "INCIDENCIA GUARDANDO PROGRAMA"

 IF !::oScript:IsFunction(::cOnClose)
    RETURN .F.
 ENDIF

RETURN ::RunScript(::cOnClose)

METHOD CLOSE() CLASS DPEDIT

 //   ::DestroyMsg()

 ::ONCLOSE()

 ::lClose:=.T.

 IF ValType(::oDlg)="O" .AND. ::oDlg:hWnd>0
//    ::DestroyMsg()
    AEVAL(::oDlg:aControls,{|o,n| IIF(o:oToolMsg=NIL,NIL,o:oToolMsg:End()) })
    KillControl(::oDlg,::lDesign)
    ::oDlg:End()
 ENDIF

 IF ValType(::oWnd)="O" .AND. ::oWnd:hWnd>0
    ::oWnd:End()
 ENDIF

 ::End()

RETURN NIL

/*
// Devuelve Descripción del Campo, ";" CRLF
*/
METHOD Level(cField) CLASS DPEDIT
   LOCAL cText:="",nAt

   DEFAULT ::aLevel:=EJECUTAR("DPGETLEVELTABLE",::oTable:cTable)

   nAt:=ASCAN(::aLevel,{|a,n| cField==a[1] })

   IF nAt>0
      RETURN ::aLevel[nAt,2]
   ENDIF

RETURN cText

/*
// Asigna Valor a la Etiqueta
*/

METHOD SetLevel(cField,cText) CLASS DPEDIT
   LOCAL nAt

   DEFAULT ::aLevel:=EJECUTAR("DPGETLEVELTABLE",::oTable:cTable)

   nAt:=ASCAN(::aLevel,{|a,n| cField==a[1] })

   IF nAt>0
      ::aLevel[nAt,2]:=cText
   ENDIF

RETURN NIL


METHOD ValidDlg() CLASS DPEDIT

   LOCAL lResp,I

//   ? LEN(::oDlg:aControls)
//
//   AEVAL(::oDlg:aControls,{|o,n|MensajeErr(o:nLastKey,o:Classname())})
//   ? ::oDlg:nLastKey,::oWnd:nLastKey
// ? ::oToolMsg:ClassName()

   ::DestroyMsg()

   IF ::oDlg:nLastKey=27 .OR. !Empty(::cTable)
      RETURN .F.
   ENDIF

//   RETURN .T.

   DEFAULT ::bValid:={||.T.}

   lResp:=EVAL(::bValid)
   lResp:=IIF( ValType(lResp)="L" , lResp , .T.  )

RETURN lResp

METHOD SetKey(nKey,bAction) CLASS DPEDIT
   LOCAL nAt

   IF ValType(bAction)="O"
      bAction:=bAction:bAction
   ENDIF

   AADD(::aSetKey,{nKey,bAction})

RETURN nIL

/*
// Cancelar buscar
*/
METHOD CancelFind()
   LOCAL oGet,I,uValue

   DEFAULT ::aFind:={}

   FOR I := 1 TO LEN(::aFind)

       oGet       :=::aFind[I,3]
       oGet:bValid:=::aFind[I,4]
       oGet:bWhen :=::aFind[I,5] // Este lo hace Lento

       IF "COMBO"$oGet:ClassName()
          oGet:SetItems(::aFind[I,7])
//        ? "ASI ES",LEN(oGet:aItems)
//        oGet:Select(::aFind[i,8])
       ENDIF

   NEXT

   IF !Empty(::aFind) .AND. !::lFound
      ::Load(0)
   ENDIF

   ::aFind:={}
   ::lFind:=.F.

RETURN .T.
/*
// Realiza la Búsqueda de Controles, solo en Controles Asociados a Variables
*/
METHOD Find() CLASS DPEDIT
    LOCAL I,cVar,oGet,aItems,nAt,uValue,aOptions

    ::Load(4)
    ::aFind:={}
    ::lFind:=.F.

    FOR I := 1 TO LEN(::oTable:aFields)
       cVar:="O"+UPPE(::oTable:Fieldname(I))

       IF ::IsDef(cVar)

         oGet  :=::Get(cVar)

         IF ValType(oGet)<>"O"
            // ? i,oGet:ClassName(),cVar,::oTable:FieldName(i)
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

//            ADEL(aItems ,LEN(aItems))
//            ASIZE(aItems,LEN(aItems)-1)

         ELSE

            aItems:=NIL
            nAt   :=0
            EVAL(oGet:bSetGet,CtoEmpty(oGet:VarGet())) // Coloca Vacio
            oGet:Refresh(.T.)

         ENDIF

         AADD(::aFind,{::oTable:Fieldname(I),cVar,oGet,oGet:bValid,oGet:bWhen,uValue,aItems,nAt})

         oGet:bWhen :={||.T.}
         oGet:bValid:=FrmFindBlq(Self,oGet,::oTable:Fieldname(I),.T.)

       ENDIF

    NEXT

RETURN .T.

/*
// Recuperar Focus
*/
METHOD GotFocus(lAuto)
   LOCAL nLastKey:=::oDlg:nLastKey,I,oBtn

   DEFAULT lAuto:=.F.

//   oDp:lSetDialog:=::lDialog

   ::Prepare()
   MOVER(Self,::cVarPublic)

   oDp:cHelpRtf:=::cFileRtf

   Memory(-1) //

   IF ValType(lAuto)="L" .AND. !lAuto
      ::oWnd:GotFocus()
   ENDIF

   IF ValType(::oControl)="O" .AND. ::oControl:oWnd:hWnd>0


//      IF !("XBR"$::oControl:ClassName()) // 29/03/2016 SCROLLGET No se puede refrescar, pierde la Posicion ::nArrayAt, ::nRowSel
        DpFocus(::oControl)
        ::oControl:Refresh(.T.)
        ::oDlg:nLastKey:=nLastKey
        ::oControl:ForWhen()
// oDp:oFrameDp:SetText(::oControl:ClassName()+" EN DPEDIT "+TRAN( !("TXB"$::oControl:ClassName()),"L"))
//    ENDIF

   ENDIF

   IF ValType(::bGotFocus)="B" // JN 06/11/2016
      EVAL(::bGotFocus,Self)
   ENDIF

   AEVAL(::aBrwFocus,{|a,n| EJECUTAR("BRWGOTFOCUS",a)})

   IF ValType(::oBar)="O" .OR. ValType(::oBtn_Font)="O" .AND. ValType(oBar:aControls)="A"   // Fuente de la Barra

     FOR I=1 TO LEN(::oBar:aControls)

       oBtn:=::oBar:aControls[I]

       IF "BTN"$oBtn:ClassName()
          oBtn:SETFONT(::oBtn_Font)
       ENDIF

     NEXT I

   ENDIF


   // FRMBRWGOTFOCUS(Self)  // Recupera las Posiciones del Browse

RETURN NIL


METHOD LostFocus()

//   SETDIALOG(::lDialog)
// oDp:lSetDialog:=::lDialog
// oDp:lLbxDialog:=::lDialog
// VP("lLbxDialog",::lDialog) // JN
// oDp:oFrameDp:SetText("LOSTFOCUS "+IIF(::lDialog,"SI","NO")+" "+IIF(oDp:lSetDialog,"SI","NO"))

   IF !VP("oControl")=NIL // JN
      ::oControl:=VP("oControl")
   ENDIF

   SetKey(VK_F10,::bKeyF10)

   IF ValType(::bLostFocus)="B" // JN 06/11/2016
      EVAL(::bLostFocus,Self)
   ENDIF

   AEVAL(::aBrwFocus,{|a,n| EJECUTAR("BRWLOSTFOCUS",a)})

 //FRMBRWLOSTFOCUS(Self) // Copia los Browse

RETURN NIL

/*
// Asigna Valor y Refresca Objeto
*/
METHOD SetMemo(cField,cTitle,nTop,nLeft,nWidth,nHeight) CLASS DPEDIT

   LOCAL aData:={},nNumMemo:=0

   DEFAULT cTitle:="Descripción Amplia"

   IF !::IsDef(cField) // Campo no Existe
     RETURN .F.
   ENDIF

   nNumMemo:=::Get(cField)

   cTitle:=GetFromVar(cTitle)

   ::aMemo:={cField,cTitle,nTop,nLeft,nWidth,nHeight,nNumMemo,"",""}
   // Agrega un Nuevo Boton
   // ::EditButtons(cTitle,"MEMO.bmp"  ,{||aData:=EJECUTAR("MEMOEDIT",nNumMemo,::aMemo[7],cTitle,nTop,nLeft,nWidth,nHeight)})
   ::EditButtons(cTitle,"xMemo2.bmp"  ,{||KillGetCol(::oBody),;
                                        ::oEditMemo:=_DPMEMOEDIT(Self,::oEditMemo),;
                                        ::LinkEdit(::oEditMemo) })

RETURN NIL

/*
// Refresca LBX
*/
METHOD LbxRefresh() CLASS DPEDIT
    LOCAL oDpLbx,aKey,cKey,I

    IF ::nNumLbx>0 // Refrescan los Datos el ListBox

       CursorWait()

       oDpLbx:=GetDpLbx(::nNumLbx)

       IF ValType(oDpLbx)="O" .AND. ALLTRIM(oDpLbx:cTable)=ALLTRIM(::oTable:cTable)


          IF oDpLbx:oCursor:RecCount()=0 // Ya Tiene el Primer Registro
             AEVAL(oDpLbx:aBtns,{|o|o[8]:Enable()})
          ENDIF

          aKey:=_VECTOR(oDpLbx:cOrderBy)

          cKey:=IIF( Len(aKey)>0,::Get(aKey[1]),NIL)

          oDpLbx:Refresh(::nOption=1,cKey)

          IF LEN(aKey)>1 .AND. ValType(cKey)="C"
             FOR I=2 TO LEN(aKey)
                aKey[I]:=ALLTRIM(aKey[I])
                // ? Ï,aKey[I],oDpLbx:cOrderBy
                cKey:=cKey+CTOO(::Get(aKey[I]),"C")
               NEXT
          ENDIF

       ELSE

          oDpLbx:=NIL


       ENDIF
       // JN 19/08/2013

       IF ValType(oDpLbx)="O"
        oDpLbx:Refresh(::nOption=1,cKey)
       ENDIF

    ENDIF

RETURN ValType(oDpLbx)="O"

/*
// Asigna Valor y Refresca Objeto
*/
METHOD SetBmp(cField,cTitle,nTop,nLeft,nWidth,nHeight) CLASS DPEDIT

   LOCAL aData:={},nNumMemo:=0,cFileBmp:="bitmap.bmp"
   LOCAL oEdit:=Self

   DEFAULT cTitle:="Asociar Imagen" // ,cFileBmp:="bitmap.bmp"

   IF !::IsDef(cField) // Campo no Existe
     MensajeErr("Campo "+cField+" No Existe")
     RETURN .F.
   ENDIF

   // cFileBmp:=::Get(cField)
   //? cFILEBMP,FILE(cFileBmp)

   cTitle:=GetFromVar(cTitle)

   ::EditButtons(cTitle , cFileBmp ,{|cFile|KillGetCol(::oBody),;
                                      cFileBmp:=EJECUTAR("FILEBMP",oEdit,cField,cFileBmp,cTitle,nTop,nLeft,nWidth,nHeight),;
                                      oEdit:SET(cField,cFileBmp)})

RETURN NIL

/*
// Asigna Valor y Refresca Objeto
*/
METHOD SetAdjuntos(cField,cTitle,cTabla,lRun) CLASS DPEDIT
   LOCAL aData:={},nNumMemo:=0,cFileBmp:="adjuntar.bmp",oFrm:=SELF

   DEFAULT cTitle:="Asociar Archivos Adjuntos",;
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

   ::EditButtons(cTitle , cFileBmp ,{||::nFileMain:=EJECUTAR("DPFILEEMPMAIN",::nFileMain,cTitle,::cTable,(::nOption=0 .OR. ::nOption=2),nil,nil,nil,oFrm)})

RETURN NIL

/*
// Borra un Nuevo Registro
*/
METHOD DelRecord(cWhere,lIntRef) CLASS DPEDIT

   LOCAL lResp:=.T.
   LOCAL cKey :=::oTable:cPrimary
   LOCAL uNext:=::oTable:GetNext()
   LOCAL uKey :=::oTable:GetDataKey(NIL,cKey)

   // ErrorSys(.T.)
   // ? ::oTable:cSql
   // ? ::oTable:ClassName()

   IF EMPTY(uNext)
      uNext:=::oTable:GetPrev()
   ENDIF

   IF EMPTY(uNext)
      uNext:=::oTable:GetNext()
   ENDIF

   DEFAULT cWhere:=" WHERE "+cKey+GetWhere("=",::Get(cKey))

   EJECUTAR("DPAUDELIMOD",Self)

   IF ::oTable:Delete(cWhere,.T.,.T.) // Borra el Registro

     // ? "AQUI DICE QUE SI ELIMINA",::cTable,uKey
     AUDITAR("DELI" , NIL ,::cTable , uKey , NIL, SELF )

   ELSE

     // lResp:=.F.
     RETURN .F.

   ENDIF

   IF EMPTY(uNext)

      IF !MacroEje([ISTABINC("]+::cTable+[")])

         MensajeErr("Tabla: "+GetTableName(::cTable)," No Tiene Permiso para Agregar Registros")
         ::lCancel:=.T.
         ::Close()

      ELSE

         ::lCancel   :=.F.      // Cancelación Solicitada Afirmativa
         ::lChange   :=.T.
         ::lFromValid:=.F.
         ::lFirst    :=.T.

         ::Load(1)

      ENDIF

   ELSE

      ::oTable:cSql:="SELECT * FROM "+::oTable:cTable+" WHERE "+BuildConcat(::oTable:cPrimary)+;
                               GetWhere("=",uNext) + " LIMIT 1"

//    ::oTable:cSql:=BuildLimit(::oTable:cSql,oDp:cTypeBD)

      ::oTable:Reload()
      ::Load(0)

   ENDIF

RETURN lResp

/*
// Asigna Valor y Refresca Objeto
*/
METHOD PutVar(cVarName,nValue,oSay) CLASS DPEDIT
    LOCAL cObj

    cVarName:=UPPE(ALLTRIM(cVarName))

    IF ::IsDef(cVarName)
      ::Set(cVarName,nValue)
    ENDIF

    cObj:=[O]+cVarName

    IF oSay=NIL .AND. ::IsDef(cObj)
       oSay:=::Get(cObj)
    ENDIF

    IF ValType(oSay)="O" .AND. !"TCHE"$oSay:ClassName()
      IF ValType(nValue)="C"
        oSay:SetText(nValue)
      ELSE
        EVAL(oSay:bSetget,nValue)
      ENDIF
      // oSay:VarPut( nValue,.T.)
      oSay:Refresh(.T.)
    ENDIF

RETURN NIL

/*
// Construye el Where del Registro
*/
METHOD GetWhere(cFields,lPrimary) CLASS DPEDIT
   LOCAL cWhere:="",I,aVar

   DEFAULT cFields:=::cPrimary  ,lPrimary:=.F.

   aVar  :=_VECTOR(STRTRAN(cFields,"+",","))
   cWhere:=""

   FOR I := 1 TO LEN(aVar)

     cWhere:=cWhere+;
             IIF( Empty(cWhere) , "" , " AND " )+;
             aVar[I]+GetWhere("=",::Get(aVar[I]))

   NEXT I

   cWhere:= cWhere +;
            IF( !Empty(::cScopeF)," AND "+::cScopeF , "")

RETURN " WHERE "+cWhere


/*
// Mantiene la Repetición de Campos
*/
METHOD RepeatGet(aFields,aExcluye) CLASS DPEDIT
    LOCAL uValue,oGet,cVar,I,cGet,nAt

    DEFAULT aFields:={}

    IF ValType(::oTable)!="O" .OR. ::oTable:RecCount()=0
       RETURN .T.
    ENDIF

    IF Empty(::aRepeatGet)

      IF Empty(aFields)
        ::aRepeatGet:={}
        AEVAL(::oTable:aFields,{|a,i|AADD(::aRepeatGet,a[1])})
      ENDIF

      DEFAULT aExcluye:={}

      aExcluye     :=IIF( ValType(aExcluye)="C" , _VECTOR(aExcluye) , aExcluye)
      ::aRepeatGets:=IIF( ValType(aFields) ="C" , _VECTOR(aFields ) , aFields )

      AEVAL(aExcluye    ,{|a,i|  aExcluye[i]  :=UPPE(a)})
      AEVAL(::aRepeatGet,{|a,i|::aRepeatGet[i]:=UPPE(a)})

      FOR I := 1 TO LEN(aExcluye)

         nAt:=ASCAN(::aRepeatGet,aExcluye[I])
         IF nAt>0
            ADEL(::aRepeatGet,nAt)
            ASIZE(::aRepeatGet,LEN(::aRepeatGet)-1)
         ENDIF
      NEXT

   ENDIF

   // Ahora hace el proceso de Repetición de Gets
   FOR I := 1 TO LEN(::aRepeatGet)
      cVar:=UPPE(::aRepeatGet[I])
      cGet:="O"+cVar

      IF ::ISDEF(cVar) // Busca Primero en las Variables

         uValue:=oTable:Get(cVar)

         // ? "aqui se repiTE",cVar,uValue

         ::Set(cVar,uValue) // Lo Asigna en la Variable

         IF ::ISDEF(cGet)
             oGet:=::Get(cGet)
             // ? oGet:ClassName()
             // oGet:SetText(uValue,.T.)
             IIF( ValType(oGet)="O", oGet:VarPut(uValue,.T.) , NIL)

         ENDIF

      ENDIF

   NEXT

   STORE NIL TO oGet

RETURN .T.

/*
// Carga Datos para el Formulario
*/
METHOD List(cWhere,cTitle) CLASS DPEDIT
     LOCAL lResp,uValue,uValueIni,cSql

     uValue   :=::oTable:GetValue(::oTable:cPrimary)
     uValueIni:=uValue

     DEFAULT ::cListWhere:=::cScope,;
             cWhere      :=::cListWhere

     IF ValType(::cList)="U"

       MsgInfo("Hay que Desarrollar List/Paginado")

     ELSE

       DO CASE

          CASE ValType(::cList)="C" .AND. ".BRW"$UPPE(::cList)

//           MensajeErr(::cListWhere,"DESDE TDEPEDIT")

              SETDBSERVER(::oDb)

              //lResp:=DPBRWPAG(::cList,NIL,@uValue,::oTable:cPrimary,.T.,::cListWhere,::cListTitle,::oDb)
              lResp:=DPBRWPAG(::cList,NIL,@uValue,::oTable:cPrimary,.T.,cWhere,::cListTitle,::oDb)

              SETDBSERVER()

              // oDp:cSqlBrw:=oDp:cSql

              IF uValueIni!=uValue .AND. ValType(uValueIni)==ValType(uValue)// Seleccionó el Registro

                ::oTable:cSql:="SELECT * FROM "+::oTable:cTable+" WHERE "+BuildConcat(::oTable:cPrimary)+;
                               GetWhere("=",uValue) + " LIMIT 1"

                // ::oTable:cSql:=BuildLimit(::oTable:cSql,oDp:cTypeBD)

                ::oTable:Reload()

                // ? ::oTable:cSql

                ::Load(0)

//                IF DpVersion()>=4
                   EJECUTAR("DPSAVEFIND",::oTable:cTable,::oTable:cPrimary,uValue)
//                ENDIF

              ENDIF


          CASE  ValType(::cList)="C"

             EJECUTAR(::cList,::oTable,SELF)
             ::Load(0)

          CASE ValType(::cList)="B"

             EVAL(::cList,::oTable    )

       ENDCASE

     ENDIF

RETURN NIL

/*
// Impresión
*/
METHOD RUNPRINT() CLASS DPEDIT

     //MsgInfo("Hay que Desarrollar PRINT")
     IF ValType(::cPrint)="U"

       MsgInfo("Hay que Desarrollar Print")

     ELSE

       ::RunScr(::cPrint) // Ejecutar ImpresióN
/*
       IIF( ValType(::cPrint)="C",EJECUTAR(::cPrint,::oTable ) , NIL )
       IIF( ValType(::cPrint)="B",EVAL(::cPrint    ,::oTable ) , NIL )
*/
     ENDIF

RETURN NIL

/*
// Carga Datos para el Formulario
*/
METHOD Load(nOption,nSkip) CLASS DPEDIT
    LOCAL nOption_:=::nOption,cAction:="",nArrayAt:=0,I,lRet
    LOCAL nRowSel :=0,cKey,uKey

    DEFAULT nSkip:=0

    ::DestroyMsg()
    ::nOption   :=nOption
    ::aDataTable:={} // Utilizada por scrollget para repetir campos

    DEFAULT ::nOption:=0 // Visualizar 03/01/2021

    IF ::oTable:RecCount()>0

      ::oTable:Reload()

// IF ::lCancel
// ? "CANCELADO"
  // ::oTable:Browse()
// ENDIF

      ::cWhere    :=GetSqlWhere(::oTable:cSql)
      ::aDataTable:=ACLONE(::otable:aDataFill)

// ::oTable:Browse()
// ? CLPCOPY(::oTable:cSql)

    ELSEIF ::nOption!=1
       ::oWnd:SetText(cAction+::cTitle)
       ::nOption:=0
       RETURN .T.
    ENDIF

// ? "aqui es load"

    ::CreateVars(,,,.F.)

    ::UpDateFromTable() // 14/02/2021


    IF ::nRecno<>::nRecnoOld .AND. ::nOption=0
//? ::nRecno,::nRecnoOld,"Cambia el registro"
       ::nRecnoOld:=::nRecno
       // AEVAL(::aScrollGets,{|o|o:UpdateFromForm(),o:SetEdit(.F.,::nOption)})
       // AEVAL(::aScrollGets,{|o|o:SetEdit(.F.,::nOption)}) // 14/02/2021
       // AEVAL(::aScrollGets,{|o|o:SetEdit(.F.,::nOption)}) // 14/02/2021
    ENDIF

    // AEVAL(::aScrollGets,{|o|o:SetEdit(.F.,::nOption)}) // 14/02/2021
    // IF ::nOption=0
    // ? ::nOption,"::nOption"

    // 14/02/2021 ::UpDateFromTable()
    // ENDIF

    IF ::nOption=2 .AND. !EMPTY(::cView) // Programa de Consulta

       cKey :=::oTable:cPrimary
       uKey :=::oTable:GetDataKey(NIL,cKey)

       AUDITAR("DCON" , NIL ,::cTable , uKey , NIL, SELF )

       IF ValType(::cView)="C" .AND. "("$::cView // Function
//          ::RunScr(::cView)  19/08/2013
          ::RunScript(::cView)
       ELSEIF ValType(::cView)="C"  // Programa DpXbase
          ::nOption:=0
          RETURN EJECUTAR(::cView,Self)
       ENDIF

       IF ValType(::cView)="B"
          ::nOption:=0
          RETURN EVAL(::cView,Self)
       ENDIF

    ENDIF

    IF ::nOption=1 // 25/12/2020 .OR. ::nOption=3
       ::CreateVars(,,,.F.)
// ? "DEBE VACIAR LAS VARIABLES"
       ::SetDefValue() // Asigna los Valores por Defecto
    ENDIF

    // 03/01/2021, sus valores cambian cuando se modifica
    IF ::nOption=3
      ::UpDateFromTable(.F.)
//? "MODIFICAR",GETPROCE()
    ENDIF

//  AEVAL(::aScrollGets,{|o|o:UpdateFromForm(), IIF( ::nOption=1 .OR. ::nOption=3 ,o:SetEdit(.T.,::nOption) , o:SetEdit(.F.,::nOption))})
/* 21/02/2021
IF ::nOption=1  .OR. ::nOption=3 .OR. ::nOption=0
    // 03/01/2021, cambia los valores.
    // AEVAL(::aScrollGets,{|o|o:UpdateFromForm(),o:SetEdit(.T.,::nOption)})
    AEVAL(::aScrollGets,{|o|o:SetEdit(.T.,::nOption)})
? "aqui es modificar o Incluir"
ENDIF
*/
    // 05/07/2018 , Si no devuelve Logico=Error
    lRet=::RunScr(::cLoad)

    //lRet:=CTOO(lRet,"L")

    IF ValType(lRet)<>"L"
       lRet:=.F.
    ENDIF

    IF ValType(::cLoad)="C" .AND. !lRet // ::RunScr(::cLoad)  // programa que ejecuta la Cancelación

       ::nOption:=nOption_
       ::CreateVars(,,,.F.) //  ::oTable ) // Regenera las Variables para seguir Incluyendo

    ENDIF

    // JN 08/12/2018, Debe Asignar los Valores por Defecto; CreateVars los vaciaba al incluir

//  25/12/2020
//  IF ::nOption=1 .OR. ::nOption=3

    IF ::nOption=1
       ::SetDefValue() // Asigna los Valores por Defecto
    ENDIF

    // 03/01/2021, modificar cambia el valor
// IF ::nOption=3
// ? "AQUI UNICO LUGAR DONDE EDITA"

       AEVAL(::aScrollGets,{|o| IIF( ::nOption=1 .OR. ::nOption=3 ,o:SetEdit(.T.,::nOption) , o:SetEdit(.F.,::nOption))})

//    ELSE
//      AEVAL(::aScrollGets,{|o| o:UpdateFromForm(), IIF( ::nOption=1 .OR. ::nOption=3 ,o:SetEdit(.T.,::nOption) , o:SetEdit(.F.,::nOption))})
//    ENDIF


    IF ::oScript:IsFunction(::cAfterLoad)
       ::RunScr(::cAfterLoad)
    ENDIF


    ::RECCOUNT(.F.) // 10/10/2016
    ::CheckBtn()

    IF ::nOption=1 .AND. ValType(::oIntRef)="O" // Incluir no la Necesita
       ::oIntRef:End()
       ::oIntRef:=NIL
    ENDIF

    IF IsIntRef(::oTable:cTable) .AND. ::nOption!=1.AND."WHERE"$::oTable:cSql
       IF ValType(::oIntRef)="O"
         ::oIntRef:End()
         ::oIntRef:=NIL
       ENDIF
       ::oIntRef:=IntRef(::oTable) // Solo Para Modificar
    ENDIF

// IF !EMPTY(::oBody) .AND. ValType(::oBody:oBrw)="O"
// ENDIF

    ::BuildLink() // Repinta los relacionados

    cAction:=IIF( ::nOption=1,oDp:cInc   , cAction )
    cAction:=IIF( ::nOption=3,oDp:cMod   , cAction )
    cAction:=IIF( ::nOption=4,oDp:cBus   , cAction )

    ::cSingular:=IIF( Empty(::cSingular) , ::cTitle , ::cSingular )

    // ? ::nOption,"::nOption",oDp:cBus,oDp:IsDef("_NMTRABAJADOR"),GetFromVar("oDp:_NMTRABAJADOR")
    IF ::nOption=0
      ::oWnd:SetText(::cTitle) // ::cTitle)
    ELSE
      ::oWnd:SetText(cAction+::cSingular) // ::cTitle)
    ENDIF

    // Campo Memo
    IF !EMPTY(::aMemo)
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

    IF ValType(::oDlg)="O" .AND. ::nOption=1 .AND. ::nMode=1 .and. !Empty(::oDlg:aControls)

       FOR I=1 TO LEN(::oDlg:aControls)
          IF ::oDlg:aControls[I]:ClassName()="TFOLDER"
             AEVAL(::oDlg:aControls[I]:aDialogs,{|oDlg,n|SetFocusGet(oDlg,NIL,.T.,NIL)})
          ENDIF
       NEXT I

    ENDIF

    SetFocusGet(::oDlg,NIL,.T.,::oFocus) // APunta al primer Get

    IF ::nOption=0 .AND. ValType(::oDlg:oBar)="O"
       I:=ASCAN(::oDlg:aControls,{|o,n|o:ClassName()="TXBROWSE"})
       DpFocus(IF( I>0,::oDlg:aControls[I], ::oDlg )) // :aControls[1])
    ELSE
       ::oControl:=::oFocus
      // DpFocus(::oFocus)
    ENDIF

    IF ::nOption=2
       ::nOption:=0
    ENDIF

RETURN .T.


/*
// Revisa las Posiciones de los Botones
*/
METHOD CheckBtn() CLASS DPEDIT
    LOCAL I,nStep:=3,nWidth,nHeight,aHide:={},aShow:={},nLin:=30

   // ::oWnd:oBar:Hide()

    nWidth :=::aButtons[1,1]:nWidth
    nHeight:=::aButtons[1,1]:nHeight

    AEVAL(::aButtons,{|a,n| a[1]:Hide() }) // Apaga todos los Botones

    FOR I := 1 TO LEN(::aButtons)

      IF "Cancel"$::aButtons[I,3]
         ::aButtons[1,1]:lCancel:=.T.
//         EJECUTAR("INSPECT",::aButtons[1,1])
      ENDIF

      // ::aButtons[I,1]:bWhen:={||.T.}
      // ::aButtons[I,1]:Hide()
      /* IF ValType(::aButtons[I,6])="B" .AND. !EVAL(::aButtons[I,6])
        ::aButtons[I,1]:Disable()
        ::aButtons[I,1]:ForWhen()
      ELSEIF !::aButtons[I,1]:lActive // := .f
        ::aButtons[I,1]:Enable()
        ::aButtons[I,1]:ForWhen()
      ENDIF */
      // ? ValType(::aButtons[I,1]:bWhen)

      IF !EVAL(::aButtons[I,1]:bWhen) // ((::nOption=1 .OR. ::nOption=3) .AND. ::aButtons[I,6]) .OR. ;
          // (::nOption=0 .AND. !::aButtons[I,6])
         AADD(aHide,I)
         //::aButtons[I,1]:Hide()
      ELSE
         ::aButtons[I,1]:Enable()
         ::aButtons[I,1]:Move(3,nStep)
         ::aButtons[I,1]:SetSize(nWidth,nHeight)
         AADD(aShow,I)
         // ::aButtons[I,1]:Show()
         nStep:=nStep+(nWidth)+1

         IF ValType(::aButtons[I,6])="B" .AND. !EVAL(::aButtons[I,6]) // Inactiva
           ::aButtons[I,1]:Disable()
         ENDIF

      ENDIF
/*      IF !EVAL(oBody:aBtnsWhen[I])
         oBody:aBtns[I]:Hide()
      ELSE
         oBody:aBtns[I]:Move(oBrw:nTop+oBrw:nHeight+5,oBrw:nLeft+nStep)
         oBody:aBtns[i]:SetSize(35,35)
         nStep:=nStep+(35)+1
         oBody:aBtns[I]:Show()
      ENDIF*/
   NEXT

   AEVAL(aHide,{|n|::aButtons[n,1]:Hide()})
   AEVAL(aShow,{|n|::aButtons[n,1]:Show(),nLin:=nLin+::aButtons[n,1]:nWidth()})

   // ::oWnd:oBar:Show()
   // ::oWnd:oBar:Refresh()
   // ::oWnd:o
   // ? ::aButtons[1,1]:oBar:ClassName()
   // EVAL(::oDlg:bGotFocus)
   // AEVAL(::aButtons,{|aButtons,i|aButtons[1]:ForWhen()})

   IF ::nOption=0
      ::lCancel:=.T. // Puede Salir
   ENDIF

   // Reposiciona la Posición de Registros
   // 19/09/2016 no es necesario reubiscarse
   IF ValType(::oSayRecord)="O" .AND. (::nOption=0) .AND. nLin>10
      ::oSayRecord:Move(0,nLin)
      ::oSayRecord:Show()
   ENDIF

   IIF(lMdi,DpFocus(::oDlg),NIL)

RETURN .T.

/*
// Ejecutar Botones de Tipo Documento
*/
METHOD RUNBTN(nPos,lValid) CLASS DPEDIT
   LOCAL lResp:=.F. ,I
   PRIVATE oDpEdit:=SELF

   PUBLICO(::cVarPublic,SELF)

   SETDBSERVER(::oDb)

   IF lValid
      lResp:=EVAL(BLOQUECOD(::aButtons[nPos,4]))
   ELSE
      lResp:=EVAL(BLOQUECOD(::aButtons[nPos,5]))
   ENDIF

   SETDBSERVER(NIL)

RETURN lResp

/*
// Cierra los Editores Asociados
*/
METHOD CloseAllEdit() CLASS DPEDIT
   LOCAL lResp:=.T.,oWnd,I,oEdit

   FOR I=1 TO LEN(::aDpEdit)
      oEdit:=::aDpEdit[I]
      IF (ValType(::aDpEdit[I])="O".AND.::aDpEdit[I]:nNumEdit>0)
         oWnd:=oEdit:oWnd
         IIF( ValType(oWnd)="O" , (oWnd:Restore(),oWnd:End()) , NIL)
      ENDIF
   NEXT I

   AEVAL(::aDpEdit,{|a,n|::KillEdit(a)})

RETURN lResp


METHOD CreateWindow(cIcon,nTop,nLeft,nRight,nBottom) CLASS DPEDIT
   // LOCAL cFileRtf
   LOCAL cMemo
   LOCAL oDpEdit     :=SELF
   LOCAL oDlg
   LOCAL oIco
   LOCAL aPutControls:={}

   LOCAL nClrPane:=::nClrPane,nClrText,I,U

   DEFAULT nTop     :=5,;
           nLeft    :=5,;
           nRight   :=400,;
           nBottom  :=600

   IF ValType(::oDlg)="O" .OR. !::lWnd
      RETURN NIL
   ENDIF

   oDp:aFonts:={}
   oDp:aPutControls:={}

   // oDp:Set("aFonts",{})
   // oDp:Set("aPutControls",{})

   //? ::cFile,FILE(::cFile)
   // JN 18/01/2014
   IF !Empty(::cFile) .AND. !FILE(::cFile) .AND. !Empty(oDp:cFileSource) .AND. FILE(oDp:cFileSource+::cFile)
     COPY FILE (oDp:cFileSource+::cFile) TO (::cFile)
     EJECUTAR("DPSCRIMPORT",::cFile)
   ENDIF

   oDp:cHelpRtf:=""

   IF !ISDPSTD() .AND. !Empty(::cFile) .AND. !FILE(::cFile) .AND. ::lFileEdt
      EJECUTAR("DPDIRAPLDOWN",cFileNoPath(::cFile),cFilePath(::cFile),.F.,oDp:lServerDown)
   ENDIF

   IF !ISDPSTD() .AND. !Empty(::cFile) .AND. !FILE(::cFile) .AND. ::lFileEdt
     EJECUTAR("DPREGSOPORTEAUTO","EDT",::cFile,"Archivo "+cFileNoPath(cFileNoExt(::cFile))+" no Existe")
   ENDIF

   IF !Empty(::cFile) .AND. (File(::cFile) .OR. ISDPSTD())

//? ::cFile,"::cFile EN TDPDIT()",cMemo

      cMemo:=IF(ISDPSTD(),GETFILESTD(::cFile,.T.),MemoRead(::cFile))
      cMemo:=IF(Empty(cMemo) .OR. ValType(cMemo)<>"C","",cMemo)
      aPutControls:=StrTran(cMemo,CHR(13))

//      aPutControls:=StrTran(IF(ISDPSTD(),GETFILESTD(::cFile),MemoRead(::cFile)),CHR(13))
      aPutControls:=_VECTOR(aPutControls,CHR(10))

      FOR i := 1 TO len(aPutControls)

        aPutControls[i]:=_Vector(aPutControls[i],"|")

        IF LEFT(aPutControls[I,1],4)="FONT" // .OR.aPutControls[I,1]="TSBUTTON"
           LOOP
        ENDIF

        IF aPutControls[I,1]="FILERTF"
           ::cFileRtf:=aPutControls[i,2]
//         oDp:cHelpRtf:=::cFileRtf
           LOOP
        ENDIF

        IF LEN(aPutControls[I])<10
//? "FALTA ",LEN(aPutControls[I]),ViewArray(aPutControls[u])
           AEVAL(ARRAY(11-LEN(aPutControls[I])),{|a,n| AADD(aPutControls[I],"0")})
        ENDIF

        FOR u := 2 TO 10

           ///IF LEN(aPutControls)>=u
             aPutControls[i,u]:=VAL(aPutControls[i,u])
           //ENDIF

        NEXT u

      NEXT

      IF LEN(aPutControls)>0
       nTop    :=MAX(aPutControls[1,_NTOP    ],1)
       nLeft   :=MAX(aPutControls[1,_NLEFT   ],1)
       nBottom :=aPutControls[1,_NBOTTOM ]
       nRight  :=aPutControls[1,_NRIGHT  ]
       nClrPane:=aPutControls[1,_NCLRPANE]
       nClrText:=aPutControls[1,_NCLRTEXT]
      ENDIF

      oDp:aPutControls:=aPutControls

      nTop :=MAX(nTop ,0)
      nLeft:=MAX(nLeft,0)

    //  ? nTop,nLeft,nBottom,nRight

  ENDIF

  oDp:cHelpRtf:=::cFileRtf

//DEFINE ICON oIco RESNAME "ICON"

  oIco:= oDp:oIco

  IF lMdi

    IF ::lDlg

      DEFINE WINDOW ::oWnd MDICHILD TITLE "Cargando... "+::cTitle ; // FROM nTop,nLeft TO nRight,nBottom; //
             FROM nTop,nLeft TO 1,180-180;
             NOMAXIMIZE PIXEL OF DpWndMain();
             COLOR nClrText,nClrPane
             // FONT oDp:oFontMenu
             // COLOR  0 , GetSysColor( COLOR_BTNFACE ) //,; // CLR_LIGHTGRAY,;

      ::oWnd:cCaption:=::cTitle

      DEFINE TIMER ::oTmr INTERVAL 500 OF ::oWnd ACTION {||NIL}
      ACTIVATE TIMER ::oTmr

      ::oWnd:SetIcon(oIco)
      SendMessage( ::oWnd:hWnd, 128, 1, oIco:hICON )

      IF !Empty(::cFileBmp) .AND. FILE("BITMAPS\"+::cFileBmp)

        DEFINE BRUSH ::oBrush;
               FILE "BITMAPS\"+::cFileBmp

      ENDIF

      DEFINE DIALOG ::oDlg;
             FROM 0,0 TO nRight,nBottom;
             BRUSH ::oBrush;
             COLOR nClrText,nClrPane;
             STYLE WS_CHILD OF ::oWnd PIXEL

      ::oDlg:cCaption:=::cTitle


      // No existe Color Fondo Impar
      IF !__objHasMsg(::oDlg, "lClrPaneDp" )
        __objAddData(::oDlg,"lClrPaneDp")
        ::oDlg:lClrPaneDp:=.F.
      ENDIF

      IF ::lVScroll

        DEFINE SCROLLBAR ::oDlg:oVScroll VERTICAL OF ::oWnd

        ::oWnd:oVScroll:=::oDlg:oVScroll

      Endif

      IF ::lHScroll

        DEFINE SCROLLBAR ::oDlg:oHScroll HORIZONTAL OF ::oWnd

        ::oWnd:oHScroll:=::oDlg:oHScroll

      Endif



    ELSE

      DEFINE WINDOW ::oWnd MDICHILD TITLE ::cTitle ; // FROM nTop,nLeft TO nBottom,nRight;
             PIXEL OF DpWndMain();
             FROM nTop,nLeft TO 0,0;
             COLOR nClrText,nClrPane

/*;
             COLOR  0 , GetSysColor( COLOR_BTNFACE ) //,; // CLR_LIGHTGRAY,;
*/

      SET MESSAGE OF ::oWnd

      DEFINE TIMER ::oTmr INTERVAL 500 OF ::oWnd ACTION {||NIL}
      ACTIVATE TIMER ::oTmr

      ::oWnd:SetIcon(oIco)
      ::oWnd:Hide()

      ::oDlg:=::oWnd

    ENDIF

  ELSE

    IF !Empty(::cFileBmp)

      DEFINE BRUSH ::oBrush;
             FILE "BITMAPS\"+::cFileBmp

    ENDIF

    nTop:=MIN(130,nTop) // 03/12/2015

    IF ValType(::oDpLbx)="O"
       oWndMain:=::oDpLbx:oBar // 03/12/2015
    ENDIF


    DEFINE DIALOG ::oDlg;
           FROM nTop,nLeft TO nBottom,nRight;
           BRUSH ::oBrush;
           COLOR nClrText,nClrPane PIXEL TITLE ::cTitle OF oWndMain

    ::oDlg:lHelpIcon:=.F.


    IF ::lVScroll
       DEFINE SCROLLBAR ::oDlg:oVScroll VERTICAL OF ::oDlg
    Endif

    IF ::lHScroll .OR. .T.
       DEFINE SCROLLBAR ::oDlg:oHScroll HORIZONTAL OF ::oDlg
    Endif

  ENDIF

  oDlg:=::oDlg
  oDlg:Cargo:={nTop,nLeft,nBottom,nRight}

//? ::oDlg:oHScroll,::oDlg:oHScroll,::oDlg:oHScroll:ClassName(),::oDlg:oHScroll:ClassName()

  //::oDlg:bGotFocus    = { || Publico(::cVarPublic , oDpEdit) }
  //::oDlg:bLostFocus   = { || Publico(::cVarPublic , oDpEdit) }
  //::oWnd:bGotFocus    = { || oDlg:SetFocus() , Publico(::cVarPublic , oDpEdit) }
  //::oWnd:bLostFocus   = { || RestoreEdit(oDpEdit:nNumEdit) }

  ::oDlg:bKeyDown:= { |nKey| oDpEdit:RunKey(nKey) }
  // METHOD KeyChar( nKey, nFlags ) CLASS TDialog

  SetWndDefault( IIF( ::lDlg , ::oDlg , ::oWnd ))

   /* IF ::nType=1 // Tipo Documento
      DEFINE BUTTONBAR 3D SIZE 40,40 OF GetWndDefault()
   ENDIF */

RETURN ::oDlg

/*
// Asigna el tempolizar en el formulario
*/
METHOD   SetEdit(lEdit) CLASS DPEDIT
   LOCAL I,oControl,nControl:=0
RETURN .T.

/*
// Asigna el tempolizar en el formulario
*/
METHOD RunKey(nKey) CLASS DPEDIT
   LOCAL cBtn:="",nBtn:=0

//? "RunKey",Getproce(),::nOption

   IF ::lEscClose .AND. nKey=27 .AND. Empty(::cTable)
      ::Close()
   ENDIF

   IF Empty(::cTable)
      RETURN .T.
   ENDIF

   DO CASE

//      CASE nKey=73 .AND. ::nOption=0 .AND. !Empty(::cTable)
//          cBtn:="NEW"
          // AADD(::aButtons,{NIL,"xNew.bmp"     ,"Incluir  " ,"oDpEdit:Load(1)                         "      ,"oDpEdit:nOption =0"  , {||.T.} ,"NEW"   })

//      CASE nKey=67 .AND. ::nOption=0 .AND. !Empty(::cTable)
//          cBtn:="VIEW"
//    CASE nKey=77 .AND. ::nOption=0 .AND. !Empty(::cTable)
//
//        cBtn:="OPEN"

//    CASE nKey=69 .AND. ::nOption=0 .AND. !Empty(::cTable) // E
//        cBtn:="DELETE"

//      CASE nKey=66 .AND. ::nOption=0 .AND. !Empty(::cTable)  // B
//          cBtn:="FIND"

      CASE nKey=76 .AND. ::nOption=0 .AND. !Empty(::cTable) // L

          cBtn:="LIST"

//      CASE nKey=80 .AND. ::nOption=0 .AND. !Empty(::cTable) // L
//
//          cBtn:="PRINT"

      CASE (nKey=27 .AND. ::nOption=0 .AND. !Empty(::cTable)) // .OR. (nKey=27 .AND. ::lEscClose .AND. :)  // ESC

         ::Close() // ::oWnd:End()

      CASE nKey=27 .AND. (::nOption=1 .OR. ::nOption=3 .OR. ::nOption=4) .AND. !Empty(::cTable)

         cBtn:="CANCEL"

      CASE nKey=VK_F10 .AND. (::nOption=1 .OR. ::nOption=3) .AND. !Empty(::cTable)

         cBtn:="SAVE"

      CASE nKey=VK_F1

//? "AQUI ES F1"

   OTHER

     IF ::nOption=0 .AND. !Empty(::cTable)
        nBtn:=ASCAN(::aButtons,{|a,n|a[8]=nKey})
        IF nBtn>0
           cBtn:=::aButtons[nBtn,7]
        ENDIF
     ENDIF

   END CASE

   IF !Empty(cBtn) .AND. nBtn=0
      //? nKey,cBtn
      nBtn:=ASCAN(::aButtons,{|a,n|a[7]=cBtn})
   ENDIF

   IF nBtn>0 .AND. ValType(::aButtons[nBtn,1])="O"
      EVAL(::aButtons[nBtn,1]:bAction)
   ENDIF

RETURN .T.

/*
// Asigna Icono a la Ventana
*/
METHOD SetIcon(cFile) CLASS DPEDIT
  LOCAL oIco,cFileIco

  IF ValType(::oWnd)!="O"
     RETURN .F.
  ENDIF


  cFileIco:=DPBMP(cFile) // oDp:cPathBitMaps+cFile
  // cFileIco:="..\BITMAPS\"+cFile
  cFileIco:=cFileIco+IIF( "."$cFileIco , "" , ".ICO")

  DEFINE ICON oIco FILENAME (cFileIco)

  ::oWnd:SetIcon(oIco)

//  oIco:End()
  oIco:=NIL

RETURN .T.

/*
   IF lEdit=::lEdit // Son Iguales
     RETURN .T.
   ENDIF

   ::lEdit:=lEdit

   IF !::lEdit
     ::aWhen :={}
     ::aValid:={}
   ENDIF

   FOR I := 1 TO LEN(::oDlg:aControls)
      oControl:=::oDlg:aControls[I]
      IF oControl:ClassName()$"TGET,TBMPGET,TCOMBOBOX,TRADIO,TCHECKBOX"
        IF !::lEdit
          AADD(::aWhen,oControl:bWhen)
          oControl:bWhen:={||.F.}
          oControl:Disable()
        ELSE
          nControl++
          oControl:bWhen:=::aWhen[nControl]
        ENDIF
      ENDIF
   NEXT

RETURN .T.

*/
/*
// Asigna el tempolizar en el formulario
*/
METHOD SetTimer(bAction,nInterval) CLASS DPEDIT // [JN]

   DEFAULT bAction:={||NIL}

   IF nInterval!=NIL
     ::oTmr:End()
     DEFINE TIMER ::oTmr INTERVAL nInterval OF ::oWnd ACTION {||NIL}
     ACTIVATE TIMER ::oTmr
   ENDIF

   ::oTmr:bAction:=bAction

RETURN ::oTmr

/*
// Asigna una Tabla al Formulario y Crea las Variables
*/
METHOD SetTable(oTable,lCreateWindow, oDb) CLASS DPEDIT // [JN]
    LOCAL cDataKey,cWhere,I , cField , uValue

    DEFAULT lCreateWindow:=.T.

    ::lSetDef    :=.T.
    ::lCancel    :=.F.

    IF ValType(oTable)="C" // Indica el Comando

       DEFAULT oDb:=GetOdbc(oTable)

       DEFAULT ::oDb:=oDb // Si no tiene BD Asignada

//MensajeErr(oDb:ClassName(),::oDb:ClassName(),"oDb:ClassName(),::oDb:ClassName())

       oTable:=OpenTable(oTable,.F.,::oDb)

    ELSE

       ::oDb:=oTable:oOdbc // Base de Datos

    ENDIF

//MensajeErr("aqui es SETTABLE"+::oDb:ClassName(),"SETTABLE")
// oTable:Browse()
// Aqui Asigna los Valores por Defecto
// IF oTable:lSetFieldDef
//   EJECUTAR("DPTABLESETDEF",oTable)
// ENDIF

    FOR I=1 TO LEN(oTable:aFields)

      cField:=oTable:FieldName(I)
      uValue:=oTable:FieldGet(I)

      __objAddData( Self, cField)  // oTable:FieldName(I))

      IF oTable:aFields[I,2]="M" .AND. ValType(uValue)="C"
         uValue:=ALLTRIM(uValue)
         oTable:FieldPut(I, uValue)      // ALLTRIM(oTable:FieldGet(I)))
         oTable:aFields[i,3]=len(uValue) // ALLTRIM(oTable:FieldGet(I)))
      ENDIF

      oSend(Self , cField	 , uValue) // oTable:FieldGet(I))
      ::Set(cField,uValue)

    NEXT I

    AEVAL(oTable:aFields,{|a,n| __objAddData( Self, a[1]),;
                                oSend(Self , a[1] , oTable:FieldGet(n)) })

    ::aFieldsTable:=ACLONE(oTable:aFields)

// ViewArray( oTable:aFields ,,,.F.)
// MensajeErr(oTable:cTable,"SetTable()")
// !ISTABINC(::cTable)

    ::cTable:=ALLTRIM(UPPE(oTable:cTable)) // Ya tengo el nombre de la tabla
    ::oTable:=oTable

    // JN 30/12/2014, para que nose guarden valores por defecto en TTABLE
    ::oTable:lSetFieldDef:=.F.

// ::oTable:Browse()
// ? ::oTable:ClassName(),"::ClassName()",ErrorSys(.T.)
// ViewArray(::oTable:aFields,NIL,NIL,.F.)

    ::cTopic   :=::cTable
    // ::cTopic   :=IIF( EMPTY(::cTopic  ) , ::cTable            , ::cTopic   ) // Nombre del Topico
    ::cFileChm :=IIF( EMPTY(::cFileChm) , GetFileChm(::cTable), ::cFileChm ) // Archivo de Ayuda

    IF LEN(::aButtons)>0 // Mapa de Archivos

       ::aButtons[1,6]:=BLOQUECOD("ISTABINC('"+::cTable+"')")
       ::aButtons[2,6]:=BLOQUECOD("ISTABCON('"+::cTable+"')")
       ::aButtons[3,6]:=BLOQUECOD("ISTABMOD('"+::cTable+"')")
       ::aButtons[4,6]:=BLOQUECOD("ISTABELI('"+::cTable+"')")
       ::aButtons[7,6]:=BLOQUECOD("ISTABPRN('"+::cTable+"')")

       AEVAL({1,2,3,4,7},{|a,n| ::aButtons[n,8]:=IF(EVAL(::aButtons[n,6]),::aButtons[n,8],0)})

/*
 // Remueve el Uso de las Teclas, si esta Bloqueda la Opción
       ::aButtons[1,8]:=IF(EVAL(::aButtons[1,6]),::aButtons[1,8],0) // Remueve el Uso de las Teclas, si esta Bloqueda la Opción
       ::aButtons[2,8]:=IF(EVAL(::aButtons[2,6]),::aButtons[2,8],0) // Remueve el Uso de las Teclas, si esta Bloqueda la Opción
       ::aButtons[3,8]:=IF(EVAL(::aButtons[3,6]),::aButtons[3,8],0) // Remueve el Uso de las Teclas, si esta Bloqueda la Opción
       ::aButtons[4,8]:=IF(EVAL(::aButtons[4,6]),::aButtons[4,8],0) // Remueve el Uso de las Teclas, si esta Bloqueda la Opción
       ::aButtons[7,8]:=IF(EVAL(::aButtons[7,6]),::aButtons[7,8],0) // Remueve el Uso de las Teclas, si esta Bloqueda la Opción
*/

    ENDIF

    // ? ::oTable:cSql,ValType(::oTable:aFields)
    // ? "SETTABLE",::nMode

//    MensajeErr(::nMode,  "LEN"+LSTR(LEN(::oTable:aDataFill))) // Debe Buscar el Registro Mayor

    IF ::nMode=1 .AND. LEN(::oTable:aDataFill)=0 // :oTable:RecCount()=0 // Debe Buscar el Registro Mayor

      ::oTable:GotoMax(NIL,::cScope,NIL)
      ::cWhere:=GetSqlWhere(::oTable:cSql)
      ::lEof:=.T.
      ::lBof:=!::lEof

    ENDIF

   ::CreateVars(NIL,NIL,NIL,NIL,lCreateWindow) // Crea las Variables Según la tabla

   IF EMPTY(::oTable:cPrimary) //.AND.!EMPTY(::oTable:IndexKey(1))
      ::oTable:cPrimary:=::oTable:FieldName(1) // IndexKey(1)
   ENDIF


   IF !EMPTY(::oTable:cPrimary)  .AND. !(::nOption=1.OR.::nOption=0)
     ::cDataKey:=::oTable:GetDataKey(NIL,::oTable:cPrimary)
   // ELSEIF !(::nOption!=1
   //  ::cDataKey:=::oTable:GetDataKey(1) // Busca según el Primer Indice
  ENDIF

//  ? "HASTA AQUI OK"
  ::lFirst:=.F.
  // ? ::oTable:cPrimary

  IF EMPTY(::oTable:GetValkey(::oTable:cPrimary))
     ::lFirst :=.T. // Es el Primer Registro
     ::nOption:=1
     ::CreateVars(NIL,NIL,NIL,NIL,lCreateWindow) // Crea las Variables Según la tabla
     //  ::LOAD(1)
  ENDIF

  IF IsIntRef(::oTable:cTable) .AND. ::nOption!=1.AND."WHERE"$::oTable:cSql .AND. ::nOption!=0// Necesario para Update/Commit
     ::cWhere :=GetSqlWhere(::oTable:cSql)
     // ::cWhere :=::oTable:cSql
     // ::cWhere :=Subs(::cWhere,AT("WHERE",::cWhere),LEN(::cWhere))
     ::oIntRef:=IntRef(::oTable) // Solo Para Modificar
  ENDIF

  EJECUTAR("LOADLINKINSERT",::oTable:cTable) // JN 4/2/2017

  oTable:End()

RETURN .T.

/*
// Define Scroll para Campos Adicionales
*/
METHOD SCROLLFIELD(cTitle) CLASS DPEDIT

   ::EditButtons(cTitle,"addrec.bmp"  ,{||KillGetCol(::oBody),;
                                        ::oEditScr:=ScrollField(Self,cTitle),;
                                        ::LinkEdit(::oEditScr) })

RETURN .T.
/*
// Asigan el ScrollGet hacia el Formulario
*/
METHOD SCROLLGET(cTable,cFileScg,cExcluye,oWnd,nType,oFont,nStyle) CLASS DPEDIT // [JN]

   LOCAL oForm:=Self
   LOCAL oScroll

// ? "SCROLLGET"

   DEFAULT cTable  :=::cTable
   DEFAULT cFileScg:=cFileName(::cFileEdit)
   DEFAULT oWnd    :=GetWndDefault() //::oDlg
   DEFAULT nType   :=1 // ScrollGet

   ::CreateWindow()

   // ? "SCROLLGET",cTable,cFileScg,oForm:nOption
   // ? nStyle,ValType(nStyle),"nStyle"

   oScroll:=SCROLLGET(cTable,cFileScg,cExcluye,oWnd,nType,oForm,oFont,nStyle)

   AADD(::aScrollGets,oScroll)

   oScroll:oForm:=Self  // Asocia ScrollGet con el Formulario

   IF oForm:nOption=0 // No puede Modificar
     oScroll:SetEdit(.F.,0)
   ENDIF

RETURN  oScroll

//
// Inicia los Procesos del Formulario
//
//THOD InitBody() CLASS DPEDIT // [JN]
/*
   // ::aCopyEdit:=aClone( ::aVars )

   IF Valtype(::oBody)!="O" // No Hay Cuerpo
      RETURN .T.
   ENDIF
   // ? ::cScript,"INITBODY"
   IF !EMPTY(::oBody:cScript)
      // Hay que Compilar este Programa
      IF !EMPTY(::oBody:cBegin)
        ::oBody:RunScr(::oBody:cBegin)  // Inicia Nuevamente la Carga de Datos
      ENDIF
   ENDIF

   // ? ::cScript,"INITBODY, AL FINAL"
*/
//TURN .T.

//
// Permite Activar el Formulario
//
METHOD Activate(bInit,bValid,lErrorSys) CLASS DPEDIT // [JN]

  LOCAL cVarPublic:=::cVarPublic,lSalir:=.f.,I,cName,oMenu,oEdit:=SELF // nMode:=::nMode
  LOCAL cDo
  LOCAL oWnd   :=::oWnd
  LOCAL oDlg   :=::oDlg
  LOCAL nTop
  LOCAL nLeft
  LOCAL nBottom
  LOCAL nRight
  // DEFAULT bInit  :={||.T.}
  LOCAL nNumEdit,aPoint


  ::lMdi:=lMdi


 DEFAULT lErrorSys:=.F.


  // Evitar que la Ayuda lo Llame
  // oWnd:nHelpId:=0
  // oDlg:nHelpId:=0
IF lErrorSys
   ErrorSys(.T.)
ENDIF

  IF EMPTY(oDlg) .AND. ::lWnd .AND. .F.
? "AQUI CREA LA VENTANA CUANDO NO ESTA DEFINIDA"
     ::CreateWindow()
  ENDIF

  IF ValType(::oScript)="O"

//    IF ValType(::oScript)!="O"
//       ::SetScript()
//    ENDIF
//    ? ::cScript,::oScript:ClassName()
    ::cScript:=::oScript:cProgram

  ENDIF

  // No requiere Ventana
  IF !::lWnd
      RETURN NIL
  ENDIF


  oDp:cHelpTopic:=::cTopic
  oDp:cFileChm  :=::cFileChm

  // Tamaño de las Columnas
/*
  IF ValType(::oBody)="O" .AND. ::oBody:nMaxCols>0
    FOR I := ::oBody:nMaxCols+1 TO  LEN(::oBody:oBrw:aCols)
      ::oBody:oBrw:DelCol(I)
    NEXT
  ENDIF

  IF ValType(::oBody)="O" // .AND. ::oBody:nMaxCols>0
    ::oBody:AddButtons("xCancel2.bmp"     ,"Cancelar       "  ,{||::oBody:lCancel:=.T.,::oBody:CANCEL(),::oBody:lCancel:=.F.},;
                                                           {||(len(::oBody:oBrw:aArrayData)>1 .AND. ::oBody:nOption!=0) .OR. ::oBody:nOption=3})
    IF !EMPTY(::oBody:oTable:RecCount())
       ::oBody:oBrw:aArrayData:=::oBody:BuildArray()
    ENDIF

  ENDIF
*/
  DEFAULT ::aCopyEdit:=aClone( ::aVars )
  DEFAULT ::cSingular:=::cTitle

  oWnd   :=::oWnd
  oDlg   :=::oDlg
  nTop   :=MAX(::oDlg:Cargo[1],0)
  nLeft  :=MAX(::oDlg:Cargo[2],0)
  nBottom:=::oDlg:Cargo[3]
  nRight :=::oDlg:Cargo[4]

  IF ::nMode=1 .AND. ::lFirst // Es el Primer Registro
    DEFAULT bInit :={||oEdit:Load(1)}
  ENDIF

  DEFAULT bInit :={||.T.}
  DEFAULT bValid:={||oEdit:ONCLOSE(),oEdit:Cancel(.T.,.T.)} // oEdit:Cancel(NIL,.T.)}

  oDp:aId:={}        // inicia el Control del Id
  AutoId(::oDlg)
  // cDo:="AutoId"
  // DO &cDo With ::oDlg
  // AutoId(::oDlg) // AutoIdentifica los Controles

  ::lSalir:=.F.
  ::lValid:=.F.

  // ? "ACTIVATE"
  // Debe Evaluar los ComboBox

  AEVAL(::oDlg:aControls,{|a,i| IIF( "TCOMBO"$a:ClassName() , ComboIni(a)  , nil  )})

  ::aControls:=ACLONE(::oDlg:aControls)

  ActivateKey( ::aSetKey , ::oDlg )


  // JN 07/11/2016
  // 22/11/2016 ::aBrwFocus:=EJECUTAR("FRMGETBRW",Self)

  // FRMBRWLOSTFOCUS(Self) // Asigna bLostFocus a los Formularios XBROWSE

  IF lMdi

    ::oWnd:SetText(::cTitle)

//? "aqui es MDI",::lDlg,getproce()

    IF ::lDlg

       ACTIVATE DIALOG ::oDlg NOWAIT  ;
                ON INIT (oDlg:Move(0,0),;
                         oEdit:lActivated:=.T.,;
                         IIF( oEdit:lMsgBar ,  oDlg:oMsgBar := TMsgBar():New( oDlg, "", .F., .F., .F., .F.,,,, ),NIL),;
                         PutBar(oEdit),;
                         IIF( oEdit:lMsgBar .OR. !Empty(oEdit:aMsgItem) , oDlg:oMsgBar:SetColor(oDlg:nClrText,oDlg:nClrPane) , NIL ),;
                         oEdit:SETMSGBAR_ON(),;
                         PutControls(oDlg,.t.,nil,nil,oEdit:lDesign,oEdit),AjustButtons(oEdit),; // ,_oDlg:nLastKey:=9);
                         EVAL(bInit),eval(oEdit:bInit),oEdit:bInit:={||.T.},;
                         IF(Empty(oEdit:aScrollSize),NIL, TScrWnd():New(oEdit:oDlg,oEdit:aScrollSize[1],oEdit:aScrollSize[2],oEdit:aScrollSize[3],oEdit:aScrollSize[4])),;
                         IIF( oEdit:nMode=0,SetFocusGet(oDlg,NIL,.F.,oEdit:oFocus),NIL),.F.);
                         VALID (lSalir:=oEdit:ValidDlg(),IIF( lSalir, (oDp:Set("aId",{}),oEdit:oWnd:End()) , NIL),lSalir) // ::lSalir:=EVAL(bValid),lSalir)

      ::oDlg:Cargo:={.F.,NIL,NIL} // no ha sido cambiado


      oEdit:=Self

      ACTIVATE WINDOW ::oWnd;
             ON INIT  ( ChangeMenu(oWnd,oDlg,oEdit:cFileEdit,oEdit,SELF),; // oWnd:Move(oDlg:nTop , oDlg:nLeft, oDlg:nWidth , oDlg:nHeight ) ,;
                        oEdit:OnInit(),;
                        oEdit:lActivated:=.T.,;
                        PUBLICO(oEdit:cVarPublic,oEdit),;
                        oEdit:SETMSGBAR_ON(),;
                        IF(   oEdit:lMdiBar,oEdit:SETBOTBAR(),NIL),;
                        IF(Empty(oEdit:aScrollSize),NIL, TScrWnd():New(oEdit:oDlg,oEdit:aScrollSize[1],oEdit:aScrollSize[2],oEdit:aScrollSize[3],oEdit:aScrollSize[4])),;
                        AjustaDlg(oWnd,oDlg,nTop,nLeft,nBottom,nRight,oEdit),;
                        IF(oEdit:lSetColorGroup,EJECUTAR("SETCOLORTOGROUP",oEdit:oDlg,oEdit),NIL));
                        ON RESIZE (oDlg:SetSize(::oWnd:nWidth-7,::oWnd:nHeight-IIF( ::lMsgBar, (::oDlg:oMsgBar:nHeight*1.5)+2,0)),EVAL(oEdit:bResized));
                VALID (lSalir:=oEdit:ValidWnd(bValid),;
                       IIF(lSalir,(SaveDlg(oDlg,.t.,oEdit:cFileEdit,oEdit:lDesign,NIL,NIL,oEdit),oEdit:CloseAllEdit(),::oDlg:End(),::oDlg:aControls:=NIL,oDlg:=NIL,oEdit:End(),.T.),.F.))


    ELSE


      // ? oDlg:Cargo[3],oDlg:Cargo[4]
      oWnd:SetSize(oDlg:Cargo[3]-oDlg:Cargo[1],oDlg:Cargo[4])

      // ? "oDlg:Cargo",LEN(oDlg:Cargo)
      //oDlg:Cargo:={nTop,nLeft,nBottom,nRight}

      ::oWnd:Cargo:={.F.,NIL,NIL} // no ha sido cambiado
      ::oDlg:=::oWnd

      // Restaurar Controles
      PutControls(oWnd,.t.,nil,nil)
      PutBar(oEdit)
      AjustButtons(oEdit)
//    oEdit:   InitBody()

      ACTIVATE WINDOW ::oWnd   ;
               ON INIT  (oEdit:lActivated:=.T.,;
                         oEdit:SETMSGBAR_ON(),;
                         ChangeMenu(oWnd,oWnd,oEdit:cFileEdit,oEdit) , EVAL(bInit) , EVAL(oEdit:bInit));
               VALID (lSalir:=EVAL(bValid),;
                        IIF(lSalir,(oWnd:bValid:={||.t.},oEdit:DestroyMsg(),SaveDlg(oWnd,.t.,::cFileEdit,.F.,NIL,oEdit:lDesign,oEdit),oEdit:CloseAllEdit(),::End()),.F.))

    ENDIF


  ELSE


     ::oDlg:Cargo:={.F.,NIL,NIL} // no ha sido cambiado
     ::oWnd:=::oDlg

     nTop:=MAX(nTop,130) // JN 03/12/2015


     IF ValType(::oDpLbx)="O"
        aPoint   := AdjustWnd(::oDpLbx:oBar:aControls[1], nBottom, nRight )
        nTop     :=aPoint[1]+5
        nLeft    :=aPoint[2]-8
     ENDIF

     ::oDlg:bLostFocus := { || ::LostFocus() }
     ::oDlg:bGotFocus  := { || ::GotFocus(.T.),RestoreEdit(nNumEdit,oEdit) }

IF !::lMdiBar

     ACTIVATE DIALOG ::oDlg;
              ON INIT (ChangeMenu(NIL,oDlg,oEdit:cFileEdit,oEdit,SELF) ,;
                       IIF( oEdit:lMsgBar , oDlg:oMsgBar := TMsgBar():New( oDlg, "", .F., .F., .F., .F.,,,, ),NIL),;
                       oEdit:lActivated:=.T.,;
                       PutBar(oEdit),;
                       oDlg:Move( nTop , nLeft, nBottom, nRight ,.T. ),;
                       IIF( oEdit:lMsgBar .OR. !Empty(oEdit:aMsgItem) ,oDlg:oMsgBar:SetColor(oDlg:nClrText,oDlg:nClrPane),NIL),;
                       oEdit:SETMSGBAR_ON(),;
                       PutControls(oDlg,.t.,nil,nil,oEdit:lDesign,oEdit),AjustButtons(oEdit),; // ,_oDlg:nLastKey:=9);
                       EJECUTAR("SETCOLORTOGROUP",oDlg,oEdit),;
                       EVAL(bInit));
                       VALID (lSalir:=EVAL(bValid),;
                              IIF(lSalir,(oEdit:DestroyMsg(),SaveDlg(Self,.t.,oEdit:cFileEdit,lMdi,oEdit:cTable,oEdit:lDesign,oEdit)),.F.))
ELSE

   ::oDlg:=::oBar

/*
   ::oWnd:bValid:={||lSalir:=EVAL(bValid),;
                     IIF(lSalir,(oEdit:DestroyMsg(),SaveDlg(Self,.t.,oEdit:cFileEdit,lMdi,oEdit:cTable,oEdit:lDesign,oEdit)),.F.))}

   // Dialogo
*/

//? ::oDlg:ClassName()

ENDIF

  ENDIF

  PUBLICO(cVarPublic,oEdit)

  nNumEdit:=oEdit:nNumEdit

  IF lMdi .AND. oWnd!=NIL

    // ::oWnd:bGotFocus :={||::GotFocus(.T.)}

    oWnd:bGotFocus  := { || ::GotFocus(.T.),RestoreEdit(nNumEdit,oEdit) }
    oWnd:bLostFocus := { || ::LostFocus() } // CheckXbrw(oEdit:oDlg)}

  ENDIF

  // oDlg:bLostFocus  = { || __MXRELEASE(cVarPublic) }
  // ::aCopyVars:=aClone( ::aVars ) // Copia los Valores y asegurar el Cancel

  oDp:cHelpTopic:=::cTopic
  oDp:cFileChm  :=::cFileChm

  oDp:lKeyOn:=.T.

  // JN 22/11/2016
  ::aBrwFocus:=EJECUTAR("FRMGETBRW",Self)

  IF ::lAutoClose
     ::nOption:=0
     ::Cancel(.F.,NIL)
     ::Close()
  ENDIF

RETURN NIL

METHOD SETMSGBAR_ON() CLASS DPEDIT
    LOCAL I,bAction,oDlg:=IF(ValType(::oWnd)="O",::oWnd,::oDlg)

    //? oDlg:oMsgBar,"oDlg:oMsgBar"

    // JN 11/03/2019, No puede existir MSG con ScrollGet
    IF ::lVScroll
       RETURN .F.
    ENDIF

    IF oDlg:oMsgBar=NIL  .AND. !Empty(::aMsgItem)
        SET MESSAGE OF oDlg
    ENDIF

    FOR I=1 TO LEN(::aMsgItem)

       DEFAULT  ::aMsgItem[I,1]:=""                  ,;
                ::aMsgItem[I,2]:=LEN(::aMsgItem[I,1]),;
                ::aMsgItem[I,3]:={||.T.}

       bAction:=BloqueCod(::aMsgItem[I,3])

       DEFINE MSGITEM ::aMsgItem[I,4] OF oDlg:oMsgBar;
                      PROMPT ::aMsgItem[I,1] SIZE ::aMsgItem[I,2];
                      ACTION EVAL(bAction)

    NEXT I

RETURN .T.


/*
// Asigna enlaces entre tablas, para Visualización
*/
// METHOD ViewTable(cTable,cField,cWhere,CampoLocal,CampoForeing) CLASS DPEDIT
METHOD ViewTable(cTable,cField,cCampoLink,cCampoLocal,cControlSay,cName) CLASS DPEDIT

     Local oTable,cWhere,uValue
     local cSQL

     IF EMPTY(cCampoLocal)
        ? "ViewTable",cTable,cField,cCampoLink
        ? cCampoLocal,"cCampoLocal","Vacio"
     ENDIF

     // ? cCampoLocal,cTable

     uValue:=::Get(cCampoLocal)

     cWhere:=cCampoLink+GetWhere("=",uValue)

     DEFAULT cField:="*" // Todos los Campos
     DEFAULT cControlSay:="o"+cField

     IF !EMPTY(cWhere) // .AND.!" WHERE "$" "+cWhere
        cWhere:=" WHERE "+cWhere
     ENDIF

     cSql  :="SELECT "+cField+" FROM "+cTable+" "+cWhere

     oTable:=OpenTable(cSql,.t.)

     DEFAULT cName:="O"+cTable // Posible Nombre del Cursor

     ::ADD(UPPE(ALLTRIM(cName)),oTable) // Asignar una Variable Virtual oForm:oCliente, Este queda Asi

     //? cName,"cName"

     AADD(::aTableLink,{cTable,cField,cCampoLink,cCampoLocal,cControlSay,oTable})

     //? cTable,cField,cCampoLink,cCampoLocal,cControlSay

RETURN oTable

/*
// Recontruye los Valores de las Tablas Asociadas
*/
METHOD BuildLink() CLASS DPEDIT
   LOCAL I,cWhere,cSql

   FOR I := 1 TO LEN(::aTableLink)

      cSql  :="SELECT "+::aTableLink[I,2]+" FROM "+::aTableLink[I,1]+" WHERE "+;
               ::aTableLink[I,3]+GetWhere("=",::Get(::aTableLink[I,4]))

      ::aTableLink[I,6]:cSql:=cSql
      ::aTableLink[I,6]:Reload()

   NEXT

RETURN .T.

/*
// Carga los Datos hacia las Variables
*/
METHOD RunScript(cFunction,uPar1,uPar2)  CLASS DPEDIT
   LOCAL lResp:=.t.

   cFunction:=STRTRAN(cFunction,"()","")

   ::nOption:=0
   ::Prepare()

  IF !::oScript:IsFunction(cFunction)

     MensajeErr("Función : "+cFunction+" No Existe ")

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



METHOD RunScr(cProgram)  CLASS DPEDIT
   LOCAL lResp:=NIL,cTest

   PRIVATE oTable :=::oTable
   PRIVATE oDpEdit:=Self

// oDp:oFrameDp:SetText(cProgram+" "+::oScript:cProgram)
// cTest:="oDpEdit:"+cProgram+"()"
// ? cProgram,"cProgram",::cScript," DE DPEDIT",::oScript:cProgram

   IF (ValType(cProgram)="C" .AND. !EMPTY(cProgram)) .AND. ::oScript=NIL
      ::oScript:=COMPILA(::cScript,.T.)
   ENDIF

   IF ValType(::oScript)="O"  .AND. !::oScript:IsFunction(cProgram)
      ::oScript:=COMPILA(::cScript,.T.)
   ENDIF

   IF ValType(cProgram)="C" .AND. !EMPTY(cProgram)

      SETSCRIPT(::oScript)

      IF !ValType(::oScript)="O"
         ::oScript:=COMPILA(::cScript,.T.)
      ENDIF

      IF !ValType(::oScript)="O"
         ::oScript:=GETSCRIPT(::cScript)
      ENDIF

      IF !ValType(::oScript)="O"
        MensajeErr("FUNCTION "+cProgram+" no existe en "+::cScript)
        RETURN .F.
      ENDIF

      IF !::oScript:IsFunction(cProgram)
        MensajeErr("FUNCTION "+cProgram+" no existe en "+::oScript:cProgram)
        RETURN NIL
      ENDIF

      VP("SCRRUNFUNCTION",cProgram) // Nombre de la Funcion
  //    ? "ASI",cTest," A, VER "
  //    lResp:=MACROEJE(ctest)
  //    ? "lResp",lResp
      lResp:=SCRRUN(::cScript,SELF)  // Nombre del programa
//      ? "lResp",lResp,"Desde CANCEL en Script "
   ELSEIF ValType(cProgram)="B"
      lResp:=EVAL(cProgram,SELF)
   ENDIF

   lResp:=IIF( Valtype(lResp)!="L" , .T. , lResp )

   STORE NIL TO oTable,Self

RETURN lResp

/*
// Verifica si existe o no
*/
METHOD IsDef( cName ) CLASS DPEDIT
RETURN __objHasMsg( self, cName )
/*
  LOCAL lResp:=.F.

  ::lIsDef:=.t.

  lResp   :=(oSend(Self,cName)!=NIL)

  ::lIsDef:=.f.

RETURN lResp
*/

/*
 *  DPEDIT:Add()
 */
METHOD Add( cName, xValue ) CLASS DPEDIT // [ER]


//      ? "add",::IsDef(cName),cName,xValue
//      IF !::IsDef(cName)
        __objAddData( Self, cName )
///      ENDIF

      oSend(Self , cName , xValue)

/*
//    local nClassH := ::ClassH
//   if cName != NIL
      // cName:=uppe(alltrim(cName))
      if (::nPos := aScan( ::aVars, { |e,n| e[1] == cName } )) != 0

         ::aVars[::nPos,2] := xValue

      else

         aAdd( ::aVars, { cName, xValue } )

         ::nPos := Len(::aVars)

//        GenDataEdit( Self , cName , ::nPos )

         // Nuevos Valores
         // __objAddData( Self, cName)
         // oSend(Self,cName,xValue)
         // __ObjSetValueList( self, { { cName, xValue } } )

      endif

      ::cName  := cName // uppe(alltrim(cName))
//   endif
*/
RETURN Self

/*
// Prepara tablas enlazadas con el encabezado
*/
METHOD LinkTable(cTable,cSql) CLASS DPEDIT
  LOCAL nPos,aFields,I,cDataHead:="",cWhere:="",nLen:=0
  LOCAL nTop:=2,nLeft:=2,nWidth:=20,nHeight:=40
  LOCAL cLinkBody:="",cLinkHead:=""
  LOCAL aLinkHead:="" // Campos del Encabezado
  LOCAL aLinkBody:="" // Campos del Cuerpo
  LOCAL aBodyLink:={} // Enlace Encabezado Cuerpo
  LOCAL aHeadLink:={} // Enlace Cuerpo Encabezado
  LOCAL oTable,cSqlDoc,cLink:="",aData //,uData

  LOCAL aLista

  IF EMPTY(::cSqlIni)
     ::cSqlIni:=cSql
  ENDIF

  aLinkHead:=GetLinkTable(::cTable,cTable  ,1,.T.)

  IF ValType(aLinkHead)!="A"
     MsgAlert("Relación "+::cTable+" Con "+cTable,"No es Posible")
     RETURN .F.
   ENDIF

   aLinkBody:=GetLinkTable(::cTable,cTable  ,2,.T.)

   cWhere :=" "
   I      :=1

   WHILE I<=LEN(aLinkHead)

       // nPos     :=::GetPos(aLinkHead[I])
       cDataHead:=""

       IF ::IsDef(aLinkHead[I]) // nPos>0

//         cDataHead:=::aVars[nPos,2]
         cDataHead:=::Get(aLinkHead[I])
         nLen     :=nLen+LEN(cDataHead)

         IF ::nOption=1  .AND. ::nMode=0 // Incluir
            cDataHead:=RIGHT(STRZERO(VAL(oDp:cUsuario)+VAL(ALLTRIM(STR(SECONDS(),10,0))),10),LEN(cDataHead))
            cDataHead:=RIGHT(cDataHead,nLen)
         ENDIF

         //
         // Crea las Variables en el Encabezado
         //
/*       ::nPos:=::GetPos( aLinkBody[I] )

         IF ::nPos=0
            ::add(aLinkBody[I]) // Crea la Variable
         ENDIF

         ::aVars[::nPos,2] := cDataHead // Remplazado por Abajo*/

         IF !::IsDef(aLinkBody[I])
            ::Add(aLinkBody[I],cDataHead)
         ENDIF

         ::Set(aLinkBody[I],cDataHead)

// ? "cDataHead",cDataHead

         AADD(aBodyLink,{aLinkBody[I],cDataHead}) // Enlaza Cuerpo Encabezado
         AADD(aHeadLink,{aLinkHead[I],aLinkBody[I]})

         cLink:=cLink+IIF( Empty(cLink),"" , ",")+GetWhere("",cDataHead)


       /*  IF i=1
            uData:=cDataHead
         ELSEIF ValType(cDataHead)="C"
            uData+=cDataHead
         ENDIF */

         cWhere+=IIF( !EMPTY(cWhere) , " AND " , "" )+ALLTRIM(aLinkBody[I])+GetWhere("=",cDataHead) // "='"+cDataHead+"'"

       ELSE

          MsgAlert(aLinkHead[I]," no esta en el Formulario ")

       ENDIF

       I++

    ENDDO //NEXT

//   ? "ANTES ERA ",cWhere,"cWhere",::nOption,cSql

    cSql  :=STRTRAN(cSql,"{WHERE}",cWhere)

//   ? "AHORA ESTA",cWhere,"Comando de Relacion",cSql,::nOption,cTable

    // oTable:=tTable():New( NIL , cSql  , NIL  , cTable , 0 )
    oTable:=OpenTable(cSql,.T.)
    oTable:aBodyLink:=aBodyLink
    oTable:aHeadLink:=aHeadLink
    oTable:cWhere   :=cWhere

   // ? cSql,"cSql",oTable:ClassName()
   // oTable:oCursor:End()
/*
    IF ::nOption=1 .AND. .F. .AND. ISTABLA("DPCTRDOC") // .OR. ::nOption=0 // Incluir
    // Hay que Buscar el Cuerpo Caido

       cLink  :="{"+cLink+"}"

       cSqlDoc:="SELECT * FROM DPCTRDOC WHERE CTR_ESTADO='A'  AND "+;
                                             "CTR_HWND  "+GetWhere("<>",oDp:oFrameDp:hWnd)+" AND "+;
                                             "CTR_TABLA "+GetWhere("=" ,::oTable:cTable  )+" AND "+;
                                             "CTR_TABBOD"+GetWhere("=" ,cTable)           +" AND "+;
                                             "CTR_US    "+GetWhere("=" ,oDp:cUsuario)+" LIMIT 1"

       ::oCtrDoc:=OpenTable(cSqlDoc,.T.) // Busca Disponible

       IF ::oCtrDoc:RecCount()>0

          cLink:=STRSQLOFF(::oCtrDoc:CTR_LINK)
          cSql :=ALLTRIM(STRSQLOFF(::oCtrDoc:CTR_SQL ))

          aData:=MACROEJE(cLink)

          IF LEN(aData)=LEN(aBodyLink) // Transfiere los viejos valores
             FOR I=1 TO LEN(aBodyLink)
               // ? "Cuerpo",ValType(aLinkBody[I,1]),ValType(aLinkBody[I,2]),aData[I]
               aBodyLink[I,2]:=aData[I]
               ::Set(aLinkBody[I],aData[i])
             NEXT I
          ELSE
             ? "Error en Recuperación",len(aData),LEN(aBodyLink)
          ENDIF

       ENDIF

       ::oCtrDoc:End()
       ::oCtrDoc:=OpenTable("SELECT * FROM DPCTRDOC WHERE CTR_ESTADO='C' LIMIT 1 ",.T.) // Busca Disponible
       ::oCtrDoc:lAppend:=.F.
       IF ::oCtrDoc:RecCount()=0
          ::oCtrDoc:AppendBlank()
       ENDIF
       ::oCtrDoc:Replace("CTR_NUMFRM",::nNumEdit)
       ::oCtrDoc:Replace("CTR_TABLA" ,::oTable:cTable)
       ::oCtrDoc:Replace("CTR_ESTADO","A"   )
       ::oCtrDoc:Replace("CTR_SQL"   ,STRTRAN(cSql,"'",'"'))
       ::oCtrDoc:Replace("CTR_FECHA" ,Date())
       ::oCtrDoc:Replace("CTR_TABBOD",cTable)
       ::oCtrDoc:Replace("CTR_US"    ,oDp:cUsuario)
       ::oCtrDoc:Replace("CTR_HORA"  ,TIME())
       ::oCtrDoc:Replace("CTR_HWND"  ,oDp:oFrameDp:hWnd)
       ::oCtrDoc:Replace("CTR_IP"    ,GETHOSTBYNAME( GETHOSTNAME()))
       ::oCtrDoc:Replace("CTR_PC"    ,GETHOSTNAME()) // Nombre del PC
       ::oCtrDoc:Replace("CTR_LINK"  ,STRTRAN(cLink,"'",'"'))         // Data de Enlace

       IF ::oCtrDoc:lAppend
          ::oCtrDoc:Commit()
       ELSE
          ::oCtrDoc:Commit("WHERE CTR_ESTADO='C'" ) // LIMIT 1")
       ENDIF

       ::oCtrDoc:cPrimary:="CTR_IP,CTR_PC,CTR_NUMFRM,CTR_TABBOD,CTR_TABLA,CTR_HWND"

    ENDIF
*/

RETURN oTable

/*
// Asignación del Cuerpo del Documento
*/
// METHOD SetBody(oTable,cScript,aFields,cPreFix) CLASS DPEDIT // ,aLinkHead,aLinkBody) CLASS DPEDIT
// RETURN ::oBody

/*
 *  DPEDIT:Del()
 */

METHOD Del( cName ) CLASS DPEDIT
/*
local nPos

//   if cName != NIL
    cName:=UPPE(ALLTRIM(cName))
      if (nPos := aScan( ::aVars, { |e,n| e[1] == cName } )) != 0
         aDel( ::aVars, nPos )
         ::aVars  := aSize( ::aVars, Len(::aVars) - 1 )
         ::nPos   := 0
         ::cName  := ""
      endif
//   endif
*/
RETURN Self

/*
 *  DPEDIT:Get()
 */
METHOD Get( cName,lOk ) CLASS DPEDIT                  // [by ER]
RETURN  oSend(Self,cName)

/*
 *  DPEDIT:Set()
 */
METHOD Set( cName, xValue, lRefresh ) CLASS DPEDIT
   LOCAL oObj,cObj

   DEFAULT lRefresh:=.F.

   //oSend(Self , cName , xValue)

   __objAddData( Self, cName )
   __objSendMsg( Self, cName , xValue )

   Eval(::bSet,cName,xValue)

// 06/05/2021 JN se llamará asi misma
//   IF lRefresh
//    ::VarPut(cName,xValue,.F.)
// ENDIF

   IF lRefresh .AND. ::ISDEF(cName)

      oObj:=::Get(cName)

      IF ValType(oObj)="O"
        oObj:VarPut(xValue,.T.)
        oObj:Refresh(.T.)
      ENDIF

   ENDIF


   AEVAL(::aScrollGets,{|o| o:SetFromForm(cName,xValue)})

RETURN nil // Self

/*
// Asigna Valor y refresca su Contenido
*/
METHOD VarPut(cName,xValue,lSet)
   LOCAL cObj:="o"+cName,oObj

   DEFAULT lSet:=.T.

   IF lSet
     ::Set(cName,xValue)
   ENDIF

   IF ::ISDEF(cObj)

      oObj:=::Get(cObj)

      IF ValType(oObj)="O"
        oObj:VarPut(xValue,.T.)
        oObj:Refresh(.T.)
      ENDIF

   ENDIF

RETURN NIL

/*
 *  DPEDIT:GetPos()                         // [by ER]
 */
METHOD GetPos( cName ) CLASS DPEDIT

   ::cName  := cName

   MensajeErr(GetProce(),"Método GetPos(), Derogado")

RETURN ::nPos := aScan( ::aVars, { |e,n| e[1] == cName } )

/*
 *  DPEDIT:Release()
 */
METHOD Release() CLASS DPEDIT

   ASIZE(::aVars,0)
   ::cName  := ""
   ::nPos   := 0

RETURN Self

/*
// Obtiene uno o varios campos del Formulario
*/
METHOD  GetValue(cField) CLASS DPEDIT
  local aFields:=_VECTOR(cField,IIF("+"$cField,"+",",")),I
  local uValue :=""

  IF Len(aFields)=1
     return ::Get(cField)
  ELSE
     AEVAL(aFields,{|a,i|uValue:=uValue+CTOO(::Get(a),"C")})
  ENDIF

RETURN uValue

/*
// Destruye los Edit Asociados
*/
METHOD KillEdit(oEdit)
  LOCAL nAt

  IF ValType(oEdit)!="O"
     RETURN .F.
  ENDIF

  WHILE (nAt:=ASCAN(::aDpEdit,{|o,n|o=NIL}),nAt>0)
     ADEL(::aDpEdit,nAt)
     ASIZE(::aDpEdit,LEN(::aDpEdit)-1)
  ENDDO

  nAt:=aScan( ::aDpEdit, { |o,n| ValType(oEdit)="O" .AND. o:nNumEdit == oEdit:nNumEdit } )

  IF nAt>0
    ADEL(::aDpEdit,nAt)
    ASIZE(::aDpEdit,LEN(::aDpEdit)-1)
  ENDIF

RETURN .T.

/*
 *  OnError()
 */
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS DPEDIT

  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N //,oFrameDp
  LOCAL lResp:=.F.

  cMsg   := ALLTRIM(UPPE(__GetMessage()))

  if Left( cMsg, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2,Len(cMsg))

      // ::add(cMsg,uValue)
      __objAddData( Self, cMsg )
      oSend(Self ,cMsg , uValue)

   ELSE

     IF ::lIsDef   // Llamada por ::IsDef()
        RETURN NIL
     ENDIF

      // Busca si tiene alguna función oScript
         ::cName  := ""
         ::nPos   := 0

      __QQPUB(::cVarPublic)
      PUBLICO(::cVarPublic,Self)

      // 24/04/2023
      IF !ValType(::oScript)="O"
         ::oScript:=GetScript(::cScript)
      ENDIF

      // ? ::oScript:cProgram,"cProgram, 1",cMsg
      // 26/04/2023
      IF !ValType(::oScript)="O" .OR. !::oScript:IsFunction(cMsg)
         ::oScript:=COMPILA(::cScript,.T.)
      ENDIF

      // ? ::oScript:cProgram,"cProgram, 2",cMsg

      IF !ValType(::oScript)="O" .OR. !::oScript:IsFunction(cMsg)

          COMPILA(::cScript,.T.)
          ::oScript:=GetScript()

          IF !ValType(::oScript)="O" .OR. !::oScript:IsFunction(cMsg)

// ViewArray(::oScript:aFunctions)

             MensajeErr("REQUIERE COMPILAR "+::cScript+CRLF+"Funcion "+cMsg+CRLF+GetProce(),"Funcion no existe: ")
          ENDIF

          // MensajeErr("FUNCTION "+cMsg+CRLF+GETPROCE(),"Funcion no existe: ")
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

     ENDIF

     IF !lScript
        MsgAlert(cMsg,"Programa no está en ejecución"+::cScript)
     ENDIF


   endif

RETURN NIL

/*
// Finalizar Formulario
*/
METHOD End( ) CLASS DPEDIT                // [by ER]
  LOCAL nAt
  LOCAL I,cWhere,oEdit

  IF ::lSqlBegin
     ::UnLockTable()
  ENDIF

  ::oControl:=NIL
  ::oFocus  :=NIL
  ::cIdFrm  :=NIL

  IIF( ValType(::oBrush)="O", ::oBrush:End(),NIL)

  AEVAL(::aTableLink,{|a,n|a[6]:End()})

  // Cerrar Editores Asociados
  // ? "salir",Getproce()

  ::lSalir:=.T.

  IIF( ValType(::oBody)="O",::oBody:End() ,NIL )

  //Destroy(::oDlg)

  IIF( ValType(::oTable     )="O",::oTable:End()           ,NIL)
//  IIF( ValType(::oToolMsg   )="O",::oToolMsg:End()         ,NIL)
  ::DestroyMsg()

  oBody:=NIL

  // Debe quitar de la lista el Objeto
  // ojo

  nAt:=aScan( aDpEdit, { |e,n| e[1] == nNumEdit } )

  // Elimina la instancia del objeto
/*  IF nAt>0
     __MXRELEASE(aDpEdit[nAt,2])
     ADEL(aDpEdit,nAt)
     aSize(aDpEdit,Len(aDpEdit)-1)
  ENDIF*/

/*
  DEFAULT ::oDlg:aControls:={}

  FOR I := 1 TO LEN(::oDlg:aControls)
     IF ::oDlg:aControls[I]:ClassName()="XBROWSE"
        ::oDlg:aControls[I]:CancelEdit()
     ENDIF
  NEXT
*/

  IIF(ValType(::oTmr)="O",::oTmr:End(),NIL) // Destruye timer
  ::oTmr:=NIL

  // Cierra el Control del Documento
//  IF ValType(::oCtrDoc)="O"
//     ::oCtrDoc:lAppend:=.F.
//     ::oCtrDoc:Replace("CTR_ESTADO","C") // Concluido
//     ::oCtrDoc:Commit(::oCtrDoc:GetWhereKey()) //cWhere)
//     ::oCtrDoc:End()
//     ::oCtrDoc:=NIL
//  ENDIF

  IF nAt>0
//     __MXRELEASE(aDpEdit[nAt,2])
     ADEL(aDpEdit,nAt)
     aSize(aDpEdit,Len(aDpEdit)-1)
  ENDIF

  EVAL(::bEnd) // Al Finalizar

  // AEVAL(::aScrollGets,{|o|o:End()})

  // ? "cierra dpedit"

  IF ValType(::oEditScr)="O"
     ::oEditScr:End()
     ::oEditScr:=NIL
  ENDIF

  IF ValType(::oDlg)="O"
     KillControl(::oDlg,::lDesign)
  ENDIF

  IF ::lMdi .AND. ValType(::aVars)="A"

    FOR I := 1 TO LEN(::aVars)
       IF ValType(::aVars[I,2])="O"
         ::aVars[I,2]:=NIL
       ENDIF
    NEXT

//    ::aVars   :=NIL
//    ::oTable  :=NIL
//    ::oWnd    :=NIL
//    ::oEditScr:=NIL

  ENDIF

  // Recupera Archivo DE Ayuda
  oDp:cHelpRtf:=::cFileRtfOld


  // HB_GCALL() // Recolector de Basura

 //   :="CTR_IP,CTR_PC,CTR_NUMFRM,CTR_TABDOC,CTR_TABLE,CTR_HWND"

RETURN .T.


/*
// Creación de Variables desde el Cursor de un Browse
*/

METHOD CreateVars( oObj,oCursor,nOption ,lView , lCreateWindow, aFields ) CLASS DPEDIT

   LOCAL aVars:={} ,I ,cName ,cKey ,nOrder:=0 ,aKeys ,nAt , cWhere, aIndex,uValue,cField,aStruct:={},oTable

   // DEFAULT oCursor   :=::oCursor
   DEFAULT nOption      :=::nOption
   DEFAULT lView        :=.F.
   DEFAULT lCreateWindow:=.T.
   DEFAULT aFields      :={}

   ::aRecord:={}      // Copia del Registro que esta en Edición

   //? "CreateVars" ,ErrorSys(.t.)

   // ? ErrorSys(.T.),"AQUI ES NATIVO" // OJO
   aStruct:=::oTable:aFields

   AEVAL(::oTable:aFields,{|aField,i|AADD(::aRecord,::oTable:Get(aField[1]))})


   IF ::nOption=3 // Modificar


   ENDIF

   IF ::oTable=NIL
//MensajeErr(" ojo" )
      ::oTable:=TTABLE():New( nil , cWhere  , nil  , ::cTable, ::nOption )
      ::oTable:aFill()
      ::oTable:oCursor:End() // no necesito el Cursor
   ENDIF

   // Crea las variables desde la tabla //
   // ? "CREATEVARS",::oTable:ClassName(),::oTable:Fcount(),::oTable:cSql,LEN(::oTable:aFields)
   // ? ::oTable:Fcount(),"Fcount()"

   FOR I := 1 TO ::oTable:Fcount()

//    cName :=ALLTRIM(UPPE(::oTable:FieldName(i)))
      cName :=::oTable:FieldName(i)
      nAt   :=ASCAN(aFields,{|a,n|a==cName})

//    ErrorSys(.t.)
//    ? cName,nAt,::nOption

      // Solo Duplicará las Necesarias, aFields solo son las Necesarias
      IF ::nOption=1 .AND. nAt>0
  //     ? "Este no",cName
         LOOP
      ENDIF

      uValue:=::oTable:FieldGet(i)

//      IF ::oTable:aFields[I,2]="C"
//        uValue:=PADR(uValue,::oTable:aFields[I,3])
//? LEN(uValue),::oTable:aFields[I,1],::oTable:aFields[I,3]
//      ENDIF

//      uValue:=IIF( ValType(uValue)="C",LEFT(uValue,Len(uValue)-0), uValue )

      // ? uValue, ::oTable:aFields[I,1],::oTable:aFields[I,2]

      IF ::oTable:aFields[I,2]="C"
        uValue:=PADR(uValue,::oTable:aFields[I,3])
        ::oTable:Set(::oTable:aFields[I,1],uValue)
        uValue:=IIF( nOption=1 , CTOEMPTY(uValue,::oTable:aFields[I,2],::oTable:aFields[I,3]), uValue )
        uValue:=STRSQLOFF(uValue)
        // JN 03/04/2014, debe estar vacio para asignar valores por defecto
        ::Set(::oTable:aFields[I,1],uValue)
        ::lSetDef:=.T.

// ? "CAMPO VACIO", ::oTable:aFields[I,1]
// ? uValue,LEN(uValue),::oTable:aFields[I,1]
      ENDIF


// ::nPos:=::GetPos( cName )
//    ? "será revisado si ",cName,"existe"
//
//    ? cName,::IsDef(cName),uValue,"CREATEVAR SETVAR"

      IF !::IsDef(cName) // ::nPos=0
         ::add(cName,uValue) // Crea la Variable
      // ELSE
      //   ::Set(cName,uValue)
      ENDIF

      ::Set(cName,uValue)
      ::add(cName+"_",uValue)

      // IF lView
      //
      // ENDIF

      // ::aVars[::nPos,2] := uValue // oCursor:FieldGet(i) // uValue //  GetParam( 1, 1 )

   NEXT

   ::lSetDef:=.T.

   IF ::nOption=1 // JN 12/08/2018 .AND. ::lSetDef
     EJECUTAR("DPEDITSETDEF",SELF)
     ::lSetDef:=.T.
   ENDIF

   IF ::nOption=1 .OR. ::nOption=3
      ::SetDefValue()
   ENDIF

   // Debe los Datos de las tablas indices, necesario Para Modificar //

   IF EMPTY(::aDataKey) // Necesario para Identificar el registro que se modificará

     IF EMPTY(::oTable:cPrimary)
        ::oTable:cPrimary:=::oTable:aFields[1,1]
     ENDIF

     ::aDataKey:=_VECTOR(::oTable:cPrimary,",")
     ::cWhere  :=""

     FOR I := 1 TO LEN(::aDataKey)
        cField:=::aDataKey[I]
        // uValue:=::oTable:FieldGet(::oTable:FieldPos(cField)) // NO FUNCIONO
        uValue:=::oTable:Get(cField)
        // ::cWhere:=::cWhere+IIF( Empty(::cWhere),"" , " AND " )+ cField+" = '"+CTOO(uValue,"C")+"'"
        ::cWhere:=::cWhere + IIF( Empty(::cWhere),"" , " AND " ) + cField + GetWhere("=",uValue) // +CTOO(uValue,"C")+"'"
        // ? ::cWhere,"AQUI ES"
        ::aDataKey[i]:=uValue
     NEXT

     ::cWhere:=" WHERE "+::cWhere

// ? ::cWhere
//   ::aDataKey,::oTable:GetDataKey(nOrder))
/*
     WHILE ( nOrder++ , ::nOption!=1 )

        IF EMPTY(::oTable:IndexKey(nOrder))               // JN 29/12/2002
           EXIT
        ENDIF

       // IF EMPTY(::oCursor:IndexKey(nOrder))
       //   EXIT
       // ENDIF

       AADD(::aDataKey,::oTable:GetDataKey(nOrder))

     ENDDO
*/
   ENDIF

   // ? ::oTable:INV_DESCRI,LEN(::oTable:INV_DESCRI)
   //IF ValType(::oTable)="O"
      // ::oTable:oCursor:End() // Ya no lo necesito
      // ::oTable:oCursor:=NIL
   // ENDIF

   // ya no necesito los arreglos de ttabla //
   // ::oTable:aDataFill:={} // Data Completa de la tabla (Original)
   // oCursor:=NIL

   IF lCreateWindow
      ::CreateWindow() // Verifica las Ventanas
   ENDIF

   //? "POR AHORA NO CREA LAS VENTANA"
   // Hay que cerrar el Cursor, ya no lo necesito, solo necesito el Odbc

RETURN NIL

/*
// Valida que la Clave no se repita
*/
METHOD ValUnique( uValue , cField , cMsg , cWhere , lExcluye ) CLASS DPEDIT
   LOCAL lFound:=.F.,I:=0,aFields:={}
   LOCAL oCursor,cSql
   LOCAL oTable,nField
   LOCAL cDataNew,cDataKey
   LOCAL nOption:=IIF( ::lSaveHead,3,::nOption)

   IF nOption=0
      RETURN .T.
   ENDIF

   DEFAULT cField:=::oTable:cPrimary ,;
           lExcluye:=.F.

   DEFAULT cWhere:=""

   DEFAULT uValue:=::oTable:GetDataKey(Nil,cField)

   DEFAULT cMsg  :=[ Registro ]+ALLTRIM(CTOO(uValue,"C"))+[ ya existe ]+CRLF+" Intente Nuevamente"

   IF nOption!=1
     cDataKey:=::oTable:GetDataKey(NIL,cField)
   ENDIF

   ::DestroyMsg()

   IF !EMPTY(cWhere) .AND. "AND"!=LEFT(ALLTRIM(UPPER(cWhere)),3)
       cWhere:=" AND "+cWhere
   ENDIF

   IF ValType(uValue)="A"

      cWhere :=""
      aFields:=_Vector(cField,",")

      FOR I=1 TO LEN(uValue)
         cWhere:=cWhere+IIF(Empty(cWhere),""," AND ")+;
                 aFields[I]+GetWhere("=",uValue[I])
      NEXT I

      cSql  :="SELECT "+STRTRAN(cField,"+",",")+" FROM "+::cTable+ " "+;
              " WHERE "+cWhere

   ELSE

       cSql  :="SELECT "+STRTRAN(cField,"+",",")+" FROM "+::cTable+ " "+;
               " WHERE "+cTokenWhere(cField,uValue,::cTable)+IIF( !EMPTY(cWhere),cWhere,"")

   ENDIF


   IF !ChkSql(cSql)
     MensajeErr(cSql,"Expresión Incorrecta ::VALUNIQUE()")
     RETURN .F.
   ENDIF

   oDp:lExcluye:=lExcluye

   oTable:=OpenTable(cSql,.T.,::oDb,,.F.)

// ? CLPCOPY(cSql),"VALUNIQUE"

   lFound:=(oTable:Reccount()>0)

   IF oTable:RecCount()=0 // Tabla Vacia
      IF ::lSaveHead // Ya fue Creado el Documento
         ::IsSaveHead(::nOption=1,cField)
      ENDIF
      oTable:End()
      RETURN .T.
   ENDIF

   IF nOption=1 .AND. lFound // oTable:Reccount()>0

      oTable:End()
      lFound:=.T.

   ELSEIF nOption=3 .AND. lFound // Modificar

      // El encontrado se igual al mismo
      //? cField

      // cDataNew:=oTable:GetDataKey(NIL,STRTRAN(cField,"+",","))
      //oTable:End()
      // oTable:=OpenTable(cSql,.t.)
      // oTable:Browse()
      cDataNew:=oTable:GetDataKey(Nil,cField)

      // ? oTable:FieldGet(1),oTable:FieldGet(2)
//     ? cDataNew,cField,cDataKey,cSql
//                                                         cd?

      IF cDataNew==cDataKey
         lFound:=.F. // Se modifica el mismo
      ELSE
  //       ? "CAMBIO"
      ENDIF
   ENDIF

   oTable:End()

/*
   ? cKey,nOrder," Este es el Comando"

   lFound:=::oTable:DbseekDb(cKey,uValue)

   IF lFound.AND.::nOption=3 // Modificar
      uValue:=::aDataKey[nOrder]
      lFound:=!ALLTRIM(UPPE(uValue))=ALLTRIM(UPPE(cKey))
   ENDIF
*/
   IF lFound
      oDp:oControl:MsgErr(cMsg,"Registró inválido")
      //::ShowMsg( cMsg,NIL,NIL, NIL)
   ENDIF

RETURN !lFound

/*
// Asume por Defecto las Opciones Básicas del Formulario
*/
METHOD SetDefault() CLASS DPEDIT

  // Estos son Procedi  mientos Script
  ::cCancel  :="CANCEL"
  ::cPreSave :="PRESAVE"
  ::cPostSave:="POSTSAVE"
  ::cLoad    :="LOAD"

  IF ValType(::oTable)="O"
     ::cOrderBy :=::oTable:cPrimary // Asume PrimaryKey como Modo de Orden
  ENDIF

RETURN NIL


/*
// Genera Valores Incrementales de cualquier campo
*/

METHOD Incremental( cFieldName , lZero , cSql , cWhere , oDb ) CLASS DPEDIT
    // Busca el Mayor de los Valores y los Muestra //
    LOCAL uValue,nLen,aZero:={} // nPos,uValue,nLen,nAt LOCAL aValue
    LOCAL oTable,nAt,nLenZero // Determina LenZero

    DEFAULT cWhere    :=""
    DEFAULT cFieldName:=::oTable:cPrimary

//  JN 13/05/2014
//  DEFAULT cSql      :="SELECT MAX("+cFieldName+") AS "+cFieldName+" FROM "+::oTable:cTable+" "+;
//                      IIF( !"WHERE"$cWhere.AND.!EMPTY(cWhere)," WHERE ","" ) + cWhere

   DEFAULT cSql      :="SELECT "+cFieldName+" FROM "+::oTable:cTable+" "+;
                      IIF( !"WHERE"$cWhere.AND.!EMPTY(cWhere)," WHERE ","" ) + cWhere +;
                      " ORDER BY "+cFieldName+" DESC LIMIT 1"

    IF lZero=NIL
       nAt  :=ASCAN(oDp:aFieldZero,{|a,n| a[1]=::cTable })
       aZero:=IF(nAt=0,{},oDp:aFieldZero[nAt,2])
       lZero:=ASCAN(aZero,{|a,n|a[1]=cFieldName})>0
    ENDIF

    uValue:=SQLINCREMENTAL(::oTable:cTable,cFieldName,cWhere,oDb,NIL,lZero)

    // JN 31/05/2018
    IF Empty(uValue) .AND. ValType(uValue)="C"
       uValue:=STRZERO(1,LEN(uValue))
    ENDIF

    nAt   :=ASCAN(::aIncremental, { |c,i| c[1] == cFieldName } )

    IF nAt=0
       AADD(::aIncremental,{cFieldName,lZero,cSql,cWhere})
    ENDIF

RETURN uValue

/*
// Cancelar Formulario
*/
METHOD Cancel( lAsk , lFromValid )  CLASS DPEDIT
   LOCAL lCancel:=.T.,cWhere:="",i,uValueOld,cSql,cField:="",oTable,oDpLbx

   ::DestroyMsg()

   CursorWait()

   DEFAULT lFromValid:=.F.

   ::CancelFind()

   DEFAULT lAsk      :=::nOption<>2 // Consulta no Requiere Confirmar.T.
   DEFAULT lFromValid:=.F.

   IF !::cTable=NIL .AND. ::cTable="NMTRABAJADOR"
      // ? GETPROCE(),"VACIO",::cCancel,::nOption,"nOption"
   ENDIF

// IF ValType(::oTable)="O" .AND. "DPINV"$::oTable:cTable
//  ? ::lCancel,"lCancel ",GETPROCE()
// ENDIF

// IF ::nOption>0
   //? ::lCancel,::nOption,"::lCancel,::nOption"
   // ::UpDateFromTable(.T.)
// ENDIF

// ? "AQUI CANCEL",GETPROCE()

   IF !Empty(::cTable) .AND. ValType(::oTable)="O"

      ::RECCOUNT() // 31/12/2023, Si cancela y no existe registro debe salir

     // ? ::nRecCount,"Cant. Registros",::nOption,"opction",GETPROCE()

      IF ::nRecCount=0

        ::bValid:={||.T.} // 08/01/2024., evitar que se clave
        // clave el sistema   ::CLOSE()
        RETURN .T.
      ENDIF

   ENDIF

   ::ONCLOSE()

   IF ::lCancel  // Ya fué Aceptado Cancelar

//      IF !MsgNoyes("Desea Cancelar",::cTitle)
//          RETURN .F.
//      ENDIF

      ::nRecnoOld:=0 // Obliga a refrescar los datos
      KillxBrw(::oDlg)
      IIF( ::nMode=0 , ::End() , ::nOption:=0 )

      RETURN .T.

   ENDIF

   IF ValType(::aCopyEdit)!="A"
      ::lChange:=.F.
   ENDIF

   IF ValType(::aCopyEdit)="A" .AND. LEN( ::aCopyEdit )!=LEN( ::aVars )
          ::lChange:=.T.
   ELSEIF ValType(::aCopyEdit)="A"
      FOR I := 1 TO LEN(::aCopyEdit)
        ::lChange:=IIF( ValType(::aCopyEdit[I,2])!=ValType(::aVars[I,2]), .T. , ::lChange )
        IF ::lChange
           ::lChange:=IIF( ValType(::aCopyEdit[I,2])!="O" .AND. (::aVars[I,2]!=::aCopyEdit[I,2]), .T. , ::lChange )
        ENDIF
      NEXT I
   ENDIF

//   IF Valtype(::oBody)="O".AND.!::oBody:CheckExit(.F.)
//      RETURN .F.
//   ENDIF

   IF !::lChange .AND. !lFromValid // No hubo Ningún Cambio

     ::lCancel:=.T.

     IF ::nMode=1 .AND. ::nOption=1 // .AND.  (Empty(::GetValue(::oTable:cPrimary)) .OR. ::oTable:Count()=0)

//    ? COUNT(::cTable,::cScope),"CUANTOS",::cTable,::cScope

        IF Empty(::GetValue(::oTable:cPrimary)) .OR. MsgNoYes("Desea Cancelar ",::cTitle)

           ::nMode:=0  // Obliga la Salida

           IF COUNT(::cTable,::cScope)=0
              ::End()
           ENDIF

           IF !::RunScr(::cCancel) // programa que ejecuta la Cancelación
              RETURN .F.
           ENDIF

        ELSE

           IF !::RunScr(::cCancel) // programa que ejecuta la Cancelación
              RETURN .F.
           ENDIF

           RETURN .F.
        ENDIF

     ENDIF

     IIF( ::nMode=0 .AND. ValType(::oWnd)="O".AND.::oWnd:hWnd>0,::oWnd:End(),NIL)
     IIF( ::nMode=0 , ::End() , ::nOption:=0)

     RETURN .T.

   ELSEIF !::lChange .AND. lFromValid

      ::lCancel:=.T.

     RETURN .T.

   ENDIF

   IF lAsk.AND.!MsgNoYes("Desea Cancelar ",::cTitle)
      // ? "DIJO QUE NO"
      RETURN .F.
   ENDIF

   CursorWait()

   lCancel:=.T.

   IF !::RunScr(::cCancel) // programa que ejecuta la Cancelación
      RETURN .F.
   ENDIF

   // ? "DICE QUE SI, ¿QUE PASA?"

   // Si tiene cuerpo y esta incluyendo debe eliminar los cuerpos
/*
   IF Valtype(::oBody)="O"

     IF ::nOption=1 // Incluir
       ::SetMsg("Procesando Cancelación del Contenido")

       // ? ::oBody:oTable:cPrimary," CANCELA TODO"
       // ::oBody:oTable:DeleteAll(.T.) //oOdbc:Execute(cSql)

       cWhere:=""
       //oTable:=::oBody:oTable
       cField:=::oBody:oTable:cPrimary
       FOR I=1 TO LEN(::oBody:aHeadLink)
         // uValue   :=::Get( ::oBody:aHeadLink[I,1] )
         uValueOld:=::Get( ::oBody:aHeadLink[I,2] )
         cWhere:=cWhere+IIF( empty(cWhere) ,"" , " AND " )+::oBody:aHeadLink[I,2]+"='"+uValueOld+"'"
         cField:=cField+IIF( Empty(cField) ,"" , ","     )+::oBody:aHeadLink[I,2]
       NEXT I

       cSql  :="DELETE FROM "+::oBody:oTable:cTable+" WHERE "+cWhere
       oTable:=OpenTable("SELECT * FROM "+::oBody:oTable:cTable+" WHERE "+cWhere,.T.)
       oTable:cPrimary:=cField // Asigna Como Clave, la Relación con la Cabeza
       oTable:DeleteAll(.T.) // Borra los Asociados

       ::oBody:oTable:oOdbc:Execute(cSql, "Cancelando Cuerpo "+::oBody:oTable:cTable)

     ENDIF

     ::oBody:nOption:=0
     ::nOption:=0

     AjustButtons(SELF)

   ENDIF
*/
   IF lCancel .AND. !lFromValid
      ::lCancel:=.T.
     // ? "AQUI SI PUEDE CANCELAR Y CIERRA EL WND"
      IIF( ::nMode=0 .AND. ValType(::oWnd)="O".AND.::oWnd:hWnd>0,::oWnd:End(),NIL)
      IIF( ::nMode=0 ,  ::End() , ::nOption:=0)
   ELSEIF lCancel .AND. !lFromValid
      ::lCancel:=.T.
   ENDIF

   // Verificación del Browse
   IF ::nMode=0 .AND. ::nNumLbx>0 // Refrescan los Datos el ListBox

      CursorWait()
      oDpLbx:=GetDpLbx(::nNumLbx)

//      ? ::oTable:cTable,"cTable"

      IF ValType(oDpLbx)="O" .AND. ValType(::oTable)="O" .AND. ALLTRIM(oDpLbx:cTable)=ALLTRIM(::oTable:cTable)
         IF oDpLbx:oCursor:RecCount()=0 // Está Vacio
            oDpLbx:oWnd:End() // Debe Cerrarse
         ENDIF
       ENDIF

       oDpLbx:=NIL

    ENDIF



    IF ::nMode=1 .AND. ::lFirst
      //? "AQUI DICE CLOSE"
      ::lCancel:=.T.  // Ya fué Aceptado Cancelar
      ::Close() // Cerrar
      RETURN .T.
    ENDIF

    // ? "METHOD CANCEL DICE ",lCancel

RETURN lCancel

/*
// Methodo IsSaveHead()
// Llamado desde el Cuerpo para Generar el Primer Registro,
// Es necesario para la Integridad Relacional
*/
METHOD IsSaveHead(lUpdate,cPrimary)
   Local lResp:=.T.,I,cSql,nAt,oBody,uValue,cOldValue,oIntRef,oTable,cWhere,aLinkBody

   DEFAULT lUpDate:=.F.
   DEFAULT cPrimary:=::oTable:cPrimary

   IF lUpDate .AND. ::lSaveHead
      ::lSaveHead:=.F.
   ENDIF

  //? "issavehead"

  //  IF (::nOption!=1 .OR. ::lSaveHead) .AND. !lUpdate
   IF ::lSaveHead .AND. !lUpdate
      Return .T.
   ENDIF

   // Grabar la Cabeza
   ::oTable:lAppend:=.F.

   IF !lUpdate .AND. ::lSaveHead // Debe verificar que no exista
//      ? ::oTable:cPrimary
      uValue :=::oTable:GetValKey(::oTable:cPrimary)
      oTable :=OpenTable("SELECT * FROM "+::oTable:cTable+" WHERE "+BuildConcat(::oTable:cPrimary)+GetWhere("=",uValue)+" LIMIT 1",.T.)
      lUpdate:=oTable:RecCount()>0
      oTable:End()
   ENDIF

   // ? lUpdate,IIF( lUpdate,"Modificar"," Agregar Registro "),uValue

   IF lUpdate
  //   ? cPrimary,"cPrimary"
     cOldValue:=::oTable:GetValkey(cPrimary) // ::oTable:cPrimary)
     oIntRef:=INTREF(::oTable) //  Guarda todos los Valores
   ELSE
     cOldValue:=""
     ::oTable:AppendBlank()
     ::cWhere:=""
   ENDIF

//   ? cPrimary,"HAY QUE BUSCAR ",::oTable:cPrimary
//   ? ::cWhere,"para actualizar issavehead",uValue,"SI ESTA DEBE INCREMENTAR"

   DO WHILE ::nOption=1

//      ? "PARECE QUE ES AQUI"

      cSql:="SELECT "+::oTable:cPrimary+" FROM "+::oTable:cTable+" WHERE CONCAT("+::oTable:cPrimary+")"+;
            GETWHERE("=",::GetValue(::oTable:cPrimary))

      oTable:=OpenTable(cSql,.T.)

      IF oTable:RecCount()>0 // Encontrado
         oTable:End()
        // Debe Disparar los Incrementales
        FOR I=1 TO LEN(::aIncremental)
           uValue:=::Incremental(::aIncremental[I,1],::aIncremental[I,2],::aIncremental[I,3],::aIncremental[I,4])
           ::PutVar( ::aIncremental[I,1], uValue )
        NEXT I
        IF I=0
           ? "VALIDAR, NO HAY CAMPOS INCREMENTALES"
           EXIT
        ENDIF
        LOOP
      ENDIF

      oTable:End()

      cSql:="SELECT "+::oTable:cPrimary+" FROM "+::oTable:cTable+" ORDER BY "+::oTable:cPrimary+" DESC LIMIT 1"
      oTable:=OpenTable(cSql,.T.)
      oTable:End()
      EXIT

   ENDDO

   AEVAL(::oTable:aFields,{|a,i,cName,uValue|cName:=::oTable:FieldName(i),uValue:=::Get(cName),::oTable:Replace(cName,uValue)})

   // ? "ANTES DE GRABAR",::cWhere
   ::lSetFieldDef:=.T. // JN 15/09/2015 Restaurado en Nomina // JN 30/12/2014, inactiva los valores por defectos segun programa DPTABLESETDEF


   ::oTable:Commit(::cWhere,.f.,.t.,.t.) //  Commit( cWhere , lReload , lJoin , lIni )

   // IF !::oTable:lAppend // Verifica Integridad Relacional
   //  oIntRef:Run(::oTable) //  Guarda todos los Valores
   // IntRef(::oTable:cTable,cPrimary,cOldValue,::oTable:GetValkey(cPrimary) , .T.)
   // ENDIF

   ::lSaveHead:=.T.

   nAt:=AT(" WHERE ",::oTable:cSql)

   IF nAt>0 .AND. cPrimary=::oTable:cPrimary

      ::cWhere:=[ CONCAT(]+cPrimary+[)]+GetWhere("=",;
                ::oTable:GetValkey(cPrimary))

      cSql:=LEFT(::oTable:cSql,nAt+6)+::cWhere+" LIMIT 1"

      ::oTable:cSql:=cSql
      ::oTable:Reload()

      IF ValType(oIntRef)="O"
        oIntRef:Run(::oTable) //  Guarda todos los Valores
        oIntRef:End()
      ENDIF

   ENDIF
/*
   oBody:=::oBody

   IF ValType(oBody)="O" .AND. cPrimary=::oTable:cPrimary // Relación Encabezado Cuerpo

      aLinkBody:=GetLinkTable(::cTable,oBody:oTable:cTable  ,2,.T.)
      cWhere   :=""

      FOR I=1 TO LEN(oBody:aHeadLink)
         uValue:=::Get(oBody:aHeadLink[I,1]) // Valor referencia, Cabeza
         oBody:aBodyLink[I,2]:=uValue
         cWhere+=IIF( !EMPTY(cWhere) , " AND " , "" )+ALLTRIM(aLinkBody[I])+GetWhere("=",uValue) // "='"+cDataHead+"'"
      NEXT I

      oBody:oTable:cSql:=GetSqlSelect(oBody:oTable:cSql,.T.)+" WHERE "+cWhere

   ENDIF
*/

RETURN lResp

// Revisa Valores Faltantes para la Integridad Ref
METHOD ChkIntRef(cField) CLASS DPEDIT
    LOCAL aData:=GetFieldIntRef(::oTable:cTable),i,aResp:={}

    FOR I := 1 TO LEN(aData)
      IF (!","$aData[I,2]) .AND. Empty(::Get(aData[I,2]))
         AADD(aResp,{aData[I,1],aData[I,2]})
      ENDIF
    NEXT

RETURN aResp

/*
// Contador Ejecuta desde el Inicio de la Barra de Botones
*/
METHOD RECCOUNT(lIni) CLASS DPEDIT
   LOCAL oTable,oFont
   LOCAL aCampos:=IF(Empty(::cOrderBy),{},_VECTOR(::cOrderBy)),cFieldX:=""
   LOCAL cDesde :="",cHasta:=""
   LOCAL cWhere :=nil // IF(Empty(::cScope),""," WHERE "+::cScope)
   LOCAL cOper  :="=",cSql

   IF Empty(::oTable) // 31/12/2023
      RETURN NIL
   ENDIF

   ::cPrimary:=::oTable:cPrimary
   cWhere :=IF(Empty(::cScope),""," WHERE "+::cScope)


   DEFAULT lIni:=.T. // Inicia y obtiene el primer y Ultimo Registro, Cuando se busca o lista el registro debe obtener la posicion del Registro

   AEVAL(aCampos,{|a,n| cFieldX:=cFieldX+IF(Empty(cFieldX),"",",")+a })

   IF lIni

     ::nRecCount:=COUNT(::oTable:cTable,::cScope)
     ::nRecno   :=1

     // Primero
     cSql:="SELECT "+::cPrimary+" FROM "+::oTable:cTable+cWhere+" ORDER BY CONCAT("+::cPrimary+") LIMIT 1"
     oTable:=OpenTable(cSql,.T.)
     AEVAL(aCampos,{|a,n| cDesde :=cDesde +IF(Empty(cDesde ),"",",")+CTOSQL(oTable:FieldGet(a)) })
     ::cDesde:="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cDesde+")"
     oTable:End()

     // Ultimo
     cSql:="SELECT "+::cPrimary+" FROM "+::oTable:cTable+cWhere+" ORDER BY CONCAT("+::cPrimary+") DESC LIMIT 1"
     oTable:=OpenTable(cSql,.T.)
     AEVAL(aCampos,{|a,n| cHasta :=cHasta +IF(Empty(cHasta ),"",",")+CTOSQL(oTable:FieldGet(a)) })
     ::cHasta:="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cHasta+")"
     oTable:End()

// ? GETPROCE()

   ELSE

     // Actual
     ::cActual:=""
     cOper  :="<="
     AEVAL(aCampos,{|a,n| ::cActual:=::cActual+IF(Empty(::cActual ),"",",")+CTOSQL(::Get(a)) })

//? ::cActual,"::cActual",cFieldX,::cPrimary,GETPROCE()

     ::cActual:="CONCAT("+cFieldX+")"+cOper+"CONCAT("+::cActual+")"
     // 29/10/2023 Agregar ::cScope
     ::nRecno:=COUNT(::oTable:cTable,::cActual+IF(!Empty(::cScope)," AND "+::cScope,"")) //29/10/2023 + ::cScope
     ::lBof:=(::nRecno=1)
     ::lEof:=(::nRecno=::nRecCount)

// ? ::nRecno,::nRecCount,CLPCOPY(oDp:cSql)

   ENDIF

   IF ValType(::oSayRecord)="O"

     DEFINE FONT oFont NAME "Tahoma" SIZE 0, -10 BOLD

     ::oSayRecord:SetColor(oDp:nClrYellowText,oDp:nClrYellow)
     ::oSayRecord:SetFont(oFont)
     ::oSayRecord:Refresh(.T.)


//? "aqui deben pintar el recno",getproce(),::oSayRecord:ClassName()


   ENDIF

// ? ::GetWhereMax(::cOrderBy, "", .F.)
// ? ::GetWhereMax(::cOrderBy, "", .T.)
// ? ::nRecCount,"::nRecCount",cFieldX,
// ? ::cDesde,::cHasta
/*
 LOCAL cWhere,nCuantos:=0,oTable,cOrderBy,aCampos
//LOCAL aCampos:=_VECTOR(::cOrderBy)
 LOCAL cOper:="<="
 LOCAL cDesde:="",cHasta:="",cFieldX:=""

 IF ::oTable=NIL
    RETURN NIL
 ENDIF

 ::cOrderBy:=::oTable:cPrimary
 ::cPrimary:=::oTable:cPrimary

// ? ::cScope,"::cScope"

 cOrderBy:=::oTable:cPrimary
 aCampos :=_VECTOR(cOrderBy) // ::oTable:cPrimary)

 DEFAULT lIni:=.T. // Inicia y obtiene el primer y Ultimo Registro, Cuando se busca o lista el registro debe obtener la posicion del Registro

 AEVAL(aCampos,{|a,n| cFieldX:=cFieldX+IF(Empty(cFieldX),"",",")+a,;
                      cHasta :=cHasta +IF(Empty(cHasta ),"",",")+CTOSQL(::Get(a)) })

//AEVAL(aCampos,{|a,n| cFieldX:=cFieldX+IF(Empty(cFieldX),"",",")+a,;
//                      cHasta :=cHasta +IF(Empty(cHasta ),"",",")+CTOSQL(oTable:FieldGet(a)) })

 cHasta:="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cHasta+")"

 cOper:=">="
 cWhere :=::GetWhereMax(cOrderBy , "", .f.) // JN 14/04/16
 oTable :=OpenTable("SELECT "+cOrderBy+" FROM "+::cTable+" "+cWhere)
 AEVAL(oTable:aFields,{|a,n| cDesde :=cDesde +IF(Empty(cDesde ),"",",")+CTOSQL(oTable:FieldGet(n)) })
 oTable:End()

//? cDesde,"cDesde"

 ::cSqlIni:=oDp:cSql

// oTable:Browse()
 cDesde  :="CONCAT("+cFieldX+")"+cOper+"CONCAT("+cDesde+")"
 cWhere  :=::cScope+IF(Empty(::cScope),""," AND ")+"("+cDesde+" AND "+cHasta+")"

 IF lIni
   ::nRecCount:=COUNT(::cTable,::cScope,::oDb)
   ::cSqlFin:=oDp:cSql
   ::nRecno   :=::nRecCount
 //? ::nRecCount,"::nRecCount",CLPCOPY(oDp:cSql)
 ELSE
   ::nRecno   :=COUNT(::cTable,cWhere,::oDb)
//? COUNT(::cTable,cWhere),"Encontrado",CLPCOPY(oDp:cSql)
   ::oSayRecord:Refresh(.T.)
 ENDIF

 IF ValType(::oSayRecord)="O"
   ::oSayRecord:Refresh(.T.)
 ENDIF

// ? cDesde,cHasta,::cTable,::cScope,::nRecCount
*/
RETURN NIL

/*
// Busca el Where Según el Valor mas alto
*/
METHOD GetWhereMax(cField , cWhere , lMax) CLASS DPEDIT

   LOCAL oTable,uValue,aFields:={},I,cList,nCount:=0,nT1:=SECONDS()

   DEFAULT cWhere:="",lMax:=.T.

// ? ::cScope

   IF !","$cField

      IF !Empty(::cScope) .AND. !Empty(cWhere)

         cWhere:=cWhere +   " " + IIF( Empty(cWhere) , " WHERE ", " AND " ) +::cScope

      ENDIF

      IF !Empty(::cScopeF) .AND. !Empty(cWhere)

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

      uValue:=SQLGET(::cTable,cField,cWhere+" ORDER BY "+cField+IIF( lMax , " DESC ", "" )+" LIMIT 1")

      IF !Empty(uValue)

        RETURN " WHERE "+cField+GetWhere( "=" , uValue )

      ENDIF

      RETURN ""

   ENDIF

   nCount:=MYCOUNT(::cTable,::cScope,::oDb)

   IF nCount=0
      RETURN ""
   ENDIF

   aFields:=_VECTOR(cField)

   IF !Empty(::cScope)

     cWhere:=cWhere + " " + IIF( Empty(cWhere) , " WHERE ", " AND " ) +::cScope

   ENDIF

   FOR I := 1 TO LEN(aFields)

     oTable:=OpenTable("SELECT "+IIF( lMax , "MAX", "MIN" )+"("+aFields[I]+") FROM " + ::cTable + cWhere , .T. ,::oDb )

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

RETURN cWhere


/*
// Guardar Registro
*/
METHOD SAVE( lModifica ) CLASS DPEDIT
    // Salva los Datos del Registro Encabezado //
    LOCAL I,uValue,cName,oGets,uValueOld,cWhere:="",cSql,cKey,aFields,cField:="",aKey //,oDlg
    LOCAL oDpLbx,oTable,cUpDate:="" //,cSql
    LOCAL aFieldsCommon:={},cNewWhere:="",lChange:=.F.,nOption:=::nOption
    LOCAL aIntRef:={},oObj,nLen

    DEFAULT lModifica:=.F. // Indica que debe Activar Modificar, Luego de Incluir

    CursorWait()

//    IF ::nOption=3
//        EJECUTAR("DPAUDELIMOD",Self)
//    ENDIF

    // Verifica si hay cambios en DpBoDY

    IF ValType(::oBody)="O"
       IF !::oBody:CheckExit(.T.) // Verifica salica con Grabar
          RETURN .F.
       ENDIF
    ENDIF

    SysRefresh()

    IF ValType(::cPreSave)="C" .AND. !::RunScr(::cPreSave) // programa que ejecuta la Cancelación
       RETURN .F.
    ENDIF

    /*
    // 04/02/2017 , Asigna Valores del Formulario antes de Revisar Integridad Referencial
    */
    FOR I := 1 TO LEN(::oTable:aFields)

       cName :=ALLTRIM(UPPE(::oTable:FieldName(i)))
       uValue:=::Get( cName )

       // Campos Caracter (No memos ni Blob), 30/12/2016 , Solo asigna los campos Caracter segun su longitud
       IF ::oTable:aFields[I,2]="C" .AND. ValType(uValue)="C"
         uValue:=LEFT(uValue,::oTable:aFields[I,3])
       ENDIF

       ::oTable:Replace(cName,uValue)

    NEXT

    /*
    // 04/2/2017, solo se ejecuta antes del begin sequence
    // Aqui se agregan los registros vinculados inexistentes
    */
    IF ValType(::oTable)="O"  .AND. ::lCreaRegIntRef

       // ::oTable:CreaRegIntRef() JN 8/2/2017 Suspendido genera incidencias
       ::oTable:lCreaRegIntRef:=.F. // Ya lo ejecutó

    ENDIF

    // 19/04/2023 Registros de Auditoría
    IF !Empty(::cKeyAudita)
       ::oTable:cKeyAudita:=::cKeyAudita
    ENDIF

    IF ::nOption=3
       EJECUTAR("DPAUDELIMOD",Self)
    ENDIF

    ::DestroyMsg()

//? ::cFieldFile,::nFileMain,"::cFieldFile,::nFileMain"

    IF !Empty(::cFieldFile) // .AND.
      ::Set(::cFieldFile,::nFileMain )
    ENDIF

    // AEVAL(::oDlg:aControls,{|o,n|o:nLastKey:=0})

    IF !Empty(::cTable) .AND. ::lSqlBegin

      /*
      // Asigna La Longitud de todos los Campos segun Ancho
      */

      DpSqlBegin(NIL,NIL , ::cTable)

// ? "BEGIN ::cTable",::cTable

    ENDIF

    ::oDlg:nLastKey:=0

/*    FOR I := 1 TO LEN(::oTable:aFields)

       cName :=ALLTRIM(UPPE(::oTable:FieldName(i)))
       uValue:=::Get( cName )

       ::oTable:Replace(cName,uValue)

    NEXT */

    // Revisión del Campo Memo


    IF !EMPTY(::aMemo)

       ::aMemo[7]:=DPMEMOSAVE(::aMemo[7],::aMemo[8],::aMemo[9])
       ::Set(::aMemo[1],::aMemo[7]) // Asigna el Valor

       ::oTable:Replace(::aMemo[1],::aMemo[7]) // Aqui es Asigando al Objeto TTABLE

//     ? ::aMemo[1],::aMemo[7],"AQUI DEBE GUARDAR"

       IF ValType(::oEditMemo)="O"   // Edición de Campo Memo
         ::oEditMemo:oWnd:End()
         ::oEditMemo:=NIL
       ENDIF

    ENDIF

//   IF Empty(::oTable:aFields)
//        ::oTable:aFields:=::aFieldsTable
//   ENDIF

    ::oTable:lAppend:=.F.

    /*
    // Necesito Obtener los Valores para DPLBX
    */

    // ::LbxRefresh()

    IF ::nNumLbx>0 // Refrescan los Datos el ListBox


       CursorWait()

       oDpLbx:=GetDpLbx(::nNumLbx)

       IF ValType(oDpLbx)="O" .AND. ALLTRIM(oDpLbx:cTable)=ALLTRIM(::oTable:cTable)

          IF oDpLbx:oCursor:RecCount()=0 // Ya Tiene el Primer Registro
             AEVAL(oDpLbx:aBtns,{|o| IF( ValType(o)="O",o[8]:Enable(),NIL)})
          ENDIF

          aKey:=_VECTOR(oDpLbx:cOrderBy)
          cKey:=IIF( Len(aKey)>0,::Get(aKey[1]),NIL)

          IF .F. .AND. LEN(aKey)>1 .AND. ValType(cKey)="C"

             FOR I=2 TO LEN(aKey)
                aKey[I]:=ALLTRIM(aKey[I])
                uValue :=::Get(aKey[I])

                IF ValType(uValue)="C"
                   nLen  :=ASCAN(oDpLbx:oCursor:aFields,{|a,n|ALLTRIM(a[1])=ALLTRIM(aKey[I])})
                   uValue:=IIF(nLen=0,uValue,LEFT(uValue,oDpLbx:oCursor:aFields[nLen,3]))
                ENDIF

                // ? Ï,aKey[I],oDpLbx:cOrderBy
                // cKey:=cKey+CTOO(::Get(aKey[I]),"C")
                cKey:=cKey+CTOO(uValue,"C")

               NEXT
          ENDIF

       ELSE

          oDpLbx:=NIL

       ENDIF

    ENDIF

    IF ::nOption=1.AND.!::lSaveHead

       ::oTable:AppendBlank()
       ::cWhere:=""

    ELSE

      cWhere:=::cWhere

      // ? ::cWhere, "es quien dice como grabar"

    ENDIF

/*
// 04/02/2017 , fue removido en la Parte Superior Antes de Revisar la Integridad Referencial
    FOR I := 1 TO LEN(::oTable:aFields)

       cName :=ALLTRIM(UPPE(::oTable:FieldName(i)))
       uValue:=::Get( cName )

       // Campos Caracter (No memos ni Blob), 30/12/2016 , Solo asigna los campos Caracter segun su longitud
       IF ::oTable:aFields[I,2]="C" .AND. ValType(uValue)="C"
          uValue:=LEFT(uValue,::oTable:aFields[I,3])
       ENDIF

       ::oTable:Replace(cName,uValue)

    NEXT
*/

    ::cWhere:=GetSqlWhere(" "+::cWhere)

    cWhere:=cWhere +;
            IIF( oDp:cTypeBD$"DBF.ADS.ADT.MSSQL" ,"" , IIF(::nOption=1 .OR. " LIMIT "$cWhere , "" , " LIMIT 1" ))

    //? oDp:cTypeBD

    cNewWhere:=::oTable:GetWhereKey(::oTable:cPrimary)+;
                IIF( oDp:cTypeBD$"DBF.ADS.ADT.MSSQL",""," LIMIT 1")

    // ? cWhere, "ANTES DEL BUILDLIMIT"," EL OTRO ES ",cNewWhere
    cWhere:=BUILDLIMIT(cWhere)

    // ? cNewWhere,oDp:cTypeBD,"SAVE",cWhere
    // ::oTable:Commit(cWhere,NIL,NIL,.T.) // cWhere + IIF( ::nOption=1 .OR. " LIMIT "$cWhere , "" , " LIMIT 1" )) // " WHERE "+::oTable:IndexKey(1)+"= '"+::oTable:GetDataKey(1)+"'")
    // Nueve Where, según Nuevo Registro
    // ? "MODIFICARA BAJO ESTE WHERE : ",cWhere,::oTab
    // ? cWhere,"cWhere, para grabar y listo"

    IF (aIntRef:=::ChkIntRef(),!EMPTY(aIntRef)) .AND. ::lIntRef
       EJECUTAR("DPMSGINTREF",aIntRef,SELF)
       RETURN .F.
    ENDIF

//    IF ::nOption<>1 // Modificar
//      cWhere:=::oTable:cWhere
//    ENDIF

    //? cWhere,"GRABAR",::oTable:cWhere,"DESDE OTABLE"
    // oDp:lMySqlNativo:=.F.

//    ? ::oTable:ClassName(),"TDPEDIT"

//    IF ValType(::oTable)<>"O"
//       MensajeErr("No puede ejecutar Locktable, ::oTable es null")
//    ENDIF

    IF ::lSqlBegin
      ::LockTable(::oTable)
    ENDIF

    IF !Empty(::cFieldFile) // .AND.
       ::oTable:Replace(::cFieldFile,::nFileMain)
    ENDIF

    oDp:cSqlCommit:=""
    oDp:cError    :=""
    oDp:cSqlErr   :=""
                           // Where , lReload , lJoin , lIni , lCreaIntRef,lError
    IF !::oTable:Commit(cWhere,.T.,NIL,.T.,NIL,::lErrorSave) // cWhere + IIF( ::nOption=1 .OR. " LIMIT "$cWhere , "" , " LIMIT 1" )) // " WHERE "+::oTable:IndexKey(1)+"= '"+::oTable:GetDataKey(1)+"'")

       IF ::lSqlBegin
         DpSqlRollBack()
         ::UnLockTable()
       ENDIF

       // oDp:cError:=oDp:oMySqlCon:oError:GetError()
       IF Empty(oDp:cSqlErr)
          oDp:cSqlErr:=MemoRead("TEMP\SQLMSGERROR.TXT")
       ENDIF

       //IF Empty(oDp:cError)
       //   oDp:cError:=""
       // ENDIF
       // cSql:=oDp:cSqlCommit
       // CLPCOPY(oDp:cSql)

       DPWRITE("TEMP\"+::oTable:cTable+"_GETPROCE.SQL",GETPROCE())

       // ? GETPROCE(),"GETPROCE()"

       MensajeErr("Registro no pudo almacenado"+CRLF+" Revise Referencias con Otras Tablas"+CRLF+oDp:cSqlErr,"Registro no Grabado")

       DPFOCUS(::oFocus)
       // DPWRITE("TEMP\"+::oTable:cTable+"_INSERT.SQL",oDp:cSql+" "+oDp:cError)

       RETURN .F.

    ENDIF

    IF ::lSqlBegin
      ::UnLockTable()
    ENDIF

    // ? ::oTable:GetWhereKey(::oTable:cPrimary),"save"
    // ? ::oTable:GetDataKey(NIL,::oTable:cPrimary)

    AUDITAR(IIF(::nOption=1,"DINC","DMOD" ) ,;
            NIL ,::cTable , ::oTable:GetDataKey(NIL,::oTable:cPrimary),NIL, SELF)


//?  ::cTable,::oTable:GetDataKey(NIL,::oTable:cPrimary),"AUDITAR"

    ::oDlg:nLastKey:=0

    IF !EMPTY(::aMemo) // Campo Memo
      (::aMemo[7]:=0,::aMemo[8]:="",::aMemo[9]:="")
    ENDIF

//    ? "NUEVO QUERY DESPUES DE GRABAR ",::oTable:cSql

    IF ValType(::oIntRef)="O" // Integridad Relacional

      ::oTable:cSql:=GetSqlSelect(::oTable:cSql,.T.)+" "+cNewWhere
      ::oTable:Reload()

      ::cWhere:=GetSqlWhere(::oTable:cSql)
      ::oIntRef:Run(::oTable) // Solo Para Modificar
      ::oIntRef:End()
      ::oIntRef:=NIL

    ELSE
      // ? "NO HAY INTREL"
    ENDIF

    // El Documento Requiere Posicionar el Nuevo Registro
    IF !EMPTY(cNewWhere) .AND. ::nMode=1
       // ::oTable:cSql:="SELECT * FROM "+::oTable:cTable+" "+cNewWhere

// ? ::oTable:cSql,"ANTES"

       IF !" WHERE "$::oTable:cSql
          MensajeErr("Requiere Clausula Where "+::oTable:cSql)
       ELSE
          ::oTable:cSql:=GetSqlSelect(::oTable:cSql,.T.)+" "+cNewWhere
       ENDIF

// ? ::oTable:cSql,"AQUI GENERA INCIDENCIA"

       ::oTable:Reload()

    ENDIF
    // ? "POSTSAVE",::cPostSave
    // ::oWnd:nLastKey:=0
    // IIF(ValType(::oDlg)="O" , ::oDlg:nLastKey:=0 , NIL )

    // AQUI TODO OK
// OJO 4/2

    IF ::lSqlBegin
      DpSqlCommit()
    ENDIF

    IF ValType(::cPostSave)="C" .AND. !EMPTY(::cPostSave)
       ::RunScr(::cPostSave) // programa que ejecuta la Cancelación
    ENDIF

    IF ::nOption=1 .AND. !lModifica

      ::oTable:Reload() // Recarga nuevamente el Query


// ? "POSTGRABAR",ErrorSys(.T.)


      // IF .T. // No Reinicia las Variables
      // ? LEN(::aRepeatGet),"aRepeatGet"
      ::CreateVars(,,,.F.,,::aRepeatGet) //  ::oTable ) // Regenera las Variables para seguir Incluyendo
      // ENDIF

      FOR I=1 TO LEN(::aIncremental)

        uValue:=::Incremental(::aIncremental[I,1],::aIncremental[I,2],::aIncremental[I,3],::aIncremental[I,4])

        ::Set( ::aIncremental[I,1], uValue )

      NEXT I

      ::oDlg:nLastKey:=0

// ? "ahora refresca los controles",::oDlg:ClassName()
/*
// Reemplazado por SETFOCUSGET
      FOR  I := 1 TO LEN(::oDlg:aControls)
        IF "GET"$::oDlg:aControls[I]:ClassName()// $"TGET,"
          oGets:=IIF( oGets=NIL , ::oDlg:aControls[I] , oGets )
          ::oDlg:aControls[I]:Refresh()
         ENDIF
      NEXT
*/
      // Debe Borrar el Contenido del Browse

      IF ValType(::oBody)="O"

         ::oBody:NewJob()
/*
         ::oBody:oTable:aDataFill:={} // Esta Vacia
         ::oBody:oBrw:aArrayData :={}
         ::oBody:AppendBlank(.T.)
         // ojo ::oBody:oBrw:aArrayData:=::oBody:aData
         ::oBody:oBrw:Refresh()
         ::oBody:ActivateGets()
         IF !EMPTY(::oBody:cLoad)
           ::oBody:RunScr(::oBody:cLoad)  // Inicia Nuevamente la Carga de Datos
         ENDIF

         AjustButtons(SELF)
*/
       ENDIF

       // Permitir Cancelar
       ::lChange  :=.F.
       ::aCopyEdit:=aClone( ::aVars ) // Copia los Valores y asegurar el Cancel

//    ELSEIF ::nOption=3 // Modificar
//
//       IIF( ValType(::oWnd)="O",::oWnd:End(),::oDlg:End())

    ENDIF


    IF ValType(oDpLbx)="O" // nNumLbx>0 // Refrescan los Datos el ListBox

        oDpLbx:Refresh(::nOption=1,cKey)

/*       CursorWait()

       oDpLbx:=GetDpLbx(::nNumLbx)

       // ? ::nNumLbx,"::nNumLbx"
       // cKey  :=::oTable:GetValKey(::oTable:cPrimary)
       // ? oDpLbx:cTable,::oTable:cTable
       IF ValType(oDpLbx)="O" .AND. ALLTRIM(oDpLbx:cTable)=ALLTRIM(::oTable:cTable)

          IF oDpLbx:oCursor:RecCount()=0 // Ya Tiene el Primer Registro
             AEVAL(oDpLbx:aBtns,{|o|o[8]:Enable()})
          ENDIF

         cKey:=::Get(oDpLbx:cOrderBy)

         ? cKey,"cKey",oDpLbx:cOrderBy

         IF !EMPTY(oDpLbx:cOrderBy)
            cKey  :=::oTable:GetValKey(oDpLbx:cOrderBy)
         ENDIF

         oDpLbx:Refresh(::nOption=1,cKey)

       ENDIF

       oDpLbx:=NIL
*/
    ENDIF

    IF lModifica  // Activa Modificar Luego de Incluir

      ::nOption:=3

      IF empty(::oTable:aDataFill) // Hace la Recarga
         ::oTable:End()
         ::oTable:=OpenTable(::oTable:cTable,.T.)
      ENDIF

      AUDITAR("DMOD" , NIL ,::cTable , "cClave" ,NIL, SELF)

      FOR I := 1 TO LEN(::oTable:aFields)
        ::oTable:aDataFill[1,I]:=::oTable:Get(::oTable:aFields[I,1])
      NEXT

      aFields:=_VECTOR(::oTable:cPrimary,IIF("+"$::oTable:cPrimary,"+",","))

      ::cWhere:=""

      FOR I := 1 TO LEN(aFields)
         ::cWhere+=IF(!EMPTY(::cWhere)," AND ","") + aFields[i]+"='"+::oTable:Get(aFields[i])+"'"
      NEXT

      ::cWhere:=" WHERE "+::cWhere

      IIF( oGets!=NIL,oGets:SetFocus(),NIL )

//      AjustButtons(SELF)
//    IF ValType(::oBody)="O"
//       ::oBody:nOption:=0 OJO AQUI
//    ENDIF

      CursorArrow()

// ? GETPROCE()

      RETURN .T.

   ENDIF

   IF (!::lAutoInclude .OR. ::nOption=3) .AND. ::nMode!=1 // Modificar
      ::lCancel:=.T.
      IIF( ValType(::oWnd)="O" ,::oWnd:End(),::oDlg:End())
   ENDIF

// AEVAL(::aScrollGets,{|o|o:KillEdit(),o:oBrw:Gotop(),o:oBrw:Refresh()})
   AEVAL(::aScrollGets,{|o|o:KillEdit(),o:oBrw:Gotop(.T.)})

   CursorWait()

   // Cierra Edición del Campo Memo

   SysRefresh(.T.)

//   ? ::nMode,::nOption,"AL GRABAR"

   IF ::nMode=1 .AND. ::nOption=1

      IF ::lFirst
        ::oTable:GotoMax(NIL,::cScope)
        ::oTable:Reload()
      ENDIF
      ::lFirst:=.F.

      ::Load(::nOption)

   ELSEIF ::nMode=1 .AND. ::nOption=3

      // ? ::CODIGO,::oTable:CODIGO," YA MODIFICO",::NOMBRE
      // ::oTable:Reload()
      // ? ::oTable:RecCount(),::oTable:CODIGO,::oTable:Fcount()
      // ::CreateVars()
      // ? ::CODIGO
      // ::nOption:=0
      // ::CheckBtn()
      // SetFocusGet(::oDlg,NIL,.T.) // APunta al primer Get

      ::oTable:cSql:="SELECT * FROM "+::oTable:cTable+" "+cNewWhere

      // ? ::oTable:cSql
      ::oTable:Reload()
      ::nOption:=0
      IF ValType(::oBody)="O"
         AEVAL(::oBody:oBrw:aCols,{|o,n|o:nEditType:=0})
         ::oBody:nOption:=0
         AjustButtons(SELF)
      ENDIF
      ::Load(0)
      ::oWnd:nLastkey:=0
      ::oDlg:nLastKey:=0

   ENDIF

   // Refresca los Controles
   IF ::nMode=0 .AND. ::nOption=1 .AND. ValType(::oTable)="O" // Sigue Incluyendo

//      ? "AQUI"
      ::RunScr(::cLoad)

      IF ValType(::oWnd)="O" .AND. ::oWnd:hWnd=0
         ::End()
         RETURN .T.
      ENDIF

      // ErrorSys(.T.)
      // ? "AQUI"

      FOR I := 1 TO LEN(::oTable:aFields)
         cField:="O"+::oTable:aFields[I,1]
//         ? cField,I
         IF ::ISDEF(cField) // Objeto
           oObj:=::Get(cField)
//           ? cField,oObj:ClassName()
           IF !oObj:ClassName()$"TRADMENU" .AND. ValType(oObj)="O"
             uValue:=oObj:VarGet() // ::Get(cField):VarGet()
             ::Get(cField):VarPut(uValue,.T.)
           ENDIF
//             ? "REPINTA",uValue
//           ENDIF
         ENDIF
      NEXT

      // ::Load(::nOption)

   ENDIF

   // ::oWnd:nLastkey:=0
      ::oDlg:nLastKey:=0
   // oEditGet:KeyBoard(0)
   // ::oFocus:SetFocus()
   // IF
   // ENDIF
   // ::oFocus:nLastKey:=0
   //? ValType(::oFocus),"::oFocus"

   IF ValType(::oFocus)="O"
      ::oFocus:SetFocus()
      ::oFocus:nLastKey:=0
      SetFocusGet(::oDlg,NIL,.F.,::oFocus) // :ClassName() // APunta al primer Get
      ::oFocus:ForWhen()
      DpFocus(::oFocus)
   ENDIF

   ::oDlg:nLastKey:=0
//  DpWait() // Genera un Dialogo para Detener las Teclas
    CursorArrow()
    ::lSaveHead:=.F. // No ha grabado el encabezado

//    ? "TERMINO DE GRABAR"
//  ? "LISTO"

RETURN .T.

//----------------------------------------------------------------------------//
METHOD Paste() CLASS DPEDIT

  LOCAL cVar:="o"+::oTable:cPrimary,oObj

  IF ::ISDEF(cVar)
     oObj:=::Get(cVar)
  ENDIF

  IF EJECUTAR("DPRESTFIND",::cTable,::oTable:cPrimary,oObj)
     FrmFindBlq(Self,oObj,::oTable:cPrimary)
  ENDIF

RETURN NIL


METHOD ShowMsg(cToolTip,oControl,nClrPane,nClrText,nRow, nCol) CLASS DPEDIT
   local oFont, aPos, hOldFont, nTxtWidth := 0, nTxtHeight
   local aToolTip:={}, nLenToolTip:=0

   DEFAULT oControl:=GetControl() ,;
           nClrPane:=oDp:nMsgToolClrPane,;
           nClrText:=oDp:nMsgToolClrText

IF .T.

   IF ValType(oControl)!="O"
      MensajeErr("no se puede mostrar "+cToolTip+" en ShowMsg(" )
      RETURN NIL
   ENDIF

   ::DestroyMsg()

/*
   IF ValType(::oToolMsg)="O"
     ::oToolMsg:End()
     ::oToolMsg:=NIL
   ENDIF
*/

   DEFAULT nCol := 7-7, nRow := oControl:nHeight() + 7-5

   // ? "Mensaje" , ValType(cToolTip) , cToolTip,len(cToolTip)
   // , ;
   //        cToolTip := "Mensaje de Error "
   //  ::cToolTip
   // cToolTip:=cToolTip

//   if .T. // oToolMsg == nil

      DEFINE FONT oFont NAME GetSysFont() SIZE 0, -8

      DEFINE WINDOW ::oToolMsg FROM 0, 0 TO 1, 5+2 ;
         STYLE nOr( WS_POPUP, WS_BORDER ) ;
         COLOR 0, nClrPane OF oControl

// COLOR 0, NIL OF oControl // Self

         // COLOR 0, RGB( 255, 255, 225 ) OF oControl // Self

      aPos = { nRow, nCol }

      aPos = ClientToScreen( oControl:hWnd, aPos )

      nTxtHeight := Max(14, GetTextHeight( ::oToolMsg:hWnd )-2)

      // aToolTip = Array( nLenToolTip := MLCount( cToolTip, 254 ) )
      // cToolTip:="el mensaje de error"
      // ? cToolTip,"Mensaje"

      cToolTip   :=STRTRAN( cToolTip, CHR(10) ,"" )
      aToolTip   :=_VECTOR( cToolTip, CHR(13))

      // ? aToolTip[1],"uno"

      nLenToolTip:=LEN(aToolTip)

      AEval( aToolTip, {|c, n| nTxtWidth := Max( nTxtWidth, GetTextWidth( 0, aToolTip[ n ], oFont:hFont ) + 7 ) } )

      // ? nTxtWidth,"ancho"

      if aPos[ 2 ] + nTxtWidth + 3 > GetSysMetrics( SM_CXSCREEN )
         aPos[ 2 ] = GetSysMetrics( SM_CXSCREEN ) - nTxtWidth - 3
      endif

      ::oToolMsg:Move( aPos[ 1 ], aPos[ 2 ], nTxtWidth, nTxtHeight * nLenToolTip + 3 )
      ::oToolMsg:Show()

      hToolTip = ::oToolMsg:hWnd

      SetBkMode( ::oToolMsg:GetDC(), 1 )
   //   SetTextColor( ::oToolMsg:hDC, CLR_BLUE )
      SetTextColor( ::oToolMsg:hDC, nClrText )
      hOldFont = SelectObject( ::oToolMsg:hDC, oFont:hFont )
      AEval( aToolTip, {| c, n | TextOut( ::oToolMsg:hDC, n * nTxtHeight - (nTxtHeight-1), 2, aToolTip[ n ] ) } )
      SelectObject( ::oToolMsg:hDC, hOldFont )
      ::oToolMsg:ReleaseDC()
      oFont:End()

      oControl:oToolMsg:=::oToolMsg

   endif

//ENDIF


   oDp:oToolMsg:= ::oToolMsg
   lToolTip = .t.

return nil

METHOD SETBTNBAR(nAlto,nAncho,oBar,nCol) CLASS DPEDIT
  LOCAL oMdi:=SELF

  DEFAULT nCol:=15,;
          oBar:=::oBar

  ::oBar:=oBar

  __objAddData(oBar,"CARGO" )
  __objSendMsg(oBar,"CARGO",ARRAY(10))

  oBar:Cargo:=ARRAY(10)
  oBar:Cargo[1]:=.T.

  ::lBarDef:=.F. // Evitar que las definiciones del control se oriente a la barra de botones

  oBar:SetColor(NIL,oDp:nGris)

  AEVAL(oBar:aControls,{|oBtn,n|oBtn:CARGO:="TXBUTTON",oBtn:nInitId:=0,oBtn:nResult:=0,oBtn:Move(5,nCol+1),oBtn:SetSize(nAncho,nAlto),nCol:=nCol+oBtn:nWidth()})

  AutoId(oBar)

  IF !__objHasMsg( oMdi, "oBrw") .AND. ValType(::oBrw)="O"

    // ::bValid   :={|| EJECUTAR("BRWSAVEPAR",oMdi)}

    EJECUTAR("BRWRESTOREPAR",oMdi)

  ENDIF

//  LoadFileEdt(oMdi:cFileEdit,oMdi)
//
//  PutControls(oBar,.t.,.t.,nil,NIL,oMdi)
//
  SetWndDefault( oBar)

  oBar:=::oBar

  ::lMdiBar:=.T.

RETURN NIL



STATIC FUNCTION PutPostEdit(oBrw , oBody , nCol , cField)
     LOCAL lResp,oGet,lDo:=.f.,cPicture:=""
     LOCAL nField:=oBody:oTable:FieldPos(cField)

     // ? nField,cField

     IF oBody:oTable:aFields[nField,2]="L" // Logico
        oBrw:aCols[nCol]:bStrData   := { |oCol| IIF( oBrw:aArrayData[oBrw:nArrayAt,nCol], "Si", "No ")}
     ENDIF

     IF oBody:oTable:aFields[nField,2]="N" // Numerico

        IF EMPTY(oBrw:aCols[nCol]:cSayPicture)
           oBrw:aCols[nCol]:cSayPicture:=ALLTRIM(oBody:oTable:GetPicture(cField,.T.))
        ENDIF

        IF EMPTY(oBrw:aCols[nCol]:cEditPicture)
           oBrw:aCols[nCol]:cEditPicture:=ALLTRIM(oBody:oTable:GetPicture(cField,.F.))
        ENDIF

//      ? "AQUI",oBrw:aCols[nCol]:cEditPicture
//      oBrw:aCols[nCol]:bStrData   := { |oCol| IIF( oBrw:aArrayData[oBrw:nArrayAt,nCol], "Si", "No ")}
        oBrw:aCols[nCol]:bStrData   := { |oCol| TRAN(oBrw:aArrayData[oBrw:nArrayAt,nCol],oCol:cSayPicture)}
     ENDIF

     oBrw:aCols[nCol]:lValid:=IIF( nCol=1,.f.,.t.)    // Indica que es Valida la Data

     oBrw:aCols[nCol]:bOnPostEdit   := {|o,x| oBrw:aArrayData[ oBrw:nArrayAt, nCol ] := x,;
                                              oBody:Set(cField,x),;
                                              IIF( o:nEditType=2 .OR. o:nEditType=5 .OR. o:nEditType=4,(lResp:=oBody:RunScrCol(NIL , nCol , o , cField ,x , .T. ,o:cValid),;
                                              oBrw:aCols[nCol]:lValid:=lResp ),NIL),;
                                              oBody:CheckCol(o,nCol),VP("lRunBtn",.F.),lResp}
     // }
     // oBrw:aCols[nCol]:bEditValid    :={||MsgAlert("no funciona"),.f.}

     oBrw:aCols[nCol]:bEditValid    :={|uValue,nLastKey,o,lDo | oGet :=o:oEditGet,;
                                                                oBody:Set(cField ,uValue ),;
                                                                lDo  :=IIF(lDo=nil .AND. ValType(oGet)="O",(oGet:nLastkey=13.OR.oGet:nLastKey==9),lDo),;
                                                                lResp:=oBody:RunScrCol(NIL , nCol , o , cField ,uValue , lDo,o:cValid),;
                                                                lResp:=IIF( lDo ,lResp ,o:lValid ),; // IIF( ValType(oGet)="O",oGet:nLastKey:=0,NIL),; oBody:CheckCol(o,nCol),;
                                                                oBrw:aCols[nCol]:lValid:=lResp   ,;
                                                                oGet:=nil,lResp}

     oBrw:aCols[nCol]:bEditBlock    := {|nRow,nColx,oCol|VP("lRunBtn",.T.),oBody:RunScrCol("BTN", nCol , oCol , cField , NIL , .T.,oCol:cAction ),VP("lRunBtn",.F.)}


/*
     oBrw:aCols[nCol]:bOnPostEdit:={|o,x|oBody:aData[oBrw:nArrayAt, nCol ] := x,;
                                         oBody:Set(cField ,x ),;
                                         IIF( o:nEditType>0 .AND. 1=0 ,;
                                         eval(oBrw:aCols[nCol]:bEditValid,x,0,o) ,;
                                         .f. ),;
                                         oBody:CheckCol(o,nCol)}

     oBrw:aCols[nCol]:bEditValid :={|uValue,nLastKey,o| oBody:Set(cField ,uValue ),;
                                                        lResp:=oBody:RunScrCol("VAL" , nCol , o , cField ,uValue),;
                                                        oBrw:aCols[nCol]:lValid:=lResp   ,;
                                                        lResp}
*/
RETURN .T.


/*
// Ajusta los Botones del cuerpo del Documento
*/

STATIC FUNCTION AjustButtons(oEdit)
   LOCAL oBody,oBrw,I,nStep:=0,oDlg
   oBody:=oEdit:oBody
   oEdit:=NIL

   IF ValType(oBody)!="O" .OR. ValType(oBody:oBrw)!="O"
      RETURN NIL
   ENDIF

   IF EMPTY(oBody:aEditType)
      oBody:Create()
   ENDIF

   // Toma la Posicion del Browse
   oBrw:=oBody:oBrw

   FOR I := 1 TO LEN(oBody:aBtns)
      IF !EVAL(oBody:aBtnsWhen[I])
         oBody:aBtns[I]:Hide()
      ELSE
         oBody:aBtns[I]:Move(oBrw:nTop+oBrw:nHeight+5,oBrw:nLeft+nStep,35,35,.T.)
         oBody:aBtns[i]:SetSize(35,35)
         nStep:=nStep+(35)+1
         oBody:aBtns[I]:Show()
//         oBody:aBtns[I]:Refresh(.T.) // ForWhen()
      ENDIF
   NEXT

   DpFocus(oBrw)

RETURN NIL

/*
// Asigna los Botones
*/
FUNCTION PutBar(oEdit) // CLASS DPEDIT
   LOCAL oBar,I,oBtn,nLen,cFileG
   LOCAL aLista:={},nAt,oFont,oMenu
   LOCAL nInsert:=7 // Posicion donde sera insertada el boton

   IF oEdit:nMode=0
      RETURN NIL
   ENDIF

   IF Empty(oEdit:aButtons)
      oEdit:DEFBTN()
   ENDIF

   nInsert:=ASCAN(oEdit:aButtons,{|a,n| a[7]=="TOP"})
 //? nInsert,"nInsert"

   nInsert:=IF(nInsert=0,7,nInsert-1)

   PUBLICO("oDpEdit",oEdit)

  //  DEFINE BUTTONBAR oBar OF oEdit:oDlg 3D

   DEFINE BUTTONBAR oBar 3D SIZE 40+3,oEdit:nBarHeight OF oEdit:oDlg // GetWndDefault()

   oEdit:oBar:=oBar

   oBar:bRClicked :={||.T.}

   //oEdit:aButtons:=ARRAY(oEdit:nMaxButtons)    // Cantidad de Botones (Maximo 20)
   // Botones de Opciones de Navegación
//   ViewArray(oEdit:aOpcButtons)

    FOR I := 1 TO LEN(oEdit:aOpcButtons) // Hay que Insertar los Nuevos Botones


       IF Empty(oEdit:aOpcButtons[I,4])
          oEdit:aOpcButtons[I,4]:="oDpEdit:nOption=0"
       ENDIF

//      ? oEdit:aOpcButtons[I,4],"AQUI ES" , VALTYPE(oEdit:aOpcButtons[I,4])

//       IF !Empty(oEdit:aOpcButtons[I,4]) .AND. !MacroEje(oEdit:aOpcButtons[I,4])
// 03/12/2015          LOOP
//     ENDIF

       AADD(oEdit:aButtons,NIL)
       AINS(oEdit:aButtons,nInsert+I)


// 03/12/2015 es necesario que los botones pueda ser utilizados en diversas opciones
/*
       oEdit:aButtons[7+I]:={NIL,;
                            oEdit:aOpcButtons[I,1],;
                            oEdit:aOpcButtons[I,2],;
                            oEdit:aOpcButtons[I,3],;
                            "oDpEdit:nOption =0"  ,;
                            NIL                   ,;
                            oEdit:aOpcButtons[I,1],;
                            oEdit:aOpcButtons[I,5];
                           }
*/

       aLista:=ACLONE(oEdit:aOpcButtons[I])

       oEdit:aButtons[nInsert+I]:={NIL,;                     //1
                            aLista[1],;   //2
                            aLista[2],;   //3
                            aLista[3],;   //4
                            aLista[4],;   //5
                            NIL                   ,;
                            aLista[1],;
                            aLista[5];
                            }

/*

       oEdit:aButtons[7+I]:={NIL,;                     //1
                            oEdit:aOpcButtons[I,1],;   //2
                            oEdit:aOpcButtons[I,2],;   //3
                            oEdit:aOpcButtons[I,3],;   //4
                            oEdit:aOpcButtons[I,4],;   //5
                            NIL                   ,;
                            oEdit:aOpcButtons[I,1],;
                            oEdit:aOpcButtons[I,5];
                            }
*/

   NEXT

   // Botones de Opciones de Edicion
   nLen:=LEN(oEdit:aButtons)-1

   FOR I := 1 TO LEN(oEdit:aEditButtons) // Hay que Insertar los Nuevos Botones

       AADD(oEdit:aButtons,NIL)
       AINS(oEdit:aButtons,nLen+I)

       IF Empty(oEdit:aEditButtons[I,4])
          oEdit:aEditButtons[I,4]:="oDpEdit:nOption !=0 .AND. oDpEdit:nOption!=4"
       ENDIF

       // Estos Botones deben estar condicionados por el Usuario JN 03/12/2015

       oEdit:aButtons[nLen+I]:={NIL,;                     //1
                                oEdit:aEditButtons[I,1],; //2
                                oEdit:aEditButtons[I,2],; //3
                                oEdit:aEditButtons[I,3],; //4 Accion
                                oEdit:aEditButtons[I,4],; //5 Condicion //  03/12/2015 "oDpEdit:nOption !=0 .AND. oDpEdit:nOption!=4",;
                                NIL ,;
                                oEdit:aEditButtons[I,1],;
                                oEdit:aEditButtons[I,5]}

   NEXT I

//  ViewArray(oEdit:aButtons)

   nAt:=ASCAN(oEdit:aButtons,{|a,n|a[7]=="XBROWSE"})

   IF !Empty(oEdit:cBtnList) .AND. nAt>0
      oEdit:aButtons[I,2]:=oEdit:cBtnList
   ENDIF


   For I=1 TO LEN(oEdit:aButtons) // nMaxButtons

//      DEFINE BUTTON FILE "BITMAPS\GO.BMP" oEdit:aButtons[I,1] OF oBar ACTION MSGALERT("OK")
     // ? i,oDp:cPathBitMaps+oEdit:aButtons[I,2]
      oMenu:=NIL

      nAt:=ASCAN(oEdit:aBtnMnu,{|a,n| oEdit:aButtons[I,7]=a[1]})

      IF nAt>0
          // ASCAN(oEdit:aBtnMnu,{|a,n| oEdit:aButtons[I,7]=a[1]})>0

         oMenu:=EJECUTAR("TDOCENCBTNMNU",oEdit,oEdit:aButtons[I,7])

//? oMenu:ClassName(),"oMenu"

         IF !Empty(oEdit:aBtnMnu[nAt,4])
            oEdit:aButtons[I,2]:=oEdit:aBtnMnu[nAt,4]
         ENDIF

      ENDIF

      IF oEdit:aButtons[I,7]="BROWSE" .AND. !Empty(oEdit:cBtnList)
         oEdit:aButtons[I,2]:=oEdit:cBtnList
      ENDIF

      cFileG:=DPBMP(oEdit:aButtons[I,2])
      cFileG:=STRTRAN(cFileG,".","G.")

      // cFileG:=STRTRAN(oDp:cPathBitMaps+oEdit:aButtons[I,2],".","G.")

      IF oMenu=NIL

        DEFINE BUTTON oBtn OF oEdit:oBar;
               ACTION 1=1;
               FILE DPBMP(oEdit:aButtons[I,2]),NIL,cFileG;
               NOBORDER;
               TOOLTIP oEdit:aButtons[I,3]

        oBtn:SetSize(oEdit:nBtnWidth,oEdit:nBtnHeight,.T.)

      ELSE

        DEFINE BUTTON oBtn OF oEdit:oBar;
               ACTION 1=1;
               MENU oMenu;
               NOBORDER;
               FILE DPBMP(oEdit:aButtons[I,2]),NIL,cFileG;
               TOOLTIP oEdit:aButtons[I,3]

        oBtn:SetSize(oEdit:nBtnWidth+30,oEdit:nBtnHeight+45,.T.)

      ENDIF

      oBtn:lCancel:=.T.  // 23/06/2020

      oEdit:aButtons[I,1]:=oBtn


/*
      IF oMenu=NIL

        DEFINE XBUTTON oEdit:aButtons[I,1] OF oBar;
               ACTION 1=1;
               FILE oDp:cPathBitMaps+oEdit:aButtons[I,2],NIL,cFileG; //          SHAPE; // CAPTION cMsg;
               MESSAGE oEdit:aButtons[I,3];
               TOOLTIP oEdit:aButtons[I,3]; //            NOBOX; //            BITMAP OPAQUE; //            ADJUST; //          BRUSH oBrush;
               SIZE 40,40

      ELSE

        DEFINE XBUTTON oEdit:aButtons[I,1] OF oBar;
               ACTION 1=1;
               MENU oMenu;
               FILE oDp:cPathBitMaps+oEdit:aButtons[I,2],NIL,cFileG; //          SHAPE; // CAPTION cMsg;
               MESSAGE oEdit:aButtons[I,3];
               TOOLTIP oEdit:aButtons[I,3]; //            NOBOX; //            BITMAP OPAQUE; //            ADJUST; //          BRUSH oBrush;
               SIZE 40,40

       ENDIF
*/


/*;
             COLORS CLR_WHITE, { nRGB(243,250,200), CLR_BLACK, 5 }   // degraded background color            ROUNDRECT   // elliptic button
*/
//    ? oEdit:aButtons[I,5]

      oEdit:aButtons[I,1]:bAction:=BuildRun(oEdit,I,.T.)
      oEdit:aButtons[I,1]:bWhen  :=BuildRun(oEdit,I,.F.)

      IF oEdit:aButtons[I,7]="CLOSE" //,i,"xbutton"
         oEdit:aButtons[I,1]:lCancel:=.T.
         // ? i,"xCancel"
      ENDIF

      // oEdit:aButtons[I,5]:=BuildRun(oEdit,I,.F.)
      // oEdit:aButtons[I,1]:ForWhen()

   NEXT I

   // AEVAL(oEdit:aButtons,{|a,i|a[I,1]:ForWhen()})

   oEdit:RECCOUNT(.T.)  // Obtiene la Cantidad de Registros
   oEdit:RECCOUNT(.F.)  // Obtiene la Posición del Registro Actual

   DEFINE FONT oFont NAME "Tahoma" SIZE 0, -10 BOLD

   oBar:SetColor(NIL,oDp:nGris)
   Aeval(oBar:aControls,{|o|o:SetColor(NIL,oDp:nGris)})

   nAt:=60+(LEN(LSTR(oEdit:nRecCount))*10)

   @ .8,0 SAY oEdit:oSayRecord PROMPT MI(" Registro ",613,"DPEDIT")+CRLF+" R:"+LSTR(oEdit:nRecNo)+"/"+LSTR(oEdit:nRecCount) PIXEL SIZE nAt,18*2 BORDER FONT oFont OF oBar;
              COLOR oDp:nClrYellowText,oDp:nClrYellow

   IF ValType(oEdit:oSayRecord)="O"
      oEdit:oSayRecord:Hide()
   ENDIF

   oEdit:Checkbtn()

RETURN NIL

FUNCTION BuildWhere(aValues)
   LOCAL cWhere:=""

   AEVAL(aValues,{ |a,i| cWhere+=IIF( EMPTY(cWhere), "" , " AND ") + a[1] + GetWhere("=",a[2]) })

RETURN cWhere

STATIC FUNCTION RestoreEdit(nNumEdit,oEdit)
   LOCAL nAt
   // AADD(aDpEdit,{nNumEdit,cVarPublic,Self})
   //LOCAL nAt
   //nAt:=aScan( aDpEdit, { |e,n| e[1] == nNumEdit } )

   IF !Empty(oEdit:cVarPublic)
//      nAt:=AT(" ",oEdit:cVarPublic)
//      IF nAt>0
//        oEdit:cVarPublic:=LEFT(oEdit:cVarPublic,nAt)
//      ENDIF
      // ? oEdit:cVarPublic,oEdit:ClassName()
         __QQPUB(ALLTRIM(oEdit:cVarPublic),oEdit)
      PUBLICO(ALLTRIM(oEdit:cVarPublic),oEdit)
      IF ValType(oEdit:oBody)="O"
         PUBLICO("oBody",oEdit:oBody)
      ENDIF
   Endif

   SetScript(oEdit:oScript) // Asigna Script por Defecto

   oDp:cHelpTopic:=oEdit:cTopic
   oDp:cFileChm  :=oEdit:cFileChm

   //oEdit:oWnd:cTitle:=str(nNumEdit)
   //IF nAt>0 // Rescata el Valor
   //   PUBLICO(aDpEdit[nAt,2],aDpEdit[nAt,3])
      // STORE aDpEdit[nAt,2] TO aDpEdit[nAt,3]
   //ENDIF

   SetKey(VK_F10,{||oEdit:RunKey(VK_F10)})

RETURN .T.

/*
// Crea una Caja de Dialogo con la clase DpEdit
*/
FUNCTION Dialog(cTitle,cFileEdt,cVarName,oWnd,lModal,nTop,nLeft,nWidth,nHeight)
   Local oForm

   oForm:=DpEdit():New(cTitle,cFileEdt,cVarName,.T.,.T.,oWnd,lModal,nTop,nLeft,nWidth,nHeight)

   oForm:SetScript()

   SetWndDefault( oForm:oDlg )

// ? "AQUI ES DIALOG 2",ErrorSys(.T.),oForm:ClassName()

RETURN oForm

/*
// Antigua Función DP19, Hace caja de Dialogo
*/
/*
FUNCTION caj_wnd(nTop,nLeft,nWidth,nHeight,cTitle,lModal,cFileEdit,cVarName,oWnd)
   LOCAL oForm

   oForm:=DpEdit():New(cTitle,cFileEdit,cVarName    ,.T.       ,.T.      ,oWnd,lModal,nTop,nLeft,nWidth,nHeight)
   SetWndDefault( oForm:oDlg )

RETURN .T.
*/

/*
// Define el Bloque de Código para Validar y Ejecutar los Botone del Formulario
*/
STATIC FUNCTION BuildRun(oEdit,nPos,lValid)
    PUBLICO("oDpEdit",oEdit)
RETURN {||oEdit:RunBtn(nPos,lValid)}

/*
// Ubica el Primer Get y Apunto el Foco
*/
FUNCTION SetFocusGet(oDlg,cList,lRefresh,oFocus)

   LOCAL I,oGet,bWhen,oControl,nFolder,cPicture,uData,cCaption
   LOCAL cGets:="TGET,TMULTIGET,TSAY,TBMPGET" // ,cCaption,lValue
   LOCAL oBrush

   // DEFINE BRUSH oBrush COLOR nRGB( 0, 0, 0) // 245, 245 )

   DEFAULT cList:="TGET,TMULTIGET,TCOMBOBOX,TBMPGET,TCHECKBOX" //,TBMPGET"

   DEFAULT lRefresh:=.T.

   IF ValType(oDlg)!="O" .OR. ValType(oDlg:aControls)!="A"
      RETURN .F.
   ENDIF

   FOR I := 1 TO LEN(oDlg:aControls)

      oControl:=oDlg:aControls[I]
      oControl:nLastKey:=0

      // IF "TCHE"$oControl:ClassName() // $"TCHECK"
         // oControl:SetText(oControl:cForm)
         //    oControl:cCaption:=oControl:cForm
      // ENDIF

      IF oControl:ClassName()$cList // oDlg:aControls[I]:ClassName()$cList
         // ? oDlg:aControls[I]:ClassName(),oDlg:aControls[I]:GetText()
         IIF( "GET"$oControl:ClassName() , oControl:SetPos(1) , NIL)

         IF oGet=NIL
            IF oFocus=NIL
              oGet:=oControl
            ELSE
              oGet:=oFocus // Get del Focus
            ENDIF
         ENDIF

//         ? oGet:lActive,oGet:lFocused,"oGet:lActive,lFocused"
//         IF !oGet:lActive
//          oGet:=NIL
//         ENDIF

      ENDIF

      IF lRefresh  //.AND. .F.

         bWhen:=oDlg:aControls[i]:bWhen
         // oDlg:aControls[i]:bWhen:={||.T.} // Refresh(.t.)
         // oDlg:aControls[I]:Enable()

         IF oDlg:aControls[i]:ClassName()$"TCOMBOBOX"
            oDlg:aControls[I]:SetText(Eval(oDlg:aControls[I]:bSetGet))
            oDlg:aControls[I]:nAt:=0
            ComboIni(oDlg:aControls[i],.F.)
            oDlg:aControls[i]:Refresh()
         ENDIF

         IF oDlg:aControls[i]:ClassName()$cGets

           // ? oDlg:aControls[i]:ClassName(),oDlg:aControls[i]:VARGET()

            cPicture:=oDlg:aControls[I]:cPicture

            IF cPicture!=nil
               uData:=oDlg:aControls[i]:VarGet()
               oDlg:aControls[I]:SetText(tran(oDlg:aControls[I]:VarGet(),cPicture),.T.) // SetText(oDlg:aControls[I]:GetText(),.T.)
            ELSE
               oDlg:aControls[I]:SetText(oDlg:aControls[I]:VarGet(),.T.) // SetText(oDlg:aControls[I]:GetText(),.T.)
           ENDIF

         ENDIF

         IF ValType(oControl:bWhen)="B" .AND. !EVAL(oControl:bWhen)
             oControl:Disable()
         ENDIF

         oDlg:aControls[I]:bWhen:=bWhen

         IF oDlg:aControls[I]:ClassName()="TFOLDER"
            AEVAL(oDlg:aControls[I]:aDialogs,{|o,n|o:SetFocus()})
         ENDIF

   /*METHOD SetText( cText ) INLINE ;
                           ::cCaption := cText,;
                           SetWindowText( ::hWnd, cText ) */

//         oDlg:aControls[I]:ForWhen()

      ENDIF
   NEXT

   // oDlg:ForWhen()

//   IF ::nOption=0
//      ::SetColor(GetSysColor(COLOR_GRAYTEXT) , GetSysColor( COLOR_BTNFACE ))
//   ENDIF

//  ? oGet:Classname()

   IF !EMPTY(oGet)
      //? oGet:ClassName()
      oGet:ForWhen()
      DpFocus(oGet)
      oGet:SetFocus(oGet)
      oGet:nLastKey:=0
      oGet:oWnd:nLastKey:=0

   ENDIF

   IIF( ValType(oBrush)="O",oBrush:End(),NIL)

   oDlg:nLastKey:=0

RETURN oGet

/*
// Devuelve Lista de Formularios
*/
FUNCTION GetaDpEdit()
RETURN aDpEdit

/*
FUNCTION Incrusta( oWnd, oDlg, lCentra )

  static lActivo := .F.
  local  aRect, nPosY, nPosX

  DEFAULT lCentra := .F.

  if !(lActivo)
     lActivo = .T.

     // Muevo dialogo a 0,0 para que quede bien incrustado...
     oDlg:Move(0,0)

     // Primero Seteo Como si Windows Fuera Estandar...
     oWnd:SetSize(oDlg:nWidth+8, oDlg:nHeight+27)

     // Calculo diferencia !
     aRect  := GetClientRect(oWnd:hWnd)
     nPosY := ABS(ABS(aRect[3]-aRect[1])-ABS(oWnd:nTop-oWnd:nBottom))
     nPosX := ABS(ABS(aRect[4]-aRect[2])-ABS(oWnd:nRight-oWnd:nLeft))

     // Ahora la pongo JUSTA, JUSTA...
     oWnd:SetSize(oDlg:nWidth+nPosX,oDlg:nHeight+nPosY)
     iif( lCentra, oWnd:Center(), oWnd:Move(0,0) )

     lActivo := .F.
  endif

RETURN .T.

NOTA: Modifique el tercer parámetro a lógico y cambie el nombre de algunas variables para que sea más entendible. (Daniel Andrade)
*/

//----------------------------------------------------------------------------//

FUNCTION AjustaDlg(oWnd,oDlg,nTop,nLeft,nBottom,nRight,oEdit)
  LOCAL nWidth:=100,nHeight:=100,aPoint  // JN 30/12/2018
  LOCAL aRect, nPosY, nPosX

// Rescata el Area Ocupada por la Barra de Mensaje
   oDlg:SetSize(oDlg:nWidth(),oDlg:nHeight(),.T.)

///? oEdit:oAdjustWnd:ClassName()

   IF ValType(oEdit:oAdjustWnd)="O"

      // Debe Ajustarse
      aPoint  := AdjustWnd( oEdit:oAdjustWnd, nWidth, nHeight )
      oDlg:Move(aPoint[1],aPoint[2],NIL,NIL,.T.)

      IF "XBROWSE"$oEdit:oAdjustWnd:ClassName()
         EJECUTAR("FRMMOVE",oEdit,oEdit:oAdjustWnd)
      ENDIF

   ELSE

      oDlg:Move(0,0)

   ENDIF

// SetColor(oDlg:nClrText,oDlg:nClrPane),;
// oWnd:Move( max(oWnd:nTop,1) , max(oWnd:nLeft,1),nBottom,nRight,.T.)
// oDlg:SetSize(oWnd:nWidth-7,oWnd:nHeight-(oDlg:oMsgBar:nHeight*1.5)+2)
// oDlg:oMsgBar := TMsgBar():New( oDlg, "", .F., .F., .F., .F.,,,, )
//  oDlg:CoorsUpdate()

  // Primero Seteo Como si Windows Fuera Estandar...
  // oWnd:SetSize(oDlg:nWidth+8, oDlg:nHeight+27)
  // Calculo diferencia !

  aRect  := GetClientRect(oWnd:hWnd)
  nPosY := ABS(ABS(aRect[3]-aRect[1])-ABS(oWnd:nTop-oWnd:nBottom))
  nPosX := ABS(ABS(aRect[4]-aRect[2])-ABS(oWnd:nRight-oWnd:nLeft))

  // Ahora la pongo JUSTA, JUSTA...
  //

  // nPosY:=nPosY+oDlg:oMsgBar:nHeight()
  nPosX:=0
  nPosY:=-1

  oWnd:Move(oWnd:nTop,oWnd:nLeft,oDlg:nWidth(),oDlg:nHeight(),.T.)
  //oWnd:Restore()
  //oWnd:SetText("ASI ES")
  //oWnd:Moved(0,0)

RETURN .F.


/*
// Ejecuta DPMEMOEDIT de DPWIN32.HRB
*/
FUNCTION _DPMEMOEDIT()
   PARAM uPar1,uPar2
RETURN MACROEJE("DPMEMOEDIT(uPar1,uPar2)")

/*
// Grabar Campo Memo
*/
FUNCTION DPMEMOSAVE(nNumMemo,cMemo,cDescri,oDb )
   LOCAL oTable,cSql,lAppend:=.T. ,cWhere:=""
   LOCAL aChar:=ARRAY(32)

   DEFAULT cDescri:=""

   IF EMPTY(nNumMemo) .AND. EMPTY(cMemo+cDescri)
      RETURN nNumMemo
   ENDIF

   IF EMPTY(nNumMemo)

      // Agregar
      /*
      cSql:="SELECT MAX(MEM_NUMERO)  FROM "+oDp:cDpMemo
      oTable:=OpenTable(cSql,.T.)
      nNumMemo:=oTable:FieldGet(1)+1
      oTable:End()
      */

      cSql:="SELECT * FROM "+oDp:cDpMemo+" WHERE 1=0"  // DPMEMO"
      nNumMemo:=SQLINCREMENTAL(oDp:cDpMemo,"MEM_NUMERO",NIL,oDb)

      // lAppend:=.T.

   ELSE

      // Modificar
      cSql:=" SELECT * FROM "+oDp:cDpMemo+;
            " WHERE MEM_NUMERO"+GetWhere("=",nNumMemo)+;
            "  AND  MEM_ID"    +GetWhere("=", oDp:cIdMemo)

      //lAppend:=.F.

   ENDIF

   oTable:=OpenTable(cSql,.T.,oDb)

  // ? oTable:RecCount(),"RecCount()" ,oTable:cWhere
   oTable:lAppend:=.F.

   IF oTable:RecCount()=0
      //lAppend
      oTable:AppendBlank()
      oTable:lAppend:=.T.
      oTable:cWhere:=NIL
   ENDIF

   oTable:Replace("MEM_NUMERO",nNumMemo   )
   oTable:Replace("MEM_DESCRI",cDescri    )
   oTable:Replace("MEM_MEMO"  ,cMemo      )
   oTable:Replace("MEM_ID"    ,oDp:cIdMemo)

//? oTable:cWhere, " Este es el cWhere", oTable:lAppend

   oTable:Commit(oTable:cWhere)

// ? oTable:cSql,nNumMemo,oTable:cWhere

   oTable:End()

RETURN nNumMemo

/*
// Grabar Campo Memo
// cMemo,cDescri Vienen Como Referencia
*/
FUNCTION DPMEMOREAD(nNumMemo,cMemo,cDescri,lMemo,oDb)
   LOCAL oTable,cSql

   DEFAULT cMemo  :=""
   DEFAULT cDescri:=""
   DEFAULT lMemo  :=.F. // Leer a Memo

   // ? "DPMEMOREAD",nNumMemo

   IF EMPTY(nNumMemo)
      RETURN ""
   ENDIF

   IF lMemo

     cSql  :="SELECT MEM_DESCRI,MEM_MEMO FROM "+oDp:cDpMemo+" WHERE MEM_NUMERO"+GetWhere("=",nNumMemo)
     oTable:=OpenTable(cSql,.t.,oDb)
     cMemo := oTable:MEM_MEMO

   ELSE

     cSql:="SELECT MEM_DESCRI FROM "+oDp:cDpMemo+" WHERE MEM_NUMERO"+GetWhere("=",nNumMemo)
     oTable:=OpenTable(cSql,.t.,oDb)
     cMemo :=""

   ENDIF

   cDescri:=oTable:MEM_DESCRI // Regresa como Referencia

   oTable:End()

RETURN cMemo

/*
// Mata el Get que esta en la casilla, genera error con DPEDIT
*/
FUNCTION KillGetCol(oBrw)
   LOCAL I

   IF ValType(oBrw)!="O"
      RETURN .F.
   ENDIF

   IF oBrw:ClassName()="DPBODY"
      oBrw:=oBrw:oBrw
   ENDIF

   FOR I:= 1 TO LEN(oBrw:aCols)
      IF ValType(oBrw:aCols[I]:oEditGet)="O"
        oBrw:aCols[I]:oEditGet:bLostFocus:=NIL
        oBrw:aCols[I]:oEditGet:End()
        oBrw:aCols[I]:oEditGet:=NIL
      ENDIF
   NEXT

RETURN .T.

/*
// Ejecuta ScrollGet Asociado al Formulario
*/
STATIC FUNCTION ScrollField(oEdit,cTitle)
   LOCAL oDpEdit,cExcluye:="",I,cField,uValue,oTable,oBar

   IF ValType(oEdit:oEditScr)="O" .AND. oEdit:oEditScr:oWnd:hWnd!=0 // >0
      oEdit:oEditScr:oWnd:Restore()
      RETURN oEdit:oEditScr
   ENDIF

   oTable :=OpenTable(oEdit:oTable:cSql,.T.)
   oDpEdit:=DPEDIT():New(cTitle,"TEST.edt", , .F. )

   oDpEdit:nOption:=oEdit:nOption
   oTable:cPrimary:=oEdit:oTable:cPrimary

   oDpEdit:SetTable( oTable , .T. )

   //DEFINE BUTTONBAR oBar _3D OF oDpEdit:oWnd
/*
   DEFINE BUTTONBAR oBar 3D SIZE 40,40 OF oEdit:oDlg

   DEFINE BUTTON FILE oDp:cPathBitMaps+"SAVEPRJ.BMP"   OF oBar ACTION MSGALERT("SAVE")
   DEFINE BUTTON FILE oDp:cPathBitMaps+"CANCEL2.BMP"   OF oBar ACTION MSGALERT("CANCEL")
   DEFINE BUTTON FILE oDp:cPathBitMaps+"EXIT.BMP"      OF oBar ACTION oDpEdit:Close()
*/
   oDpEdit:oScroll:=oDpEdit:SCROLLGET("DPCOTIZA","XXX.SCG",cExcluye,oDpEdit:oDlg)
//   oDpEdit:oScroll:oBrw:SetSize(600,200,.T.)

   // Trapaso de Variables
   FOR I := 1 TO LEN(oEdit:oTable:aFields)
      cField :=oEdit:oTable:aFields[i,1]
      oDpEdit:Set(cField,oEdit:Get(cField))
      uValue :=oEdit:Get(cField)
      oDpEdit:oScroll:Put(cField,uValue,2)
   NEXT

   oDpEdit:bSet:={|cName,uValue|oEdit:Set(cName,uValue)} // (METHOD Set( cName, xValue ) CLASS DPEDIT

  // oDpEdit:oScroll:=oDpEdit:SCROLLGET("DPCOTIZA","XXX.SCG",cExcluye,oDpEdit:oWnd)

   oDpEdit:bInit:={||BuildBar(oDpEdit)}
   oDpEdit:Activate()

   oDpEdit:bEnd:={||oTable:=NIL,;
                    oEdit:KillEdit(oDpEdit),;
                    oEdit:oEditScr:=NIL    ,;
                    oDpEdit:=NIL}

   // oDpEdit:oScroll:oBrw:nColSel:=2
   // oDpEdit:oScroll:oBrw:Refresh()
   // oDpEdit:oScroll:oBrw:aCols[2]:Edit()
   // oDpEdit:oScroll:oBrw:KeyBoard(13)

   // Al Finalizar

RETURN oDpEdit

/*
// Crea la Barra de Actividades
*/
STATIC FUNCTION BuildBar(oEdit)
   LOCAL oBar,oBtn,oBtn1

   DEFINE BUTTONBAR oBar 3D SIZE 40,40 OF oEdit:oDlg

   DEFINE XBUTTON oBtn1 OF oBar;
          ACTION (oEdit:lCancel:=.t.,oEdit:nOption:=0,oEdit:Close());
          FILE DPBMP("SAVEPRJ.BMP");
          MESSAGE "Aceptar y Regresar";
          TOOLTIP "Aceptar y Regreasar";
          SIZE 40,40;
          COLORS CLR_WHITE, { nRGB(243,250,200), CLR_BLACK, 5 }

   DEFINE XBUTTON oBtn OF oBar;
          ACTION oEdit:Close();
          FILE DPBMP("EXIT.BMP");
          MESSAGE "Cancelar y Cerrar";
          TOOLTIP "Cancelar y Cerrar";
          SIZE 40,40;
          COLORS CLR_WHITE, { nRGB(243,250,200), CLR_BLACK, 5 }

   oEdit:oScroll:oBrw:bKeyDown := {| nKey | if( nKey == VK_F2, EVAL(oBtn1:bAction) , NIL )}

   DpFocus(oEdit:oScroll:oBrw)
   // oEdit:oScroll:oBrw:nColSel:=2
   //oEdit:oScroll:oBrw:KeyBoard(13)

RETURN .F.
/*
// Destruye los Controles
*/
FUNCTION KillControl(oDlg,lDesign)
   LOCAL I

   DEFAULT lDesign:=.F.

   IF ValType(oDlg)!="O" .OR. ValType(oDlg:aControls)!="A" .OR. lDesign
      RETURN .F.
   ENDIF

   FOR I := 1 TO LEN(oDlg:aControls)
      IF "XBRO"$oDlg:aControls[I]:ClassName()
         //AEVAL(oDlg:aControls[I]:aCols,{|oCol|cCol:})
         oDlg:aControls[I]:CancelEdit()
      ENDIF
      // oDlg:aControls[I]:End()
   NEXT

   // oDlg:aControls:={}

RETURN .T.

/*
// Destruye la Edición del xBrowse
*/
STATIC FUNCTION KillxBrw(oDlg)
   LOCAL I

   IF ValType(oDlg)!="O" .OR. ValType(oDlg:aControls)!="A"
      RETURN .F.
   ENDIF

   FOR I := 1 TO LEN(oDlg:aControls)
      IF "XBRO"$oDlg:aControls[I]:ClassName()
         oDlg:aControls[I]:CancelEdit()
       ENDIF
   NEXT

RETURN .T.

/*
STATIC FUNCTION CheckxBrw(oDlg)
   LOCAL I
   IF ValType(oDlg)!="O" .OR. ValType(oDlg:aControls)!="A"
      RETURN .F.
   ENDIF
//   ? oDlg:ClassName(),oDlg,oDlg:aControls
   FOR I := 1 TO LEN(oDlg:aControls)
      IF "XBRO"$oDlg:aControls[I]:ClassName()
         //AEVAL(oDlg:aControls[I]:aCols,{|oCol|cCol:})
         oDlg:aControls[I]:CancelEdit()
       ENDIF
      //oDlg:aControls[I]:End()
   NEXT
RETURN .T.
*/

/*
// Revisa los Datos del ComboBox

STATIC FUNCTION CheckCombo(oDlg) // Revisa los ComboBox
   LOCAL I,U

   FOR I := 1 TO LEN(oDlg:aControls)
//      ? I,oDlg:aControls[I]:ClassName()
      IF "TCOMBO"$oDlg:aControls[I]:ClassName()
//          ? I,"COMBOBOX",oDlg:aControls[I]:VarGet()
          ComboIni(oDlg:aControls[I])
      ENDIF
      IF "TFOLD"$oDlg:aControls[I]:ClassName()
          FOR U := 1 TO LEN(oDlg:aControls[I]:aDialogs)
             CheckCombo(oDlg:aControls[I]:aDialogs[U]) // Revisa los ComboBox
          NEXT
      ENDIF
   NEXT

RETURN .T.
*/


/*
// Revisa las Variables Públicas
*/
FUNCTION ClearDpEdit()
   LOCAL I,oEdit,nAt

   FOR I=1 TO LEN(aEdit)
      oEdit:=aEdit[I]
      IF ValType(oEdit)="O" .AND. ValType(oEdit:oWnd)="O"
          IF oEdit:oWnd:hWnd=0
             KillControl(oEdit:oDlg)
             oEdit:Release()
             oEdit:aVars:={}
             ADEL(aEdit,I)
             ASIZE(aEdit,LEN(aEdit)-1)


             nAt:=aScan( aEdit, { |e,n| e:cVarPublic==oEdit:cVarPublic } )

             IF nAt=0 // Borra Variable No necesaria
                oEdit:KillEdit(oEdit)
      //        oDp:oFrameDp:SetText("borrado "+oEdit:cVarPublic)
                __MXRELEASE(oEdit:cVarPublic)
             ENDIF

             // Busca que Exitan Otros Formularios

          ENDIF
      ELSE
          // Reafirma
         PUBLICO(oEdit:cVarPublic,oEdit)
      ENDIF
   NEXT I

RETURN NIL

/*
// Bloque de Código para Buscar
*/
STATIC FUNCTION FrmFindBlq(oFrm,oGet,cField)
RETURN {|lResp|lResp:=EJECUTAR(oFrm:cFind,oFrm,oGet,cField),lResp:=IIF( ValType(lResp)!="L" , .F. , lResp )}

FUNCTION ActivateKey(aKey,oDlg)
  LOCAL I

  IF Empty(aKey)
     RETURN NIL
  ENDIF

  FOR I=1 TO LEN(oDlg:aControls)

    IF oDlg:aControls[I]:bKeyDown=NIL
      oDlg:aControls[I]:bKeyDown:={|nKey| RUN_KEY(nKey,aKey)}
    ENDIF

  NEXT I

  //  ActivateKey(::aSetKey,::oDlg)

RETURN .T.

STATIC FUNCTION RUN_KEY(nKey,aKeys)
  LOCAL nAt:=ASCAN(aKeys,{|a,n|a[1]=nKey})

RETURN IIF( nAt>0 , EVAL(aKeys[nAt,2],nKey) , NIL )
// EOF

FUNCTION ISEDTIDRUN(cId,cVarName,aData)
  LOCAL nAt:=0

  nAt:=ASCAN(aDpEdit,{|a,n| a[2]=cVarName .AND. a[3]:cIdFrm=cId})

  // Si para como @referencia, se pueden leer sus datos

  IF ValType(aData)="A"
    aData:={}
    AEVAL(aDpEdit,{|a,n| AADD(aData,{a[2],a[3]:cIdFrm}) })
  ENDIF

//? nAt,CLPCOPY(cId),cVarName

  IF nAt>0
     aDpEdit[nAt,3]:oWnd:Restore()
  ENDIF

RETURN nAt>0

FUNCTION DPDIALOGO(cTitle,cFile,cVarName) // ,nLin,nCol,nWidth,nHeight)
   LOCAL lWindows:=.T.

   DEFAULT cTitle  :="Dialogo",;
           cVarName:="oFrm"

   oDp:oDlgRun:=DPEDIT():New(cTitle,cFile,cVarName , .T., .T. )

RETURN oDp:oDlgRun

FUNCTION DPDIALOGORUN(bInit)

  DEFAULT bInit:={||.T.}

  IF ValType(bInit)="C"

     IF !":"$bInit
        bInit:=oDp:oDlgRun:cVarPublic+":"+bInit
     ENDIF

     bInit:=BLOQUECOD(bInit)

  ENDIF

  oDp:oDlgRun:Activate(bInit)

//  oDp:oDlgRun:oDlg:Move(oDp:oDlgRun:nLin,oDp:oDlgRun:nCol,oDp:oDlgRun:nWidth,oDp:oDlgRun:nHeight,.T.)

RETURN NIL

FUNCTION CloseAllEdit()

   LOCAL lResp:=.T.,oWnd,I,oEdit

   FOR I=1 TO LEN(aDpEdit)

      oEdit:=aDpEdit[I]

      IF (ValType(aDpEdit[I])="O")
         oEdit:bValid:={||.T.}
         oWnd:=oEdit:oWnd
         IIF( ValType(oWnd)="O" , (oWnd:Restore(),oWnd:End()) , NIL)
      ENDIF

   NEXT I

   aDpEdit:={}

RETURN lResp

FUNCTION DpMsgRun(cText,cTitle,bBlq,nData,lStop,lReset)
  LOCAL cTitle_

  IF ValType(oDp:oMsgRun)="O"

    DEFAULT cTitle:=oDp:oMsgRun:cTitle,;
            cText :=oDp:oMsgRun:cText
  ENDIF

  DEFAULT lReset:=.T.

  IF lReset
     DpMsgClose()
  ENDIF

  cText:=CTOO(cText,"C")
  nData:=CTOO(nData,"N")

  IF !ValType(oDp:oMsgRun)="O"
    // EJECUTAR("MSGRUNVIEW",cTitle,cText,nData,lStop,lReset)
    MSGRUNVIEW(cTitle,cText,nData,lStop,lReset)
  ENDIF

  IF !cTitle=NIL
     cTitle_:=oDp:oMsgRun:oWnd:cTitle
     oDp:oMsgRun:oWnd:SetText(cTitle)
  ENDIF

  IF !ValType(oDp:oMsgRun)="O" .OR. oDp:oMsgRun=NIL
     // EJECUTAR("MSGRUNVIEW",cTitle,cText,nData,lStop,lReset)
     MSGRUNVIEW(cTitle,cText,nData,lStop,lReset)
  ENDIF

  oDp:oMsgRun:FRMTEXT(cText,bBlq)

  IF !cTitle_=NIL
     oDp:oMsgRun:oWnd:SetText(cTitle_)
  ENDIF

  SysRefresh(.T.)

RETURN oDp:oMsgRun

FUNCTION DpMsgClose()

  DEFAULT oDp:oMsgRun:=NIL

  IF oDp:oMsgRun=NIL
     RETURN NIL
  ENDIF

// RETURN oDp:oMsgRun:oWnd:Hide()
// RETURN

  oDp:oMsgRun:Close() // FRMCLOSE()
  oDp:oMsgRun:=NIL

RETURN NIL


FUNCTION DpMsgHide()

  IF oDp:oMsgRun=NIL
     RETURN NIL
  ENDIF

  IF oDp:oMsgRun:oWnd=NIL
     RETURN NIL
  ENDIF

RETURN oDp:oMsgRun:oWnd:Hide()
// oDp:oMsgRun:FRMHIDE()

FUNCTION DpMsgSetTotal(nCant,cTitle,cText,lStop,lReset)

 DEFAULT oDp:oMsgRun:oMeter:=NIL

 IF ValType(oDp:oMsgRun)="O"

    cTitle:=oDp:oMsgRun:cTitle
    cText :=oDp:oMsgRun:cText

 ELSE

    //? "no existe",oDp:oMsgRun:ClassName()
    // EJECUTAR("MSGRUNVIEW",cTitle,cText,nCant,lStop,lReset)

    MSGRUNVIEW(cTitle,cText,nCant,lStop,lReset)

    //? "regreso",oDp:oMsgRun:ClassName()

 ENDIF

//?  oDp:oMsgRun:ClassName()

 IF oDp:oMsgRun:oMeter=NIL
    oDp:oMsgRun:Close()
    oDp:oMsgRun:=NIL
    MSGRUNVIEW(cTitle,cText,nCant,lStop,lReset)
 ENDIF

 IF ValType(oDp:oMsgRun:oMeter)="O"
    oDp:oMsgRun:oMeter:Hide()
    RETURN oDp:oMsgRun:oMeter:SetTotal(nCant)
 ENDIF

RETURN NIL


FUNCTION DpMsgSet(n,lSay,bBlq,cSay)
  LOCAL cText

  DEFAULT lSay:=.F.,;
          cSay:=""

  IF oDp:oMsgRun=NIL
     RETURN NIL
  ENDIF

  DEFAULT oDp:oMsgRun:oMeter:=NIL

  IF oDp:oMsgRun:oMeter=NIL
     DpMsgSetTotal(1)
     // oDp:oMsgRun:FRMSETTOTAL(1)
  ENDIF

  IF oDp:oMsgRun:oMeter=NIL
     RETURN NIL
  ENDIF

  oDp:oMsgRun:oMeter:Show()

  IF n>=oDp:oMsgRun:nData
     oDp:oMsgRun:nData:=n
  ENDIF

  IF lSay

     // cText:=cSay+IF(Empty(cSay),""," ")+LSTR(n)+"/"+LSTR(oDp:oMsgRun:nData)

     cText:=LSTR(n)+"/"+LSTR(oDp:oMsgRun:nData)+IF(!Empty(cSay)," "+cSay,"")

//? oDp:oMsgRun:ClassName(),cText,LEN(cText)

     oDp:oMsgRun:FRMTEXT(cText,bBlq)

     // DpMsgSetText(cText,bBlq)

  ENDIF

RETURN oDp:oMsgRun:oMeter:Set(n)

FUNCTION DpMsgSetText(cSay,bBlq)
  LOCAL cText

  DEFAULT cSay:=""

  IF ValType(oDp:oMsgRun)<>"O"
    cSay:=ALLTRIM(cSay)
    cSay:=IIF(LEN(cSay)>100,LEFT(cSay,100),cSay)
    MSGRUNVIEW(cSay,cSay)
  ENDIF

  // IF oDp:oMsgRun<>NIL
  // oDp:oMsgRun:oSay:Show()
  oDp:oMsgRun:FRMTEXT(cSay)
  //  ENDIF


  //oDp:oMsgRun:FRMTEXT(cText,bBlq)

// oDp:oMsgRun:FRMTEXT(cText,bBlq)
//  oDp:oFrameDp:SetText(oDp:oMsgRun:ClassName())
// ,"DpMsgSetText"

/*
  oDp:oMsgRun:oSay:Show()
  oDp:oMsgRun:oSay:SetText(cText)
  oDp:oMsgRun:oSay:Refresh(.T.)

  oDp:oFrameDp:SetText(cText)
*/

  IF ValType(bBlq)="B"
     EVAL(bBlq)
  ENDIF

RETURN NIL


FUNCTION MSGRUNVIEW(cTitle,cText,nData,lStop,lReset)
   LOCAL oDlg,oFontB
   LOCAL aCoors:=GetCoors( GetDesktopWindow() )

   DEFAULT oDp:oMsgRun:=NIL,;
           lReset     :=.F.

   DEFAULT cTitle:="Procesando",;
           cText :=SPACE(100),;
           nData :=0,;
           lStop :=.F.

   nData:=CTOO(nData,"N")

   IF oDp:nGris2=12632256
     EJECUTAR("CONFIGSYSLOAD")
   ENDIF

   IF ValType(oDp:oMsgRun)="O" .AND. oDp:oMsgRun:oWnd:hWnd>0

      IF !lReset

        IF nData=0 .AND. oDp:oMsgRun:oMeter<>NIL

           oDp:oMsgRun:oMeter:Hide()

           IF !oDp:oMsgRun:oBtnStop=NIL
              oDp:oMsgRun:oBtnStop:Hide()
           ENDIF

        ENDIF

        oDp:oMsgRun:lStop:=.F.
        oDp:oMsgRun:oWnd:SetText(cTitle)
        oDp:oMsgRun:oWnd:Show()
        oDp:oMsgRun:oWnd:Refresh(.T.)

        RETURN oDp:oMsgRun

      ENDIF

      oDp:oMsgRun:Close()

   ENDIF

   // DPEDIT():New(cTitle,"DPMSGRUNVIEW.EDT","oFrm",.T.)
   DPEDIT():New(cTitle,"","oFrm",.F.)


   oDp:oMsgRun:=oFrm
   oDp:oMsgRun:lMsgBar :=.F.
   oDp:oMsgRun:nRadio  :=1
   oDp:oMsgRun:cText   :=cText
   oDp:oMsgRun:nRecord :=nData
   oDp:oMsgRun:lStop   :=.F.
   oDp:oMsgRun:oMeter  :=NIL
   oDp:oMsgRun:cTitle  :=cTitle
   oDp:oMsgRun:nData   :=nData
   oDp:oMsgRun:lStop_  :=lStop
   oDp:oMsgRun:lReset  :=lReset
   oDp:oMsgRun:oBtnStop:=NIL
   oDp:oMsgRun:oMeter  :=NIL
   oDp:oMsgRun:nClrPane:=oDp:nGris2

   oDp:oMsgRun:CreateWindow(NIL,aCoors[3]/2,aCoors[4]/2,130,400)


//   @ 0.5,.5 SAY "Proceso" SIZE 22,08 COLOR  NIL,oDp:nGris FONT oFontB

   @ 1,.5 SAY oDp:oMsgRun:oSay PROMPT oDp:oMsgRun:cText SIZE 350,30 COLOR NIL,oDp:nGris2

   IF nData>0
      @ 02,.5 METER oDp:oMsgRun:oMeter VAR oDp:oMsgRun:nRecord SIZE 190,10
   ENDIF

   IF lStop
     @ 4,01 BUTTON oDp:oMsgRun:oBtnStop PROMPT " Detener " ACTION oDp:oMsgRun:lStop:=.T.
   ENDIF

   IF nData>0
      oDp:oMsgRun:oMeter:SetTotal(nData)
   ENDIF

   oDp:oMsgRun:Activate()

   //SysRefresh(.T.)

   IF nData>0
      oDp:oMsgRun:oMeter:SetTotal(nData)
   ENDIF

   oDp:oMsgRun:FrmCentrar()

   IF nData>0
      oDp:oMsgRun:oMeter:SetTotal(nData)
   ENDIF

RETURN oDp:oMsgRun

FUNCTION SAYGRUPO(oObj,oFrm,n)
    LOCAL oSay

    IF !oFrm=NIL
         DEFAULT oFrm:aSayGroup:={}
    ENDIF

    IF oFrm=NIL

       @oObj:nTop()+0,oObj:nLeft()+9 STSAY oSay;
                          PROMPT oObj:GetText() OF oObj:oWnd;
                          COLORS oDp:nClrGroup SIZE oObj:nWidth(), 19 FONT oObj:oFont;
                          PIXEL

       RETURN oSay

    ELSE

       @oObj:nTop()+0,oObj:nLeft()+9 STSAY oFrm:aSayGroup[n];
                      PROMPT oObj:GetText() OF oObj:oWnd;
                      COLORS oDp:nClrGroup SIZE oObj:nWidth(), 19 FONT oObj:oFont;
                      PIXEL
    ENDIF

     // oFrm:aSayGroup[n]:Refresh(.T.)

RETURN oFrm:aSayGroup[n]
// EOF



