/*
// TMDI
// Encabezado del Documento
// 03/03/2002
*/

#include "FiveWin.ch"
#include "InKey.ch"
#include "xbrowse.ch"
#include "\DWH\LIBS\TSBUTTON.CH"
#include "Obj2Hb.ch"

// #define SM_CXSCREEN         0

MEMVAR oDp

STATIC nTop,nLeft,nWidth,nHeight,nContar
STATIC nNumEdit,aForms

FUNCTION DpMDI(cTitle,cVarName,cFileEdt,lWindows,oMenu)
RETURN TMDI():New(cTitle,cVarName,cFileEdt,lWindows,oMenu)

CLASS TMDI

    DATA aKeys,aSize,aSizeRestore
    DATA aBrwFocus INIT {}
    DATA aMsgItem  INIT {}
    DATA aParam    INIT {}   // Parametros recibidos desde oScript
    DATA aSayGroup INIT ARRAY(250)
    DATA aFieldsTable INIT {}
    DATA aTableLink   INIT {}
    DATA aSetKey      INIT {}

    DATA nForms,nClrPane,nClrText

    DATA cTitle,cVarName,cScript,cOnClose INIT ""

    DATA cCancel   INIT "CANCEL"
    DATA cPreSave  INIT "PRESAVE"
    DATA cPostSave INIT "POSTSAVE"
    DATA cLoad     INIT "LOAD"
    DATA cOrderBy  INIT ""
    DATA cTable    INIT "" // Nombre de la tabla del Browse necesario para definir colores con BRWSELCOLORFIELD

    DATA cMsgBar // Mensaje de Barra

    DATA oWnd
    DATA oDlg  // Compatibilidad con TDPEDIT
    DATA oScript,oControl
    DATA oFrom     // jn 9/2/2024 Refiere a la clase objeto donde buscar funciones que no estan en oScript, necesario para seccionar el programa o utilizar funcionalidades de otros formularios
                   // similar al SET PROCE TO
    DATA oFocus    // Este el Focus para Incluir y Modificar
    DATA oBrw      // Browse Propietario del Mdi
    DATA oBar
    DATA oMenu  INIT NIL
    DATA oBtnFilter
    DATA oDb,oTable
    DATA oFontBtn INIT NIL
    DATA oFontBrw INIT NIL // Fuente del Browse


    DATA aScrollSize INIT {} // Datos del ASIZE
    DATA oFrmLink    INIT NIL // Formulario Enlace, si esta activo ejecutara Refrescar

    DATA bLostFocus INIT {|| .T.} // 01/04/2024 NIL
    DATA bGotFocus  INIT {|| .T.} // 01/04/2024
    DATA bValid     INIT {|| .T.}

    DATA lMsgError,lIsDef
    DATA lActivated
    DATA lMsgBar
    DATA lDesign  INIT .F.
    DATA lBarDef  INIT .F.   // Controles de la barra Definible (Modo Diseño)
    DATA lBmpGetBtn INIT .F. // Asigna Boton > en controles BMPGET
    DATA lMdiBar    INIT .T. // Controles estan




    DATA lOpenTableChk INIT oDp:lOpenTableChk

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

    DATA cTextGroup   INIT ""


    DATA nClrPane1 INIT oDp:nClrPane1
    DATA nClrPane2 INIT oDp:nClrPane2
    DATA nClrPane3 INIT 0

    DATA nOption   INIT 0

    DATA nClrPaneBar INIT oDp:nGris // necesario para gotfocus

    DATA lVScroll INIT .F.
    DATA lHScroll INIT .F.
    DATA aScrollSize INIT {}

    DATA cMsgBar,cFileEdit INIT ""

    METHOD New( cTitle , cVarName , cFileEdit )
    METHOD Windows(nTop,nLeft,nWidth,nHeight,lMax)
    METHOD CreateWindow(X,x1,x2,y1,y2,lMax) INLINE  ::Windows(x1,x2,y1,y2,lMax)
    METHOD SetMenu(oMenu)
    METHOD NewFilter(nOption) INLINE EJECUTAR("BRWNEWFILTER",Self,nOption)


    METHOD IsDef( cName ) INLINE __objHasMsg( self, cName )

//    METHOD RestoreControls() INLINE ( AutoId(::oBar))
//
//,;
//                                    LoadFileEdt(::cFileEdit,SELF),;
//                                    PutControls(::oBar,.t.,.t.,nil,NIL,SELF),;
//                                    SetWndDefault(::oBar))

    METHOD SetScroll(n1,n2,n3,n4) INLINE (::lVScroll:=.T.,;
                                          ::lHScroll:=.T.,;
                                          ::lMsgBar :=.F.,;
                                          ::aScrollSize:={n1,n2,n3,n4})

    METHOD Activate(bInit,bEnd)

    // METHOD Resize() INLINE IIF(::aSize[1]=NIL , NIL  ,;
    // METHOD Resize() INLINE ::oWnd:Move(oWnd:nTop,oWnd:nLeft,::aSize[3],::aSize[4],.T.)

    METHOD GotFocus(lAuto)
    METHOD LostFocus()
    METHOD MensajeErr(cVar,cTitle) INLINE MensajeErr(cVar,cTitle)
    METHOD IsFunction(cFunction) INLINE ::oScript:IsFunction(cFunction)
//    METHOD ListBrw(cFileBrw)
    METHOD Get(cVarName)        INLINE __objSendMsg(Self,cVarName)
    METHOD Set(cVarName,uValue) INLINE (__objAddData( Self, cVarName ),__objSendMsg(Self,cVarName,uValue))
    METHOD Add(cVarName,uValue) INLINE ::Set(cVarName,uValue)

    METHOD SetMsgItem(cText,nlen,bAction) INLINE AADD(::aMsgItem,{cText,nLen,bAction,NIL})  // JN 12/07/2018
    METHOD SetMsgItemText(nId,cText)      INLINE IF(nId>0 .AND. nId<=LEN(::aMsgItem), (::aMsgItem[nId,4]:SetText(CTOO(cText,"C")),::aMsgItem[nId,4]:Refresh()),NIL)
    METHOD SetMsg(cMsgBar)                INLINE (::cMsgBar:=cMsgBar,;
                                                  IF(ValType(::oWnd)="O",(::oWnd:oMsgBar:SetMsg(::cMsgBar),::oWnd:oMsgBar:Refresh()),NIL))

//  METHOD Add(cField,uValue)
//  METHOD IsDef(cVarName)
//  METHOD RunKey( nKey, nFlags)

    METHOD RunScript(cFunction,nPar1,nPar2,nPar3,nPar4)
    METHOD IsFunction(cFunction) INLINE ::oScript:IsFunction(cFunction)
// 10/04/2016
    METHOD SetScript() /* INLINE (::oScript:=GetScript(),;
                               ::cScript:=VP("SCRPROGRAM")) */

    METHOD SETBTNBAR(nAlto,nAncho,oBar,nCol)

    METHOD SETTABLE()

    METHOD SetKey(nKey,bAction)

    METHOD SetDefault() // 8/5/2021
    METHOD ViewTable(cTable,cField,cWhere) // Indica la cantidad de tablas asociadas

//    METHOD Mensaje(cMensaje,cTitle)
    METHOD Prepare() INLINE (PUBLICO("oMdi",Self),PUBLICO(::cVarName,Self),MOVER(Self,::cVarName))
    METHOD End() INLINE ::oWnd:End()
    METHOD ONCLOSE()            // jn 01/09/2023
    METHOD SetFunction(cProgram) // 9/02/2024
    METHOD Close() INLINE ::End()
    METHOD BTNSETCOLOR(nOption) INLINE EJECUTAR("BRWMENUCOLORSET",Self,nOption) // EJECUTADO DESDE BRBTNMENUCOLOR, BOTON COLOR

    METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                             WndHandleEvent( Self, nMsg, nWParam, nLParam )

    ERROR HANDLER OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 )

ENDCLASS

METHOD New(cTitle, cVarName , cFileEdit , lWindows,oMenu) CLASS TMDI
     LOCAL cScript

     DEFAULT lWindows:=.F.

     ::aKeys        :={}
     ::lIsDef       :=.F.
     ::cTitle       :=cTitle
     ::cVarName     :=cVarName
     ::lActivated   :=.F.
     ::lMsgBar      :=.T.
     ::lWindows     :=lWindows
     ::cFileEdit    :=cFileEdit
     ::aSizeRestore :={NIL,NIL,NIL,NIL}
     ::oMenu        :=oMenu
     ::lOpenTableChk:=oDp:lOpenTableChk

     oDp:lOpenTableChk:=.F.

     PUBLICO("oMdi",Self) // 8/04/2020

     PUBLICO(::cVarName,Self)
     MOVER(Self,::cVarName)

     PUBLICO("oMdi",Self)
     // MOVER("oEdit",::cVarName)

     ::SetScript() // 02/09/2023
     ::cScript :=::oScript:cProgram
     ::cOnClose:="ONCLOSE"

     IF ValType(::oScript)="O"
        ::aParam:=ACLONE(::oScript:aParams) // Parametros
     ELSE
        ::aParam:={}
     ENDIF

     IF lWindows
        ::Windows() // Tamaño del Video
     ENDIF


     oDp:aId:={}

     IF !"FORMS\"$UPPER(::cFileEdit)
        ::cFileEdit:="FORMS\"+::cFileEdit
     ENDIF

     // Restaura Verificar Tabla
     oDp:lOpenTableChk:=::lOpenTableChk

RETURN SELF

/*
// Asignar Menu en MDI
*/
METHOD SetMenu(oMenu) CLASS TMDI

     ::oMenu:=oMenu

     IF !::oWnd:oMenu=NIL
        ::oWnd:SetMenu(::oMenu)
     ENDIF

RETURN .T.

/*
// Asume por Defecto las Opciones Básicas del Formulario
*/
METHOD   SetDefault() CLASS TMDI

  // Estos son Procedi  mientos Script
  ::cCancel  :="CANCEL"
  ::cPreSave :="PRESAVE"
  ::cPostSave:="POSTSAVE"
  ::cLoad    :="LOAD"

  IF ValType(::oTable)="O"
     ::cOrderBy :=::oTable:cPrimary // Asume PrimaryKey como Modo de Orden
  ENDIF

RETURN NIL


METHOD Windows(nTop,nLeft,nWidth,nHeight,lMax,oMenu) CLASS TMDI
     LOCAL oWnd
     LOCAL oIco

     IF ValType(oMenu)="O"
       ::oMenu:=oMenu
     ENDIF

     DEFAULT lMax:=.F.

     // Restaura los parametros del formato MDI, Especificamente Tamaño
     EJECUTAR("MDIRESTORE",Self)

     nWidth :=IF(::aSizeRestore[4]=NIL,nWidth ,::aSizeRestore[4])
     nHeight:=IF(::aSizeRestore[3]=NIL,nHeight,::aSizeRestore[3])

     ::aSize:={nTop,nLeft,nWidth,nHeight}

     IF ValType(nTop)="N" .AND. .F.
        nTop   :=0
        nLeft  :=0
        nWidth :=0
        nHeight:=0
     ENDIF

     // ::LoadData(0) // Carga los Datos en las Variables
     // DEFAULT nTop:=10,nLeft:=20,nWidth:=20,nHeight:=20
     //? ::nClrPane,::nClrText

//     DEFINE ICON oIco RESNAME "ICON"

     oIco:= oDp:oIco

   IF ValType(::oMenu)="O"

//       DEFINE WINDOW ::oWnd FROM 2,2 TO 28, 75
//       MENU oMenu NOZOOM;
//       NOMINIMIZE

      IF lMax

        DEFINE WINDOW ::oWnd;
               TITLE ::cTitle;
               FROM nTop,nLeft TO nWidth,nHeight PIXEL;
               COLOR ::nClrPane,::nClrText MENU ::oMenu

// ;  FONT oDp:oFontMenu

       ELSE

         DEFINE WINDOW ::oWnd;
               TITLE ::cTitle;
               FROM nTop,nLeft TO nWidth,nHeight PIXEL;
               COLOR ::nClrPane,::nClrText MENU ::oMenu NOZOOM;

       ENDIF

     // NO APLICA ::oWnd:SetFont(oDp:oFontMenu)

*/

//;
//              NOZOOM
//  MENU ::oMenu;
// NOZOOM
//       NOMINIMIZE
//    ENDIF


   ELSE

     IF lMax

       DEFINE WINDOW ::oWnd;
              MDICHILD OF oDp:oFrameDp;
              TITLE ::cTitle;
              FROM nTop,nLeft TO nWidth,nHeight PIXEL;
              COLOR ::nClrPane,::nClrText MENU ::oMenu

// ;  FONT oDp:oFontMenu
     ELSE

      DEFINE WINDOW ::oWnd;
             MDICHILD OF oDp:oFrameDp;
             TITLE ::cTitle;
             FROM nTop,nLeft TO nWidth,nHeight PIXEL;
             COLOR ::nClrPane,::nClrText;
             NOMAXIMIZE MENU ::oMenu
//;  FONT oDp:oFontMenu

      ENDIF

    ENDIF


    if ::lVScroll
       DEFINE SCROLLBAR ::oWnd:oVScroll VERTICAL OF ::oWnd
    endif

    if ::lHScroll
      DEFINE SCROLLBAR ::oWnd:oHScroll HORIZONTAL OF ::oWnd
    endif

     ::oWnd:SetIcon(oIco)
     SetWndDefault( ::oWnd )

     ::oWnd:Hide()

     ::oDlg:=::oWnd


RETURN Self
/*
// Recuperar Focus
*/
METHOD GotFocus(lAuto) CLASS TMDI
   // LOCAL oFont

   DEFAULT lAuto:=.F.

   ::Prepare()

   IF !lAuto
      ::oWnd:GotFocus()
   ENDIF

   MOVER(Self,::cVarName)

   //PUBLICO("oMdi",Self)

   // MOVER(Self,"oEdit")

   AEVAL(::aBrwFocus,{|a,n| EJECUTAR("BRWGOTFOCUS",a,SELF)})

   IF ValType(::bGotFocus)="B"
      EVAL(::bGotFocus) // personalizable
   ENDIF

RETURN NIL


/*
// Ejecución de Teclas
METHOD RunKey( nKey, nFlags)

   LOCAL nAt:=ASCAN(::aKeys,{|aKey,nAt|aKey[1]=nKey})

   IF nAt>0

      ::Prepare()
      Eval(::aKeys[nAt,2],Self)

   ENDIF

  //? nAt,nKey,"RUNKEY"

RETURN .T.
*/

/*
// Ejecuta Función del Script
*/
METHOD RunScript(cFunction,uPar1,uPar2,uPar3,uPar4,uPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)  CLASS TMDI

  LOCAL lResp:=.t.

  ::Prepare()

  IF !ValType(::oScript)="O"
     ::SetScript(::cScript)
  ENDIF

  IF !ValType(::oScript)="O"
     MensajeErr("Script"+::cScript,"No Está Compilado en Memoria ")
     RETURN NIL
  ENDIF

  IF !::oScript:IsFunction(cFunction)

     ::MensajeErr("Función : "+cFunction+" No Existe en  "+::oScript:cProgram)

  ELSE

     ::oScript:cError:=""

     lResp:=::oScript:Run(cFunction,uPar1,uPar2,uPar3,uPar4,uPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)

     // ? ::oScript:cError,"Error",oDp:cMsgError

     IF !Empty(::oScript:cError)
         MensajeErr(::oScript:cError,::cScript+ "FUNCTION "+cFunction)
     ENDIF

  ENDIF

RETURN lResp

/*
// Perdida de Focus
*/
METHOD LostFocus()

   IF !VP("oControl")=NIL // JN
      ::oControl:=VP("oControl")
   ENDIF

   IF !::bLostFocus=NIL
     EVAL(::bLostFocus,SELF)
   ENDIF

   AEVAL(::aBrwFocus,{|a,n| EJECUTAR("BRWLOSTFOCUS",a)})

   IF ValType(::bLostFocus)="B"
      EVAL(::bLostFocus) // personalizable
   ENDIF

RETURN NIL

METHOD SetTable(oTable) CLASS TMDI
    LOCAL cDataKey,cWhere,I,cField,uValue,lOpen:=.F.,oDb

    IF ValType(oTable)="C" // Indica el Comando

       oDb:=GetOdbc(oTable)

       DEFAULT ::oDb:=oDb // Si no tiene BD Asignada

       oTable:=OpenTable(oTable,.F.,::oDb)
       lOpen :=.T.

    ELSE

       ::oDb:=oTable:oOdbc // Base de Datos

    ENDIF

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
    ::oTable:=oTable

    IF lOpen
       ::oTable:End()
    ENDIF

RETURN :: oTable


METHOD SETSCRIPT(cScript)  CLASS TMDI

     IF !Empty(cScript)

        ::oScript:=GetScript(cScript)

        IF !ValType(::oScript)="O"
           LoadScript(cScript)
        ENDIF

        ::cScript:=cScript

        RETURN NIL
     ENDIF


     DEFAULT ::oScript:=oDp:oRunLine:oFunction:oScript
     DEFAULT ::cScript:=::oScript:cProgram


RETURN ::oScript


/*
// Asigna enlaces entre tablas, para Visualización
*/
// METHOD ViewTable(cTable,cField,cWhere,CampoLocal,CampoForeing) CLASS DPEDIT
METHOD ViewTable(cTable,cField,cCampoLink,cCampoLocal,cControlSay,cName) CLASS TMDI

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

METHOD SetKey(nKey,bAction) CLASS TMDI
   LOCAL nAt

   IF ValType(bAction)="O"
      bAction:=bAction:bAction
   ENDIF

   AADD(::aSetKey,{nKey,bAction})

RETURN nIL



/*
// Activa el Formulario
*/
METHOD ACTIVATE(bInit,bEnd,bResized,lErrorSys) CLASS TMDI
    //LOCAL oEdit:=SELF
    LOCAL oMdi:=Self,lSalir:=.F.
    LOCAL bAction,I

    DEFAULT lErrorSys:=.F.

    IF ValType(::oWnd)!="O"
      ::Windows()
    ENDIF

    ::SetScript()

    DEFAULT bInit   :={||.T.} ,;
            bEnd    :={||EJECUTAR("TMDIEND",oMdi),EVAL(oMdi:bValid),oMdi:ONCLOSE()},;
            bResized:={||.T.}

    bInit   :=BloqueCod(bInit)

    IF lErrorSys
      ErrorSys(.T.)
    ENDIF


    IF ValType(::oBar)="O" .AND. FILE(::cFileEdit)
       AutoId(::oBar)
       LoadFileEdt(::cFileEdit,oMdi)
       PutControls(::oBar,.t.,.t.,nil,NIL,Self)
    ENDIF

    bEnd    :=BloqueCod(bEnd)
    bResized:=BloqueCod(bResized)

    IF ::lMsgBar .OR. !Empty(::aMsgItem)  .OR. !Empty(::cMsgBar)

      DEFAULT ::cMsgBar:=::cTitle

      SET MESSAGE OF ::oWnd TO ::cMsgBar

    ENDIF

    ::aBrwFocus:=EJECUTAR("FRMGETBRW",Self)  // 08/11/2016

    ::oWnd:bGotFocus  :={||oMdi:GotFocus(.T.)}
    ::oWnd:bLostFocus :={||oMdi:LostFocus(.T.)}

// ChangeMenu(::oWnd,nil,"",oEdit),;

    FOR I=1 TO LEN(::aMsgItem)

       DEFAULT  ::aMsgItem[I,1]:=""                  ,;
                ::aMsgItem[I,2]:=LEN(::aMsgItem[I,1]),;
                ::aMsgItem[I,3]:={||.T.}

       bAction:=BloqueCod(::aMsgItem[I,3])

       DEFINE MSGITEM ::aMsgItem[I,4] OF ::oWnd:oMsgBar;
                      PROMPT ::aMsgItem[I,1] SIZE ::aMsgItem[I,2];
                      ACTION EVAL(bAction)

    NEXT I
//
//                   EJECUTAR("MDIRESTORECONTROLS",oMdi:oWnd,oMdi),;
//


    ACTIVATE WINDOW ::oWnd;
            ON INIT (EJECUTAR("MDISYSMENU",oMdi:oWnd,oMdi),;
                     EVAL(bInit) ,;
                     EJECUTAR("MDIRESTORECONTROLS",oMdi:oWnd,oMdi),;
                     IF(Empty(oMdi:aScrollSize),NIL, TScrWnd():New(oMdi:oWnd,oMdi:aScrollSize[1],oMdi:aScrollSize[2],oMdi:aScrollSize[3],oMdi:aScrollSize[4])),;
                     IF(ValType(::oWnd)="O",::oWnd:Show(),NIL),;
                     oMdi:lActivated:=.T. ) ;
            ON RESIZE EVAL(bResized);
            VALID (EJECUTAR("MDISAVECONTROLS",oMdi:oWnd,oMdi),EVAL(bEnd));
            ON MOVE EJECUTAR("BRWMOVED",oMdi:oWnd,oMdi);
            ON DOWN EJECUTAR("BRWMOVED",oMdi:oWnd,oMdi)


    IF ValType(::oBar)="O"

       DEFAULT oDp:nComboHeight:= 140

       AEVAL(::oBar:aControls,{|o| IF("COMBOBOX"$o:ClassName(),o:SetSize(NIL,oDp:nComboHeight),NIL)})

    ENDIF

    IF !Empty(::cScript)
      ::SETSCRIPT(::cScript)
    ENDIF


    IF ::lBmpGetBtn
       BMPGETBTN(::oBar)
    ENDIF

    IF ValType(::oBrw)="O" .AND. ::oFontBrw=NIL
      ::oFontBrw:=::oBrw:oFont
    ENDIF

//;
//                   IF(lSalir,::oWnd:End(),NIL))

//  ::oWnd:Refresh(.T.)
//  ::oWnd:bGotFocus :={||::GotFocus(.T.)}
  //::oWnd:bLostFocus:={||::LostFocus()}
  // SysRefresh(.T.) OJO 01/12/2007
  // ?EVAL(::oWnd:bGotFocus)

RETURN Self

/*
 *  OnError()
 */
METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TMDI

  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N
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
/*
    ::Prepare()

    IF !ValType(::oScript)="O"
       ::SetScript(::cScript)
    ENDIF
*/

    // busca prioritariamente en el script local, luego lo busca en el script asociado

    IF ValType(::oFrom)="O" .AND. !::oScript:Isfunction(cMsg)

// ? "AQUI BUSCAMOS LA FUNCION EN LA PPAL, LUEGO EN EL SIGUIENTE",::oForm

       IF !::oFrom:Isfunction(cMsg)

          MensajeErr("FUNCION "+cMsg+" no existe en "+::oFrom:cProgram)

       ELSE

          ::oFrom:cError:=""
          lResp:=::oFrom:Run(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)
          RETURN lResp

       ENDIF

       IF !Empty(::oFrom:cError)
         MensajeErr(::oFrom:cError,::oFrom:cScript+ "FUNCTION "+cMsg)
       ENDIF

    ELSE

      uValue:=::RunScript(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)

    ENDIF


/*
    IF !::oScript:IsFunction(cMsg)
        MensajeErr("REQUIERE COMPILAR "+::cScript+CRLF+"Funcion "+cMsg+" no Existe "+::CLASSNAME())
        uValue:=.F. // La mayoria de las Funciones requieren .T. / .F.
    ELSE
       // Ejecuta la Función
        RETURN ::RunScript(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11))
       // ::oScript:lNoError:=!::lMsgError
       ? "AQUI CORRE A EL SCRIPT"
       uValue=::oScript:Run(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)
    ENDIF
*/
  ENDIF

RETURN uValue

METHOD SETBTNBAR(nAlto,nAncho,oBar,nCol) CLASS TMDI
  LOCAL oMdi:=SELF,oBtn,I

  DEFAULT nCol:=15,;
          oBar:=::oBar

  ::oBar:=oBar

  //
  // IF oBar:nHeigth()><nAlto
  //   oBar:SetSize(NIL,nHeigth+2,.T.)
  // ENDIF

  __objAddData(oBar,"CARGO" )
  __objSendMsg(oBar,"CARGO",ARRAY(10))

  oBar:Cargo:=ARRAY(10)
  oBar:Cargo[1]:=.T.

  ::lBarDef:=.T.

  oBar:SetColor(NIL,oDp:nGris)

  FOR I=1 TO LEN(oBar:aControls)

     oBtn:=oBar:aControls[I]
     oBtn:CARGO:="TXBUTTON"

     IF "BTN"$oBtn:ClassName() .OR. "XBUTT"$oBtn:ClassName()

        oBtn:nInitId:=0
        oBtn:nResult:=0
        oBtn:Move(5,nCol+1)
        oBtn:SetSize(nAncho,nAlto)
        nCol:=nCol+oBtn:nWidth()

     ENDIF

  //  AEVAL(oBar:aControls,{|oBtn,n|oBtn:CARGO:="TXBUTTON",oBtn:nInitId:=0,oBtn:nResult:=0,oBtn:Move(5,nCol+1),oBtn:SetSize(nAncho,nAlto),nCol:=nCol+oBtn:nWidth()})

  NEXT I

  AutoId(oBar)

  IF !__objHasMsg( oMdi, "oBrw")

    ::bValid   :={|| EJECUTAR("BRWSAVEPAR",oMdi)}

    EJECUTAR("BRWRESTOREPAR",oMdi)

  ENDIF

//  LoadFileEdt(oMdi:cFileEdit,oMdi)
//
//  PutControls(oBar,.t.,.t.,nil,NIL,oMdi)
//
  SetWndDefault( oBar)

RETURN NIL

/*
// Ejecuta cuando se Cierra el Formulario, caso cuando cierra el Proceso de Reversion, concluye el proceso.
*/
METHOD ONCLOSE() CLASS TMDI

 IF Empty(::cOnClose)
   RETURN .T.
 ENDIF

 IF ::oScript=NIL
   ::oScript   :=GetScript(::cScript)
 ENDIF

 IF !ValType(::oScript)="O"
    RETURN .F.
 ENDIF

 IF !::oScript:IsFunction(::cOnClose)
    RETURN .T.
 ENDIF

RETURN ::RunScript(::cOnClose)

/*
// Asocia otro objeto SCRIPT para ejecuta funciones Asociadas 08/02/2024
*/
METHOD SetFunction(cScript) CLASS TMDI

  ::oFrom:=GetScript(cScript)

  IF ::oFrom=NIL
     COMPILA(cScript,nil) // compila y pasa a memoria
     ::oFrom:=GetScript(cScript)
  ENDIF

RETURN ::oFrom


FUNCTION BMPGETBTN(oBar,oFont,nAncho)
  LOCAL I,oGet,oBtn

  DEFAULT nAncho:=16


  IF oBar=NIL
     RETURN NIL
  ENDIF

  IF "BMPGET"$oBar:ClassName()

     oGet:=oBar

     @ oGet:nBottom-oGet:nHeight(),oGet:nLeft+oGet:nWidth();
       BUTTON oBtn PROMPT ">" PIXEL;
       SIZE nAncho,oGet:nHeight()+0   FONT oGet:oFont OF oGet:oWnd CANCEL FONT oFont

     oBtn:bAction:=oGet:bAction
     oBtn:bWhen  :=oGet:bWhen

     oBtn:Move(oGet:nTop,oGet:nLeft+oGet:nWidth)

     oBtn:Refresh(.T.)

     RETURN oBtn

  ENDIF

  FOR I=1 TO LEN(oBar:aControls)

      oGet:=oBar:aControls[I]

      IF "BMPGET"$oGet:ClassName()

         @ oGet:nBottom-oGet:nHeight(),oGet:nLeft+oGet:nWidth() BUTTON oBtn PROMPT ">" PIXEL;
                           SIZE nAncho,oGet:nHeight()+1 FONT oGet:oFont OF oBar CANCEL  FONT oFont

         oBtn:bAction:=oGet:bAction
         oBtn:bWhen  :=oGet:bWhen


      ENDIF

  NEXT I

  IF ValType(oBtn)="O"
    oBtn:ForWhen(.T.)
  ENDIF

RETURN oBtn

/*
// Restaurar FONT en la barra de botones 16/08/2025
*/

FUNCTION MDIBARGOTFOCUS(oFrm,oBar,oFont)
   LOCAL I,oBtn

   DEFAULT oBar :=oFrm:oBar,;
           oFont:=oFrm:oFontBtn

   FOR I=1 TO LEN(oBar:aControls)

      oBtn:=oBar:aControls[I]

      IF "BTN"$oBtn:ClassName()
         oBtn:SETFONT(oFont)
      ENDIF

   NEXT I


RETURN .T.

/*
FUNCTION SETBTNGRPACTION(oMdiCli,cAction)
   LOCAL nGroup:=LEN(oMdiCli:oOut:aGroup)
   LOCAL oBtn  :=ATAIL(oMdiCli:oOut:aGroup[ nGroup, 2 ])

   oBtn:bAction   :=BLOQUECOD(cAction)
   oBtn:bLButtonUp:=oBtn:bAction
   oBtn:CARGO     :=cAction

RETURN oBtn
*/

#pragma BEGINDUMP

#include <Windows.h>
#include <wininet.h>
#include <hbapi.h>


HB_FUNC(QUITA_BTMENU)
{
 // HMENU MenuH = GetSystemMenu(( HWND ) hb_parnl( 1 ),FALSE);
  HMENU MenuH = GetSystemMenu(GetForegroundWindow(),FALSE);
  INT lMenu = hb_parnl( 2 );

  if ( lMenu == 1 ){
     DeleteMenu(MenuH, SC_MINIMIZE, MF_BYCOMMAND);
     EnableMenuItem(MenuH, SC_MINIMIZE, MF_GRAYED);
     }
  else if ( lMenu == 2 ){
     DeleteMenu(MenuH, SC_MAXIMIZE, MF_BYCOMMAND);
     EnableMenuItem(MenuH,SC_MAXIMIZE,MF_GRAYED);
     }
  else if ( lMenu == 3 ){
     DeleteMenu(MenuH, SC_CLOSE, MF_BYCOMMAND);
     EnableMenuItem(MenuH,SC_CLOSE,MF_GRAYED);
    }
  else {
     DeleteMenu(MenuH, SC_MINIMIZE, MF_BYCOMMAND);
     EnableMenuItem(MenuH, SC_MINIMIZE, MF_GRAYED);
     DeleteMenu(MenuH, SC_MAXIMIZE, MF_BYCOMMAND);
     EnableMenuItem(MenuH,SC_MAXIMIZE,MF_GRAYED);
     DeleteMenu(MenuH, SC_CLOSE, MF_BYCOMMAND);
     EnableMenuItem(MenuH,SC_CLOSE,MF_GRAYED);
  }

}

#pragma ENDDUMP
// EOF









