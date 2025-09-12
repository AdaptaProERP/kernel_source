/*
##############################################################################
# TXBrowse & TXBrwColumn class                                               #
# Copyright OZ Software 2002                                                 #
# Written by Ignacio Ortiz de Zuñiga                                         #
# (only available for FWH)                                                   #
##############################################################################
*/

#include "FiveWin.ch"
#include "InKey.ch"
#include "xbrowse.ch"

#define GWL_STYLE             -16

#define GW_HWNDFIRST            0
#define GW_HWNDNEXT             2

#define SM_CYVSCROLL            20
#define SM_CYHSCROLL             3

#define CS_DBLCLKS              8

#define COLOR_SCROLLBAR         0
#define COLOR_BACKGROUND        1
#define COLOR_ACTIVECAPTION     2
#define COLOR_INACTIVECAPTION   13
#define COLOR_MENU              4
#define COLOR_WINDOW            5
#define COLOR_WINDOWFRAME       6
#define COLOR_MENUTEXT          7
#define COLOR_WINDOWTEXT        8
#define COLOR_CAPTIONTEXT       9
#define COLOR_ACTIVEBORDER      10
#define COLOR_INACTIVEBORDER    11
#define COLOR_APPWORKSPACE      12
#define COLOR_HIGHLIGHT         13
#define COLOR_HIGHLIGHTTEXT     14
#define COLOR_BTNFACE           15
#define COLOR_BTNSHADOW         16
#define COLOR_GRAYTEXT          17
#define COLOR_BTNTEXT           18
#define COLOR_INACTIVECAPTIONTEXT 19
#define COLOR_BTNHIGHLIGHT      20

#define COL_EXTRAWIDTH        6
#define ROW_EXTRAHEIGHT       4
#define COL_SEPARATOR         2
#define BMP_EXTRAWIDTH        5

#define RECORDSELECTOR_WIDTH 25

#define BITMAP_HANDLE         1
#define BITMAP_PALETTE        2
#define BITMAP_WIDTH          3
// 3
#define BITMAP_HEIGHT         4
//4

#define DATATYPE_RDD          0
#define DATATYPE_ARRAY        1
//#define DATATYPE_ODBCDIRECT   2

#define VSCROLL_MAXVALUE      10000  // never set values above 32767

#ifdef __HARBOUR__
   #define DT_TOP                      0x00000000
   #define DT_LEFT                     0x00000000
   #define DT_CENTER                   0x00000001
   #define DT_RIGHT                    0x00000002
   #define DT_VCENTER                  0x00000004
   #define DT_BOTTOM                   0x00000008
   #define DT_WORDBREAK                0x00000010
   #define DT_SINGLELINE               0x00000020
   #define DT_EXPANDTABS               0x00000040
   #define DT_TABSTOP                  0x00000080
   #define DT_NOCLIP                   0x00000100
   #define DT_EXTERNALLEADING          0x00000200
   #define DT_CALCRECT                 0x00000400
   #define DT_NOPREFIX                 0x00000800
   #define DT_INTERNAL                 0x00001000
   #define DT_EDITCONTROL              0x00002000
   #define DT_PATH_ELLIPSIS            0x00004000
   #define DT_END_ELLIPSIS             0x00008000
   #define DT_MODIFYSTRING             0x00010000
   #define DT_RTLREADING               0x00020000
   #define DT_WORD_ELLIPSIS            0x00040000
   #define DT_NOFULLWIDTHCHARBREAK     0x00080000
   #define DT_HIDEPREFIX               0x00100000
#else
   #define DT_LEFT                     0
   #define DT_CENTER                   1
   #define DT_RIGHT                    2
   #define DT_VCENTER                  4
   #define DT_WORDBREAK                16
   #define DT_SINGLELINE               32
   #define DT_MODIFYSTRING             65536
   #define DT_EDITCONTROL              8192
#endif

#ifndef __HARBOUR__
   #xtranslate   DrawTextEx => DrawText
#endif

 #command @ <nRow>, <nCol> BMPGET [ <oGet> VAR ] <uVar> ;
            [ <dlg: OF, WINDOW, DIALOG> <oWnd> ] ;
            [ PICTURE <cPict> ] ;
            [ VALID <uValid> ] ;
            [ <color:COLOR,COLORS> <nClrFore> [,<nClrBack>] ] ;
            [ SIZE <nWidth>, <nHeight> ]  ;
            [ FONT <oFont> ] ;
            [ <design: DESIGN> ] ;
            [ CURSOR <oCursor> ] ;
            [ <pixel: PIXEL> ] ;
            [ MESSAGE <cMsg> ] ;
            [ <update: UPDATE> ] ;
            [ WHEN <uWhen> ] ;
            [ <lCenter: CENTER, CENTERED> ] ;
            [ <lRight: RIGHT> ] ;
            [ ON CHANGE <uChange> ] ;
            [ <readonly: READONLY, NO MODIFY> ] ;
            [ <pass: PASSWORD> ] ;
            [ <lNoBorder: NO BORDER, NOBORDER> ] ;
            [ <help:HELPID, HELP ID> <nHelpId> ] ;
            [ <resource: NAME, RESNAME, RESOURCE> <cResName> ];
            [ ACTION <uAction> ] ;
       => ;
          [ <oGet> := ] TBmpGet():New( <nRow>, <nCol>, bSETGET(<uVar>),;
             [<oWnd>], <nWidth>, <nHeight>, <cPict>, [{|This|<uValid>}],;
             <nClrFore>, <nClrBack>, <oFont>, <.design.>,;
             <oCursor>, <.pixel.>, <cMsg>, <.update.>, <{uWhen}>,;
             <.lCenter.>, <.lRight.>,;
             [\{|nKey, nFlags, Self| <uChange>\}], <.readonly.>,;
             <.pass.>, [<.lNoBorder.>], <nHelpId>, , , , , ,;
             ,<cResName>, [{|This|<uAction>}] )




STATIC lDo
MEMVAR oDp // JN

//----------------------------------------------------------------------------//
//----------------------------------------------------------------------------//

CLASS TXBrwColumn

   DATA oBrw,;          // Browse conteiner
        oDataFont,;     // Data font object, by default oBrw:oDataFont
        oHeaderFont,;   // Header font object, by default oBrw:oHeaderFont
        oFooterFont,;   // Footer font object, by default oBrw:oFooterFont
        oDragWnd,;      // Temporal window used for swaping columns
        oBtnList,;      // Button for edit with listbox
        oBtnElip,;      // Button for edit with user code-block
        oEditGet,;      // Get object for editing
        oEditLbx,;      // Listbox object for editing
        oFontGet,;      // Font de la Casilla
        oFrm            // Formulario AdaptaPro 2 Dic 2012

   DATA aBitmaps        // Two dimension arrays that holds all the bitmaps added
                        // aBitmaps[n, 1] -> handle
                        // aBitmaps[n, 2] -> palette
                        // aBitmaps[n, 3] -> width
                        // aBitmaps[n, 4] -> heigth

   DATA aOpcColor INIT {} // JN Opciones y Colores. {VALOR,COLOR} desde DPCAMPOSOPC, previanmente precargado desde BRWRESTOREPAR

#ifdef __HARBOUR__
   DATA aEditListTxt,;  // Array with all the literals to be shown on the Edit listbox
        aEditListBound  // Array with all the data to bound to the the edit get
                        // Wen this array is nil then aEditListTxt act as the bound array


#else
   DATA aEdListTxt,;    // Array with all the literals to be shown on the Edit listbox
        aEdListBound    // Array with all the data to bound to the the edit get
                        // Wen this array is nil then aEditListTxt act as the bound array
#endif

   DATA bStrData,;      // String data codeblock (returns a string)
        bBmpData        // Ordinal bitmap data codeblock (returns a
                        // number in the range 1-len(aBitmaps))

   DATA bEditValue,;    // codeblock to retrieve the value of the edit, if nil then bstrData is used
        bEditValid,;    // codeblock to validate edit gets
        bEditBlock,;    // codeblock to evaluate for the  "..." button.
        bEditWhen,;     // JN: Condicion de cada Columna
        bInitGet ,;     // Iniciación del Get
        bPostGet,;      // Finalización del Get
        bWhenGetValue        // := Clausula When para el Get

   DATA bKeyAction      // Bloque de Ejecución JN


        // bEditType,;     // := Determina el Tipo de Gets
        // bPicture  ;     // Determina Picture del GET
                        // If there is a Get active the value returned by the block is stuffed on the get


   DATA bOnPostEdit     // Code-block to be evaluated after the edition of the column.
                        // It receives ...

   DATA bLClickHeader,; // codeblock to be evaluated when left clicking on the header
        bRClickHeader,; // codeblock to be evaluated when right clicking on the header
        bLDClickHeader,; // Por ahora, mientras se define JN 27/07
        bLClickFooter,; // codeblock to be evaluated when left clicking on the footer
        bRClickFooter,; // codeblock to be evaluated when right clicking on the footer
        bLDClickData,;  // codeblock to be evaluated when left double clicking on the data
        bRClickData     // codeblock to be evaluated when right clicking on the data

   DATA bClrHeader,;    // default color pair for header
        bClrFooter,;    // default color pair for footer
        bClrStd,;       // default color pair for normal rows
        bClrSel,;       // default color pair for selected row
        bClrSelFocus,;  // default color pair for selected row when control has focus
        bClrEdit        // default color pair for edition

   DATA cHeader,;       // header string
        cFooter,;       // footer string
        cEditPicture,;  // Picture mask to be used for Get editing
        cSayPicture ,;  // := JN Picture de Visualización
        cOrder,;        // Used internally for autosorting (""->None, "A"->Ascending, "D"->Descending)
        cToolTip,;      // JN Evitar Error en Caja de Dialogo
        cValid,;        // JN Procedimiento de Validacion en DPEDIT/DPBODY
        cWhen,;         // Procedimiento de Prevalidacion DPEDIT/DPBODY
        cAction,;        // Procedimiento de Ejecución de Action/BTN
        cField,;          // Nombre del Campo caso de personalizacion del diccionario de datos
        cType,;          // tipo de Datos
        cFunction,;      // nombre de la función dentro de LBX para validar y Grabar su contenido
        nLen,;           //  longitud del campo CHAR para rellenarlo para su edición
        cTotal           // Formula del Total Para sumar Columnas, Ejemplo: [9]-[10]

   // DATA aSelect         // Selección Previa

   DATA nWidth,;        // Column width
        nWidthL,;       // Ancho del ListBox
        nDisplayCol,;   // Actual column display value in pixels
        nCreationOrder,;// Ordinal creation order of the columns
        nResizeCol,;    // Internal value used for column resizing
        nColArray ,;    // Posición del Array, necesario para el programa
        nPos            // Actual column position in the browse. If columns is not visible nPos == 0

   DATA nKey INIT 0     // Tecla de Función Asociadas a Cada Columna JN


   DATA nDataStrAlign,; // Data string alignment (left, center, right)
        nHeadStrAlign,; // Header string alignment (left, center, right)
        nFootStrAlign   // Footer string alignment (left, center, right)

   DATA nDataBmpAlign,; // Data bitmap alignment (left or right)
        nHeadBmpAlign,; // Header bitmap alignment (left or right)
        nFootBmpAlign   // Footer bitmap alignment (left or right)

   DATA nDataStyle,;    // Style for data string (DrawTextEx() flags)
        nHeadStyle,;    // Style for header string (DrawTextEx() flags)
        nFootStyle      // Style for footer string (DrawTextEx() flags)

   DATA nHeadBmpNo,;    // header ordinal bitmap to use of ::aBitmaps
        nFootBmpNo;     // footer ordinal bitmap to use of ::aBitmaps
        AS NUMERIC

   DATA hEditType;      // 0-> None, 1-> Get, 2-> ListBox, 3-> User block
        AS NUMERIC;     // 4-> Get+ListBox, 5-> Get+User block
        HIDDEN          // For types 4 and 5 the buttons only update the edit value
                        // with the array aEditListBound on case 4 and with the value
                        // returned by the codeblock on case 5.
                        // It must be accesed through ::nEditType

   DATA lAllowSizing,;  // If .t. Column visual resizing is allowed
        lEditBorder,;   // If .t. Edit Get has border
        lHide,;         // If .t. the column is not visible
        lOnPostEdit,;   // .t. when OnPostEdit validating
        lValid,;        // JN: Indica si ralizo la validacion
        lWidth,;        // JN Ancho como el Get Natural
        lBmpGet,;       // JN BmpGet
        lColor,;        // Color de Get Standar
        lEraseBox,;     // Borra la Caja de Edición
        lAutoList;      // Auto ListBox, Dispara el ListBox
        AS LOGICAL

   DATA lCustom   INIT .F. // JN Personalizable
   DATA lAvg      INIT .F. // Total es Promediado
   DATA lTotal    INIT .F. // Totalizador

   DATA lSelectAll INIT .T. // seleccionar todo, JN 30/06/2024

   DATA lButton INIT .F.  // Indica si EDIT_GET_LISTBOX, ejecuta el postvalid cuando se ejecuta el boton o cuando se valida la columna (JN20082014)

   DATA Cargo           // For your own use

   METHOD New( oBrw )   // Creates a new instance of the class

   METHOD End()         // Destroys the object

   METHOD Edit( nKey )  // Goes to Edit mode (::nEditMode should be 1,2, 0 3)
                        // When nEditMode is greater than 0,this method is called
                        // automatically when doubleclicling with the mouse or pushing
                        // the Enter key. nKey is a first key to stuff into the Get
                        // Note: On multiline gets Ctrl+Enter is used to confirm the edition

   METHOD nEditType();  // Sets or Gets de Edit Mode
          SETGET        // 0=none, 1=Get, 2=Listbox, 3=block, 4=Get+Listbox, 5 = Get+block

   METHOD AddResource( cRes )  // Adds a new bitmap to the ::aBitmpas array giving his resource name
   METHOD AddBmpFile( cFile )  // Adds a new bitmap to the ::aBitmpas array giving his file name
   METHOD AddBmpHandle( hBmp ) // Adds a new bitmap to the ::aBitmpas array giving his bitmap handle

   METHOD DefStyle( nAlign,;  // Aid method to set the flag style for draw text operation
                lSingleLine ) // Draw Text operations are based on the Windows API function
                              // DrawTextEx(). This method receives two parameters: the alignment
                              // and if is a single line or multiline.


   METHOD Show() INLINE ( ::lHide := .f.,;             // Hides the column
                          ::oBrw:GetDisplayCols(),;
                          ::oBrw:Refresh() )

   METHOD Hide() INLINE ( ::lHide := .t.,;             // Shows the column
                          ::oBrw:GetDisplayCols(),;
                          ::oBrw:Refresh() )

   METHOD SetKey(nKey,bAction) // Asignar Tecla de Función al control get

   METHOD IsEdit() // Determina si la Columna esta Editable, JN
   METHOD KillGet()

   // The rest of the methods are used internally

   METHOD Adjust()
   // METHOD SetCombo()

   METHOD HeaderHeight()
   METHOD HeaderWidth()
   METHOD FooterHeight()
   METHOD FooterWidth()
   METHOD DataHeight()
   METHOD DataWidth()

   METHOD PaintHeader( nRow, nCol, nHeight, lInvert, hDC )
   METHOD PaintData( nRow, nCol, nHeight, lHighLite, lSelected, nOrder )
   METHOD EraseData( nRow, nCol, nHeight, hBrush, lFixHeight )
   METHOD Box( nRow, nCol, nHeight, lDotted)
   METHOD PaintFooter( nRow, nCol, nHeight, lInvert )

   // JN
   METHOD VarPut(uValue,lRefresh)
   METHOD RUN(cFunction,uPar1,uPar2,uPar3,uPar4,uPar5) INLINE ::oBrw:RUN(cFunction,uPar1,uPar2,uPar3,uPar4,uPar5) // 15/03/2025

   METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                             WndHandleEvent( Self, nMsg, nWParam, nLParam )


#ifdef __HARBOUR__
   METHOD HeaderLButtonDown( nRow, nCol, nFlags )
   METHOD HeaderLButtonUp( nRow, nCol, nFlags )
   METHOD FooterLButtonDown( nRow, nCol, nFlags )
   METHOD FooterLButtonUp( nRow, nCol, nFlags )
#else
   METHOD HLButtonDown( nRow, nCol, nFlags )
   METHOD HLButtonUp( nRow, nCol, nFlags )
   METHOD FLButtonDown( nRow, nCol, nFlags )
   METHOD FLButtonUp( nRow, nCol, nFlags )
#endif
   METHOD MouseMove( nRow, nCol, nFlags )
   METHOD ResizeBeg( nRow, nCol, nFlags )
   METHOD ResizeEnd( nRow, nCol, nFlags )

   METHOD CreateButtons()
   METHOD ShowBtnList()
   METHOD RunBtnAction()
   METHOD PostEdit()

   METHOD IsVisible( lComplete ) INLINE ( ! ::lHide .and. ::oBrw:IsDisplayPosVisible( ::nPos, lComplete ) )
   METHOD RefreshFooter() // JN 21/04/2016
   METHOD OpcGetColor(uValue) // JN 21/04/2016

//   METHOD PaintFooter( nRow, nCol, nHeight, lInvert )  // 21/04/2016
//   METHOD PaintHeader( hDC, aCols, nLast, hWhitePen, hGrayPen, hColPen )
//   METHOD PaintFooter( hDC, nBrwWidth, nBrwHeight, hWhitePen, hGrayPen )


ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( oBrw, uValue ) CLASS TXBrwColumn

   ::oBrw  := oBrw

   ::nPos  :=LEN(oBrw:aCols)+1 // JN
   ::nLen  :=0                 // JN 30/06/2024
   // ? ::nPos

   ::aBitmaps := {}

   ::cHeader  := ""
   ::cFooter  := ""
   ::cOrder   := ""
   ::lColor   :=.F.
   ::lEraseBox:=.F.

   ::lAutoList:=.F. //  JN Auto ListBox, Dispara el ListBox

   ::nDisplayCol    := 0
   ::nCreationOrder := len( oBrw:aCols ) + 1

   ::nColArray      :=::nCreationOrder // jn 21/11/2024

   ::lAllowSizing := .t.

   // JN 29/09/2020 para crear las columas Numerica

   IF ValType(uValue)="N"

     DEFAULT ::nDataStrAlign := AL_RIGHT,;
             ::nDataBmpAlign := AL_RIGHT,;
             ::nHeadStrAlign := AL_RIGHT,;
             ::nFootStrAlign := AL_RIGHT,;
             ::nHeadBmpAlign := AL_RIGHT,;
             ::nFootBmpAlign := AL_RIGHT

// ? ::ClassName(),"NUMERICA"

   ENDIF

return Self

//----------------------------------------------------------------------------//

METHOD End() CLASS TXBrwColumn

   local nFor

   if ::oBtnList != nil
      ::oBtnList:hBitmap1 := 0
      ::oBtnList:End()
   endif

   if ::oBtnElip != nil
      ::oBtnElip:End()
   endif

   if ::oEditGet != nil
      ::oEditGet:End()
   endif

   if ::oEditLbx != nil
      ::oEditLbx:End()
   endif

   for nFor := 1 to len( ::aBitmaps )
      // PalBmpFree( ::aBitmaps[ BITMAP_HANDLE ], ::aBitmaps[ BITMAP_PALETTE ] )
      PalBmpFree( ::aBitmaps[nFor, 1], ::aBitmaps[ nFor,2 ] )
   next

return nil

//----------------------------------------------------------------------------//

METHOD Adjust() CLASS TXBrwColumn

   DEFAULT ::oDataFont   := ::oBrw:oFont,;
           ::oHeaderFont := ::oBrw:oFont,;
           ::oFooterFont := ::oBrw:oFont

   DEFAULT ::nDataStrAlign := AL_LEFT,;
           ::nDataBmpAlign := AL_LEFT,;
           ::nHeadStrAlign := AL_LEFT,;
           ::nFootStrAlign := AL_LEFT,;
           ::nHeadBmpAlign := AL_LEFT,;
           ::nFootBmpAlign := AL_LEFT

   DEFAULT ::bClrHeader   := ::oBrw:bClrHeader,;
           ::bClrFooter   := ::oBrw:bClrFooter,;
           ::bClrStd      := ::oBrw:bClrStd,;
           ::bClrSel      := ::oBrw:bClrSel,;
           ::bClrSelFocus := ::oBrw:bClrSelFocus ,;
           ::bClrEdit     := ::bClrStd

   DEFAULT ::nWidth := Max( Max( ::HeaderWidth(),;
                                 ::FooterWidth() ),;
                            ::DataWidth() ) + COL_EXTRAWIDTH

   DEFAULT ::nDataStyle := ::DefStyle( ::nDataStrAlign, ( ::oBrw:nDataLines == 1 ) ),;
           ::nHeadStyle := ::DefStyle( ::nHeadStrAlign, ( ::oBrw:nHeaderLines == 1 ) ),;
           ::nFootStyle := ::DefStyle( ::nFootStrAlign, ( ::oBrw:nFooterLines == 1 ) )

   ::CreateButtons()

return nil

//----------------------------------------------------------------------------//

METHOD HeaderHeight() CLASS TXBrwColumn

   local nHeight

   nHeight := 0

   if !Empty( ::cHeader ) .and. ::oHeaderFont != nil
      nHeight := FontHeight( ::oBrw, ::oHeaderFont )
   endif

   if ::nHeadBmpNo > 0 .and. ::nHeadBmpNo <= len( ::aBitmaps )
      nHeight := Max( nHeight, ::aBitmaps[ ::nHeadBmpNo, BITMAP_HEIGHT ] )
   endif

return nHeight

//----------------------------------------------------------------------------//

METHOD HeaderWidth() CLASS TXBrwColumn

   local cText, cLine
   local nWidth, nFrom, nLen, nFor, nTemp

   cText  := ::cHeader
   nWidth := 0
   nFrom  := 1

   if !Empty( cText )
       nLen  := len( cText )
       While nFrom <= nLen
         cLine  := ExtractLine( cText, @nFrom )
         nWidth := Max( nWidth, ::oBrw:GetWidth( cLine, ::oHeaderFont ) )
       enddo
   endif

   if ::nHeadBmpNo > 0 .and. ::nHeadBmpNo <= len( ::aBitmaps )
      nWidth += ::aBitmaps[ ::nHeadBmpNo, BITMAP_WIDTH ] + BMP_EXTRAWIDTH
   endif

   if ::nHeadBmpNo == -1
      nTemp := nWidth
      for nFor := 1 to len( ::aBitmaps )
         nWidth := Max( nWidth, nTemp + ::aBitmaps[ nFor, BITMAP_WIDTH ] + BMP_EXTRAWIDTH )
      next
      ::nHeadBmpNo := 0
   endif

return Max( nWidth, 16 )

//----------------------------------------------------------------------------//

METHOD FooterHeight() CLASS TXBrwColumn

   local nHeight

   nHeight := 0

   if !Empty( ::cFooter ) .and. ::oFooterFont != nil
      nHeight := FontHeight( ::oBrw, ::oFooterFont )
   endif

   if ::nFootBmpNo > 0 .and. ::nFootBmpNo <= len( ::aBitmaps )
      nHeight := Max( nHeight, ::aBitmaps[ ::nFootBmpNo, BITMAP_HEIGHT ] )
   endif

return nHeight

//----------------------------------------------------------------------------//

METHOD FooterWidth() CLASS TXBrwColumn

   local cText, cLine
   local nWidth, nFrom, nLen

   cText  := ::cFooter
   nWidth := 0
   nFrom  := 1

   if !Empty( cText )
       nLen  := len( cText )
       While nFrom <= nLen
         cLine  := ExtractLine( cText, @nFrom )
         nWidth := Max( nWidth, ::oBrw:GetWidth( cLine, ::oFooterFont ) )
       enddo
   endif

   if ::nFootBmpNo > 0 .and. ::nFootBmpNo <= len( ::aBitmaps )
      nWidth += ::aBitmaps[ ::nFootBmpNo, BITMAP_WIDTH ] + BMP_EXTRAWIDTH
   endif

return Max( nWidth, 16 )

//----------------------------------------------------------------------------//

METHOD DataHeight() CLASS TXBrwColumn

   local nHeight, nBmp

   nHeight := 0

   if ::bStrData != nil .and. ::oDataFont != nil
      nHeight := FontHeight( ::oBrw, ::oDataFont )
   endif

   if ::bBmpData != nil
      nBmp  := Eval( ::bBmpData,Self )
      if nBmp > 0 .and. nBmp <= len( ::aBitmaps )
         nHeight := Max( nHeight, ::aBitmaps[ nBmp, BITMAP_HEIGHT ] )
      endif
   endif

return nHeight

//----------------------------------------------------------------------------//

METHOD DataWidth() CLASS TXBrwColumn

   local cText, cLine
   local nWidth, nFrom, nBmp, nLen

   nWidth := 0
   nFrom  := 1

   if ::bStrData != nil

       // IF ValType(::oBrw:aArrayData)="A" // .AND. ValType(::oBrw:aArrayData[::oBrw:nArrayAt,::nPos])<>"C"         // ValType(::bStrData)<>"B"
          // ? ::oBrw:nArrayAt,::oBrw:nColSel,ValType(::oBrw:aArrayData[::oBrw:nArrayAt,::oBrw:nColSel])         // ValType(::bStrData)<>"B"
         //  ? "AQUI ES, Error",::nPos,::oBrw:nArrayAt,ValType(::oBrw:aArrayData[::oBrw:nArrayAt,::oBrw:nColSel])
       // ENDIF
       cText := Eval( ::bStrData, Self )
       // IF ValType(cText)<>"C"
         // ? "AQUI ES",cText,ValType(cText)
       //  cText:=""
       // ENDIF
       nLen  := len( cText )
       While nFrom <= nLen
         cLine  := ExtractLine( cText, @nFrom )
         cLine  := Replicate( "B", Len( cLine ) )
         nWidth := Max( nWidth, ::oBrw:GetWidth( cLine, ::oDataFont ) )
       enddo
   endif

   if ::bBmpData != nil
      nBmp  := Eval( ::bBmpData,Self )
      if nBmp > 0 .and. nBmp <= len( ::aBitmaps )
         nWidth += ::aBitmaps[ nBmp, BITMAP_WIDTH ] + BMP_EXTRAWIDTH
      endif
   endif

   if ::nEditType > 1
      nWidth += 15
   endif

return nWidth

//----------------------------------------------------------------------------//

METHOD PaintHeader( nRow, nCol, nHeight, lInvert, hDC ) CLASS TXBrwColumn

   local hBrush
   local oFont
   local aColors, aBitmap
   local cHeader
   local nWidth, nBmpRow, nBmpCol, nBmpNo
   local lOwnDC

   DEFAULT lInvert := .f.

   if ::bClrHeader == nil
      ::Adjust()
   endif

   if nCol != nil
      if nCol != 0
         ::nDisplayCol := nCol
      endif
   else
      nCol := ::nDisplayCol
   endif

   if !lInvert
      aColors := Eval( ::bClrHeader )
   else
      aColors := {CLR_WHITE, CLR_BLUE }
   endif

   if hDC == nil
      hDC := ::oBrw:GetDC()
      lOwnDC := .f.
   else
      lOwnDC := .t.
   endif

   oFont   := ::oHeaderFont
   cHeader := ::cHeader
   nWidth  := ::nWidth
   hBrush  := CreateSolidBrush( aColors[ 2 ] )
   nBmpNo  := ::nHeadBmpNo

   FillRect( hDC, {nRow, nCol, nRow + nHeight, nCol + nWidth}, hBrush )

   DeleteObject( hBrush )

   nCol    += ( COL_EXTRAWIDTH / 2 )
   nWidth  -=  COL_EXTRAWIDTH
   nRow    += ( ROW_EXTRAHEIGHT / 2 )
   nHeight -=  ROW_EXTRAHEIGHT


   if nBmpNo > 0 .and. nBmpNo <= len( ::aBitmaps )
      aBitmap := ::aBitmaps[ nBmpNo ]
      nWidth  -= aBitmap[ BITMAP_WIDTH ]
      if Empty(cHeader)
         nBmpCol := nCol + ( ( nWidth - aBitmap[ BITMAP_WIDTH ] ) / 2 )
      elseif ::nHeadBmpAlign == AL_LEFT
         nBmpCol := nCol
         nCol    += aBitmap[ BITMAP_WIDTH ] + BMP_EXTRAWIDTH
      else
         nBmpCol := nCol + nWidth
      endif
      nWidth  -= BMP_EXTRAWIDTH
      nBmpRow := ( nHeight - aBitmap[ BITMAP_HEIGHT ] ) / 2 + 4
      PalBmpDraw( hDC, nBmpRow, nBmpCol,;
                  aBitmap[ BITMAP_HANDLE ],;
                  aBitmap[ BITMAP_PALETTE ],;
                  aBitmap[ BITMAP_WIDTH ],;
                  aBitmap[ BITMAP_HEIGHT ];
                  ,, .t., aColors[ 2 ] )
   endif

   if Empty( cHeader )
      ::oBrw:ReleaseDC()
      return nil
   endif

   oFont:Activate( hDC )
   SetTextColor( hDC, aColors[ 1 ] )
   SetBkColor( hDC, aColors[ 2 ] )

   DrawTextEx( hDC, cHeader,;
               {nRow, nCol, nRow + nHeight, nCol + nWidth},;
               ::nHeadStyle )
   oFont:Deactivate( hDC )

   if !lOwnDC
      ::oBrw:ReleaseDC()
   endif

return nil

//----------------------------------------------------------------------------//
// JN
METHOD SetKey(nKey,bAction) CLASS TXBrwColumn // Asignar Tecla de Función al control get

   bAction:=BloqueCod(bAction)

   ::nKey      :=nKey
   ::bKeyAction:=bAction

RETURN NIL

//----------------------------------------------------------------------------//

METHOD PaintFooter( nRow, nCol, nHeight, lInvert ) CLASS TXBrwColumn

   local hDC, hBrush
   local oFont
   local aColors, aBitmap
   local cFooter
   local nWidth, nBmpRow, nBmpCol, nBmpNo

   DEFAULT lInvert := .f.

   if nCol != nil
      ::nDisplayCol := nCol
   else
      nCol := ::nDisplayCol
   endif

   if !lInvert
      aColors := Eval( ::bClrFooter )
   else
      aColors := {CLR_WHITE, CLR_BLUE }
   endif

   hDC     := ::oBrw:GetDC()
   oFont   := ::oHeaderFont
   cFooter := ::cFooter
   nWidth  := ::nWidth
   hBrush  := CreateSolidBrush( aColors[ 2 ] )
   nBmpNo  := ::nFootBmpNo

   FillRect( hDC, {nRow, nCol, nRow + nHeight, nCol + nWidth}, hBrush )
   DeleteObject( hBrush )

   nCol    += ( COL_EXTRAWIDTH / 2 )
   nWidth  -=  COL_EXTRAWIDTH
   nRow    += ( ROW_EXTRAHEIGHT / 2 )
   nHeight -=  ROW_EXTRAHEIGHT

   if nBmpNo > 0 .and. nBmpNo <= len( ::aBitmaps )
      aBitmap := ::aBitmaps[ nBmpNo ]
      nWidth  -= aBitmap[ BITMAP_WIDTH ]
      if Empty(cFooter)
         nBmpCol := nCol + ( ( nWidth - aBitmap[ BITMAP_WIDTH ] ) / 2 )
      elseif ::nFootBmpAlign == AL_LEFT
         nBmpCol := nCol
         nCol    += aBitmap[ BITMAP_WIDTH ] + BMP_EXTRAWIDTH
      else
         nBmpCol := nCol + nWidth
      endif
      nWidth  -= BMP_EXTRAWIDTH
      nBmpRow := nRow + ( nHeight - aBitmap[ BITMAP_HEIGHT ] ) / 2 + 2
      PalBmpDraw( hDC, nBmpRow, nBmpCol,;
                  aBitmap[ BITMAP_HANDLE ],;
                  aBitmap[ BITMAP_PALETTE ],;
                  aBitmap[ BITMAP_WIDTH ],;
                  aBitmap[ BITMAP_HEIGHT ];
                  ,, .t., aColors[ 2 ] )
   endif

   if Empty( cFooter )
      ::oBrw:ReleaseDC()
      return nil
   endif

   oFont:Activate( hDC )
   SetTextColor( hDC, aColors[ 1 ] )
   SetBkColor( hDC, aColors[ 2 ] )

   DrawTextEx( hDC, cFooter,;
               {nRow, nCol, nRow + nHeight, nCol + nWidth},;
               ::nFootStyle )
   oFont:Deactivate( hDC )

   ::oBrw:ReleaseDC()

return nil

//----------------------------------------------------------------------------//

METHOD PaintData( nRow, nCol, nHeight, lHighLite, lSelected, nOrder ) CLASS TXBrwColumn

   local hDC, hBrush
   local oFont
   local aColors, aBitmap
   local cData
   local nWidth, nBmpRow, nBmpCol, nBmpNo, nBmpWidth, nButtonRow, nButtonCol,;
         nRectWidth, nRectHeight, nRectCol, nStyle, nType

   IF ValType(::oBrw)!="O" .OR. ::oBrw:hWnd=0 // 10/02/2004 JN, se cae cuando se cierra
      RETURN .F.
   ENDIF

   DEFAULT lHighLite := .f.,;
           lSelected := .f.,;
           nOrder    := 0

   If ::oEditGet != nil .or. ::oEditLbx != nil .or. ::oBrw:nLen == 0
      return nil
   Endif

   if nCol != nil
      ::nDisplayCol := nCol
   else
      nCol := ::nDisplayCol
   endif

   if ::bStrData != nil
      cData := Eval( ::bStrData, Self )
   else
      cData := ""
   endif

   if ::bBmpData != nil
      nBmpNo := Eval( ::bBmpData,Self )
   else
      nBmpNo := 0
   endif

   if !lHighLite
      aColors := Eval( ::bClrStd,Self ) // JN SELF
   else
      if ::oBrw:hWnd == GetFocus()
         aColors   := Eval( ::bClrSelFocus,Self )
      else
         aColors := Eval( ::bClrSel ,Self)
      endif
   endif

   hDC     := ::oBrw:GetDC()
   oFont   := ::oDataFont
   nWidth  := ::nWidth
   hBrush  := CreateSolidBrush( aColors[ 2 ] )
   nStyle  := ::oBrw:nColDividerStyle
   nType   := ::nEditType

   if nStyle == 0
      nRectWidth := nWidth + 2
      nRectCol   := nCol
   elseif nStyle < 5 .and. nOrder > 1
      nRectWidth := nWidth + 1
      nRectCol   := nCol - 1
   else
      nRectWidth := nWidth
      nRectCol   := nCol
   endif

   nRectHeight := nRow + nHeight

   FillRect( hDC, {nRow, nRectCol, nRectHeight, nRectCol + nRectWidth }, hBrush )

   nCol    += ( COL_EXTRAWIDTH / 2 )
   nWidth  -=  COL_EXTRAWIDTH
   nRow    += ( ROW_EXTRAHEIGHT / 2 )
   nHeight -=  ROW_EXTRAHEIGHT

   if nType > 1
      nButtonRow := nRow
      nButtonCol := nCol + nWidth - 10
      nWidth -= 15
   endif

   if nBmpNo > 0 .and. nBmpNo <= len( ::aBitmaps )
      aBitmap := ::aBitmaps[ nBmpNo ]
       nWidth  -= aBitmap[ BITMAP_WIDTH ]
      if ::bStrData == nil
         nBmpCol := nCol + Max( ( nWidth - aBitmap[ BITMAP_WIDTH ] ) / 2, 0 )// + 2
      elseif ::nDataBmpAlign == AL_LEFT
         nBmpCol := nCol
         nCol    += aBitmap[ BITMAP_WIDTH ] + BMP_EXTRAWIDTH
      else
         nBmpCol := nCol + nWidth
      endif
      nWidth    -= BMP_EXTRAWIDTH
      nBmpWidth := aBitmap[ BITMAP_WIDTH ]
      If nWidth < 0
         nBmpWidth := ::nWidth - COL_EXTRAWIDTH
      Endif
      nBmpRow := nRow + Max( ( nHeight - aBitmap[ BITMAP_HEIGHT ] ) / 2, 0 )
      BmpDrawNoStretch( hDC, nBmpRow, nBmpCol,;
                        aBitmap[ BITMAP_HANDLE ],;
                        aBitmap[ BITMAP_PALETTE ],;
                        nBmpWidth,;
                        aBitmap[ BITMAP_HEIGHT ];
                        ,, .t., aColors[ 2 ] )

   endif

   if !Empty( cData )
      oFont:Activate( hDC )
      SetTextColor( hDC, aColors[ 1 ] )
         SetBkColor( hDC, aColors[ 2 ] )
      DrawTextEx( hDC, cData,;
                  {nRow, nCol, nRow + nHeight, nCol + nWidth},;
                  ::nDataStyle )
      oFont:Deactivate( hDC )
   endif

   if nType > 1
      if lSelected
         WndBoxRaised(hDC, nButtonRow -1 , nButtonCol - 1,;
                     nButtonRow + nHeight, nButtonCol + 11 )
         if nType == EDIT_LISTBOX .or. nType == EDIT_GET_LISTBOX
            ::oBtnElip:Hide()
            ::oBtnList:Move( nButtonRow, nButtonCol, 11, nHeight, .f.)
            ::oBtnList:Show()
            ::oBtnList:GetDC()
            FillRect( hDC, {nButtonRow, nButtonCol,  nButtonRow + nHeight , nButtonCol + 11 },;
                      ::oBtnList:oBrush:hBrush )
            ::oBtnList:Paint()
            ::oBtnList:ReleaseDC()
         else
            ::oBtnList:Hide()
            ::oBtnElip:Move( nButtonRow, nButtonCol, 11, nHeight, .f.)
            ::oBtnElip:Show()
            ::oBtnElip:GetDC()
            FillRect( hDC, {nButtonRow, nButtonCol,  nButtonRow + nHeight , nButtonCol + 11 },;
                      ::oBtnElip:oBrush:hBrush )
            ::oBtnElip:Paint()
            ::oBtnElip:ReleaseDC()
         endif
      endif
   endif

   DeleteObject( hBrush )

   ::oBrw:ReleaseDC()

return nil

//----------------------------------------------------------------------------//

METHOD EraseData( nRow, nCol, nHeight, hBrush, lFixHeight ) CLASS TXBrwColumn

   local hDC
   local aColors
   local nWidth
   local lCreated

   DEFAULT lFixHeight := .f.

   lCreated := .f.

   if nCol != nil
      ::nDisplayCol := nCol
   else
      nCol := ::nDisplayCol
   endif

   if hBrush == nil
      aColors  := Eval( ::bClrStd , Self )
      hBrush   := CreateSolidBrush( aColors[ 2 ] )
      lCreated := .t.
   endif

   hDC     := ::oBrw:GetDC()
   nWidth  := ::nWidth

   if ::oBrw:nColDividerStyle < LINESTYLE_INSET
      nCol--
      nWidth++
   endif

   if ::oBrw:nColDividerStyle == LINESTYLE_NOLINES
      nWidth += 2
   endif

   if !lFixHeight .and. ::oBrw:nRowDividerStyle > LINESTYLE_NOLINES
      nHeight --
   endif

   if !lFixHeight .and. ::oBrw:nRowDividerStyle >= LINESTYLE_INSET
      nHeight --
   endif

   FillRect( hDC, {nRow, nCol, nRow + nHeight, nCol + nWidth}, hBrush )

   if lCreated
      DeleteObject( hBrush )
   endif

return nil

//----------------------------------------------------------------------------//

METHOD Box( nRow, nCol, nHeight, nType ) CLASS TXBrwColumn

   local hDC
   local nWidth

   DEFAULT nType := 1

   if nCol != nil
      ::nDisplayCol := nCol
   else
      nCol := ::nDisplayCol
   endif

   hDC     := ::oBrw:GetDC()
   nWidth  := ::nWidth

   if ::nPos > 1 .and. ::oBrw:nColDividerStyle < LINESTYLE_INSET
      nCol--
      nWidth++
   endif

   do case
   case nType == 1 // dotted
      DrawFocusRect( hDC, nRow, nCol, nRow + nHeight - 1, nCol + nWidth - 1 )
   case nType == 2 // sollid
      WndBox( hDC, nRow, nCol, nRow + nHeight - 1, nCol + nWidth - 1 )
   case nType == 3 // Raise
      WndBoxRaised( hDC, nRow, nCol, nRow + nHeight - 1, nCol + nWidth - 1 )
   endcase

return nil

//----------------------------------------------------------------------------//

METHOD AddResource( cResource ) CLASS TXBrwColumn

   local aBmpPal

   aBmpPal := PalBmpLoad( cResource )

   if aBmpPal[ BITMAP_HANDLE ] != 0
      Aadd(aBmpPal, nBmpWidth( aBmpPal[ BITMAP_HANDLE ] ) )
      Aadd(aBmpPal, nBmpHeight( aBmpPal[ BITMAP_HANDLE ] ) )
      Aadd(::aBitmaps, aBmpPal )
      PalBmpNew( 0, aBmpPal[ BITMAP_HANDLE ], aBmpPal[ BITMAP_PALETTE ] )
      return .t.
   endif

return .f.

//----------------------------------------------------------------------------//

METHOD AddBmpFile( cFile ) CLASS TXBrwColumn

   local aBmpPal

   aBmpPal := PalBmpRead( ::oBrw:GetDC(), cFile )

   ::oBrw:ReleaseDC()

   if aBmpPal[ BITMAP_HANDLE ] != 0
      Aadd(aBmpPal, nBmpWidth( aBmpPal[ BITMAP_HANDLE ] ) )
      Aadd(aBmpPal, nBmpHeight( aBmpPal[ BITMAP_HANDLE ] ) )
      Aadd(::aBitmaps, aBmpPal )
      return .t.
   endif

return .f.

//----------------------------------------------------------------------------//

METHOD AddBmpHandle( hBmp ) CLASS TXBrwColumn

   local aBmpPal

   aBmpPal := { hBmp, 0 }

   if aBmpPal[ BITMAP_HANDLE ] != 0
      Aadd(aBmpPal, nBmpWidth( aBmpPal[ BITMAP_HANDLE ] ) )
      Aadd(aBmpPal, nBmpHeight( aBmpPal[ BITMAP_HANDLE ] ) )
      Aadd(::aBitmaps, aBmpPal )
      return .t.
   endif

return .f.

//----------------------------------------------------------------------------//

METHOD DefStyle( nAlign, lSingleLine ) CLASS TXBrwColumn

   local nStyle

   nStyle := nOr( DT_MODIFYSTRING, DT_EDITCONTROL )

   do case
   case nAlign == AL_LEFT
      nStyle  := nOr( nStyle, DT_LEFT )
   case nAlign == AL_RIGHT
      nStyle  := nOr( nStyle, DT_RIGHT )
   case nAlign == AL_CENTER
      nStyle  := nOr( nStyle, DT_CENTER )
   end case

   if lSingleLine
      nStyle := nOr( nStyle, DT_SINGLELINE, DT_VCENTER)
   else
      nStyle := nOr( nStyle, DT_WORDBREAK)
   endif

return nStyle

//----------------------------------------------------------------------------//

#ifdef __HARBOUR__
   METHOD HeaderLButtonDown( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#else
   METHOD HLButtonDown( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#endif

   if ::oBrw:nCaptured == 0 .and. ::oBrw:lAllowColSwapping
      ::oBrw:oCapCol   := Self
      ::oBrw:nCaptured := 1
      ::oBrw:Capture()
      ::PaintHeader( 2, nil, ::oBrw:nHeaderHeight - 3, .t. )
   endif

return nil

//----------------------------------------------------------------------------//

#ifdef __HARBOUR__
   METHOD HeaderLButtonUp( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#else
   METHOD HLButtonUp( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#endif

   local oCol
   local nCol
   local lDragged

   lDragged := .f.

   if ::oDragWnd != nil
      ::oDragWnd:End()
      ::oDragWnd := nil
      lDragged := .t.
   endif

   ::PaintHeader( 2, nil, ::oBrw:nHeaderHeight - 3 )

   if !lDragged
      if ::bLClickHeader != nil .and. ;
         nMRow <= ::oBrw:nHeaderHeight  ;
         .and. nMCol <= ( ::nWidth + ::nDisplayCol )
         Eval( ::bLClickHeader, nMRow, nMCol, nFlags, Self )
      endif
   else
      nCol := ::oBrw:MouseColPos( nMCol )
      if nCol > 0
         oCol := ::oBrw:ColAtPos( nCol )
         if oCol:nCreationOrder != ::nCreationOrder
            ::oBrw:SwapCols( Self, oCol )
         endif
      endif
   endif

return nil

//----------------------------------------------------------------------------//

#ifdef __HARBOUR__
   METHOD FooterLButtonDown( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#else
   METHOD FLButtonDown( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#endif

   if ::oBrw:nCaptured == 0 .and. ::blClickFooter != nil
      ::oBrw:oCapCol   := Self
      ::oBrw:nCaptured := 2
      ::oBrw:Capture()
      ::PaintFooter( ::oBrw:FooterRow()+ 1 , nil, ::oBrw:nFooterHeight - 4, .t. )
   endif

return nil

//----------------------------------------------------------------------------//

#ifdef __HARBOUR__
   METHOD FooterLButtonUp( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#else
   METHOD FLButtonUp( nMRow, nMCol, nFlags ) CLASS TXBrwColumn
#endif

   ::PaintFooter( ::oBrw:FooterRow()+ 1 , nil, ::oBrw:nFooterHeight - 4)

   if ::bLClickFooter != nil .and. ;
      nMRow >= ::oBrw:FooterRow() .and. ;
      nMRow <= ( ::oBrw:FooterRow() + ::oBrw:nFooterHeight - 3 ) .and. ;
      nMCol >= ::nDisplayCol .and. ;
      nMCol <= ( ::nWidth + ::nDisplayCol )
      Eval( ::bLClickFooter, nMRow, nMCol, nFlags, Self )
   endif

return nil

//----------------------------------------------------------------------------//

METHOD ResizeBeg( nMRow, nMCol, nFlags ) CLASS TXBrwColumn

   local nCol, nWidth

   if ::oBrw:nCaptured == 0
      ::oBrw:oCapCol   := Self
      ::oBrw:nCaptured := 3
      ::oBrw:Capture()
      nCol   := ::nDisplayCol + ::nWidth
      nWidth := nCol + iif( ::oBrw:nColDividerStyle >= LINESTYLE_INSET, 3, 1)
      ::nResizeCol := nCol
      InvertRect( ::oBrw:GetDC(), { 0, nCol - 1 , ::oBrw:BrwHeight(),  nWidth + 1} )
      ::oBrw:ReleaseDC()
   endif

return nil

//----------------------------------------------------------------------------//

METHOD ResizeEnd( nMRow, nMCol, nFlags ) CLASS TXBrwColumn

   local nWidth

   if ::nResizeCol != nil
      nWidth := ::nResizeCol + iif( ::oBrw:nColDividerStyle >= LINESTYLE_INSET, 3, 1)
      InvertRect( ::oBrw:GetDC(), { 0, ::nResizeCol - 1, ::oBrw:BrwHeight(),  nWidth + 1 } )
      ::oBrw:ReleaseDC()
      if Abs( nMCol - ::nDisplayCol - ::nWidth ) > 2
         ::nWidth := Max( nMCol - ::nDisplayCol, 10 )
         ::oBrw:Refresh()
      endif
      ::nResizeCol := nil
   endif

return nil

//----------------------------------------------------------------------------//

METHOD MouseMove( nMRow, nMCol, nFlags ) CLASS TXBrwColumn

   local nRow, nCol, nWidth
   local hDC

   do case
   case ::oBrw:nCaptured == 1 // header
      if ::oDragWnd == nil .and. ;
         nMRow <= ::oBrw:nHeaderHeight  .and. ;
         nMCol <= ( ::nWidth + ::nDisplayCol ) .and. ;
         nMCol >= ::nDisplayCol
         return nil
      endif

      if !::oBrw:lAllowColSwapping
         return nil
      endif

      nRow := 0
      nCol := nMCol - ( ::nWidth / 2 )

      if ::oDragWnd == nil
         ::PaintHeader( 2, nil, ::oBrw:nHeaderHeight - 3 )
         DEFINE WINDOW ::oDragWnd OF ::oBrw STYLE WS_CHILD
         ::oDragWnd:bPainted := {| hDC | ::PaintHeader( 0, 0, ::oBrw:nHeaderHeight, .t., hDC ),;
                                 WndRaised( ::oDragWnd:hWnd, hDC ) }
         ::oDragWnd:Move(nRow, nCol, ::nWidth, ::oBrw:nHeaderHeight)
         ACTIVATE WINDOW ::oDragWnd
      else
         ::oDragWnd:Move(nRow, nCol, ::nWidth, ::oBrw:nHeaderHeight, .t.)
      endif

   case ::oBrw:nCaptured == 3 // width
      CursorWE()
      if nMCol > ( ::nDisplayCol + 10 ) .and. nMCol < ( ::oBrw:BrwWidth() - 10 )
         hDC    := ::oBrw:GetDC()
         nWidth := iif( ::oBrw:nColDividerStyle >= LINESTYLE_INSET, 3, 1)
         if ::nResizeCol != nil
            InvertRect( hDC, { 0, ::nResizeCol - 1 , ::oBrw:BrwHeight(),  ::nResizeCol + nWidth + 1 } )
         endif
         ::nResizeCol := nMCol
         InvertRect( hDC, { 0, nMCol - 1, ::oBrw:BrwHeight(),  nMCol + nWidth + 1} )
         ::oBrw:ReleaseDC()
      endif

   endcase

return nil

//----------------------------------------------------------------------------//

METHOD CreateButtons() CLASS TXBrwColumn

   local aColors

   if ::oBtnList != nil .and. ::oBtnElip != nil
      ::oBtnList:Hide()
      ::oBtnElip:Hide()
      return nil
   endif

   if ::oBrw:lCreated

      aColors := Eval( ::bClrHeader )

      @ 0,0 BTNBMP ::oBtnList RESOURCE "" OF ::oBrw NOBORDER SIZE 0,0
      ::oBtnList:hBitmap1 := ::oBrw:hBmpBtnList
      ::oBtnList:bAction := {|| ::ShowBtnList() }
      ::oBtnList:SetFont( ::oDataFont )
      ::oBtnList:SetColor( aColors[ 1 ], aColors[ 2 ] )
      // ::oBtnList:lCancel:=.T. // JN

      @ 0,0 BTNBMP ::oBtnElip OF ::oBrw NOBORDER SIZE 0,0;
            FILE "BITMAPS\findxcol.bmp"

      ::oBtnElip:cCaption :="" // "..."
      ::oBtnElip:bAction := {|| ::RunBtnAction() }
      ::oBtnElip:SetFont( ::oDataFont )
      ::oBtnElip:SetColor( aColors[ 1 ], aColors[ 2 ] )
      ::oBtnElip:lCancel:=.T.

      ::oBtnList:Hide()
      ::oBtnElip:Hide()

   endif

return nil

//----------------------------------------------------------------------------//

METHOD nEditType( nType ) CLASS TXBrwColumn

   if nType != nil
      ::hEditType := ntype
      ::CreateButtons()
   endif

return ::hEditType

//----------------------------------------------------------------------------//

METHOD Edit( nKey ) CLASS TXBrwColumn

   local aColors
   local uValue
   local nRow, nCol, nWidth, nHeight
   local hBrush
   local lCenter, lRight
   local lFocus:=.t. // jn
   Local uValue2 // jn
   LOCAL oCol  :=SELF // JN 30/09/2024
   // local cPicture // := JN

//   IF ValType(::oBrw:oLbx)="O" // JN
//      ::oBrw:oLbx:End()
//      ::oBrw:oLbx:=NIL
//   ENDIF


// IF ::oBtnList != nil .AND. ::lAutoList   // Auto ListBox, Dispara el ListBox
//   ::oBtnList:Refresh()
//   EVAL(::oBtnList:bAction)
// ENDIF

   DEFAULT lDo:=.F.

   //? "POSTEDIT"

   if ::bOnPostEdit == nil
      MsgStop("oCol:bOnPostEdit not defined",;
              "Fivewin: Class TXBrwColumn")
      return .f.
   endif

   IF ValType(::bInitGet)="B"
      EVAL(::bInitGet,SELF) // Iniciación de la Columna, Prepara los Valores
   ENDIF

   //IF ValType(::bEditType)="B" // JN
      // ::oBrw:CancelEdit()
      //   ::nEditType:=EVAL(::bEditType,::oBrw:nRowSel,::oBrw:nColSel,SELF) // Genera el Tipo de Gets
      // IF ::nEditType!=2
      //  ::aEditListBound:=NIL
      //  ::aEditListTxt  :=NIL
      // ENDIF
   // ENDIF

   // Determinar PICTURE
   // IF ValType(::bPicture)="B" // JN
   //   ::cEditPicture:=EVAL(::bPicture,::oBrw:nRowSel,::oBrw:nColSel,SELF)
   //   ::cEditPicture:=IIF( EMPTY(::cEditPicture), NIL  , ::cEditPicture )
   // ENDIF

      If ::nEditType == EDIT_LISTBOX
      return ::ShowBtnList( nKey )
   Endif

   If ::nEditType == EDIT_BUTTON
      return ::RunBtnAction()
   Endif

   If ::oEditGet != nil
      ::oEditGet:End()
   Endif

   if ::bEditValue == nil
      ::bEditValue := ::bStrData
   endif

   DEFAULT ::cEditPicture := ""

   uValue  := Eval( ::bEditValue )
   aColors := Eval( ::bClrEdit, self )
   lCenter := ( ::nDataStrAlign == AL_CENTER )
   lRight  := ( ::nDataStrAlign == AL_RIGHT )

   nRow    := ( ( ::oBrw:nRowSel - 1 ) * ::oBrw:nRowHeight ) + ::oBrw:HeaderHeight()

//? ValType(::bEditValue),"VALOR",uValue,Valtype(uValue)
//   ::oBrw:DrawLine()

IF ::lEraseBox
   hBrush := CreateSolidBrush( aColors[ 2 ] )
   ::EraseData( nRow, ::nDisplayCol, ::oBrw:nRowHeight , hBrush )
   DeleteObject( hBrush )
ENDIF

/*   IF !lDo .AND. ::lAutoList
     ::oBtnList:Refresh()
     EVAL(::oBtnList:bAction)
     lDo:=.T.
   ENDIF */

   ::oBrw:SetFocus()

   IF ValType(::bWhenGetValue)="B"
      uValue2:=Eval(::bWhenGetValue)
   ENDIF

   if Empty(::cEditPicture) .and. ::oBrw:nDataLines > 1

      ::oEditGet := TMultiGet():New(0,0,{ | u | If(PCount()==0,uValue,uValue:= u ) },;
                                    ::oBrw,0,0,,.F.,aColors[ 1 ],aColors[ 2 ];
                                    ,,.F.,,.F.,,lCenter,lRight,.F.,,,.F.,.T.,.T. )

   else // JN 5TO PAR ERA 0 ahora es nil


      DEFAULT ::lBmpGet:=.F.

      DEFAULT ::oFontGet:=::oDataFont

      IF ::lBmpGet

         @ 0,0 BMPGET ::oEditGet VAR uValue;
               NAME "BITMAPS\xfindxcol.bmp";
               ACTION (MsgAlert("Falta Acción"));
               PICTURE ::cEditPicture;
               COLOR aColors[ 1 ],aColors[ 2 ];
               SIZE NIL,::oBrw:nRowHeight +2;
               OF ::oBrw:oWnd FONT ::oFontGet

      ELSE

      // 30/09/2024
      IF ValType(::cType)="C"
         uValue:=CTOO(uValue,::cType)
      ENDIF

      IF ValType(uValue)="C" .AND. ::nLen>0
         uValue:=PADR(uValue,::nLen)
      ENDIF

      IF !Empty(oCol:cField)
        DPWRITE("TEMP\field_edit_"+oCol:cField+".TXT",CTOO(uValue,"C")+" Type="+ValType(uValue))
      ENDIF

      // 03/01/2021

      ::oEditGet := TGet():New(0,0,{ | u | If(PCount()==0,uValue,uValue:= u ) },;
                               ::oBrw,NIL,0,::cEditPicture,NIL,aColors[ 1 ],aColors[ 2 ];
                                  ,::oFontGet,.F.,,.F.,,.F.,,lCenter,lRight,,.F.,.f.,.T.,,.F.,,,,)

      ENDIF

      // ::oEditGet:cPicture:=::cEditPicture // jn
      // ? ::oEditGet:cPicture

   endif

   oDp:oGetFocus:=::oEditGet
   oDp:oCol     :=SELF    // Columna Editable

   nRow    := ( ( ::oBrw:nRowSel - 1 ) * ::oBrw:nRowHeight ) + ::oBrw:HeaderHeight() + 2
   nCol    := ::nDisplayCol + 3
   nWidth  := ::nWidth - 4
   nHeight := ::oBrw:nRowHeight - 4

   if ::nEditType > 2
      nWidth -= 13
   endif

//   Toda la logica del Valid ha sido pasada el metodo PostEdit() 30/03/03
//   if ::bEditValid != nil
//      ::oEditGet:bValid := ::bEditValid
//   endif

   //  por ahora ::oEditGet:bKeyDown   := { |nKey| EditGetkeyDown(Self, nKey) }

    if .F. .AND. ::oBtnList != nil .AND. ::lAutoList   // Auto ListBox, Dispara el ListBox
       lFocus:=.F.
       ::oBtnList:Refresh()
       EVAL(::oBtnList:bAction)
    endif

   ::oEditGet:bKeyDown   := { |nKey| EditGetkeyDown(Self, nKey) }
  //  oDp:oCol     :=SELF    // Columna Editable

   ::oEditGet:bLostFocus := { || iif(::oEditGet != nil, ::PostEdit(Eval( ::oEditGet:bSetGet )), NIL )} // 01/02/2018

   // ::oEditGet:bLostFocus := { || iif(::oEditGet != nil, ::PostEdit(Eval( ::oEditGet:bSetGet )), ) , IF(ValType(::oEditGet)="O",::oEditGet:End(),NIL) } // 01/02/2018
   ::oEditGet:bChange    := { |k,f,o| ::oBrw:nLastKey := k, .t. }
   ::oEditGet:bGotFocus  := NIL
   // 21/01/2018
   ::oEditGet:bGotFocus  := {|| ::lOnPostEdit:=.F. , IIF(ValType(::oEditGet)="O" , ::oEditGet:nLastKey := 0 , NIL ) } //  01/02/2018 NIL // JN 10/02/2004

//   RETURN .T.

   // IF !Empty(::aSelect)
   // OJO POR AHORA   ::oEditGet:bGotFocus  := { |k,f,o| ::oBrw:oLbx:=PutLista(::oEditGet,::oBrw,::aSelect,SELF)}  // JN
   // ENDIF

   ::oEditGet:nLastKey := 0  // No Cancela
   // ::oEditGet:oWnd     :=::oBrw:oWnd // JN necesario para el Cancel

   // IF ::nWidth!=NIL // JN Cantidad de Caracteres
   //   nWidth:=MIN(::nWidth,nWidth)
   // ENDIF

   IF ::lWidth // .AND.  ::nEditType == 1 // EDIT_BUTTON
       // JN El Ancho del Gets
      nWidth:=MIN(nWidth,(::oEditGet:nWidth*2)-2)
   ENDIF

   IF !::lBmpGet
      ::oEditGet:Move(nRow, nCol, nWidth, nHeight, .t. )
   ENDIF

   IF lFocus // JN
     ::oEditGet:SetFocus()
   ENDIF

   IF ::lSelectAll   // 30/06/2024
      ::oEditGet:SelectAll()
   ENDIF

   ::oBrw:lEditMode := .t.

   if ::oBtnElip != nil
      ::oBtnElip:Refresh()
   endif

   if ::oBtnList != nil
      ::oBtnList:Refresh()
   endif

   if ::lEditBorder
      WndBoxIn( ::oBrw:GetDC(), nRow-1, nCol-1, nRow + nHeight + 1, nCol + nWidth + 1)
      ::oBrw:ReleaseDC()
   endif

   If nKey != nil
      PostMessage( ::oEditGet:hWnd, WM_CHAR, nKey )
   Endif

   ::oBrw:nLastEditCol := ::nPos

   IF ValType(::bPostGet)="B"
      EVAL(::bPostGet,SELF) // Iniciación de la Columna, Prepara los Valores
   ENDIF

   // JN
   IF uValue2<>NIL .AND. ValType(::oEditGet)="O"
      // ? "uValue, desde el Get",uValue2
      ::oEditGet:VarPut(uValue2,.t.) // VarPut(uValue2,.T.)
      // removido por blanquear la columna 8/2/2017 ::oEditGet:KeyBoard(13) // JN 8/02/2017, Blanquea la columna  bLostFocus)
   ENDIF

// IF ::oBtnList != nil .AND. ::lAutoList   // Auto ListBox, Dispara el ListBox
//   ::oBtnList:Refresh()
//   EVAL(::oBtnList:bAction)
// ENDIF

return .t.

//----------------------------------------------------------------------------//

function EditGetkeyDown(Self, nKey)      // jn

   local lExit

   lExit := .f.

   //? nKey,::nKey
   // ::oEditGet:bLostFocus := { || iif(::oEditGet != nil, ::PostEdit(Eval( ::oEditGet:bSetGet )), ) }

   do case

      case nKey == VK_ESCAPE
         lExit := .t.

      case nKey == VK_RETURN

         // ? "aqui es enter"

         if Empty(::cEditPicture) .and. ::oBrw:nDataLines > 1
            if !GetKeyState( VK_CONTROL )
               lExit := .t.
            endif
         else
            lExit := .t.
         endif

         IF ValType(::oEditGet)="O" // JN
             // ? "AQUI SE EJECUTARA ENTER"
             ::lOnPostEdit:=.F.
             // ::oEditGet:bLostFocus := { || iif(::oEditGet != nil, ::PostEdit(Eval( ::oEditGet:bSetGet )), ) }
             // ::oEditGet:bChange    := { |k,f,o| ::oBrw:nLastKey := k, .t. }
             ::oEditGet:nLastKey:=13
             EVAL(::oEditGet:bLostFocus)
             // ? "AQUI ES ENTER "
         ENDIF

      case nKey == VK_F6 .AND. ValType(::bEditBlock)="B"

         // ::lOnPostEdit:=.T.
         // ::oBrw:Paint()
         EVAL(::bEditBlock)
         // ? "F6"

         RETURN 0

      case nKey == ::nKey // JN

         IIF( ValType(::bKeyAction)="B",  EVAL(::bKeyAction,::oEditGet,nKey,Self) ,  NIL )

//         ::oBrw:aArrayData[::oBrw:nArrayAt,::nPos]:="AQUI ES"

         IF ValType(::oEditGet)="O"

             ::PostEdit(Eval( ::oEditGet:bSetGet ))

         ELSEIF !oDp:cBrwPagGet=NIL

            // El control se Destruyo, hay que colocar en Valor en la columna
            // oDp:cBrwPagGet:=uValue
            ::oBrw:aArrayData[::oBrw:nArrayAt,::nPos]:= oDp:cBrwPagGet // "AQUI ES"
            ::PostEdit(oDp:cBrwPagGet) // Eval( ::oEditGet:bSetGet ))

         ENDIF

      case !(nKey == ::nKey) .AND. ValType(::bKeyAction)="B" .AND. ValType(::oEditGet)="O"// JN

         // mensajeErr(::nKey,nKey)
         // ::oEditGet:bLostFocus:=NIL
         // ::oBrw:nLastKey:=nKey

         EVAL(::bKeyAction,nKey,self)

      OTHER

//         IIF( ValType(::bKeyAction)="B",  EVAL(::bKeyAction,::oEditGet,nKey,Self) ,  NIL )

      // ? nKey

   end case

   /*if Valtype(::oEditGet)=!"O"
      RETURN 0
   Endif*/

   If lExit .AND. ValType(::oEditGet)="O"
      ::oEditGet:nLastKey := nKey
      ::oEditGet:Assign()
      //::oEditGet:End() modificado por linea abajo para control de Valid en ::OnPostEdit() 30/03/03
      ::PostEdit(Eval( ::oEditGet:bSetGet ))
   Endif

return 0

//----------------------------------------------------------------------------//

METHOD ShowBtnList( nKey ) CLASS TXBrwColumn

   local aBound
   local xValue
   local hBrush
   local nAt, nRow, nCol, nWidth, nHeight

   if ::aEditListTxt == nil
      MsgStop("oCol:aEditListTxt not defined", "Fivewin: Class TXBrwColumn")
      return .f.
   endif

   if ::bOnPostEdit == nil
      MsgStop("oCol:bOnPostEdit not defined",;
              "Fivewin: Class TXBrwColumn")
      return .f.
   endif

   ::oBrw:nColSel := ::nPos

   if ::bEditValue == nil
      ::bEditValue := ::bStrData
   endif

   nAt     := Max(Ascan( ::aEditListBound, Eval( ::bEditValue ) ), 1)
   nRow    := ( ::oBrw:nRowSel * ::oBrw:nRowHeight ) + ::oBrw:HeaderHeight() - 1
   nCol    := ::nDisplayCol - 2
   nWidth  := ::nWidth + 3

//  03/01/2021   nHeight := len(::aEditListTxt) * ( FontHeight( ::oBrw, ::oBrw:oFont ) ) + 2

   // Toma en cuenta el font de la Columna
   IF ::oDataFont=NIL
      nHeight := len(::aEditListTxt) * ( FontHeight( ::oBrw, ::oBrw:oFont ) ) + 2
   ELSE
      nHeight := len(::aEditListTxt) * ( FontHeight( ::oBrw, ::oDataFont ) ) + 2 // 03/01/2021
   ENDIF


   If nRow + nHeight > ::oBrw:BrwHeight()
      If (::oBrw:BrwHeight() - nRow) < ::oBrw:nRowHeight
         do while ( nRow -  nHeight - ::oBrw:nRowHeight + 1 ) < 0
            nHeight -= FontHeight( ::oBrw, ::oBrw:oFont )
         enddo
         nRow :=  nRow - nHeight - ::oBrw:nRowHeight + 1
      else
         nHeight := ::oBrw:BrwHeight() - nRow
      Endif
   Endif

   if ::aEditListBound == nil
      aBound := Array( len( ::aEditListTxt ) )
      Aeval( aBound, {|v,e| aBound[ e ] := e } )
   else
      aBound := ::aEditListBound
   endif

   if ::oEditGet != nil
      ::oEditGet:End()
      ::oEditGet := nil
      hBrush := CreateSolidBrush( Eval( ::bClrSel )[ 2 ] )
      ::EraseData( ( ( ::oBrw:nRowSel - 1 ) * ::oBrw:nRowHeight ) + ::oBrw:HeaderHeight(),;
                  ::nDisplayCol, ::oBrw:nRowHeight , hBrush )
      DeleteObject( hBrush )
   endif

   // oDataFont 03/01/2020
   @ 0, 0 LISTBOX ::oEditLbx VAR nAt OF ::oBrw SIZE 0,0 ITEMS ::aEditListTxt FONT ::oDataFont

   // ::oEditLbx:SetColor(oDp:Get_nCltText,oDp:Get_nClrPane) 03/01/2021
   ::oEditLbx:SetColor(oDp:Get_nCltText,oDp:nGris2) // Asume color del Formulario Definible



   //::oEditLbx:bLostFocus := {|| iif(::oEditLbx != nil, ::oEditLbx:End(),) }
   // Cambiado por la linea de abajo el día 27/01/03 para resolver el problema
   // de click fuera del listbox

   ::oEditLbx:bLostFocus := {|| iif(::oEditLbx != nil,;
                                     ::PostEdit( nil, .t. ),;
                                    ) }

   ::oEditLbx:bLostFocus := {|| xValue :=IIF( nAt >0,aBound[ nAt ],NIL),;
                                iif(::oEditLbx!=NIL.AND.(::oEditLbx:oWnd:nLastKey == VK_RETURN .OR. ::oEditLbx:oWnd:nLastKey == VK_TAB ) .and. nAt > 0, xValue := aBound[ nAt ], ),;
                                ::PostEdit( xValue, .t. ) } // JN: Seleccionar Con Enter


   ::oEditLbx:bLButtonUp := {|| ::oEditLbx:Change(),;
                                ::PostEdit( aBound[ nAt ], .t. );
                                }

   ::oEditLbx:bKeyDown   := {|k|  iif( k == VK_RETURN .and. nAt > 0, xValue := aBound[ nAt ], ),;
                                 iif( k == VK_RETURN .or. k == VK_ESCAPE, ::PostEdit( xValue, .t. ), ) }

   DEFAULT ::nWidthL:=nWidth

   // No debe Exceder
   WHILE ::nWidthL+nCol>::oBrw:BrwWidth()
       nCol--
   ENDDO

   ::nWidthL:=::nWidth // Asume el Ancho de la Columa replanteada JN 10/01/2024

   ::oEditLbx:Move(nRow, nCol+1, ::nWidthL, nHeight, .t. )
   ::oEditLbx:SetFocus()

return .t.

//----------------------------------------------------------------------------//

METHOD RunBtnAction() CLASS TXBrwColumn

   local nRow, nCol
   local hBrush

   if ::bEditBlock == nil
      MsgStop("oCol:bEditBlock not defined", "Fivewin: Class TXBrwColumn")
      return .f.
   endif

   ::oBrw:nColSel := ::nPos

   if ::oEditGet != nil
      ::oEditGet:End() // NO NECESITO QUE LO DESTRUYA
      ::oEditGet := nil
      hBrush := CreateSolidBrush( Eval( ::bClrSel )[ 2 ] )
      ::EraseData( ( ( ::oBrw:nRowSel - 1 ) * ::oBrw:nRowHeight ) + ::oBrw:HeaderHeight(),;
                  ::nDisplayCol, ::oBrw:nRowHeight , hBrush )
      DeleteObject( hBrush )
   endif

   nRow := ( ::oBrw:nRowSel * ::oBrw:nRowHeight ) + ::oBrw:HeaderHeight() - 3
   nCol := ::nDisplayCol

   ::PostEdit( Eval( ::bEditBlock, nRow, nCol, Self ), .t. )

return .t.

//----------------------------------------------------------------------------//

METHOD PostEdit( xValue, lButton ) CLASS TXBrwColumn
   Local lResp // := JN
   Local lGoNext := .f.
   LOCAL oDlg
   LOCAL nAt    :=::oBrw:nArrayAt
   LOCAL nRowSel:=::oBrw:nRowSel
   LOCAL lEdit  :=::nEditType>0

   ::lButton:=lButton

// oDp:oFrameDp:SetText(lstr(::oEditGet:nLastKey))
// ? lButton,GETPROCE()
// oDp:oCol:=NIL // JN

   If ::lOnPostEdit
      return nil
   Endif

   ::lOnPostEdit := .t.

   DEFAULT lButton := .f.

   do case

   case ::nEditType == EDIT_GET

      // ? ::oEditGet:nLastKey," SE REQUIERE CANCELAR ",::oEditGet:oWnd:lValidating
      // Aqui Solicita el Boton Cancelar

      If ::oEditGet != nil

//         MensajeErr(LSTR(::oEditGet:nLastKey))
//         MensajeErr(LSTR(::oEditGet:oWnd:nLastKey))

//         ErrorSys(.T.)


         IF ::oEditGet:nLastKey=0 // Para Cancelar ," SE REQUIERE CANCELAR ",::oEditGet:oWnd:lValidating

             ::KillGet()
//::oEditGet:End()
             ::oEditGet:=NIL

             // DPFOCUS()

             // ::oBrw:nArrayAt:=nAt
             // ::oBrw:nRowSel :=nRowSel

// ? "AQUI PIERDE FOCUS"

             // DPFOCUS()

//DPFOCUS(::oBrw)
//           ? "AQUI, killget",GETPROCE()
//           Aeval(::oBrw:aCols,{|oCol|oCol:KillGet()})
             // ::oEditGet:End()
             // ::oEditGet:=NIL
//             ? "ES AQUI?"
//             ? "ESTE LO MATA CANCEL"
//             MensajeErr(::oEditGet:oWnd:nLastKey,"TECLA ::oWnd")

             // ? "AQUI PIERDE EL FOCO"
             // ::oEditGet:nLastKey:=1

             RETURN NIL

         ENDIF

         if ::bEditValid != nil

            if !Eval( ::bEditValid, Eval( ::oEditGet:bSetGet ), ::oEditGet:nLastKey, Self )
            //   ? "AQUI ACTIVA EL FOCO"
               IF ::oEditGet!=NIL
                 ::oEditGet:SetFocus()
                 ::oEditGet:SelectAll()
               ENDIF
               ::lOnPostEdit := .f.
               return nil
            endif
         endif

         Eval( ::bOnPostEdit, Self, Eval( ::oEditGet:bSetGet ), ::oEditGet:nLastKey )

         IF  ValType(::oEditGet)="O" // JN
            lGoNext := ( ::oEditGet:nLastKey == VK_RETURN )
         ENDIF

         ::KillGet() // JN
         // ::oEditGet:End()
         // ::oEditGet := nil
      endif

   case ::nEditType == EDIT_LISTBOX

      // ? ::oEditLbx:nLastKey,::oEditLbx:Classname()

      If ::oEditLbx != nil .and. IsWindow( ::oEditLbx:hWnd )
         ::oEditLbx:End()
         ::oEditLbx := nil
      Endif

      Eval( ::bOnPostEdit, Self, xValue, 0)

      lGoNext:=.T. // JN Necesario para que se Dezplace en la Proxima Columna

   case ::nEditType == EDIT_BUTTON

      if ::bOnPostEdit != nil
         Eval( ::bOnPostEdit, Self, xValue, 0 )
      endif

   case ::nEditType == EDIT_GET_LISTBOX


      If ::oEditLbx != nil .and. IsWindow( ::oEditLbx:hWnd )
         ::oEditLbx:End()
         ::oEditLbx := nil
      Endif

      if !lButton

          If ::oEditGet != nil


             if ::bEditValid != nil // jn
               if !Eval( ::bEditValid, Eval( ::oEditGet:bSetGet ), ::oEditGet:nLastKey, Self )
                 ::oEditGet:SetFocus()
                 ::oEditGet:SelectAll()
                 ::lOnPostEdit := .f.
                 return nil
              endif // jn
            endif

            Eval( ::bOnPostEdit, Self, Eval( ::oEditGet:bSetGet ), ::oEditGet:nLastKey )
            lGoNext := ( ::oEditGet:nLastKey == VK_RETURN )
            ::oEditGet:End()
            ::oEditGet := nil

         Endif


      elseif xValue != nil

         Eval( ::bOnPostEdit, Self, xValue, 0 )

         ::oBrw:nLastKey = VK_RETURN
         lGoNext :=.T.

      endif

   case ::nEditType == EDIT_GET_BUTTON

      // ? "ESTE ES EDIT_GET_BUTTON",lButton,xValue

      if !lButton

         If ::oEditGet != nil

            lResp   :=Eval( ::bOnPostEdit, Self, Eval( ::oEditGet:bSetGet ), ::oEditGet:nLastKey ) // JN

            IF ValType(::oEditGet)="O" .AND. ValType(lResp)="L" .AND. !lResp // JN
              ::oEditGet:nLastKey:=0
            ENDIF // JN

            lGoNext:=.T.

            IF ::oEditGet!=NIL

               lGoNext :=  (::oEditGet:nLastKey == VK_RETURN )

               ::oEditGet:End()

            ENDIF

            ::oEditGet := nil

         endif
      elseif xValue != nil
         if ::bOnPostEdit != nil
            // lGoNext := ( ::oEditGet:nLastKey == VK_RETURN )
//            ::oBrw:nLastKey = VK_RETURN
//            lGoNext :=.T.
            Eval( ::bOnPostEdit, Self, xValue, 0 )

//            ::oBrw:nColSel:= ::oBrw:nLastEditCol


         endif
      endif

   endcase

   // ? ::oBrw:ClassName(),::oBrw:nArrayAt

   ::oBrw:SetFocus()
   ::oBrw:DrawLine( .t. )

   ::lOnPostEdit := .f.


//   ? lGoNext,;
//     ::oBrw:nColSel,;
//     ::oBrw:nLastEditCol

   // JN
   IF ::oBrw:lDownAuto .AND. (::nEditType == EDIT_GET       .OR. ;
                              ::nEditType == EDIT_LISTBOX   .OR. ;
                              ::nEditType == EDIT_GET_BUTTON)


      // ? "cambia, GoDown"
      // ::oBrw:GoDown()
      //  ::oBrw:DrawLine()
      // ::oBrw:DrawLine(.T.)
      ::oBrw:KeyBoard(VK_DOWN)
      // ::oBrw:DrawLine(.T.)

      lGoNext:=.F.

   ENDIF

   // Jn, Forzar Foco
   IF ValType(oDp:oFocus)="O" // JN
     //  ? "aqui es el nuevo foco, ",::Classname(),oDp:oFocus:cTitle // ClassName()
     //oDp:oFocus:SetFocus()
     DpFocus(oDp:oFocus)
     oDp:oFocus:=NIL
     lGoNext:=.F.

     // ? getproce()

   ENDIF

   If lGoNext
      ::oBrw:GoNextCtrl()
   Endif

return nil

//----------------------------------------------------------------------------//

METHOD IsEdit() CLASS TXBrwColumn
   LOCAL lResp:=.T.

   IF ::nEditType=0
      Return .F.
   ENDIF

   DO CASE

     case ::nEditType == EDIT_GET  .AND.  ::oEditGet != nil
          lResp:=.F.

     case ::nEditType == EDIT_GET_LISTBOX .AND. ::oEditLbx != nil
          lResp:=.F.

   ENDCASE

RETURN lResp

//----------------------------------------------------------------------------//

METHOD KillGet()   CLASS TXBrwColumn
//  LOCAL nAt    :=::oBrw:nArrayAt
//  LOCAL nRowSel:=::oBrw:nRowSel
//  LOCAL lEdit  :=::nEditType>0

  If ::oEditGet != nil
     ::oEditGet:End()
  Endif

  IF ::oBtnElip!=NIL
     ::oBtnElip:Hide()
     // ::oBtnElip:=NIL
  ENDIF

  ::oEditGet:=NIL

//  ::oBrw:nArrayAt:=nAt
//  ::oBrw:nRowSel :=nRowSel

RETURN NIL

Method VarPut(uValue,lRefresh,nAt) CLASS TXBrwColumn

   DEFAULT nAt     :=::oBrw:nArrayAt,;
           lRefresh:=.F.

   ::oBrw:aArrayData[nAt,::nPos]:=uValue

   IF lRefresh
     ::oBrw:DrawLine(.T.)
     ::oBrw:SetFocus()
   ENDIF

RETURN    .T.

//----------------------------------------------------------------------------//
// JN 21/04/2016
METHOD RefreshFooter() CLASS TXBrwColumn

   if ! Empty( ::oBrw:nFooterHeight ) .and. ::IsVisible()
      ::PaintFooter(::oBrw:FooterRow()+1,nil,::oBrw:nFooterHeight-4)
   endif

return nil
//------------------------------------------------------------------//
METHOD OpcGetColor(uValue) CLASS TXBrwColumn
  LOCAL nAt,nClrText:=0

  uValue:=ALLTRIM(uValue)
  nAt   :=ASCAN(::aOpcColor,{|a,n| uValue==a[1]})

  IF nAt>0
    nClrText:=::aOpcColor[nAt,2]
  ENDIF

  // aOpcColor
RETURN nClrText


// jn static
FUNCTION FontHeight( oBrw, oFont )

   local hDC
   local nHeight

   hDC := oBrw:GetDC()
   oFont:Activate( hDC )
   nHeight := GetTextHeight( oBrw:hWnd, hDC )
   oBrw:ReleaseDC()

return nHeight

//----------------------------------------------------------------------------//

STATIC FUNCTION ExtractLine(cText, nFrom)

  local cLine, cSearch, nAt

  cSearch := Substr(cText, nFrom)

  nAt := At(CRLF, cSearch)

  if nAt > 0
    cLine := Substr(cSearch, 1, nAt - 1 )
    nFrom := nAt + 2
  else
    cLine := cText
    nFrom := len(cText) + 1
  endif

Return cLine

/*
function ShowLista( oBrw, oGet )

   local aBound:={100,200,300}
   local xValue
   local hBrush
   local nAt:=1, nRow, nCol, nWidth, nHeight
   local oCol,oEditLbx
   local aEditListTxt,oDlg

   aEditListTxt:={"A:  100.00","B:  200.00","C:  300.00"}

   oCol    := oBrw:aCols[oBrw:nColSel]
   nRow    := ( oBrw:nRowSel * oBrw:nRowHeight ) + oBrw:HeaderHeight() - 1
   nCol    := oCol:nDisplayCol - 2
   nWidth  := oCol:nWidth + 3
   nHeight := len(aEditListTxt) * ( FontHeight( oBrw, oBrw:oFont ) ) + 2

   If nRow + nHeight > oBrw:BrwHeight()
      If (oBrw:BrwHeight() - nRow) < oBrw:nRowHeight
         do while ( nRow -  nHeight - oBrw:nRowHeight + 1 ) < 0
            nHeight -= FontHeight( oBrw, oBrw:oFont )
         enddo
         nRow :=  nRow - nHeight - oBrw:nRowHeight + 1
      else
         nHeight := oBrw:BrwHeight() - nRow
      Endif
   Endif


   @ 0, 0 LISTBOX oEditLbx VAR nAt OF oBrw SIZE 0,0 ITEMS aEditListTxt // COLOR 23233,234324
   oEditLbx:SetColor(oDp:Get_nCltText,oDp:Get_nClrPane)

   oEditLbx:bLButtonUp := {|| oEditLbx:Change()}

   oEditLbx:bKeyDown   := {|k|  iif( k == VK_RETURN .and. nAt > 0, (xValue := aBound[ nAt ],EVAL(oGet:bSetGet,xValue),EVAL(oEditLbx:bLostFocus),oEditLbx:End()), ),;
                                iif( k == VK_RETURN .or. k == VK_ESCAPE, NIL, NIL)}

   oEditLbx:Move(nRow, nCol, nWidth, nHeight, .t. )

return oEditLbx

FUNCTION PUTLISTA(oGet,oBrw)
   LOCAL bGotFocus:=oGet:bGotFocus,oLbx

   oGet:bLostFocus:=NIL
   oLbx:=ShowLista( oBrw, oGet )
   oGet:bGotFocus:=bGotFocus

   oLbx:bLDblClick:={||EVAL(olbx:bLostFocus),oLbx:End()}
   oLbx:bLostFocus:={||oGet:bGotFocus:=bGotFocus,oGet:SetFocus(),oGet:bGotFocus:=NIL,oLbx:End(),;
                       NIL}
   DpFocus(oLbx)

RETURN oLbx
*/




























