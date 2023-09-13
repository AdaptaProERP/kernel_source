//
// CLASS DPBRWPAG
// Ejecución de Browse Paginado
// JN 03/01/2003
//
#INCLUDE "FIVEWIN.CH"
#INCLUDE "XBROWSE.CH"
#INCLUDE "\DWH\LIBS\TSBUTTON.CH"

STATIC cGetValue // Valor Seleccionado por el Dialogo
STATIC lSelect   // Indica si fue seleccionado
// STATIC bInit
MEMVAR oDp
// , nClrPane1  ,   nClrPane2

FUNCTION DPBRWPAG(cFile,oGet,uValue,cField,lForced,cWhere,cTitle,oDb,cOnlyWhere,cJoin)
  LOCAL cWhere_
  LOCAL oBrwPag,oTable,cSql

// ? cFile,oGet,uValue,cField,lForced,cWhere,cTitle,oDb,cOnlyWhere,"DPBRWPAG(),cFile,oGet,uValue,cField,lForced,cWhere,cTitle,oDb,cOnlyWhere",ErrorSys(.T.)

  DEFAULT oDb:=GETDBSERVER()

  oDp:lLbxRun:=.F.

// ? oDb:ClassName(),"DPBRWPAG",oDb:oConnect:cUser

  lSelect:=.F.

  //bInit  :={||.T.}
  // return .T.
  // DEFAULT oGet:=GetControl()

  DEFAULT lForced:=.F.
  DEFAULT cWhere :=""

  // Debe buscar si el campo Registro existe //
  // ? getproce()

  CURSORWAIT()

  oBrwPag:=TDPBRWPAG():New(cFile , cTitle , cWhere , oDb ,cOnlyWhere , cJoin )

  DEFAULT cField:=oBrwPag:cPrimary

  DEFAULT uValue:=IIF(ValType(oGet)="O",EVAL(oGet:bSetGet),NIL)

// ? uValue,oGet:ClassName()

//?   ErrorSys(.T.),GETPROCE()

  IF uValue!=NIL

    cSql:="SELECT "+cField+" FROM "+oBrwPag:cTabla+" WHERE "+cField+GetWhere("=",uValue)+" LIMIT 1"

//? cSql,"cSql",GETPROCE(),cWhere,"<-cWhere"

    oTable:=OpenTable(cSql,.T.,oDb)
    oDp:cSqlBrw:=cSql
    oDp:cWhere :=oTable:cWhere

//    ? CLPCOPY(cSql)

    DPWRITE("TEMP\DPBRWPAG_"+ALLTRIM(oTable:cTable)+".SQL",cSql)

    IF oTable:RecCount()>0 .AND. !lForced
      oTable:End()
      oBrwPag:End()
      RETURN .T.
    ENDIF

  ENDIF

  oBrwPag:oGet:=oGet
  oBrwPag:Run()
  oBrwPag:End()

  uValue:=cGetValue // Puede Pasar por Referencia

 // ? uValue,oGet

  IF ValType(oGet)="O" .AND. ValType(oGet:bSetget)="B" .AND. uValue!=NIL

//     ? uValue,"LISTO"

     EVAL(oGet:bSetGet,uValue)

     oGet:VarPut(uValue,.T.)
     oGet:ForWhen(.T.)
     oGet:Refresh(.T.)
     DpFocus(oGet)
   //   ? "aqui es"
   //  ? EVAL(oGet:bSetGet)
   //   DpFocus(oGet)
   //IF !lForced
       oGet:KeyBoard(13)
     // ENDIF
  ENDIF

//  ? "LISTO",uValue

RETURN lSelect

CLASS TDpBrwPag // FROM TDpClass

   DATA oDb
   DATA oBar

   DATA cSql   INIT ""
   DATA cTabla
   DATA cFileBrw
   DATA cTitle
   DATA cInner  INIT ""
   DATA cInner2 INIT ""
   DATA cInner3 INIT ""
   DATA cInner4 INIT ""
   DATA cInner5 INIT ""
   DATA cSelect
   DATA cOrder
   DATA cGroupBy
   DATA cWhere
   DATA cPrimary
   DATA cDesde
   DATA cHasta
   DATA cSelectRec // Seleccionar Registro
   DATA cWhere_
   DATA cHaving
   DATA cPrgBrw INIT "" // Browse Personalizado 13/09/2023

   DATA aFields
   DATA aHeader
   DATA aCols
   DATA aJoin
   DATA aColColor INIT {}
   DATA aData     INIT {}

   DATA  bLDblClick,;  // Accion que debe Ejecutar el Browse
         bRClicked,;
         bFor          // Localizador de Registro


   DATA bValid INIT {|| .T. }

   DATA nRow,;
        nCol,;
        nWidth,;
        nHeight,;
        nLimit,;
        nPages,;
        nPage

   DATA oDlg
   DATA oBrw
   DATA oTable
   DATA oGet

   DATA lHScroll
   DATA lVScroll
   DATA lEof
   DATA lFullSize INIT .F.
   DATA lTotal    INIT .T. // Incluye Totales


   METHOD New( cFileBrw ,  cTitle , cWhere , oDb ,cOnlyWhere , cJoin)
   METHOD AddCol( cField , cHeader , nWidth , cPict , lTotal, cView,nClrText,cFont)
   METHOD BuildArray()
   METHOD SetJoin(cTable,cTableO,cInner)
   METHOD Select()
   METHOD EditFile()
   METHOD Find()
   METHOD Skip(nStep)
   METHOD Bottom()

   METHOD HandleEvent(nMsg, nWParam, nLParam ) EXTERN ;
                      WndHandleEvent( Self, nMsg, nWParam, nLParam )


   METHOD Run()
   METHOD End()

ENDCLASS

/*
 *  TDPBRWPAG:New()
 */

METHOD New( cFileBrw , cTitle , cWhere ,oDb ,cOnlyWhere , cJoin ) CLASS TDPBRWPAG

    LOCAL cHeader,cField,i,nWidth,cPict,cHelp,uValue,lTotal,cView,nClrText:=0,cFont

//? cFileBrw , cTitle , cWhere ,oDb ,cOnlyWhere,"cFileBrw , cTitle , cWhere ,oDb ,cOnlyWhere"

    ::oDb    :=oDb
    ::nRow   :=10
    ::nCol   :=10
    ::nWidth :=30
    ::nHeight:=20
    ::nLimit :=13
    ::nPage  :=0
    ::nPages :=0

    ::lHScroll := .F.
    ::lVScroll := .F.

    ::aCols   :={}
    ::cTitle  :=cTitle
    ::cWhere  :=""
    ::cPrimary:=""
    ::aJoin   :={} // Lista de Tablas Enlazadas
    ::lEof    :=.F.

    ::bLDblClick:={||nil}  // Accion que debe Ejecutar el Browse
    ::bRClicked :={||nil}

//  cFileBrw  :=IIF("\"$cFileBrw,cFileBrw,oDp:cPathLbx+cFileBrw) //  oDp:cLbxPath
    cFileBrw  :=oDp:cPathLbx+cFileNoPath(cFileBrw)
//? cFileBrw,"cFileBrw"
    ::cFileBrw:=cFileBrw


// ? "AQUI 1",ErrorSys(.T.)

    IF FILE(cFileBrw) .OR. ISDPSTD()

      IF ISDPSTD()
         IniGetLbx(GETFILESTD(cFileBrw),cFileBrw)
      ELSE
         IniGetLbx(MEMOREAD(cFileBrw),cFileBrw)
      ENDIF

// ? "dos" ,MEMOREAD(cFileBrw)

      DEFAULT ::cTitle  :=GetLbx("TITLE"   )

      IF Empty(::cTitle)

      ENDIF

// ? "tres",ErrorSys(.t.),::cTitle
// ViewArray(LBXGETLINES())

      ::cTitle  :=GetFromVar(::cTitle)

// ? ::cTitle,"::cTitle","cuafd"


      ::cTabla  :=GetLbx("TABLE"  )
      ::cSelect :=GetLbx("SELECT" )
      ::cInner  :=GetLbx("INNER"  )
      ::cInner2 :=GetLbx("INNER2" )
      ::cInner3 :=GetLbx("INNER3" )
      ::cInner4 :=GetLbx("INNER4" )
      ::cInner5 :=GetLbx("INNER5" )

      ::cPrgBrw :=GetLbx("PRGBRW") // Programación DpXbase personalizado

      IF !Empty(cJoin)
         ::cInner:=IIF(Empty(::cInner),cJoin,cJoin+" "+::cInner)
      ENDIF

      ::cWhere  :=GetLbx("WHERE"  )
      ::cOrder  :=GetLbx("ORDER BY",.T.) // si esta NIL, devuelve ""
      ::cGroupBy:=GetLbx("GROUP BY",.T.)
      ::nWidth  :=GetLbx("WIDTH"   ,NIL,"N")
      ::cHaving :=GetLbx("HAVING")

/*
      ::lFullSize INIT .F.
   DATA  lFullHight INIT .F.
   DATA  lIniFilter INIT .F.
   DATA  lFullWidht INIT .F.
*/


// ? "uactrotres",ErrorSys(.t.)

      ::nHeight :=GetLbx("HEIGHT"  ,NIL,"N")
      ::nRow    :=GetLbx("TOP"     ,NIL,"N")
      ::nCol    :=GetLbx("LEFT"    ,NIL,"N")
      ::nLimit  :=GetLbx("LIMIT"   ,NIL,"N")
      ::lVScroll:=GetLbx("lVScroll",NIL,"L")
      ::LHScroll:=GetLbx("LHScroll",NIL,"L")
      ::lTotal  :=GetLbx("LTOTAL"  ,.F.,"L")

      cHelp     :=GetLbx("HELP"    ,NIL,"C")

      ::lFullSize:=GetLbx("LFULLSIZE"  ,.F.,"L")  // Abarca todo el Video Disponible

      ::cPrimary:=::cOrder

// ? "dddd"

      IF !Empty(cOnlyWhere)
         ::cWhere:=cOnlyWhere
      ENDIF

      IF !Empty(cHelp)
        oDp:cHelpTopic:=cHelp
      ENDIF

      IF !Empty(::cWhere) .AND. "&"$::cWhere
        ::cWhere:=BuildMacro(::cWhere)
      ENDIF

      // JN 27/07/2019
      IF " JOIN "$cWhere .AND. !Empty(::cWhere)

         cWhere  :=cWhere + " AND "+::cWhere
         ::cWhere:=""

      ENDIF

// ? "aqui 5",::cWhere,"<-::cWhere,",cWhere,"<-cWhere"

      ::cTitle:=BuildMacro(::cTitle )

      IF !EMPTY(cWhere)

         // ::cWhere:=cWhere
         IF !Empty(::cWhere)

            IF " JOIN "$cWhere

              ::cWhere_:=cWhere+ " AND "+::cWhere
              cWhere   :=""

// ? cWhere,"<-cWhere_",cWhere,"<-cWhere",::cWhere,"<-::cWhere","DEBE CAMBIAR WHERE POR AND, DEFINITIVO "

            ELSE

              ::cWhere_:=::cWhere+IIF( EMPTY(cWhere) , "" , " AND " )+cWhere

            ENDIF

// ? "AQUI ES ",::cWhere_," cWhere",cWhere," AQUI NACE EL PROBLEMA",cWhere,"cWhere"

         ELSE

            ::cWhere_:=cWhere

         ENDIF

      ELSE

         ::cWhere_:=::cWhere // Utilizado para el Query, evita incluir los where de tablas vinculadas, evitar incidencia campo no existe cuando regresa a TDOCENC

      ENDIF

// ? "aqui 6"

      FOR I := 1 TO 30 //Len(oBrw:aCols)


        cHeader :=GetLbx("COL"+STRZERO(I,2)+"_HEADER")
        cField  :=GetLbx("COL"+STRZERO(I,2)+"_FIELD")
        nWidth  :=GetLbx("COL"+STRZERO(I,2)+"_WIDTH")
        cPict   :=GetLbx("COL"+STRZERO(I,2)+"_PICTURE")
        lTotal  :=GetLbx("COL"+STRZERO(I,2)+"_TOTAL",.F.,"L")
        cView   :=GetLbx("COL"+STRZERO(I,2)+"_VIEW")
        nClrText:=GetLbx("COL"+STRZERO(I,2)+"_CLRTEXT",NIL,"N")
        cFont   :=GetLbx("COL"+STRZERO(I,2)+"_FONT"   ,NIL,"C")

        DEFAULT lTotal:=.F.

        IF ValType(cHeader)="C" .AND. "&oDp:"$cHeader
           cHeader:=MACROEJE(cHeader)
        ENDIF

        // ? cHeader,cField,nWidth
        IF !Empty(cHeader)
          cHeader:=GetFromVar(cHeader)
        ENDIF

        IF !EMPTY(cHeader) .AND. !EMPTY(cField)
          ::AddCol(cField , cHeader , nWidth , cPict,lTotal,cView,nClrText,cFont)
        ENDIF
      NEXT

      DEFAULT ::cWhere_:=""

      FOR I := 1 TO 10 //JOIN

        cJoin:=GetLbx("JOIN"+STRZERO(I,2))

        // Si JOIN, ESTA EN EL WHERE DEBE ESCLUIRLO

        IF !EMPTY(cJoin) .AND. !(cJoin$::cWhere_)

// ? cJoin,I,cWhere,"cWhere",::cWhere,"<-::cWhere",::cWhere_,"<-::cWhere_"

          ::SetJoin(cJoin,NIL)

        ENDIF

      NEXT

      FOR I := 1 TO 10 //JOIN

        cJoin:=GetLbx("LEFTJOIN"+STRZERO(I,2))

        IF !EMPTY(cJoin) .AND. !(cJoin$::cWhere_)
          ::SetJoin(cJoin,NIL,"LEFT")
        ENDIF

      NEXT


    ELSE

      ? cFileBrw,MI("no Existe",1005)

    ENDIF

// ? ::oDb:ClassName(),"DPBRWPAG",::oDb:oConnect:cUser,"AQUI EN DPBRWPAG"
//    ? ::cTitle,cTitle

RETURN SELF

/*
// Select, Seleccionar Registro
*/
METHOD Select() CLASS TDPBRWPAG
   LOCAL nAt,uValue,aFields:={},I,cWhere:=::cWhere

   aFields:=_VECTOR(::cPrimary)

   //? ::cPrimary

   FOR I := 1 TO LEN(aFields)
      nAt:=ASCAN(::aCols,{|a|a:cField=aFields[I]})
      IF nAt=0
         MensajeErr(aFields[I]+MI(" No Encontrado en Columna",1006))
         LOOP
      ENDIF

      IF I=1
      //  ? nAt, ::cPrimary
        uValue:=::oBrw:aArrayData[::oBrw:nArrayAt,nAt]
        cWhere:=::cWhere+IIF(Empty(::cWhere),"", " AND " )+aFields[I]+GetWhere("=",uValue)
      ELSE
        uValue:=CTOO(uValue,"C")+CTOO(::oBrw:aArrayData[::oBrw:nArrayAt,nAt],"C")
        cWhere:=cWhere+ " AND "+aFields[I]+GetWhere("=",::oBrw:aArrayData[::oBrw:nArrayAt,nAt])
      ENDIF
   NEXT

  oDp:cWhere:=cWhere
  oDp:cWhereBrw:=cWhere

/*
   nAt:=ASCAN(::aCols,{|a,i|a:cField=::cPrimary})
   //? nAt,oBrw:nArrayAt
   uValue:=::oBrw:aArrayData[::oBrw:nArrayAt,nAt]
*/

   cGetValue:=uValue

   oDp:cBrwPagGet:=uValue

   lSelect:=.F.

   ::oTable:End()
   ::oDlg:End()
   ::End()

RETURN .T.



/*
// Run, Ejecuta el Editor
*/
METHOD Run() CLASS TDPBRWPAG
    LOCAL oDlg,I,oBrw,oFont,aArray:={},oBrwPag:=SELF,cJoin:="",oDpBrw:=SELF,cView,oCol,nAt
    LOCAL aData:={}
    LOCAL nStyle      :=GetLbx("STYLE"       ,NIL,"N")
    LOCAL nClrPane1   :=GetLbx("COLORPANE1"  ,NIL,"N")
    LOCAL nClrPane2   :=GetLbx("COLORPANE2"  ,NIL,"N")
    LOCAL nClrText    :=GetLbx("COLORTEXT"   ,NIL,"N")
    LOCAL nFreeze     :=GetLbx("FREEZE"      ,NIL,"N")
    LOCAL nHeaderLines:=GetLbx("HEADERLINES" ,NIL,"N")
    LOCAL aTotal      :={}
    LOCAL nClrCol     :=0
    LOCAL lTotal      :=.F.

    LOCAL bInit    :={||.T.}

/*
    ::cSelect:=""

    FOR I := 1 TO LEN(::aCols)
      ::cSelect:=::cSelect+ IIF( Empty(::cSelect), "" , "," ) + ::aCols[I]:cField
    NEXT I
{*/
    if empty(::cOrder) .AND. !empty(::aCols)
      ::cOrder:=::aCols[1]:cField
    Endif

    ::cSelect:=IIF( EMPTY(::cSelect) , "*" , ::cSelect )

    ::cSelect:="SELECT "+::cSelect+" FROM "+::cTabla

    FOR I := 1 TO LEN(::aJoin)
      cJoin:=cJoin + IIF( Empty(cJoin), "" , CRLF ) + ::aJoin[I]
    NEXT I

    ::cSelect:=::cSelect+" "+cJoin

// ? ::cSelect

    aArray:=::BuildArray()

// ? "aqui es array"
// ViewArray(aArray)
   SysRefresh(.T.)

 //? len(aArray)
 //? len(aArray[1])

    IF EMPTY(aArray)

       WHILE LEN(aArray)<2
         aData:={}
         FOR I := 1 TO LEN(::aCols)
           AADD(aData,::aCols[I]:cHeader)
         NEXT I
         AADD(aArray,aData)
       ENDDO

    ENDIF

//aTotal:=RunMacro("ATOTALES(uPar)",a
// ? ErrorSys(.T.),"VA PARA ATOTALES"

    aTotal:=RUNMACRO("ATOTALES(oPar1)",aArray)
    ::aData:=aArray // copiamos el arreglo

// ViewArray(aTotal)
// ViewArray(::aCols)
// ? ::cTitle,"::cTitle"
// GenBlock
// ? ::lTotal,"::lTotal"

// ? "aqui empieza el formulario"
   IF !Empty(::cPrgBrw)
      EJECUTAR(::cPrgBrw,Self)
      RETURN ::oDlg
   ENDIF

    DEFINE DIALOG oDlg TITLE GetFromVar(::cTitle);
           FROM ::nRow,::nCol TO ::nRow+::nHeight,::nCol+::nWidth

    ::oDlg:=oDlg
    ::oDlg:lHelpIcon:=.F.

    oBrw := TXBrowse():New( oDlg )

    IF nStyle=1
      oBrw:nColDividerStyle    := LINESTYLE_BLACK
      oBrw:nRowDividerStyle    := LINESTYLE_BLACK
      oBrw:lColDividerComplete := .t.
      oBrw:nMarqueeStyle       := MARQSTYLE_HIGHLROW
    ENDIF

//    ? nClrPane1,nClrPane2
//    ? ::cFileBrw

    IF !EMPTY(nClrPane1) .AND. !EMPTY(nClrPane2)
      bInit:={||oBrw:SetColor(nClrText,nClrPane1)}
      oBrw:bClrStd := {|| {nClrText, iif( oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }
    ELSEIF !EMPTY(nClrPane1)
      oBrw:bClrStd := {|| {nClrText, nClrPane1} }
      bInit:={||oBrw:SetColor(nClrText,nClrPane1)}
    ENDIF

//Errorsys(.t.)

    IIF( !Empty(nFreeze),oBrw:nFreeze:= nFreeze,NIL)

    oBrw:SetFont(oFont)

    oBrw:lHScroll        := ::lHScroll
    oBrw:lVScroll        := ::lVScroll
    oBrw:nRowSel         := 1
    oBrw:lRecordSelector := .F.
    oBrw:nHeaderLines    := MAX(nHeaderLines,1)
    oBrw:lFooter         :=.T.  //::lTotal
    oBrw:nFooterLines    := 1

/*
   oA26:oBrw:lFooter     := .T.
   oA26:oBrw:lHScroll    := .F.
   oA26:oBrw:nHeaderLines:= 2
   oA26:oBrw:nDataLines  := 1
   oA26:oBrw:nFooterLines:= 1
*/


    oBrw:SetArray(aArray,.F.) // JN

    //oBrw:lFooter         :=.T.  //::lTotal
    //oBrw:nFooterLines    := 1

// ViewArray(::aCols)

    FOR I := 1 TO LEN(::aCols)

      cView  :=::aCols[I]:cView
      oCol   :=oBrw:aCols[I]
      nClrCol:=::aCols[I]:nClrText

      IF !Empty(::aCols[I]:cHeader) .AND. ValType(::aCols[I]:cHeader)="C"
// .AND. (";"$::aCols[I]:cHeader)
 // ? ::aCols[I]:cHeader ,i , ";"$
        ::aCols[I]:cHeader:=ALLTRIM(::aCols[I]:cHeader)
        oBrw:aCols[I]:cHeader:=STRTRAN(::aCols[I]:cHeader,";",CRLF)
      ELSE
        ::aCols[I]:cHeader:=ALLTRIM(::aCols[I]:cHeader)
        oBrw:aCols[I]:cHeader:=::aCols[I]:cHeader
      ENDIF

      IF ValType(::aCols[I]:nWidth)="N"
        oBrw:aCols[I]:nWidth:=::aCols[I]:nWidth
      ENDIF

      IF LEN(aArray)>0 .AND. ValType(aArray[1,I])="N"
         oBrw:aCols[I]:nDataStrAlign:= AL_RIGHT
         oBrw:aCols[I]:nHeadStrAlign:= AL_RIGHT
      ENDIF

      IF !Empty(::aCols[I]:cPicture)
         // ::aCols[I]:oBrw:=oBrw
         BrwSetPicture(oBrw,::aCols[I]:cPicture , I)

         // oBrw:aCols[I]:bStrData     :={||oBrw:=oFrmDat:oBrw,TRAN(oBrw:aArrayData[oBrw:nArrayAt,4],"9,999,999,999.99")}
      ENDIF


// IF ValType(aArray[1,I])="L"
//  ? "LOGICO",aArray[1,I],cView,oCol:ClassName()
// ENDIF

// ? cView,"cView"

      IF !Empty(aArray) .AND. ValType(aArray[1,I])="L" .AND. cView="2"

//.AND. ValType(cView)="C" .AND. cView="2" .AND. ValType(aArray[1,I])="L"

// ? "AQUI AGREGA LOGICO 2 "

          oCol:AddBmpFile("BITMAPS\xCheckOn.bmp")
          oCol:AddBmpFile("BITMAPS\xCheckOff.bmp")
          oCol:bBmpData:=GetFromLogic( oBrw, I )
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    := {||""} // GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL))

      ENDIF

//    IF ValType(cView)="C" .AND. cView="3" .AND. ValType(aArray[1,I])="L" // Visualiza Logicos con Bitmaps
      IF !Empty(aArray) .AND. ValType(aArray[1,I])="L" .AND. cView="3"


// ? "AQUI AGREGA LOGICO 3"

          oCol:AddBmpFile("BITMAPS\ledverde.bmp")
          oCol:AddBmpFile("BITMAPS\ledrojo.bmp")
          oCol:bBmpData:=GetFromLogic( oBrw, I )
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    := {||""} // GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL))
      ENDIF


      IF !Empty(aArray) .AND. ValType(aArray[1,I])="L" .AND. cView="4"
          oCol:AddBmpFile("BITMAPS\checkverde.bmp")
          oCol:AddBmpFile("BITMAPS\checkrojo.bmp")
          oCol:bBmpData:=GetFromLogic( oBrw, I )
          oCol:nDataStyle  := oCol:DefStyle( AL_LEFT, .F.)
          oCol:bStrData    := {||""} // GenCursorBlock(::oCursor , nFor , IIF(nFor=1,{||IIF(oBrw:nArrayAt>0,::oCursor:GotoId(oBrw:nArrayAt),NIL)},NIL))
      ENDIF



      IF ::aCols[I]:lTotal .AND. !Empty(::aCols[I]:cPicture)
         oBrw:lFooter               :=.T.
         oBrw:aCols[I]:cFooter      :=FDP(aTotal[I],"9"+::aCols[I]:cPicture)
         oBrw:aCols[I]:nFootStrAlign:= AL_RIGHT
         lTotal:=.T.
      ENDIF

      oBrw:aCols[I]:bLClickHeader := {|r,c,f,o| SortArray( o, oBrw:aArrayData ) }


      // Color de la Columna
      IF nClrCol<>0
         oBrw:aCols[I]:bClrStd:=BRWCOLSETCOLOR(oBrw,nClrCol,nClrPane1,nClrPane2)
      ENDIF

      // Color Definible de la Columna, segun campo Estado

      nAt:=ASCAN(::aColColor,{|a,n| a[1]=I})

      IF nAt>0
//      ViewArray(::aColColor[nAt])


//                 OJO, COLOCAR EL FONT NEGRITA

        oBrw:aCols[I]:bClrStd:=BRWCOLSETCOLORDEF(oBrw,::aColColor[nAt,1],::aColColor[nAt,2],::aColColor[nAt,3],nClrPane1,nClrPane2)

      ENDIF


      IF !Empty(::aCols[I]:cFont)
         oBrw:aCols[I]:oDataFont:=oFont(::aCols[I]:cFont)
      ENDIF

    NEXT

    IF lTotal

       oCol:=oBrw:aCols[1]
       oCol:cFooter      :=ALLTRIM(FDP(LEN(oBrw:aArrayData),"9,999,999"))
       oCol:nFootStrAlign:=AL_RIGHT
    ENDIF


//  ::bLDblClick:={||oBrwPag:Select()}
//  oBrw:bRClicked :={||eval(::bRClicked )}

    oBrw:bLDblClick:={||oBrwPag:Select()} // eval(::bLDblClick)}
    oBrw:bKeyDown := {| nKey | if( nKey == 13  ,oBrwPag:Select() , NIL )}

    oBrw:CreateFromCode()

    ::oBrw:=oBrw

    ::oBrw:SetColor(0,15790320)
    // ::oBrw:bClrHeader:= {|| {0,14671839 }}
    // ::oBrw:bClrFooter:= {|| {0,14671839 }}


    ::oBrw:bClrFooter          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
    ::oBrw:bClrHeader          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}

    // Recupera los parametros
    EJECUTAR("BRWPAGRESTOREPAR",oBrwPag)

    ::bValid   :={|| EJECUTAR("BRWPAGSAVEPAR",SELF)}

    ACTIVATE DIALOG oDlg ON INIT (EJECUTAR("DPBRWPAGINI",oDpBrw,oDlg,oBrw),;
                                  DPBRWSETBAR(oDlg,oBrw,oDpBrw),;
                                  ChangeSysMenu(oBrwPag),;
                                  Eval(bInit),.F.);
                            VALID (oBrw:End(),oDpBrw:oTable:End(),EVAL(oBrwPag:bValid) ,.T.)

RETURN ::oDlg

/*
// Hacer Arreglo
*/
METHOD BuildArray(lForWard,lPut,lBottom) CLASS TDPBRWPAG
   LOCAL cSql,cWhere:="",I,U,nField,uValue,aData:={},aLines:={}
   LOCAL aArray:={}
   LOCAL aOpc  :={} // Campos con las Opciones

   DEFAULT  ::cOrder:=::cPrimary
   DEFAULT  lForWard:=.T.
   DEFAULT  lPut    :=.F.
   DEFAULT  lBottom :=.F.

   IF " JOIN "$::cWhere .AND. " WHERE "$::cWhere

//    ? "AQUI SE TIENE QUE CAMBIAR JOIN ",::cWhere

   ENDIF

   IF " JOIN "$::cWhere_
//? "EL SCOPE",::cWhere_,"<-"
   ENDIF

   IF lForWard.AND.!EMPTY(::cHasta)
      cWhere:=" WHERE "+::cOrder+GetWhere(">",::cHasta)
   ENDIF

   IF !lForWard.AND.!EMPTY(::cDesde)
      cWhere:=" WHERE "+::cOrder+GetWhere("<",::cDesde)
   ENDIF

   IF !Empty(::cWhere_)

      IF " JOIN "$::cWhere_

// ? "EL SCOPE",::cWhere_,"<-",cWhere

        cWhere:=::cWhere_+IF(Empty(cWhere),""," AND ")+cWhere

      ELSE

        cWhere:=cWhere+IIF( EMPTY(cWhere) , "" , " AND " )+;
                IIF( "WHERE "$cWhere, "" ," WHERE " )+::cWhere_

      ENDIF

// ? "AQUI LE SUMA EN ::cWhere_",::cWhere_

   ENDIF

// ? CLPCOPY(cWhere),"WHERE DPBRWPAG"

   cWhere:=KillWhere(cWhere)
   cWhere:=IIF( EMPTY(cWhere), "", " WHERE " )+cWhere

// ? cWhere,"DEPUES DE KILLWHERE"

   IF !Empty(::cInner) .AND. Left(::cInner,1)='"'
     ::cInner:=MacroEje(::cInner)
   ENDIF

   IF !Empty(::cInner2) .AND. Left(::cInner2,1)='"'
     ::cInner2:=MacroEje(::cInner2)
   ENDIF

   DEFAULT ::cInner :="",;
           ::cInner2:="",;
           ::cInner3:="",;
           ::cInner4:="",;
           ::cInner5:=""

//   ? ::cInner,::cInner2,ValType( ::cInner),ValType(::cInner2)

   ::cInner       :=::cInner+" "+::cInner2+" "+::cInner3+" "+::cInner4+" "+::cInner5

   IF !Empty(::cInner)

// ? "AQUI CONTACTENA EL WHERE ",cWhere,::cInner

      cWhere:=::cInner+" "+cWhere

      // ? cWhere,"WHERE"
   ENDIF

//? cWhere,"cWhere AQUI EN DPBRWPAG",::cInner,"<-cInner",GETPROCE()

   cWhere:=ALLTRIM(cWhere)

   IF " JOIN "$cWhere .AND. LEFT(cWhere,5)="WHERE"
      cWhere:=" "+SUBS(cWhere,6,LEN(cWhere))
   ENDIF

   cSql:=::cSelect +" "+ cWhere +" "+;
         IIF(EMPTY(::cGroupBy),""," GROUP BY "+::cGroupBy+" ")+;
         IIF(EMPTY(::cHaving ),""," HAVING   "+::cHaving +" ")+;
         " ORDER BY " + ::cOrder +;
          IIF( !EMPTY(::nLimit ) , ;
          " LIMIT " + STR(::nLimit,3,0),;
          "")

//? CLPCOPY(cSql),"SQL DEFINITIVO",GETPROCE()
// aArray:=ACLONE(::oTable:aDataFill)
// ? CLPCOPY(cSql),"BUILDARRAY"

   IF .F. .AND. !ChkSql(cSql)
      MemoWrit("TEMP\BRWPAG_"+::cTabla+"_Error.SQL",cSql)
      ? CLPCOPY(cSql),::cFileBrw
   ENDIF
   //    Err   orsys(.t.)

   IIF( ValType(::oTable)="O" , ::oTable:End() , NIL )

   IF Empty(::nPages) .AND. ::nLimit>0 // Debe Contar Cuantas Paginas son
      ::oTable:=OpenTable("SELECT COUNT(*) FROM "+::cTabla+cWhere ,.t.,::oDb)
      ::nPages:=DIV(::oTable:FieldGet(1),::nLimit)
      ::nPages:=IIF( ::nPages!=int(::nPages) , INT(::nPages)+1 , ::nPages )
      ::nPage :=0
      ::oTable:End()
   ENDIF

// ? CLPCOPY(cSql),"ANTES DEL BROWSE"

   ::oTable:=OpenTable(cSql,.t.,::oDb)

   DPWRITE("TEMP\"+cFileNoExt(cFileNoPath(::cFileBrw))+".SQL",cSql)

// ? CLPCOPY(cSql)

   IF Empty(::oTable:aDataFill)

//? CLPCOPY(cSql)

       aLines:={}
       aData :={}
       AEVAL(::oTable:aFields,{|a,n| AADD(aLines,CTOEMPTY(::oTable:FieldGet(n))) })
       ::oTable:aDataFill:=ACLONE(aData)

   ENDIF

   oDp:cSqlBrw:=cSql
   // ? CLPCOPY(cSql)

   // ::oTable:Browse()
   // ? cSql,::oTable:RecCount()

   IF ::oTable:RecCount()<::nLimit // Es EOF
      ::lEof:=.T.
   ENDIF

   IF ::oTable:RecCount()>0 .AND. ::nLimit>0

     ::nPage:=::nPage+ IIF( lForWard , 1 , -1 )

     // ? ::oDlg:ClassName()
     // ? ::oDlg:cTitle
     // ::oDlg:cTitle:=STR(::nPages)+" / "+STR(::nPage)

     ::oTable:GoTop()
     ::cDesde:=::oTable:FieldGet(::cOrder)

     ::oTable:GoBottom()
     ::cHasta:=::oTable:FieldGet(::cOrder)

     aArray:=ACLONE(::oTable:aDataFill)

     //? "Repinto",::cDesde,::cHasta,cSql

   ELSE

     ::lEof:=.T.

   ENDIF

   IF ::nLimit=0
     aArray:=ACLONE(::oTable:aDataFill)
   ENDIF

   FOR I=1 TO LEN(::oTable:aFields)

     IF !Empty(GetOptions(::oTable:cTable,::oTable:aFields[I,1])) // Campos Con Opciones

        AADD(aOpc,{I,::oTable:aFields[I,1]})
        AADD(::aColColor,{I,::oTable:cTable,::oTable:aFields[I,1]})

     ENDIF


   NEXT I

   ::oTable:End()

   /*
   //Agrega las Opciones por Campos
   */
   FOR I=1 TO LEN(aArray)

      FOR U=1 TO LEN(aOpc)
         nField:=aOpc[U,1]
         uValue:=aArray[I,nField]
         aArray[I,nField]:=SAYOPTIONS(::oTable:cTable,aOpc[U,2],uValue,nField)
      NEXT U

   NEXT I

   //aValues:=GetOptions(cTable,cField) // Campos Con Opciones
   // ? ::cDesde,::cHasta

   IF lPut .AND. !Empty(aArray)
     ::oBrw:aArrayData(aArray)
     ::oBrw:Refresh(.T.)
     IF lBottom
       ::oBrw:GoBottom()
     ENDIF
     ::oBrw:DrawLine()
   ENDIF

   IF ValType(::oDlg)="O"
      // ::oDlg:cTitle:=STR(::nPages)+" / "+STR(::nPage)
   ENDIF

 // ? "fin de aArray"
 // ViewArray(aArray)

   ::cSql:=oDp:cSql


RETURN aArray

METHOD EditFile() CLASS TDPBRWPAG
   LOCAL oDlg,oMemo,cMemo:="",lSalir:=.F.,lGrabar:=.F.
   LOCAL oFont

   DEFINE FONT oFont NAME "Courier New" BOLD SIZE 0,-14

   IF !File(::cFileBrw) // Lee Archivo por Defecto
      cMemo:=MemoRead(oDp:cPathLbx+"DEFAULT.BRD")
   ELSE
      cMemo:=MemoRead(::cFileBrw)
   ENDIF

   DEFINE DIALOG oDlg TITLE MI("Editar : ",1007)+::cFileBrw;
          FROM 0,0 TO 30+4,50+10+5+60

   @ 0,0 GET oMemo VAR cMemo OF oDlg MEMO HSCROLL FONT oFont

   @ 2.5+11.2,07 BUTTON MI(" Grabar   ",1008) OF oDlg;
             ACTION (lGrabar:=.T.,MemoWrit(::cFileBrw,cMemo)) SIZE 27,14

   @ 2.5+11.2,17 BUTTON MI(" Cancelar ",1009) OF oDlg;
             ACTION (lSalir:=.T.,oDlg:End())   SIZE 27,14 CANCEL

   @ 2.5+11.2,27 BUTTON MI(" Salir    ",1010) OF oDlg;
             ACTION (IIF( lGrabar,MemoWrit(::cFileBrw,cMemo),NIL),lSalir:=.T.,oDlg:End()) SIZE 27,14

   ACTIVATE DIALOG oDlg CENTERED;
            ON INIT oMemo:Move(0,0,oDlg:nWidth-15,oDlg:nHeight-80,.T.);
            VALID lSalir

   IF lGrabar
     // ::Rebuild(.T.)
   ENDIF

RETURN NIL

/*
// Buscar Registros
*/
METHOD Find(lForWard,lPut,lBottom) CLASS TDPBRWPAG
   ? MI("BUSCAR",1011)
RETURN .T.

/*
// Dezplaza linea por Linea
*/
METHOD Skip(nStep) CLASS TDPBRWPAG

   IF nStep>0

     // ::oBrw:nRowSel++
     IF ::oBrw:nRowSel=::oBrw:nLen .AND. ::nLimit>0
        ::oTable:GoTop()
        ::cHasta:=::oTable:GetValKey(::cOrder)
        ::BuildArray(.T.,.T.,.T.)
     ELSE
        ::oBrw:GoDown()
     ENDIF

   ELSE

     IF ::oBrw:nRowSel=1 .AND. ::nLimit>0
        ::cHasta:=SqlGetMax(::cTabla,::cOrder,::cOrder+GetWhere("<",::cHasta),::nLimit)
        ::BuildArray(.T.,.T.,.F.)
     ELSE
        ::oBrw:GoUp()
     ENDIF

     // ::oBrw:nRowSel--
   ENDIF

   // ? ::oBrw:nRowSel,::oBrw:nDataLines,::oBrw:nLen,::oBrw:RowCount()

   IF ::oBrw:nRowSel<1
     // Deber Refrescar la Siguiente Pagina
   ENDIF

   ::oBrw:DrawLine(.T.)

RETURN .T.

/*
// Enlaza con Otra Tabla
*/
METHOD SetJoin(cTabla,cTablaO,cInner) CLASS TDPBRWPAG
   LOCAL aLink,I,nAt,cLink:="",aFields1,aFields2
   LOCAL cJoin

   DEFAULT cTablaO:=::cTabla,cInner:=" INNER "

   cJoin:=ALLTRIM(cTablaO)+"."+ALLTRIM(cTabla)

   aLink:=GetLinks(::cTabla)
   nAt  :=ASCAN(aLink,{|a,n| ALLTRIM(a[1])==cJoin})

   IF nAt>0

     aFields1:=_VECTOR(aLink[nAt,2])
     aFields2:=_VECTOR(aLink[nAt,3])

     FOR I := 1 TO LEN(aFields1)

        cLink:=cLink+ IIF( Empty(cLink) , "" , " AND " )+;
               cTablaO+"."+aFields1[I]+"="+cTabla+"."+aFields2[I]


     NEXT

     cJoin:=" "+cInner+" JOIN "+cTabla+" ON "+cLink
/*
     cJoin:=" INNER JOIN "+cTabla+" ON "+BuildConcat(aLink[nAt,2])+"="+BuildConcat(+aLink[nAt,3])
*/
//     ? cJoin,"cJoin"

     AADD(::aJoin,cJoin)

   ENDIF

RETURN nAt>0

/*
Agrega Columnas
*/
METHOD AddCol(cField,cHeader,nWidth,cPict,lTotal,cView,nClrText,cFont) CLASS TDPBRWPAG
   LOCAL oCol

   IF Empty(nWidth)
      nWidth:=NIL
   ELSE
      nWidth:=CTOO(nWidth,"N")
   ENDIF

   oCol:=TBRWCOL():New(cField,cHeader,nWidth,cPict,lTotal,cView,nClrText,cFont)

   AADD(::aCols,oCol)

RETURN oCol

/*
// Final del Archivo
*/
METHOD Bottom() CLASS TDPBRWPAG

  LOCAL oDpBrw:=SELF

  IF oDpBrw:nLimit>0 // Paginado

     oDpBrw:cHasta:=SqlGetMax(oDpBrw:cTabla,oDpBrw:cOrder,NIL,oDpBrw:nLimit)
     oDpBrw:cDesde:=""
     oDpBrw:nPage :=oDpBrw:nPages+1
     oDpBrw:BuildArray(.T.,.T.,.T.)

  ELSE

     ::oBrw:GoBottom()

  ENDIF

RETURN .T.

/*
// Culminar la Clase
*/
METHOD End() CLASS TDPBRWPAG

   IF ValType(::oTable)="O"
     ::oTable:End()
   ENDIF

RETURN NIL

/*
// Clase BrwCol
*/

CLASS TBRWCOL

   DATA cField
   DATA cHeader
   DATA cPicture
   DATA nWidth
   DATA lTotal INIT .F.
   DATA cView  INIT ""
   DATA nClrText INIT 0
   DATA cFont    INIT ""

   METHOD New( cField , cHeader , nWidth , cPict ,lTotal,cView,nClrText,cFont)
   METHOD End()

ENDCLASS

/*
 *  TDPBRWPAG:New()
 */

METHOD New( cField,cHeader, nWidth , cPict ,lTotal,cView,nClrText,cFont) CLASS TBRWCOL

    DEFAULT lTotal:=.F.

    ::cField  :=cField
    ::cHeader :=cHeader
    ::nWidth  :=nWidth
    ::cPicture:=cPict
    ::lTotal  :=lTotal
    ::cView   :=cView
    ::nClrText:=nClrText
    ::cFont   :=cFont

RETURN SELF

METHOD End() CLASS TBRWCOL
RETURN NIL
/*
//  Barra de Botones
*/
STATIC FUNCTION DPBRWSetBar(oDlg,oBrw,oDpBrw)

   LOCAL oBar, oCursor , I,oBtn ,oFont,oBtnx

   DEFINE FONT oFont NAME "Tahoma" SIZE 0,-8 BOLD

   DEFINE CURSOR oCursor HAND

   DEFINE BUTTONBAR oBar SIZE 26+40-18, 26+35-18 3D OF oDlg

/*
     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XFIND.BMP"; //  SHAPE; // CAPTION cMsg;
            TOOLTIP MI("BUSCAR",1011);         //  PROMPT MI("BUSCAR",1011);             TEXT ON_BOTTOM;
            FONT oFont;
            ACTION EJECUTAR("BRWSETFIND",oBrw);
            OF oBar


     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\FILTRAR.BMP",NIL,"BITMAPS\FILTRARG.BMP"";
            TOOLTIP MI("BUSCAR",1011);
            FONT oFont;
            ACTION EJECUTAR("BRWSETFILTER",oBrw);
            WHEN LEN(oBrw:aArrayData)>1;
            OF oBar

     oBtn:cToolTip:=MI("Filtrar Registros",1012)

     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\OPTIONS.BMP",NIL,"BITMAPS\OPTIONSG.BMP";
            TOOLTIP MI("BUSCAR",1011);
            FONT oFont;
            ACTION EJECUTAR("BRWSETOPTIONS",oBrw);
            WHEN LEN(oBrw:aArrayData)>1;
            OF oBar

     oBtn:cToolTip:=MI("Filtrar Registros",1012)


     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XFIND.BMP"; //  SHAPE; // CAPTION cMsg;
            TOOLTIP MI("BUSCAR",1011);         //  PROMPT MI("BUSCAR",1011);             TEXT ON_BOTTOM;
            FONT oFont;
            ACTION EJECUTAR("BRWSETFIND",oBrw);
            OF oBar


     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XTOP.BMP"; //          SHAPE; // CAPTION cMsg;
            TOOLTIP "Primero";       //  PROMPT "Primero";  TEXT ON_BOTTOM;
            FONT oFont;
            OF oBar;
            ACTION (oDpBrw:cDesde:="",;
                    oDpBrw:cHasta:="",;
                    oDpBrw:nPage :=0 ,;
                    oDpBrw:BuildArray(.T.,.T.))

     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XSIG.BMP"; //          SHAPE; // CAPTION cMsg;
            TOOLTIP "Siguiente";     //           PROMPT "Siguiente";             TEXT ON_BOTTOM;
            FONT oFont;
            OF oBar;
            ACTION (oDpBrw:Skip(1))

     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XANT.BMP";
            TOOLTIP "Anterior"; //  PROMPT "Anterior";
            FONT oFont; //     TEXT ON_BOTTOM;
            OF oBar;
            ACTION (oDpBrw:Skip(-1))

     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XFIN.BMP"; //          SHAPE; // CAPTION cMsg;
            TOOLTIP "Ultimo";
            FONT oFont;
            OF oBar;
            ACTION oDpBrw:Bottom()

     DEFINE BUTTON oBtn ;
            FILE "BITMAPS\XSALIR.BMP"; //      SHAPE; // CAPTION cMsg;
            TOOLTIP "Salir";          //      PROMPT "Cerrar";         TEXT ON_BOTTOM;
            FONT oFont;
            OF oBar;
            ACTION (oDpBrw:oDlg:End())
*/


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XFIND.BMP";
          ACTION EJECUTAR("BRWSETFIND",oDpBrw:oBrw)

   oBtn:cToolTip:=MI("BUSCAR",1011)


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\FILTRAR.BMP";
          ACTION EJECUTAR("BRWSETFILTER",oDpBrw:oBrw);
          WHEN .T. // LEN(oBrw:aArrayData)>0;


   oBtn:cToolTip:=MI("Filtrar Registros",1012)


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\OPTIONS.BMP";
          ACTION EJECUTAR("BRWSETOPTIONS",oDpBrw:oBrw)

   oBtn:cToolTip:=MI("Filtrar por Campo",1013)


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\EXCEL.BMP";
          ACTION (EJECUTAR("BRWTOEXCEL",oDpBrw:oBrw,oDpBrw:cTitle,""))

   oBtn:cToolTip:=MI("Exportar hacia Excel",1014)

   // oDpBrw:oBtnXls:=oBtn

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\html.BMP";
          ACTION (EJECUTAR("BRWTOHTML",oDpBrw:oBrw,NIL,oDpBrw:cTitle))

   oBtn:cToolTip:=MI("Generar Archivo html",1015)

   //oDpBrw:oBtnHtml:=oBtn

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\PREVIEW.BMP";
          ACTION EJECUTAR("BRWPREVIEW",oDpBrw:oBrw,"oDpBrw:cTitle",.T.)

   oBtn:cToolTip:=MI("Previsualización",1016)


   oDp:oDpBrw:=oDpBrw
   oDp:oDlg  :=oDlg
   oDp:oBrw  :=oBrw

   EJECUTAR("DPBRWADDBTN",oBar,oDlg,oBrw,oDpBrw)

   //oDpBrw:oBtnPreview:=oBtn

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xTOP.BMP";
          ACTION (oDpBrw:oBrw:GoTop(),oDpBrw:oBrw:Setfocus())

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xSIG.BMP";
          ACTION (oDpBrw:oBrw:PageDown(),oDpBrw:oBrw:Setfocus())

  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xANT.BMP";
          ACTION (oDpBrw:oBrw:PageUp(),oDpBrw:oBrw:Setfocus())

  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xFIN.BMP";
          ACTION (oDpBrw:oBrw:GoBottom(),oDpBrw:oBrw:Setfocus())

  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XSALIR.BMP";
          ACTION (oDpBrw:oDlg:End())

   oBar:bRClicked := {|| NIL }

   Aeval(oBar:aControls, {|oCtl| oCtl:oCursor := oCursor})

   oBar:SetColor(CLR_BLACK,oDp:nGris) // 15724527)
   AEVAL(oBar:aControls,{|o,n|o:SetColor(CLR_BLACK,oDp:nGris)})

   oDpBrw:oBar:=oBar

   EJECUTAR("DPBRWPAGBAR",oBar,oDlg,oBrw,oDpBrw)

   oBrw:Move(oBar:nHeight(),2,oDlg:nWidth()-10,oDlg:nHeight()-(93-10),.T.)

RETURN oBar

/*
// Obtiene la Clave, segun la Posicion nLimit, Anterior
*/
// STATIC FUNCTION SqlGetLimit(cTable,cField,uValue,nLimit)
//
// RETURN uValue

STATIC FUNCTION ChangeSysMenu(oBrwPag)

   local oSysMenu,bBlq1,bBlq2,bBlq3

   IF oBrwPag:oDlg=NIL
      RETURN NIL
   ENDIF

   bBlq1:={||oBrwPag:EditFile()}

   bBlq2:=[MsgAlert("LISTO")]
   bBlq2:=BloqueCod(bBlq1)

   bBlq3:=[MsgAlert("LISTO")]
   bBlq3:=BloqueCod(bBlq3)

   REDEFINE SYSMENU oSysMenu OF oBrwPag:oDlg

   SEPARATOR

   MenuAddItem( MI("&Editar Parametros ",1017),, .F.,,bBlq1,,,,,,, .F.,,, .F. )

   MenuAddItem( MI("&Preparar Columnas ",1018),, .F.,,bBlq2,,,,,,, .F.,,, .F. )

   MenuAddItem( MI("&Grabar Tamaño ",1019),, .F.,,bBlq3,,,,,,, .F.,,, .F. )

   // MenuAddItem( MI("&Asignar Grabar Tamaño ",1019),, .F.,,bBlq3,,,,,,, .F.,,, .F. )

   ENDMENU

RETURN NIL

Static Function MenuPop( oBtn )

   Local oMenu, aRect

   aRect := GetClientRect( oBtn:hWnd )

   MENU oMenu POPUP
      MENUITEM "&Undo" ACTION MsgAlert("UNO")
      SEPARATOR
/*      MENUITEM "Cu&t"  RESOURCE "Cortar"
      MENUITEM "&Copy" RESOURCE "Copiar"
      MENUITEM "&Paste" RESOURCE "Pegar"
      MENUITEM "&Delete" RESOURCE "Borrar"
      SEPARATOR */
      MENUITEM "&Select All"
   ENDMENU

   ACTIVATE POPUP oMenu AT aRect[ 3 ] + 1, aRect[ 2 ] OF oBtn

Return Nil

/*
// Coloca el Picture
*/
FUNCTION BrwSetPicture(oBrw,cPict,nCol)
  LOCAL oCol:=oBrw:aCols[nCol]
  oCol:oBrw:=oBrw
  oCol:bStrData :={||TRAN(oCol:oBrw:aArrayData[oCol:oBrw:nArrayAt,nCol],cPict)}
RETURN NIL

 FUNCTION BuildMacro(cExp)

   IF LEFT(cExp,2)=[&"]
      cExp:=SUBS(cExp,2,LEN(cExp))
      cExp:=MACROEJE(cExp)
   ENDIF
RETURN cExp

STATIC FUNCTION GetFromLogic( oBrw, nField )
RETURN {||IIF(oBrw:aArrayData[oBrw:nArrayAt,nField],1,2) }

FUNCTION BRWCOLSETCOLOR(oBrw,nClrText,nClrPane1,nClrPane2)
RETURN {|nClrText| {nClrText, iif( oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }

STATIC FUNCTION BRWCOLSETCOLORDEF(oBrw,nCol,cTable,cField,nClrPane1,nClrPane2)
RETURN {|nClrText| {nClrText:=GETCOLOR_OPC(oBrw,nCol,cTable,cField), iif( oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }

STATIC FUNCTION GETCOLOR_OPC(oBrw,nCol,cTable,cField)
  LOCAL uValue  :=oBrw:aArrayData[oBrw:nArrayAt,nCol]
  LOCAL cSay    :=SAYOPTIONS(cTable,cField,uValue)
RETURN nClrText:=oDp:nClrOptions


// RETURN {|uValue|uValue:= oCursor:FieldGet( nField ) , IIF( uValue , 1 , 2 )}











