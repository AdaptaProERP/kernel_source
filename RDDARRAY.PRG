 /*
 *  TRDDARRAY()
 *  Clase para el manejo de Arreglos como una DBF
 *  Juan navas
 */

 MEMVAR oDp

#INCLUDE "FIVEWIN.CH"
/*
PROCE MAIN()

  LOCAL oData,I

  oData:=TRddArray():New()

  FOR I := 1 TO 5
    oData:Append()
    oData:Replace("CDATA",REPLICATE(CHR(64+I),i))
    oData:Replace("NDATA" ,I)
    oData:Replace("DDATA",DATE()+I)
  NEXT I

  oData:GoTop()

  WHILE !oData:Eof()
    ? oData:Recno(),oData:CDATA,oData:NDATA,oDATA:DDATA
    oData:DbSkip()
  ENDDO

  oData:Goto(1)
  oData:Delete()
  ? oData:RecCount(),"Registros"
  ? oData:LocateFor({|oData|oData:NDATA=4})
  ? oData:NDATA,"LOCALIZADO"
  ? oData:LocateFor({|oData|oData:NDATA=100}),"NO EXISTE"
  ? oData:NDATA,"NO LOCALIZADO"

  oData:Goto(2)
  oData:Replace("NDATA",100)
  ? oData:LocateFor({|oData|oData:NDATA=100}),"DEBE EXISTIR 100"
  oData:Goto(3)
  oData:Replace("NUEVOCAMPO","NUEVO CAMPO")
  ? oData:NUEVOCAMPO

  oData:End()

RETURN
*/
/*
 * tRddArray()
 */
CLASS tRddArray

  DATA aData,aFields,nRecno,oBrw,lEof,cSql,lSend
  DATA cTitle,lMdi,bRun

  DATA oWnd

  METHOD New(aData)
  METHOD Replace(cField,uValue)
  METHOD Append()
  METHOD AddField(cField , lData , uValue)

  METHOD DbSkip(nSkip) INLINE (nSkip   :=IIF( nSkip=nil,1  , nSkip ),;
                               ::nRecno:=::nRecno+nSkip,;
                               ::Goto(::nRecno))

  METHOD Eof()         INLINE ::lEof .OR. Empty(::aData) // ::nRecno>=LEN(::aData)
  METHOD Delete()
  METHOD GoTop()       INLINE ::Goto(1)
  METHOD GoBottom()    INLINE ::Goto(::RecCount())
  METHOD Goto(nRecno)
  METHOD RecCount()    INLINE LEN(::aData)
  METHOD LocateFor()
  METHOD DeleteFor(bFor)
  METHOD SetEmpty()
  METHOD Zap()        INLINE (::aData:={},::nRecno:=0,::lEof:=.T.)
  METHOD FromCursor()
  METHOD FieldGet(nField)
  METHOD SetArray(aData)
  METHOD SetUnique(nCol)
  METHOD FieldPos(cField) INLINE ASCAN(::aFields,cField)
  METHOD Browse(cTitle,lOrder,oFont,lMdi,aHeader,bRun)
  METHOD CopyFrom(oData)
  METHOD Recno()      INLINE ::nRecno
  METHOD IsDef(cField)
  METHOD End() INLINE (IIF(ValType(::oWnd)="O" .AND. ::oWnd:hWnd>0 ,::oWnd:End(),NIL),;
                       ::aData:={})
  METHOD HANDLEEVENT() INLINE NIL

  ERROR HANDLER OnError( cMsg, nError )

ENDCLASS

/*
 *  tRddArray:New()
 */
METHOD New(aData,cTitle,lMdi,bRun) CLASS tRddArray

   ::aData  :={}
   ::lSend  :=.F.
   ::aFields:={}
   ::nRecno :=0
   ::lEof   :=.F.

   DEFAULT oDp:lViewArrayMdi:=.F.,;
           cTitle:=::ClassName(),;
           lMdi  :=oDp:lViewArrayMdi

   ::cTitle:=cTitle
   ::lMdi   :=lMdi
   ::bRun   :=bRun

   IF !Empty(aData)
      ::SetArray(aData,bRun)
   ENDIF

RETURN Self

METHOD IsDef(cField) CLASS tRddArray
    LOCAL lIsDef
    ::lSend:=.T.
    lIsDef:=(Osend(Self,cField)!=NIL)
    ::lSend:=.F.
RETURN lIsDef

METHOD SetArray(aData,bRun) CLASS tRddArray
    LOCAL nCol,I

    ::aData  :={}
    ::aFields:={}

    IF ValType(aData)<>"A" .OR. Empty(aData)
        RETURN .F.
    ENDIF

    IF ValType(aData[1])<>"A" // Debe Convertirlo Multidimensional
       FOR I := 1 TO LEN(aData)
         AADD(::aData,{aData[I]})
       NEXT
    ELSE
       ::aData:=ACLONE(aData)
    ENDIF

    FOR I := 1 TO LEN(::aData[1])
      ::AddField("COL"+STRZERO(I,2),.F.)
    NEXT I

    ::Gotop()

RETURN .T.

/*
// Unificar Codigos
*/
METHOD SetUnique(nCol) CLASS tRddArray
   LOCAL aData:={},I,nAt

   IF Empty(::aData)
      RETURN NIL
   ENDIF

   FOR I := 1 TO LEN(::aData)
      nAt :=ASCAN(aData,{|a,n|a[nCol]=::aData[I,nCol]})
      // ? nAt,::aData[I,nCol]
      IF nAt=0
         AADD(aData,::aData[I])
      ENDIF
   NEXT

   // ? LEN(::aData),Len(aData)

   ::aData:=ACLONE(aData)
   ::Gotop()

RETURN NIL

METHOD Replace(cField,uValue) CLASS tRddArray

     LOCAL nAt:=ASCAN(::aFields, cField)

     // ? nAt,cField

     IF nAt=0

        ::AddField(cField,.T.,uValue)

        nAt:=LEN(::aFields)

     ENDIF

     ::lSend:=.T.

     IF oSend(Self,cField)=NIL
        __objAddData( Self, cField )
     ENDIF

     ::lSend:=.F.

     oSend(Self,cField,uValue)

     IIF( Empty(::aData) ,  ::Append() , NIL)


//     IF nAt=0 .OR. nAt>LEN(::aData[::nRecno])
//         ? "AQUI MAL",nAt
//     ENDIF

     ::aData[::nRecno,nAt]:=uValue

RETURN uValue

/*
// Copia desde Otra Copia
*/
METHOD CopyFrom(oData) CLASS tRddArray

     Aeval(oData:aFields,{|a|::AddField(a,.F.)})

    ::aData  :=ACLONE(oData:aData  )
    ::aFields:=ACLONE(oData:aFields)
    ::lEof   :=Empty(::aData)
    ::Gotop()

RETURN .T.

METHOD FieldGet(nField) CLASS tRddArray

    nField:=IIF( ValType(nField)="C" , ::FieldPos(UPPE(nField)) , nField )

    // ? ::nRecno,nField,::aFields[nField]

RETURN ::aData[::nRecno,nField]
/*
// Agregar
*/
METHOD AddField(cField,lData,uValue) CLASS tRddArray
   LOCAL nAt:=ASCAN(::aFields,cField)

   DEFAULT lData:=.T.

   IF nAt>0
      RETURN NIL
   ENDIF

   AADD(::aFields,cField)

   ::lSend:=.T.

   IF oSend(Self,cField)=NIL
      __objAddData( Self, cField )
   ENDIF

   ::lSend:=.F.

   IF lData
      AEVAL(::aData,{|aLine,n|AADD(aLine,uValue),;
                              ::aData[n]:=aLine})
      // ,::aData[n,Len(::aData[n])]:=uValue})
   ENDIF

RETURN NIL
/*
// DeleteFor
*/
METHOD DeleteFor(bEval) CLASS tRddArray

   IF Empty(::aData)
      RETURN .F.
   ENDIF

   ::GoTop()

   // ? ::RecCount(),"RecCout"

   WHILE ::RecCount()>0 .AND. !::Eof()
      IF Eval(bEval,Self)
         ::Delete()
         LOOP
      ENDIF
      ::DbSkip()
  ENDDO

RETURN NIL

/*
// Localizar
*/
METHOD LocateFor(bEval) CLASS tRddArray
   LOCAL I:=0,lFound:=.F.

   IF Empty(::aData)
      RETURN .F.
   ENDIF

   ::GoTop()
   ::Goto(1)

   IF Eval(bEval,Self)
      // ? "ENCONTRADO A LA PRIMERA"
      RETURN .T.
   ENDIF

   WHILE ++I<=LEN(::aData) .AND. ((::Goto(i),lFound:=Eval(bEval,Self)),!lFound)
      // GOTO(I)
      // AEVAL(::aFields,{|a,n|oSend(Self,a,::aData[I,n])})
   ENDDO

   // ? "ENCONTRADO EN ",I

RETURN ( IIF( lFound , ::Goto(I-1) , ::Goto(0) ) ,lFound )

METHOD Goto(nRecno) CLASS tRddArray

   ::lEof:=.F.

   IF nRecno>Len(::aData)
      ::lEof:=.T.
   ENDIF

   ::nRecno:=MIN(LEN(::aData),nRecno)

   IF ::nRecno>0

      AEVAL(::aFields,{|a,n|oSend(Self,a,::aData[::nRecno,n])})

   ELSE

     ::SetEmpty()

   ENDIF

RETURN NIL

METHOD SetEmpty() CLASS tRddArray

     LOCAL aLine:=Array(Len(::aFields))

     IF !Empty(::aData)

       Aeval(::aFields,{|a,n|aLine[n]:=CtoEmpty(::aData[1,n]),;
                             OSend(Self,a,aLine[n])})

     ELSE

       Aeval(::aFields,{|a,n|OSend(Self,a,a)})

     ENDIF

RETURN aLine

/*
// Borra Lista
*/
METHOD Delete(nRecno) CLASS tRddArray

   DEFAULT nRecno:=::nRecno

   ADEL(::aData,nRecno)
   ::aData:=ASIZE(::aData,Len(::aData)-1)
   ::Goto(::nRecno)

RETURN NIL

/*
// Agrega un Nuevo Elemento
*/
METHOD Append() CLASS tRddArray
   LOCAL aLine:=::SetEmpty()

   AADD(::aData,aLine)

   ::nRecno:=LEN(::aData)

RETURN NIL

/*
// Importa desde un Cursor SQL
*/
METHOD FromCursor(oCursor) CLASS tRddArray
   LOCAL lFrom:=ValType(oCursor)="O"

   IF !lFrom

      IF !ChkSql(oCursor)
         MensajeErr(oCursor+CRLF+"Inválido","FromCursor:tRddArray")
         RETURN NIL
      ENDIF

      oCursor:=OpenTable(oCursor,.T.)
      ::cSql :=oCursor:cSql
      AEVAL( oCursor:aFields , {|a|::AddField(a[1],.f.) } )
   ELSE
      ::cSql:=oCursor
   ENDIF

   ::aFields:={}
   ::aData  :={}
   ::nRecno :=0

   IF !Empty(oCursor:aDataFill)

      //
      AEVAL(oCursor:aFields,{|a,n|::Replace(a[1],oCursor:aDataFill[1,n])})
      ::nRecno:=1
      ::aData  :=ACLONE(oCursor:aDataFill)
      ::Goto(1)

      // ? ::aData[1,1],::aFields[1]

   ENDIF

// ? LEN(oCursor:aFields) , LEN(oCursor:aDataFill[1]) , ::aFields[1] // LEN(::aData[1])
// ? LEN(oCursor:aDataFill),LEN(::aData),"LISTO"

   IF !lFrom
      oCursor:End()
   ENDIF

   ::Gotop()

RETURN oCursor

/*
// Edicion del Browse
*/
#INCLUDE "XBROWSE.CH"

METHOD Browse(cTitle,lOrder,oFont,lMdi,aHeader,bRun) CLASS tRddArray
   LOCAL oBrw,oWnd,oCol,I,oIco,nClrPane1,nClrPane2,oRdd:=SELF
   LOCAL nAt

   DEFAULT cTitle:=::cTitle,;
           lOrder:=.T.,;
           oDp:lViewArrayMdi:=.T.,;
           aHeader:={},;
           bRun   :=::bRun

   DEFAULT lMdi:=oDp:lViewArrayMdi

   IF ValType(aHeader)="C"
      // aHeader:=_VECTOR(aHeader,";",CRLF)
      aHeader:=_VECTOR(aHeader,",")
   ENDIF

   AEVAL(aHeader,{|a,n| aHeader[n]:=STRTRAN(aHeader[n],";",CRLF) })

   DEFAULT ::lMdi:=oDp:lViewArrayMdi

   DEFAULT oDp:nClrPane1:=16773345,;
           oDp:nClrPane2:=16769476

   nClrPane1:=oDp:nClrPane1
   nClrPane2:=oDp:nClrPane2

   IF EMPTY(::aData)
      MensajeErr("Consulta Vacia")
      RETURN .T.
   ENDIF

   IF oFont=NIL
     DEFINE FONT oFont NAME "Tahoma" SIZE 0,-12 // -14
   ENDIF

   IF !lMdi

// oDp:lViewArrayMdi

     DEFINE DIALOG oWnd TITLE "Browse "+cTitle;
     OF DpWndMain()

     ::oWnd:=oWnd

   ELSE

     DEFINE WINDOW oWnd MDICHILD;
            TITLE "Browse "+cTitle ;
            OF DpWndMain()

     DEFINE ICON oIco RESNAME "ICON"
     oWnd:SetIcon(oIco)
     oIco :=NIL

     ::oWnd:=oWnd

   ENDIF

   oBrw := TXBrowse():New( oWnd )
   oBrw:lHScroll:=.t.
   oBrw:lVScroll:=.t.

   nAt:=ASCAN(aHeader,{|c,n| CRLF$c })

 // ? nAt,"nAt"

   IF nAt>0
     oBrw:nHeaderLines:= 2
   ENDIF

   IF Empty(::aData)
     oBrw:SetArray({{"Vacio"}})
   ELSE
     oBrw:SetArray(::aData,lOrder)
   ENDIF

   FOR I:= 1 to Len(oBrw:aCols)

    oBrw:aCols[I]:cHeader  := IIF( I<=Len(::aFields),::aFields[i],NIL)
    oBrw:aCols[I]:oDataFont:=oFont

    IF I<=LEN(aHeader)
       oBrw:aCols[I]:cHeader  := aHeader[I]
    ENDIF

      IF ValType(::aData[1,I])="N"
        oBrw:aCols[I]:nDataStrAlign:= AL_RIGHT
        oBrw:aCols[I]:nHeadStrAlign:= AL_RIGHT
      ENDIF

      IF ValType(::aData[1,I])="C"
//
//        oCol:bStrData:={|oBrw|GetArrayData(oBrw)}
// {|oBrw| GetArrayData(oBrw)}
// LEFT(oBrw:aArrayData[oBrw:nArrayAt,I],250)}
         ENDIF

       oBrw:aCols[I]:bLClickHeader := {|r,c,f,o| SortArray( o, oBrw:aArrayData ) }

   NEXT


   oBrw:bClrStd               := {|nClrText|nClrText:=0,;
                                           {nClrText,iif( oBrw:nArrayAt%2=0, nClrPane1, nClrPane2 ) } }

   oBrw:bClrHeader          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}
   oBrw:bClrFooter          := {|| { oDp:nLbxClrHeaderText, oDp:nLbxClrHeaderPane}}

   oBrw:CreateFromCode()
   oBrw:SetFont(oFont)

   IF !lMdi
// !oDp:lViewArrayMdi

     ACTIVATE DIALOG oWnd ON INIT (oWnd:Move(10,10,600,600),oBrw:Move(0,0,600-15,600-15,.t.))

   ELSE

    oWnd:oClient := oBrw
    oWnd:SetControl( oBrw )

    ACTIVATE WINDOW oWnd ON INIT RDDBOTBAR(oWnd,oBrw,cTitle,oRdd);
                    VALID (oFont:End(),.T.)

   ENDIF


   ::oBrw:=oBrw

RETURN oBrw

/*
 *  OnError()
 */
METHOD OnError( uValue , nError) CLASS tRddArray

  LOCAL cMsg   := UPPE(ALLTRIM(__GetMessage()))

  if SubStr( cMsg, 1, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2)
      __objAddData( Self, cMsg )
      oSend(Self,cMsg,uValue)
      RETURN uValue

  ELSEIF !::lSend

        _ClsSetError( _GenError( nError, ::ClassName(), cMsg ) )

  ENDIF

RETURN NIL

/*
// Convierte un Valor Vacio
STATIC FUNCTION CtoEmpty(uValue,cType,nLen)

   DEFAULT cType:=ValType(uValue)

   IF cType="C" .AND. EMPTY(nLen) .AND. ValType(uValue)="C"
      nLen:=LEN(uValue)
   ENDIF

   DO CASE
      CASE ValType(uValue)="U".AND.cType$"CM"
            uValue:=SPACE(nLen)
      CASE ValType(uValue)="U".AND.cType="N"
            uValue:=0.00
      CASE ValType(uValue)="U".AND.cType="D"
            uValue:=CTOD("")
      CASE ValType(uValue)="U".AND.cType="L"
            uValue:=.F.
      CASE ValType(uValue)="C"
           uValue:=SPACE(nLen) // LEN(uValue))
      CASE ValType(uValue)="N"
           uValue:=0
      CASE ValType(uValue)="D"
           uValue:=CTOD("")
      CASE ValType(uValue)="L"
           uValue:=.F.
   ENDCASE

RETURN uValue
*/

/*
// Barra de Botones
*/
STATIC FUNCTION RDDBOTBAR(oWnd,oBrw,cTitle,oRdd)
   LOCAL oCursor,oBar,oBtn,oFont
   LOCAL nLin:=0

   DEFINE CURSOR oCursor HAND
   DEFINE BUTTONBAR oBar SIZE 52-15,60-15 OF oWNd 3D CURSOR oCursor
   DEFINE FONT oFont  NAME "Arial"   SIZE 0, -10 BOLD


   IF !oRdd:bRun=NIL

      DEFINE BUTTON oBtn;
             OF oBar;
             NOBORDER;
             FONT oFont;
             FILENAME "BITMAPS\RUN.BMP";
             ACTION EJECUTAR("RDDARRAYRUN",oRdd:bRun,oBrw,oRdd)

       oBtn:cToolTip:="Ejecutar"

       oBrw:bLDblClick:=oBtn:bAction // {|oBrw|EJECUTAR("RDDARRAYRUN",oBrw,oRdd)}

   ENDIF

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XFIND.BMP";
          ACTION EJECUTAR("BRWSETFIND",oBrw)

   oBtn:cToolTip:="Buscar"


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\FILTRAR.BMP";
          ACTION EJECUTAR("BRWSETFILTER",oBrw)

   oBtn:cToolTip:="Filtrar Registros"

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\OPTIONS.BMP",NIL,"BITMAPS\OPTIONSG.BMP";
          ACTION EJECUTAR("BRWSETOPTIONS",oBrw);
          WHEN LEN(oBrw:aArrayData)>1

   oBtn:cToolTip:="Filtrar según Valores Comunes"


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\EXCEL.BMP";
          ACTION EJECUTAR("BRWTOEXCEL",oBrw,cTitle,"")

   oBtn:cToolTip:="Exportar hacia Excel"


   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\html.BMP";
          ACTION EJECUTAR("BRWTOHTML",oBrw)

   oBtn:cToolTip:="Generar Archivo html"

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\PREVIEW.BMP";
          ACTION (EJECUTAR("BRWPREVIEW",oBrw))

   oBtn:cToolTip:="Previsualización"

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xTOP.BMP";
          ACTION (oBrw:GoTop(),oBrw:Setfocus())

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xSIG.BMP";
          ACTION (oBrw:PageDown(),oBrw:Setfocus())

  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xANT.BMP";
          ACTION (oBrw:PageUp(),oBrw:Setfocus())


  DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\xFIN.BMP";
          ACTION (oBrw:GoBottom(),oBrw:Setfocus())



   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\XSALIR.BMP";
          ACTION oWnd:End()

  oBrw:SetColor(0,oDp:nClrPane1)

//  EVAL(oBrw:bChange)

  oBar:SetColor(CLR_BLACK,oDp:nGris)

  AEVAL(oBar:aControls,{|o,n|o:SetColor(CLR_BLACK,oDp:nGris)})

RETURN .T.

FUNCTION SortArray( oCol, aData )

   local aCols, aSel, aSelData
   local cOrder
   local nAt, nFor, nLen, nSize, nCont
   local lOneCol

   aCols   := oCol:oBrw:aCols
   cOrder  := oCol:cOrder
   nAt     := oCol:nCreationOrder
   nLen    := len( aCols )
   lOneCol := Valtype( aData[ 1 ] ) != "A"

   for nFor := 1 to nLen
      aCols[ nFor ]:nHeadBmpNo := 0
      aCols[ nFor ]:cOrder := ""
   next

   if oCol:oBrw:nMarqueeStyle == MARQSTYLE_HIGHLROWMS
      aSel  := oCol:oBrw:aSelected
      nLen  := len( aSel )
      If lOneCol
         for nFor := 1 to nLen
            aData[ aSel[ nFor ] ] := { aData[ aSel[ nFor ] ], nil }
         next
      else
         nSize := len( aData[ 1 ] )
         for nFor := 1 to nLen
            Aadd(aData[ aSel[ nFor ] ], nil)
         next
      endif
   endif

   if cOrder == "" .or. cOrder == "D"
      aData := Asort( aData,,, {|x,y| x[ nAt ] < y[ nAt ] } )
      oCol:cOrder := "A"
      oCol:nHeadBmpNo := 1
   else
      aData := Asort( aData,,, {|x,y| x[ nAt ] > y[ nAt ] } )
      oCol:cOrder := "D"
      oCol:nHeadBmpNo := 2
   endif

   if oCol:oBrw:nMarqueeStyle == MARQSTYLE_HIGHLROWMS
      nLen  := len(aData)
      nCont := 1
      for nFor := 1 to nLen
         If lOneCol
            If Valtype( aData[ nFor ] ) == "A"
               aSel[ nCont++ ] := nFor
               aData[ nFor ] := aData[ nFor, 1 ]
            Endif
         else
            If len( aData[ nFor ] ) > nSize
               aSel[ nCont++ ] := nFor
               aSize( aData[ nFor ], nSize )
            Endif
         endif
      next
   Endif

   oCol:oBrw:Refresh()

RETURN nil

STATIC FUNCTION Rdd_GetFieldArray(oBrw,nField)
  //LOCAL uValue:=oBrw:aArrayData[oBrw:nArrayAt,nField]
RETURN {|| "aqui"}



