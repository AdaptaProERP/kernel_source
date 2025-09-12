#INCLUDE "FIVEWIN.CH"
#INCLUDE "XBROWSE.CH"
#INCLUDE "TCAL.CH"
#INCLUDE "TDSN.CH"
// #include "tsbutton.ch"
//#INCLUDE "ADS.CH"

// WEB CAM
#DEFINE FALSE  .F.
#DEFINE TRUE   .T.

#Define DC_CALL_STD 0x0020

Static _oWebcam_ := nil, DllWebCam
Static cWebCamDriver := "Microsoft WDM Image Capture (Win32)"

MEMVAR oDp,oCol

#define  HKEY_CLASSES_ROOT       2147483648
#define  HKEY_CURRENT_USER       2147483649
#define  HKEY_LOCAL_MACHINE      2147483650
#define  HKEY_USERS              2147483651
#define  HKEY_PERFORMANCE_DATA   2147483652
#define  HKEY_CURRENT_CONFIG     2147483653
#define  HKEY_DYN_DATA           2147483654

#DEFINE MIMESIZE    2964  //multiple of 3, 4, 76, and 78 -> perfect mimeblock for IN and OUT
#define SYSCOUNT    12

#DEFINE TEST

FUNCTION SETCOLORTOGROUP(oDlg,oFrm)
   LOCAL aControls:={},I,U,Z,oObj,oObj2,cText,bSetGet,oFolder // ,oDlg

   IF oDlg=NIL
      RETURN .T.
   ENDIF

   IF "EDIT"$oFrm:ClassName()
      oDlg:=oFrm:oDlg
   ENDIF

   IF "TDOC"$oFrm:ClassName()
      oDlg:=oFrm:oDlg
   ENDIF

   FOR I=1 TO LEN(oDlg:aControls)

      oObj:=oDlg:aControls[I]

      IF oObj:ClassName()="TFOLDER"

        oFolder:=oObj

         FOR U=1 TO LEN(oFolder:aDialogs)

             oDlg:=oFolder:aDialogs[U]

             FOR Z=1 TO LEN(oDlg:aControls)

               oObj:=oDlg:aControls[Z]

               IF oObj:ClassName()="TGROUP"
                  AADD(aControls,oObj)
               ENDIF

             NEXT Z

         NEXT U

         oObj:=NIL

      ENDIF

//? oObj:ClassName()

      IF oObj:ClassName()="TGROUP"
         AADD(aControls,oObj)
      ENDIF

   NEXT I

   FOR I=1 TO LEN(aControls)

     oObj:=aControls[I]

     SAYGRUPO(oObj,oFrm,I)

   NEXT I

   IF ValType(oObj2)="O"
     SysRefresh(.T.)
   ENDIF

   aControls:={}

RETURN .T.


FUNCTION DPDIARIO_HABIL(dDesde,dHasta,lSabado,lDomingo)
RETURN EJECUTAR("DPDIARIO_HABIL",dDesde,dHasta,lSabado,lDomingo)

FUNCTION DPDIARIO_NOHABIL(dDesde,dHasta,lSabado,lDomingo)

  DEFAULT dDesde:=FCHINIMES(oDp:dFecha),;
          dHasta:=FCHFINMES(oDp:dFecha)

  dDesde:=EVAL(oDp:bFecha,dDesde)
  dHasta:=EVAL(oDp:bFecha,dHasta)

RETURN (dHasta-dDesde)+1-EJECUTAR("DPDIARIO_HABIL",dDesde,dHasta,lSabado,lDomingo)


FUNCTION ScreenHeight()
   oDp:aCoors:=GetCoors( GetDesktopWindow() )
RETURN  oDp:aCoors[3]

FUNCTION ScreenWidth()
  oDp:aCoors:=GetCoors( GetDesktopWindow() )
RETURN  oDp:aCoors[4]

/*
FUNCTION SELCERT()
RETURN EJECUTAR("SELCERT")
*/

/*
// necesario para convertir 1.232,33 en 1234.33
*/
FUNCTION COMATOPUNTO(cVal,lVal)

   DEFAULT lVal:=.T.

   IF !("."$cVal .OR. ","$cVal) .AND. !ISALLDIGIT(cVal)

     // Todas las columnas son necesarias Numéricas
     IF lVal
        RETURN 0
     ENDIF

   ENDIF

   cVal:=STRTRAN(cVal,".","")
   cVal:=STRTRAN(cVal,",",".")

   IF lVal
      cVal:=VAL(cVal)
   ENDIF

RETURN cVal

/*
// Relleno de ZERO
*/
FUNCTION FILLZERO(cExp,nLen)
   IF ValType(cExp)="N"
     cExp:=ALLTRIM(STR(cExp))
   ENDIF
   cExp:=LEFT(ALLTRIM(cExp),nLen)
   cExp:=REPLI("0",nLen-LEN(cExp))+cExp
RETURN cExp


FUNCTION XFONT(oFont)
/*   LOCAL cFont:=;
   "'"+alltrim(cValToChar(oFont:cFaceName))+"'" +","+;
   cValToChar(oFont:nInpHeight )    +","+;
   cValToChar(oFont:nInpWidth  )    +","+;
   cValToChar(oFont:nEscapement  )    +","+;
   cValToChar(oFont:nOrientation )    +","+;
   cValToChar(oFont:nWeight     )    +","+;
   cValToChar(oFont:lBold       )    +","+;
   cValToChar(oFont:lItalic   )    +","+;
   cValToChar(oFont:lUnderLine  )    +","+;
   cValToChar(oFont:lStrikeOut )    +","+;
   cValToChar(oFont:nCharSet      )    +","+;
   cValToChar(oFont:nOutPrecision  )    +","+;
   cValToChar(oFont:nClipPrecision  )    +","+;
   cValToChar(oFont:nQuality       )    +","+;
   cValToChar(oFont:nPitchFamily   )    +","+;
   cValToChar(oFont:hFontOld      )

RETURN cFont
*/
RETURN oFont:cParam

/*
FUNCTION ISRUNMDI(cFrm)
   LOCAL oFrm

   IF Type(cFrm)="O"

      oFrm:=MacroEjec(cFrm)

      IF oFrm:oWnd:hWnd>0
         RETURN EJECUTAR("BRRUNNEW",oFrm,GetScript())
      ENDIF

   ENDIF

RETURN .T.
*/

/*
// Valida la Bï¿½squeda de un Registro segï¿½n Campo en una Tabla
*/
FUNCTION NOTFOUND(cTable,cField,uValue,cMsg)
   LOCAL lRet:=.T.
   LOCAL oTable
   LOCAL cSql:=[SELECT ]+cField+[ FROM ]+cTable+[ WHERE ]+cField+[ ]+GETWHERE("=",uValue)

   oTable:=OpenTable(cSql,.T.)
   lRet  :=oTable:RecCount()>0

   oTable:End()

   IF lRet .AND. ValType(cMsg)="C"
      MsgInfo(cMsg,MI("Advertencia"))
   ENDIF

RETURN !lRet

FUNCTION SETBRWBLQBMPFILE(oBrw)
  LOCAL oCol,I

  FOR I=1 TO LEN(oBrw:aCols)

    oCol:=oBrw:aCols[I]

    IF ValType(oBrw:aArrayData[1,I])="L"
      oCol:=BRWBLQBMPFILE(oBrw,I)
    ENDIF

  NEXT I

RETURN .T.

FUNCTION BRWBLQBMPFILE(oBrw,nCol)
    LOCAL oCol:=oBrw:aCols[nCol]

    oCol:AddBmpFile("BITMAPS\checkverde.bmp")
    oCol:AddBmpFile("BITMAPS\checkrojo.bmp")
    oCol:bStrData    :={||""}

    oCol:bBmpData:={||IIF(oBrw:aArrayData[oBrw:nArrayAt,nCol],1,2)}

RETURN oCol

FUNCTION BRWSETCLRSTD(oBrw,nClrPane1,nClrPane2,nClrText,oFrm)

     DEFAULT nClrPane1:=oFrm:nClrPane1,;
             nClrPane2:=oFrm:nClrPane2,;
             nClrText :=oFrm:nClrText

     oBrw:bClrStd := {||{nClrText,iif( oBrw:nArrayAt%2=0, nClrPane1, nClrPane2 ) } }

     oBrw:SetColor(nClrText,nClrPane1)

RETURN oBrw:bClrStd




FUNCTION SPACEFIELD(cTable,cField)
RETURN SPACE(LEN(SQLGET(cTable,cField,"1=0")))

/*
// Obtiene el MDI de un Control
*/
FUNCTION GetMdi(oControl)
   LOCAL oDlg:=GetDlg(oControl)
RETURN IIF(ValType(oDlg)="O",oDlg:oWnd ,NIL )

/*
// Valida el Typo de Expresiï¿½n
// bBlq, Pasa por Referencia
*/
FUNCTION VALIDEXP(cExp,cMetodo,bBlq)
  LOCAL cExp2:=cExp

  oDp:lCallar  :=.T.
  oDP:cMsgError:=""

  // Hay que Quitar los Comentarios

  cExp2:=STRTRAN(cExp2,CRLF,"")
  cExp2:=STRTRAN(cExp2,";","")

  bBlq:=BloqueCod(cExp2)

  oDp:lCallar:=.F.

  IF !ValType(bBlq)="B"
     MsgAlert("Metodo :"+cMetodo+CRLF+cExp+CRLF+oDp:cMsgError,"Expresiï¿½n Incorrecta")
     RETURN .F.
  ENDIF

RETURN .T.

/*
// Revisa los ComboBox, segun el valor que posee, nunca puede esta vacia la lista
*/
/*
FUNCTION ComboIni(oCbx)
   LOCAL uValue:=Eval(oCbx:bSetGet),nAt

   IF oCbx:nAt!=0
      RETURN .T.
   ENDIF

   // Comunmente incluir
   IF oCbx:nAt=0 .AND. Empty(uValue) .AND. !EMPTY(oCbx:aItems)
      oCbx:nAt:=1
      EVAL(oCbx:bSetGet,oCbx:aItems[1])
      oCbx:=NIL
      RETURN .T.
   ENDIF                                        

   // Modificaciï¿½n con Valores no Encontrados
   IF oCbx:nAt=0 .AND. !Empty(uValue) .AND. !EMPTY(oCbx:aItems) .AND. ValType(uValue)="C"
      oCbx:nAt:=aScan( oCbx:aItems, { |a| LEFT(a,LEN(uValue))=uValue } )
      IIF( oCbx:nAt>0 , EVAL(oCbx:bSetGet,oCbx:aItems[oCbx:nAt]) , NIL )
   ENDIF

RETURN .T.
*/

/*
// Editar tabla
*/
FUNCTION LbxTable(cTable,cField,uValue,cDpLbx,oGet,cVarName,cWhere) // ,oGet,lEditar(cTable,cField,cFileLbx,uValue,cDpLbx)
   // ? "lbxtable",cTable,cField,cFileLbx,uValue,cDpLbx
   oDp:lLbxTable:=.T. // Utilizado por SCROLLGET

RETURN SeekTable(cTable,cField,uValue,cDpLbx,oGet,.t.,cVarName,cWhere) // oGet,lEditarcTable,cField,uValue,cDpLbx,NIL,.T.)

/*
// Realiza Busqueda simple en un tabla
*/
FUNCTION SeekTable(cTable,cField,uValue,cDpLbx,oGet,lEditar,cVarName,cWhere)
   LOCAL oTable,cSql,lFound,oDpLbx,bBlq,_oCol,oBrw,nCol,nRow,nRowSel
   LOCAL oTxScroll:=oDp:oTxScroll //,cVarName
   LOCAL oDpBody  :=oDp:oDpBody
   LOCAL cDsn  :=GETDSN(cTable)
   LOCAL oDb   :=OpenOdbc(cDsn)

   // LOCAL oDb      :=GetDsn(cTable)
   // ? "seektable",cTable,cField,uValue,cDpLbx

   DEFAULT lEditar:=.F. // No es Obligado

   cSql  :="SELECT "+cField+" FROM "+cTable+" WHERE  "+cField+GetWhere("=",uValue)+;
           IIF(Empty(cWhere),""," AND "+cWhere)

  // oTable:=MyTable(cSql,.T.)

// ? cSql,oDb:ClassName(),oDb:cName

   oTable:=OpenTable(cSql,.T.,oDb)
   lFound:=oTable:Reccount()>0

// ? lFound,"lFound,SeekTable"

   IF lEditar    // Obliga a Editar
      lFound:=.F.
   ENDIF

   oTable:End()

   IF !lFound .AND. !Empty(cDpLbx)

      IF oDp:IsDef("oTxScroll")
        oBrw    :=oTxScroll:oBrw
        nCol    :=oBrw:nColSel
        nRow    :=oBrw:nArrayAt
        nRowSel :=oBrw:nRowSel

        IF nCol>0 .AND. nCol<=LEN(oBrw:aCols)
          IF oGet!=NIL
            oGet    :=oBrw:aCols[nCol]:oEditGet
            cVarName:=oGet:cVarName
            bBlq:={|uValue,oGet|PutFromLbx(_oCol,uValue,oTxScroll,oGet,cVarName)}
          ELSE // Debe Escribir Directo en la Casilla
            // oGet:=Puede ser el Nombre de la Variable
            bBlq:={|uValue|PutOnCell(oTxScroll,uValue,nRow,nCol,oGet,NIL,nRowSel)}
//? "aqui es la opcion 2, dpwin32",nRow,nRowSel
          ENDIF
        ENDIF
      ENDIF
/*
      IF oDp:IsDef("oDpBody")
        oBrw    :=oDpBody:oBrw
        nCol    :=oBrw:nColSel
        nRow    :=oBrw:nArrayAt
        IF nCol>0 .AND. nCol<=LEN(oBrw:aCols)
          IF oGet!=NIL
            oGet    :=oBrw:aCols[nCol]:oEditGet
            cVarName:=oGet:cVarName
            bBlq:={|uValue,oGet|PutFromLbx(_oCol,uValue,oDpBody,oGet,cVarName)}
          ELSE // Debe Escribir Directo en la Casilla
            // oGet=Puede ser cVarName
         // bBlq:={|uValue|PutOnCell(oDpBody,uValue,nRow,nCol,oGet,cVarName)}
            bBlq:={|uValue|PutOnCell(oDpBody,uValue,nRow,nCol,oGet,cVarName,nRowSel)}
          ENDIF
        ENDIF
      ENDIF
*/
      // cFileLbx,cTitle,cWhere,lPage,cField,uValue,aParam,lDialog,oDb
      oDpLbx:=DpLbx(cDpLbx,NIL,cWhere,NIL,NIL,uValue,NIL,NIL,oDb)

// ? "aqui dice luis que debe ser",oDpLbx:oWnd:ClassName()

      IF ValType(oDpLbx)="O"
        // ? " aqui es, hay que buscar ",oGet:ClassName(),uValue
        oDpLbx:GetValue(cField,uValue,bBlq)
        // ? "aqui es",cField,"dpwin32"
        // oDpLbx:GetValue(cField,uValue,bBlq,uValue)
        // lFound:=.T. // JN 28/03/2016 en valid scrollget genera .T. // El Foco debe Estar en DpLbx
        oDp:oFocus:=oDpLbx:oBrw // Obliga a xBrowse a Cambiar el Foco

        IF ValType(oTxScroll)="O" // 28/03/2016
           oTxScroll:lMsgErr:=.F.
        ENDIF

      ENDIF

//      DPFOCUS(oDpLbx:oBrw)

  //? "AQUI DEBE SER"

   ENDIF

RETURN lFound

/*
// Escribe en la Casilla ScrollGet
*/
FUNCTION PutFromLbx(oCol,uValue,oTxScroll,nCol,oGet,cVarName)
  LOCAL aData,nAt

  IF oTxScroll:oBrw:oWnd:hWnd>0 // Si no estï¿½ Apagado

    aData:=oTxScroll:aData

    nAt  :=ASCAN(aData,{|a,n|UPPE(ALLTRIM(a[1]))=UPPE(ALLTRIM(oGet:cVarName))})

    IF nAt>0 // Reposiciona Columna

      DEFAULT cVarName:=oGet:cVarName

      oTxScroll:PutData(oCol,uValue,oTxScroll:oBrw:nColSel,cVarName,nAt)

      oTxScroll:oBrw:DrawLine(.T.)
      oTxScroll:oBrw:SetFocus()

    ENDIF

  ENDIF

RETURN .T.

/*
// Asigna Valor Directo Sobre la Casilla
// Llamado por RunButton, SCROLLGE
*/
FUNCTION PutOnCell(oTxScroll,uValue,nRow,nCol,oGet,cVarName,nRowSel)

      oTxScroll:PutData( NIL,uValue,nCol,NIL,nRow ) // Graba en la Casilla

      oTxScroll:PutRefresh(oTxScroll:oBrw:aCols[nCol],nRow,uValue,NIL,nRowSel,.T.)

      oTxScroll:oBrw:SetFocus()

RETURN NIL

FUNCTION SqlFieldGet(cTable,cFieldWhere,uValue,cFieldName)
RETURN SeekField(cTable,cFieldWhere,uValue,cFieldName)

/*
// Realiza Busqueda simple en un tabla, sobre el campo referencia
*/
FUNCTION SeekField(cTable,cFieldWhere,uValue,cFieldName)
   LOCAL cSql,nAt,oTable,uFieldValue

   DEFAULT cFieldName:=cFieldWhere

   //Sintax: SeekField("DPTABLAS","TAB_NUMERO,TAB_NOMBRE","001DPCAMPOS","TAB_DESCRI")

   IF ("+"$cFieldWhere.OR.","$cFieldWhere) .AND. !"CONCAT("$UPPE(cFieldWhere)
       cFieldWhere:=STRTRAN(cFieldWhere,"+",",")
       cFieldWhere:="CONCAT("+cFieldWhere+")"
   ENDIF

   cSql       :="SELECT "+cFieldName+" FROM "+cTable+" WHERE  "+cFieldWhere+GetWhere("=",uValue)
   // ? cSql
   oTable     :=OpenTable(cSql,.T.)
   uFieldValue:=oTable:FieldGet(cFieldName)
   oTable:End()

RETURN uFieldValue

/*
// Valida si el Campo estï¿½ Vacio y Emite mensaje
*/
FUNCTION VACIO(uValue,cMsg)
   IF !EMPTY(uValue)
      IF !EMPTY(cMsg)
         MsgInfo(MI("Advertencia"),cMsg)
      ENDIF
      RETURN .F.
   ENDIF
RETURN .T.

/*
// Calcular Porcenjentaje
*/
FUNCTION PORCEN(nValue1,nValue2,nDecRound)
   LOCAL nValue

   DEFAULT nDecRound:=2

   nValue:=IIF(nValue2=0,0,nValue1*(nValue2/100))

RETURN ROUND(nValue,nDecRound)

FUNCTION TRUNCAR(nValue,nDec)
  LOCAL nResult,nValor
  DEFAULT nValue:=0, nDec:=2

  nValor  := 10 ^ nDec
  nResult := DIV(INT(nValue*nValor),nValor)

RETURN nResult

// RETURN DP_INT2D(nValue)


/*
// Determina la Proxima Fecha Anual
*/
FUNCTION FCHANUAL(dFecha,dNueva)
      LOCAL nYear
     // PROCESA //
     // Conviente determinada fecha en la fecha del aï¿½o en XAA
     // 02/05/90 - > 18/07/95 = 02/05/95
     // SI LA FECHA DADA ES MAYOR QUE LA FECHA REQUERIDA DEBE
     // SOLICITAR LA FECHA INDMEDIATA INFERIOR

     DEFAULT dFecha:=oDp:dFecha
     DEFAULT dNueva:=oDp:dFecha

     nYear:=YEAR(CTOO(dNueva,"D"))

     IF VALTYPE(dFecha)=[D] // SI ES FECHA -> CARACTER

        IF Month(dFecha)=2 .AND. DAY(dFecha)=29  .AND. nYear<>Year(dFecha) // Bisiesto
           dFecha:=dFecha-1
        ENDIF

        dFecha:=DTOC(dFecha)

     ENDIF

     IF VALTYPE(dNueva)=[D] // SI ES FECHA CARACTER
        dNueva :=DTOC(dNueva)
     ENDIF

     dFecha:=LEFT(dFecha,6)+RIGHT(dNueva,4)  // DD/MM + AA

     IF EMPTY(CTOD(dFecha)) // Aï¿½O VICIESTO
        RETURN FchFinMes(CTOD("01/"+SUBS(dFecha,4,6)))
     ENDIF

RETURN CTOD(dFecha)

/*
// Actualiza las tablas
*/
FUNCTION UPDATETABLE(cTable,cFile,cClave,cDsn,oSay,cText,lTraza,lBrowse,cMemo)
   LOCAL lFound,aKey:=_VECTOR(cClave),cWhere:="",I,oTable,nContar:=0
   LOCAL cFileTxt:="TRAZASQL\"+cTable+".TXT",cLine:=""
   LOCAL aFieldPos:={}

   DEFAULT lTraza :=oDp:lTracer,;
           lBrowse:=.F.

   cMemo:=""

   IF !File(cFile)
      RETURN .F.
   ENDIF

// ? [UPDATETABLE(]

   DEFAULT cText:="Recuperando Tabla "

   lMkDir("TRAZASQL")

   IF(ValType(oSay)="0",oSay:SetText(cText+cTable),NIL)

   CLOSE ALL
   USE (cFile) VIA "DBFCDX" ALIAS "ORIGEN"

   IF lBrowse
? cFile,"cFile"
      BROWSE()
      GO TOP
   ENDIF

   // Copia la Relaciï¿½n
   IF ValType(cDsn)="O"
     oTable:=OpenTable("SELECT * FROM "+cTable,.F.,cDsn)
   ELSE
     oTable:=OpenTable("SELECT * FROM "+cTable,.F.)
   ENDIF

   aFieldPos:=ALIAS_FIELDPOS(oTable)  // Arreglo {Table,Alias}

   IF lBrowse
     ViewArray(aFieldPos)
   ENDIF

   oTable:End()

   GO TOP
   oDp:lExcluye:=.F.

   WHILE !EOF()

     cWhere:=""
     AEVAL(aKey,{|a,n,nPos,uValue|;
                  nPos  :=FieldPos(a),;
                  uValue:=FieldGet(nPos),;
                  cWhere:=cWhere + IIF( Empty(cWhere) , "" , " AND " )+;
                          a+GetWhere("=",uValue)})
//?    cWhere

     oDp:lExcluye:=.F.

     IF ValType(cDsn)="O"
       oTable:=OpenTable("SELECT * FROM "+cTable+" WHERE "+cWhere,.T.,cDsn)
     ELSE
       oTable:=OpenTable("SELECT * FROM "+cTable+" WHERE "+cWhere,.T.)
     ENDIF

     oTable:lAuditar:=.F.

     DPWRITE("TEMP\"+cTable+"_UPDATETABLE.SQL",oDp:cSql)

// ? oDp:cSql,cWhere,"EN FUNCION UPDATETABLE",oTable:RecCount()

     IF oTable:RecCount()=0
        oTable:cWhere:=""
        oTable:AppendBlank()
        cMemo:=cMemo+IIF(Empty(cMemo),"",CRLF)+"Insert"
     ELSE
        cMemo:=cMemo+IIF(Empty(cMemo),"",CRLF)+" Actualizar "+cWhere
     ENDIF

     cLine:=""

//   AEVAL( aFieldPos,{|a,n| oTable:FieldPos(a[1],FIELDGET(a[2])), MensajeErr(FIELDGET(a[2],FIELDNAME(a[2])))})

     AEVAL( aFieldPos,{|a,n| oTable:Replace(oTable:aFields[a[1],1],FIELDGET(a[2]))})

     //IF lTraza
     //  cLine:=""
     //  AEVAL( aFieldPos,{|a,n| cLine:=cLine+IIF(Empty(cLine),",","")+FieldName(a[1])+"="+ALLTRIM(CTOO(FieldGet(a[1]),"C"))})
     //  cMemo:=cMemo+IIF(Empty(cMemo),"",CRLF)+cLine
     //ENDIF
     // ENDIF

     cLine:=""
/*
     FOR I := 1 TO FCOUNT()

        IF oTable:FieldPos(FieldName(I))>0
          cLine:=cLine+IIF(Empty(cLine),",","")+FieldName(I)+"="+ALLTRIM(CTOO(FieldGet(I),"C"))
          oTable:Replace(FieldName(I),FieldGet(I))
        ENDIF

     NEXT
*/
//   cMemo:=cMemo+IIF(Empty(cMemo),"",CRLF)+cLine

     oTable:lSetFieldDef:=.F.
     oTable:Commit(oTable:cWhere )
     oTable:End()

     IF lTraza
       cMemo:=cMemo+IIF(Empty(cMemo),"",CRLF)+oDp:cSql
     ENDIF


     IF ++nContar>10

        IIF( ValType(oSay)="O", oSay:SetText("Recuperando Tabla "+cTable+" "+LSTR(RECNO())+"/"+LSTR(RECCO())),NIL)

        nContar:=0
        SysRefresh(.T.)

     ELSE


        CursorWait()

     ENDIF

     DBSKIP()

   ENDDO

   USE

   SysRefresh(.T.)

   IF lTraza
     DPWRITE(cFileTxt,cMemo)
   ENDIF

RETURN .T.

FUNCTION ALIAS_FIELDPOS(oTable)
    LOCAL aFieldPos:={}
    AEVAL(oTable:aFields,{|a,n,nAt| nAt:= FIELDPOS(oTable:aFields[n,1]),;
                                    IF(nAt>0, AADD(aFieldPos,{n,nAt}),NIL) })
RETURN aFieldPos

/*
// IMPORTDBF32
*/
FUNCTION IMPORTDBF32(cNumTab,cTable,cDsn,oSay,lFromDbf,lCopy)
RETURN EJECUTAR("IMPORTDBF32",cNumTab,cTable,cDsn,oSay,lFromDbf,lCopy)
/*
  LOCAL oOdbc:=OPENODBC(cDsn),aTablas:={},nStep:=0,I,lMemo
  LOCAL cFile:=cFileNoExt(cFileName(cTable))
  LOCAL aFields
  LOCAL oTable,nAt,cFileDbf
  LOCAL cFileInd:=STRTRAN(UPPE(cTable),".DBF",".IND")
  LOCAL aFieldPos:={}
  LOCAL lCrear:=.F.

  DEFAULT lFromDbf:=.F.,lCopy:=.T.

  IF !FILE(cTable)
     MensajeErr("Tabla "+cTable+" no Existe, no puede ser importada mediante IMPORTDBF32(")
     RETURN .T.
  ENDIF

  IF lFromDbf

     USE DATADBF\DPCAMPOS VIA "DBFCDX" ALIAS "DPCAMPOS"

     SET FILTER TO ALLTRIM(UPPE(DPCAMPOS->CAM_TABLE))==ALLTRIM(UPPE(cFile))

     GO TOP
     aFields:={}

     WHILE !DPCAMPOS->(EOF())
        DPCAMPOS->(AADD(aFields , {DPCAMPOS->CAM_NAME,DPCAMPOS->CAM_TYPE,DPCAMPOS->CAM_LEN,DPCAMPOS->CAM_DEC,ALLTRIM(DPCAMPOS->CAM_COMMAN)}))
        DPCAMPOS->(DBSKIP())
     ENDDO

     USE

  ENDIF

  USE (cTable) VIA "DBFCDX"

  IF EMPTY(ALIAS())
     ? "Archivo : "+cTable+" No pudo ser Abierto"
     USE
     Return .F.
  ENDIF

  SET FILTER TO

  IF !oOdbc:File(cFile)

     oOdbc:CreateTableDp( cFile, aFields , cFileInd )

     lCrear:=.T.

     IF (oDp:cTypeBD="MYSQL" .AND. oDp:lNativo) .AND. !oDp:oMySqlCon:Reconnect=NIL
       oDp:oMySqlCon:Reconnect()
     ENDIF

     SysRefresh()

  ENDIF

  IF oDp:cTypeDb="MYSQL"
     //  JN 10/06/2016 Innecesario cerrar la tablas MySQL MyCloseAll()
  ENDIF

  // La Toma del DBF
  aFields:=DBSTRUCT()


  IF !oOdbc:File(cFile)
     ? "Error al Crear: "+cFile,oOdbc:cQuery
     MemoWrit("ERR.SQL",oOdbc:cQuery)
  ENDIF

//ADDTABLE(cNumTab,cTable,cDsn,!oDp:cDsndConfig=cDsn)

  ADDTABLE(cNumTab,cFile,cDsn,!oDp:cDsnData=cDsn)

  aTablas:=GetTables()

 // nAt    :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cFile) })
 // ? nAt,cTable,Len(aTablas),aTablas[nAt,2],"ENCONTRADO"
 // viewarray(aTablas)

  IF !lCrear .AND. COUNT(cFile)>0
     USE
     RETURN .T.
  ENDIF

  oTable:=OpenTable(cFile,.F.,oOdbc,.F.)

  //oTable:=OpenTable(cFile,.F.,oOdbc,.F.)

  aFieldPos:=ALIAS_FIELDPOS(oTable)  // Arreglo {Table,Alias}


  IF !ValType(oTable)="O"
    ? "Error en Tabla "+cFile
    USE
    RETURN .F.
  ENDIF

  IF empty(oTable:aFields)
     ? "Error en Tabla "+cFile
     USE
     RETURN .F.
  ENDIF

  IF oTable:RecCount()>0 // Tiene Datos
     USE
     oTable:End()
     RETURN .T.
  ENDIF

  lMemo:=ASCAN(aFields,{|a,n|a[2]="M"})>0

  IF lMemo
    oTable:SetInsert(1)
  ELSE
    oTable:SetInsert(Min(RecCount(),1000))
  ENDIF

  SET ORDE TO 0
  DBGOTOP()

  IIF( ValType(oSay)="O" , oSay:SetText("Importando desde "+Lower(cFileNoPath(cTable))+" para "+Lower(cDsn)+"."+cFileNoExt(cFile)) , NIL )

  // BROWSE()
  // ? cFile
  CURSORWAIT()

  WHILE !EOF()

     VP("lCreate",.T.) // Indica que ha Creado la Estructura de DpConfig
     oTable:Append() // Blank()
     nStep++

     IF nStep>10
        nStep:=0
        SysRefresh()
     ELSE
        CursorWait()
     ENDIF

     IF lMemo

 //      AEVAL(aFields,{|a,i|oTable:Replace(FieldName(i),FieldGet(i))})

       AEVAL( aFieldPos,{|a,n| oTable:FieldPut(a[1],FIELDGET(a[2])) })

       oTable:Commit()

     ELSE

//       AEVAL(aFields,{|a,i|oTable:ReplaceSpeed(FieldName(i),FieldGet(i),.F.)})

       AEVAL( aFieldPos,{|a,n| oTable:ReplaceSpeed(otable:FieldName(a[1]),FIELDGET(a[2]),.F.) })

       oTable:CommitSpeed(.F.)

     ENDIF


     // BROWSE()
     DBSKIP()

  ENDDO

  // ? "LISTO",ALIAS(),cFile
  USE

  // ? oTable:cSql

  oTable:End()
  oOdbc:=NIL

RETURN NIL
*/

/*
// Revisa las Tablas del Dsn
*/
FUNCTION DsnCheckTable(cDsn,oMeter,aDsn,lIntRef,lSay)
// ? GETPROCE()
RETURN EJECUTAR("DsnCheckTable",cDsn,oMeter,aDsn,lIntRef,lSay)

/*
   LOCAL oOdbc,aTables:=GetTables()
   LOCAL aTablas:={},nAt,I,aExist:={},cType

   DEFAULT aDsn            :=LoadDsn(),;
           lIntRef         :=.T.      ,;
           oDp:lSayCheckTab:=.F.      ,;
           lSay            :=oDp:lSayCheckTab

   oDp:aChkTable:={}  // Contiene Lista de Tablas
   cType:=aDsn[1,4]

   IF cType$"DBF,ADS" // Solo para xSQL
      Return .T.
   ENDIF

   EJECUTAR("DPINTDELFAILED",cDsn) // Remueve

   DEFAULT cDsn:=oDp:cDsnData

   LOADTABLAS(.T.)

   aTables:=GetTables()

   cDsn   :=ALLTRIM(UPPE(cDsn))
   oOdbc  :=OpenOdbc(cDsn) // oDp:cDsnData)
   aExist :=oOdbc:GetTables("*", .F. ) // Existentes

   AEVAL(aExist ,{|a,n| aExist[n]:=UPPE(ALLTRIM(a))})

   IF cDsn=UPPE(ALLTRIM(oDp:cDsnData))
     AEVAL(aTables,{|a,n|IIF(ALLTRIM(UPPE(a[3]))="<MULTIPLE>" .OR. ALLTRIM(UPPE(a[3]))=cDsn ,AADD(aTablas,a),NIL)})
   ELSE
     AEVAL(aTables,{|a,n|IIF(ALLTRIM(UPPE(a[3]))=cDsn,AADD(aTablas,a),NIL)})
   ENDIF

   oDp:oMeter:Show()
   oDp:oMeter:SetTotal(Len(aTablas))
   oDp:oMeter:Refresh()

   // TAB_NUMERO Viene vacio, debe ser sustituido por TAB_NOMBRE
   AEVAL(aTablas,{|a,n| aTablas[n,1]:=IF(Empty(a[1]),a[2],a[1])})

   ADEPURA(aTablas,{|a,n| "VIEW_"$a[2]})

   oDp:lLoadTablas:=.F. // JN 31/05/2016 (Evita recargar las tablas en LOADTABLAS()

   // Previamente Actualizï¿½ las estructuras
   IF !Empty(aExist) .OR. .T.
     oDp:oSay:SetText("Ejecutando Preactualizaciï¿½n")  // +" en Base de Datos : "+cDsn)
     EJECUTAR("SQLDB_PREUPDATE",cDsn,Empty(aExist))   // jn 09/10/2014
   ENDIF

   FOR I := 1 TO LEN(aTablas)

      oDp:oMeter:Set(I)
      // oDp:oSay:SetText("Verificando Tabla: "+aTablaS[I,2]) // +" en Base de Datos : "+cDsn)
      //? aTablas[i,2],I,"esta debe estar chequeando"
      nAt:=ASCAN(aExist,aTablas[I,2])

      oDp:cMsgOdbc:={}

      IF nAt=0
        oDp:oSay:SetText("Creando Tabla: "+aTablaS[I,2]) // +" en Base de Datos : "+cDsn)
        oDp:lCrearTablas:=.T. // Evita Ejecutar DPSQLBINTONUM
      ELSE

// ? I,aTablas[i,2],ValType(aTablas[i,2])

        Checktable(aTablas[i,2]) // Crea la Tablas en la Base de datos
        oDp:oSay:SetText("Actualizando Tabla: "+aTablaS[I,2]+" R:"+LSTR(COUNT(aTablaS[I,2]))) // +" en Base de Datos : "+cDsn)
      ENDIF


      //Checktable(aTablas[i,1]) // Crea la Tablas en la Base de datos, jn 11/08/2015 crear la tabla segun nombre
      Checktable(aTablas[i,2],nil,nil,nil,lSay) // Crea la Tablas en la Base de datos

      // JN 21/11/2016
      IF !Empty(oDp:cMsgOdbc)
         AADD(oDp:aChkTable,{"CHKTABLE",aTablaS[I,2],cDsn,oDp:cMsgOdbc})
         EJECUTAR("DPINTSAVEFAILED",aTablaS[I,2],"",cDsn,oDp:cMsgOdbc,"CHKTABLE") // Guarda la Incidencia
      ENDIF

      CursorWait()
      SysRefresh(.T.)

   NEXT


   // Verificando Indices
   // Genera marca de Revisiï¿½n
   IF Empty(oOdbc:GetTables("*", .F. ))
      MensajeErr("No se Visualizan Tablas en: "+cDsn+CRLF+"Revise los provilegios del Usuario ["+oDp:cLogin+"] en el Administrador de la Base de Datos","Serï¿½ Cerrado "+oDp:cDpSys)
      oOdbc:End()
      CloseAllDsn()
      Salir()
   ENDIF

   // Ejecuta la Actualizaciï¿½n de la Base de Datos
   // EJECUTAR("SQLDB_UPDATE")

   oDp:oMeter:SetTotal(Len(aTablas))

   FOR I := 1 TO LEN(aTablas)
      oDp:oMeter:Set(I)
      oDp:oSay:SetText("Creando Indices : "+aTablaS[I,2]) // +" en Base de Datos : "+cDsn)
      EJECUTAR("BUILDINDEX",aTablas[I,2],.F.)
      CursorWait()
      SysRefresh(.t.)
   NEXT

// Verificando Indices
// Genera marca de Revisiï¿½n
//  oDp:lChkIntRef:=.F.  // No revisa Integridad Referencial
//   IF cDsn=UPPE(ALLTRIM(oDp:cDsnData))
//      aTablas:=LOADTABLAS(.T.)
//   ENDIF


   IF Empty(aTablas)
      MensajeErr("No hay Tablas para Integridad Referencial"+CRLF+GETPROCE())
   ENDIF

   oDp:oMeter:SetTotal(Len(aTablas))

   FOR I := 1 TO LEN(aTablas)
//   IF lIntRef  .OR. .T.
        oDp:oMeter:Set(I)
        oDp:oSay:SetText("Creando Integridad Referencial : "+aTablaS[I,2]) // " en Base de Datos : "+cDsn)
        // EJECUTAR("BUILDINTREF",aTablas[I,2],.F.,NIL,.F.) // No genera Indices
        EJECUTAR("BUILDINTREF",aTablas[I,2],.T.,NIL,.F.,lSay) // No genera Indices  cTable,lDelete,cTableD,lIndex
        CursorWait()
        SysRefresh(.t.)
//      ENDIF
   NEXT

   oDp:oSay:SetText("Ejecutando Post-Actualizaciï¿½n [SQLDB_POSTUPDAT]")
   EJECUTAR("SQLDB_POSTUPDAT",cDsn,LEN(aExist)=0)      // jn 09/10/2014

   MemoWrit(alltrim(cDsn)+".Chk","Revisado "+dtoc(date())+time())

RETURN .T.
*/

/*
// Copia un Cursor hacia Dbf
*/

/*
FUNCTION CopytoDbf(oCursor,cFile,oMeterR,cVia,lConvert,lOptions)
      LOCAL cAlias:=ALIAS(),cIndex,I,nLen,nField,uValue,cField

      cFile :=cFile + IIF( !"."$cFile ,".DBF" , "" )
      cIndex:=STRTRAN(UPPE(cFile),".DBF",".CDX")

      DEFAULT lConvert:=.T.,lOptions:=.F.

      CURSORWAIT()

      IF lOptions

        oCursor:GoTop()
        WHILE !oCursor:Eof()

          FOR I := 1 TO oCursor:FCount()

             uValue:=oCursor:FieldGet(I)
             IF ValType(uValue)<>"C"
                LOOP
             ENDIF

             cField:=oCursor:FieldName(I)
             uValue:=SayOptions(oCursor:cTable,cField,uValue)

             IF !Empty(uValue) .AND. uValue<>oCursor:FieldGet(I)
               nField :=oCursor:FieldPos(cField)
               oCursor:Replace(cField,uValue)
               nLen   :=oCursor:aFields[nField,3]
               nLen   :=Max(nLen,LEN(uValue))
               oCursor:aFields[nField,3]:=nLen
            ENDIF

         NEXT

         oCursor:DbSkip()

        ENDDO


      ENDIF


      DEFAULT cVia:=RDDSETDEFAULT()

      FERASE(cFile)

      IF File(cFile)
         MensajeErr("Fichero "+cFile+" estï¿½ en Uso")
         RETURN .F.
      ENDIF

      // ? LEN(oCursor:aFields),"CAMPOS"
      // ? oCursor:aFields[1,1]
      FERASE(cIndex)
      DBCREATE(cFile,oCursor:aFields,cVia)

      USE (cFile) VIA "DBFCDX" EXCLU
      SET ORDE TO 0

      IF ValType(oMeterR)="O"
        oMeterR:SetTotal(oCursor:RecCount())
        oMeterR:Set(0)
      ENDIF


      oCursor:GoTop()

      WHILE !oCursor:Eof()

         IF ValType(oMeterR)="O"
           oMeterR:Set(oCursor:Recno())
         ENDIF

         DBAPPEND()  // Agrega un Registro en DBF

         AEVAL(oCursor:aFields,{|a,i,uValue| uValue:=CheckField(oCursor:FieldGet(i),a[2],lConvert), FieldPut(i , uValue)})

         oCursor:Skip(1)

      ENDDO

      IF ValType(oMeterR)="O"
        oMeterR:Set(oCursor:RecCount())
      ENDIF


      USE

      DPSELECT(cAlias)

RETURN .T.
*/

/*
// Hace la Conversiï¿½n de los Valores de los Campos
*/
STATIC FUNCTION CheckField(uValue,cType,lConvert)

   LOCAL cTypeU:=ValType(uValue)

   DEFAULT lConvert:=.F.

   IF cTypeU="C" // .AND. ValType(lConvert)="L"
      IF !lConvert
        uValue:=ANSITOOEM(uValue)
      // ELSE
      //  uValue:=OEMTOANSI(uValue)
      ENDIF
   ENDIF

   IF cTypeU=cType
      Return uValue
   ENDIF

   DO CASE

     /* CASE cTypeU="C" .AND. cType="C"

           uValue:=ANSITOOEM(uValue) */

      CASE cTypeU="C" .AND. cType="D" // Fecha Requerida

           uValue:=CTOD(uValue)

      CASE cType="M"

           uValue:=ALLTRIM(uValue)

      OTHER

//          ? cType,cTypeU,"NECESITA Convertir",uValue

   ENDCASE


RETURN uValue
// EOF

/*
// Pasa los Datos desde DBF Hacia Tablas SQL
*/
FUNCTION DBFTOSQL(cFile,cTable,cClave,oMeter,cRdd)
   LOCAL cAlias:=ALIAS(),oTable,cSql,nField,uValue,aFields,I

   CURSORWAIT()

   DEFAULT cRdd:=RDDSETDEFAULT()

   USE (cFile) VIA (cRdd) SHARED

   aFields:=_VECTOR(cClave,",")

   AEVAL(aFields,{|a,i|aFields[i]:=FieldPos(a)})

   //nField:=FIELDPOS(cClave)

   SET ORDE TO 0

   IF ValType(oMeter)="O"
     oMeter:SetTotal(RecCount())
   ENDIF

   WHILE !EOF()

      uValue:=""
      AEVAL(aFields,{|nField|uValue:=uValue+IIF( Empty(uValue),"" ," AND " )+FieldName(nField)+"='"+FieldGet(nField)+"'" })

      cSql  :="SELECT * FROM "+cTable+" WHERE "+uValue
      oTable:=OpenTable(cSql,.T.)

      IF oTable:RecCount()=0
         oTable:AppendBlank()
      ENDIF

      FOR I := 1 TO FCOUNT()
//         oTable:Replace(FieldName(I),OemToAnsi(Fieldget(I)))
        oTable:Replace(FieldName(I),Fieldget(I))
      NEXT

      oTable:Commit("WHERE "+uValue)

      oTable:End()

      IF ValType(oMeter)="O"
        oMeter:Set(Recno())
      ENDIF

      SKIP

   ENDDO

   USE

   DPSELECT(cAlias)

RETURN .T.

/*
// Genera Nombre para Archivos Temporales para ser Impresos por Crystal Report
// Utilizado en PRENOMINA
*/
// JN 11/01/2014

FUNCTION TempoCrp(cName,lNoUse)

  DEFAULT lNoUse:=.T.

  cName:=oDp:cPathCrp+cName+IIF(!"."$cName,".DBF","")

  FERASE(cName)

  IF FILE(cName)
     lNoUse:=.F.
     MensajeErr("Archivo "+cName+" esta Abierto o Protegido")
  ENDIF

RETURN cName

/*
   LOCAL cFile,nContar:=0,cAlias:=ALIAS()
   DEFAULT cName :="TEMPO"
   DEFAULT lNoUse:=.T. // SobreEscribe el Temporal Si no estï¿½ Usado

   WHILE .T.
     cFile:=oDp:cPathCrp+cName+IIF( nContar=0,"",STRZERO(nContar,3))+".DBF"
     IF FILE(cFile)
        USE (cFile) EXCLU NEW
        IF lNoUse .AND. !EMPTY(FIELDNAME(1)) // Estï¿½ Disponible
          ZAP
          USE
        ELSE
          USE
          nContar++
          LOOP
       ENDIF
     ENDIF
     EXIT
   ENDDO
   DPSELECT(cAlias)

RETURN cFile
/*

/*
// Genera los Tï¿½tulos del Reporte/Crystal
*/
// JN 11/01/2014

FUNCTION CREATEHEAD(cFile,aData,cTitulo)
RETURN EJECUTAR("CREATEHEAD",cFile,aData,cTitulo)
/*
   LOCAL aStruct:={},cType,nLen,nDec,I,cAlias:=ALIAS(),aSave:={}

   DEFAULT aData  :={}
   DEFAULT cFile  :="TEMPO.DBF"
   DEFAULT cTitulo:="NO TITULO"

//   ErrorSys(.T.)

   AADD(aData,{"USUARIO" ,PADR(oDp:cUsuario ,02) })
   AADD(aData,{"USNOMBRE",PADR(oDp:cUsNombre,40) })
   AADD(aData,{"EMPRESA" ,PADR(oDp:cEmpresa ,60) })
   AADD(aData,{"TITULO"  ,PADR(cTitulo      ,80) })
   AADD(aData,{"FECHASIS",oDp:dFecha   })        // Fecha del Sistema

   FOR I := 1 TO LEN(aData)
      nLen :=0
      nDec :=0
      cType:=ValType(aData[I,2])
      DO CASE
        CASE cType="C"
           nLen:=LEN(ALLTRIM(aData[I,2]))
        CASE cType="D"
           nLen:=8
        CASE cType="L"
           nLen:=1
        CASE cType="N"
           nLen:=LEN(ALLTRIM(STR(aData[I,2])))
           nDec:=aData[I,2]-INT(aData[I,2])
           nDec:=IIF(nDec=0,0,LEN(ALLTRIM(STR(nDec))))
      ENDCASE

      IF Len(aData[i])>2 // Longitud Programada
         nLen:=aData[i,3]
//         ? nLen,"nlen"
      ENDIF

      nLen:=MAX(1,nLen)
      IF cType!="U"
        AADD(aStruct,{aData[I,1],cType,nLen,nDec})
        AADD(aSave,aData[I,2])
      ENDIF
   NEXT

   FERASE(cFile)

   IF File(cFile)
      MensajeErr("Archivo: "+cFile+" Posiblemente Abierto")
      RETURN .F.
   ENDIF

   DBCREATE(cFile,aStruct,"DBFCDX")

   USE (cFile) NEW EXCLU VIA "DBFCDX"
   APPEND BLANK

   // AEVAL(aSave,{|a,i|a:=IIF( ValType(a)="C",ANSITOOEM(a),a),FieldPut(i,a)})
   AEVAL(aSave,{|a,i| FieldPut(i,a)})

   USE

   DPSELECT(cAlias)

RETURN .T.
*/


/*
// Obtiene el Primer Valor del Campo
*/
FUNCTION SqlGetValue(cSql)
   LOCAL oTable:=OpenTable(cSql,.T.),uValue

   IF oTable:ShowError(cSql)
      RETURN NIL
   ENDIF

   uValue:=oTable:FieldGet(1)
   oTable:End()

RETURN uValue

/*
// Obtiene el Valor de un Campo
*/
FUNCTION SqlGetField(cTabla,cField,cFieldWhere,uValue)
    LOCAL cSql,cWhere,oTable

    DEFAULT cFieldWhere:=cField

    IF !EMPTY(uValue)
       cWhere:="WHERE "+cFieldWhere+GetWhere("=",uValue)
    ELSE
       cWhere:=IIF( !" WHERE "$cWhere," WHERE " , "" )+cWhere
    ENDIF

    cSql      :="SELECT "+cField+" FROM "+cTabla+" "+cWhere+" LIMIT 1"

    oTable:=Opentable(cSql,.T.)
    uValue:=oTable:FieldGet(1)
    oTable:End()

RETURN uValue

/*
// Obtiene la Clausula Where de una Sentencia
*/
FUNCTION SqlGetWhere(cSql)
   LOCAL nAt   :=AT(" WHERE ",Uppe(cSql))
   LOCAL cWhere:=""
   IF nAt>0
     cWhere:=SUBS(cSql,nAT,LEN(cSql))
   ENDIF
   nAt:=AT("ORDER BY ",Uppe(cWhere))
   IF nAt>0
     cWhere:=LEFT(cWhere,nAt-1)
   ENDIF
RETURN " "+ALLTRIM(cWhere)+" "

/*
// Permite Crear, Claves Forï¿½neas
*/
FUNCTION SetForeingKey(cTablaO,cFieldsO,cTablaD,cFieldsD)
   LOCAL lResp:=.T.

   ? cTablaO,cFieldsO,cTablaD,cFieldsD

RETURN lResp


#define SW_NORMAL              1
#define SW_MAXIMIZE            3
#define SW_MINIMIZE            6
#define SW_RESTORE             9

Function Calculadora()
 LOCAL hWnd

 If ( hWnd:= SeekCalculadora() ) == 0
     WinExec( 'Calc.Exe', SW_NORMAL )
 Else
     BringWindowToTop( hWnd )
     ShowWindow( hWnd, SW_RESTORE )
 EndIf

return Nil

// ------------------------------------------------------------------ //
STATIC Function SeekCalculadora()
 LOCAL aCaptions:= {'Calc','Calculator','Calculadora'}
 LOCAL nPos, hWnd
    For nPos = 1 to Len(aCaptions)
        If ( hWnd:= FindWindow( 0, aCaptions[nPos] ) ) > 0
           return hWnd
        EndIf
    Next
return hWnd
/*
// Actualizacion de Saldos, DPSLD
*/
FUNCTION ACTSLD(cTabla,cSubCod,cCodigo,cCol,dFecha,nCantid,nActual)
/*
  LOCAL nMonto,cPeriodo,cMes,cSql,cWhere,oTable

  cTabla:=CTOZERO(cTabla,4)

  DEFAULT cSubCod   :=""
  DEFAULT dFecha    :=oDp:dFecha
  DEFAULT cCodigo   :=""
  DEFAULT nCantid   :=0
  DEFAULT nActual   :=1
  DEFAULT cCol      :="D" // Columna del Debe
  DEFAULT oDp:cDpSld:="DPSLD" // Tabla de Saldos por Defecto


  cPeriodo:=STRZERO(YEAR(dFecha),4)
  cMes    :=STR(MONTH(dFecha),2)

  nMonto:=nCantid*nActual

  // ? cTabla,cPeriodo,cMes,nMonto

  cWhere:="SLD_TABLA "+GetWhere("=",cTabla  )+" AND "+;
          "SLD_SUBCOD"+GetWhere("=",cSubCod )+" AND "+;
          "SLD_CODIGO"+GetWhere("=",cCodigo )+" AND "+;
          "SLD_PERIOD"+GetWhere("=",cPeriodo)

  cSql:="SELECT SLD_"+cCol+cMes+" FROM "+oDp:cDpSld+" WHERE "+cWhere


  oTable:=OPENTABLE(cSql,.T.)
  IF oTable:Eof()
     oTable:AppendBlank()
     oTable:Replace("SLD_CODIGO",cCodigo )
     oTable:Replace("SLD_TABLA" ,cTabla  )
     oTable:Replace("SLD_SUBCOD",cSubCod )
     oTable:Replace("SLD_PERIOD",cPeriodo)
  ENDIF
  oTable:Replace("SLD_"+cCol+cMes,oTable:FieldGet(1)+nMonto)
  oTable:Commit(cWhere)
  oTable:End()

  ? cSql,cPeriodo
*/
RETURN .T.

/*
11/09/2016 UBICADA EN LIB32.PRG
FUNCTION CTOZERO(cNum,nLen)
   IF ValType(cNum)="N"
      RETURN STRZERO(cNum,nLen)
   ELSE
      nLen:=MAX(nLen,LEN(cNum))
      cNum:=REPLI("0",nLen-LEN(ALLTRIM(cNum)))+cNum
   ENDIF
RETURN cNum
*/
/*
Determinar la Fecha de un Archivo
*/
FUNCTION FILEDATE(cFile)
  LOCAL dFecha:=CTOD("")
  IF FILE(dFecha)
    dFecha := Directory( cFile )[1,3]
  ENDIF
RETURN dFecha

/*
// Determina el Picture, Segun Decimales y Enteros
*/
/*
11/09/2016
FUNCTION GetPicture(nEnt,nDec)
RETURN REPLI("9",nEnt-(nDec+IIF(nDec>0 ,1 ,0))) + IIF( nDec>0,"."+Repli("9",nDec) , "" )
*/

/*
// Genera la Sintaxis para el Comando inner Join con Todas las Tablas
*/
FUNCTION GetSqlInnerAll(cTableE,cExcluye)
   LOCAL cInner:="",I,aTabla,cSelect:=""
   LOCAL aLinks:=GetLinks(cTableE)

   FOR I := 1 TO LEN(aLinks)
      IF !aLinks[I,1]$cSelect

        aTabla:=_VECTOR(aLinks[I,1],".")

        IF ValType(cExcluye)="C" .AND.  aTabla[2]$cExcluye //=aTabla[2]
           LOOP
        ENDIF

//        ? aLinks[I,1],aTabla[1],aTabla[2],cExcluye

        aLinks[I,2]:=IIF( ","$aLinks[I,2],"CONCAT("+aLinks[I,2]+")",aLinks[I,2])
        aLinks[I,3]:=IIF( ","$aLinks[I,3],"CONCAT("+aLinks[I,3]+")",aLinks[I,3])
        cInner:=cInner+IIF( EMPTY(cInner),"" , CRLF )+;
                " INNER JOIN "+aTabla[2]+" ON "+aLinks[I,2]+"="+aLinks[I,3]

      ENDIF
      cSelect:=cSelect+aLinks[I,1]
   NEXT I

RETURN cInner

/*
// Genera la Sintaxis para el Comando inner Join con Todas las Tablas
*/
FUNCTION GetSqlInner(cTableE,cTableD)
   LOCAL cInner:="",I
   LOCAL aLinks:=GetLinks(cTableE)

   FOR I := 1 TO LEN(aLinks)

      IF UPPE(ALLTRIM(cTableE)+"."+ALLTRIM(cTableD))=aLinks[I,1]
        aLinks[I,2]:=IIF( ","$aLinks[I,2],"CONCAT("+aLinks[I,2]+")",aLinks[I,2])
        aLinks[I,3]:=IIF( ","$aLinks[I,3],"CONCAT("+aLinks[I,3]+")",aLinks[I,3])
        cInner:=cInner+IIF( EMPTY(cInner),"" , CRLF )+;
                " INNER JOIN "+cTableD+" ON "+aLinks[I,2]+"="+aLinks[I,3]+CRLF

        EXIT

      ENDIF

   NEXT I

RETURN cInner

/*
// Obtiene la Lista de Campos de una Tabla
*/
FUNCTION GetSqlField(cTable,cField)
   LOCAL cList:="",oTable

   DEFAULT cField:="*"

   oTable:=OpenTable("SELECT "+cField+" FROM "+cTable,.F.)
   AEVAL(oTable:aFields,{|aField,i|cList:=cList+IIF( !Empty(cList) ,"," ,"" )+aField[1]})

   oTable:End()

RETURN cList


// Determina los Montos en Letras
FUNCTION ENLETRAS(MONTO,nLen,lConDec)
     LOCAL INT,DEC,I,DIGITO,VALNUM,POS,CAD:=""
     LOCAL ENLETRAS:=""

     DEFAULT lConDec:=.T. // lConDec:=IIF(lConDec=NIL,.T.,lConDec) // CON DECIMALES

     INT   :=INT(MONTO)
     DEC   :=MONTO-INT
     CAD   :=ALLTRIM(STR(INT,20,0))
     IF nLen=NIL
        nLen=90
     ENDIF
     FOR I=1 TO LEN(CAD) // TO 1 STEP -1

         DIGITO=RIGHT(CAD,I)
         VALNUM=VAL(DIGITO)

         DO CASE
            CASE VALNUM<10 // POS=1.AND.LEN(CAD)=1 .AND. VALNUM>0
               ENLETRAS=UNIDAD(VALNUM)
            CASE VALNUM>9 .AND.VALNUM<21
               ENLETRAS=DECENA(VALNUM)
            CASE VALNUM>20 .AND.VALNUM<100
               ENLETRAS=UND10(VALNUM)
            CASE VALNUM>99 .AND.VALNUM<1000
               ENLETRAS=CENTENA(VALNUM)
            CASE VALNUM>999 .AND.VALNUM<100000 // CIEN MIL
               ENLETRAS=UNIDADMIL(VALNUM)
            CASE VALNUM>99999 .AND.VALNUM<1000000 // MAS DE CIEN MIL
               ENLETRAS=CIENMIL(VALNUM)
            CASE VALNUM>999999  // .AND.VALNUM<100000000 // MAS DE UN MILLON
               ENLETRAS=MILLONES(VALNUM)
         ENDCASE

     NEXT I
     ENLETRAS=ENLETRAS+IIF(!lConDec,""," CON "+RIGHT(TRAN(DEC,[9.99]),2)+"/100")

     IF LEN(ENLETRAS)<nLen
        ENLETRAS=ENLETRAS+SPACE(nLen-LEN(ENLETRAS))
     ENDIF

RETURN ENLETRAS


STATIC FUNC UNIDAD(MONTO)
     LOCAL UNIDAD :={"","UNO","DOS","TRES","CUATRO","CINCO","SEIS","SIETE","OCHO","NUEVE"}
     IF MONTO<1
        RETURN ""
     ENDIF
RETURN UNIDAD[MONTO+1]
STATIC FUNC DECENA(MONTO)
     LOCAL DECENA :={"DIEZ","ONCE","DOCE","TRECE","CATORCE","QUINCE","DIECISEIS","DIECISIETE","DIECIOCHO","DIECINUEVE","VEINTE"}
     IF MONTO<10
        RETURN UNIDAD(MONTO)
     ENDIF
RETURN DECENA[MONTO-9]
STATIC FUNC UND10(MONTO)
     LOCAL UND10  :={"DIEZ","VEINTE","TREINTA","CUARENTA","CINCUENTA","SESENTA","SETENTA","OCHENTA","NOVENTA","CIEN"}
     LOCAL UNIDAD :=[],ENTRE10,_UND10,_UNIDAD,Y
     IF MONTO<20
        RETURN DECENA(MONTO)
     ENDIF
     ENTRE10=INT(MONTO/10)
     _UNIDAD =MONTO-(ENTRE10*10)
     IF _UNIDAD>0
        UNIDAD=UNIDAD(_UNIDAD)
        Y=[ Y ]
     ELSE
        Y=[]
        UNIDAD=[]
     ENDIF
     IF ENTRE10=2.AND._UNIDAD>0
        RETURN "VEINTI"+UNIDAD
     ENDIF
RETURN UND10[ENTRE10]+Y+UNIDAD
STATIC FUNC CENTENA(MONTO)
     LOCAL CENTENA :={"CIEN","DOSCIENTOS","TRESCIENTOS","CUATROCIENTOS","QUINIENTOS","SEISCIENTOS","SETECIENTOS","OCHOCIENTOS","NOVECIENTOS"}
     LOCAL ENTRE100:=INT(MONTO/100),UND10,ADICION:=[]
     // Buscar unidades de 10
     IF MONTO<100
        RETURN UND10(MONTO)
     ENDIF
     UND10:=MONTO-(ENTRE100*100)
     IF ENTRE100=1 .AND. UND10>0
        CENTENA[1]=[CIENTO]
     ENDIF
     DO CASE
       CASE UND10<10 // POS=1.AND.LEN(CAD)=1 .AND. VALNUM>0
          ADICION=UNIDAD(UND10)
       CASE UND10>9 .AND.UND10<21
          ADICION=DECENA(UND10)
       CASE UND10>20 .AND.UND10<100
          ADICION=UND10(UND10)
     ENDCASE
RETURN CENTENA[ENTRE100]+" "+ADICION
STATIC FUNC UNIDADMIL(MONTO)
     LOCAL ENTRE1000:=INT(MONTO/1000)
     LOCAL ENTRE100 :=MONTO-(ENTRE1000*1000),ELMIL:=[],CENTENA:=""
     DO CASE
        CASE ENTRE1000=1
           ELMIL="UN MIL"
        CASE ENTRE1000>1.AND.ENTRE1000<10
           ELMIL=UNIDAD(ENTRE1000)+" MIL"
        CASE ENTRE1000>9.AND.ENTRE1000<21
           ELMIL=DECENA(ENTRE1000)+" MIL"
       CASE ENTRE1000>20 .AND.ENTRE1000<100
           ELMIL=UND10(ENTRE1000)+" MIL"
       CASE ENTRE1000>99 .AND.ENTRE1000<1000
           ELMIL=CENTENA(ENTRE1000)+" MIL"
     ENDC
     CENTENA=CENTENA(ENTRE100)
RETURN ELMIL +" "+CENTENA
STATIC FUNC CIENMIL(MONTO)
     LOCAL PRIMERO,SEGUNDO,CIENMIL
     CIENMIL=STR(MONTO,6)
     PRIMERO=LEFT(CIENMIL,3)
     SEGUNDO=RIGHT(CIENMIL,3)
     IF VAL(PRIMERO)>0
        PRIMERO=ALLTRIM(CENTENA(VAL(PRIMERO)))
     ELSE
        RETURN CENTENA(MONTO)
        PRIMERO=""
     ENDIF
     IF RIGHT(PRIMERO,3)="UNO"
        PRIMERO=LEFT(PRIMERO,LEN(PRIMERO)-1)
     ENDIF
     CIENMIL=IIF(!EMPTY(PRIMERO),PRIMERO+" MIL ","")+ALLTRIM(CENTENA(VAL(SEGUNDO)))
RETURN CIENMIL
STATIC FUNC MILLONES(MONTO)
     LOCAL PRIMERO:=STR(MONTO),SEGUNDO,RESP,XRESTO
     PRIMERO=STR(MONTO,12)
     SEGUNDO=VAL(RIGHT(PRIMERO,6))
     PRIMERO=VAL(LEFT(PRIMERO,6))
     IF PRIMERO<2
        RESP="UN MILLON"
     ELSE
        RESP=ALLTRIM(CIENMIL(PRIMERO))+" MILLONES"
     ENDIF
     XRESTO=CIENMIL(SEGUNDO)
RETURN RESP+" "+XRESTO


/*
// Mensaje de Error, Luego puede ser Personalizado
*/
FUNCTION DpMsgAlert(cMsg,cTitle)
RETURN MsgAlert(cMsg,cTitle)

/*
// Verifica la Integridad Referencial
// Necesaria para Evitar Eliminar un Registro Asociado
*/
FUNCTION IsLinkIntRef(cTabla,uValue)
   LOCAL lResp:=.T.
   ? cTabla,uValue
RETURN lResp

Function NtoStr(nTotal, cDiv, cMoneda)
// Recibe un valor num,rico y devuelte una cadena traducida a nuevos pesos
LOCAL aVals,a2Digs,aPeriodos,cMonto,cCentavos
LOCAL __CRESULT,_CTEXTRES,cMillar
LOCAL NDIGITO,NPERIODO,CUNIDAD,CCENTENA

   RETURN ENLETRAS(nTotal)
/*
DEFAULT cMoneda:=" Bolï¿½vares "

  aVals := { "Ciento ", "Doscientos ", "Trescientos ", "Cuatrocientos ","Quinientos ",;
             "Seiscientos ", "Setecientos ", "Ochocientos ", "Novecientos",;
             "Dieci", "Veinti", "Treinta ", "Cuarenta ", "Cincuenta ",;
             "Sesenta ", "Setenta ", "Ochenta ", "Noventa ",;
             "Un ", "Dos ", "Tres ", "Cuatro ", "Cinco ", "Seis ","Siete ","Ocho ", "Nueve " }

  a2Digs    := { "Once ", "Doce ", "Trece ", "Catorce ", "Quince " }

  aPeriodos := { "Millones ", "Millï¿½n ", "Mil ", "", " Five" }

  cDiv     := If(cDiv == Nil, "P", cDiv)
  cMonto   := Str(nTotal, 12, 2)
  cCentavos:= SubStr(cMonto, AT(".", cMonto) + 1)
  cMonto   := SubStr(cMonto, 1, AT(".", cMonto) - 1)

  If ( cDiv == "P" )
     _cTextRes := " Con " + cCentavos + "/100 "+cMoneda
  Else
     _cTextRes := " Con " + cCentavos + "/100 "+cMoneda
  End

  __cResult := "( "
  If ( nTotal == 0.00 )
    __cResult += "Cero " + _cTextRes + " )"
    Return ( __cResult )
  End
  If ( Int(nTotal) == 0 )
    __cResult += "Cero " ///+ _cTextRes + " )"
///    Return ( __cResult )
  End
  nDigito := 1
  nPeriodo := 2
  While ( nDigito <= Len(cMonto) )
    cMillar  := SubStr(cMonto, nDigito, 1)
    cCentena := SubStr(cMonto, nDigito+1, 1)
    cUnidad  := SubStr(cMonto, nDigito+2, 1)
    Do Case
      Case SubStr(cMonto, nDigito, 3) = "000"
           nDigito += 3
           nPeriodo++
           Loop
      Case cMillar $ " 0"
      Case cMillar $ "1"
           If !( cCentena $ "0" )
             __cResult += aVals[1]
           Else
             If cUnidad $ "0"
               __cResult += "cien "
             Else
               __cResult += aVals[1]
             EndIf
           EndIf
      Case cMillar $ "23456789"
             __cResult += aVals[Val(cMillar)]
   EndCase
   Do Case
      Case cCentena $ "0"
           If ( cUnidad $ "0" )
              __cResult += aPeriodos[nPeriodo]
           Else
              If ( nPeriodo == 2 )
                __cResult += aVals[18+Val(cUnidad)] + aPeriodos[nPeriodo-1]
              Else
                __cResult += aVals[18+Val(cUnidad)] + aPeriodos[nPeriodo]
              End
           EndIf
      Case cCentena $ " "
           If !( cUnidad $ " " )
              If ( nDigito == 2 .AND. cUnidad $ "1" )
                __cResult += aVals[18 + Val(cUnidad)] + aPeriodos[nPeriodo]
              Else
                If ( nPeriodo == 2 .AND. cUnidad <> "1" )
                  __cResult += aVals[18 + Val(cUnidad)] +aPeriodos[nPeriodo-1] ///"mil "
                Else
                  __cResult += aVals[18 + Val(cUnidad)] + aPeriodos[nPeriodo]
                End
             EndIf
           EndIf
      Case cCentena $ "1"
           If cUnidad $ "0"
             __cResult += "diez "
           Else
             If cUnidad $ "12345"
               __cResult += a2Digs[Val(cUnidad)]
             Else
               __cResult += aVals[10] + aVals[18 + Val(cUnidad)]
             EndIf
           EndIf
           __cResult += aPeriodos[nPeriodo]
      Case cCentena $ "2"
           If cUnidad $ "0"
              __cResult += "veinte "
           Else
              __cResult += aVals[11] + aVals[18 + Val(cUnidad)]
           EndIf
           If ( nPeriodo == 2 )
             __cResult += aPeriodos[nPeriodo-1]
           Else
             __cResult += aPeriodos[nPeriodo]
           End
      Case cCentena $ "3456789"
           If cUnidad $ "0"
             __cResult += aVals[9+Val(cCentena)]
           Else
             __cResult += aVals[9+Val(cCentena)]+ "y "+aVals[18+Val(cUnidad)]
           EndIf
           If ( nPeriodo == 2 )
             __cResult += aPeriodos[nPeriodo-1]
           Else
            __cResult += aPeriodos[nPeriodo]
           End
    EndCase
    nDigito += 3
    nPeriodo++
  EndDo
  __cResult += _cTextRes // + " )"
  __cResult:=STRTRAN(__cResult,"("," ")
  // __cResult:=STRTRAN(__cResult,")","")
  */
Return ( __cResult )

/*
// Ediciï¿½n de Descripciï¿½n Amplia
// 237.92.51
*/

#INCLUDE "TSBUTTON.CH"
#include "RichEdit.ch"

FUNCTION DPMEMOEDIT(oEditOrg,oEditRun,oFocus,bRun,lReadOnly)
   LOCAL oEditMemo,oCursor,oMemo,cMemo:="",oBar ,nNumMemo:=0
   LOCAL nPane:=nRGB(243,250,200),aMemo,oBtn1

// ::aMemo:={cField,cTitle,nTop,nLeft,nWidth,nHeight,"",nNumMemo}

   DEFAULT bRun     :={||NIL },;
           lReadOnly:=.F.

   IF ValType(bRun)="C"
      bRun:=MacroEje(bRun)
   ENDIF

   IF ValType(oEditRun)="O" // Ejecuciï¿½n Actual
      oEditRun:oWnd:SetFocus()
      oEditRun:oWnd:Restore()
      RETURN oEditRun
   ENDIF

   IF Empty(oDp:hDllRtf) // Carga RTF
      oDp:hDLLRtf := LoadLibrary( "Riched20.dll" )
   ENDIF

   cMemo:=ALLTRIM(oEditOrg:aMemo[8])

   IF !EMPTY(oEditOrg:aMemo[7]) .AND. EMPTY(oEditOrg:aMemo[8])

      nNumMemo:=oEditOrg:aMemo[7]

      cMemo:=SqlGet(oDp:cDpMemo,"MEM_MEMO","MEM_NUMERO"+GetWhere("=",oEditOrg:aMemo[7])+" AND "+;
                                           "MEM_ID"    +GetWhere("=",oDp:cIdMemo))

      // Algunas veces, no lee el memo

    // ? cMemo,"DPMEMOEDIT(" ,nNumMemo,oDp:cDpMemo,CLPCOPY(oDp:cSql)

      IF Empty(cMemo)
        cMemo:=SqlGet(oDp:cDpMemo,"MEM_MEMO","MEM_NUMERO"+GetWhere("=",oEditOrg:aMemo[7]))
      ENDIF

    // ? cMemo,"DPMEMOEDIT(" ,nNumMemo,oDp:cDpMemo,CLPCOPY(oDp:cSql)


   ENDIF

   cMemo:=ALLTRIM(cMemo)

   oEditMemo:=DPEDIT():New(oEditOrg:aMemo[2],"DPMEMO.edt" , NIL ) // , .T. )

   oEditMemo:cScript :=""
   oEditMemo:lDlg    :=.F.
   oEditMemo:nNumMemo:=nNumMemo
   oEditMemo:CreateWindow()
   oEditMemo:cMemo   :=cMemo

   DEFINE CURSOR oCursor HAND

   IF lReadOnly
     @ 0,0 RICHEDIT oMemo VAR oEditMemo:cMemo OF oEditMemo:oWnd HSCROLL READONLY
   ELSE
     @ 0,0 RICHEDIT oMemo VAR oEditMemo:cMemo OF oEditMemo:oWnd HSCROLL // NOBORDER
   ENDIF

   oEditMemo:oWnd:oClient    := oMemo
   oEditMemo:oWnd:oClient:bKeyDown := {| nKey | if( nKey == VK_F2, EVAL(oEditMemo:oBtn:bAction) , NIL )}
   oEditMemo:oWnd:SetControl( oMemo )

   oEditMemo:bInit:={||SetBar(oEditMemo,oEditOrg,oFocus,bRun),DpFocus(oMemo),.f.}

   oEditMemo:Activate()
   //{||oEditMemo:oWnd:SetSize(100,100,.T.)})
   oEditMemo:oWnd:bValid:={||oBtn1:=NIL,oEditOrg:oEditMemo:=NIL,.T.}

   IF !oDp:aWndSizeMemo[1]=NIL
      oEditMemo:oWnd:Move(NIL,NIL,oDp:aWndSizeMemo[1],oDp:aWndSizeMemo[2],.T.)
   ENDIF

   // DpFocus(oMemo)

RETURN oEditMemo

/*
// Barra de Botones para la Descripcion Amplia
*/
STATIC FUNCTION SETBAR(oEdit,oEditOrg,oFocus,bRun)
   LOCAL oBar,oBtn,oBtnOk,oFont

   DEFINE BUTTONBAR oBar 3D SIZE 40,40 OF oEdit:oDlg

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME oDp:cPathBitMaps+"XSAVE.BMP";
          ACTION (oEditOrg:aMemo[8]:=oEdit:oWnd:oClient:GetText(),;
                  oEdit:lCancel:=.t.,oEdit:nOption:=0,oEdit:Close(),;
                  EVAL(bRun,oEditOrg,oEdit),;
                  DpFocus(oFocus))

   oBtn:cToolTip:="F2: Aceptar y Regresar"
/*

   DEFINE BUTTON oBtn OF oBar;
          ACTION (oEditOrg:aMemo[8]:=oEdit:oWnd:oClient:GetText(),;
                  oEdit:lCancel:=.t.,oEdit:nOption:=0,oEdit:Close(),;
                  EVAL(bRun,oEditOrg,oEdit),;
                  DpFocus(oFocus));
          FILENAME oDp:cPathBitMaps+"XSAVE.BMP";
          SIZE 40,40
*/

//;
//          MESSAGE "F2: Aceptar y Regresar";
//          TOOLTIP "F2: Aceptar y Regresar";
//          COLORS CLR_WHITE, { nRGB(243,250,200), CLR_BLACK, 5 }
// oEdit:oBtn:=oBtn

/*
 DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME "BITMAPS\NOTAENTREGA.BMP";
          MENU oCONSSEM:MENUFACTURA("DELPRODUCTO","NEN");
          ACTION oCONSSEM:BRAVINENTOFAV()

   oBtn:cToolTip:="Consultar Notas de Entrega"
*/
  //        MESSAGE "Esc: Cancelar y Cerrar";
  //        TOOLTIP "Esc: Cancelar y Cerrar";

/*
   DEFINE BUTTON oBtn OF oBar;
          ACTION oEdit:Close();
          FILENAME oDp:cPathBitMaps+"XSALIR.BMP";
          SIZE 40,40
*/

   DEFINE BUTTON oBtn;
          OF oBar;
          NOBORDER;
          FONT oFont;
          FILENAME oDp:cPathBitMaps+"XSALIR.BMP";
          ACTION oEdit:Close()

  oBtn:cToolTip:="Salir y Cerrar"


// ;
//        COLORS CLR_WHITE, { nRGB(243,250,200), CLR_BLACK, 5 }
// SysRefresh()

  @ 0,100 SAY IF(Empty(oDp:cIdMemo),""," [ID:"+oDp:cIdMemo+"] ")+IF(oEdit:nNumMemo=0,"Nuevo","Reg:"+LSTR(oEdit:nNumMemo,8))+" " OF oBAR BORDER SIZE 100,20 PIXEL

  oBar:SetColor(NIL,oDp:nGris)
  AEVAL(oBar:aControls,{|o|o:SetColor(NIL,oDp:nGris)})

RETURN NIL

/*
// Realiza Busqueda simple en un tabla
*/
FUNCTION XCALENDAR()
   LOCAL oBrw,nCol,nRow,oGet,cVarName,oCol
   LOCAL oTxScroll:=oDp:oTxScroll //,cVarName
   LOCAL bFecha,dFecha,bValid

//   ? "AQUI ES XCALENDAR"

   IF oDp:IsDef("oTxScroll")
      oBrw    :=oTxScroll:oBrw
      nCol    :=oBrw:nColSel
      nRow    :=oBrw:nArrayAt
      dFecha  :=oDp:oTxScroll:aView[nRow,nCol]

//      ? dFecha

//   ? "SI EXISTE oTXSCROLL",nRow,nCol,oBrw
//
//      bFecha:=LbxDate() // oNMTRABAJADOR:oFECHA_ING,oNMTRABAJADOR:FECHA_ING)
//
//   ? bFecha

      IF nCol>0 .AND. nCol<=LEN(oBrw:aCols)
         oGet    :=oBrw:aCols[nCol]:oEditGet
         oCol    :=oBrw:aCols[nCol]
         dFecha  :=ColDate(oCol,oTxScroll:aView[nRow,nCol])
         oTxScroll:aView[nRow,nCol]:=dFecha //uValue
         oBrw:DrawLine(.T.)
         oTxScroll:PutRefresh(oCol,nRow,dFecha)
      ENDIF
   ENDIF
RETURN .T.

FUNCTION SAYDATE(dFecha)
   dFecha:=CTOO(dFecha,"D")
   IF ValType(dFecha)<>"D"
      RETURN ""
   ENDIF
RETURN ALLTRIM(CSEMANA(dFecha))+" "+STRZERO(DAY(dFecha),2)+" "+;
       " de "+ALLTRIM(CMES(dFecha))+" "+STRZERO(YEAR(dFecha),4)

/*
 * $Id: calendar.prg,v 1.4 2001/11/10 14:09:39 RRamirez Exp $
 *
 * Calendar control by Ricardo Ramï¿½rez
 */


Function ColDate( oCol, dFecha , bValid )
   Local oWnd, aPoint, oCalendar,nRow,nCol,oBrw
   Local dOldDate, bAction, lNormal
   Local nWidth, nHeight, oFont
   Local bNew :=dFecha

   nWidth  := 146
   nHeight := 175

   IF EMPTY(dFecha)
      oBrw  :=oCol:oBrw
      dFecha:=oBrw:aArrayData[oBrw:nArrayAt,oCol:nPos]
   ENDIF

   dOldDate := dFecha
   lNormal  := .F.

   DEFAULT bValid:=.F.

   bAction:={||bNew:=oCalendar:dNewDate}

   //ERRORSYS(.T.)

   DEFINE FONT oFont NAME "Verdana" SIZE 0, 12

   lNormal  := .t.

//   nRow    := ( ( oCol:oBrw:nRowSel - 1 ) * oCol:oBrw:nRowHeight ) + oCol:oBrw:HeaderHeight() + 2
//   nCol    := oCol:nDisplayCol + 3

//   aPoint   := {nRow+oCol:oBrw:nTop,nCol+oCol:oBrw:nLeft,oCol:nWidth - 4,oCol:oBrw:nRowHeight - 4}
//   aPoint   := {0,0,oCol:nWidth,oCol:oBrw:nRowHeight}

   DEFINE DIALOG oWnd  TITLE "Calendario" //;
      //FROM aPoint[1] + 1, aPoint[2] TO aPoint[1] + nHeight, aPoint[2] + nWidth ;
      // PIXEL
      // STYLE  nOR( DS_MODALFRAME, WS_POPUP) OF oCol:oBrw //:oWnd

   oWnd:oFont = oFont

      //COLORS CLR_WHITE, CLR_BLUE, CLR_GREEN, CLR_HRED, CLR_RED, CLR_YELLOW;
   @ 0, 0 CALENDAR oCalendar VAR dFecha OF oWnd;
          BUTTONS  POPUP ;
          COLORSUNDAYS;
          PIXEL

//   oCalendar:Move(nRow, nCol, nWidth, nHeight, .t. )

   oCalendar:bLDblClick := {|nRow| IIF( nRow >= oCalendar:nTopDays(), ;
                                   (lNormal := .T., oWnd:End()) , ) }

   oWnd:bKeyDown := {|nKey| IIF( nKey == VK_ESCAPE, ;
                                eval( bAction )   , ;
                                IIF( nKey == VK_RETURN, ;
                                     (lNormal := .T., ;
                                      oCalendar:dDate := oCalendar:dNewDate, ;
                                      eval( bAction) ), ) ;
                                ) }

//   oWnd:Move( NIL, NIL, oCalendar:nWidth(), oCalendar:nHeight() ,.t.)

   ACTIVATE DIALOG oWnd CENTERED;
      ON INIT ( oCalendar:SetBtnText( "Hoy","Nada" ), ;
                oWnd:SetSize(oCalendar:nWidth()+5, oCalendar:nHeight()+25 ,.t.),;
                oWnd:bLostFocus := {|| oWnd:bLostFocus := nil, eval(bAction)} );
      VALID ( .T. )


   IF ValType(oBrw)="O" .AND. !EMPTY(bNew)
      oBrw:aArrayData[oBrw:nArrayAt,oCol:nPos]:=bNew
   ENDIF

   STORE NIL TO oBrw,oWnd,oCol,oCalendar

Return bNew

/*
// Inspector de Objetos
*/
FUNCTION OInspect( oObject )

   local oDlg, oLbx, oSay, oGet
   local cProp := ""
   local aObjData, cData, uData
   local n
   local cVar := "                             "
   local nEn
   LOCAL lInspect := .f.
   LOCAL ActInfo
   LOCAL oWndDefault:= GetWndDefault()

   IF ValType(oObject)!="O"
      MensajeErr(cValToChar(oObject))
      RETURN .T.
   ENDIF

   DEFINE DIALOG oDlg FROM 10, 10 TO 400, 400 PIXEL

   @ 10, 10 SAY oSay VAR cVar SIZE 150, 22 OF oDlg PIXEL

   if valtype ( oObject ) == "O"

      aObjData = aOData( oObject )

      for n = 1 to Len( aObjData )
         cData = aObjData[ n ]
         uData = OSend( oObject, cData )
         aObjData[ n ] = PadR( cChr2Data( cData ), 17 ) + Chr( 9 ) +;
             ValType( uData ) + "  " + cValToChar( uData )
      next

      oDlg:cTitle := oObject:ClassName()
      uData = OSend( oObject, aObjData[ 1 ] )
   else
      if valtype( oObject ) == "A"
         aObjData := {}
         for n = 1 to Len( oObject )
            cData = oObject[n]
            aadd( aObjData, cValToChar( cData ) )
         next
  uData := aObjData[1]
      endif

   endif

   //@ 34, 10  GET oGet VAR uData SIZE 150, 12 OF oDlg PIXEL

   @ 58, 10  LISTBOX oLbx VAR cProp ITEMS aObjData ;
             SIZE 150, 90 OF oDlg PIXEL ;
             ON CHANGE ( oSay:Refresh(), oSay:cTitle := cProp  ) ;
             ON DBLCLICK ( uData := OSend( oObject, aObjData[ oLbx:GetPos() ] ),OInspect( uData ) )


// OInspect2 serï¿½a una copia de la misma funciï¿½n con otro nombre.
// Un saludo
//if( left oGet:VarPut( OSend( oObject, cProp ) ), oGet:Refresh() )

   ACTIVATE DIALOG oDlg CENTERED

   SetWndDefault(oWndDefault) // Restaura Video

Return nil

static function cChr2Data(uValue); RETURN CTOO(uValue,"C")

/*
// Compara los Valores de Ambos Objetos, Si estï¿½ Vacio lo Asume
*/
FUNCTION Igualar(oGetDesde,oGetHasta)
   LOCAL uDesde,uHasta

   IF ValType(oGetDesde)!="O"
      RETURN .T.
   ENDIF

   uDesde:=oGetDesde:VarGet()
   uHasta:=oGetHasta:VarGet()

   IF !EMPTY(uHasta) .OR. (EMPTY(uDesde).AND.EMPTY(uHasta))
      RETURN .T.
   ENDIF

   oGetHasta:VarPut(uDesde)
   oGetHasta:Refresh(.T.)
   oGetHasta:KeyBoard(13)

RETURN .F.


/*
// Visualizaciï¿½n de Deputaraciï¿½n
*/
FUNCTION DpDebug(cText,uValue,lStop)
   LOCAL oWndDefault:= GetWndDefault()
   LOCAL oBrw,oBtn,cClass:=""
   LOCAL lSalir  :=.F.
   LOCAL cView   :=""
   LOCAL lEmpty  :=.F. // cText=NIL

   DEFAULT oDp:aDebug:={}

   IF cText=NIL
      cText :="oDp"
      uValue:=oDp
      cView :=oDp:ClassName()
   ENDIF

   DEFAULT uValue    :=cText
   DEFAULT lStop     :=.F.

   IF ValType(uValue)="O"
      cClass:=uValue:ClassName()
   ENDIF

   oDp:lStop:=lStop
   cText    :=cValToChar(cText)

   cView    :=cValToChar(uValue) + IIF(ValType(uValue)="O" , uValue:ClassName() , "" )

   IF !lEmpty
     AADD(oDp:aDebug,{cText,cView,uValue})
   ENDIF

   IF oDp:oDlgDebug!=NIL

      oDp:oDlgDebug:Show()
      oBrw:=oDp:oDlgDebug:aControls[1]

      DebugIni(oDp:aDebug,oBrw)

      oBrw:Refresh()

      WHILE oDp:lStop
         SysRefresh(.T.)
      ENDDO

      SetWndDefault(oWndDefault) // Restaura Video

      RETURN .T.

   ENDIF

   DEFINE DIALOG oDp:oDlgDebug TITLE "DpXbase <Depurador> " FROM 0,0 TO 20,40

   oDp:oDlgDebug:lHelpIcon:=.F.

   oBrw:=TXBrowse():New( oDp:oDlgDebug )
   oBrw:SetArray(oDp:aDebug)
   oBrw:lRecordSelector:=.F.    // if true a record selector column is displayed

   DebugIni(oDp:aDebug,oBrw)

   oBrw:bLDblClick:={||Oinspect(oBrw:aArrayData[oBrw:nArrayAt,3])}

   oBrw:CreateFromCode()

   @ 0.2,0  BUTTON " Segir   " ACTION oDp:lStop:=.F.       SIZE 25,15 PIXEL

   @ 0.2,31 BUTTON " Ocultar " ACTION (oDp:lStop:=.F.,;
                                     oDp:oDlgDebug:Hide()) SIZE 25,15 PIXEL

   @ 0.2,61 BUTTON " Iniciar " ACTION (oDp:lStop:=.F.,;
                                       DebugIni({},oBrw))         SIZE 25,15 PIXEL

   ACTIVATE DIALOG oDp:oDlgDebug NOWAIT;
            ON INIT(oBrw:Move(40,0,oDp:oDlgDebug:nWidth()-5,oDp:oDlgDebug:nHeight()-55,.T.),oBrw:DelCol(3),.F.);
            VALID lSalir

   WHILE oDp:lStop
      SysRefresh(.T.)
   ENDDO

   SetWndDefault(oWndDefault) // Restaura Video

RETURN NIL

STATIC FUNCTION DebugIni(aData,oBrw)
    LOCAL aIni:={},cView,uValue

    IF Empty(aData)
       uValue   :=oDp
       cView    :=cValToChar(uValue) + IIF(ValType(uValue)="O" , uValue:ClassName() , "" )
       AADD(aData,{oDp, cView , oDp })
       oBrw:GoTop()
    ENDIF

    oDp:aDebug:=aData

    oBrw:SetArray(aData)

    oBrw:aCols[1]:cHeader:="Variable "
    oBrw:aCols[1]:nWidth :=55

    oBrw:aCols[2]:cHeader:="Valor"
    oBrw:aCols[2]:nWidth :=200

//   oBrw:GoBottom()
    oBrw:DelCol(3)
//   oBrw:Refresh()

RETURN .T.
/*
  IF ISOEM( cText )
     cText = OEMTOANSI( cText )
  ENDIF
*/

/*
// Ejecuciï¿½n para la Reinstalaciï¿½n
// Verifica si Dptablas Existe o si Tiene Datos
*/

/*
FUNCTION REINSTALL(oMeter,oSay)
   LOCAL oDlg,oFont,oFontB,lSalir:=.F.,lRun:=.F.
   LOCAL aDsn:=LoadDsn()
   LOCAL oOdbc,oBtn
   LOCAL cType:=aDsn[1,4]
   LOCAL cPath:=aDsn[1,5]

   IF cType$"DBF,ADS"

      IF !FILE(cPath+"DPTABLAS.DBF")
         RETURN .F.
      ENDIF

   ELSE

      oOdbc  :=OPENODBC(oDp:cDsnConfig)

      IF !oOdbc:File("DPTABLAS") // Estï¿½ Instalando
        return .T.
      ENDIF

   ENDIF

   oDp:lSeguir:=.F. // Debe Reinstalar

   // DEFINE FONT oFont NAME "Arial" SIZE 0, 12 BOLD
   DEFINE FONT oFontB NAME "Arial" SIZE 0, -14 BOLD
   DEFINE FONT oFont  NAME "Arial" SIZE 0, -14

   DEFINE DIALOG oDlg TITLE "Reinstalaciï¿½n "+oDp:cDpSys FROM 0,0 TO 15,55;
          COLOR CLR_BLACK,15724527

   oDlg:lHelpIcon:=.F.

   @ .5  ,.5 SAY "IMPORTANTE" FONT oFontB;
             COLOR CLR_BLACK,15724527

   @ 1.5 ,.5 SAY "Este proceso actualiza todos los Componentes de Ejecuciï¿½n:"+CRLF+;
                ""+CRLF+;
                " Estructuras de datos "    +CRLF+;
                " Generador de Reportes y "+CRLF+;
                " Demï¿½s Definiciones Generales"  +CRLF+;
                " "+CRLF+;
                "Desea Ejecutarlo?" SIZE 180-10,60+5 FONT oFont;
                COLOR CLR_BLACK,15724527

                 // COLOR 23545,0

   //@ 5,5 BITMAP oDp:cBitMap SIZE 50,50
   @ 0.5,22-2+2 BITMAP oDp:oBmp FILENAME "bitmaps\caja2.bmp" OF oDlg SIZE 40,40 // BORDER // ADJUST NOBORDER

   @ 5,18 BUTTON oBtn PROMPT " Reinstalar " ACTION (lRun:=MsgNoYes("Las definiciones Anteriores del Estandar serï¿½n borradas","Desea Reinstalar"),IIF( lRun,(lSalir:=.T.,oDlg:End()),nil)) SIZE 35+4,NIL
   oBtn:cToolTip:="Ejecutar Actualizaciï¿½n"

   @ 5,27 BUTTON oBtn PROMPT " Seguir     " ACTION (oDp:lSeguir:=.T.,lSalir:=.T.,oDlg:End()) SIZE 35+4,NIL
   oBtn:cToolTip:="Cerrar Ventana y Regresar"

   ACTIVATE DIALOG oDlg CENTERED VALID lSalir ON INIT (DpFocus(oBtn),.F.)

   IF lRun
      CursorWait()
      EJECUTAR("DPUPDATETAB",oMeter,oSay)
      // DeleteConfig(oOdbc,cPath,NIL,oMeter,oSay)
   ENDIF

   // Salir(.t.)

RETURN lRun
*/

/*
// Reinicia todas las tablas

STATIC FUNCTION DeleteConfig(oOdbc,cPath)
     LOCAL aTablas:={},I,oTable,aUpDate:={}

     DEFAULT oOdbc:=OPENODBC(oDp:cDsnConfig)

     AADD(aTablas,"DPCAMPOS")
     AADD(aTablas,"DPTABLAS")
     AADD(aTablas,"DPINDEX")
     AADD(aTablas,"DPCAMPOSOP")
     AADD(aTablas,"DPLINK")
     AADD(aTablas,"DPREPORTES")
     AADD(aTablas,"DPPROGRA")
     AADD(aTablas,"DPGRUREP")
     AADD(aTablas,"DPMENU")
     AADD(aTablas,"DPBOTBAR")

     AADD(aUpDate,{"DPTABLAS"   ,"TAB_ALTER"})
     AADD(aUpDate,{"DPINDEX"    ,"IND_ALTER"})
     AADD(aUpDate,{"DPCAMPOS"   ,"CAM_ALTER"})
     AADD(aUpDate,{"DPCAMPOSOPC","OPC_ALTER"})
     AADD(aUpDate,{"DPLINK"     ,"LNK_ALTER"})
     AADD(aUpDate,{"DPREPORTES" ,"REP_ALTER"})
     AADD(aUpDate,{"DPPROGRA"   ,"PRG_ALTER"})
     AADD(aUpDate,{"DPMENU"     ,"MNU_ALTER"})
     AADD(aUpDate,{"DPBOTBAR"   ,"BOT_ALTER"})

     // IF !ISFIELD("DPMENU","MNU_TIPO")
     //  oOdbc:EXECUTE("DROP TABLE DPMENU")
     // ENDIF

     FOR I := 1 TO LEN(aUpdate)

        IF oOdbc:File(aUpDate[I,1])

           oTable:=OpenTable("SELECT * FROM "+aUpdate[I,1],.F.,oOdbc,.F.)

           IF oTable:IsDef(aUpdate[I,2])

             oTable:End()
             oTable:=OpenTable("SELECT * FROM "+aUpDate[I,1]+" WHERE "+aUpDate[I,2]+"=1",.T.)
             IF oTable:RecCount()>0
               oTable:CTODBF(aUpdate[I,1]+".DBF")
             ELSE
               FERASE(aUpDate[I,1]+".DBF")
             ENDIF

           ELSE

             FERASE(aUpdate[I,1]+".DBF")

           ENDIF

           oTable:End()

        ENDIF

     NEXT

     FOR I := 1 TO LEN(aTablas)


        IF ValType(oOdbc)="O" .AND. oOdbc:File(aTablas[I])

          oOdbc:Execute("DELETE FROM "+aTablas[I]," Borrar Registros de "+aTablas[I])

        ELSEIF ValType(oOdbc)!="O"

          // ? cPath+cFileNoExt(aTablas[I])+".CDX" , "ES DBF",aTablas[I]
          FERASE(cPath+aTablas[I]+".DBF")
          FERASE(cPath+aTablas[I]+".CDX")
          FERASE(cPath+aTablas[I]+".FPT")

          IF !FILE(cPath+aTablas[I]) // Fue Borrado
             SELE A
             USE ("DATADBF\"+aTablas[I]) VIA "DBFCDX" NEW
             COPY TO (cPath+aTablas[I])
             USE
           ENDIF

         ENDIF

     NEXT


RETURN .T.
*/

/*
// Presenta lista de Tablas
*/
FUNCTION BDLIST(cTable,aFields,lGroup,cWhere,cFielFind,cOrderBy,oControl,oDb)
LOCAL cTitle,aTitle,cFind,cFilter,cSgdoVal
RETURN EJECUTAR("REPBDLIST",cTable,aFields,lGroup,cWhere,cTitle,aTitle,cFind,cFilter,cSgdoVal,cOrderBy,cOrderBy,oControl,oDb)
/*
// Presenta lista de Tablas
*/
FUNCTION BDSELECT(cTable,aFields,lGroup,cWhere)
RETURN EJECUTAR("REPBDSELECT",cTable,aFields,lGroup,cWhere)

FUNCTION REPFECHA(cTable,cField,cTitle,nPeriodo,cWhere)
RETURN EJECUTAR("REPRGOFECHA",cTable,cField,cTitle,nPeriodo,cWhere)

// Fï¿½rmula de Gauss para el cï¿½lculo de la Pascua en un aï¿½o cualquiera
// Calcula la semana santa y el resto de celebraciones cristianas en torno a ella
// Tal y como aparecï¿½o en "ASTRONOMICAL COMPUTING, SKY & TELESCOPE", Marzo, 1986  */

FUNCTION PASCUA(xFecha,lArray)
LOCAL y,y1,a,b,b1,c,d,d1,e,f,g,h,h1,i,c1,k,l1,l,m,n,n1,p,pascua,dPascua,aFechas:=Array(0)

  Default xFecha:=Date(),lArray:=.F.

  Do Case
     Case Valtype(xFecha)="D"
          y=Year(xFecha)

     Case Valtype(xFecha)="C"
          y=val(xFecha)

     Case Valtype(xFecha)="N"
          y=xFecha

     Otherwise
          MsgInfo("El aï¿½o suministrado para el cï¿½lculo es incorrecto.","Cï¿½lculo Pascua")
          Return If(lArray,aFechas,Ctod("  -  -    "))
  EndCase

  If y<100
     y:=y+2000
  Endif

  //Atenciï¿½n: el aï¿½o debe ser mayor que 1583
  If y<=1583
     MsgInfo("El aï¿½o suministrado para el cï¿½lculo es incorrecto.","Calculo Pascua")
     Return If(lArray,aFechas,Ctod("  -  -    "))
  Endif

  Y1 := Y / 19
  A  := Int((Y1 - Int(Y1)) * 19 + 0.001)
  B1 := Y / 100
  B  := Int(B1)
  C  := Int((B1 - Int(B1)) * 100 + 0.001)
  D1 := B / 4
  D  := Int(D1)
  E  := Int((D1 - Int(D1)) * 4 + 0.001)
  F  := Int(((B + 8) / 25) + 0.001)
  G  := Int((B - F + 1) / 3)
  H1 := (19 * A + B - D - G + 15) / 30
  H  := Int((H1 - Int(H1)) * 30 + 0.001)  //calcula el mes
  C1 := C / 4
  I  := Int(C1)
  K  := Int((C1 - I) * 4 + 0.001)
  L1 := (32 + 2 * E + 2 * I - H - K) / 7
  L  := Int((L1 - Int(L1)) * 7 + 0.001)
  M  := Int((A + 11 * H + 22 * L) / 451)
  N1 := (H + L - 7 * M + 114) / 31
  N  := Int(N1)
  P  := Int((N1 - N) * 31 + 0.001)  //calcula el dï¿½a

  pascua := IIF(n=3, "Marzo", "Abril")

  dPascua:=Ctod(Strzero(P+1,2)+"-"+Strzero(N,2)+"-"+Strzero(y,4))

  //Array con     Nombre de la fiesta       Dia     Festivo nacional o domingo
  Aadd(aFechas,{"Año nuevo"              ,Ctod("01-01-"+Strzero(y,4)),.T.})
  Aadd(aFechas,{"Domingo de carnaval"    ,dPascua-49,.F.})
  Aadd(aFechas,{"Miercoles de Ceniza"    ,dPascua-49+3,.F.})
  Aadd(aFechas,{"1er Domingo de Cuaresma",dPascua-42,.T.})
  Aadd(aFechas,{"La Pasión"              ,dPascua-14,.T.})
  Aadd(aFechas,{"Domingo de Ramos"       ,dPascua- 7,.T.})
  Aadd(aFechas,{"Jueves Santo"           ,dPascua- 3,.F.})
  Aadd(aFechas,{"Viernes Santo"          ,dPascua- 2,.T.})
  Aadd(aFechas,{"Domingo de Pascua"      ,dPascua   ,.T.})
  Aadd(aFechas,{"Ascensión"              ,dPascua+39,.F.})
  Aadd(aFechas,{"Pentecostes"            ,dPascua+49,.T.})
  Aadd(aFechas,{"Santa Trinidad"         ,dPascua+56,.T.})
  Aadd(aFechas,{"Corpus Christi"         ,dPascua+60,.T.})
  Aadd(aFechas,{"Sagrado corazón"        ,dPascua+68,.F.})
  Aadd(aFechas,{"Natividad del Señor"    ,Ctod("25-12-"+Strzero(y,4)),.T.})

Return If(lArray,aFechas,dPascua)

/*
// Genera la Integridad Referencial en Todas las Tablas
*/
FUNCTION BuildIntRef(cTable)
  LOCAL cSql
  LOCAL oOdbc:=GETODBC(cTable)
  LOCAL oLink

  oLink:=OpenTable("SELECT LNK_FIELD,LNK_FIELS FROM DPLINK WHERE LNK_TABLES"+GetWhere())

  cSql:="ALTER TABLE "+cTable+" TYPE = InnoDB"

  // Cambia el Tipo de Dato
  IF !oOdbc:Execute(cSql,"ALTER TABLE")
     MensajeErr(cSql,"Tipo de Tabla")
  ENDIF

RETURN .T.

/*
// Calcula Redondeo
*/

FUNCTION DPROUND(nValor,nRed)
   LOCAL R1,V1,V2,V3

   DEFAULT nRed:=oDp:nRedondeo

   R1   :=ROUND(nValor,nRed)
   V1   :=10^(nRed)
   V2   :=V1*(nValor-R1)
   V3   :=DIV(oDp:nDec_0_4,10)

   IF V2<.5 .AND.V2>0
      V2=DIV(V3,V1)
   ELSE
      V2=0
   ENDIF

RETURN R1+V2



/*
//ttttttttttttttttttttttttttttttttttttttttttttt
FUNCTION DPROUND(nNumero,nDec)
       LOCAL nResult:=0,nDecimales:=0,nTamano
       LOCAL cDecimales,cNumero
       LOCAL nObjetivo:=0,nPosDec:=0,nPosObj:=0

       DEFAULT nDec:=2,nNumero:=0

       SET DECIMALS TO nDec

       nTamano:=LEN(ALLTRIM(STR(nNumero)))          // Longitud del Numero
       cNumero:=ALLTRIM(STR(nNumero))               // Numero en Texto
       nPosDec:=AT(".",cNumero)                     // Posicion del punto decimal en el nro.
       nDecimales:=(nTamano -  nPosDec)             // Numero de Decimales del Nro.
       cDecimales:=RIGHT(cNumero,nDecimales)        // Decimales en Texto

       IF nDecimales <= nDec .OR. nPosDec = 0
          Return nNumero
       ENDIF

    // ?? "nNumero " , cNumero
    // ?? "nDecimales= ",nDecimales,"  cNumero= ", cNumero ," --> ", cDecimales

       nObjetivo:=VAL(SUBSTR(cDecimales,nDec+1,1))
       nPosObj  :=nPosDec+nDec


    // ?? "nObjetivo ",nObjetivo

       IF nObjetivo < 5
          nResult:=VAL(STUFF(cNumero,nPosObj+1,LEN(cNumero)-nPosObj,"0" ))
    //      nResult:=DPINT(nNumero*(1 * 10 ^ nDec ))
       ELSE
          nResult:=DPINT(nNumero*(1 * 10 ^ nDec ))+1
          nResult:=nResult/(1 * 10 ^ nDec )
       ENDIF

       SET DECIMALS TO _SET_DECIMALS

    RETURN nResult

//ttttttttttttttttttttttttttttttttttttttt
*/

/*
FUNCTION DPROUND20(VALOR,RED)
   // Obtiene la Parte decimal de 1 a 4 = 5 no de 1 a 4 = 0
   LOCAL ELRED:=5
   LOCAL R1   :=ROUND(VALOR,RED)
   LOCAL V1   :=10^(RED)
   LOCAL V2   :=V1*(VALOR-R1)
   LOCAL V3   :=DIV(oDp:nDec_0_4,10)
   IF V2<.5 .AND.V2>0
      V2=DIV(V3,V1)
   ELSE
      V2=0
   ENDIF
RETURN R1+V2
*/

/*
// Calculo del Precio de Venta
*/

FUNCTION DPCALPRECIO(nCosto,nUtil,cMetodo)
     LOCAL nPrecio:=0

     DEFAULT cMetodo:=oDp:cCalPrecio

     IF Left(cMetodo,1)="C"
        nPrecio:=nCosto+PORCEN(nCosto,nUtil)
     ELSE
        nUtil:=DIV(100-nUtil,100)
        nPrecio:=DIV(nCosto,nUtil)
     ENDIF

RETURN nPrecio

/*
// Valores pasan por Referencia
*/
FUNCTION FCHEJER(dDesde,dHasta,cAno)
  LOCAL nYear:=YEAR(dHasta),nAno

  cAno  :=STRZERO(CTOO(cAno,"N"),4)
  nAno  :=YEAR(dHasta)-YEAR(dDesde)

  dHasta:=CTOD(LEFT(DTOC(dHasta),6)+cAno)
  dDesde:=CTOD(LEFT(DTOC(dDesde),6)+STRZERO(Val(cAno)-nAno,4))

RETURN dDesde
/*
// Totaliza un Array
*/
FUNCTION ATOTALES(aData,bBlq,nColIf)
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




FUNCTION SAYTEXTO(cText,oFont,oDlg)
  LOCAL nLin,nCol,oGet

  DEFAULT oDlg:=GetWndDefault()

  oGet:=ATAIL(oDlg:aControls)

  oGet:cToolTip:=cText
  nLin:=oGet:nTop-7.5
  nCol:=oGet:nLeft

  @ nLin,nCol STSAY cText PIXEL;
              SIZE 190,10;
              COLORS CLR_BLUE FONT oFont

RETURN .T.

FUNCTION SAYTEXTO2(cText,oFont,oDlg,nWidth,nHeight,nClrPane,nClrText)
  LOCAL nLin,nCol,oGet,oSay

  DEFAULT oDlg    :=GetWndDefault(),;
          nWidth  :=190,;
          nHeight :=10 ,;
          nClrPane:=oDp:nGris2,;
          nClrText:=0

  oGet:=ATAIL(oDlg:aControls)

  oGet:cToolTip:=cText
  nLin:=oGet:nTop-7.5
  nCol:=oGet:nLeft

  @ nLin,nCol SAY oSay;
              PROMPT cText PIXEL;
              SIZE nWidth,nHeight;
              FONT oFont COLOR nClrText,nClrPane

RETURN oSay


/*
// Convierte la Expresiï¿½n en Cadena
*/
FUNCTION _C(cExp)

  IF ValType(cExp)="C"
     RETURN cExp
  ENDIF

RETURN CTOO(cExp,"C")
// EOF

*/
// Verifica la Clave de Conformaciï¿½n
*/
FUNCTION GetUsuario(lMsg,cTopic,lIni)
  LOCAL oForm,oBmp,bInit,cUsuario:=SPACE(20),cPass:=SPACE(20),cLoginSAS:=SPACE(120) //,oPass,oUsuario
  LOCAL oFontB,oJuan
  LOCAL oSave,oNext
  LOCAL oTable,lAceptar:=.F.
  LOCAL nEmpresas:=COUNT("DPEMPRESA")
  LOCAL lDpXbase:=oDp:lDpXbase,cMemo:="",aTask:={}
  LOCAL bF7,bF8,bF9,bF10
  LOCAL oData:=DATACNF("GETUSUARIO","PC")
  LOCAL oTitula,oDataSet
  LOCAL cFile :=GetModuleFileName( GetInstance() )
  LOCAL aFiles:=DIRECTORY(cFile)

  oDp:lAfiliaSAS:=COUNT("DPAFILIASAS","SAS_ACTIVO=1")>1

  //  27/01/2025 EJECUTAR("CONFIGSYSLOAD")
  DpMsgClose()

  IF !Empty(aFiles)
    oDp:cExeRev:=LEFT(DTOC(aFiles[1,3]),6)+RIGHT(DTOC(aFiles[1,3]),2)+" / "+aFiles[1,4]
  ELSE
    oDp:cExeRev:=cFile+" sin Datos"
  ENDIF

  oDp:cIdTras:="GETUSER" // IDE de Traducciï¿½n

  // Guarda la opcion Seleccioar Empresa

  DEFAULT oDp:lIniSelEmp:=.T.

  // AutoIngreso del Sistema

  MACROEJE([CONF_LEERLLAVE("FORM")]) // Lee los datos de la Llave indicados en DPLLAVE.HRB

// MensajeErr("AQUI ES SALIDA")

  EJECUTAR("GETUSUARIO_AUTO")

//? COUNT("DPUSUARIOS"),ErrorSys(.T.)

/*
  IF EJECUTAR("GETUSUARIO_AUTO")
     RETURN .T.
  ENDIF
*/

  oDp:cGet_Usuario:=""
  oDp:cGet_Clave  :=""

  oDataSet:=DATACNF("GETUSUARIO","PC")
  oDp:lIniSelEmp:=oDataSet:Get("lIniSelEmp",oDp:lIniSelEmp)
  oDataSet:End()

  oDp:cTitular:=ALLTRIM(IF(Empty(oDp:cTitular),oDp:cEmpresa,oDp:cTitular))

  IF oDp:cTitular<>NIL
    oDp:oFrameDp:SetText(oDp:cDpSys+" ["+oDp:cTitular+"]")
    SetMsgLicencia()
  ENDIF

  IF ISFIELD("DPPCLOG","PC_APLTASK")

    aTask:=EJECUTAR("DPGETTASK")
    aTask:=IF(ValType(aTask)="A",aTask,{})

    cMemo:=""
    AEVAL(aTask,{|a,n| cMemo:=cMemo+IIF(Empty(cMemo),"",CRLF)+a[1] })
    SQLUPDATE("DPPCLOG","PC_APLTASK",cMemo,"PC_IP"+GetWhere("=",oDp:cIpLocal))

  ENDIF

/*
  oDp:oFrameDp:oMsgBar:SetMsg("Licencia "+oDp:cLicencia+" "+oDp:cTitular )
  oDp:oFrameDp:oMsgBar:cMsgDef:=oDp:oFrameDp:oMsgBar:cMsg
  oDp:oFrameDp:oMsgBar:ShowMsgBar()
*/
// ? oDp:oFrameDp:oMsgBar:cMsg


  DEFAULT cTopic:="DPCLAVE",;
          lIni  :=.T.

  bF7 :=SetKey(VK_F7 ,{|p,l,v| SETDELCLAVE(3,2,.T.) } )
  bF8 :=SetKey(VK_F8 ,{|p,l,v| SETDELCLAVE(3,6,.T.) } )
  bF9 :=SetKey(VK_F9 ,{|p,l,v| SETDELCLAVE(3,7,.T.) } )
  bF10:=SetKey(VK_F10,{|p,l,v| SETDELCLAVE(3,8,.T.) } )

  RELEASECONF()
  // Crea las Variable Utilizadas para la Vigencia de Licencia

//  IF oDp:nVersion<6
//      HRBLOAD("DPWIN32V5X.HRB")
//      MACROEJE("HRBISDPXBASE()")
//  ENDIF

  oDp:aHorario:={} // Horario del Usuario

  IIF( ValType(oDp:oDlgIni)="O" , oDp:oDlgIni:Hide() , NIL)

  DEFAULT lMsg:=.F. // Indica Mensaje

  oTable  :=OPENTABLE("SELECT OPE_NOMBRE,OPE_CLAVE FROM DPUSUARIOS LIMIT 1",.T.)
  cUsuario:=SPACE(20) // oTable:aFields[1,3])
  cPass   :=SPACE(20) // oTable:aFields[2,3]+10)

//? oTable:aFields[1,3],oTable:aFields[2,3],"1,2 campos"
// ? LEN(cUsuario),LEN(cPass),[LEN(cUsuario),LEN(cPass)]

  IF oTable:RecCount()=0

     IF lMsg
        MensajeErr(MI("No hay Usuarios Registrados",320))
     ENDIF

     oTable:End()
     oDp:cUsuario:="00"

     EJECUTAR("GETUSUARIO", lIni )

     PCLOG(1) // Realiza el Log del Usuario 1=Ingreso

     RETURN .F.


  ENDIF

  oTable:End()

  IF ValType(oDp:oDlgIni)="O"
     oDp:oDlgIni:End()
  ENDIF

  oDp:lDpXbase:=.F.


  /*
  // Caso de Licencias Concluidas
  */


  // ? oDp:dFchFinLic,"oDp:dFchFinLic"

  IF oDp:dFchFinLic<CTOD("30/06/2020")
    oDp:dFchFinLic:=CTOD("31/07/2020")
  ENDIF

  DEFINE FONT oFontB   NAME "Tahoma" SIZE 0,-10 BOLD
/*
  bInit:={||oForm:oDlg:Move(80,80,350+80+100,440+40,.t.),;
            oBmp:Move(0+26,0,260,oForm:oDlg:nHeight()-30,.T.),;
            oJuan:Move(380,290,200,30,.T.),;
            oForm:oUsuario:SetColor(0,NIL ),;
            oForm:oPass:SetColor(0,NIL),;
            IF(oTitula=NIL,NIL,oTitula:Move(0,0,oForm:oDlg:nWidth(),25)),;
            DPFOCUS(oForm:oUsuario),.F.}
*/
  bInit:={||oForm:oDlg:Move(80,80,800,440+40-40,.t.),;
            oForm:oDlg:SetColor(NIL,oDp:nGris2),;
            oBmp:Move(0+26,0,260+50+50+50,oForm:oDlg:nHeight()-30,.T.),;
            oJuan:Move(380-30,290+50+50+150,200,30,.T.),;
            oForm:oUsuario:SetColor(0,NIL ),;
            oForm:oPass:SetColor(0,NIL),;
            oForm:SetColorSay(NIL,oDp:nGris2),;
            IF(oTitula=NIL,NIL,oTitula:Move(0,0,oForm:oDlg:nWidth(),25)),;
            IF(!oDp:lAfiliaSAS,oForm:oLoginSAS:HIDE(),NIL),;
            IF(oDp:lAfiliaSAS,DPFOCUS(oForm:oLoginSAS),DPFOCUS(oForm:oUsuario)),.F.}


  oDp:cSHD:="" // 02/02/2023 Serial Disco Duro, será almacenado en DPPCLOG
  MACROEJE("ISPCPRG()") // Obtiene los dato del serial del disco duro

  oDp:cLicencia :=GETLLAVE_DATA("LIC_NUMERO") // 29/07/2025

// ? oDp:cLicencia,GETLLAVE_DATA("LIC_NUMERO")

  // oDp:lDpxbase:=.t.

  oDp:lIniTracer:=!Empty(oDp:cFileToScr)

  oForm:=Dialog(MI("Seguridad de Acceso ",321)+" Servidor "+oDp:cIp+" PC:"+oDp:cPcName+" SHD"+oDp:cSHD,"DPCLAVE.EDT", NIL , NIL , .F.)

  DPWRITE("TEMP\PCNAME",oDp:cPcName)

// ? "LUEGO DE DIALOG FORMULARIO DE USUARIO ",ErrorSys(.T.)

  oForm:cFileChm:="CAPITULO1.CHM"
  oForm:cTopic  :=cTopic

  oForm:lAutoInclude:=.F.
  oForm:lCancel     :=.T.
  oForm:nIntento    :=0
  oForm:nLenPass    :=LEN(cPass)
  oForm:bAceptar    :={|lValue|lAceptar:=lValue}
  oForm:nSeconds    :=SECONDS()
  oForm:nTimeMax    :=5*(10*60) // Tiempo de espera
  oForm:cLoginSAS   :=SPACE(120)
  oForm:cTopic      :=cTopic



  oForm:nClrPane    :=oDp:nGris2 // Color Definible

// ? "LUEGO DE DIALOG FORMULARIO DE USUARIO 2 ",ErrorSys(.T.)


  @ 0.0,00 BITMAP oBmp FILENAME "bitmaps\registro.bmp" OF oForm:oDlg // ADJUST

  @ 12,50+10 CHECKBOX oDp:lIniSelEmp PROMPT ANSITOOEM(MI("Seleccionar Empresas",329));
          WHEN nEmpresas>1 VALID (DPFOCUS(oForm:oUsuario),.T.)

  @ 12,50+10  CHECKBOX  oDp:lIniTracer PROMPT ANSITOOEM(MI("Traza ejecución",329)) VALID (DPFOCUS(oForm:oUsuario),.T.)

  @ 4.5,20+10 SAY MI("Usuario:",322) RIGHT
  @ 5.5,20+10 SAY MI("Clave:"  ,304,"MYSQL") RIGHT

// cLoginSAS:="loginsas"

  @ 3.1,15+10 GET oForm:oLoginSAS VAR cLoginSAS WHEN oDp:lAfiliaSAS

  @ 5.1,15+10 GET oForm:oUsuario VAR cUsuario
  @ 6.1,15+10 GET oForm:oPass    VAR cPass PASS VALID ValidUsuario(oForm,.F.,cPass)

  oForm:oUsuario:cToolTip:=MI("Nombre del Usuario",324)
  oForm:oPass   :cToolTip:=MI("Clave de Usuario",304,"MYSQL")


// ? "LUEGO DE DIALOG FORMULARIO DE USUARIO 3 ",ErrorSys(.T.)


  @13-1,18+15+25+4 XBUTTON oNext;
           SIZE 50, 19 FONT oFontB;
           FILE "BITMAPS\XNEXT.BMP","BITMAPS\XNEXT.BMP";
           PROMPT MI("Ingresar",324) NOBORDER; // WHEN oNomina:oLee:Recno()>1;
           ACTION ValidUsuario(oForm,.T.);
           COLORS CLR_BLACK, { CLR_WHITE, oDp:nGris2, 1 };       //  16765864

/*
 @13-1,18+15+25+4 BTNBMP oNext TOP ;
                  SIZE 50, 19+10 ;
                  FILENAME "BITMAPS\XNEXT.BMP",NIL,"BITMAPS\XNEXT.BMP";
                  PROMPT MI("Ingresar",324) NOBORDER;
                  ACTION ValidUsuario(oForm,.T.)
*/
  oNext:cToolTip:=MI("Ingresar al Sistema",324)+" "+oDp:cDpSys




  @13-1,31+15+25+4 XBUTTON oSave;
           SIZE 50, 19 FONT oFontB;
           FILE "BITMAPS\XSALIR.BMP","BITMAPS\XSALIR.BMP";
           PROMPT MI("Salir",6,"DPWIN") NOBORDER; // WHEN oNomina:oLee:Recno()>1;
           ACTION (lAceptar:=.F.,oForm:Close());
           COLORS CLR_BLACK, { CLR_WHITE, oDp:nGris2, 1 };       // 16765864

/*
 @90,160 BTNBMP oSave TOP;
        SIZE 50, 19+10 ;
        FILENAME "BITMAPS\XSALIR.BMP",NIL,"BITMAPS\XSALIR.BMP";
        PROMPT MI("Salir",6,"DPWIN") NOBORDER;
        ACTION (lAceptar:=.F.,oForm:Close())
*/

  oSave:cToolTip:=MI("Cerrar Sistema",4,"DPWIN")
  oSave:lCancel :=.T.

  @ oSave:nTop+22, 45+15 SAY oJuan PROMPT MI("Desarrollado por",325)+": Juan Navas"+CRLF+"jnavas@adaptaproerp.com" PIXEL SIZE NIL,20

// oDp:cExeRev:="Falta esta Variable"

  @ oSave:nTop+01, 45+25 SAY MI("Revisión: ",326)+oDp:cExeRev      PIXEL SIZE NIL,20

  @ oSave:nTop+02, 45+25 SAY "DpXbase : "+oDp:cDpXbaseAct PIXEL SIZE NIL,20


  @ oSave:nTop+03, 45+25 SAY MI("Licencia: ",327)+oDp:cLicencia   PIXEL SIZE NIL,20
  @ oSave:nTop+03, 45+25 SAY MI("Periodo : ",328)+DTOC(oDp:dFchIniLic)+" - "+DTOC(oDp:dFchFinLic) PIXEL SIZE NIL,20
//  @ oSave:nTop+04, 45 SAY "Titular : "+oDp:cTitular

  IF oDp:nVersion>=5.1 .AND. !oDp:lTracer

     IF oDp:dFchFinLic-DPFECHA()>0
        @ oSave:nTop+03, 45+25 SAY MI("Vigencia",330)+": "+ LSTR(oDp:dFchFinLic-DPFECHA(.T.))+" "+MI("Días",333)  PIXEL SIZE NIL,20
     ELSE
        @ oSave:nTop+03, 45+25 SAY MI("Vigencia Concluida desde",331)+" "+LSTR(DPFECHA(.T.)-oDp:dFchFinLic)+" Días"  PIXEL SIZE NIL,20
     ENDIF

     @ 0,0 SAY oTitula PROMPT " "+oDp:cTitular  PIXEL SIZE NIL,20

  ENDIF

  IF oDp:lAfiliaSAS
    @ 12,20+10 SAY MI("Afiliado:",322) RIGHT
  ENDIF

// ? "LUEGO DE DIALOG FORMULARIO DE USUARIO 4 ",ErrorSys(.T.)

  oForm:Activate(bInit)

  IF !lAceptar

  // JN 4/4/2018 Validar Usuario Dormido
    IF oForm:nTimeMax>0 .AND. ABS(Seconds()-oForm:nSeconds) >= (oForm:nTimeMax)
      MYSQLCHKCONN() // Validar la Apertura de la BD Usuarios Dormidos
    ENDIF

    MensajeErr(oDp:cDpSys+MI(" será Cerrado ",332))
    SALIR(.T.)

  ELSE

    IF !oDp:dFchFinLic-DPFECHA()>0  .AND. !MACROEJE("ISPCPRG()")
       MensajeErr(MI("Sólo podrá ser utilizada en Modo de Consulta",334),MI("Licencia Concluida el",335)+" "+DTOC(oDp:dFchFinLic))
    ENDIF

    SetKey(VK_F7 ,bF7  )
    SetKey(VK_F8 ,bF8  )
    SetKey(VK_F9 ,bF9  )
    SetKey(VK_F10,bF10 )

    CursorWait()

    PCLOG(1) // Realiza el Log del Usuario 1=Ingreso

    oDp:cGet_Usuario:=cUsuario
    oDp:cGet_Clave  :=cPass

    // Ubicado en DPLLAVE
    MACROEJE("EVL_AUTO(.T.)")

    EJECUTAR("GETUSUARIO", lIni )

    CursorWait()

  ENDIF

  SQLUPDATE("DPUSUARIOS","OPE_IP",oDp:cIpLocal,"OPE_NUMERO"+GetWhere("=",oDp:cUsuario  ))
  ISLICCONCLUIDA(.T.)

  IF oDp:lIniTracer .AND. Empty(oDp:cFileToScr)
     oDp:cFileToScr:="TRAZA\TRAZA.TXT"
  ENDIF

  oDp:lDpXbase:=lDpXbase

RETURN .T.

//FUNCTION  IFLICCONCLUIDA(lResp)
//RETURN ISLICCONCLUIDA(lResp)

/*
// Validar Usuario
*/
STATIC FUNCTION ValidUsuario(oForm,lButton)
 LOCAL cLogin:=ALLTRIM(oForm:oUsuario:VarGet())
 LOCAL cPass :=ALLTRIM(oForm:oPass:VarGet())
 LOCAL dFecha:=MemoRead("DP\DPLOGIN.DP")
 LOCAL lResp :=EJECUTAR("VALIDUSUARIO",oForm,lButton)
 LOCAL oTable

 IF !Empty(dFecha)

    dFecha:=CTOD(ENCRIPT(dFecha,.F.))

    IF dFecha>=oDp:dFecha

       FERASE("DP\DPLOGIN.DP")

       oTable:=OpenTable("DPCONFIG",.T.)
       oTable:Replace("CON_INVDEL","")
       oTable:Replace("CON_DPSIS" ,"")
       oTable:Replace("CON_FECHAI",CTOD(""))
       oTable:Commit("1=1")
       oTable:End()

    ENDIF

 ENDIF

 IF UPPE(ALLTRIM(cLogin))="*DATAPRO*" .OR. UPPE(ALLTRIM(cPass))="*DATAPRO*"

    DPWRITE("DP\DPLOGIN.DP",ENCRIPT(DTOC(oDp:dFecha+5),.T.))

    oForm:oUsuario:MsgErr(MI("Usuario no Encontrado",336))

    RETURN .F.

 ENDIF

 IF UPPE(ALLTRIM(cLogin))="*CLAVE*" .OR. UPPE(ALLTRIM(cPass))="*CLAVE*"
    DPWRITE("DP\DPLOGIN.DP",ENCRIPT(DTOC(oDp:dFecha+8),.T.))
    oForm:oUsuario:MsgErr(MI("Usuario no Encontrado",336))
    RETURN .F.
 ENDIF

RETURN lResp


/*
   LOCAL cLogin:=ALLTRIM(oForm:oUsuario:VarGet())
   LOCAL cPass :=ALLTRIM(oForm:oPass:VarGet())
   LOCAL oTable,lFound,cField,I,dFecha
   LOCAL cAmIni,cAmFin,cPmIni,cPmFin,cWhere
   LOCAL aSemana:={"DOMINGO","LUNES","MARTES","MIERCOLES","JUEVES","VIERNES","SABADO","DOMINGO"}
   LOCAL cDb    :=GETDSN("DPUSUARIOS")
   LOCAL cLoginE:=""
   LOCAL cPassE :=""

   // JN 4/4/2018 Validar Usuario Dormido
   IF .t. // oForm:nTimeMax>0 .AND. ABS(Seconds()-oForm:nSeconds) >= (oForm:nTimeMax)
     MYSQLCHKCONN() // Validar la Apertura de la BD Usuarios Dormidos
   ENDIF
   // JN 15/12/2022

   MySqlStart(.T.)

   IF cDb=oDp:cDsnData
      SQLUPDATE("DPTABLAS","TAB_DSN",".CONFIGURACION","TAB_NOMBRE"+getWhere("=","DPUSUARIOS"))
      LOADTABLAS(.T.)
   ENDIF

   IF !ISFIELD("DPUSUARIOS","OPE_ENCRIP")
      EJECUTAR("DPCAMPOSADD","DPUSUARIOS","OPE_ENCRIP" ,"L",01,0,"Encriptado")
      SQLUPDATE("DPUSUARIOS","OPE_ENCRIP",.F.,"OPE_ENCRIP IS NULL")
   ENDIF

   IF ISFIELD("DPUSUARIOS","OPE_ALLPC")
      SQLUPDATE("DPUSUARIOS","OPE_ALLPC" ,.T.     ,"OPE_ALLPC  IS NULL")
   ENDIF

   DEFAULT lButton:=.F. // Presionado

   IF oForm:oPass:nLastKey=38 // Subir
       RETURN .T.
   ENDIF

   // jn 06/07/2013
   oDp:oMySqlCon:ReConnect()

   IF UPPE(ALLTRIM(cLogin))="*DATAPRO*" .OR. UPPE(ALLTRIM(cPass))="*DATAPRO*"

      DPWRITE("DP\DPLOGIN.DP",ENCRIPT(DTOC(oDp:dFecha+5),.T.))

      oForm:oUsuario:MsgErr(MI("Usuario no Encontrado",336))

      RETURN .F.

   ENDIF

   IF UPPE(ALLTRIM(cLogin))="*CLAVE*" .OR. UPPE(ALLTRIM(cPass))="*CLAVE*"
      DPWRITE("DP\DPLOGIN.DP",ENCRIPT(DTOC(oDp:dFecha+8),.T.))
      oForm:oUsuario:MsgErr(MI("Usuario no Encontrado",336))
      RETURN .F.
   ENDIF

   dFecha:=MemoRead("DP\DPLOGIN.DP")

   IF !Empty(dFecha)

      dFecha:=CTOD(ENCRIPT(dFecha,.F.))

      IF dFecha>=oDp:dFecha

         FERASE("DP\DPLOGIN.DP")

         oTable:=OpenTable("DPCONFIG",.T.)
         oTable:Replace("CON_INVDEL","")
         oTable:Replace("CON_DPSIS" ,"")
         oTable:Replace("CON_FECHAI",CTOD(""))
         oTable:Commit("1=1")
         oTable:End()

      ENDIF

   ENDIF

   cWhere:="WHERE OPE_NUMERO"+GetWhere("=",EJECUTAR("GETUSERNCRIP",cLogin,cPass))

   oDp:aExcluye:={}

   oTable:=OpenTable("SELECT * FROM DPUSUARIOS "+cWhere,.T.)


   lFound:=(oTable:RecCount()>0)

   IF lFound .AND. !oTable:OPE_ACTIVO .AND. !EJECUTAR("USINACTIVO",oTable:OPE_NUMERO)
      oTable:End()
      oForm:oUsuario:MsgErr(MI("Usuario",322)+" "+oTable:OPE_NUMERO+" "+MI("Inactivo",337))
      RETURN .F.
   ENDIF

   IF lFound

      oDp:cUsuario:=oTable:OPE_NUMERO

      cLoginE:=OPE_NOMBRE()
      cPassE :=OPE_CLAVE()

      IF !ALLTRIM(cLoginE)==ALLTRIM(cLogin) .OR. !ALLTRIM(cPassE)==ALLTRIM(cPass)
         lFound:=.F.
      ENDIF

   ENDIF

   IF !lFound

      oTable:End()
      oForm:nIntento:=oForm:nIntento+1

      IF oForm:nIntento>=3
         oForm:oUsuario:MsgErr(oDp:cDpSys+MI(" serï¿½ Cerrado ",332),MI("Usuario no Admitido ",338))
         oForm:Close()
         RETURN .F.
      ENDIF

      oForm:oUsuario:MsgErr(MI("Usuario",322)+" "+cLogin+" "+MI("y Clave no Encontrado, revise tecla mayúscula/minúscula",339)+CRLF+"DB:"+cDb+"/ "+MI("Servidor:",300,"MYSQL")+oDp:cIp,MI("Intento",340)+" "+LSTR(oForm:nIntento)+"/3")

      RETURN .F.

   ELSE


      cField:=LEFT(aSemana[DOW(oDP:dFecha)],6)

      IF !oTable:FieldGet("OPE_"+cFIELD) // Dia, no Autorizado
        oTable:End()
        oForm:oUsuario:MsgErr(MI("Usuario no Está Autorizado para Acceder el día",341)+" "+UPPE(CSEMANA(oDP:dFecha)))
        RETURN .F.

      ELSE

        cField:=LEFT(cField,3)

        cAmIni:="OPE_"+cField+"AIN"
        cAmFin:="OPE_"+cField+"AFI"
        cPmIni:="OPE_"+cField+"PIN"
        cPmFin:="OPE_"+cField+"PFI"

        oDp:aHorario:={oTable:FieldGet(cAmIni),;
                       oTable:FieldGet(cAmFin),;
                       oTable:FieldGet(cPmIni),;
                       oTable:FieldGet(cPmFin),.T.}

       oDp:cUsuario  :=oTable:OPE_NUMERO // Nï¿½mero del Usuario
       oDp:cUsNombre :=OPE_NOMBRE(oDp:cUsuario) // Nï¿½mero del Usuario
       oDp:cMapaTab  :=oTable:OPE_MAPTAB   // Cï¿½digo del Mapa por Archivo
       oDp:cMapaMnu  :=oTable:OPE_MAPMNU   // Cï¿½digo del Mapa por Menï¿½
       oDp:cClave    :=oTable:OPE_CLAVE
       oDp:lUsEncript:=CTOO(oTable:OPE_ENCRIP,"L")   // Usuarios Encriptados

       IF oDp:lUsEncript
         oDp:cUsNombre :=ENCRIPT(oTable:OPE_NOMBRE,.F.)
       ENDIF

       oTable:End()

       oDp:aMapaTab  :={} // Mapa de Opciones x Tabla
       oDp:aMapaMnu  :={} // Mapa de Opciones por Menï¿½

      ENDIF

      REV_HORA(.T.) // Revisa la Hora del Usuario

      LEEMAPAS()

      IF ValType(oDp:oItemUs)="O"
        oDp:oItemUs:SetText("[U:"+oDp:cUsuario+"]") //  +oDp:cUsNombre)
      ENDIF

   ENDIF

   // Acceso desde Otro PC
   cWhere:="EXU_CODUSU"+GetWhere("=",oDp:cUsuario)+" AND "+;
           "EXU_TABLA" +GetWhere("=","DPPCLOG"   )+" AND "+;
           "EXU_CODIGO"+GetWhere("=",oDp:cPcName )+" AND "+;
           "EXU_SELECT"+GetWhere("=",.T.         )

   oTable:End()

   IF !oTable:OPE_ALLPC .AND. COUNT("DPEMPUSUARIO",cWhere)=0
     DPWRITE("TEMP\DPUSUARIOS.SQL",oDp:cSql)
     MensajeErr("Usuario ["+oDp:cUsuario+" "+oDp:cUsNombre+"] "+MI("no Tiene Permiso para Acceder al PC",342)+" "+oDp:cIpLocal)
     RETURN .F.
   ENDIF

   IF !lFound

      oForm:oPass:lPassWord:=.F.
      oForm:oPass:VarPut(SPACE(oForm:nLenPass),.T.) // Lo Pone Vacio
      oForm:oPass:lPassWord:=.T.

      DpFocus(oForm:oUsuario)
      EVAL(oForm:bAceptar,.F.)

      Return .T.

   ELSE

      EVAL(oForm:bAceptar,.T.)
      oForm:Close()

   ENDIF

RETURN lFound
*/

/*
// Verifica si el Drive Existe
*/
FUNCTION DPISDRIVE(cDrive)
   LOCAL cOldDrive:=CurDrive()+":"
   LOCAL lResp    :=lChDir(cDrive)

   lChDir(cOldDrive) // Regresa la Unidad

RETURN lResp

FUNCTION EXPORTDPXBASE(cPath)
    LOCAL oOdbc  :=OPENODBC(oDp:cDsnConfig)

    DeleteConfig(oOdbc,cPath,.F.)

RETURN .T.

/*
// Agregar Tablas DBF
*/
FUNCTION IMPORTDBF(cTable,cFile,cText,oSay,lDicc)
     LOCAL oTable,lMemo:=.F. ,nContar:=0

     DEFAULT lDicc:=.F.

     USE (cFile) ALIAS DATA READONLY

     GO TOP

     oTable:=OpenTable("SELECT * FROM "+cTable,.F.,NIL,lDicc)

     IF ASCAN(oTable:aFields,{|a,n| a[2]="M" } )>0
       lMemo:=.T.
     ELSE
      // oTable:SetInsert(1000)
     ENDIF

     WHILE !EOF()


       IF ++nContar>10

           CursorWait()
           IF( ValType(oSay)="O", oSay:SetText(cText+" "+LSTR(RECCO())+"/"+LSTR(RECNO())) , NIL)

           SysRefresh(.T.)
           nContar:=0

       ENDIF

       oTable:AppendBlank()

       IF !lMemo  .AND. .F.

           AEVAL(DbStruct(),{|a,n| oTable:ReplaceSpeed(a[1],FieldGet(n)) } )
           oTable:CommitSpeed(.F.)

       ELSE

           AEVAL(DbStruct(),{|a,n| oTable:Replace(a[1],FieldGet(n)) } )
           oTable:Commit()

       ENDIF

       SKIP

     ENDDO

     IF !lMemo
       oTable:CommitSpeed(.T.)
     ENDIF

// ? oTable:RecCount(),Recco(), cTable,cFile
     oTable:End()



     USE

RETURN NIL
/*
// Reinicia todas las tablas
*/
STATIC FUNCTION DeleteConfig(oOdbc,cPath,lDelete,oMeter,oSay)

//     LOCAL aTablas:={},I,oTable,aUpDate:={},cDir

   EJECUTAR("DPUPDATETAB",oMeter,oSay)

/*
     DEFAULT lDelete:=.T.

     RELEASETABLES()

     ADDTABLE("0001","DPTABLAS"  ,oDp:cDsnConfig,.T.)

     IF COUNT("DPTABLAS")=0

       ADDTABLE("0001","DPTABLAS"  ,oDp:cDsnConfig,.T.)
       ADDTABLE("0002","DPCONFIG"  ,oDp:cDsnConfig,.T.)
       ADDTABLE("0003","DPBOTBAR"  ,oDp:cDsnConfig,.T.)
       ADDTABLE("0004","DPCAMPOS"  ,oDp:cDsnConfig,.T.)
       ADDTABLE("0005","DPCAMPOSOP",oDp:cDsnConfig,.T.)
       ADDTABLE("0006","DPLINK"    ,oDp:cDsnConfig,.T.)
       ADDTABLE("0007","DPINDEX"   ,oDp:cDsnConfig,.T.)

       AEVAL(DpGetTables(),{|a,n| SQLDELETE(a),IMPORTDBF(a,"datadbf\"+a+".dbf") })
       LOADTABLAS()

     ENDIF

RETURN NIL // EJECUTAR("DPUPDATETAB")
*/

/*
     cPath:=IIF( Empty(cPath), "personalizado\", cPath )

     lMKDIR(STRTRAN(cPath,"\",""))

     AADD(aTablas,"DPTABLAS")
     AADD(aTablas,"DPINDEX")
     AADD(aTablas,"DPCAMPOS")
     AADD(aTablas,"DPCAMPOSOP")
     AADD(aTablas,"DPLINK")
     AADD(aTablas,"DPREPORTES")
     AADD(aTablas,"DPPROGRA")
     AADD(aTablas,"DPGRUREP")
     AADD(aTablas,"DPMENU")
     AADD(aTablas,"DPBOTBAR")

     AADD(aUpDate,{"DPTABLAS"   ,"TAB_ALTER"})
     AADD(aUpDate,{"DPINDEX"    ,"IND_ALTER"})
     AADD(aUpDate,{"DPCAMPOS"   ,"CAM_ALTER"})
     AADD(aUpDate,{"DPCAMPOSOPC","OPC_ALTER"})
     AADD(aUpDate,{"DPLINK"     ,"LNK_ALTER"})
     AADD(aUpDate,{"DPREPORTES" ,"REP_ALTERA"})
     AADD(aUpDate,{"DPPROGRA"   ,"PRG_ALTER"})
     AADD(aUpDate,{"DPMENU"     ,"MNU_ALTER"})
     AADD(aUpDate,{"DPBOTBAR"   ,"BOT_ALTER"})

     IF !ISFIELD("DPMENU","MNU_TIPO") .AND. lDelete
       oOdbc:EXECUTE("DROP TABLE DPMENU")
     ENDIF

     FOR I := 1 TO LEN(aUpdate)

        IF oOdbc:File(aUpDate[I,1])

           oTable:=OpenTable("SELECT * FROM "+aUpdate[I,1],.F.)

           IF oTable:IsDef(aUpdate[I,2])

             oTable:End()
             oTable:=OpenTable("SELECT * FROM "+aUpDate[I,1]+" WHERE "+aUpDate[I,2]+"=1",.T.)
             IF oTable:RecCount()>0
               oTable:CTODBF(cPath+aUpdate[I,1]+".DBF")
             ELSE
               FERASE(aUpDate[I,1]+".DBF")
             ENDIF

           ELSE

             FERASE(aUpdate[I,1]+".DBF")

           ENDIF

           oTable:End()

        ENDIF

     NEXT

     FOR I := 1 TO LEN(aTablas)


        IF ValType(oOdbc)="O" .AND. oOdbc:File(aTablas[I]) .AND. lDelete

          oOdbc:Execute("DELETE FROM "+aTablas[I]," Borrar Registros de "+aTablas[I])

        ELSEIF ValType(oOdbc)!="O"

          // ? cPath+cFileNoExt(aTablas[I])+".CDX" , "ES DBF",aTablas[I]
          FERASE(cPath+aTablas[I]+".DBF")
          FERASE(cPath+aTablas[I]+".CDX")
          FERASE(cPath+aTablas[I]+".FPT")

          IF !FILE(cPath+aTablas[I]) // Fue Borrado
             SELE A
             USE ("DATADBF\"+aTablas[I]) VIA "DBFCDX" NEW
             COPY TO (cPath+aTablas[I])
             USE
           ENDIF

         ENDIF

     NEXT
 */

RETURN .T.

// Determinar Carnaval

FUNCTION CARNAVAL(nAno)
   LOCAL dFecha,aData

   DEFAULT nAno:=YEAR(oDp:dFecha)

   aData :=PASCUA(CTOD("01/01/"+STRZERO(nAno,4)),.T.)

RETURN aData[2,2]+1

// Determinar Semana Santa PP

FUNCTION SEMANASANTA(nAno)
   LOCAL dFecha,aData

   DEFAULT nAno:=YEAR(oDp:dFecha)

   aData :=PASCUA(CTOD("01/01/"+STRZERO(nAno,4)),.T.)

RETURN aData[7,2]

// 11/09/2016
/*
FUNCTION DPINT(nValor)
RETURN VAL( LEFT( STR(nValor), AT(".", STR(nValor) )-1))
*/

FUNCTION NMROUND(nNumero,nDec)
   LOCAL nResult:=0,nDecimales:=0,nTamano
   LOCAL cDecimales,cNumero

   LOCAL nObjetivo:=0,nPosDec:=0,nPosObj:=0

   DEFAULT nDec:=2,nNumero:=0

   SET DECIMALS TO nDec

   nTamano:=LEN(ALLTRIM(STR(nNumero)))          // Longitud del Numero
   cNumero:=ALLTRIM(STR(nNumero))               // Numero en Texto
   nPosDec:=AT(".",cNumero)                     // Posicion del punto decimal en el nro.
   nDecimales:=(nTamano -  nPosDec)             // Numero de Decimales del Nro.
   cDecimales:=RIGHT(cNumero,nDecimales)        // Decimales en Texto

   IF nDecimales <= nDec .OR. nPosDec = 0
      Return nNumero
   ENDIF

   nObjetivo:=VAL(SUBSTR(cDecimales,nDec+1,1))
   nPosObj  :=nPosDec+nDec

   IF nObjetivo < 5
      nResult:=VAL(STUFF(cNumero,nPosObj+1,LEN(cNumero)-nPosObj,"0" ))
   ELSE
      nResult:=DPINT(nNumero*(1 * 10 ^ nDec ))+1
      nResult:=nResult/(1 * 10 ^ nDec )
   ENDIF

   SET DECIMALS TO _SET_DECIMALS

RETURN nResult

FUNCTION INLIST(cTexto, cCarBusc1, cCarBusc2, cCarBusc3, cCarBusc4, cCarBusc5, cCarBusc6, cCarBusc7, cCarBusc8, cCarBusc9, cCarBusc10)
 LOCAL I, cTextAux


 IF VALTYPE(cTexto)!="C"
    MensajeErr("Los Parï¿½metros deben ser tipo Caracter")
    RETURN .F.
 ENDIF

 FOR I=1 TO 10
     cTextAux:="cCarBusc"+ALLTRIM(STR(I))
     cTextAux:=&cTextAux
     IF VALTYPE(cTextAux)="C"
        IF cTextAux $ cTexto
           //ALERT("ENCONTRADO")
           RETURN .T.
        ENDIF
     ELSE
        //MensajeErr("El Parï¿½metro: "+ALLTRIM(STR(I))+" debe ser tipo Caracter")
        RETURN .F.
     ENDIF
 NEXT
RETURN .F.

#define ST_COMMAND  1
#define ST_SENDFILE 2
#define FILE_BLOCK 8000

FUNCTION DpServer(nPort,cScrRead,cScrWrite,cScrClose)
   LOCAL oSocket,oClient

   DEFAULT nPort    :=2000,;
           cScrRead :="SERVERREAD"  ,;
           cScrWrite:="SERVERWRITE" ,;
           cScrClose:="SERVERCLOSE"


   oSocket = TSocket():New( nPort )


   oSocket:bAccept = { | oSocket | oClient := TSocket():Accept( oSocket:nSocket ),;
                         oClient:Cargo  := ST_COMMAND,;
                         oClient:bRead  := { | oSocket | EJECUTAR(cScrRead , oSocket , oClient ) },;
                         oClient:bClose := { | oSocket | EJECUTAR(cScrClose, oSocket ) } }

//                      oClient:bWrite := { | oSocket | EJECUTAR(cScrWrite, oSocket , oClient ) },;

   oSocket:Listen()

RETURN oSocket


FUNCTION LEERDIR(cDir,cMask,cDirNoVal,cExtNoVal)
   LOCAL cLine :="",I,aData:={} ,aList2:={},nAt,cFile,cExt
   LOCAL aFiles:={},aExt:={}

   DEFAULT cMask    :="*.*" , ;
           cDirNoVal:=""    , ;
           cExtNoVal:=""

   cDirNoVal:=UPPE(ALLTRIM(cDirNoVal))
   cExtNoVal:=UPPE(ALLTRIM(cExtNoVal))
   aFiles   :=DIRECTORY(cDir+cMask,"D")
   aExt     :=_VECTOR(cExtNoVal,",")

   AEVAL(aFiles,{|a,n| aFiles[n,1]:=ALLTRIM(a[1]) }) // Quitamos los espacios vacios

   nAt   :=ASCAN(aFiles,{|a,n| "."==a[1]})
   IF(nAt>0, ARREDUCE(aFiles,nAt), NIL )

   nAt   :=ASCAN(aFiles,{|a,n| ".."==a[1]})
   IF(nAt>0, ARREDUCE(aFiles,nAt), NIL )

   FOR I=1 TO LEN(aFiles)

      cFile:= aFiles[I,1]

      IF aFiles[I,5]="D"

        // Directorio no Vï¿½lidos
        IF !Empty(cDirNoVal) .AND. (UPPE(cFile)+",")$(cDirNoVal+",")
           // "directorio no vï¿½lido", aFiles[I,1]
           LOOP
        ENDIF

        aList2:=LEERDIR( cDir + IIF(Empty(cDir) .OR. RIGHT(cDir,1)="\","","\") + aFiles[I,1] + "\" , cMask ,;
                         cDirNoVal,cExtNoVal)

        AEVAL(aList2, {|a,n| AADD(aData, {a[1],a[2],a[3],a[4],a[5],a[6]})})

      ELSE

       // Extenciones No vï¿½lidas
       cExt:= UPPE("."+cFileExt(cFile))

       IF ASCAN( aExt,{|a,n| a=cExt})>0
          LOOP
       ENDIF

//       IF !Empty(cExtNoVal) .AND. ((cExt$cExtNoVal) .OR. AT(cExt,cExtNoVal)>0)
//         ? "extensiï¿½n no vï¿½lido", aFiles[I,1]
//           LOOP
//        ENDIF

        IF Empty(cFileName(cFile))
           LOOP
        ENDIF

        AADD(aData,{cDir,aFiles[I,1],aFiles[I,2],aFiles[I,3],aFiles[I,4],aFiles[I,5]})

      ENDIF

   NEXT I

   FOR I=1 TO LEN(aData)
      IF Empty(cFileName(aData[I,1]))
         ARREDUCE(aData,I)
      ENDIF
   NEXT I

   // AEVAL(aData,{|a,n| IF ("."+UPPE(cFileExt(cFile)))$cExtNoVal , ARREDUCE(aData,n))}
   // ViewArray(aFiles)

RETURN aData

/*
Fecha en 6 dï¿½gitos
*/
FUNCTION F6(dFecha,lSepara)

  DEFAULT lSepara:=.F.

  IF lSepara

     RETURN STRZERO(DAY(dFecha),2)+"/"+STRZERO(MONTH(dFecha),2)+"/"+;
            RIGHT(STRZERO(YEAR(dFecha),4),2)

  ENDIF

RETURN STRZERO(DAY(dFecha),2)+STRZERO(MONTH(dFecha),2)+;
               RIGHT(STRZERO(YEAR(dFecha),4),2)


//
//

//----------------------------------------------------------------------------

Function fMimeDec(cIn,cOut)
Local nIn, nOut, c

   nOut := FCreate(cOut,0)
   nIn  := FOpen(cIn,0)

   ffMimeDec(nIn,nOut)

   FClose(nOut)
   FClose(nIn)

Return nil

//----------------------------------------------------------------------------

Function fMimeEnc(cIn,cOut)
Local nIn, nOut

   nOut := FCreate(cOut,0)
   nIn  := FOpen(cIn,0)

   ffMimeEnc(nIn,nOut)

   FClose(nOut)
   FClose(nIn)

Return NIL

//----------------------------------------------------------------------------

Function ffMimeDec(nIn,nOut)
local cOut, n, cIn, nS:=0, cPre:="", nMod, lEOF:= .f.

   While !lEOF
      cIn     := Space(MIMESIZE)
      if (n   := FRead(nIn,@cIn,MIMESIZE)) < MIMESIZE
         cIn  := substr(cIn,1,n)
         lEOF := .t.
      endif

      cIn     :=      strtran(cIn,Chr(13)+Chr(10),"")

      if at(chr(13),cIn)>0
         cIn     :=      strtran(cIn,Chr(13),"")
      endif
      if at(chr(10),cIn)>0
         cIn     :=      strtran(cIn,Chr(10),"")
      endif
      cIn     := cPre+cIn
      if !lEOF
         if (nMod := len(cIn)%4) > 0
            cPre  := substr(cIn,len(cIn)-nMod+1)
            cIn   := substr(cIn,1,len(cIn)-nMod)
         else
            cPre  := ""
         endif
      endif

      if !empty(cIn)
         cOut := cMimeDec(cIn)
         FWrite(nOut, cOut, Len(cOut))
      endif

      If ++nS%SYSCOUNT=0
         sysrefresh()
      Endif
   Enddo

Return nil

//----------------------------------------------------------------------------

Function ffMimeEnc(nIn,nOut)
Local cIn, cOut, n, nn:=0, cOut2, nS:=0, lEOF:=.f.

   cIn  := Space(MIMESIZE)
   cOut := ""

   While !lEOF
      if (n     := FRead(nIn,@cIn,MIMESIZE)) < MIMESIZE
         lEOF   := .t.
      endif

      cOut2 := cMimeEnc(substr(cIn,1,n))
      nn    += len(cOut2)
      cOut  += cOut2

      while nn >= 76
         FWrite(nOut,Substr(cOut,1,76)+Chr(13)+Chr(10),78)
         nn-=76
         cOut := Substr(cOut,77)

         If ++nS%SYSCOUNT=0
            sysrefresh()
         Endif
      enddo

   Enddo

   if nn>0
      FWrite(nOut,cOut+Chr(13)+Chr(10),nn+2)
      nn:=0
   Endif

Return NIL

#ifndef __XPP__

function MimeExt( cExt )

   local nHandle, cValue:= space( 50 )
   local nLen:= 50

   if RegOpenKey( HKEY_LOCAL_MACHINE,;
         "SOFTWARE\Classes\." + cExt, @nHandle ) == 0

      RegQryValueEx( nHandle, "Content Type", 0, 1, cValue, nLen )
      RegCloseKey( nHandle )
   endif

return if( empty( alltrim( cValue )), "application/octet-stream", cValue )

DLL32 FUNCTION RegQryValueEx( HKEY AS LONG, VALUE AS LPSTR, RES1 AS LONG,;
   @TYPE AS PTR, BUFFER AS LPSTR, @SIZE AS PTR ) AS LONG ;
   PASCAL FROM "RegQueryValueExA" LIB "ADVAPI32"

#endif

//----------------------------------------------------------------------------

/*
Yo uso winexec("",<parametro>")
quiero recordar que cambiando el parametro a 1 o 5 no te sale, prueba cambiando waitrun con lo mismo.
*/

FUNCTION DpGetFileFtp(oFrm,cText,cDirDown,cDirFtp,lShow)
   //LOCAL cErr,cIp,cRun,oFile,nT,cBat,cOut
   LOCAL cBin    :=Lower(cFilePath(LOWER(GetModuleFileName( GetInstance() ))))
   LOCAL cFileOrg:=cBin+"DP\DPFTPGETV4.DLL"
   LOCAL cMemo   :=MEMOREAD(cFileOrg),nT,cIp
   LOCAL cFile,cRun,oFile,cErr:="",nAt
   LOCAL cFileFtp:=cBin+"dp\dpftprun.dll"
   LOCAL cBat    :=cBin+"dpftprun.bat"
   LOCAL cOut    :=cBin+"dp\dpftplog.dll"

/*
   LOCAL cUrl    :="topicodigital.com"
   LOCAL cUser   :="admintd"
   LOCAL cPass   :="topicodigital01"
*/

   LOCAL cUrl    :="topicodigital.com"
   LOCAL cUser   :="admintd"
   LOCAL cPass   :="topicodigital01"

   //LOCAL cDirFtp :="www/dpsge-v4"

   DEFAULT cDirDown:="download\"   ,;
           cDirFtp :="www/dpsge-v4",;
           oDp:lChkUrl:=.F.        ,;
           lShow      :=.F.

   cOut    :=cBin+"temp\"+strtran(time(),":","")+".tmp"

   CursorWait()

   IF !FILE(cFileOrg)
     RETURN cFileOrg+" no Existe"
   ENDIF

   cMemo:=STRTRAN(cMemo,"{URL}"      , cUrl    )
   cMemo:=STRTRAN(cMemo,"{DIR}"      , cDirFtp )
   cMemo:=STRTRAN(cMemo,"{GETFILE}"  , cText   )
   cMemo:=STRTRAN(cMemo,"{LOGIN}"    , cUser   )
   cMemo:=STRTRAN(cMemo,"{CLAVE}"    , cPass   )

 //? cMemo,"asi queda" ,cOut
 // MensajeErr("No hay Conexiï¿½n Internet o "+cMemo+" no fuï¿½ Encontrada")

    IF !oDp:lChkUrl

     EJECUTAR("DPCHKOMEMO",oFrm,"Verificando Conexiï¿½n con el Servidor")

     WsaStartUp()

     cIp := GETHOSTBYNAME(cUrl) // se supone que siempre esta activo

     IF "0.0.0"$cIp

//     MensajeErr("No hay Conexiï¿½n Internet o "+cUrl+" no fuï¿½ Encontrada")

       EJECUTAR("DPCHKOMEMO",oFrm,"No hay Conexiï¿½n Internet o "+cUrl+" no fuï¿½ Encontrada")

       RETURN "No hay Conexiï¿½n Internet o "+cUrl+" no fuï¿½ Encontrada"

      ENDIF

      EJECUTAR("DPCHKOMEMO",oFrm,"Servidor "+cIp+" Encontrado")

      oDp:lChkUrl:=.T.

   ENDIF

   SysRefresh(.t.)

   EJECUTAR("DPCHKOMEMO",oFrm,"Descargando Archivos Solicitados")

   ferase(cFileFtp)

   IF FILE(cFileFtp)
     EJECUTAR("DPCHKOMEMO",oFrm,"Arhivo estï¿½ "+cFileFtp+" en Uso")
     RETURN "Arhivo estï¿½ "+cFileFtp+" en Uso"
   ENDIF

   IF FILE(cOut)
     EJECUTAR("DPCHKOMEMO",oFrm,"Arhivo estï¿½ "+cOut+" en Uso")
     RETURN "Arhivo estï¿½ "+cOut+" en Uso"
   ENDIF


// ? "se regresa"
//
// return "se regresa"

? cFileFtp,cMemo

   DpWrite( cFileFtp  , cMemo)

/*
   oFile:=TFile():New( cFileFtp )
   oFile:PutStr(cMemo)
   oFile:Close()
*/


//   ? cFileFtp,cMemo

   cErr:=cMemo
   cErr:=STRTRAN(cErr,cUser,"*****")
   cErr:=STRTRAN(cErr,cPass,"*****")
   nAt :=AT("BINARY",UPPE(cErr))
   cErr:=IF( nAt>0 , SUBS(cErr,nAt+6,LEN(cErr)),cErr)

   IF lShow  .OR. .T.
      EJECUTAR("DPCHKOMEMO",oFrm,"Ejecutando "+cErr)
   ENDIF

   cRun:="FTP.EXE -s:"+cFileFtp+" -n > dp\dpftplog.dll"
//  cRun:="FTP.EXE -s:"+cFileFtp+" -n >"+cOut // dp\dpftplog.dll"
//  cRun:=STRTRAN(cRun,"1>","")
// ? cOut,cRun,"regresa",cOut,cFileFtp,cOut

   DpWrite( cBat ,cRun)

? cBat,cRun,file(cBat)

// return ""



//   EJECUTAR("DPCHKOMEMO",oFrm,cMemoSay+CRLF+cRun)


/*
   oFile:=TFile():New( cBat )
   oFile:PutStr(cRun)
   oFile:Close()
*/

   CursorWait()

   nT:=Seconds()

   WAITRUN(cbat,0)


   EJECUTAR("DPCHKOMEMO",oFrm,"Descargado ("+LSTR(seconds()-nT)+;
                                           ") Segundos "+ CRLF +cOut)

   cErr:=MemoRead(cOut) // "dp\dpftplog.dll")

? cErr,cOut,FILE(cOut)

   cErr:=STRTRAN(cErr,cUser,"*****")
   cErr:=STRTRAN(cErr,cPass,"*****")

   IF lShow .OR. .T.
      EJECUTAR("DPCHKOMEMO",oFrm,"Ejecutando "+cErr)
   ENDIF

/*
   ferase(cOut) // "dp\dpftplog.dll")
   dpwrite(cOut,DTOS(oDp:dFecha))
   oFile:=TFile():New( cOut ) // "dp\dpftplog.dll" )
   oFile:PutStr(DTOC(oDp:dFecha))
   oFile:Close()
*/

   SysRefresh(.t.)

RETURN cErr

/*
// Actualizaciï¿½n FTP (DataPro)
*/
FUNCTION DPRUNFTP(cMemo,cType,oFrm)
  LOCAL cUrl    :="topicodigital.com"
  LOCAL cUser   :="admintd"
  LOCAL cPass   :="topicodigital01"
  LOCAL cFileOut:="temp\dpfileOut.log"
  LOCAL cFileFtp:="temp\dpfileftp.ftp"
  LOCAL cBat    :="dpftprun.bat"
  LOCAL oFile,cRun

  DEFAULT cType:="B2C"

  oDp:lTracer:=.F.

  ferase(cFileOut)

  IF FILE(cFileOut)
     cMemo:="Archivo "+cFileOut+" estï¿½ en Uso"
     EJECUTAR("DPCHKOMEMO",oFrm,cMemo)
     RETURN cMemo
  ENDIF

  ferase(cFileFtp)

  IF FILE(cFileFtp)
     cMemo:="Archivo "+cFileFtp+" estï¿½ en Uso"
     EJECUTAR("DPCHKOMEMO",oFrm,cMemo)
     RETURN cMemo
  ENDIF

  cMemo:=STRTRAN(cMemo,"{URL}"      , cUrl    )
  cMemo:=STRTRAN(cMemo,"{LOGIN}"    , cUser   )
  cMemo:=STRTRAN(cMemo,"{CLAVE}"    , cPass   )

//  ? cMemo , cFileFtp

? cFileFtp , cMemo

  DpWrite(  cFileFtp , cMemo)

? cFileFtp, cMemo

// RETURN cMemo



/*
  oFile:=TFile():New( cFileFtp )
  oFile:PutStr(cMemo)
  oFile:Close()
*/

  cRun:="FTP.EXE -s:"+cFileFtp+" -n > "+cFileOut // ftp.log"


//? cBat,cFileFtp

/*
  oFile:=TFile():New( cBat )
  oFile:PutStr(cRun)
  oFile:Close()
*/

  DpWrite(  cBat , cRun)


// oDp:oFrameDp:SetText(TIME())
  CursorWait()
  WAITRUN(cbat,0)

//? cFileOut

  cMemo:=MemoRead(cFileOut)

  ferase(cBat)
  ferase(cFileFtp)

  oFile:=TFile():New( cFileFtp )
  oFile:PutStr(DTOC(oDp:dFecha)+"-"+TIME())
  oFile:Close()

  oFile:=TFile():New( cBat )
  oFile:PutStr(DTOC(oDp:dFecha)+"-"+TIME())
  oFile:Close()

RETURN cMemo

FUNCTION DpPutFtp(cText,cDirDown,cDirFtp,oFrm,cFtmCmd,lShow)
/*
  LOCAL cFileOrg:="DP\DPFTPPUTV4.DLL"
  LOCAL cMemo   :=MEMOREAD(cFileOrg),oFile
  LOCAL cUrl    :="topicodigital.com"
  LOCAL cUser   :="admintd"
  LOCAL cPass   :="topicodigital01"
  LOCAL cFileLog:="dp\dpftplog.dll"
  LOCAL cRun,cIp
  LOCAL cBat    :="dpftprun.bat"
  LOCAL cFileFtp:="dp\dpftprun.dll"
  LOCAL cFileLic:=lower("dp\"+oDp:cLicencia+".zip")


  DEFAULT cDirDown   :="download\",;
          cDirFtp    :="www/dpsge-v4",;
          oDp:lChkUrl:=.F.,;
          lShow      :=.T.

  oDp:lTracer:=.F.

//? cFilelic$cText,cFilelic,cText

  IF !FILE(cFileOrg)
    cMemo:="Archivo "+cFileOrg+" no Existe"
    EJECUTAR("DPCHKOMEMO",oFrm,cMemo)
    RETURN cMemo
  ENDIF

  IF !ISPCDP() .AND. !(cFilelic$cText)
     RETURN "Pc no estï¿½ Autorizado para Actualizar el Sistema"
  ENDIF

  IF !oDp:lChkUrl

    EJECUTAR("DPCHKOMEMO",oFrm,"Verificando Conexiï¿½n con el Servidor")

    WsaStartUp()

    cIp := GETHOSTBYNAME(cUrl) // se supone que siempre esta activo

    IF "0.0.0"$cIp

      cMemo:="No hay Conexiï¿½n Internet o "+cUrl+" no fuï¿½ Encontrada"
      EJECUTAR("DPCHKOMEMO",oFrm,cMemo)

      RETURN cMemo

    ENDIF

    EJECUTAR("DPCHKOMEMO",oFrm,"Conexion Ok,"+cIp+", Subiendo Archivos")

  ENDIF

  oDp:lChkUrl:=.T.

  IF Empty(cMemo)

  ENDIF

  EJECUTAR("DPCHKOMEMO",oFrm,cText)

//? "cMemo",cMemo

  cMemo:=STRTRAN(cMemo,"{URL}"      , cUrl    )
  cMemo:=STRTRAN(cMemo,"{DIR}"      , cDirFtp )
  cMemo:=STRTRAN(cMemo,"{PUTFILE}"  , cText   )
  cMemo:=STRTRAN(cMemo,"{LOGIN}"    , cUser   )
  cMemo:=STRTRAN(cMemo,"{CLAVE}"    , cPass   )

  ferase(cFileFtp)

  IF FILE(cFileFtp)
     EJECUTAR("DPCHKOMEMO",oFrm,"Archivo "+cFileFtp+" estï¿½ en Uso")
     RETURN "Archivo "+cFileFtp+" estï¿½ en Uso"
  ENDIF

  //oFile:=TFile():New( cFileFtp )
  //oFile:PutStr(cMemo)
  // oFile:Close()

  DpWrite(cFileFtp,cMemo)

 //? cFileFtp,cMemo

  ferase("dp\dpftplog.dll")
  cRun:="FTP.EXE -s:"+cFileFtp+" -n > dp\dpftplog.dll"

  //oFile:=TFile():New( cBat )
  //oFile:PutStr(cRun)
  //oFile:Close()

  DpWrite(cBat,cRun)

// ? cbat,cRun

  // Viene como @parï¿½metro
  cFtmCmd:=cMemo

// oDp:oFrameDp:SetText(TIME())
//  CursorWait()
// EJECUTAR("DPCHKOMEMO",oFrm,cRun)
//? "aqui en dpwin32"

  SysRefresh(.t.)
  CursorWait()

// ? cBat,"Corriendo",cMemo

  EJECUTAR("DPCHKOMEMO",oFrm,"Ejecutando  "+cBat)

// ? cBat,cMemo,"esto es lo que se ejecutara"

  WAITRUN(cBat,0)

// ? "listo ejecutado"


  SysRefresh(.t.)


//  oFile:=TFile():New( cBat )
//  oFile:PutStr(DTOC(oDp:dFecha)+"-"+TIME())
//  oFile:Close()




   // La aplicaciï¿½n sigue funcionando
// WinExec(cbat)

//cErr,cMemo


/*
  cMemo:=STRTRAN(cMemo,"{URL}"      , cUrl    )
//cMemo:=STRTRAN(cMemo,"{DIR}"      , cDirFtp )
//cMemo:=STRTRAN(cMemo,"{GETFILE}"  , cText   )
  cMemo:=STRTRAN(cMemo,"{LOGIN}"    , cUser   )
  cMemo:=STRTRAN(cMemo,"{CLAVE}"    , cPass   )

? cMemo

  oFile:=TFile():New( cFileFtp )
  oFile:PutStr(cMemo)
  oFile:Close()

  ferase(cFileLog)
  ferase(cFileFtp)

  IF File(cFileLog)
    MensajeErr("Archivo "+cFileLog,"Posiblemente estï¿½ abierto")
  ENDIF

  cRun:="FTP.EXE -s:"+cFileFtp+" -n > "+cFileLog

  oFile:=TFile():New( cBat )
  oFile:PutStr(cRun)
  oFile:Close()

  CursorWait()

? cBat,cRun

  WAITRUN(cBat,0)

  oFile:=TFile():New( cBat )
  oFile:PutStr(DTOC(oDp:dFecha)+"-"+TIME())
  oFile:Close()

  cMemo:=MemoRead(cFileLog)

? cMemo


   cMemo:=MemoRead(cFileLog)

 ? cMemo,"MemoRead", cFileLog

   cMemo:=STRTRAN(cMemo,cUser,"*****")
   cMemo:=STRTRAN(cMemo,cPass,"*****")

   IF lShow
     EJECUTAR("DPCHKOMEMO",oFrm,cMemo)
   ENDIF


   IF "desconectado"$Lower(cMemo) .OR. "Connection timed out"$cMemo

//   cMemo:="Servicio Desconectado"
     oDp:lChkUrl:=.F.

     EJECUTAR("DPCHKOMEMO",oFrm,cMemo)


   ENDIF

  cMemo:=DTOC(oDp:dFecha)+"-"+TIME()
  DpWrite(cBat,cMemo)

*/
RETURN NIL
// cMemo
// EOF

FUNCTION CTABLE32(cSql)
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
// ? aData[1],"CTABLE32"
  ENDIF

  cTable:=STRTRAN(cTable,"(","")

RETURN cTable




/*
   LOCAL cTable,nAt

   cSql     :=cSql+" "
   cSql     :=STRTRAN(cSql,CRLF," ")



   cTable   :=IIF( EMPTY(cTable),ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql))),cTable)
// ? cTable,"cTable"
   cTable   :=IIF(" " $cTable,LEFT(cTable,AT(" " ,cTable)),cTable)
   cTable   :=IIF(CRLF$cTable,LEFT(cTable,AT(CRLF,cTable)),cTable)

// ? cTable,"cTable"
*/
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
*/

RETURN ALLTRIM(cTable)

// Remueve la Clave de Conformaciï¿½n

FUNCTION RELEASECONF()
   LOCAL oData,aFechas:={},dFecha,I,cFile:="",cData:=""

   oData   :=DATACONFIG("CDNFRM2520","ALL")

   FOR I=1 TO 10
      cData:="" // ENCRIPT(DTOC(aFechas[I]),.T.)
      cData:=oData:Get("F"+LSTR(I),cData)

      IF !Empty(cData)
         cData:=ENCRIPT(cData,.F.)
         AADD(aFechas,CTOD(cData))
      ENDIF

   NEXT I


   FOR I=1 TO LEN(aFechas)

      IF aFechas[I]<=oDp:dFecha
         // Remuebe la Clave de Conformaciï¿½n
         SQLUPDATE("DPCONFIG",{"CON_INVDEL","CON_SERIAL"},{"",""})
         oData:Set("F"+LSTR(I),"")
         oData:Save(.T.)
      ENDIF

   NEXT

   oData:End(.F.)

RETURN NIL

STATIC FUNCTION SETDELCLAVE(nCant,nDias,lAdd)
   LOCAL oData,nVeces,aFechas:={},dFecha,I,cFile:="",cData:=""

   DEFAULT nCant:=3 ,;
           nDias:=30 ,;
           lAdd :=.T.

RETURN  .T.

   IF lAdd
      MensajeErr("Tecla de Funciï¿½n Sin Asignaciï¿½n "+LSTR(nCant)+":"+LSTR(nDias))
   ENDIF

// ? nCant,nDias,lAdd

   oData   :=DATACONFIG("CDNFRM2520","ALL")

   nVeces:=oData:Get("N255",0) // Cantidad de Veces que ha sido Conformado, en caso de ser Cero Genera tres Solicitudes de Claves cada Mes

   // Lee las Retenciones de Clave para Sumarlas
   FOR I=1 TO 10

      cData:="" // ENCRIPT(DTOC(aFechas[I]),.T.)
      cData:=oData:Get("F"+LSTR(I),cData)

      IF !Empty(cData)
         cData:=ENCRIPT(cData,.F.)
         AADD(aFechas,CTOD(cData))
      ENDIF

   NEXT I

   // Si no seran Agregados mas Remociones se Devuelve
   IF !lAdd .AND. !Empty(aFechas)
      RETURN .F.
   ENDIF

   IF nVeces=0

      dFecha:=oDp:dFecha+nDias

      FOR I=1 TO 3

         IF ASCAN(aFechas,dFecha)=0
            AADD(aFechas,dFecha)
         ENDIF

         dFecha:=dFecha+nDias

      NEXT I

   ENDIF

   FOR I=1 TO LEN(aFechas)
      cData:=ENCRIPT(DTOC(aFechas[I]),.T.)
      oData:Set("F"+LSTR(I),cData,.T.)
   NEXT I

   oData:Save(.T.)

RETURN .T.

// EOF

FUNCTION DPHORA()
   LOCAL cTime:=TIME()

   DEFAULT oDp:lDateSrv:=.F.

   IF oDp:lDateSrv
      cTime:=EJECUTAR("DPTIMESRV")
   ENDIF

RETURN cTime

FUNCTION DPFECHA(lServer)
   LOCAL dFecha:=oDp:dFecha

   DEFAULT lServer:=.F.

   DEFAULT oDp:lDateSrv:=.F.

   IF oDp:lDateSrv .OR. lServer
      dFecha:=EJECUTAR("DPFECHASRV")
   ENDIF

   // Fallï¿½ DPFECHASRV
   IF !ValType(dFecha)="D"
      dFecha:=DATE()
   ENDIF

RETURN dFecha

FUNCTION DPFECHABD()
  EJECUTAR("DPFECHASRV")
RETURN oDp:cFechaDB

FUNCTION DPHORABD()
  EJECUTAR("DPFECHASRV")
RETURN oDp:cTimeDB

FUNCTION SINPUNTERO(cField)
   LOCAL nAt

   cField:=ALLTRIM(cField)
   nAt   :=AT(".",cField)

   IF nAt>0
      cField:=SUBS(cField,nAt+1,LEN(cField))
   ENDIF

RETURN cField

FUNCTION DPBUILDPICTURE(nInt,nDec)
 LOCAL cPicture,n3

    DEFAULT nDec:=0

    n3      :=DIV(nInt,3)
    cPicture:=REPLI(",999",n3)+;
              IIF(nDec=0,"","."+REPLI("9",nDec))

    IF LEFT(cPicture,1)=","
      cPicture:="9"+cPicture
    ENDIF

RETURN cPicture

   FUNCTION OPE_CARGO(cCodigo)
RETURN OPE_NOMBRE(cCodigo,"OPE_CARGO")

FUNCTION OPE_CLAVE(cCodigo)
RETURN OPE_NOMBRE(cCodigo,"OPE_CLAVE")

FUNCTION OPE_NOMBRE(cCodigo,cField)
  LOCAL cNombre:=""

  DEFAULT cCodigo:=oDp:cUsuario,;
          cField :="OPE_NOMBRE"

  cNombre:=SQLGET("DPUSUARIOS",cField+",OPE_ENCRIP","OPE_NUMERO"+GetWhere("=",cCodigo))

  // Convierte en Logico
  IF !Empty(oDp:aRow)
     oDp:aRow[2]:=CTOO(oDp:aRow[2],"L")
  ENDIF

  IF !Empty(oDp:aRow) .AND. oDp:aRow[2]
     cNombre:=ENCRIPT(cNombre,.F.)
  ENDIF

RETURN cNombre

//FUNCTION IFLICCONCLUIDA()
//  MensajeErr(GETPROCE())
//RETURN .T.

FUNCTION ISLICCONCLUIDA(lSay)
 LOCAL nDias :=oDp:dFchFinLic-oDp:dFecha
 LOCAL dPlazo:=FCHFINMES(oDp:dFchFinLic+89)

 DEFAULT lSay:=.T.

// ? "aqui es licocnlucida?"

 IF MACROEJE("ISPCPRG()")
    RETURN .T.
 ENDIF

// RETURN .F.

 IF nDias<0

    nDias:=ABS(dPlazo-oDp:dFecha)

    IF lSay

      MensajeErr("Muchas Gracias por Utilizar: "+oDp:cDpSys+CRLF+;
                 "Licencia : "+oDp:cLicencia+" Concluida el "+DTOC(oDp:dFchFinLic)+CRLF+;
                 "Plazo para Renovar "+DTOC(dPlazo)+", Dispone de "+LSTR(nDias)+" Días"+CRLF+;
                 "Solo podrá utilizar las opciones de consultas "+CRLF+;
                 "Contáctenos en www.adaptaproerp.com, ventas@adaptaproerp.com")

      EJECUTAR("WEBRUN","www.adaptaproerp.com")

    ENDIF

    RETURN .F.

 ENDIF

RETURN .T.

/*
// Solo para Version 5.X
FUNCTION HRBISDPXBASE(cNumLicencia)
  LOCAL nVersion ,nFinancia,dFchCrea,nAnoslic

  LOCAL lDpXbase,nAt ,nAdd
  LOCAL aMeses :={"E","F","M","A","Y","J","L","G","S","O","N","D"}
  LOCAL nMes   :=STRZERO(MAX(ASCAN(aMeses,SUBS(cNumLicencia,9,1)),1),2)
  LOCAL nAno   :=SUBS(cNumLicencia,7,2)
  LOCAL dFchIni:=CTOD("01/"+nMes+"/"+"20"+nAno)
  LOCAL nAnos  :=VAL(SUBS(cNumLicencia,10,1))
  LOCAL dFchFin:=FchFinMes(dFchIni+(nAnos*365))

  DEFAULT cNumLicencia:=""

  oDp:dFchIniLic:=dFchIni
  oDp:dFchFinLic:=dFchFin
  oDp:dAnosLic  :=nAnos

// ? oDp:dFchIniLic,oDp:dFchFinLic,oDp:dAnosLic


  nAdd:=IF( LEN(ALLTRIM(cNumLicencia))=23 , 1 , 0 )

  oDp:cVersion:=ATAIL(_VECTOR(STRTRAN(oDp:cDpSys," ",",")))
  oDp:nVersion:=VAL(oDp:cVersion)
  nVersion    :=oDp:nVersion

  DEFAULT cNumLicencia:=""

   IF oDp:lDemo
//     cNumLicencia:="0050036N3006AGL00160"
   ENDIF

   IF RIGHT(cNumLicencia,5)="00000" .AND. ValType(oDp:oFrameDp)="O" .AND. !("Demo"$oDp:oFrameDp:cTitle)
       oDp:oFrameDp:SetText(oDp:oFrameDp:cTitle+" [Demostraciï¿½n]")
   ENDIF

   lDpXbase:=SUBS(cNumLicencia,13+nAdd,1)="A" //.AND. !(DPNIVLIC()$"12")

// UBS(cNumLicencia,13+nAdd,1),cNumLicencia

   aMeses:={"E","F","M","A","Y","J","L","G","S","O","N","D"}

   nAt         :=STRZERO(MAX(ASCAN(aMeses,SUBS(cNumLicencia,8,1)),1),2)
   nFinancia   :=VAL(SUBS(cNumLicencia,10+nAdd,2))// +20
   dFchCrea    :=FCHFINMES("01/"+nAt+"/200"+SUBS(cNumLicencia,7,1))
   nAnosLic    :=VAL(SUBS(cNumLicencia,9,1))

   oDp:nAnosLic:=nAnosLic
   oDp:dFchCrea:=dFchCrea

   // ? dFchCrea,nAt
   //? lDpXBase,"ISDPXBASE()"
   // LOCAL lDpXBase:=SUBS(
   // lDpXbase:=.t.
   // lDpXbase:=.T. // QUITAR
    IF oDp:lDemo
//       lDpXBase:=.F.
    ENDIF

// ?  oDp:lIsDpXbase

   IF !oDp:lIsDpXbase
       oDp:lDpXbase:=.F.
       lDpXbase:=.F.
       RETURN .F.
   ENDIF

RETURN lDpXBase
*/
/*
// A cada Char le sumara 5 caracteres
*/


FUNCTION SetMsgLicencia()

  oDp:oFrameDp:oMsgBar:SetMsg("Licencia "+oDp:cLicencia+" "+oDp:cTitular )
  oDp:oFrameDp:oMsgBar:cMsgDef:=oDp:oFrameDp:oMsgBar:cMsg
  oDp:oFrameDp:oMsgBar:ShowMsgBar()

RETURN NIL
/*
// Determina si todos los Digitos son Alfabeticos
*/
FUNCTION ISALLALPHA(cClave)
  LOCAL lResp:=.T.,c,I

  FOR I=1 TO LEN(cClave)
     c:=SUBS(cClave,I,1)
     IF !ISALPHA(c)
        lResp:=.F.
        EXIT
     ENDIF
  NEXT I

RETURN lResp

// Utilizado en la Prueba de TESTAGET

FUNCTION GenLocalBlock( aGets, n )
return bSETGET( aGets[ n ] )

FUNCTION ENCRIPT2(cText,lEnc,nIni)
  LOCAL ctext2:="",cChar,I,nAt

  DEFAULT cText:="ADAPTAPRO",;
          lEnc :=.T.,;
          nIni :=5

  IF !lEnc
     nIni :=VAL(ENCRIPT(LEFT(cText,2),.F.))
     cText:=SUBS(cText,3,LEN(cText))
  ENDIF

  FOR I=1 TO LEN(cText)
    cChar:=SUBS(cText,I,1)
    cChar:=CHR(ASC(cChar)+IF(lEnc,nIni,-nIni))
    cText2:=cText2+cChar
  NEXT I

  IF lEnc
    cText2:=ENCRIPT(STRZERO(nIni,2),.T.)+cText2
  ENDIF

RETURN cText2

/*
// JN  12/08/2014
*/
FUNCTION DPTRUNCATE(nValor,nDec)

   DEFAULT nDec:=2

   nDec:=VAL("1"+REPLI("0",nDec))
   nValor:=INT(nValor*nDec)/100

RETURN nValor

FUNCTION ISVERSION4()
RETURN oDp:nVersion>=4

/*
// Devuelve el Valor de oDp:aRow
*/
FUNCTION DPSQLROW(nCol,uDef)
   LOCAL cType  :=VALTYPE(uDef)
   LOCAL uDefOrg:=uDef

   IF !Empty(oDp:aRow)
      uDef:=IIF(nCol>0 .AND. nCol<=LEN(oDp:aRow),oDp:aRow[nCol],uDef)
   ELSE
      uDef:=IIF(nCol>0 .AND. nCol<=LEN(oDp:aRowEmpty),oDp:aRowEmpty[nCol],uDef)
   ENDIF

   IF ValType(uDef)="L"
      RETURN uDef
   ENDIF

   // Tipo de Datos es Fecha
   IF uDefOrg="D"
      cType:="DATE"
   ENDIF

   IF uDef<>NIL .AND. !ValType(uDef)=cType
      uDef:=CTOO(uDef,cType)
   ENDIF

RETURN uDef


/*
// Incremental, Compatibilidad 5.1
*/

FUNCTION DPXGETPROCE(lCrear)

  DEFAULT lCrear:=.F.

  IF lCrear

     oDp:lLines:=.T.

     IF ValType(oDp:oRunLine)="O"
       oDp:cDpxOld:=ALLTRIM(oDp:oRunLine:oScript:cProgram)+":"+oDp:oRunLine:oFunction:cName+"("+oDp:oRunLine:cLine+")"
     ENDIF

     oDp:cDpXbaseTraza:=""

     RETURN CTOO(oDp:cDpXbaseTraza,"C")

  ELSE

     // Desactiva
     oDp:lLines:=.T.
     oDp:cDpXbaseTraza:=""

  ENDIF

  DEFAULT oDp:cDpxOld:=""

RETURN CTOO(oDp:cDpxOld,"C")+CRLF+CTOO(oDp:cDpXbaseTraza,"C")


// Algunos programas DpXBase lo utilizan en Version 5.1
FUNCTION DPSQLGETMAX(cTabla,cField,cWhere,nLimit,lMax)
RETURN SQLGETMAX(cTabla,cField,cWhere,nLimit,lMax)

/*
// Incremental, Compatibilidad 5.1
*/

FUNCTION DPSQLINCREMENTAL(cTable,cField,cWhere,oOdbc,cMax,lZero)
RETURN SQLINCREMENTAL(cTable,cField,cWhere,oOdbc,cMax,lZero)

/*
// Determina si x registro existe
*/
FUNCTION ISSQLFIND(cTable,cWhere,oDb)
RETURN COUNT(cTable,cWhere,oDb)>0



FUNCTION DPINCREMENTAL32(cNumero,lZero)
RETURN DPINCREMENTAL(cNumero,lZero)

FUNCTION DPRUNMULTI(cProgra,cVar,uPar1,uPar2,uPar3,uPar4,uPar5,uPar6,uPar7,uPar8)
//PROCE MAIN(lAuto,cTipPag,cCodSuc,cCodPro,cRecord,lView)
    LOCAL cMain,oFrm,nContar:=0,lOk:=.F.,oPago,nAt
    LOCAL cMemo
    LOCAL cFile:="dpxbase\dpcbtepagox.dxb"

    DEFAULT oDp:aContarVar:={}

    DEFAULT cProgra:="DPCBTEPAGO",;
            cVar   :="oProPagX"

    nAt    :=ASCAN(oDp:aContarVar,{|a,n| a[1]==cProgra})

    IF nAt=0
       AADD(oDp:aContarVar,{cProgra,nContar})
       nAt:=LEN(oDp:aContarVar)
    ENDIF

    nContar:=oDp:aContarVar[nAt,2]

    cMain:=cVar

    WHILE nContar<20 //.AND. nContar<5

       // nContar:=MAX(nContar,0)

       cVar :=cMain+IIF(Empty(nContar),"",LSTR(nContar))

       nContar++

       IF TYPE(cVar)="O"

          oFrm:=&cVar.

          IF ValType(oFrm)="O" .AND. ValType(oFrm:oWnd)="O" .AND. oFrm:oWnd:hWnd>0

             nContar:=nContar+1

             cVar:=cMain+LSTR(nContar)

          ELSE

             EXIT

          ENDIF

       ELSE

         EXIT

       ENDIF

    ENDDO

    IF nContar=0

       oPago:=EJECUTAR(cProgra,uPar1,uPar2,uPar3,uPar4,uPar5,uPar6,uPar7,uPar8)
//lAuto,cTipPag,cCodSuc,cCodPro,cRecord,lView)

    ELSE

       IF DPSCRIPTRUN(cProgra+LSTR(nContar))=0

          cMemo:=MemoRead(cFile)
          cMemo:=STRTRAN(cMemo,cVar,cVar+LSTR(nContar))
          cMemo:=STRTRAN(cMemo,Lower(cVar),cVar+LSTR(nContar))

          cFile:=cProgra+LSTR(nContar)

          dpwrite("dpxbase\"+cFile+".dxb",cMemo)

       ENDIF

       cFile:=cProgra+LSTR(nContar)

       oPago:=EJECUTAR(UPPE(cFile),uPar1,uPar2,uPar3,uPar4,uPar5,uPar6,uPar7,uPar8)

       FERASE("dpxbase\"+cFile+".dxb")

    ENDIF

    oDp:aContarVar[nAt,2]:=nContar

RETURN oPago


FUNCTION SQLUPDATE_JOIN(cTable,cInner,cField,cUpdate,cWhere,lView,oOdbc)
   LOCAL oTable,lRet:=.T.,cSql ,I,cTableO:=cTable
   LOCAL nT1 ,lTracer:=oDp:lTracer

   DEFAULT cWhere:=""

   IF Empty(cField)
     MensajeErr("Requiere parï¿½metro cField, SQLUPDATEINNER(cTable,cInner,cField,cUpdate,cWhere,lView,oDb) ")
     RETURN .F.
   ENDIF

   IF Empty(cInner)
     MensajeErr("Requiere parï¿½metro cInner, SQLUPDATEINNER(cTable,cInner,cField,cUpdate,cWhere,lView,oDb) ")
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

   cSql:="UPDATE "+cTableO+;
         " "+cInner+" "+;
         " SET "

   FOR I=1 TO LEN(cUpdate)
      cSql:=cSql + IIF(I>1 , "," , "" ) +;
            cField[I]+GetWhere("=",cUpdate[I])
   NEXT I

   cSql:=cSql + " "+cWhere

   oDp:cSql:=cSql

   DEFAULT oOdbc   :=GETODBC(cTable)

   nT1:=SECONDS()
   oOdbc:Execute(oDp:cSql)

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
// DataSet para DpConfig
*/
FUNCTION DATACNF(cGroup,cMode,lLoad,cTable,cUser,cScope,cIp)
RETURN DATASET(cGroup,cMode,lLoad,"DPDATACNF",cUser,cScope,cIp)


/*
// Valida los Permisos del Usuario
*/
FUNCTION ACCESSFIELD32(cTable,cField,nOption)

  LOCAL nAt,lResp:=.T.,cRun

  DEFAULT cTable :="DPUSUARIOS",;
          cField :="OPE_NOMBRE",;
          nOption:=3

  // Si el formulario es Cero, Ningun Campo tiene Opciï¿½n para Editar

  // buscar
  IF nOption>3
     RETURN .T.
  ENDIF

  IF nOption=0 .OR. nOption=2 .OR. ValType(cTable)<>"C" .OR. ValType(cField)<>"C" // No tiene acceso a la Consulta
     RETURN .F.
  ENDIF

  cTable :=ALLTRIM(cTable)
  cField :=ALLTRIM(cField)
  nOption:=IIF(nOption=0,2,nOption)
  nAt    :=ASCAN(oDp:aMapaCampos,{|a,n| a[1]==cTable .AND. a[2]==cField})

//? nAt,cTable,cField,"ACCESSFIELD"
  IF nAt=0 .OR. nOption=4
     RETURN .T.
  ENDIF

  lResp:=!(oDp:aMapaCampos[nAt,2+nOption])

RETURN lResp

/*
// Obtiene Texto de La Etiquetda
*/

FUNCTION LEVELFIELD(cTable,cField)
  LOCAL aLevel,cText:="",nAt

  DEFAULT cTable:="DPUSUARIOS",;
          cField:="OPE_NOMBRE",;
          oDp:aLevel:={}

  aLevel:=EJECUTAR("DPGETLEVELTABLE",cTable)

  nAt:=ASCAN(aLevel,{|a,n| cField==a[1] })

  IF nAt>0
     RETURN aLevel[nAt,2]
  ENDIF

RETURN cText

FUNCTION RUNSTD(cProgra,p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)
   LOCAL oScript:=NIL
   LOCAL cMemo  :="" // GETFILESTD("DPXBASE\"+ALLTRIM(cProgra)+".DXB")
   LOCAL uValue,cFunction,nAt

   DEFAULT oDp:aRunStd:={}

   cProgra:=ALLTRIM(UPPE(cProgra))

   nAt:=ASCAN(oDp:aRunStd,{|a,n|a[1]==cProgra})

   IF nAt=0

     cMemo  :=GETFILESTD("DPXBASE\"+cProgra+".DXB",.T.,.T.)

     IF Empty(cMemo)
       MensajeErr("Programa "+cProgra+" no encontrado en DPSTD\dpdxb.dxbx")
       RETURN NIL
     ENDIF

     oScript:=TScript():New("")

     oScript:cProgram:=uppe(ALLTRIM(cProgra))
     oScript:Reset()
     oScript:lPreProcess := .T.
     oScript:LoadPCode(cMemo+CRLF)

     AADD(oDp:aRunStd,{cProgra,oScript})

   ELSE

     oScript:=oDp:aRunStd[nAt,2]

   ENDIF

   uValue:=oScript:Run(NIL, @p1, @p2, @p3, @p4, @p5, @p6, @p7, @p8, @p9, @p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)

   //oScript:End()

RETURN uValue

/*
// Remover ceros del Lado Izquierdo
*/
FUNCTION SIN_CEROS(cCero)

   cCero:=ALLTRIM(cCero)

   WHILE LEFT(cCero,1)="0"
       cCero:=SUBS(cCero,2,LEN(cCero))
   ENDDO

RETURN cCero

FUNCTION URLDownLoad(cUrl, cSaveAs)

// oDp:hLibUrl= LOADLIBRARY( "   urlmon.dll")
// ? oDp:hLibUrl,ValType(oDp:hLibUrl),"aqui es"

if !URLDownloadToFile( 0, cUrl, cSaveAs, 0, 0 ) == 0
   retu.f.
endif

// FREELIBRARY( oDp:hLibUrl )

RETURN .T.

/*
DLL32 FUNCTION URLDownloadToFilellave(pCaller AS LONG,szURL AS STRING, szFileName AS STRING, dwReserved AS LONG, lpfnCB AS LONG);
AS LONG PASCAL;
FROM "URLDownloadToFileA";
LIB oDp:hlibUrl
*/

DLL32 FUNCTION URLDownloadToFile(pCaller AS LONG,szURL AS STRING, szFileName AS STRING, dwReserved AS LONG, lpfnCB AS LONG);
AS LONG PASCAL;
FROM "URLDownloadToFileA";
LIB "urlmon.dll"


DLL32 FUNCTION wCamGetDrvDesc2;
       (nDriver AS _INT,;
        cName AS STRING,;
        nName AS LONG,;
        cVersion AS STRING,;
        nVersion AS LONG) AS BOOL PASCAL;
FROM "capGetDriverDescriptionA";
LIB "avicap32.dll"


/*
// Crea Registros con sus respectivos Vinculos
*/
FUNCTION CREATERECORD(cTable,aFields,aValues,aLink,lCommit)
   LOCAL oTable,cWhere:="",I

   DEFAULT cTable :="DPINV",;
           aFields:={"INV_CODIGO"},;
           aValues:={"INDEFINIDO"},;
           lCommit:=.F.

RETURN RUNNEW("INSERTRECORD",cTable,aFields,aValues)
/*
   IF ValType(aFields)="C"
      aFields:={aFields}
   ENDIF

   IF ValType(aValues)="C"
      aValues:={aValues}
   ENDIF

   // Primero debe crear los Registros no Incluidos

   DEFAULT aLink:=RUNNEW("CREATERECORDLNK",cTable,ACLONE(aFields),ACLONE(aValues))
   // DEFAULT aLink:=CREATERECORDLNK(cTable,ACLONE(aFields),ACLONE(aValues))

   FOR I=1 TO LEN(aLink)

     IF !Empty(aLink[I,3])
       CREATERECORD(aLink[I,3],aLink[I,4],aLink[I,2],NIL,.T.) // Debe Guardar todos los Vinculos
     ENDIF

   NEXT I

   IF !lCommit
      RETURN .T.
   ENDIF

//? "AQUI GUARDA ",cTable

RETURN  CREATERECORDMAIN(cTable,aFields,aValues,ACLONE(aLink),lCommit) // Llama CREATERECORDMAIN
// EJECUTAR("CREATERECORD",cTable,aFields,aValues,ACLONE(aLink),lCommit) // Llama CREATERECORDMAIN
*/

FUNCTION GETLBXDB()
   LOCAL oDpLbx:=GetDpLbx(oDp:nNumLbx)

   IF ValType(oDpLbx)="O"
      RETURN oDpLbx:oDb
   ENDIF

RETURN NIL

/*
FUNCTION DPXRUN(cProgram ,p1 , p2 , p3 , p4 , p5 , p6, p7, p8, p9, p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)
   LOCAL cFilePpo  :=oDp:cPathDxb+ALLTRIM(cProgram)+".DXB"
   LOCAL cFileScr  :="SCRIPT\"+ALLTRIM(cProgram)+".SCR"
   LOCAL oScript,cFunction,lRet

   cProgram:=ALLTRIM(UPPE(cProgram))

   oScript:=TScript():New("")
   oScript:cProgram:=uppe(ALLTRIM(cProgram))
   oScript:Reset()
   oScript:lPreProcess := .T.

   oScript:LoadPCode(cFilePpo)

   IF oDp:lTracerIni

     MsgRun("Programa "+cProgram,"Ejecutando desde ",{||;
     lRet:=oScript:Run(cFunction, @p1, @p2, @p3, @p4, @p5, @p6, @p7, @p8, @p9, @p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20);
     })

   ELSE

     lRet:=oScript:Run(cFunction, @p1, @p2, @p3, @p4, @p5, @p6, @p7, @p8, @p9, @p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)

   ENDIF

   IF ValType(oScript)="O" .AND. !EMPTY(oScript:cError)
      oScript:MsgError(cProgram,cFunction)
      oDp:cScriptErr:=oScript:cError // Error Script
      lRet:=.F.
   ENDIF

   oScript:End()

RETURN lRet
*/


/*
// Verifica en la Tabla si Puede Modificar
*/
FUNCTION SETTABINC(cTable,lResp,nCol)
   LOCAL nAt

   DEFAULT lResp:=.F.,;
           nCol:=2

   nAt   :=ASCAN(oDp:aMapaTab,{|a|a[1]==UPPE(cTable)})

   IF nAt>0
      oDp:aMapaTab[nAt,nCol]:=lResp
   ENDIF

RETURN lResp

FUNCTION SETTABCON(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,3)

FUNCTION SETTABMOD(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,4)

FUNCTION SETTABELI(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,5)

FUNCTION SETTABPRN(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,6)

// Si Registra Auditorï¿½a
FUNCTION SETTABAUD(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,7)

// Si Consulta BRW
FUNCTION SETTABQRY(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,8)

// Si Consulta BRW
FUNCTION SETTABMNU(cTabla,lResp)
RETURN SETTABINC(cTabla,lResp,9)


FUNCTION IMPORTDPTABLAS(lMain)
RETURN EJECUTAR("IMPORTDPTABLAS",lMain)
//    ? "EXPORTADO HACIA DPXBASE IMPORTDPTABLAS"
/*
    LOCAL aTablas:={} ,I,cDo
    CLOSE ALL

    USE "DATADBF\DPTABLAS.DBF" NEW SHARED VIA "DBFCDX"

    // Determinan Diccinario de Datos y Configuracion Versiï¿½n 5.
    SET FILTER TO (("-"$FIELD->TAB_DSN .OR. "."$FIELD->TAB_DSN) .OR. FIELD->TAB_CONFIG)
    DBGOTOP()

// ? "AQUI ES IMPORTDPTABLAS"
// BROWSE()
// GO TOP

    DBEVAL({||AADD(aTablas,{FIELD->TAB_NUMERO,ALLTRIM(FIELD->TAB_NOMBRE)+".DBF"}) })
    USE

    // Primero debe Importar a Campos
    I :=ASCAN(aTablas,{|a,n|cFileNoExt(a[2])="DPCAMPOS"})

    oDp:aLogico    :={} // Evita que sea Leido por TMYTABLE

    IF I>0
      RunMacro([IMPORTDBF32("]+aTablas[I,1]+[","DATADBF\]+aTablas[I,2]+[",oDp:cDsnConfig,oDp:oSay,.T.,.T.)])
    ENDIF

    oDp:aLogico    :=NIL // Ahora si Puede leerlo

    I :=ASCAN(aTablas,{|a,n|cFileNoExt(a[2])="DPTABLAS"})

    IF I>0
      RunMacro([IMPORTDBF32("]+aTablas[I,1]+[","DATADBF\]+aTablas[I,2]+[",oDp:cDsnConfig,oDp:oSay,.T.,.T.)])
    ENDIF

    FOR I := 1 TO LEN(aTablas)

      IF !oDp:oMeter=NIL
        oDp:oMeter:Set(I)
        oDp:oSay:SetText("Evaluando Tabla: "+cFileNoExt(aTablaS[I,2]))
      ENDIF

      cDo:=[IMPORTDBF32("]+aTablas[I,1]+[","DATADBF\]+aTablas[I,2]+[",oDp:cDsnConfig,oDp:oSay,.T.,.T.)]
      cDo:=BLOQUECOD(cDo)
      EVAL(cDo)

    NEXT I

    SQLUPDATE("DPTABLAS","TAB_VISTA",.F.,"LEFT(TAB_NOMBRE,5)"+GetWhere("<>","VIEW_"))
*/

RETURN NIL

/*
// Valida Acceso a Cada Campo
*/

DLL32 FUNCTION URLDownloadToFilellave(pCaller AS LONG,szURL AS STRING, szFileName AS STRING, dwReserved AS LONG, lpfnCB AS LONG);
AS LONG PASCAL;
FROM "URLDownloadToFileA";
LIB oDp:hlibUrl

DLL32 FUNCTION RSProcess(npID AS LONG , nMode AS LONG ) ;
AS LONG FROM "RegisterServiceProcess" LIB "kernel32.DLL"

DLL32 FUNCTION GCP() AS LONG FROM "GetCurrentProcessId" LIB "kernel32.dll"


// FUNCTION GETRUNSCRIPT()

FUNCTION ADSCREATEDSN(aData)
  LOCAL oReg,cFileDrive,cType,oDsn,cKey
  LOCAL cDSN,cDescri,cDataBase,cServer,cUser,nDsnType,cDriver,nPort,cPass,cName

  nDsnType    :=TDSN_USER_DSN

  cDsn        :=ALLTRIM(aData[01])
  cUser       :=ALLTRIM(aData[02])
  cPass       :=ALLTRIM(aData[03])
  cServer     :=ALLTRIM(aData[04])
  cDataBase   :=ALLTRIM(aData[05])
  cDriver     :=ALLTRIM(aData[06])
  cDescri     :=ALLTRIM(aData[07])
  nPort       :=aData[08]
  cName       :=ALLTRIM(aData[09])

  cServer:=cServer+"\"+cDataBase

  // cDriver:=GetWinDir()+"\System32\"+cDriver

  cKey := "SOFTWARE\ODBC\ODBC.INI"+"\"+cDSN

  // Registro DSN de Usuario
  oReg := TReg32():Create(2147483649, cKey) // HKEY_CURRENT_USER, cKey )
  oReg:Set( "Database"          ,  cDataBase   )       // Nombre de la base de Datos
  oReg:Set( "CharSet"           ,  "ANSI"      )    // Tipo de Datos
  oReg:Set( "Compression"       ,  "Always"    )    // Compresion
  oReg:Set( "DataDictionary"    ,  "False"     )    // Usuario/Login de acceso
  oReg:Set( "DataDirectory"     ,  cServer     )    //"C:\DPWIN32\CONFIG\"  ) //cPath    )    // Clave de Acceso
  oReg:Set( "DefaultType"       ,  "FoxPro"    )    // Direcciï¿½n IP o HostName del Servidor
  oReg:Set( "Description"       ,  cDescri     )
  oReg:Set( "Driver"            ,  cDriver     ) // GetWinDir()+"\System32\adsodbc.dll" )    // Driver
  oReg:Set( "Locking"           ,  "Record"    )    // Option 3 Requerido
  oReg:Set( "MaxTableCloseCache",  "5"         )       // MaxTableCloseCache
  oReg:Set( "MemBlockSize"      ,  "64"        )       // MaxTableCloseCache
  oReg:Set( "Rows"              ,  "False"     )       // MaxTableCloseCache
  oReg:Set( "ServerTypes"       ,  "1"         )       // Local Server
  oReg:Set( "TrimTrailingSpaces", "False"      )
  oReg:Close()


  // Registro DSN de Usuario
  oReg := TReg32():Create(2147483651 , cKey) // HKEY_USER, cKey )
  oReg:Set( "Database"         ,    cDataBase )       // Nombre de la base de Datos
  oReg:Set( "CharSet"           ,  "ANSI"    )    // Tipo de Datos
  oReg:Set( "Compression"       ,  "Always" )    // Compresion
  oReg:Set( "DataDictionary"    ,  "False"  )    // Usuario/Login de acceso
  oReg:Set( "DataDirectory"     ,  cServer  )    // "C:\DPWIN32\CONFIG\"  ) //cPath    )    // Clave de Acceso
  oReg:Set( "DefaultType"       ,  "FoxPro" )    // Direcciï¿½n IP o HostName del Servidor
  oReg:Set( "Description"       ,  cDescri  )
  oReg:Set( "Driver"            ,  cDriver  ) // GetWinDir()+"\System32\adsodbc.dll" )    // Driver
  oReg:Set( "Locking"           ,  "Record" )    // Option 3 Requerido
  oReg:Set( "MaxTableCloseCache",  "5"      )       // MaxTableCloseCache
  oReg:Set( "MemBlockSize"      ,  "64"     )       // MaxTableCloseCache
  oReg:Set( "Rows"              ,  "False"  )       // MaxTableCloseCache
  oReg:Set( "ServerTypes"       ,  "1"      )       // Local Server
  oReg:Set( "TrimTrailingSpaces", "False"  )
  oReg:Close()

  // Ahora la lista del DSN

  cKey :="SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources"

  oReg := TReg32():Create(2147483649, cKey) //   HKEY_CURRENT_USER, cKey )
  oReg:Set(cDsn, cName , cDriver)
  oReg:Close()

  PUTODBCINI_(cDsn,cDriver,cName)

RETURN NIL


FUNCTION GETWHEREIN(cField,aValues)
 LOCAL cWhere:=""

 DEFAULT cField:="MOC_CODSUC",;
         aValues:={"00001","00002","00003"}

 AEVAL(aValues,{|a,n| cWhere:=cWhere+IF( Empty(cWhere),"", ",")+GetWhere("",a)})

 cWhere:=cField+" IN ("+cWhere+")"

RETURN cWhere


FUNCTION GETWHEREINNOT(cField,aValues)
 LOCAL cWhere:=""

 DEFAULT cField:="MOC_CODSUC",;
         aValues:={"00001","00002","00003"}

 AEVAL(aValues,{|a,n| cWhere:=cWhere+IF( Empty(cWhere),"", ",")+GetWhere("",a)})

 cWhere:=cField+" IN NOT ("+cWhere+")"

RETURN cWhere

/*
// Crear DSN FoxPro
*/
FUNCTION FOXCREATEDSN(aData)
  LOCAL oReg,cFileDrive,cType,oDsn,cKey
  LOCAL cDSN,cDescri,cDataBase,cServer,cUser,nDsnType,cDriver,nPort,cPass,cName

  nDsnType    :=TDSN_USER_DSN

  cDsn        :=ALLTRIM(aData[01])
  cUser       :=ALLTRIM(aData[02])
  cPass       :=ALLTRIM(aData[03])
  cDataBase   :=ALLTRIM(aData[05])
  cDriver     :=ALLTRIM(aData[06])
  cDescri     :=ALLTRIM(aData[07])
  nPort       :=aData[08]
  cName       :=ALLTRIM(aData[09])

  cKey := "SOFTWARE\ODBC\ODBC.INI"+"\"+cDSN

  // Registro DSN de Usuario
  oReg := TReg32():Create(2147483649, cKey) // HKEY_CURRENT_USER, cKey )

  oReg:Set("BackgroundFetch"   , "Yes"       )
  oReg:Set("Collate"           , "Machine"   )
  oReg:Set("Description"       ,  cDescri    )
  oReg:Set("Driver"            ,  cDriver    ) // GetWinDir()+"\System32\adsodbc.dll" )    // Driver
  oReg:Set("Exclusive"         , "No"        )
  oReg:Set("SetNoCountOn"      , "No"        )
  oReg:Set("SourceDB"          ,  cDataBase  )       // Nombre de la base de Datos
  oReg:Set("SourceType"        , "DBF"       )
  oReg:Close()

  // Registro DSN de Usuario
  oReg := TReg32():Create(2147483651 , cKey) // HKEY_USER, cKey )
  oReg:Set("BackgroundFetch"   , "Yes"       )
  oReg:Set("Collate"           , "Machine"   )
  oReg:Set("Description"       ,  cDescri    )
  oReg:Set("Driver"            ,  cDriver    ) // GetWinDir()+"\System32\adsodbc.dll" )    // Driver
  oReg:Set("Exclusive"         , "No"        )
  oReg:Set("SetNoCountOn"      , "No"        )
  oReg:Set("SourceDB"          , cDataBase   )       // Nombre de la base de Datos
  oReg:Set("SourceType"        , "DBF"       )
  oReg:Close()

  // Ahora la lista del DSN

  cKey :="SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources"

  oReg := TReg32():Create(2147483649, cKey) //   HKEY_CURRENT_USER, cKey )
  oReg:Set(cDsn, cName , cDriver)
  oReg:Close()

  PUTODBCINI_(cDsn,cDriver,cName)

RETURN NIL

/*
// Actualiza Registro WINDOWS\ODBC.INI
// Necesario para la Funciï¿½n OdbcDsnEntries()
*/
STATIC FUNCTION PUTODBCINI_(cDsn,cDriver,cName)
  LOCAL oIni,cIniFile:=GetWinDir()+"\ODBC.INI",I
  LOCAL cDrive

  // Lista de DSN
  oIni:=Tini():New( cIniFile )
  oIni:Set([ODBC 32 bit Data Sources],cDsn,cName) // Data)
  oIni:Set(cDsn,"Driver32",ALLTRIM(cDriver))
  oIni:=nil

  // Lista de Driver
  cIniFile:=GetWinDir()+"\ODBCINST.INI"

  oIni   :=Tini():New( cIniFile )
  oIni:Set([ODBC 32 bit Drivers],cName,"Installed")
  oIni:Set(cName)
  oIni:Set(cName,"Driver",cDriver)
  oIni:Set(cName,"Setup" ,cDriver )
  oIni:Set(cName,"32Bit" ,"1"    )

RETURN NIL


// Programa   : JOINTABLE
// Fecha/Hora : 02/08/2016 06:57:22
// Propï¿½sito  : Crearenlace entratablas
// Creado Por : Juan Navas
// Llamado por:
// Aplicaciï¿½n :
// Tabla      :

FUNCTION JOINTABLE(cTableA,cTableB,cJoin)
    LOCAL cSintax:="",nAt,cFieldA,cFieldB,cWhere:=""

    DEFAULT oDp:aJoinTable:={}

    DEFAULT cTableA:="NMRECIBOS",;
            cTableB:="NMHISTORICO",;
            cJoin  :="INNER"

    cTableA:=ALLTRIM(UPPER(cTableA))
    cTableB:=ALLTRIM(UPPER(cTableB))

    nAt:=ASCAN(oDp:aJoinTable,{|a,n|a[1]==cTableA .AND. a[2]=cTableB})

    IF nAt=0

       cFieldA:=SQLGET("DPLINK","LNK_FIELDS,LNK_FIELDD,LNK_WHERE","LNK_TABLES"+GetWhere("=",cTableA)+" AND LNK_TABLED"+GetWhere("=",cTableB))
       cFieldB:=DPSQLROW(2,"")
       cWhere :=DPSQLROW(3,"")

       IF Empty(cFieldA)
         cFieldA:=SQLGET("DPLINK","LNK_FIELDS,LNK_FIELDD,LNK_WHERE","LNK_TABLED"+GetWhere("=",cTableA)+" AND LNK_TABLES"+GetWhere("=",cTableB))
         cFieldB:=DPSQLROW(2,"")
         cWhere :=DPSQLROW(3,"")
       ENDIF

       IF (Empty(cFieldA) .OR. Empty(cFieldB))
          MensajeErr("Tabla "+cTableA+" no tiene Vinculo con Tabla "+cTableB)
          RETURN ""
       ENDIF

       cFieldA:=_VECTOR(cFieldA)
       cFieldB:=_VECTOR(cFieldB)

       AEVAL(cFieldA,{|a,n| cSintax:=cSintax+IF(Empty(cSintax),""," AND ")+ ALLTRIM(cFieldA[n])+"="+ALLTRIM(cFieldB[n])})

       AADD(oDp:aJoinTable,{cTableA,cTableB,cSintax})

     ELSE

      cSintax:=oDp:aJoinTable[nAt,3]

    ENDIF

    cSintax:=cJoin+" JOIN "+cTableB+" ON "+cSintax

RETURN cSintax

/*
// Fecha de 8 digitos
*/
FUNC F82(dFecha) // Devueleve la fecha en 8 caracteres
     LOCAL cMES
     dFecha=EVAL(oDp:bFecha,dFecha)

     IF EMPTY(dFecha)
        RETURN "00/00/00"
     ENDIF

     cMes  =Left(CMES(dFecha),3)
     // MES  =SUBS("EnFbMrAbMyJnJlAgSeOcNvDi",2*(MES-1)+1,2)
RETURN STRZERO(DAY(dFecha),2)+cMes+RIGHT(STRZERO(YEAR(dFecha),4),2)

FUNCTION BRWDATE(oGet, dFecha , bValid , oSayRefresh )
RETURN LBXDATE( oGet, dFecha , bValid , oSayRefresh )

FUNCTION LBXDATE( oGet, dFecha , bValid , oSayRefresh )
   Local oWnd, aPoint, oCalendar
   Local dOldDate, bAction, lNormal
   Local nWidth, nHeight, oFont
   LOCAL oBrw // JN 27/12/2016
   LOCAL oCol,nAt,nColSel
   LOCAL nRow,nCol


   DEFAULT bValid:=oGet:bValid // jn

   nWidth  := 146
   nHeight := 175

//   bAction := {|y,x| x := IIF( lNormal, oCalendar:dNewDate, dOldDate ), ;
//                   oGet:varput( x ),IIF( lNormal, oGet:Refresh(), ""), ;
//                   oWnd:End() , MsgAlert(oGet:ClassName()),oGet:SetFocus(), oGet:bValid := oGet:bTmpValid }



   DEFINE FONT oFont NAME "Verdana" SIZE 0, 12

   dOldDate := oGet:varget()
   lNormal  := .F.

   IF "XBROW"$oGet:ClassName()

        oBrw    :=oGet
        nAt     :=oBrw:nArrayAt
        oCol    := oBrw:aCols[oBrw:nColSel]
        nRow    := ( ( oBrw:nRowSel - 1 ) * oBrw:nRowHeight ) + oBrw:HeaderHeight() + 2
        nCol    := oCol:nDisplayCol + 3
        nWidth  := oCol:nWidth - 4
        nHeight := oCol:oBrw:nRowHeight - 4
        aPoint  := { nRow, nCol }
        nColSel :=oBrw:nColSel

        //nHeight :=nHeight+200

        aPoint:= ClientToScreen( oBrw:hWnd, aPoint )

        aPoint[1]:=aPoint[1]+nHeight+1 // jn 01/09/2016
        aPoint[2]:=aPoint[2]-5         // jn 01/09/2016

        // Aqui debe Asignar el Valor Directo en la Columna, del arreglo

        DEFAULT bAction := {|y,x| x := IIF( lNormal, oCalendar:dNewDate, dOldDate ), ;
                             IF(EVAL(oCol:bOnPostEdit,oCol,x),oBrw:aArrayData[nAt,nColSel]:=x,NIL),;
                             oBrw:DrawLine(.t.),;
                             oWnd:End(),;
                             IIF( ValType(oSayRefresh)="O", oSayRefresh:Refresh(.T.) , NIL ),;
                             oBrw:SetFocus()}

/*
        DEFAULT bAction := {|y,x| x := IIF( lNormal, oCalendar:dNewDate, dOldDate ), ;
                             oWnd:End(),;
                             IIF( ValType(oSayRefresh)="O", oSayRefresh:Refresh(.T.) , NIL ),;
                             oBrw:SetFocus()}
*/

        DEFINE DIALOG oWnd;
               PIXEL OF oGet:oWnd;
               STYLE nOr( WS_POPUP, WS_VISIBLE )

        oWnd:oFont = oFont


        @ 0, 0 CALENDAR oCalendar VAR dFecha OF oWnd;
               BUTTONS  POPUP ;
               COLORSUNDAYS;
               PIXEL

        oCalendar:bLDblClick := {|nRow| IIF( nRow >= oCalendar:nTopDays(), ;
                                 (lNormal := .T., oWnd:End()) , ) }

        ACTIVATE DIALOG oWnd;
                 ON INIT (oWnd:Move(aPoint[1]+46,aPoint[2],NIL,NIL,.T.),;
                          oWnd:SetSize(190-35,170),;
                          .F.)

        IF !Empty(dFecha)
           EVAL(bAction,NIL,dFecha)
        ENDIF


       RETURN dFecha

    ELSE

        aPoint   := AdjustWnd( oGet, nWidth, nHeight )

    ENDIF

    DEFAULT bAction := {|y,x| x := IIF( lNormal, oCalendar:dNewDate, dOldDate ), ;
                     oGet:varput( x ),IIF( lNormal, oGet:Refresh(), ""), ;
                     oWnd:End(),;
                     IIF( ValType(oSayRefresh)="O", oSayRefresh:Refresh(.T.) , NIL ),;
                     oGet:SetFocus(), oGet:bValid := oGet:bValid }


/*
   // 07/12/2016
   aPoint   := AdjustWnd( oGet, nWidth, nHeight )
*/

   DEFINE WINDOW oWnd ;
      FROM aPoint[1] + 1, aPoint[2] TO aPoint[1] + nHeight, aPoint[2] + nWidth ;
      PIXEL STYLE  nOR( DS_MODALFRAME, WS_POPUP) OF oGet:oWnd

   oWnd:oFont = oFont

      //COLORS CLR_WHITE, CLR_BLUE, CLR_GREEN, CLR_HRED, CLR_RED, CLR_YELLOW;
   @ 0, 0 CALENDAR oCalendar VAR dFecha OF oWnd;
          BUTTONS  POPUP ;
          COLORSUNDAYS;
          PIXEL

   oCalendar:bLDblClick := {|nRow| IIF( nRow >= oCalendar:nTopDays(), ;
                                   (lNormal := .T., oWnd:End()) , ) }

   oWnd:bKeyDown := {|nKey| IIF( nKey == VK_ESCAPE, ;
                                eval( bAction )   , ;
                                IIF( nKey == VK_RETURN, ;
                                     (lNormal := .T., ;
                                      oCalendar:dDate := oCalendar:dNewDate, ;
                                      eval( bAction) ), ) ;
                                ) }

   oWnd:Move( aPoint[1]+1, aPoint[2], oCalendar:nWidth(), oCalendar:nHeight() )

   ACTIVATE WINDOW oWnd ;
      ON INIT ( oCalendar:SetBtnText( MI("Hoy",290),MI("Nada",291) ), ;
                oWnd:bLostFocus := {|| oWnd:bLostFocus := nil, eval(bAction)} );
      VALID ( .T. )


Return dFecha

/*
// Obtiene la Fecha Inicio de Mes
*/

FUNCTION TABLAFCHINIMES(cTable,cField,cWhere)
  LOCAL dFecha

  DEFAULT cTable:="DPCBTE",;
          cField:="CBT_FECHA"

  dFecha:=SQLGETMAX(cTable,cField,cWhere)

RETURN FCHINIMES(dFecha)

FUNCTION TABLAFCHFINMES(cTable,cField,cWhere)
RETURN FCHFINMES(TABLAFCHINIMES(cTable,cField,cWhere))


*/
/*
// Obtiene la Fecha Inicio de Mes
*/
/*
FUNCTION TABLAFCHFINMES(cTable,cField,cWhere)
RETURN TABLAFCHINIMES(REPFCHINIMES(cTable,cField,cWhere))
*/

/*
// Comparador con Campo Memo MYSQL, WHERE REP_PARAM<>'' por WHERE REP_PARAM IS NOT NULL
*/
FUNCTION GetWhereMemo(cOper,uValue,cType)

   DEFAULT cType:=oDp:cTypeBD

   IF Empty(uValue) .AND. cType="MSSQL"

      uValue:=" NULL "

      IF cOper="="
          uValue:=" IS NULL "
      ENDIF

      IF cOper="<>"
          uValue:=" IS NOT NULL "
      ENDIF

      cOper=""

      RETURN uValue

   ENDIF

RETURN GetWhere(cOper,uValue)

/*
FUNCTION F82(dFecha) // Devueleve la fecha en 8 caracteres
     LOCAL cMES
     dFecha=EVAL(oDp:bFecha,dFecha)

     IF EMPTY(dFecha)
        RETURN "00/00/00"
     ENDIF

     cMes  =Left(CMES(dFecha),3)
     // MES  =SUBS("EnFbMrAbMyJnJlAgSeOcNvDi",2*(MES-1)+1,2)
RETURN STRZERO(DAY(dFecha),2)+cMes+RIGHT(STRZERO(YEAR(dFecha),4),2)
*/

/*
FUNCTION OBJINSPECT(oObj,aData)
   LOCAL oWnd, oBrw, oCol,oTabs

   DEFAULT oObj:=oDp:oFrameDp

   DEFINE WINDOW oWnd TITLE "Inspector "+oObj:ClassName() MDICHILD FROM 0,0 TO 25,15;
         NOMAXIMIZE

   @ 300, 0 TABS oTabs PROMPTS "&Data", "&Mï¿½todos" OF oMdi:oWnd PIXEL ;
            COLOR NIL,CLR_YELLOW;
            ACTION oMdi:TABSACTION()

   oWnd:oClient := oMdi:oBrw

RETURN NIL

FUNCTION TABSACTION()
RETURN NIL
*/

/*
// Devuelve STR() sin Espacios
*/
FUNCTION LTRAN(nValue,nLen,nDec)
   LOCAL cPicture

   cPicture:="999,999,999,999"+IF(nDec<>NIL .AND. nDec>0,"."+REPLI("9",nDec),"")

RETURN ALLTRIM(FDP(nValue,cPicture))


DLL32 STATIC FUNCTION wCamGetDrvDesc;
       (nDriver AS _INT,;
        cName AS STRING,;
        nName AS LONG,;
        cVersion AS STRING,;
        nVersion AS LONG) AS BOOL PASCAL;
 FROM "capGetDriverDescriptionA" LIB "avicap32.dll"




************************************************************************
*
************************************************************************
Function WebCam2(codart,xalias,ori)
  Local oDlg_Cam, oImg,oWc,bton,oGet,cFich,lVale:=.F.,cFichero:=SPACE(254)
  LOCAL oSay

  if codart=NIL
     codart:="imap" + DTOS(date()) + strtran(time(),":")
  end if
  cFichero:="\" + codart + ".bmp"
  cFich:= "\" + codart + ".bmp"

  oWC:=tWebCamPhoto():New()

//? oWC:ClassName()
//? "AQUI ES"

  DEFINE DIALOG oDlg_Cam TITLE "Adquirir imagen del Producto via WebCam" FROM 0,0 TO 290,725 PIXEL

    @ 5,180 IMAGE oImg OF oDlg_Cam SIZE 100,100 PIXEL ADJUST // 160-120,120-20 PIXEL // ADJUST

    @ 130,10 BUTTON "Capturar" OF oDlg_Cam SIZE 30,10 PIXEL ACTION oWc:Save(oImg,cFich)
    @ 130,45 BUTTON "Aceptar"  OF oDlg_Cam SIZE 30,10 PIXEL ACTION (oWc:Disconnect(),lVale:=.T.,oDlg_Cam:End())
    @ 130,80 BUTTON "Cancelar" OF oDlg_Cam SIZE 30,10 PIXEL ACTION (oWc:Disconnect(),oDlg_Cam:End())

    @ 132,180 SAY oSay PROMPT "Localizacion fichero " PIXEL
    @ 130,230 GET oGet VAR cFich OF oDlg_Cam SIZE 110,10 PIXEL WHEN .F.

    oImg:nProgress:=0

  ACTIVATE DIALOG oDlg_Cam CENTER ON INIT;
    (oWC:CreateWnd(oDlg_Cam,10,10,260,180),oWc:Connect())
  if lVale

? "AQUI ES ",cFichero
/*
     sele (xAlias)
     replace (xalias)->foto with cFichero
     &&ori:Refresh()
*/
     ori:setbmp(cFichero)
     ori:refresh()

     sysrefresh()
  end if
return lVale


Function WebCamImg(oImage1)
  Local oWnd1, oMenu, oImg, oBtn
  Local oWC, lClick := FALSE
  LOCAL lQue


  DllWebCam:=LoadLibrary("avicap32.dll")
  oWC:=tWebCamPhoto():New()

  MENU oMenu // UPDATE

    MENUITEM "File"
      MENU
        MENUITEM "Click" ACTION (oWC:Save(oImg,'C:\_CK_NEW\WEBCAM32.jpg',80), lClick := TRUE, oWnd1:Update(),;
             oMenu:Refresh())
          MENUITEM "Save/Exit" ACTION (oWC:End(), PutFoto(oImage1), oWnd1:End()) WHEN lClick
      ENDMENU

      MENUITEM "Setup"
      MENU
        MENUITEM "Source" ACTION oWC:Source()
        MENUITEM "Format" ACTION oWC:Format()
      ENDMENU
     MENUITEM "Cancel" ACTION (oWC:End(), oWnd1:End())
    MENUITEM "Exit" ACTION (lQue := CkSalir(lClick), IIF( lQue,(oWC:End(), oWnd1:End()),))

  ENDMENU
  DEFINE WINDOW oWnd1 FROM 2,2 TO 28, 75 MENU oMenu NOZOOM;
        NOMINIMIZE

    @ 25, 380 IMAGE oImg OF oWnd1 SIZE 160,120 PIXEL ADJUST UPDATE
    oImg:nProgress:=0

     @ 18.00, 22 BUTTON  oBTN PROMPT "Click" of oWnd1 SIZE 85, 22 UPDATE;
        ACTION (oWC:Save(oImg,'C:\_CK_NEW\WEBCAM32.jpg',80), lClick := TRUE, oWnd1:Update())

     @ 9.75, 70.5 BUTTON  oBTN PROMPT "Save/Exit" of oWnd1 SIZE 85, 22 WHEN lClick UPDATE;
        ACTION (oWC:End(), PutFoto(oImage1), oWnd1:End())

     @ 11.75, 70.5 BUTTON  oBTN PROMPT "Cancel" of oWnd1 SIZE 85, 22 UPDATE;
        ACTION ((lQue := CkSalir(lClick), IIF( lQue,(oWC:End(), oWnd1:End()),)))

     @ 13.75, 70.5 BUTTON  oBTN PROMPT "Source" of oWnd1 SIZE 85, 22;
        ACTION (oWC:Source())

     @ 15.75, 70.5 BUTTON  oBTN PROMPT "Format" of oWnd1 SIZE 85, 22 UPDATE;
        ACTION (oWC:Format())

    ACTIVATE WINDOW oWnd1 ON INIT ( oWC:CreateWnd(oWnd1,25, 5, 300,220), oWC:Connect() )

return nil

/*
FUNCTION CkSalir(lClick)
    LOCAL lReturn := FALSE
    IF lClick
        IF MsgNoYes("The picture will not be saved. Are you sure to quit?")
            lReturn := TRUE
        ELSE
            lReturn := FALSE
        ENDIF
    ELSE
        lReturn := TRUE
    ENDIF

RETURN (lReturn)
*/

// Programa   : DPCLONEDXB
// Fecha/Hora : Realiza Copia de  Programa DpXbase con nuevo nombre y nuevo nombre de las variable objeto
// Propï¿½sito  : Obtiene Nuevo Script
// Creado Por : Juan Navas
// Llamado por:
// Aplicaciï¿½n :
// Tabla      :

FUNCTION DPCLONEDXB(cProgram,cVar,cVar2,lDelDxb)
   LOCAL cFilePpo
   LOCAL cFileScr
   LOCAL cFunction,oScript,cText,cNew,nAt


   DEFAULT cProgram:="DPCTABCOCONVIEW",;
           cVar    :="oVMobCon"

   DEFAULT cVar2  :="oVarMdi"+LSTR(oDp:nVarMdi)

   cNew    :=cProgram+"%"+LSTR(oDp:nVarMdi)
// cFilePpo:=oDp:cPathDxb+ALLTRIM(cNew)+".DXBX"
//
// IF FILE(cFilePpo) .AND. !lDelDxb
//    RETURN cFilePpo
// ENDIF

   /*
   // Tiene Prioridad el Personalizado
   */

   cFilePpo:=oDp:cPathDxb+ALLTRIM(cProgram)+".DXB"

   IF FILE(cFilePpo)
      cText   :=Memoread(cFilePpo)
   ENDIF

   cFilePpo:=oDp:cPathDxb+ALLTRIM(cProgram)+".DXBX"

   IF Empty(cText) .AND. FILE(cFilePpo)

      // cFilePpo:=oDp:cPathDxb+ALLTRIM(cProgram)+".DXB"

      cText   :=Memoread(cFilePpo)
      cText   :=LEFT(cText,LEN(cText)-1)
      cText   :=HB_UNCOMPRESS(4096*600,cText)

   ENDIF


  // oScript  :=LoadScript(cProgram)
/*
   oScript:=TScript():New("")
   oScript:cProgram:=ALLTRIM(uppe(cProgram))
   oScript:Reset()
   oScript:lPreProcess := .T.
   oScript:LoadPCode(cFilePpo,NIL,.T.)
   cText  :=oScript:cText
   oScript:End()
*/

//   cText   :=Memoread(cFilePpo)
//   cText   :=LEFT(cText,LEN(cText)-1)
//   cText   :=HB_UNCOMPRESS(4096*600,cText)

   IF !ValType(cText)="C" .OR. Empty(cText)
      MensajeErr("Archivo "+cFilePpo+" no pudo ser Leido")
      RETURN cFilePpo
   ENDIF

// cText   :=HB_UNCOMPRESS(4096*400,cText)
//? LEN(cText),cVar,cVar2

   IF !Empty(cVar)

     cText   :=STRTRAN(cText,cVar,cVar2)

// ? AT(cVar2,cText),cVar2,cText

     IF AT(cVar2,cText)=0
       MensajeErr("Texto "+cVar+cVar2+" no Encontrado en "+cFilePpo)
       DPWRITE("TEMP\"+ALLTRIM(cProgram)+".SCR",cText)
    ENDIF

   ENDIF
// cNew    :=cProgram+"%"+LSTR(oDp:nVarMdi)

// ? cText,"LISTO"

   // Copia para Revisar
   IF MACROEJE("ISPCPRG()")
     cFilePpo:=oDp:cPathDxb+ALLTRIM(cNew)+".DXB"
     DPWRITE(cFilePpo,cText)
   ENDIF

   cFilePpo:=oDp:cPathDxb+ALLTRIM(cNew)+".DXBX"
   cText   :=HB_COMPRESS(cText)+"x"

   DPWRITE(cFilePpo,cText)

RETURN cFilePpo  // +"x"


// Programa   : RUNMDI
// Fecha/Hora : 17/05/2018 17:45:39
// Propï¿½sito  : Ejecuta un Programa DpXbase de Manera Recursiva Mediante Clone del Programa Compilado y Clones del Objeto Contenedor
// Creado Por :
// Llamado por:
// Aplicaciï¿½n :
// Tabla      :

FUNCTION RUNMDI(cProgram,cVar,p1, p2, p3, p4, p5, p6, p7, p8, p9, p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)
   LOCAL lRet,oScript,cFunction:="MAIN"
   LOCAL cFilePpo  ,cVar2,nAt

   DEFAULT cProgram   :="DPCTABCOCONVIEW",;
           cVar       :="oVMobCon",;
           oDp:lDelDxb:=.F.

   DEFAULT oDp:nVarMdi:=0,;
           oDp:aVarMdi:={}

   nAt:=ASCAN(oDp:aVarMdi,{|a,n|a[1]==cProgram})

   IF nAt=0
      AADD(oDp:aVarMdi,{cProgram,0})
      nAt:=LEN(oDp:aVarMdi)
      oDp:nVarMdi:=1
   ELSE
      oDp:nVarMdi:=oDp:aVarMdi[nAt,2]+1
   ENDIF

   oDp:aVarMdi[nAt,2]:=oDp:nVarMdi

   cVar2   :=cVar+LSTR(oDp:nVarMdi)

//? cVar,cVar2,"cVar,cVar2"

   cFilePpo:=DPCLONEDXB(cProgram,cVar,cVar2,oDp:lDelDxb)

// ? cFilePpo,"cFilePpo"

   IF FILE(cFilePpo)
      cProgram:=cFileNoPath(cFilePpo)
      cProgram:=cFileNoExt(cProgram)
      lRet:=EJECUTAR(cProgram, @p1, @p2, @p3, @p4, @p5, @p6, @p7, @p8, @p9, @p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20)
   ELSE
      MensajeErr("Programa "+cFilePpo+" no Existe")
   ENDIF

RETURN lRet

/*
// Ejecutar Clic de Un Control
*/
FUNCTION DPRUNCLICK(oObj)
RETURN PostMessage(oObj:hWnd, 1024+1024 ) // 9/3/2017 Simula Presionar Click, dibuja el recuadro 9/3/2017

FUNCTION TXTTOARRAY(cFile)
   LOCAL cMemo

   IF !FILE(cFile)
     RETURN {}
   ENDIF

   cMemo:=MemoRead(cFile)

   cMemo:=STRTRAN(cMemo,CRLF,CHR(10))
   cMemo:=_VECTOR(cMemo,CHR(10))

RETURN cMemo

/*
// Obtiene un Archivo desde las Personalizaciones locales, si no existe, las guarda en el DPDIRAPL con el objetivo de  almacenarla y distribuirla en los demas PC
*/
FUNCTION DPAPTGETLOCAL(cFile,lServer)
RETURN EJECUTAR("DPAPTGETLOCAL",cFile,lServer   )

FUNCTION DPWHERE(cField,cOper,uValue1,uValue2,cAnd)
  LOCAL cWhere

  DEFAULT cOper:="="

/*
  DEFAULT cField :="FIELD",;
          cOper  :="=",;
          uValue1:="UNO",;
          uValue2:="DOS",;
          cAnd   :=" A=B"
*/

   IF !Empty(uValue2)
      cWhere:=cField+GetWhereAnd(cField,uValue1,uValue2)
   ELSE
      cWhere:=cField+GetWhere(cOper,uValue1)
   ENDIF

   IF !Empty(cAnd)
     cWhere:=cAnd+" AND "+cWhere
   ENDIF

RETURN cWhere

FUNCTION ViewArray32(aData,cTitle,lOrder,lMdi,bRun)
   LOCAL oRdd:=tRddArray():New(),I

   DEFAULT lMdi  :=.T.    ,;
           lOrder:=.F.

   // Quitar CRLF
//   FOR I=1 TO LEN(aData)
//      AEVAL(aData[I],{|a,n| IF(ValType(a)="C", aData[I,n]:=STRTRAN(a,CRLF,""), NIL)})
//   NEXT    I


   oDp:lViewArrayMdi:=lMdi

   oRdd:SetArray(aData)

//? "ViewArray",GETPROCE()

   oRdd:Browse(cTitle,lOrder,bRun)
   oRdd:bRun:=bRun

RETURN oRdd

/*
// Devuelve el Texto                                
*/
FUNCTION COLOR_OPTIONS(cTable,cField,uValue,lLow,aItems)
  oDp:cSay:=SAYOPTIONS(cTable,cField,uValue,lLow,aItems)
RETURN oDp:nClrOptions
// RETURN nColor

//DLL32 FUNCTION QRCODE(cStr AS  LPSTR, cFile LPSTR) AS LONG PASCAL FROM "FastQRCode" LIB "QRCodelib.Dll"
//DLL32 FUNCTION FastQRCode(cStr AS  LPSTR, cFile LPSTR) AS long PASCAL LIB "QRCodelib.Dll"
// DLL32 FUNCTION  FastQRCode(cStr AS  LPSTR, cFile AS LPSTR) AS BOOL PASCAL LIB "QRCodelib.Dll"

DLL32 STATIC FUNCTION QRCode(cStr As STRING, cFile As STRING) AS LONG PASCAL ;
      FROM "FastQRCode" LIB "QRCodelib.Dll"
RETURN NIL

/*
 *  SPN.PRG
 *
 *  cShortName() - FilePath Long to Short (GetShortPathNameA())
 *
 *  Soporte a Funciones del Api de Windows
 *
 *  Andrade A. Daniel - 2002
 *  Rosario, Argentina
 *
 *  Libre Distribuciï¿½n
 */

// https://forums.fivetechsupport.com/viewtopic.php?f=6&t=37519&p=224406#p224406
/*
 *  cShortName()
 */

FUNCTION cShortName( cLong )

    Local   cShort  := Space(164) + Chr(0)
    Local   nBuffer := 164
    Local   nLen

    nLen    := GetSPName( cLong, @cShort, nBuffer )

    cShort  := Left( cShort, nLen )

RETURN( cShort )

// Windows Api
DLL32 STATIC FUNCTION GetSPName( lpszLongPath AS STRING, lpszShortPath AS STRING, lBuffer AS LONG ) ;
      AS LONG PASCAL FROM "GetShortPathNameA" LIB "kernel32"


//DLL32 FUNCTION AllocConsole() AS BOOL PASCAL LIB "KERNEL32.DLL"

/*
 LOCAL nResp
   LOCAL qrDLL

   qrDLL := LoadLibrary("QRCodelib.Dll" )

   nResp := DllCall(qrDLL,DC_CALL_STD,"FastQRCode",cStr,cFile)

   FreeLibrary(qrDLL)
*/


/*
FUNCTION Generar_QR(cStr,cFile)

   LOCAL nResp
   LOCAL qrDLL

   qrDLL := LoadLibrary("QRCodelib.Dll" )

   nResp := DllCall(qrDLL,DC_CALL_STD,"FastQRCode",cStr,cFile)

   FreeLibrary(qrDLL)

RETURN (NIL)
*/

FUNCTION FRMTEXT(cText)

  IF oDp:oMsgRun=NIL
    MSGRUNVIEW(cText,cText)
  ENDIF

  IF oDp:oMsgRun<>NIL
      // ErrorSys(.T.)
      oDp:oMsgRun:FRMTEXT(cText)
  ENDIF

RETURN oDp:oMsgRun


//----------------------------------------------------------------------------------------
/*
#pragma BEGINDUMP
#include "Windows.h"
#include "hbApi.h"
HB_FUNC( RESTOFILE )
{
HRSRC res=FindResource(NULL,"ACTVER",RT_RCDATA);
LPDWORD bytesWritten;
int size=SizeofResource(NULL,res);
HGLOBAL hRes=LoadResource(NULL,res);
unsigned char *pRes=(unsigned char *)LockResource(hRes);
HANDLE hFile=CreateFile("ACTVER.EXE",GENERIC_WRITE,0,NULL,CREATE_ALWAYS,0,NULL);
WriteFile(hFile,pRes,size,&bytesWritten,NULL);
CloseHandle(hFile);
}
#pragma ENDDUMP
*/


FUNCTION PrnGetPaper( oPrn )

   LOCAL aPoint := PrnGetSize( oPrn:hDc ), aRecmm := {}, nW := 0
   LOCAL nH := 0, wx := 0

   LOCAL aPapers:={{"DMPAPER_LETTER            " ,  1 , "Letter              " , {216,279} },;
                   {"DMPAPER_LETTERSMALL       " ,  2 , "Letter Small        " , {216,279} },;
                   {"DMPAPER_TABLOID           " ,  3 , "Tabloid             " , {280,432} },;
                   {"DMPAPER_LEDGER            " ,  4 , "Ledger              " , {432,280} },;
                   {"DMPAPER_LEGAL             " ,  5 , "Legal               " , {216,356} },;
                   {"DMPAPER_STATEMENT         " ,  6 , "Statement           " , {140,216} },;
                   {"DMPAPER_EXECUTIVE         " ,  7 , "Executive           " , {184,257} },;
                   {"DMPAPER_A3                " ,  8 , "A3                  " , {297,420} },;
                   {"DMPAPER_A4                " ,  9 , "A4                  " , {210,297} },;
                   {"DMPAPER_A4SMALL           " , 10 , "A4 Small            " , {210,297} },;
                   {"DMPAPER_A5                " , 11 , "A5                  " , {148,210} },;
                   {"DMPAPER_B4                " , 12 , "B4                  " , {250,354} },;
                   {"DMPAPER_B5                " , 13 , "B5                  " , {182,257} },;
                   {"DMPAPER_FOLIO             " , 14 , "Folio               " , {216,330} },;
                   {"DMPAPER_QUARTO            " , 15 , "Quarto              " , {215,275} },;
                   {"DMPAPER_10X14             " , 16 , "10x14 in            " , {254,356} },;
                   {"DMPAPER_11X17             " , 17 , "11x17 in            " , {280,432} },;
                   {"DMPAPER_NOTE              " , 18 , "Note                " , {216,279} },;
                   {"DMPAPER_ENV_9             " , 19 , "Envelope #9         " , {098,225} },;
                   {"DMPAPER_ENV_10            " , 20 , "Envelope #10        " , {105,241} },;
                   {"DMPAPER_ENV_11            " , 21 , "Envelope #11        " , {114,264} },;
                   {"DMPAPER_ENV_12            " , 22 , "Envelope #12        " , {109,279} },;
                   {"DMPAPER_ENV_14            " , 23 , "Envelope #14        " , {127,292} },;
                   {"DMPAPER_ENV_DL            " , 27 , "Envelope DL         " , {110,220} },;
                   {"DMPAPER_ENV_C5            " , 28 , "Envelope C5         " , {162,229} },;
                   {"DMPAPER_ENV_C3            " , 29 , "Envelope C3         " , {324,458} },;
                   {"DMPAPER_ENV_C4            " , 30 , "Envelope C4         " , {229,324} },;
                   {"DMPAPER_ENV_C6            " , 31 , "Envelope C6         " , {114,162} },;
                   {"DMPAPER_ENV_C65           " , 32 , "Envelope C65        " , {114,229} },;
                   {"DMPAPER_ENV_B4            " , 33 , "Envelope B4         " , {250,353} },;
                   {"DMPAPER_ENV_B5            " , 34 , "Envelope B5         " , {176,250} },;
                   {"DMPAPER_ENV_B6            " , 35 , "Envelope B6         " , {176,125} },;
                   {"DMPAPER_ENV_ITALY         " , 36 , "Envelope            " , {110,230} },;
                   {"DMPAPER_ENV_MONARCH       " , 37 , "Envelope Monarch    " , {098,191} },;
                   {"DMPAPER_ENV_PERSONAL      " , 38 , "6 3/4 Envelope      " , {092,165} },;
                   {"DMPAPER_FANFOLD_US        " , 39 , "US Std Fanfold      " , {378,279} },;
                   {"DMPAPER_FANFOLD_STD_GERMAN" , 40 , "German Std Fanfold  " , {216,305} },;
                   {"DMPAPER_FANFOLD_LGL_GERMAN" , 41 , "German Legal Fanfold" , {216,330} } }

   aRecmm = oPrn:Pix2Mmtr(aPoint[1], aPoint[2])

   nW = ROUND(aRecmm[1],0)
   nH = ROUND(aRecmm[2],0)

   for wx=1 to len(aPapers)
      if aPapers[wx,4,1] = nW .AND. aPapers[wx,4,2] = nH
         return {aPapers[wx,1],aPapers[wx,2],aPapers[wx,3]}
      endif
   next

return {"",0,""}

// Programa   : DBISTABLE
// Fecha/Hora : 16/10/2010 19:33:19
// Propósito  : Verifica si la Tabla Existe
// Creado Por : Juan Navas
// Llamado por:
// Aplicación :
// Tabla      :


FUNCTION DBISTABLE(cDb,cTable,lCreate)
  LOCAL aData,oDb,cSql,nAt:=0,lResp:=.F.,cFile

  DEFAULT cTable:="DPINV",;
          cDb   :=SQLGET("DPTABLAS","TAB_DSN","TAB_NOMBRE"+GetWhere("=",cTable))

  DEFAULT lCreate:=.F.

  IF !ValType(cDb)="O"

    IF "<"$cDb
     cDb:=oDp:cDsnData
    ENDIF

    IF "."$cDb
     cDb:=oDp:cDsnConfig
    ENDIF

    IF "-"$cDb
     cDb:=oDp:cDsnDicc
    ENDIF

    IF !ValType(cTable)="C"
       MensajeErr("cTable",CTOO(cTable)+" debe ser CHAR en DBISTABLE "+oDp:cOldScript)
       RETURN .F.
    ENDIF

    cTable:=ALLTRIM(UPPE(cTable))

    IF !cDb=NIL

     oDb   :=OPENODBC(cDb)

    ELSE

     oDb   :=GETODBC(cTable)

    ENDIF

  ELSE

    oDb :=cDb

  ENDIF

  IF oDb=NIL
     MensajeErr("No fue posible Abrir la BD "+CTOO(cDb,"C")+CRLF+"Para la Tabla "+cTable)
     RETURN .F.
  ENDIF

  aData :=oDb:GetTables()

// ViewArray(aData)

  nAt:=ASCAN(aData,{|a,n| ALLTRIM(UPPE(a))==cTable })

  IF !ValType(lCreate)="L"
     lCreate:=.F.
  ENDIF

  oDp:lCreateTable:=.F.

  IF nAt=0 .AND. lCreate

     cFile:="STRUCT\"+ALLTRIM(cTable)+".TXT"

     IF ISSQLFIND("DPTABLAS","TAB_NOMBRE"+GetWhere("=",cTable))
        Checktable(cTable)
     ENDIF

     IF !oDb:FILE(cTable)

        IF !FILE(cFile) .AND. ISSQLFIND("DPTABLAS","TAB_NOMBRE"+GetWhere("=",cTable))
          EJECUTAR("EJMIMPDATOS",cTable,NIL,.T.)
        ENDIF

        EJECUTAR("DPCREATEFROMTXT",cTable)

     ENDIF

     aData :=oDb:GetTables()

     nAt:=ASCAN(aData,{|a,n| ALLTRIM(UPPE(a))==cTable })

     oDp:lCreateTable:=.T.

     LOADTABLAS(.T.)

  ENDIF

  lResp:=(nAt>0)

  IF ValType(lResp)<>"L"
     lResp:=.F.
  ENDIF

RETURN lResp

FUNCTION DPMEMOREADX(cFile)
  LOCAL cText,nAt

  cText       :=Memoread(cFile )
  nAt         :=AT(CHR(2),cText)
  oDp:cFileOrg:=LEFT(cText,nAt-1)

  cText   :=SUBS(cText,nAt+1,LEN(cText))
  // cText   :=SUBS(cText,LEN(cText)-1) // Quitar el CHAR(2)

  cText   :=HB_UNCOMPRESS(4096*600,cText)

RETURN cText
// eturn


FUNCTION DPWRITEX(cFile,cMemo)

  IF MACROEJE("ISPCPRG()")
    cMemo:=cFile+CHR(2)+HB_COMPRESS(cMemo)
// +CHR(2)
// ? "AQUI SE GRABA CON MAS CHR(2)"
  ENDIF

RETURN DPWRITE(cFile,cMemo)

FUNCTION DPMIN(lStrSql1,lStrSql2)

   IF lStrSql2=NIL
      lStrSql2:=lStrSql1
   ENDIF

   IF lStrSql1=NIL
      lStrSql1:=lStrSql2
   ENDIF

RETURN IF(!lStrSql1>lStrSql2,lStrSql1,lStrSql2)

//FUNCTION BRWLBXCURSOR(oLbx,cCondic,nClrPane1,nClrPane2)
//RETURN {|| {oLbx:oCursor:FieldGet(nCol), iif( oLbx:oBrw:nArrayAt%2=0, nClrPane1  ,   nClrPane2 ) } }


DLL32 FUNCTION LeCartao(pResultado AS LPSTR,pMensagem AS LPSTR,TCTrilha1 AS LPSTR, ;
Trilha1 AS LPSTR, TCTrilha2 AS LPSTR, Trilha2 AS LPSTR, Timeout AS LPSTR) AS LONG PASCAL ;
FROM "LeCartaoDiretoSeguroA" LIB oDp:xDllTef

/*
FUNCTION FTPFROMADP(cFile,cSaveAs)
Descargar archivos desde el repositorio adaptaPro ubicado en la ruta:
http://191.96.151.60/~ftp16402
contentiene copia de las carpeta de AdaptaPro
SGEV60
DPNMWIN60

La ruta del Archivo es construida según licencia y versión en uso:
http://191.96.151.60/~ftp16402/DP"+oDp:cType+"V"+LSTR(oDp:nVersion*10)+"/"

Su utilización interna aplica cuando falta algún componente, será reemplazado
automáticamente ejemplo c:\dpsgev60\dpxbase\dpreportes.dxbx

También podrá ser descargada discrecionalmente por el usuario desde la
funcionalidad: Ejecutar comando, podrá escribir:

FTPFROMADP("dpxbase\dpreportes.dxbx"), la función devolverá .T. en el caso de
lograr descargar el archivo mediante validación del destino físico
FILE(cFileOutPut), además genera variable oDp:cUrlFileOrg para explorar el
contenido preciso del origen y validar copiándolo en el navegador.
*/

FUNCTION FTPFROMADP(cFile,cSaveAs)
  LOCAL cDir:="http://191.96.151.60/~ftp16402/DP"+oDp:cType+"V"+LSTR(oDp:nVersion*10)+"/"
  LOCAL cUrl
  LOCAL cDirD

  DEFAULT cFile  :="DPXBASE\AAA.DXBX",;
          cDirD  :=cFilePath(cFile),;
          cSaveAs:=oDp:cBin+cFile


  cFile  :=STRTRAN(cFile,"\\","\")
  cUrl   :=LOWER(cDir+cFile)
  cUrl   :=STRTRAN(cUrl,"\","/")
  cSaveAs:=lower(cSaveAs)

  ferase(cSaveAs) // Elimina el Destino

  URLDownLoad(cUrl, cSaveAs)

  oDp:cUrlFileOrg:=cUrl // Copia en Memoria del Archivo Origen
// ? CLPCOPY(cUrl),cSaveAs,FILE(cSaveAs)

RETURN FILE(cSaveAs)

DLL32 FUNCTION RemoveDir(cPathName AS LPSTR) AS BOOL PASCAL ;
FROM "RemoveDirectoryA" LIB "Kernel32.Dll"



/*
FUNCTION validateBase64( cB64 )

   LOCAL cRegex := "^[A-Za-z0-9+/]+=*$"
   LOCAL pRegex := hb_regexComp( cRegex, .T., .T. )
   LOCAL lValid := .F.

   hb_default(@cB64, "" )

   IF !Empty( hb_regex(pRegex, cB64 ) )
      lValid := .T.
   ENDIF

RETURN lValid
*/
// EOF

