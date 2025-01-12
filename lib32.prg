// #INCLUDE "FIVEWIN.CH"
#Include "FIVEWIN.CH"
//#INCLUDE "\DP32\INCLUDE\DP32.CH"

REQUEST nGetFileFilter,TFILE,WINRUN,TDOCENC,wndcopy,DISKSPACE,__MRestore ,__MSave , TMail
REQUEST TWORD,AgetPrinters,TDosPrn,PageSetup,tWebCamPhoto,testfoto,Oclone

// REQUEST GETURLTOFILE
// REQUEST WebPageContents
// REQUEST tpdf
// REQUEST tipclienthttp

REQUEST BUSCARRIF_CREATE,BUSCARRIF_SETCAPTCHA,BUSCARRIF_BUSCAR,BUSCARRIF_FREE
REQUEST EMAIL_CREATE,EMAIL_SETSERVER,EMAIL_SETAUTH,EMAIL_SETSENDER,EMAIL_ADDTO,EMAIL_SETMESSAGE,EMAIL_SETCONFIGEX,EMAIL_SENDMAIL

MEMVAR oDp

STATIC oExcel, oFile,aTablas,aTimers

STATIC aStdFiles

STATIC aModules := {}
STATIC aVarPub

//----------------------------------------------------------------------------//
// FUNCION DE VARIABLES PUBLICAS DATAPRO
//----------------------------------------------------------------------------//
// FUNCTION NTRIM(nValue)
// RETURN LTRIM(STR(nValue))

// FUNCTION  LEELLAVEDP(cUrl,lServer,cFileDp)
// RETURN .T.

FUNCTION XDPMEMOREAD(cFile,nSpace)

  LOCAL nH,cMemo,cLine

  DEFAULT cFile :="DPXBASE\DPFACTURA.DXB",;
          nSpace:=65

  cMemo:=MEMOREAD(cFile)

  oDp:cMemo:=cMemo

RETURN cMemo
/*
  nH := fopen(cFile)
  while fmove2next(nh)
     cLinea := sfreadline(nhandle)
  enddo
  fclose(nHandle)
RETURN CaArray
*/

//  IF Empty(cStr)
//     ? "VACIO",cFile
//  ENDIF

/*
// 555

? cFile,ErrorSys(.T.)

   nH := FOPEN(cFile, FC_NORMAL)
   //IF FERROR() != 0
   IF  nH!=F_ERROR
      cStr := FReadStr( nH, nSpace )
      ? cStr
      FClose( nH )
   ENDIF

? ValType(cStr),cFile
*/
RETURN cStr

FUNCTION ISFILEDXB(cFilePpo)

    IF FILE(cFilePpo)
       RETURN .T.
    ENDIF

    cFilePpo:=ALLTRIM(cFilePpo)
    cFilePpo:=LEFT(cFilePpo,LEN(cFilePpo)-1)

    // ? cFilePpo,FILE(cFilePpo)

RETURN FILE(cFilePpo)


FUNCTION VP(cVarName,uValue)
  LOCAL nAt
/*
  cVarName:=StrTran(cVarName,".","_")

  IF uValue=NIL
    RETURN oDp:Get(cVarName)
  ENDIF

  IF !oDp:IsDef(cVarName)
    oDp:Add(cVarName,uValue)
  ENDIF

  oDp:Set(cVarName,uValue)

RETURN uValue
*/

 IF !ValType(aVarPub)="A"
    aVarPub:={}
 ENDIF
 IF cVarName=CHR(0)
    FOR nAT:=1 TO LEN(aVarPub)
      aVarPub[nAt,2]:=nil
    NEXT
    aVarPub:={}
    RETURN NIL
 ENDIF
 nAt:=ASCAN(aVarPub,{|aVar|aVar[1]==cVarName}) // ESTA DEBE SER PUBLICA
 IF uValue=NIL.AND.nAt>0 // Pide Valor
    uValue=aVarPub[nAt,2] // Devuelve el Valor
 ELSE
  // ALMACENA VALOR
  IF nAt=0
    AADD(aVarPub,{cVarName,uValue})
  ELSE

    uValue:=IIF( ValType(uValue)="C".AND.uValue=CHR(0),NIL,uValue)

    aVarPub[nAt,2]:=NIL
    aVarPub[nAt,2]:=uValue
    uValue:=NIL

  ENDIF
ENDIF

RETURN uValue

/*
// Verifica si una Cadena es totalmente numerica
*/
FUNCTION ISALLDIGIT(cVar)
   LOCAL lDigit:=.T. ,I

   cVar:=ALLTRIM(cVar)

   // Caso del Primer Caracter
   IF !ISDIGIT(LEFT(cVar,1))
     RETURN .F.
   ENDIF

   FOR I=1 TO LEN(cVar)

      IF !ISDIGIT(SUBS(cVar,I,1))
         lDigit:=.F.
      ENDIF

   NEXT I

RETURN lDigit
/*
// Conversion de Datos Hacia Cadenas  //
*/
FUNCTION CTOO(uValue,cTypeReq)
   LOCAL I,cNuevo:="",cLinea:="",nSigno,cType,nAt,nAt2

   DEFAULT cTypeReq:="C"

   IF uValue=NIL .AND. cTypeReq="N"
      RETURN 0
   ENDIF

   IF ValType(uValue)="C"
      uValue:=ALLTRIM(uValue)

      IF Empty(uValue)
         uValue:=" "
      ENDIF

   ENDIF

   IF ValType(uValue)=cTypeReq
       RETURN uValue
   ENDIF

   cType:=ValType(uValue)

//   IF ValType(uValue)="O"
//      Return "Object"
//   ENDIF

   DO CASE
     CASE cTypeReq="C" .AND. cType="A"
          RETURN APrint( uValue , NIL  , .F.) + CRLF
     CASE cTypeReq="L".AND.ValType(uValue)="N"
        uValue:=(uValue=1)
     CASE cTypeReq="C".AND.uValue=NIL
        uValue="NIL"
     CASE cTypeReq="N".AND.ValType(uValue)="C"
        nSigno:=IIF("-"$uValue,-1,+1)
        // JN 28/09/2014
        nAt :=AT(".",uValue)
        nAt2:=AT(",",uValue)

        IF nAt<nAt2
          uValue:=STRTRAN(uValue,".","")
          uValue:=STRTRAN(uValue,",",".")
        ENDIF
//        uValue:=STRTRAN(uValue,".",";")  //JN 28/09/2014
        uValue:=STRTRAN(uValue,",","")
        uValue:=STRTRAN(uValue,"-","")
//        uValue:=STRTRAN(uValue,";",".") //JN 28/09/2014

        uValue:=Val(ALLTRIM(uValue))*nSigno

     CASE cTypeReq$"DF".AND.ValType(uValue)="C"
        // uValue=CTOD(uValue)
        uValue:=EVAL(oDp:bFecha,uValue) // JN 22/09/2017, Convierte la fecha desde Excel
     CASE cTypeReq="C".AND.ValType(uValue)$"DF"
        uValue=DTOC(uValue)
     CASE cTypeReq="C".AND.ValType(uValue)="L"
        uValue=IIF( uValue, ".T.", ".F." )
     CASE cTypeReq="C".AND.ValType(uValue)="A"
        cNuevo:="{["
        FOR I := 1 TO LEN(uValue)
            cLinea:=IIF( ValType(uValue[I])="O","Objets",CTOO(uValue[I],"C"))
         //   ? cLinea,"cLinea"
            WHILE ValType(cLinea)!="C"
               cLinea:=CTOO(cLinea,"C")
               IF ValType(cLinea)="C"
                  EXIT
               ENDIF
           //    ? cLinea,"cLinea","EN WHILE"
            ENDDO
           // ? cLinea,"cLinea Luego de CTOO() ",
            cNuevo:=cNuevo+IIF( I=1,"",",[")+cLinea+"]"
        NEXT
        uValue:=IIF( LEN(cNuevo)=2,"{}",cNuevo+"}")

     CASE cTypeReq="C".AND.ValType(uValue)="N".AND.uValue=INT(uValue) // Número Entero
        uValue:=ALLTRIM(STR(uValue,20,0))
     CASE cTypeReq="C".AND.ValType(uValue)="N".AND.uValue!=INT(uValue) // Número Decimales
        uValue:=ALLTRIM(STR(uValue))
     CASE cTypeReq="C".AND.ValType(uValue)<>"C" // Cadena es igual Cadena
     CASE cTypeReq="L".AND.ValType(uValue)="C" .AND. ALLTRIM(uValue)$"SN.T..F."

//        ? "AQUI",uValue

        IF uValue=".T."
           RETURN .T.
        ELSEIF uValue=".F."
           RETURN .F.
        ENDIF
        uValue=("S"$uValue) // EN uValue
     CASE cTypeReq="L".AND.ValType(uValue)="C".AND.uValue$"SINO"
        uValue=("SI"$uValue) // EN uValue
     CASE cTypeReq="L".AND.ValType(uValue)="C".AND.uValue$"YN"
        uValue=(uValue="Y") // EN uValue
     CASE cTypeReq="L".AND.ValType(uValue)="C".AND.uValue$".T..F."
        uValue=(uValue=".T.") // EN uValue
     CASE cTypeReq="L".AND.ValType(uValue)="C".AND.uValue$"TF"
        uValue=(uValue="T") // EN uValue
     CASE cTypeReq="C".AND.ValType(uValue)="B"
        uValue:="{||.T.}"
     CASE cTypeReq="C".AND.ValType(uValue)="O"
        uValue:="Objetcs"
   ENDCASE
RETURN uValue

/*
// Revisa los ComboBox, segun el valor que posee, nunca puede esta vacia la lista
*/
FUNCTION ComboIni(oCbx,lView)
   LOCAL uValue:=Eval(oCbx:bSetGet),nAt,nLen,bWhen:=oCbx:bWhen

   DEFAULT lView:=.F.

   IF lView
     ? oCbx:nAt,Eval(oCbx:bSetGet),uValue,"ComboIni",LEN(oCbx:aItems),"Items"
   ENDIF

   // ? oCbx:oGet:Classname()
   oCbx:bKeyDown:={|nKey|oCbx:oWnd:nLastKey:=nKey}
   oCbx:bWhen   :={||.T.}

   // ? "uValue",uValue,oCbx:nAt,oCbx:VarGet()

   IF ValType(uValue)="C"
      uValue:=ALLTRIM(uValue)
      nLen  :=Len(uValue)
      nAt   :=aScan( oCbx:aItems, { |a,n|UPPE(LEFT(a,nLen))==UPPE(uValue) } )
//    ? nAt,uValue

      IF (nAt=0 .AND. !Empty(oCbx:aItems)) .OR. Empty(uValue)
         uValue  :=oCbx:aItems[1]
         EVAL(oCbx:bSetGet,oCbx:aItems[1])
         oCbx:VarPut(uValue,.T.)
      ELSE
         oCbx:nAt:=nAt
         uValue  :=oCbx:aItems[nAt]
         EVAL(oCbx:bSetGet,oCbx:aItems[nAt])
         oCbx:VarPut(uValue,.T.)
      ENDIF
   ENDIF
/*
   IF !Empty(uValue)
      oCbx:nAt:=0
   ENDIF
*/
   IF oCbx:nAt!=0 .AND. !Empty(uValue)
      oCbx:bWhen:=bWhen
      RETURN .T.
   ENDIF

   // Comunmente incluir
   IF (oCbx:nAt=0 .AND. Empty(uValue)) .AND. !EMPTY(oCbx:aItems)
      oCbx:nAt:=1
      uValue  :=oCbx:aItems[1]
      EVAL(oCbx:bSetGet,oCbx:aItems[1])
      oCbx:VarPut(uValue,.T.)
      oCbx:bWhen:=bWhen

      //oCbx:SetText(uValue)
      //? "Este Vacio",oCbx:aItems[1],uValue,EVAL(oCbx:bSetGet)
      oCbx:=NIL
      RETURN .T.
   ENDIF

   // Modificación con Valores no Encontrados
   IF oCbx:nAt=0 .AND. !Empty(uValue) .AND. !EMPTY(oCbx:aItems) .AND. ValType(uValue)="C"

      uValue:=ALLTRIM(UPPE(uValue))

      // ? "busca aqui",uValue

      IF lView
         ? "uValue",uValue,oCbx:nAt,"ANTES DEL ESCAN"
      ENDIF

      oCbx:nAt:=aScan( oCbx:aItems, { |a,n| UPPE(LEFT(a,LEN(uValue)))==uValue } )

      IF lView
         ? "uValue",uValue,oCbx:nAt,"Encontrado AL FINAL",LEN(oCbx:aItems)
      ENDIF

      IIF( oCbx:nAt>0 , EVAL(oCbx:bSetGet,oCbx:aItems[oCbx:nAt]) , NIL )

      oCbx:Select(oCbx:nAt)

      IF lView
        ? oCbx:nAt,Eval(oCbx:bSetGet),uValue,"ComboIni","Resultado Items",LEN(oCbx:aItems)
      ENDIF

   ENDIF

   oCbx:bWhen:=bWhen

RETURN .T.

/*
// Revisa los Valores del Arreglo y Devuelve el mas cercano
*/
FUNCTION GetFromList(uValue,aItems)
   LOCAL nAt:=0

   IF ValType(aItems)!="A" .OR. ValType(uValue)="L"
      Return uValue
   ENDIF

   IF ValType(uValue)<>"C"  // JN 21/04/2018, SCROLLGET COMBOBOX con valor Numerico
      RETURN uValue
   ENDIF

   nAt   :=MAX(aScan( aItems, { |a| LEFT(a,LEN(uValue))=uValue }),1)
   uValue:=aItems[nAt]

RETURN uValue

/*
// Realiza la Conversión del Idioma en el programa Binario
*/
FUNCTION MI(cText,nNum,cIdTras)
  LOCAL nAt,I,aData,aText,cFile

  DEFAULT oDp:cIdTras:="DPWIN"

//oDp:cLanguage:="FRA" // Quitar

  DEFAULT cText        :="Advertencia",;
          nNum         :=0,;
          cIdtras      :=oDp:cIdTras,;
          oDp:aTranslBin:={},;
          oDp:cLanguage :="ESP"

  IF Empty(nNum) .OR. oDp:cLanguage="ESP"
     RETURN cText
  ENDIF

  IF Empty(oDp:aTranslBin)

    cFile:="BIN\"+oDp:cLanguage+"\TRANSLATOR.TXT"

    aData:=MemoRead(cFile)
    aData:=STRTRAN(aData,CRLF,CHR(10))
    aData:=_VECTOR(aData,CHR(10))

    ADEPURA(aData,{|a,n| LEFT(a,2)="//" .OR. Empty(a)}) // Comentarios

    FOR I=1 TO LEN(aData)
      aData[I]:=_VECTOR(aData[I],",")
      aData[I,1]:=CTOO(VAL(aData[I,1]),"N")
    NEXT I

    oDp:aTranslBin:=aData

  ENDIF

  nAt:=ASCAN(oDp:aTranslBin,{|a,n| a[1]=nNum })

  IF nAt>0
     cText:=oDp:aTranslBin[nAt,3]
  ELSE
     cText:=cText+" ["+cIdTras+"/"+LSTR(nNum)+"]"
  ENDIF

  cText:=CTOO(cText,"C")

RETURN cText


/*
// Genera Macrosustitución
*/
FUNCTION MacroEje( cMacro , lProtec , lRetoErr )
  LOCAL oErr

  IF EMPTY(cMacro)
     RETURN NIL
  ENDIF

  // Utilizada por Valores por Defecto
  IF LEFT(cMacro,1)="&"
     cMacro:=SUBS(cMacro,2,LEN(cMacro))
  ENDIF

  cMacro = CONV_AND( cMacro )

  IF VALTYPE( lProtec ) = "L" .AND. lProtec
     lProtec := ErrorBlock( {|oErr| BREAK(oErr) } )
     BEGIN SEQUENCE
        cMacro := &(cMacro)
        cMacro := .T.
     RECOVER USING oErr
        MensajeErr(cMacro,"error in macroeje")// oDp:cMsgError :=ErrorMessage(oErr)
        cMacro := IF( VALTYPE( lRetoErr ) = "L" .AND. lRetoErr , oErr , .F. )
     END SEQUENCE
     ErrorBlock( lProtec )
     RETURN cMacro
  ENDIF
RETURN &cMacro.

FUNCTION RUNBLQ(bExp,lSayErr)
   LOCAL lResp,oErr,cErr

   DEFAULT lSayErr:=.T.

   oDp:cErrorMsg:=""

   BEGIN SEQUENCE

      lResp:=EVAL(bExp)

   RECOVER USING oErr

       oDp:cErrorMsg:=ErrorMessage(oErr)

       IF lSayErr
          MensajeErr(oDp:cErrorMsg+CRLF+GetProce(),"Mensaje RUNBLQ")
       ENDIF

   END SEQUENCE

RETURN lResp


/* KillProcessByName( "AdaptaPro.exe" ) */

************************************************************************************************
#pragma BEGINDUMP

#include <Windows.h>
#include <hbapi.h>
#include <string.h>
#include <tlhelp32.h>

void killProcessByName(const char *filename)
{
    HANDLE hSnapShot = CreateToolhelp32Snapshot(TH32CS_SNAPALL, NULL);
    PROCESSENTRY32 pEntry;
    BOOL hRes;

    pEntry.dwSize = sizeof (pEntry);
    hRes = Process32First(hSnapShot, &pEntry);

    while (hRes)
    {
        if (strcmp(pEntry.szExeFile, filename) == 0)
        {
            HANDLE hProcess = OpenProcess(PROCESS_TERMINATE, 0,
                                          (DWORD) pEntry.th32ProcessID);
            if (hProcess != NULL)
            {
                TerminateProcess(hProcess, 9);
                CloseHandle(hProcess);
            }
        }
        hRes = Process32Next(hSnapShot, &pEntry);
    }
    CloseHandle(hSnapShot);
}

HB_FUNC( KILLPROCESSBYNAME )
{
   killProcessByName( hb_parc( 1 ) );
}

#pragma ENDDUMP
******************************************************************************************************************+


FUNCTION ACCESSFIELD(cTable,cField,nOption)

  LOCAL nAt,lResp:=.T.,cRun

  DEFAULT cTable :="DPUSUARIOS",;
          cField :="OPE_NOMBRE",;
          nOption:=3

// RETURN ACCESSFIELD32(cTable,cField,nOption)

  cRun:=[ACCESSFIELD32(]+GetWhere("",cTable)+","+GetWhere("",cField)+","+GetWhere("",nOption)+[)]

RETURN &cRun.
*/

/*
// Creación de Bloques de Código
*/

FUNCTION BLOQUECOD( cMacro,cError )

  IF VALTYPE(cMacro)="B"
    RETURN cMacro
  ENDIF

  cMacro := IIF( EMPTY(cMacro) , "{||NIL}" ,cMacro )
  cMacro := CONV_AND(cMacro)
  IF "{"$cMacro.AND."|"$cMacro.AND.LEFT(cMacro,2)!="{|"
     cMacro:="{||"+cMacro+"}"
  ELSE
    IF "|" $ cMacro
      cMacro:=IIF(!"{" $ cMacro .AND. ! "}" $ cMacro,"{"+cMacro+"}",cMacro)
    ELSE
      cMacro:="{||"+cMacro+"}"
    ENDIF
  ENDIF

  cMacro:=IIF( Empty(cMacro),"{||.T.}" ,cMacro )

  cMacro:=EJEC_DEF({||MACROEJE(cMacro)},@cError,.F.,.T.)

RETURN cMacro

FUNCTION EJEC_DEF(bBlqExp,cError,lCaer,lCallar)
   LOCAL uValue // uValue es Producto de la Ejecución
   LOCAL lError:=.F.
   LOCAL oError, xValue , bError , lBlock,mProce,n // , cError:=""

   DEFAULT lCaer:=.F.
   DEFAULT cError :="" // Llega como Referencia
   DEFAULT lCallar:=.f.

//   ErrorSys(lCaer)

  // (lError := .f.,VP("cError",""),VP("lBotar",lCaer),VP("lCallar",.T.)) // lCallar) // El Error

   BEGIN SEQUENCE

      bBlqExp:=BLOQUECOD(bBlqExp)
      uValue :=EVAL(bBlqExp)

  RECOVER // USING oError

      lError := .t.

      mProce:=""
      n:=0
      while ( n < 74+10 )
        if ! Empty(ProcName( n ) )
          mProce:=mProce+IIF( EMPTY(mProce),"" ,CRLF )+"Called " + Trim( ProcName( n ) ) +"("+ LSTR(ProcLine( n ))+")"
        endif
        n++
     end

//   ? "mProce",mProce

     MEMOWRIT("DPERROR.TXT",mProce)

//      ? "ERROR EJEC_DEF"

      if .T. // oError != Nil // .and. oScript != nil

         cError:=VP("cError") // Scr_GetError(oError)
//         VP("cError","")
         // EXPERR:="" // DPXBASE
      endif

  END SEQUENCE


   IF lError.AND.!lCallar  // EMPTY(cError)

      MsgAlert(cError,"Precaución")

      IF "OBJET"$cError
         MsgAlert("El Objeto a recibido un Valor no Válido")
      ENDIF

      RETURN .T.
   ENDIF

RETURN uValue

FUNCTION CONV_AND(cValue)

   // oDp:oFrameDp:SetText(uValue)

RETURN cValue

FUNCTION GETINI(cFileIni,cValue,cDefault)

   LOCAL uValue:="",cMemo,aValue:="",I

   IF FILE(cFileIni)
      cMemo :=MEMOREAD(cFileIni)
      cMemo :=STRTRAN(cMemo,CHR(10),"")
      aValue:=_VECTOR(cMemo,CHR(13))
      FOR I := 1 TO LEN(aValue)
         IF UPPE(cValue)=UPPE(LEFT(aValue[I],LEN(cValue)))
           uValue:=ALLTRIM(SUBS(aValue[I],AT("=",aValue[I])+1,200))
           EXIT
         ENDIF
      NEXT
   ENDIF

   // JN 25/06/2016 (Valor por Defecto)
   IF Empty(uValue) .AND. !Empty(cDefault)
      uValue:=cDefault
   ENDIF

   IF Empty(uValue) .AND. ValType(cDefault)="L"
      uValue:=cDefault
   ENDIF


   IF !Empty(cDefault) .AND. !Empty(uValue)
      uValue:=CTOO(uValue,ValType(cDefault))
   ENDIF

RETURN uValue

FUNCTION STRTOARRAY(cData,cSepara)

   IF cSepara=CRLF
      cData:=STRTRAN(cData,CRLF,",")
   ENDIF

RETURN _VECTOR(cData,cSepara)

FUNCTION _VECTOR(cPar,cSepara)
    LOCAL nPos,aLista:={},X,nLen,SEP1,SEP2

    IF ValType(cPar)="A"
       RETURN cPar
    ENDIF

    cSepara=IF(cSepara=NIL,",",cSepara) // CON LOS SEcParADORES
    SEP1="]"+cSepara+"["
    SEP2=cSepara+" "
    DO WHILE .T.
       nLen=3
       nPos=AT(SEP1,cPar) // AT("],[",cPar)
       IF nPos=0
          nPos=AT(cSepara,cPar) // AT(",",cPar)
          nLen=1
       ENDIF
       IF nPos=0
          nPos=AT(SEP2,cPar) // AT(", ",cPar)
          nLen=2
       ENDIF
       IF nPos=0
          EXIT
       ENDIF
       X=LEFT(cPar,nPos-1)
       AADD(aLista,X)
       cPar=SUBS(cPar,nPos+nLen,LEN(cPar))
    ENDDO
    IF LEN(cPar)>0 //.OR.RIGHT(XcPar,1)=cSepara // despues del SecParador esta vacio
       AADD(aLista,cPar)
    ENDIF
RETURN aLista

/*
// Obtiene el Dialogo de un Control
// GetDlg(oGet):oWnd Asi obtiene el MDI
*/

Function GetDlg(oDlg)
   LOCAL oDlg2
   IF !ValType(oDlg)="O"
      RETURN oDlg
   ENDIF
   IF oDlg:ClassName()!="TDIALOG"
      oDlg:=oDlg:oWnd
   ENDIF
   IF oDlg:ClassName()="TDIALOG"
      oDlg2:=oDlg:oWnd
      IF ValType(oDlg2)="O".AND.!"MDI"$oDlg2:ClassName() // Puede Ser TFOLDER
        oDlg:=oDlg2:oWnd  // Puede ser oDlg
      ENDIF
   ENDIF
   STORE NIL TO oDlg2
RETURN oDlg

FUNCTION SETDIALOG(lDialog)

   DEFAULT lDialog:=.F.

   oDp:lSetDialog:=lDialog

RETURN .T.



/*
³ FUNCTION SFREADLINE()
³
³  Short:
³  ------
³  SFREADLINE() Reads in text up to the next CRLF in a text file
³
³  Returns:
³  --------
³  <cLine> => line read in
³
³  Syntax:
³  -------
³  SFREADLINE(nHandle)
³
³  Description:
³  ------------
³  Reads in text up to the next CRLF in a text file. The
³  pointer is moved back to the starting position when done. To move to a
³  new line, use  FMOVE2PRIOR() or FMOVE2NEXT().
³  ---------
³
*/
FUNCTION SFREADLINE(nHandle)

local cReturnLine
local cChunk
local cBigChunk
local nOldOffset
local nAtChr13

cReturnLine := ''
cBigChunk   := ''
nOldOffset  := fseek(nHandle,0,1)
do while .T.

   // - read in a cChunk of the file
   cChunk := ''
   cChunk := freadstr(nHandle,100)

   if len(cChunk) = 0
      cReturnLine := cBigChunk
      exit
   endif

   // - add this cChunk to the big cChunk
   cBigChunk += cChunk

   // - if we've got a CR , we've read in a line
   // - otherwise we'll loop again and read in another cChunk
   if (nAtChr13 := at(chr(13),cBigChunk)) > 0

      // - go back to beginning of line
      fseek(nHandle,nOldOffset)

      // - read in from here to next CR (-1)
      cReturnLine := freadstr(nHandle,nAtChr13 - 1)

      // - move the pointer 1 byte
      fseek(nHandle,1,1)

      exit
   endif
enddo
fseek(nHandle,nOldOffset)
return cReturnLine  // if len(cReturnline)==0, is EOF!


/*
 FUNCTION FMOVE2NEXT()
³
³  Short:
³  ------
³  FMOVE2NEXT() Move to beginning of next line in a text file
³
³  Returns:
³  --------
³  <lSuccess> => success in doing so
³
³  Syntax:
³  -------
³  FMOVE2NEXT(nHandle)
³
³  Description:
³  ------------
³  Moves pointer in text file <nHandle> to beginning of
³  next line.
³
³  Presuming lines end in CRLF. Returns <expL> for
³  success. End of file would return .f.
³
*/

function fmove2next(nHandle)

local cChunk
local nOldOffset := fseek(nHandle,0,1)
local nNewOffset
local nAtChr10
local lSuccess   := .f.
do while .T.

   nNewOffset := fseek(nHandle,0,1)

   // - read in a cChunk of the file
   cChunk := ''
   cChunk := freadstr(nHandle,100)

   if len(cChunk) = 0                   // eof()
      lSuccess := .f.
      exit
   endif

   // - if we've got a CR , we've read in a line
   // - otherwise we'll loop again and read in another cChunk
   if (nAtChr10 := at(chr(10),cChunk)) > 0
      lSuccess := .t.
      // - go back to beginning of line
      fseek(nHandle,nNewOffset)
      fseek(nHandle,nAtChr10,1)
      exit
   endif
enddo

if !lSuccess
   fseek(nHandle,nOldOffset)
endif
return lSuccess

/*
// DO()
// Ejecuta un bloque de código rurante un Proceso Ciclico
*/
FUNCTION DO(bWhile,bFor,bLet,lReturn)

   bWhile :=IIF(ValType(bWhile)="C",BloqueCod(bWhile),bWhile)
   bFor   :=IIF(ValType(bFor  )="C",BloqueCod(bFor  ),bFor  )
   bLet   :=IIF(ValType(bLet  )="C",BloqueCod(bLet  ),bLet  )

   DEFAULT bWhile:={||.F.}
   DEFAULT bFor  :={||.T.}

   WHILE EVAL(bWhile)
      IF EVAL(bFor)
         lReturn:=EVAL(bLet)
      ENDIF
   ENDDO

RETURN lReturn

FUNCTION PUBLICO(cVarName,uValue)
   LOCAL aVars:={}

   IF ","$cVarName
     aVars:=_VECTOR(cVarName)
     AEVAL(aVars,{|a,n| PUBLICO(ALLTRIM(a),uValue)})
     RETURN uValue
   ENDIF

   IF !EMPTY(cVarName)
     // oDp:oFrameDp:cTitle:=PADR(cVarName,20)
     __QQPUB(cVarName)
     MOVER(uValue,cVarName)
     // PUBLIC &cVarName:=uValue
     // MOVER(uValue,cVarName)
   ENDIF

RETURN uValue

FUNCTION DIV(nDividendo,nDivisor)
RETURN IIF(nDividendo*nDivisor!=0,nDividendo/nDivisor,0)

FUNCTION CERO(uValue,lCero,lEmpty,nLen)
   LOCAL oControl

   DEFAULT lEmpty:=.F.,;
           nLen  :=LEN(uValue)

   IF EMPTY(uValue)
      RETURN lEmpty
   ENDIF

   IF LEN(uValue)=LEN(ALLTRIM(uValue)) .OR. !ALLDIGIT(uValue)
      RETURN .T.
   ENDIF

   // ? nLen,"nLen"

   uValue  :=STRZERO(VAL(uValue),nLen)
   oControl:=GetControl()

   EVAL(oControl:bSetGet,uValue)
   oControl:VarPut(uValue,.t.)
   oControl:PostMsg( WM_KEYDOWN, 13, 0 ) // Keyboard

   oControl:=nil

RETURN .F.

FUNCTION ALLDIGIT(uValue)
   LOCAL cChar,I:=0
   uValue:=ALLTRIM(uValue)
   WHILE (I++,cChar:=SUBS(uValue,I,1),I<=LEN(uValue).AND.cChar>="0".AND.cChar<="9")
   ENDDO
RETURN (I-1=LEN(uValue))

/*
// Forza el Focus de un Control
*/
FUNCTION DPFOCUS(oObj)
   LOCAL oDlg

   // IF !ValType(oObj)="O"
      // RETURN NIL
   // ENDIF

   IF ValType(oObj)="O" .AND. oObj:hWnd=0
     RETURN NIL
   ENDIF

   DEFINE DIALOG oDlg
   oDlg:nLastKey:=0
   ACTIVATE DIALOG oDLG ON INIT oDLG:End()

   IF ValType(oObj)="O"
       oObj:oWnd:nLastKey:=0
       //oObj:ForWhen()
       oObj:SetFocus()
   ENDIF

RETURN NIL

/*
// Obtiene la Ventana de un Objeto
*/
Function GetWnd(oObj,cClass)
   LOCAL oWnd
   DEFAULT cClass:="MDI"
   IF ValType(oObj)="O"
     oWnd:=oObj:oWnd
     DO WHILE !cClass$oWnd:CLASSNAME().AND.ValType(oWnd)="O"
       oWnd:=oWnd:oWnd
     ENDDO
   ENDIF
RETURN oWnd

/*
// Salva las Variables Publicas y privadas
*/

FUNCTION SaveVar(ARREGLO)
  LOCAL LAS_VAR1:={},LAS_VAR2:={},VAR,nI,cTYPE
  ARREGLO=IF(VALTYPE(ARREGLO)="C",_VECTOR(ARREGLO),ARREGLO)
  FOR nI=1 TO LEN(ARREGLO)
    VAR=ARREGLO[nI];cTYPE:=TYPE(VAR)
    IF cTYPE="A"
      AADD(LAS_VAR1,VAR) // {VAR,&VAR.})
      AADD(LAS_VAR2,ACLONE(&VAR)) //
    ELSEIF !cTYPE=[U]
      AADD(LAS_VAR1,VAR) // {VAR,&VAR.})
      AADD(LAS_VAR2,&VAR) //
    ENDIF
  NEXT nI
RETURN {LAS_VAR1,LAS_VAR2}

/*
// Restaura Varianbles Publicas y privadas , previamente salvadas
*/

FUNCTION RestVar( aVars , lPub )
  IF EMPTY( aVars )
    RETURN .F.
  ENDIF
  lPub := IF( VALTYPE( lPub ) = "L" , lPub , .F. )
  AEVAL( aVars[1], {|cVar,nPos| IF( lPub , __qqPub( cVar ) , NIL ) , &( cVar ) := aVars[2, nPos ] } )
RETURN .T.

FUNCTION DPWNDMAIN()
   LOCAL oWnd    :=VP("oWndDefault")
   LOCAL lWndMain:=VP("lWndMain")

   DEFAULT lWndMain:=.T.

   IF lWndMain
      oWnd:=WndMain()
   ENDIF

   // ? oWnd:hDC,oWnd:hWnd
RETURN oWnd

/*
// Destruir Objetos //
*/
FUNCTION DESTROY(oObj,lDestroy)
  LOCAL aControls,cClass

  aControls:=IIF( ValType(oObj)="O",oObj:aControls,oObj)
  aControls:=IIF( ValType(aControls)!="A",{},aControls)

  AEVAL(aControls,{|a,i| IIF(ValType(a)="O" , DESTROY(a,.T.) , NIL)})

  DEFAULT lDestroy:=.T. // Borra el control Principal

  IF  lDestroy.AND. ValType(oObj)="O"

    cClass:=oObj:ClassName()

    IF cClass$"TGET,MGET,TBUTTON"
      oObj:bValid:={||.T.}
      oObj:bWhen :={||.T.}
    ENDIF

    IF cClass="TGET" // Destruye el Get del Get
       oObj:oGet:End()
    ENDIF

    oObj:End()

  ENDIF

  aControls:=NIL
  oObj     :=NIL

RETURN .T.


FUNCTION CTOEMPTY(uValue,cType,nLen)
   // Convierte en Vacio un Valor

   DEFAULT cType:=ValType(uValue)

   //IF uValue=NIL
   //   RETURN NIL
   // ENDIF


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


FUNCTION CheckAccesTable(cTable,nOption)
RETURN .T.

//----------------------------------------------------------------------------//

FUNCTION HrbLoad( cFile )
   local hModule
   local nModule
   local cFilehrb:=oDp:cPathHrb+cFile
   LOCAL cBin    :=UPPER(GetModuleFileName( GetInstance() ))

   DEFAULT oDp:lRunHrb:=.T.

   //
   // indica que debe ejecutar modulos HRB 15/11/2022
   //
   // IF !oD

//   DEFAULT oDp:cBin:=UPPER(GetModuleFileName( GetInstance() ))

   /*
   // HRB generador de Reportes incluido en el binari no lo puede ejecutar
   */
   // ? cFile,"cFile QUE NO PUEDE EJECUTAR",cBin,"NO PUEDE LLAMAR HRB"

   IF "_HRB"$cBin  .OR. !oDp:lRunHrb
 // .AND. "DPGENREP"$UPPER(cFile)
 // ? "HRB generador de Reportes incluido en el binari no lo puede ejecutar"
      RETURN .T.
   ENDIF

   IF !FILE(cFileHrb)
      MensajeErr(cFileHrb+" no Existe")
      RETURN .T.
   ENDIF
   // ? Directory(oDp:cPathHrb+".hrb")
/*
   DEFAULT oDp:lLoadHrb:=!Empty(Directory(oDp:cPathHrb+".hrb"))   // No hay archivos *.hrb

   IF !oDp:lLoadHrb
      // Empty(Directory(oDp:cPathHrb+".hrb"))
      RETURN 0
   ENDIF
*/

//,aFiles:={}

//? oDp:cPathHrb+cFile,"oDp:cPathHrb+cFile"

/*
   IF !file(cFileHrb) .AND. FILE(cFile)

      lmkdir(oDp:cPathHrb)
      COPY FILE (cFile) TO (cFileHrb)
      FERASE(cFile)
      EJECUTAR("DPHRBTODIRHRB")

   ENDIF
*/

/*
   cFile:=cFileHrb

   IF !FILE(cFile)
      MsgAlert("Modulo "+cFile+" no fué encontrado",oDp:cTitleAlert)
      RETURN 0
   ENDIF
*/
   cFile := Upper( cFile )

  // ? cFile,"hrbload"

   nModule :=  Ascan( aModules, {|v| v[ 1 ] == cFile } )



   If nModule > 0
      return aModules[ nModule, 2 ]
   Endif

   hModule  := __hrbload( cFileHrb )

   Aadd( aModules, {cFile, hModule} )

// ? cFile,hModule,"hrbload,hModule"

return hModule

/*
// Baja de Memoria Todos los Modulos Recargados
*/
FUNCTION HrbRelease(cFile)
/*
    Local nAt

    IF ValType(cFile)="C"

       cFile:=cFileName(cFile)
       nAt  :=  Ascan( aModules, {|v| v[ 1 ] == cFile } )

       IF nAt=0
         Return .F.
       ENDIF

          __hrbUnLoad(aModules[nAt,2]) // Descarga el Módulo

       ADEL(aModules,nAt)
       ASIZE(aModules,Len(aModules)-1)

    ELSEIF !EMPTY(aModules)

       Aeval(aModules,{|a|__hrbUnLoad(a[2])})
       //__hrbUnLoad( aModules[1,2] )
       //__hrbload( aModules[1,1] )

       aModules:={}

    ENDIF
*/
RETURN .T.

/*
// Genera Where de Lista (FIELD="A" .OR. FIELD="B")
*/
FUNCTION GetWhereOr(cField,aLista,cOperator,cAnd)
   LOCAL cWhere:="",I,nAdd:=0,aClone:={},uDesde,uHasta,nNumero

   DEFAULT cOperator:="="
   DEFAULT aLista   :={}
   DEFAULT cAnd     :=""

   // Debe revisar la clausula IN, si es soportada por SQL

/*
   IF !EMPTY(aLista) .AND. .F.

      AEVAL(aLista,{|a,n,nAt|a:=IIF( ValType(a)="A" , a[1] , a ) , nAt:=ascan(aClone,a) , IIF( nAt=0, AADD(aClone,a),nil) })

      IF ValType(aClone[1])>0
         aSort(aClone)
         nNumero:=VAL(aClone[1])
         nAdd   :=0
         uDesde :=aClone[1]
         FOR I := 1 TO LEN(aClone)
            IF nNumero+nAdd==VAL(aClone[i])
               uHasta:=aClone[I]
            ELSE
               EXIT
            ENDIF
            nAdd:=nAdd+1
         NEXT

//         ? uDesde,aClone[1],uHasta,aClone[Len(aClone)]

         IF uDesde=aClone[1] .AND. uHasta=aClone[Len(aClone)]

            RETURN " ("+cField+GetWhere(">=",uDesde)+" AND "+;
                       cField+GetWhere("<=",uHasta)+")"
         ENDIF
      ENDIF

   ENDIF
  */

   AEVAL(aLista,{|a,n|a:=IIF( ValType(a)="A" , a[1] , a ) , cWhere+=IIF( n>1, " OR " , "") + cField + GetWhere(cOperator , a)})

   IF !Empty(cWhere)
      cWhere:=cAnd+"("+cWhere+")"
   ENDIF

RETURN cWhere

/*
// Genera WHERE, Entre Campos y Valores
*/
FUNCTION GetWhere(cSigno,uValue,cValtype,lAlltrim)
   LOCAL cWhere:=""

   DEFAULT cValType:=ValType(uValue)
   DEFAULT cSigno  :="=",lAlltrim:=.T.

   IF uValue=NIL .AND. !Empty(cSigno) // Utilizar GetWhere(Contenido), no requiere "=" Igual // LEN(cSigno)=1 //"="
      uValue:=cSigno
      cSigno:="="
   ENDIF

   IF ValType(uValue)="C"

      uValue:=STRSQL(ALLTRIM(uValue))

//      IF "'"$uValue
//         uValue:=STRTRAN(uValue,"'",["])
//      ENDIF

   ENDIF

   DO CASE

      CASE cValType="N" .OR. cValType="L"

        cWhere:=cSigno+STRSQL(uValue)

      CASE cValType="D"

        cWhere:=cSigno+CTOSQL(uValue)

        IF oDp:cTypeBD="MSSQL" .AND. "NULL"$cWhere
          cWhere:=STRTRAN(cWhere,"="," IS ")
        ENDIF

        IF oDp:cTypeBD="MSSQL" .AND. "NULL"$cWhere
          cWhere:=STRTRAN(cWhere,"<>"," IS NOT ")
        ENDIF

//      ? cWhere,"cWhere"

      OTHE

        uValue:=IIF( ValType(uValue)="C",ALLTRIM(uValue)  ,  uValue   )
        uValue:=IIF( ValType(uValue)="C",STRSQLOFF(uValue),STRSQL(uValue))

        uValue:=IF(ValType(uValue)="C",uValue,CTOO(uValue,"C")) // 17/01/2024  incidencia  TXTSAVETOARRAY

        IF "LIKE["$cSigno

           cSigno:=STRTRAN(cSigno,"X",uValue)
           cSigno:=STRTRAN(cSigno,"["," '")
           cSigno:=STRTRAN(cSigno,"]","'")

           IF "NOT_LIKE"$cSigno
             cSigno:=STRTRAN(cSigno,"NOT_LIKE"," NOT LIKE")
           ENDIF

           cWhere:=" "+cSigno+" "

         ELSE

           cWhere:=cSigno+"'"+uValue+"'"

        ENDIF

   ENDCASE

return cWhere

/*
#INCLUDE "TDSN.CH"
#define  HKEY_CURRENT_USER  2147483649        // 0x80000001
#define  HKEY_LOCAL_MACHINE 2147483650        // 0x80000002
#define  HKEY_USERS         2147483651        // 0x80000003

#define conODBC        "SOFTWARE\ODBC\ODBC.INI"
#define conODBC_DSN    "SOFTWARE\ODBC\ODBC.INI\ODBC Data Sources"
#define conODBC_DRIVER "SOFTWARE\ODBC\ODBCINST.INI"

FUNCTION Odbc32DsnEntries( nDsnType )

  LOCAL acRetVal := {}
  LOCAL cValue
  LOCAL n
  LOCAL nRet
  LOCAL nHandle

  DEFAULT nDsnType := TDSN_SYSTEM_DSN

  IF nDsnType == TDSN_USER_DSN
    nRet := RegOpenKey( HKEY_CURRENT_USER                 ,;
                        conODBC, @nHandle )
  ELSE
    nRet := RegOpenKey( HKEY_LOCAL_MACHINE                ,;
                        conODBC, @nHandle )
  ENDIF

  IF nRet == 0
    n := 0
    DO WHILE RegEnumKey( nHandle, n++, @cValue ) == 0
      IF Left( cValue, 5 ) <> "ODBC "
         aAdd( acRetVal, cValue )
      ENDIF
    ENDDO
  ENDIF

RETURN acRetVal

RETURN .T.
/*
// Convierte en Zero hacia la Izquierda una Expresión Caracter
*/
FUNCTION CTOZERO(uValue,nLen)

   DEFAULT nLen:=LEN(uValue)

   IF LEN(uValue)=LEN(ALLTRIM(uValue))
      RETURN uValue
   ENDIF

   IF ALLDIGIT(uValue)
      uValue:=STRZERO(VAL(uValue),nLen)
   ENDIF

RETURN uValue
FUNCTION clavedos()
RETURN "clavedos"
FUNCTION ClaveWin()
RETURN "clavewin"

/*
// Determina la Fecha Final del mes
*/
FUNCTION FCHFINMES(dFinMes)
     dFinMes:=EVAL(oDp:bFecha,dFinMes)
     dFinMes:=(dFinMes-Day(dFinMes))+35  // Primero del mes + 32 días
RETURN dFinMes-Day(dFinMes)     // esto obtiene un dia del mes siguiente
// RETURN dFinMes
// -Day(dFinMes)
/*
     dFinMes:=FCHINIMES(dFinMes)+27
     WHILE Month(dFinMes+1)=Month(dFinMes)
        dFinMes++
     ENDDO
RETURN dFinMes
*/

/*
// Devuelve la Fecha del Siguiente Mes
*/
FUNCTION FCHSIGMES(dFecha)
   LOCAL nDia,dFchFin,lFinMes

   dFecha :=EVAL(oDp:bFecha,dFecha)
   lFinMes:=dFecha=FchFinMes(dFecha) // Si es Final del Mes
   nDia   :=DAY(dFecha)
   dFecha :=FchFinMes(dFecha)+1     // MAX(nDia-5,1)
   dFchFin:=FchFinMes(dFecha)       // No puede pasar de aqui

   WHILE DAY(dFecha)<nDia .AND. dFecha<dFchFin
      IF Month(dFecha+1)<>Month(dFchFin)
         EXIT
      ENDIF
      dFecha++
   ENDDO

RETURN dFecha
/*
// Determina la Fecha Inicial del Mes
*/
FUNCTION FCHINIMES(dIniMes)
     dIniMes:= EVAL(oDp:bFecha,dIniMes)  // La convierte en Fecha
RETURN (dIniMes-Day(dIniMes))+1
//     dIniMes:=[01]+RIGHT(DTOC(dIniMes),LEN(SET(4))-2)
// RETURN CTOD(dIniMes)

/*
// Activa Alias
*/
FUNCTION DPSELECT(cAlias)
     LOCAL cField
     IF ValType(cAlias)<>"C" .OR.Empty(cAlias)
        RETURN .T.
     ENDIF
     cField:=cAlias+[->(FIELD(1))]
     IF !TYPE(cField)=[U]
        SELECT(cAlias)
        RETURN .T.
     ENDIF
RETURN .F.

#define  HKEY_CLASSES_ROOT       2147483648        // 0x80000000
#define  HKEY_CURRENT_USER       2147483649        // 0x80000001
#define  HKEY_LOCAL_MACHINE      2147483650        // 0x80000002
#define  HKEY_USERS              2147483651        // 0x80000003
#define  HKEY_PERFORMANCE_DATA   2147483652        // 0x80000004
#define  HKEY_CURRENT_CONFIG     2147483653        // 0x80000005

/*
//Indica simplemente si hay una impresora instalada o no
Function lIsPrinter( lMessage )
Default lMessage:=.T.

 If Len( aPrinters() ) = 0  .AND. lMessage
    MsgStop("Imposible Imprimir. No hay ninguna impresora instalada en su ;
             sistema","Sin impresora")
 Endif

Return Len( aPrinters() )>0

//Crea un array con los nombres de las impresoras instaladas en el sistema
Function aPrinters( lCompleto )
Local oKey, oReg
Local cBuffer := ""
Local nId := 0
Local aPrinters := {}

DEFAULT lCompleto := .F.  //.T. devuelve un array de 2 posiciones con la imp. y donde esta conectada

oReg := TReg32():New(HKEY_LOCAL_MACHINE,"System\CurrentControlSet\Control\Print\Printers" )

 While RegEnumKeys( oReg:nHandle, nId++, @cBuffer ) == 0

       oKey := TReg32():New(HKEY_LOCAL_MACHINE,"System\CurrentControlSet\Control\Print\Printers\" + cBuffer)
       IF !lCompleto
          aAdd( aPrinters, oKey:Get( "Name" ) )
       Else
          aAdd( aPrinters, { oKey:Get( "Name" ), oKey:Get( "Port" ) } )
       EndIf
       oKey:Close()

 EndDo

 oReg:Close()

Return aPrinters

//Devuelve información sobre la impresora indicada
function aPrinterInfo( cPrnName )
LOCAL aInfo := {}, oReg

 If ! At( "\\", cPrnName ) > 0
    oReg :=tReg32():New(HKEY_LOCAL_MACHINE,"System\CurrentControlSet\Control\Print\Printers\" + cPrnName)

    aAdd( aInfo, oReg:Get( "Name" ) )
    aAdd( aInfo, oReg:Get( "Description" ) )
    aAdd( aInfo, oReg:Get( "Port" ) )

    oReg:Close()
 Else
    return {"","","Net Printer"}
 EndIf

Return aInfo

*/

/*
// Obtiene desde una Cadena los Valores de una o Varias Variables
*/
FUNCTION GetFromVar(cExp)
   LOCAL cVar:="",nAt1,nAt2,nLen,cText,aVar:=ARRAY(3)

   aVar[1]:=oDp:lBotar
   aVar[2]:=oDp:lMsgError
   aVar[3]:=oDp:lCallar

   IF LEFT(cExp,1)="&"
      cExp:=SUBS(cExp,2,LEN(cExp))
      RETURN MacroEje(cExp)
   ENDIF

//   oDp:oFrameDp:SetText(cExp)

   IF LEFT(cExp,1)="{" .AND. !"ODP:"$UPPER(cExp)

      cExp:=STRTRAN(cExp,"{","")
      cExp:=STRTRAN(cExp,"}")
      RETURN MacroEje(cExp)

   ENDIF


   nAt1:= AT("{",cExp)+1
   nAt2:=RAT("}",cExp)
   nLen:=nAt2-nAt1
   cVar:=Subs(cExp,nAt1,nLen)

   IF !EMPTY(cVar) .AND. nAt2>0

      oDp:lBotar      :=.F.
      oDp:lMsgError   :=.F. // Indica si Muestra el Mensaje de Error DpWin32
      oDp:lCallar     :=.T.

      cText:=MacroEje(cVar) // &cVar
      cText:=STRTRAN(cExp,"{"+cVar+"}",cText)

   ELSE

      cText:=cExp

   ENDIF

   oDp:lBotar   :=aVar[1]
   oDp:lMsgError:=aVar[2]
   oDp:lCallar  :=aVar[3]

RETURN cText

/*
// Devuelve el Valor Incremental de una Cadena,
// Acepta Fracciones en caracteres
FUNCTION AutoIncremental(uValue,lZero)
   LOCAL I,cSub:="",cNum:=""
   FOR I :=LEN(uValue) TO 1
     cSub:=RIGHT(uValue,I)
     IF ISALLDIGIT(cSub)
       cNum:=cSub
     ENDIF
   NEXT

   IF LEN(cNum)>0 .AND. LEN(cNum)!=LEN(uValue)
      ? cNum,uValue
   ENDIF

RETURN uValue
*/

/*
// Determina la Lista de Procedimientos llamados
// GetProce()
*/
FUNCTION GetProce(nMax)
   LOCAL cMemo:="",n
   n := 1    // we don't disscard any info again !

   DEFAULT nMax:=74

   WHILE ( n < nMax )
       if ! Empty(ProcName( n ) )
           cMemo:=cMemo + IIF( EMPTY(cMemo),"",CRLF) + "   Called from " + Trim( ProcName( n ) ) + ;
                           "(" + LSTR( ProcLine( n ) ) + ")"
      endif
          n++
   ENDDO

RETURN cMemo
/*
Detine el Focus, se pasa automáticamente
*/
FUNCTION DpWait(cText)
   LOCAL oDlg,oBtn
   DEFINE DIALOG oDlg FROM 0,0 TO 0,0
   @ 0,0 BUTTON oBtn PROMPT "Cerrar" ACTION oDlg:End()
   ACTIVATE DIALOG oDlg ON INIT (EVAL(oBtn:bAction),.F.)
   //  ON INIT(oDlg:End())
RETURN .T.

/*
// Conversion de Valores para SQL
*/
FUNCTION CTOSQL(uValue,lIsNull)

  DEFAULT lIsNull:=.F.

   DO CASE

      CASE ValType(uValue)="C"

          uValue:=STRSQL(uValue)

      CASE ValType(uValue)="N"

         uValue:=ALLTRIM(STR(uValue))
         RETURN uValue
      CASE ValType(uValue)="L"

         RETURN IIF( uValue,"1","0")

      CASE ValType(uValue)="D"

         IF EMPTY(uValue) .AND. (oDp:cTypeBD="MSSQL" .OR. lIsNull)
            // ? "AQUI ES, DEVUELVE NULL",uValue
            RETURN 'NULL'

         ENDIF

         // IF oDp:cTypeBD="MSSQL"
            // ? "SQLDATA",uValue
         // ENDIF

         uValue:=SQLDATE(uValue)

         // uValue:=DTOS(uValue)
         // uValue:=left(uValue,4)+"-"+SUBS(uValue,5,2)+"-"+RIGHT(uValue,2)
   ENDCASE

RETURN "'"+ALLTRIM(uValue)+"'"

/*
// Mensaje de Error
*/
FUNCTION MensajeErr(cMsg,cTitle,bValid)
   LOCAL oWndDefault:= GetWndDefault(),x,lValid:=(bValid!=NIL)
   LOCAL lResp
   LOCAL lRunTimer:=oDp:lRunTimer

   DEFAULT cTitle:=oDp:cTitleMsg,;
           oDp:lMsgSayProce   :=.F.,;
           oDp:lMsgErrGetProce:=.T.,;
           oDp:lMsgOff        :=.F.

   cMsg  :=CTOO(cMsg  ,"C")
   cTitle:=CTOO(cTitle,"C")

   IF oDp:lMsgSayProce
      cMsg:=cMsg+CRLF+GETPROCE()
   ENDIF

   IF ValType(bValid)='L'
      lResp:=bValid
   ELSE
      bValid:=IIF( bValid=NIL          , {||.T.}          , bValid )
      bValid:=IIF( ValType(bValid)="L" , {||bValid}       , bValid )
      bValid:=IIF( ValType(bValid)="C" , BloqueCod(bValid), bValid )
      lResp :=Eval(bValid)
   ENDIF

   IF ValType(cMsg)="L"
      cMsg:=CTOO(cMsg,"L")
   ENDIF

   IF Empty(cMsg) .OR. (lResp .AND. lValid)
      RETURN .T.
   ENDIF

   DEFAULT cMsg:=""

   // MsgInfo(GetProce())

   IF oDp:lMsgOff
       RETURN EJECUTAR("MSGOFF",cMsg,cTitle)
   ENDIF

   oDp:lRunTimer:=.F.

   MsgInfo(CTOO(cMsg,"C")+CRLF+IF(oDp:lMsgErrGetProce,GETPROCE(),""),CTOO(cTitle,"C")) // ,"MensajeErr"

   DPWRITE("TEMP\MensajeErr"+LSTR(SECONDS())+".txt",CTOO(cMsg)+CRLF+GETPROCE()+CRLF+oDp:cDpXbaseLine)
   SetWndDefault(oWndDefault) // Restaura Video

   oDp:lRunTimer:=lRunTimer

RETURN lResp

/*
// Convierte el Valor en Lógico
*/
FUNCTION CTOLOG(uValue)
   IF ValType(uValue)="N"
      uValue:=(uValue=1)
   ENDIF
RETURN uValue

FUNCTION GetPicture(aField,lMiles)
   LOCAL cPicture:=NIL
   LOCAL nLen,nDec,cNew

   nLen:=aField[3]
   nDec:=aField[4]

RETURN BuildPicture(nLen,nDec,lMiles)


   //cPicture:=REPLI("9",nLen-nDec+IIF( nDec>0,1 ,0 ))+;
   //          IIF( nDec>0, "." + REPLI("9",nDec),"")

//   cPicture:=REPLI("9",nLen-nDec+IIF( nDec>0,1 ,0 ))+;
//             IIF( nDec>0, "." + REPLI("9",nDec),"")

/*
   IF lMiles

      nDec:=AT(".",cPicture)
      nDec:=IIF( nDec=0,LEN(cPicture) ,nDec-1 )

      FOR I := nDec TO 1 STEP -1
        cNew:="9"+IIF( nTres>2,(nTres:=0,","),"")+cNew
        nTres++
      NEXT

      IF (nDec :=AT(".",cPicture),nDec>0)
         cNew:=cNew+SUBS(cPicture,nDec,LEN(cPicture))
      ENDIF

      RETURN cNew

   ENDIF
*/
   //? nLen,nDec,"nField",cPicture,"Picture"

// RETURN cPicture

/*
// Asigna Valores para un Objeto de Tipo Get
*/
FUNCTION DpSetVar(oGet,uValue)
    LOCAL cPicture,bWhen
    LOCAL cGets:="TGET,TMULTIGET,TSAY,TBMPGET"

    IF !oGet:ClassName()$cGets
       RETURN .F.
    ENDIF

    cPicture:=oGet:cPicture
    bWhen   :=oGet:bWhen

    DEFAULT uValue:=oGet:VarGet()

    oGet:Enable()

    IF cPicture!=nil
       oGet:SetText(tran(uValue,cPicture),.T.)
    ELSE
       oGet:SetText(uValue,.T.)
    ENDIF

    IF ValType(oGet:bWhen)="B" .AND. !EVAL(oGet:bWhen)
       oGet:Disable()
    ENDIF

    oGet:bWhen:=bWhen
//   oGet:Refresh(.T.)
//   ? "Halo " , oGet:VarGet()

RETURN .T.

/*
// Salva Variables de Tipo Privada y/o Publica
*/
FUNCTION SAVE_VAR(aArray)
  LOCAL aData1:={},aData2:={},cVar,nI,cType
  aArray=IF(VALTYPE(aArray)="C",_Vector(aArray),aArray)
  FOR nI=1 TO LEN(aArray)
    cVar :=aArray[nI]
    cType:=TYPE(cVar)
    IF cType="A"
      AADD(aData1,cVar) // {cVar,&cVar.})
      AADD(aData2,ACLONE(&cVar)) //
    ELSEIF !cType=[U]
      AADD(aData1,cVar) // {cVar,&cVar.})
      AADD(aData2,&cVar) //
    ENDIF
  NEXT nI
RETURN {aData1,aData2}

/*
// Funcion Mover(uValue,cVarName)
*/
FUNCTION MOVER( xDat , cNomVar  )

   IF xDat<>NIL .AND. ValType(cNomVAr)="C"
     __MVPUT(cNomVar,xDat) // ::aPublics[nFor][VAR_NAME], ::aPublics[nFor][VAR_VALUE])
     RETURN .T.
   ENDIF

RETURN .F. // IF( xDat <> NIL .AND. VALTYPE( cNomVar ) = "C" , &cNomVar := xDat , 0 ) <> NIL

/*
// Restaurar Variables PreGrabadas
*/
FUNCTION REST_VAR( aVars , lPub )

 IF EMPTY( aVars )
    RETURN .F.
 ENDIF

 DEFAULT lPub :=.F.

 AEVAL( aVars[1], {|cVar,nPos| IF( lPub , __qqPub( cVar ) , NIL ) , &( cVar ) := aVars[2, nPos ] } )

RETURN .T.

/*
// Determina en Texto la Relación Entre Dos Valores
*/
FUNCTION GetNumRel(nRecord,nCuantos)
   LOCAL cText,nLen
   nLen:=LEN(ALLTRIM(STR(nCuantos)))
RETURN STRZERO(nRecord,nLen)+"/"+STRZERO(nCuantos,nLen)

/*
// Convierte las Fecha en: Lunes 01 / Mayo / 2004
*/
FUNCTION CFECHA(dFecha)
   LOCAL cFecha:=CTOO(dFecha,"C")

RETURN cFecha:=ALLTRIM(CSEMANA(dFecha))    +;
               " "+LEFT(cFecha,2)   + " / "+;
               ALLTRIM(CMES(dFecha))+ " / "+;
               RIGHT(cFecha,4)

/*
// Dia de la Semana

*/
FUNCTION CSEMANA(dFecha,lUpper,lAllTrim)
     LOCAL nDia
     LOCAL cDia:="Domingo  "+"Lunes    "+"Martes   "+"Miercoles"+;
     +"Jueves   "+"Viernes  "+"Sábado   "

     DEFAULT dFecha:=oDp:dFecha,lUpper:=.F.,lAllTrim:=.T.

     IF ValType(dFecha)="N"
        nDia  :=dFecha
     ENDIF

     IF ValType(dFecha)="D"
        dFecha:=CTOO(dFecha,"D")
        nDia  :=DOW(dFecha)
     ENDIF

     cDia:=IIF(nDia=0,SPACE(9),SUBS(cDia,9*(nDia-1)+1,9))

     cDia:=IIF(lUpper  ,UPPER(cDia)   , cDia )
     cDia:=IIF(lAlltrim,ALLTRIM(cDia) , cDia )

RETURN cDia


/*
// Dia del Mes
*/
FUNCTION CMES(dFecha,lUpper)
     LOCAL cMES:="Enero     "+"Febrero   "+"Marzo     "+"Abril     "+"Mayo      "+"Junio     "+;
                 "Julio     "+"Agosto    "+"Septiembre"+"Octubre   "+"Noviembre "+"Diciembre "
     LOCAL nMes,cFecha

     DEFAULT dFecha:=oDp:Fecha,lUpper:=.F.

     IF ValType(dFecha)="N"
         RETURN SUBS(cMes,10*(dFecha-1)+1,10)
     ENDIF

     cFecha:=IIF( ValType(cFecha)="C",dFecha,SPACE(10))

     IF ValType(dFecha)="C" .AND. EMPTY(VAL(dFecha))
         RETURN dFecha
     ENDIF

     IF ValType(dFecha)="C" .AND. VAL(dFecha)<13
         dFecha:=VAL(dFecha)
     ENDIF

     IF ValType(dFecha)="C" .AND. !("/"$dFecha)
        nMes:=VAL(dFecha)
     ENDIF

     IF ValType(dFecha)="N"
        nMes  :=CTOO(dFecha,"N")
     ELSE
        dFecha:=CTOO(dFecha,"D")
        nMes  :=Month(dFecha)
     ENDIF

     IF lUpper
        cMes:=UPPER(cMes)
     ENDIF

RETURN IIF(nMes<1,cFecha,SUBS(cMes,10*(nMes-1)+1,10))

/*
Determina la Hora en Formato Am/Pm
*/
FUNCTION HORA_AP(cHora)
     LOCAL nHH,nMM,cAP

     DEFAULT cHora:=TIME()

     nHH:=VAL(SUBS(cHORA,1,2))
     nMM:=VAL(SUBS(cHORA,4,2))
     cAP:=[A]
     IF nHH>12
        nHH:=nHH-12
        cAP:=[P]
     ENDIF

RETURN STRZERO(nHH,2)+[:]+STRZERO(nMM,2)+cAP

/*
// Ejecuta Bloques de Código Dependiendo de Clases
// Util para Incluir Objetos dentro de un Arreglo
*/

FUNCTION BlqParam(bBlq,oPar1,oPar2,oPar3,oPar4)
RETURN {||EVAL(bBlq,oPar1,oPar3,oPar4)}

FUNCTION HANDLEEVENT()
RETURN NIL

FUNCTION BuildPicture(nLen,nDec,lMiles)
   LOCAL cPicture:=NIL,cNew:="",aChar
   LOCAL I,nTres:=0

   DEFAULT lMiles:=.F.

   cPicture:=REPLI("9",nLen-nDec+IIF( nDec>0,1 ,0 ))+;
             IIF( nDec>0, "." + REPLI("9",nDec),"")

   IF lMiles

      nDec:=AT(".",cPicture)
      nDec:=IIF( nDec=0,LEN(cPicture) ,nDec-1 )

      FOR I := nDec TO 1 STEP -1
        cNew:="9"+IIF( nTres>2,(nTres:=0,","),"")+cNew
        nTres++
      NEXT

      IF (nDec :=AT(".",cPicture),nDec>0)
         cNew:=cNew+SUBS(cPicture,nDec,LEN(cPicture))
      ENDIF

      RETURN cNew

   ENDIF

RETURN cPicture

/*
// Ejecuta MacroEjecución con Parámetros, Util para Ejecutar Programas en Modulos Hrb
*/
FUNCTION RunMacro(cExp,oPar1,oPar2,oPar3,oPar4)
   cExp:=MacroEje("{|oPar1,oPar2,oPar3,oPar4,oPar5|"+cExp+"}")
RETURN Eval(cExp,oPar1,oPar2,oPar3,oPar4)

/*
// Ejecuta MacroEjecución con Parámetros, Util para Ejecutar Programas en Modulos Hrb
*/
FUNCTION GenBlock(cExp,oPar1,oPar2,oPar3,oPar4)
   cExp:=MacroEje("{|oPar1,oPar2,oPar3,oPar4,oPar5|"+cExp+"}")
RETURN {||Eval(cExp,oPar1,oPar2,oPar3,oPar4)}


/* ============================================================================
 * FUNCTION aGetPrinters()
 * Revision: 21.05.2000 LK, Returns an array with all printers in the "Device"
 * <new193-5>               section of Win.ini or the Registry equivalent
 *                          Relies on modified GetProfStr() in profile.c
 *                          Works on on Win flavors (3.x/95/98/NT/W2K)
 *
 * ========================================================================= */

Function aGetPrinters()
Local aPrinters, cText, cToken := Chr(15)

// Passing no second param or passing ZERO to GetProfStr()
// grabs entire section in .ini file.  Each printer is separated by a
// null char (Chr(0)) and a CRLF.  We just strip them out
   cText := StrTran( StrTran( StrTran( ;
      GetProfString( "Devices", 0 ), Chr(0), cToken ), Chr(13) ), Chr(10) )
   aPrinters := Array( Len( cText ) - Len( StrTran( cText, cToken ) ) )
   AEval( aPrinters, {|cPrn,nEle| ;
      aPrinters[nEle] := StrToken( cText, nEle, cToken ) } )

Return aPrinters

/*
// Devuelve STR() sin Espacios
*/
FUNCTION LSTR(nValue,nLen,nDec)
   LOCAL cPicture
/*
   cPicture:="999,999,999,999"+IF(nDec<>NIL .AND. nDec>0,"."+REPLI("9",nDec),"")

RETURN ALLTRIM(FDP(nValue,cPicture))
*/
   IF nValue<>INT(nValue) .AND. nDec==NIL
      nDec:=2
   ENDIF

   DEFAULT nDec:=0,nLen:=16

RETURN ALLTRIM(STR(nValue,nLen,nDec))



/*
// Copiar en ClipBoard
*/
FUNCTION ClpCopy(cText,oDlg)


   IF oDlg=NIL

      DEFINE DIALOG oDlg FROM 0,0 TO 0,0
      ACTIVATE DIALOG oDlg ON INIT (oDlg:Hide(),oDlg:move(0,0,0,0,.t.),;
                                    cText:=CopyClipBoard(cText,oDlg),oDlg:End())

   ELSE

      RETURN CopyClipBoard(cText,oDlg)

   ENDIF

RETURN cText

FUNCTION CopyClipBoard(cText,oDlg)
   local oClp

   DEFINE CLIPBOARD oClp OF oDlg:oWnd ;
      FORMAT TEXT

   if oClp:Open()
      oClp:Clear()
      oClp:SetText( cText )
      oClp:End()
   else
      MsgAlert( "Clipboard no Está Disponible" )
   endif

return cText
/*
// Fecha de 8 digitos
*/
FUNCTION F8(dFecha) // Devueleve la fecha en 8 caracteres
     LOCAL cMES
     dFecha=EVAL(oDp:bFecha,dFecha)

     IF EMPTY(dFecha)
        RETURN "00/00/00"
     ENDIF

     cMes  =Left(CMES(dFecha),3)
     // MES  =SUBS("EnFbMrAbMyJnJlAgSeOcNvDi",2*(MES-1)+1,2)
RETURN STRZERO(DAY(dFecha),2)+cMes+STRZERO(YEAR(dFecha),4)

/*
// Determina la Antiguedad
// Vienen por Referencia:nAnos,nMeses,nDias
*/
FUNCTION ANTIGUEDAD(dDesde,dHasta,nAnos,nMeses,nDias)
   LOCAL dDesdeX

   DEFAULT dHasta:=oDp:dFecha

   dDesde :=EVAL(oDp:bFecha,dDesde)
   dHasta :=EVAL(oDp:bFecha,dHasta)
   nMeses:=MESES(dDesde,dHasta  )
   nAnos :=INT(DIV(nMeses,12))
   nMeses:=nMeses-(nAnos*12)
   IF DAY(dDesde)>DAY(dHasta)
     nDias:=(day(dHasta)+30)-day(dDesde)
   ELSE
     nDias :=DAY(dHasta)-DAY(dDesde)
   ENDIF
RETURN ""+STRZERO(nAnos,2)+"a,"+STRZERO(nMeses,2)+"m,"+STRZERO(nDias,2)+"d"

/*
// Determina los Meses entre dos Fecha
*/
FUNCTION MESES(dDesde,dHasta)
  LOCAL nMeses:=0,dFinMes:=.F.,dMesAtr,nDia,nAnos:=0

  dDesde:=EVAL(oDp:bFecha,dDesde)
  dHasta:=EVAL(oDp:bFecha,dHasta)

  IF dDesde>dHasta // Ini debe ser menor que fin
     dFinMes:=dHasta
     dHasta :=dDesde
     dDesde :=dFinMes
  ENDIF

  IF EMPTY(dDesde).OR.EMPTY(dHasta) .OR. dDesde>=dHasta // .OR. (TYPE("oNm")="O".AND.oNm:lValidar)
     RETURN 0
  ENDIF

  nMeses:=0
  nAnos :=0

  //  ANTIGUEDAD(dDesde,dHasta,@nAnos,@nMeses) // ,@nAnos,@nMeses,0)
  // RETURN (nAnos*12)+nMeses

  nDia     :=DAY(dDesde)
  dFinMes  :=(dDesde=FCHFINMES(dDesde)) // Indica si la fecha final es fin de mes
  dMesAtr  :=dDesde

  DO WHILE .T.

     dDesde++
        IF DAY(dDesde)=nDia.OR.(dFinMes.AND.(dDesde=FCHFINMES(dDesde))).OR. (MONTH(dDesde)=2.AND.nDia>=28.AND.DAY(dDesde)>=28.AND.!dFinMes.AND.MONTH(dDesde)<>MONTH(dMesAtr))
        dMesAtr:=dDesde
        nMeses :=nMeses+1
        IF nDia<15
           dDesde:=FCHFINMES(dDesde) // +nDia // VA AL FIN DEL MES
           dDesde:=dDesde+nDia-2
        ELSE
           dDesde:=FCHFINMES(dDesde)+1 // MAX(27,nDia) // IIF(nDia<28,nDia,28)
           dDesde:=dDesde+nDia-5 // 3
        ENDIF
     ENDIF
     IF dDesde>=dHasta
        EXIT
     ENDIF
  ENDDO
RETURN nMeses

DLL32 Function Apagar (uFlags As LONG, dwReserved AS LONG) ;
    AS LONG PASCAL FROM "ExitWindowsEx" LIB "USER32.DLL"

//----------------------------------------------------------
// Funcion del Win32API para eliminar un Item de una clave
//----------------------------------------------------------
DLL32 STATIC FUNCTION RegDeleteValueA( nhKey AS LONG  	  ,;
	                            cValueName  AS LPSTR ) ;
AS LONG PASCAL LIB "ADVAPI32.DLL"


FUNCTION RATA(nValor1,nVAlor2)
RETURN DIV(nValor1,nValor2)*100

/*
// AUNIQUE
*/
FUNCTION AUNIQUE(aLista)
   LOCAL aNew:={},nAt,I ,cMemo:=""

   FOR I=1 TO LEN(aLista)
     nAt   :=ASCAN(aNew,{|a,n|a==aLista[I]})
     IF nAt=0
       AADD(aNew,aLista[I])
     ENDIF
   NEXT

RETURN ACLONE(aNew)

/*
// Reduce el Arreglo
*/
FUNCTION ARREDUCE(aArray,nAt)

   IF ValType(aArray[nAt])="A" // Borrar Objetos
     Aeval(aArray[nAt],{|a,n|aArray[nAt,n]:=NIL})
   ENDIF

   IF ValType(aArray[nAt])="O" // Borrar Objetos
     aArray[nAt]:=NIL // Borrar Objetos
   ENDIF

   ADEL(aArray,nAt)
   ASIZE(aArray,Len(aArray)-1)

RETURN aArray


/*
// Determina si existe Impresora
*/
FUNCTION WISPRINTER(lUser,cModel)
//   LOCAL hDC:=1
   LOCAL nContar:=0,lRet

   DEFAULT lUser  :=.F.,;
           oDp:hDC:=0

   IF oDp:hDC>0
     RETURN .T.
   ENDIF

   oDp:hDC:=0

   CursorWait()

   oDp:hDC:=GetPrintDefault( GetActiveWindow() )
   lRet   :=oDp:hDC<>0

   IF ("*"$STR(oDp:hDC))
      lRet   :=.T.
   ENDIF

   DeleteDC(oDp:hDC)

   SysRefresh()

RETURN lRet // oDp:hDC>0 // !Empty(DpGetPrinter())

/*

   hDC  := GetPrintDefault( GetActiveWindow() )

   SysRefresh()

   DeleteDC(hDC)
*/

// RETURN hDC>0

/*
// Determina si existe Impresora
FUNCTION WISPRINTER(lUser,cModel)
   LOCAL hDC:=0,nContar:=0 // 1
   DEFAULT lUser:=.F.

   CursorWait()

   MensajeErr("AQUI")

   DEFAULT oDp:hDC:=0

   // IF oDp:hDC>0   // ya Existe
   //   RETURN .T.
   // ENDIF

   WHILE oDp:hDC=0 .OR. nContar++<3

     oDp:hDC:=GetPrintDefault( GetActiveWindow() )

? oDp:hDC,nContar

     DeleteDC(oDp:hDC)

     SysRefresh()

   ENDDO

RETURN oDp:hDC>0 // !Empty(DpGetPrinter())

   hDC  := GetPrintDefault( GetActiveWindow() )

   SysRefresh()

   DeleteDC(hDC)

RETURN hDC>0
*/


/*
// Determina la Fecha Del Ultimo Trimestre
*/
FUNCTION FCHINITRI(dFecha)
   LOCAL nMes,cYear,aFecha:={},I:=1,dFchIni

   DEFAULT dFecha:=oDp:dFecha

   dFecha:=EVAL(oDp:bFecha,dFecha)
   nMes  :=MONTH(dFecha)
   cYear :=STRZERO(YEAR(dFecha),4)

   AADD(aFecha,CTOD("01/01/"+cYear))
   AADD(aFecha,CTOD("01/04/"+cYear))
   AADD(aFecha,CTOD("01/07/"+cYear))
   AADD(aFecha,CTOD("01/10/"+cYear))

   FOR I := 1 TO LEN(aFecha)
     IF dFecha>=aFecha[I]
       dFchIni:=aFecha[I]
     ENDIF
   NEXT

RETURN dFchIni

/*
// Determina la Fecha Del Ultimo Trimestre
*/
FUNCTION FCHFINTRI(dFecha)
   LOCAL nMes,cYear,aFecha:={},I:=1,dFchFin

   DEFAULT dFecha:=oDp:dFecha

   dFecha:=EVAL(oDp:bFecha,dFecha)
   nMes  :=MONTH(dFecha)
   cYear :=STRZERO(YEAR(dFecha),4)

   AADD(aFecha,CTOD("31/03/"+cYear))
   AADD(aFecha,CTOD("30/06/"+cYear))
   AADD(aFecha,CTOD("30/09/"+cYear))
   AADD(aFecha,CTOD("31/12/"+cYear))

   FOR I:=LEN(aFecha) TO 1 STEP -1
     IF aFecha[I]>=dFecha
       dFchFin:=aFecha[I]
     ENDIF
   NEXT

RETURN dFchFin
/*
// Ejecutar Ayuda CHM
*/
FUNCTION RUNCHM()
    EJECUTAR("RUNCHM",oDp:cFileChm,oDp:cHelpTopic)
RETURN .T.

/*
// Salva Auditoría por Tabla
*/
FUNCTION AUDITAR(cTipo,lConfig,cTabla,cClave,cTabAud,oObj,cMemo,aFiles,cSClave,nNumero)
RETURN EJECUTAR("AUDITORIA", cTipo,lConfig,cTabla,cClave,cTabAud,oObj,cMemo,aFiles,cSClave,nNumero)

/*
   LOCAL oTable,nAt

   DEFAULT cTabAud:="DPAUDITORIA"

   IF !oDp:lAuditar
      RETURN .F.
   ENDIF

   DEFAULT lConfig:=.T.,cTabla:="",cClave:="" // Todo es Para Configuración

   IF !Empty(cTabla)

      aTablas:=GetTables()

      nAt   :=ASCAN(aTablas,{|aVal| aVal[2] == ALLTRIM(cTabla) })

      IF nAt>0 .AND. !aTablas[nAt,10]
         cTabAud:=oDp:cDpAudita
      ENDIF

   ENDIF

   oTable:=OpenTable("SELECT * FROM "+cTabAud+" WHERE 1=0 ",.F.)

//   ? oDp:dFecha,"EN AUDITORIA",cTabAud
   cClave:=IIF( ValType(cClave)="C" , STRTRAN(cClave,"'",'"') , cClave)

   oTable:Append()
   oTable:Replace("AUD_TIPO"  ,cTipo        )
   oTable:Replace("AUD_FECHAS",oDp:dFecha   )
   oTable:Replace("AUD_FECHAO",DATE()       )
   oTable:Replace("AUD_HORA  ",HORA_AP()    )
   oTable:Replace("AUD_TABLA ",cTabla       )
// oTable:Replace("AUD_CLAVE ",STRSQL(cClave))
   oTable:Replace("AUD_CLAVE ",cClave       )
   oTable:Replace("AUD_USUARI",oDp:cUsuario )
   oTable:Replace("AUD_ESTACI",oDp:cPcName  )
   oTable:Replace("AUD_IP"    ,oDp:cIpLocal )
   oTable:Commit()
   oTable:End()

RETURN .T.
*/

/*
// Verifica si todos los Tipos de datos son Iguales
*/
FUNCTION ISTYPE(uValue1,uValue2,uValue3,uValue4,uValue5)
   LOCAL lOk:=.T.,cType:=ValType(uValue1),I
   LOCAL aValue:={uValue1,uValue2,uValue3,uValue4,uValue5}

   FOR I := 2 TO LEN(aValue)
      IF ValType(aValue[I])!=cType
         lOk:=.T.
         EXIT
      ENDIF
   NEXT

RETURN lOk

/*
// Convierte un Dbf en Arreglo
*/
FUNCTION DBFTOARRAY(cFile)
   LOCAL aLine:={},aData:={},cAlias:=ALIAS()

   USE (cFile) EXCLU VIA "DBFCDX" NEW
   GOTO TOP
   aLine:=DBSTRUCT()
   AEVAL(aLine,{|a,i|aLine[i]:=a[1]})

   DBEVAL({||AEVAL(aLine,{|a,i|aLine[i]:=FieldGet(i)}),;
             AADD(aData,ACLONE(aLine))})

   USE

   DpSelect(cAlias)

RETURN aData

/*
// Convirte la Fecha SQL en Fecha XBASE
*/
FUNCTION SQLTODATE(cFecha,lIsNull)
   LOCAL dFecha
// ,nA,nM,nD,cA,cM,cD

   DEFAULT lIsNull:=.F.

   IF lIsNull .AND. Empty(cFecha)
      RETURN "NULL"
   ENDIF

   IF ValType(cFecha)="B"
      RETURN cFecha
   ENDIF

   IF Empty(cFecha)
      RETURN CTOD("")
   ENDIF

   IF oDp:nAAF=NIL
     oDp:cSqlDate:=UPPE(oDp:cSqlDate)
     oDp:nAAF:=AT("AAAA",oDp:cSqlDate)
     oDp:nMMF:=AT("MM"  ,oDp:cSqlDate)
     oDp:nDDF:=AT("DD"  ,oDp:cSqlDate)
   ENDIF
/*
? oDp:nAAF,oDp:nMMF,oDp:nDDF,cFecha,SUBS(cFecha,oDp:nDDF,2)+"/"+;
            SUBS(cFecha,oDp:nMMF,2)+"/"+;
            SUBS(cFecha,oDp:nAAF,4) ,oDp:cSqlDate
*/
RETURN CTOD(SUBS(cFecha,oDp:nDDF,2)+"/"+;
            SUBS(cFecha,oDp:nMMF,2)+"/"+;
            SUBS(cFecha,oDp:nAAF,4))
/*
   cA:=SUBS(cFecha,oDp:nAA,4)
   cM:=SUBS(cFecha,oDp:nMM,2)
   cD:=SUBS(cFecha,oDp:nDD,2)


   dFecha:=cD+"/"+cM+"/"+cA

// ? oDp:cSqlDate,cFecha,nA,nM,nD,cA,cM,cD

RETURN CTOD(dFecha)

/*
IF GetAsyncKey(VK_UP) .OR.;
    GetAsyncKey(VK_SHIFT,VK_TAB)
    RETURN (.T.)
ENDIF
*/

function tasaamort(i,n,capital,cuota)
//     i = tipo de interés
//     n = número de meses
     local potencia:=(1 + i  / 1200) ^ n
     LOCAL oGet
     cuota   :=capital*(i / 1200)*potencia/(potencia-1)
return cuota+capital
// RETURN potencia

/*
// Funcion Subir Utilizada en DP DOS
*/
FUNCTION Up()
  IF GetAsynckey(VK_UP).or.GetAsynckey(VK_SHIFT,VK_TAB)
     return.f.
  Endif
RETURN .T.

FUNCTION ViewArray(aData,cTitle,lOrder,lMdi,bRun,aHeader,oFont)
   LOCAL oRdd:=tRddArray():New(),I

   DEFAULT lMdi  :=.T.    ,;
           lOrder:=.F.

      // Quitar CRLF
   //   FOR I=1 TO LEN(aData)
   //      AEVAL(aData[I],{|a,n| IF(ValType(a)="C", aData[I,n]:=STRTRAN(a,CRLF,""), NIL)})
   //   NEXT I


   oDp:lViewArrayMdi:=lMdi

   oRdd:SetArray(aData)
   oRdd:bRun:=bRun

   oRdd:Browse(cTitle,lOrder,oFont,lMdi,aHeader,bRun)
   // oRdd:bRun:=bRun

   RETURN oRdd

FUNCTION MAXCHAR(cOld,cNew)
RETURN IIF(cOld>cNew,cOld,cNew)

FUNCTION ISBLOCK(bBlq,lEmptu)
RETURN ValType(bBlq)="B"

FUNCTION VALHORA_AP(oGet,lEmpty)
LOCAL cHH,cMM,nAt,cMsg:="",cHora,cAp

DEFAULT lEmpty:=.T.

IF ValType(oGet)="O"
   cHora:=oGet:VarGet(oGet)
ELSE

   cHora:=ALLTRIM(oGet)

   IF Empty(cHora) .AND. lEmpty
      RETURN .T.
   ENDIF
ENDIF

nAt  :=AT(":",cHora)
cHH  :=STRZERO(VAL(LEFT(cHora,nAt-1)),2)
cHora:=SUBS(cHora,nAt+1,Len(cHora))
nAt  :=AT(":",cHora)
cMM  :=STRZERO(VAL(LEFT(cHora,nAt-1)),2)
cAP  :=RIGHT(cAp,,nAt+1,Len(cHora))
nAt  :=AT(":",cHora)
cAP  :=UPPE(RIGHT(cHora,1))

IF cHH<"00" .OR. cHH>"12"
   cMsg:="Hora ["+cHH+"] Inválida, Indique: 00 hasta 12"
ENDIF

IF cMM<"00" .OR. cMM>"60"
   cMsg:=IIF( Empty(cMsg), "" , CRLF )+;
        "Minuto ["+cMM+"] Inválido, Indique: 00 hasta 60"
ENDIF

IF !(cAp$"AP")

   cMsg:=IIF( Empty(cMsg), "" , CRLF )+;
        "Horario Debe ser: [A] o [P] "

ENDIF

IF !Empty(cMsg)
   MensajeErr(cMsg,"Hora Inválida")
ENDIF

RETURN .T.

/*
// Ejecutar Servicios
*/
FUNCTION ServiceRun(cService)

   IF !ISSERVICERUN(cService)
     ISSERVICERUN("NET START "+cService)
     RETURN ISSERVICERUN(cService)
   ENDIF

RETURN .T.

/*
// Paralizar Servicios
*/
FUNCTION ServiceStop(cService)

   IF ISSERVICERUN(cService)

      ISSERVICERUN("NET STOP "+cService)

      RETURN ISSERVICERUN(cService)

   ENDIF

RETURN .F.

/*
// Determinar Servicios
*/
FUNCTION IsServiceRun(cService)

  LOCAL cRun :=""
  LOCAL cFile:="MySql.run"
  LOCAL cBat :="Mysql.Bat"
  LOCAL cMemo:="",lRun:=.f.

  FERASE(cBat)

  IF "NET "$cService
    cRun:=cService
  ELSE
    cRun:="NET START > "+cFile
    FERASE(cFile)
  ENDIF

  MemoWrit(cBat,cRun)
  CursorWait()
  WaitRun(cBat,0)

  IF !"NET "$cService
    cMemo:=uppe(MemoRead(cFile))
    lRun :=UPPE(alltrim(cService))$cMemo
    // ? cMemo
    // FERASE(cFile)
  ENDIF

  FERASE(cBat )

RETURN lRun
/*
FUNCTION RUNBAT(cRun)
  LOCAL cBat :="run.bat"
  LOCAL cMemo:=""

  FERASE(cBat)

  MemoWrit(cBat,cRun)
  CursorWait()
  WaitRun(cBat,0)

  FERASE(cBat )

RETURN .T.
*/
FUNCTION MensajeInfo(cText,cTitle,bValid)
    LOCAL lResp:=.F.

    DEFAULT bValid:={||.F.}

    bValid:=BloqueCod(bValid)
    lResp :=EVAL(bValid)

    IF !lResp
       MsgInfo(cText,cTitle)
    ENDIF

RETURN lResp

/*
// Formato DataPro
*/
FUNCTION FDP(nValue,cPict,lDec,lRtrim,lEmpty)

   LOCAL cExp:=TRAN(nValue,cPict),nAt:=0,nLen

   DEFAULT lDec:=.T.,lRtrim:=.F.,lEmpty:=.F.

   // Plan de cuentas no puede cambiarle los puntos por ,
   IF ValType(nValue)="C"
      RETURN nValue
   ENDIF

   IF lEmpty .AND. Empty(nValue)
      RETURN SPACE(LEN(cPict))
   ENDIF

//   IF !oDp:lSpanish .OR. ValType(nValue)<>"N"
//      RETURN TRAN(nValue,cPict)
//   ENDIF

   IF !lDec  // Sin Decimales

     IF (nAt:=AT(".",cExp),nValue=Int(nValue) .AND. nAt>0)
        nLen:=LEN(cExp)
        cExp:=LEFT(cExp,nAt-1)
        cExp:=PADR(cExp,nLen)
     ENDIF

   ENDIF

//   IF !oDp:lFdp
//      RETURN cExp
//   ENDIF

   cExp:=STRTRAN(cExp,".",CHR(2))
   cExp:=STRTRAN(cExp,",",".")
   cExp:=STRTRAN(cExp,CHR(2),",")

   IF lRtrim
      cExp:=LTRIM(cExp)
   ENDIF

RETURN cExp

/*
// Determina los Decimales
*/
FUNCTION FDEC(nValue,nMax)
   LOCAL nDec:=nValue-Int(nValue),cExp

   IF nDec>0 // Hay Decimales

      cExp:=ALLTRIM(STR(nDec))

      WHILE RIGHT(cExp,1)="0"
         cExp:=LEFT(cExp,LEN(cExp)-1)
      ENDDO

      nDec:=LEN(cExp)-2

      DEFAULT nMax:=nDec

      nMax:=MIN(nMax,nDec)

      RETURN "."+REPLI("9",nMax)

   ENDIF

RETURN ""

FUNCTION MensajeSN(cText1,cText2)
RETURN MsgYesNo(cText1,cText2)

FUNCTION MensajeNS(cText1,cText2)
RETURN MsgNoYes(cText1,cText2)
/*
FUNCTION ASQL(cSql)

  LOCAL oTab,e:=OpenTable(cSql,.T.)

  oTable:End()
  oDp:cSql:=cSql
  MensajeErr(cSql,"SQL")
  MensajeErr(oDp:cSql,"oDp:cSql")

RETURN oTable:aDataFill
*/

Function SayLine(oGet)
   LOCAL n, hPen, hOldPen,x:=0,y:=0,nAncho:=10, nAlto:=20

   IF oGet=NIL .OR. .T.
      RETURN .F.
   ENDIF

   nAlto   :=oGet:Height()
   nAncho  :=oGet:Width()

   oGet:GetDc()
   hPen := CreatePen(PS_SOLID , 100, CLR_BLACK )
   hOldPen := SelectObject( oGet:hDc, hPen )

   MoveTo( oGet:hDC, y, x )
   LineTo( oGet:hDC, nAncho, nAlto)

   SelectObject( oGet:hDc, hOldPen )
   DeleteObject( hPen )
   oGet:ReleaseDc()

return NIL

// Tumbar Programa TerminateA( GetModuleHandle( cAppName )

FUNCTION DpGetPrinter()
   LOCAL cPrinter := GetProfString( "windows", "device" , "" )

   oDp:lIsPrint:=!Empty(cPrinter)

   IF !Empty(cPrinter) .AND. oDp:lIsPrint
      cPrinter:=PrnGetName()
   ENDIF

RETURN cPrinter

/*
FUNCTION WQout( aData )
  MensajeErr(GetProce(),aData[1])
RETURN .T.
*/

FUNCTION SayAction(oSay,bAction)

   bAction:=BloqueCod(bAction)

   oSay:lWantClick   := .t.
   oSay:bLClicked    := {||oSay:Capture() , EVAL(bAction), ReleaseCapture()}

Return oSay:bLClicked


FUNCTION ENCRIPT(cText,lEncrip)
     LOCAL c,cNew:="",nIni:=99,I,nLen

     IF EMPTY(cText)
        RETURN cText
     ENDIF

     nLen :=LEN(cText)
     cText=ALLTRIM(cText)

     // DESEMCRIPTAR
     FOR I=LEN(cText) TO 1 STEP -1
         C:=SUBS(cText,I,1)
         C:=ASC(C)+IIF(lEncrip,nIni,-1*nIni)
         cNew:=cNew+CHR(C)
     NEXT I

     IF !lEncrip
        cNew:=PADR(cNew,nLen)
     ENDIF

RETURN cNew

/*
// Asignación de Eventos
*/
FUNCTION DpSetTimer(bAction,cID,nSeconds)
   LOCAL nAt

   DEFAULT aTimers:={}

   DEFAULT oDp:nTimerSeconds:=60*5,;
           cId     :=LSTR(LEN(aTimers)+1),;
           nSeconds:=60*10,;
           oDp:lSayTimer:=.F.

   oDp:lRunTimer:=.T. // Indica que no debe Parar los Timers

   DPKILLTIMER(cId)

   IF oDp:oTimer=NIL

     // DEFINE TIMER oDp:oTimer INTERVAL oDp:nTimerSeconds OF oDp:oFrameDp ACTION RunTimer()
     DEFINE TIMER oDp:oTimer INTERVAL nSeconds OF oDp:oFrameDp ACTION RunTimer()
     ACTIVATE TIMER oDp:oTimer

   ENDIF


   // DEFINE TIMER  oDp:oTimer OF oDp:oFrameDp INTERVAL 50000 ACTION ClearDpEdit()
   // ACTIVATE TIMER oDp:oTimer

//  oDp:oTimer:ClassName(),cID

   bAction:=BLOQUECOD(bAction)
   nAt    :=ASCAN(aTimers,{|a,n|a[1]=cID})

   IF nAt=0
      AADD(aTimers,{cId,bAction,nSeconds,0})
   ELSE
      aTimers[nAt]:={cId,bAction,nSeconds,0}
   ENDIF

   SetTimerOn()

   oDp:aTimers:=aTimers

RETURN .T.

/*
// Finalizacion del Timer
*/
FUNCTION DPKILLTIMER(cId)

    LOCAL nAt

    DEFAULT cId     :=LSTR(LEN(aTimers)+0)

    nAt:=ASCAN(aTimers,{|a,n|a[1]=cID})

    IF nAt>0
       aTimers:=ARREDUCE(aTimers,nAt)
//    ELSE
//       MensajeErr("TIMER","No Existe")
    ENDIF

    SetTimerOn()

RETURN .T.

/*
// Ejecuta todos los Timers
*/
STATIC FUNCTION RunTimer()

   LOCAL I

 //  oDp:oFrameDp:SetText("RunTimer")

   IF !Empty(oDp:cFileToScr) // 04/02/2024
      RETURN .T.
   ENDIF

   IF !oDp:lRunTimer

      IF oDp:lSayTimer
       oDp:oFrameDp:SetText(TIME()+" #"+LSTR(LEN(aTimers))+" Desactivado")
      ENDIF

      RETURN .T.
   ENDIF

// ErrorSys(.T.)

   IF oDp:lSayTimer
      oDp:oFrameDp:SetText(TIME()+" #"+LSTR(LEN(aTimers)))
   ENDIF

   FOR I=1 TO LEN(aTimers)

      IF oDp:lSayTimer
         oDp:oFrameDp:SetText(TIME()+" #"+LSTR(LEN(aTimers))+" / RunTimer "+aTimers[I,1]+" / "+LSTR((ABS(Seconds()-aTimers[I,4])))+" >= "+LSTR(aTimers[I,3]) )
      ENDIF

      IF aTimers[I,4]=0 .OR. (ABS(Seconds()-aTimers[I,4]))>=aTimers[I,3]
         EVAL(aTimers[I,2])
         aTimers[I,4]:=Seconds()
      ENDIF

   NEXT I

RETURN .T.

// Desactiva el Timer
FUNCTION SetTimerOff()

   oDp:lRunTimer:=.F.

RETURN  .t.

// Activa el Timer
FUNCTION SetTimerOn()

   oDp:lRunTimer:=.T.

RETURN  .t.

/*
// Redondea de 5 a 0
*/
FUNCTION DP_INT2D(nValue)

    IF !nValue=0
      nValue:=VAL(LEFT(STR(nValue,19,3),18)) // JN 8/3/2008
    ENDIF

RETURN nValue

FUNCTION DPALLTRIM(uValue)
RETURN IF( Empty(uValue),"",ALLTRIM(uValue))

FUNCTION XROUND(nNumero,nDec)
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


FUNCTION DPINT(nValor,nDec)
     // cValor:=STR(cValor*100,20,0)
RETURN VAL( LEFT( STR(nValor),AT(".",STR(nValor) ) -1 ) )

FUNCTION MINCHAR(cOld,cNew)
RETURN IIF(cOld<cNew,cOld,cNew)

/*
// VALSTRTRAN
*/
FUNCTION VALSTRTRAN(nMonto,cPicture)
    LOCAL lMenos:=(nMonto<0)

    DEFAULT cPicture:="9999999999999.99"

    nMonto:=STRTRAN(TRAN(nMonto,cPicture),",","")
    nMonto:=STRTRAN(nMonto,"-","")
    nMonto:=ABS(VAL(nMonto))*IF(lMenos,-1,1)

RETURN nMonto

/*
Despliega Matriz de Datos en un Mensaje
Trabaja en combinacion con el
comando print ó ??, similiar a ?
*/
FUNCTION APrint( aData, cTitle , lShow )
  Local cMsg:=""

  Default cTitle:="Modo Debug (Activado)",;
          lShow :=.T.

  AEVAL( aData,{ |a| cMsg:=cMsg+CTOO(a,"C")+" " } )

  IF lShow
    MsgInfo( cMsg , cTitle )
  ENDIF

RETURN cMsg

FUNCTION IFFRMWND(cVar,cMethod)
    LOCAL oFrm

    IF TYPE(cVar)="O"

       oFrm:=&cVar.

       IF ValType(oFrm:oWnd)="O" .AND. oFrm:oWnd:hWnd>0
          RETURN EVAL(BloqueCod(cMethod))
       ENDIF

    ENDIF

RETURN NIL
/*
// Concatenación de Cadenas (Acumulador) con Separador
*/

FUNCTION DPCONCAT(cValor,cSep,cNew)

  DEFAULT cValor:="",;
          cSep  :=CRLF

  IF Empty(cNew)
     RETURN cValor
  ENDIF

  cValor:=cValor +IIF( Empty(cValor), "" , cSep ) + cNew

RETURN cValor

/*
//   Indicar si son pc, autorizados para Actualizar el sistema
*/
FUNCTION ISPCDP(cRef)
   LOCAL lResp:=.F.
   LOCAL cIp  :=ALLTRIM(GETHOSTBYNAME())
   LOCAL cName:=ALLTRIM(LOWER(NETNAME()))

   cName:=LOWER(cName)
   cRef :=cIp+"+"+cName

   IF (cIp="192.168.10.29" .AND. cName="jnavas")
      lResp:=.T.
   ENDIF


   IF (cIp="192.168.10.32"  .AND. cName="progdata")
      lResp:=.T.
   ENDIF

RETURN lResp

/*
// Inserta un Nuevo Elemento en un Arreglo
*/
FUNCTION AINSERTAR(aData,nAt,uValue)

   DEFAULT nAt:=LEN(aData)+1

   AADD(aData,NIL)
   AINS(aData,nAt)
   aData[nAt]:=uValue

RETURN aData


FUNCTION DpWrite(cFile,cMemo)
   LOCAL oFile

   ferase(cFile)

   IF FILE(cFile)
      RETURN .F.
   ENDIF

   oFile:=TFile():New( cFile ) // "dp\dpftplog.dll" )
   oFile:PutStr(cMemo)
   oFile:Close()

RETURN FILE(cFile)


// EOF
/*
FUNCTION MsgAlert(cText)
return WQout(getproce(),cText)

FUNCTION  WQout(cText,cTitle)
RETURN MsgInfo(GetProce(),cText)
*/

//FUNCTION MyMsgError( cError, cTitle )
//RETURN MensajeErr(GetProce(),cError)
/*
FUNCTION MYMSGINFO(cMsg,cError)
RETURN MensajeErr("MYSGINFO"+GetProce(),cError)

FUNCTION MYMSGERROR(cMsg,cError)
Return MensajeErr("MYMSGERR"+GetProce(),cError)
*/

//----------------------------------------------------------------------------//
// Detemina el Tiempo sib uso del Sistema por parte del Usuario
// Este valo se calcula desde el Control TCONTROL
//----------------------------------------------------------------------------//
FUNCTION DpGetTime(lShow)

   DEFAULT oDp:nTimeOn:=0,;
           oDp:cTimeOn:=TIME(),;
           lShow      :=.F.

   oDp:nTimeOff:=SECONDS()- oDp:nTimeOn
   oDp:nTimeOn :=SECONDS() // Ultimo Control Focalizado
   oDp:cTimeOff:=oDp:cTimeOn
   oDp:cTimeOn :=TIME()

   IF oDp:oFrameDp<>NIL .AND. lShow
      oDp:oFrameDp:SetText(LSTR(oDp:nTimeOff)+" "+oDp:cTimeOff+" "+oDp:cTimeOn)
   ENDIF

RETURN oDp:nTimeOff
/*
// Devuelve .T. si el reporte existe
*/
FUNCTION ISREPORTE(cCodRep)
RETURN ISSQLGET("DPREPORTES","REP_CODIGO",cCodRep)

// EOF
FUNCTION WHEREGET(uValue,cOper)
   LOCAL cWhere:=""

   DEFAULT cOper:="="

   IF cOper=="LIKE" .AND. ValType(uValue)="C"
      cWhere:=" LIKE "+GetWhere("","%"+ALLTRIM(uValue)+"%")
      cOper :=""
      RETURN cWhere
   ENDIF

   cWhere:=GetWhere(cOper,uValue)

RETURN cWhere

// Depura Valores vacios
// Jn 30/08/2014
FUNCTION ADEPURA(aArray,bBlq)
   LOCAL nAt,aNew:={}

   IF LEN(aArray)>0 .AND. ValType(aArray[1])="A" .AND. LEN(aArray[1])>1

     DEFAULT bBlq:={|a,n| Empty(a[1])}

   ELSE

     DEFAULT bBlq:={|a,n| Empty(a)}

   ENDIF

/*
   ASCAN(aArray,{|a,n,nAt| nAt:=ASCAN(aArray,bBlq) ,;
                           IF(nAt=0,AADD(aNew,a),NIL)})
   aArray:=ACLONE(aNew)
*/
// Si cumple la condicion, es depurado


   WHILE .T.

      nAt:=ASCAN(aArray,bBlq)

      IF nAt=0
         EXIT
      ENDIF

      ARREDUCE(aArray,nAt)

   ENDDO


RETURN aArray

FUNCTION ISFILESTD(cFile,lSay,lFile)
   LOCAL cMemo:=""

   DEFAULT lSay :=.T. ,;
           lFile:=.F.

//? "AQUI ES ISFILSTD()"

   IF !ISDPSTD() .AND. !lFile

      IF !FILE(cFile)
         IF lSay
            MensajeErr("Archivo "+cFile+" no Existe")
         ENDIF
         RETURN .F.
      ENDIF

      RETURN .T.

   ENDIF

// cMemo:=GETFILESTD(cFile,lSay,lFile)
   cMemo:=GETFILESTD(cFile,lSay,.F.,.F.)

//?  cFile,cMemo,lFile

RETURN !Empty(cMemo)


FUNCTION GETFILESTD(cFile,lSay,lFile,lRefresh)
  LOCAL cExt,cText,oScript
  LOCAL cFileStd,cFileInd,cDir:="DPSTD\"
  LOCAL aFiles,nAt,oFile,nFor
  LOCAL cFileOrg

  DEFAULT cFile   :="DPXBASE\DPCBTEPAGO.DXB",;
          lSay    :=.T.,;
          lFile   :=.F.,;
          lRefresh:=.F.

  DEFAULT aStdFiles:={}

  /*
  // JN 07/01/2018 (No depende de DPSTD
  */

  IF FILE(cFile)
     RETURN MemoRead(cFile)
  ENDIF

  IF lRefresh
     AEVAL(aStdFiles,{|a,n| a[3]:Close() })
     aStdFiles:={}
  ENDIF

  IF !ISDPSTD() .AND. !lFile

     IF !FILE(cFile)
        // Deberar Buscarlo en AdaptaPro Server
        // EJECUTAR("DPAPTGET",cFile)
        EJECUTAR("DPDIRAPLDOWN",cFileNoPath(cFile),cFilePath(cFile),.F.,.T.)
     ENDIF

     RETURN MemoRead(cFile)
  ENDIF

  cFileOrg:=cFile
  cFile   :=UPPE(ALLTRIM(cFile))
  cFile   :=cFileNoPath(cFile)
  cExt    :=cFileExt(cFile)
  cFile   :=cFileNoExt(cFile)
  nFor    :=ASCAN(aStdFiles,{|a,n| a[1]==cExt})
  cFileStd:=cDir+"DP"+cExt+"."+cExt+"X"

//oDp:lTracer:=.t.
//? "aqui",getproce()

  IF nFor=0

    cFileInd:=cDir+"DP"+cExt+"."+cExt+"I"
    cText   :=MemoRead(cFileInd)

//? cText

    aFiles  :=_VECTOR(cText,";")

    AEVAL(aFiles,{|a,n,x| aFiles[n]:=_VECTOR(a,":")})

    AEVAL(aFiles,{|a,n| aFiles[n  ]:=_VECTOR(a,":")  ,;
                        aFiles[n,2]:=VAL(aFiles[n,2]),;
                        aFiles[n,3]:=VAL(aFiles[n,3]),;
                        aFiles[n,4]:="" })

    oFile:=TFile():New( cFileStd, 0 ) // Solo Lectura

    AADD(aStdFiles,{cExt,aFiles,oFile})

    nFor:=LEN(aStdFiles)

  ELSE

    aFiles:=aStdFiles[nFor,2]

  ENDIF

  // Precision en la Busqueda del Componente
  // oDp:aFiles_Std:=aFiles
  nAt  :=ASCAN(aFiles,{|a,n|a[1]==cFile})
  cText:=""

  IF nAt>0

    IF Empty(aFiles[nAt,4])
      oFile:=aStdFiles[nFor,3]
      oFile:GoTo(aFiles[nAt,2])
      cText:=oFile:cGetStr(aFiles[nAt,3])
      cText:=HB_UNCOMPRESS(4096*400,cText)
      aFiles[nAt,4]:=cText
    ELSE
      cText:=aFiles[nAt,4]
    ENDIF

    IF Empty(cText) .AND. lSay

       MensajeErr("Componente "+cFile+" está Vacio")

       IF !FILE(cFile)
         // EJECUTAR("DPAPTGET",cFilePpo)
         EJECUTAR("DPDIRAPLDOWN",cFileNoPath(cFile),cFilePath(cFile),.F.,.T.)
       ENDIF

       // EJECUTAR("DPAPTGET",cFileOrg)

    ENDIF

  ELSE

     IF (!FILE(cFileOrg) .OR. cExt="DXB" .OR. cExt="DXBX") .AND. !Empty(OpenOdbc(oDp:cDsnConfig):GetTables()) // Debe existe la tabla para que pueda registrase, cuando arranca y no tiene tablas no puede hacer transacciones jn 18/07/2018

      cText:=SQLGET("DPPROGRA","PRG_TEXTO","PRG_CODIGO"+GetWhere("=",cFile))

      IF !Empty(cText)
//   ? "ENCONTRADO EN CODIGO FUENTE, requiere compilar "+cFileOrg
        oScript:=TScript():New("")
        oScript:cProgram:=cFile
        oScript:Reset()
        oScript:lPreProcess := .T.
        oScript:cClpFlags   := "/i"+Alltrim(oDp:cPathInc) //
        oScript:Compile(cText)
        oScript:SavePCode(cFileOrg)
      ENDIF

     ENDIF

     IF lSay
//     MensajeErr("Componente "+cFileOrg+" no Encontrado en "+cFileStd+CRLF+GETPROCE())
       // EJECUTAR("DPAPTGET",cFileOrg)
       // EJECUTAR("DPAPTGET",cFilePpo)
       EJECUTAR("DPDIRAPLDOWN",cFileNoPath(cFileOrg),cFilePath(cFileOrg),.F.,.T.)

     ENDIF


     // Debe Verificar Nuevamente su descarga

  ENDIF

RETURN cText


FUNCTION MSGDEMO()
   LOCAL lDemo:=.F.

   DEFAULT oDp:dFecha:=DATE()

//   SETLIMITREC("DPUSUARIOS",3)
//   SETLIMITREC("DPEMPRESAS",3)

   IF lDemo
     MensajeErr(oDp:cSys,"Demostración")
   ENDIF

   IF lDemo .AND. oDp:dFecha>=CTOD("30/08/2014")
      SALIR()
   ENDIF

RETURN .T.
/*
// Necesaria para buscar mensajes (Policias) mediante ?
FUNCTION WQout()
RETURN MensajeErr(GETPROCE())
*/

FUNCTION GETCLAVEDPX()
   MensajeErr("El Sistema "+oDp:cDpSys+" será cerrado")
RETURN SALIR()

FUNCTION GETCLAVE()
   MensajeErr("El Sistema "+oDp:cDpSys+" será cerrado")
RETURN SALIR()



/*
CODE: SELECT ALL  EXPAND VIEW, leer pagina Web
waitRun( "wget.exe -q -r -t6 --no-check-certificate miurl\miarchivo.txt -O c:\miarchivo.txt", 0 )
*/

/*

CLASS TWord
      DATA  oWord
      DATA  oDoc

      METHOD New()
      METHOD OpenDoc( cNombreDoc )
      METHOD Replace()
      METHOD SaveDocumento()
      METHOD Visible  INLINE ::oWord:Visible := .t.

ENDCLASS

METHOD   NEW()  CLASS TWord
  IF ( ::oWord := win_oleCreateObject( "Word.Application" ) )= NIL
   Msgstop(" Error al conectar con Word")
   return
  ENDIF

RETURN( Self )

METHOD OpenDoc( cNombreDoc )  CLASS TWord

 ::oDoc:=::oWord:Documents:open(cNombreDoc)

RETURN Nil

METHOD SaveDocumento(cNombreDoc) CLASS TWord

 ::oDoc:saveAs(cNombreDoc)

Return nil


METHOD Replace(cSrc, cRpl)   CLASS TWord
local oSel:= ::oWord:Selection


oSel:Start = 0
oSel:End = -1

WHILE oSel:Find:Execute( cSrc )
     oSel:Range:Text = cRpl
ENDDO

Return Self
*/
// eof


FUNCTION TESTFOTO()
RETURN NIL


/*
// Copia un Cursor hacia Dbf
*/
FUNCTION CopytoDbf(oCursor,cFile,oMeterR,cVia,lConvert,lOptions)
      LOCAL cAlias:=ALIAS(),cIndex,I,nLen,nField,uValue,cField

      cFile :=cFile + IIF( !"."$cFile ,".DBF" , "" )
      cIndex:=STRTRAN(UPPE(cFile),".DBF",".CDX")

//   ? "aqui"
//    ErrorSys(.t.)

      DEFAULT lConvert:=.T.,lOptions:=.F.

      CURSORWAIT()

      IF lOptions

//        oCursor:Browse()
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

        // ViewArray(oCursor:aFields)
        // oCursor:Browse()

      ENDIF

      // ? "ESTOY EN COPYTODBF"

      DEFAULT cVia:=RDDSETDEFAULT()

      FERASE(cFile)

      IF File(cFile)
         MensajeErr("Fichero "+cFile+" está en Uso")
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


/*
// Hace la Conversión de los Valores de los Campos
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

/*
Definicion de funciones

long __declspec(dllexport) BuscarRif_Create()
void __declspec(dllexport) BuscarRif_SetCaptcha( long p, LPSTR captcha)
LPSTR __declspec(dllexport) BuscarRif_Buscar(long p, LPSTR rif)
void __declspec(dllexport) BuscarRif_Free(long p)

Ejemplo de uso en c++

// Iniciamos el objeto BuscarRif
auto p = BuscarRif_Create();

// Mostramos el captcha y esperamos la captura por parte del usuario
std::string cap;
puts("Escribe Captcha");
std::cin >> cap;

// Seteamos el captcha resuelto
BuscarRif_SetCaptcha(p, (LPSTR)cap.c_str());

// Buscamos el RIF
auto res = BuscarRif_Buscar(p, (LPSTR)"J312344202");
*/

// Utilizar la funcion de Harbour // 08/10/2018
// function osend(uPar1,uPar2)
// RETURN __objSendMsg( uPar1, uPar2 )

REQUEST EASYONE_XMLTOPDF_SETMAILINFOEX
REQUEST EASYONE_XMLTOPDF_SETMAILINFO
REQUEST EASYONE_XMLTOPDF_SETLICENCE
REQUEST EASYONE_XMLTOPDF_SETFILES
REQUEST EASYONE_XMLTOPDF_SENDMAIL
REQUEST EASYONE_XMLTOPDF_QUERYXML
REQUEST EASYONE_XMLTOPDF_GETERROR
REQUEST EASYONE_XMLTOPDF_FREE
REQUEST EASYONE_XMLTOPDF_CREATEPDF
REQUEST EASYONE_XMLTOPDF_CREATE
REQUEST EASYONE_XMLTOPDF_ADDLABEL
REQUEST EASYONE_XML33_FREE
REQUEST EASYONE_XML33_CREATE
REQUEST EASYONE_VENTAVEHICULOS11_SETVENTAVEHICULOS
REQUEST EASYONE_VENTAVEHICULOS11_FREE
REQUEST EASYONE_VENTAVEHICULOS11_CREATE
REQUEST EASYONE_VENTAVEHICULOS11_ADDPARTEINFORMACIONADUANERA
REQUEST EASYONE_VENTAVEHICULOS11_ADDPARTE
REQUEST EASYONE_VENTAVEHICULOS11_ADDINFORMACIONADUANERA
REQUEST EASYONE_VEHICULOUSADO10_SETVEHICULOUSADO
REQUEST EASYONE_VEHICULOUSADO10_FREE
REQUEST EASYONE_VEHICULOUSADO10_CREATE
REQUEST EASYONE_VEHICULOUSADO10_ADDINFORMACIONADUANERA
REQUEST EASYONE_VALESDEDESPENSA10_SETVALESDEDESPENSA
REQUEST EASYONE_VALESDEDESPENSA10_FREE
REQUEST EASYONE_VALESDEDESPENSA10_CREATE
REQUEST EASYONE_VALESDEDESPENSA10_ADDCONCEPTO
REQUEST EASYONE_USECURL
REQUEST EASYONE_TPE10_SETTURISTAPASAJEROEXTRANJERO
REQUEST EASYONE_TPE10_SETDATOSTRANSITO
REQUEST EASYONE_TPE10_FREE
REQUEST EASYONE_TPE10_CREATE
REQUEST EASYONE_TIMBRADOMASIVO_SETFACHADACOREPRUEBAS
REQUEST EASYONE_TIMBRADOMASIVO_SETCONECTORPRUEBAS
REQUEST EASYONE_TIMBRADOMASIVO_SETCERTIFICADO
REQUEST EASYONE_TIMBRADOMASIVO_PROCESACOMPROBANTES
REQUEST EASYONE_TIMBRADOMASIVO_FREE
REQUEST EASYONE_TIMBRADOMASIVO_CREATE
REQUEST EASYONE_TIMBRADOMASIVO_ADDCOMPROBANTE
REQUEST EASYONE_TERCEROS11_SETPORCUENTADETERCEROS
REQUEST EASYONE_TERCEROS11_SETINFORMACIONFISCALTERCERO
REQUEST EASYONE_TERCEROS11_SETINFORMACIONADUANERA
REQUEST EASYONE_TERCEROS11_SETCUENTAPREDIAL
REQUEST EASYONE_TERCEROS11_FREE
REQUEST EASYONE_TERCEROS11_CREATE
REQUEST EASYONE_TERCEROS11_ADDTRASLADO
REQUEST EASYONE_TERCEROS11_ADDRETENCION
REQUEST EASYONE_TERCEROS11_ADDPARTEINFORMACIONADUANERA
REQUEST EASYONE_TERCEROS11_ADDPARTE
REQUEST EASYONE_SHOWRESULTADO
REQUEST EASYONE_SETQRCODEFORMAT
REQUEST EASYONE_SETLOGFILE
REQUEST EASYONE_SETLOGAS
REQUEST EASYONE_SETINVOICEONEFACHADAINFOPRUEBA
REQUEST EASYONE_SETINVOICEONEFACHADAINFO
REQUEST EASYONE_SETFEL
REQUEST EASYONE_SETFACHADAIOPRUEBAS
REQUEST EASYONE_SETFACHADAIOPRODUCCION
REQUEST EASYONE_SETDESARROLLOPRUEBAS
REQUEST EASYONE_SETDESARROLLOPRODUCCION
REQUEST EASYONE_SETDESARROLLOFACHADACOREPRUEBAS
REQUEST EASYONE_SETDESARROLLOFACHADACOREPRODUCCION
REQUEST EASYONE_SETCURLPROXY
REQUEST EASYONE_SETCOREPRUEBAS
REQUEST EASYONE_SETCOREPRODUCCION
REQUEST EASYONE_SETCONECTORPRUEBAS
REQUEST EASYONE_SETCONECTORPRODUCCION
REQUEST EASYONE_SETCONECTORCFDI
REQUEST EASYONE_SETADDENDAXML
REQUEST EASYONE_SETADDENDATEXT
REQUEST EASYONE_SERVICIOSAT_VALIDARDESCARGA
REQUEST EASYONE_SERVICIOSAT_VALIDALICENCIA
REQUEST EASYONE_SERVICIOSAT_SETSAVEFILENAME
REQUEST EASYONE_SERVICIOSAT_SETLOGFILE
REQUEST EASYONE_SERVICIOSAT_SETDOWNPATH
REQUEST EASYONE_SERVICIOSAT_SAVELOG
REQUEST EASYONE_SERVICIOSAT_RESOLVERCAPTCHA
REQUEST EASYONE_SERVICIOSAT_OBTENERCAPTCHA
REQUEST EASYONE_SERVICIOSAT_OBTENER
REQUEST EASYONE_SERVICIOSAT_MSGBOX
REQUEST EASYONE_SERVICIOSAT_LOGINC
REQUEST EASYONE_SERVICIOSAT_LOGIN
REQUEST EASYONE_SERVICIOSAT_LOADADDINFO
REQUEST EASYONE_SERVICIOSAT_GENERALLICENCIARC
REQUEST EASYONE_SERVICIOSAT_FREE
REQUEST EASYONE_SERVICIOSAT_DOWNLOADALL
REQUEST EASYONE_SERVICIOSAT_DOWNLOAD
REQUEST EASYONE_SERVICIOSAT_CUANTOSHAY
REQUEST EASYONE_SERVICIOSAT_CREATE
REQUEST EASYONE_SERVICIOSAT_CONSULTARECIBIDOSPORUUID
REQUEST EASYONE_SERVICIOSAT_CONSULTARECIBIDOSPORFECHA
REQUEST EASYONE_SERVICIOSAT_CONSULTAEMITIDOSPORUUID
REQUEST EASYONE_SERVICIOSAT_CONSULTAEMITIDOSPORFECHA
REQUEST EASYONE_SERVICIOSAT_CLR_MOSTRARCAPTCHA
REQUEST EASYONE_SERVICIOSAT_CAMBIARCAPTCHA
REQUEST EASYONE_SERVICIOSAT_ADDXSLTINFO
REQUEST EASYONE_SERVICIOSAT_ADDXQUERYINFO
REQUEST EASYONE_SERVICIOSAT_ADDXPATHINFO
REQUEST EASYONE_SERVICIOSAT_ADDLICENCE
REQUEST EASYONE_SERVICIOSAT_ADDCONSULTAEMITIDOSPOREJERCICIO
REQUEST EASYONE_SERVICIOPARCIAL10_SETPARCIALESCONSTRUCCION
REQUEST EASYONE_SERVICIOPARCIAL10_SETINMUEBLE
REQUEST EASYONE_SERVICIOPARCIAL10_FREE
REQUEST EASYONE_SERVICIOPARCIAL10_CREATE
REQUEST EASYONE_SECTORFINANCIERO10_SETSECTORFINANCIERO
REQUEST EASYONE_SECTORFINANCIERO10_FREE
REQUEST EASYONE_SECTORFINANCIERO10_CREATE
REQUEST EASYONE_RETENCIONES10_SETTOTALES
REQUEST EASYONE_RETENCIONES10_SETRETENCIONES
REQUEST EASYONE_RETENCIONES10_SETRECEPTOR
REQUEST EASYONE_RETENCIONES10_SETPERIODO
REQUEST EASYONE_RETENCIONES10_SETNACIONAL
REQUEST EASYONE_RETENCIONES10_SETEXTRANJERO
REQUEST EASYONE_RETENCIONES10_SETEMISOR
REQUEST EASYONE_RETENCIONES10_FREE
REQUEST EASYONE_RETENCIONES10_CREATE
REQUEST EASYONE_RETENCIONES10_ADDIMPRETENIDOS
REQUEST EASYONE_READFROMFILE
REQUEST EASYONE_PREMIOS10_SETPREMIOS
REQUEST EASYONE_PREMIOS10_FREE
REQUEST EASYONE_PREMIOS10_CREATE
REQUEST EASYONE_PLANESDERETIRO10_SETPLANESDERETIRO
REQUEST EASYONE_PLANESDERETIRO10_FREE
REQUEST EASYONE_PLANESDERETIRO10_CREATE
REQUEST EASYONE_PFIC10_SETPFINTEGRANTECOORDINADO
REQUEST EASYONE_PFIC10_FREE
REQUEST EASYONE_PFIC10_CREATE
REQUEST EASYONE_PAGOSAEXTRANJEROS10_SETPAGOSAEXTRANJEROS
REQUEST EASYONE_PAGOSAEXTRANJEROS10_SETNOBENEFICIARIO
REQUEST EASYONE_PAGOSAEXTRANJEROS10_SETBENEFICIARIO
REQUEST EASYONE_PAGOSAEXTRANJEROS10_FREE
REQUEST EASYONE_PAGOSAEXTRANJEROS10_CREATE
REQUEST EASYONE_PAGOENESPECIE10_SETPAGOENESPECIE
REQUEST EASYONE_PAGOENESPECIE10_FREE
REQUEST EASYONE_PAGOENESPECIE10_CREATE
REQUEST EASYONE_PAGO10_FREE
REQUEST EASYONE_PAGO10_CREATE
REQUEST EASYONE_PAGO10_ADDPAGO
REQUEST EASYONE_PAGO10_ADDDOCTORELACIONADO
REQUEST EASYONE_OPERACIONESCONDERIVADOS10_SETOPERACIONESCONDERIVADOS
REQUEST EASYONE_OPERACIONESCONDERIVADOS10_FREE
REQUEST EASYONE_OPERACIONESCONDERIVADOS10_CREATE
REQUEST EASYONE_OBRASARTE10_SETOBRASARTEANTIGUEDADES
REQUEST EASYONE_OBRASARTE10_FREE
REQUEST EASYONE_OBRASARTE10_CREATE
REQUEST EASYONE_NOTARIOSPUBLICOS10_SETDATOSUNENAJENANTE
REQUEST EASYONE_NOTARIOSPUBLICOS10_SETDATOSUNADQUIRIENTE
REQUEST EASYONE_NOTARIOSPUBLICOS10_SETDATOSOPERACION
REQUEST EASYONE_NOTARIOSPUBLICOS10_SETDATOSNOTARIO
REQUEST EASYONE_NOTARIOSPUBLICOS10_SETDATOSENAJENANTE
REQUEST EASYONE_NOTARIOSPUBLICOS10_SETDATOSADQUIRIENTE
REQUEST EASYONE_NOTARIOSPUBLICOS10_FREE
REQUEST EASYONE_NOTARIOSPUBLICOS10_CREATE
REQUEST EASYONE_NOTARIOSPUBLICOS10_ADDDESCINMUEBLE
REQUEST EASYONE_NOTARIOSPUBLICOS10_ADDDATOSENAJENANTECOPSC
REQUEST EASYONE_NOTARIOSPUBLICOS10_ADDDATOSADQUIRIENTECOPSC
REQUEST EASYONE_NOMINA12_SETSUBSIDIOALEMPLEO
REQUEST EASYONE_NOMINA12_SETSEPARACIONINDEMNIZACION
REQUEST EASYONE_NOMINA12_SETRECEPTOR
REQUEST EASYONE_NOMINA12_SETPERCEPCIONES
REQUEST EASYONE_NOMINA12_SETNOMINA
REQUEST EASYONE_NOMINA12_SETJUBILACIONPENSIONRETIRO
REQUEST EASYONE_NOMINA12_SETENTIDADSNCF
REQUEST EASYONE_NOMINA12_SETEMISOR
REQUEST EASYONE_NOMINA12_SETDEDUCCIONES
REQUEST EASYONE_NOMINA12_SETCOMPENSACIONSALDOSAFAVOR
REQUEST EASYONE_NOMINA12_SETACCIONESOTITULOS
REQUEST EASYONE_NOMINA12_FREE
REQUEST EASYONE_NOMINA12_CREATE
REQUEST EASYONE_NOMINA12_ADDSUBCONTRATACION
REQUEST EASYONE_NOMINA12_ADDPERCEPCION
REQUEST EASYONE_NOMINA12_ADDOTROPAGO
REQUEST EASYONE_NOMINA12_ADDINCAPACIDAD
REQUEST EASYONE_NOMINA12_ADDHORASEXTRA
REQUEST EASYONE_NOMINA12_ADDDEDUCCION
REQUEST EASYONE_ND_SETSEPARACIONINFO
REQUEST EASYONE_ND_SETRECEPTOR
REQUEST EASYONE_ND_SETPERCEPCIONJUBILACIONTOTAL
REQUEST EASYONE_ND_SETPERCEPCIONJUBILACIONPARCIAL
REQUEST EASYONE_ND_SETOTROSINGRESOS
REQUEST EASYONE_ND_SETOTRO
REQUEST EASYONE_ND_SETORDINARIANOLABORAL
REQUEST EASYONE_ND_SETORDINARIALABORAL
REQUEST EASYONE_ND_SETNOMINADIGITAL
REQUEST EASYONE_ND_SETINGRESOSMIXTOS
REQUEST EASYONE_ND_SETEXTRAORDINARIA
REQUEST EASYONE_ND_SETEMISORPERSONAMORAL
REQUEST EASYONE_ND_SETEMISORPERSONAFISICA
REQUEST EASYONE_ND_SETCLABE
REQUEST EASYONE_ND_FREE
REQUEST EASYONE_ND_CREATE
REQUEST EASYONE_ND_ADDSUBCONTRATACIONTERCERO
REQUEST EASYONE_ND_ADDPERCEPCIONSUBSIDIOINCAPACIDAD
REQUEST EASYONE_ND_ADDPERCEPCIONSEPARACION
REQUEST EASYONE_ND_ADDPERCEPCIONHORASEXTRA
REQUEST EASYONE_ND_ADDPERCEPCIONACCIONESOTITULOS
REQUEST EASYONE_ND_ADDPERCEPCION
REQUEST EASYONE_ND_ADDOTROSSUBSIDIOPARAEMPLEO
REQUEST EASYONE_ND_ADDOTROSCOMPENSACION
REQUEST EASYONE_ND_ADDOTROS
REQUEST EASYONE_ND_ADDDEDUCCIONINCAPACIDAD
REQUEST EASYONE_ND_ADDDEDUCCION
REQUEST EASYONE_MSGBOX
REQUEST EASYONE_MAILSERVICE_SETTO
REQUEST EASYONE_MAILSERVICE_SETSECURITYTYPE
REQUEST EASYONE_MAILSERVICE_SETPRIORITY
REQUEST EASYONE_MAILSERVICE_SETMSG
REQUEST EASYONE_MAILSERVICE_SETISBODYHTML
REQUEST EASYONE_MAILSERVICE_SETHOST
REQUEST EASYONE_MAILSERVICE_SETFROM
REQUEST EASYONE_MAILSERVICE_SETERRORFILE
REQUEST EASYONE_MAILSERVICE_SETCREDENTIALS
REQUEST EASYONE_MAILSERVICE_SETATTACHMENT
REQUEST EASYONE_MAILSERVICE_SEND
REQUEST EASYONE_MAILSERVICE_FREE
REQUEST EASYONE_MAILSERVICE_CREATE
REQUEST EASYONE_LOADXML
REQUEST EASYONE_LEYENDASFISC10_FREE
REQUEST EASYONE_LEYENDASFISC10_CREATE
REQUEST EASYONE_LEYENDASFISC10_ADDLEYENDA
REQUEST EASYONE_LEERESULTADO
REQUEST EASYONE_ISVALIDRFC
REQUEST EASYONE_INTERESESHIPOTECARIOS10_SETINTERESESHIPOTECARIOS
REQUEST EASYONE_INTERESESHIPOTECARIOS10_FREE
REQUEST EASYONE_INTERESESHIPOTECARIOS10_CREATE
REQUEST EASYONE_INTERESES10_SETINTERESES
REQUEST EASYONE_INTERESES10_FREE
REQUEST EASYONE_INTERESES10_CREATE
REQUEST EASYONE_INE11_SETINE
REQUEST EASYONE_INE11_FREE
REQUEST EASYONE_INE11_CREATE
REQUEST EASYONE_INE11_ADDENTIDAD
REQUEST EASYONE_INE11_ADDCONTABILIDAD
REQUEST EASYONE_IMPLOCAL10_SETIMPUESTOSLOCALES
REQUEST EASYONE_IMPLOCAL10_FREE
REQUEST EASYONE_IMPLOCAL10_CREATE
REQUEST EASYONE_IMPLOCAL10_ADDTRASLADOSLOCALES
REQUEST EASYONE_IMPLOCAL10_ADDRETENCIONESLOCALES
REQUEST EASYONE_IEDU10_SETINSTEDUCATIVAS
REQUEST EASYONE_IEDU10_FREE
REQUEST EASYONE_IEDU10_CREATE
REQUEST EASYONE_GETVERSIONVALUE
REQUEST EASYONE_FIRMACOMPROBANTE
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_SETRETEFECTFIDEICOMISO
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_SETINTEGRACINGRESOS
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_SETINTEGRACEGRESOS
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_SETINGRESOSOENTRADAS
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_SETDEDUCCOSALIDAS
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_FREE
REQUEST EASYONE_FIDEICOMISONOEMPRESARIAL10_CREATE
REQUEST EASYONE_EXECUTE
REQUEST EASYONE_ESCRIBERESULTADOS
REQUEST EASYONE_ESCRIBERESULTADO
REQUEST EASYONE_ENVIARADDENDA
REQUEST EASYONE_ENAJENACIONDEACCIONES10_SETENAJENACIONDEACCIONES
REQUEST EASYONE_ENAJENACIONDEACCIONES10_FREE
REQUEST EASYONE_ENAJENACIONDEACCIONES10_CREATE
REQUEST EASYONE_ECC11_SETESTADODECUENTACOMBUSTIBLE
REQUEST EASYONE_ECC11_FREE
REQUEST EASYONE_ECC11_CREATE
REQUEST EASYONE_ECC11_ADDTRASLADO
REQUEST EASYONE_ECC11_ADDCONCEPTOESTADODECUENTACOMBUSTIBLE
REQUEST EASYONE_DONAT11_SETDONATARIAS
REQUEST EASYONE_DONAT11_FREE
REQUEST EASYONE_DONAT11_CREATE
REQUEST EASYONE_DIVISAS10_SETDIVISAS
REQUEST EASYONE_DIVISAS10_FREE
REQUEST EASYONE_DIVISAS10_CREATE
REQUEST EASYONE_DIVIDENDOS10_SETREMANENTE
REQUEST EASYONE_DIVIDENDOS10_SETDIVIDOUTIL
REQUEST EASYONE_DIVIDENDOS10_FREE
REQUEST EASYONE_DIVIDENDOS10_CREATE
REQUEST EASYONE_DETALLISTA_SET_TOTALAMOUNT
REQUEST EASYONE_DETALLISTA_SET_SHIPTO
REQUEST EASYONE_DETALLISTA_SET_SELLER
//REQUEST EASYONE_DETALLISTA_SET_REQUESTFORPAYMENTIDENTIFICATION_ENTITYTYPE
REQUEST EASYONE_DETALLISTA_SET_PAYMENTTERMS
REQUEST EASYONE_DETALLISTA_SET_ORDERIDENTIFICATION_REFERENCEDATE
REQUEST EASYONE_DETALLISTA_SET_NAMEANDADDRESS
REQUEST EASYONE_DETALLISTA_SET_LINEITEM_TRADEITEMIDENTIFICATION
REQUEST EASYONE_DETALLISTA_SET_LINEITEM_TRADEITEMDESCRIPTIONINFORMATION
REQUEST EASYONE_DETALLISTA_SET_LINEITEM_TOTALLINEAMOUNT
REQUEST EASYONE_DETALLISTA_SET_LINEITEM_NETPRICE
REQUEST EASYONE_DETALLISTA_SET_LINEITEM_INVOICEDQUANTITY
REQUEST EASYONE_DETALLISTA_SET_LINEITEM_GROSSPRICE
REQUEST EASYONE_DETALLISTA_SET_INVOICECREATOR
REQUEST EASYONE_DETALLISTA_SET_DETALLISTA
REQUEST EASYONE_DETALLISTA_SET_DELIVERYNOTE_REFERENCEDATE
REQUEST EASYONE_DETALLISTA_SET_BUYER
REQUEST EASYONE_DETALLISTA_FREE
REQUEST EASYONE_DETALLISTA_CREATE
REQUEST EASYONE_DETALLISTA_ADD_TOTALALLOWANCECHARGE
REQUEST EASYONE_DETALLISTA_ADD_SPECIALINSTRUCTION_TEXT
REQUEST EASYONE_DETALLISTA_ADD_SPECIALINSTRUCTION
//REQUEST EASYONE_DETALLISTA_ADD_ORDERIDENTIFICATION_REFERENCEIDENTIFICATION
REQUEST EASYONE_DETALLISTA_ADD_ORDERIDENTIFICATION
//REQUEST EASYONE_DETALLISTA_ADD_LINEITEM_ALTERNATETRADEITEMIDENTIFICATION
REQUEST EASYONE_DETALLISTA_ADD_LINEITEM
REQUEST EASYONE_DETALLISTA_ADD_DELIVERYNOTE_REFERENCEIDENTIFICATION
REQUEST EASYONE_DETALLISTA_ADD_DELIVERYNOTE
REQUEST EASYONE_DETALLISTA_ADD_CUSTOMS
REQUEST EASYONE_DETALLISTA_ADD_CURRENCY
REQUEST EASYONE_DETALLISTA_ADD_ALLOWANCECHARGE
//REQUEST EASYONE_DETALLISTA_ADD_ADDITIONALINFORMATION_REFERENCEIDENTIFICATION
REQUEST EASYONE_DETALLISTA_ADD_ADDITIONALINFORMATION
REQUEST EASYONE_DESTRUCCION10_SETVEHICULODESTRUIDO
REQUEST EASYONE_DESTRUCCION10_SETINFORMACIONADUANERA
REQUEST EASYONE_DESTRUCCION10_SETCERTIFICADODEDESTRUCCION
REQUEST EASYONE_DESTRUCCION10_FREE
REQUEST EASYONE_DESTRUCCION10_CREATE
REQUEST EASYONE_DECRETO10_SETVEHICULOUSADOENAJENADOPERMALFAB
REQUEST EASYONE_DECRETO10_SETVEHICULONUVOSEMENAJENADOFABALPERM
REQUEST EASYONE_DECRETO10_SETRENOVACIONYSUSTITUCIONVEHICULOS
//REQUEST EASYONE_DECRETO10_SETDECRETOSUSTITVEHICULARVEHICULONUVOSEMENAJENADOFABALPERM
REQUEST EASYONE_DECRETO10_SETDECRETOSUSTITVEHICULAR
REQUEST EASYONE_DECRETO10_SETDECRETORENOVVEHICULAR
REQUEST EASYONE_DECRETO10_FREE
REQUEST EASYONE_DECRETO10_CREATE
REQUEST EASYONE_DECRETO10_ADDVEHICULOSUSADOSENAJENADOPERMALFAB
REQUEST EASYONE_CRYPTO_LOADCSD
REQUEST EASYONE_CRYPTO_GETUTCTIME
REQUEST EASYONE_CRYPTO_GENERAPETICION
REQUEST EASYONE_CRYPTO_FREE
REQUEST EASYONE_CRYPTO_EXPORT2PFX
REQUEST EASYONE_CRYPTO_CREATE
REQUEST EASYONE_CONSULTACFDI_OBTENERESTADO
REQUEST EASYONE_CONSULTACFDI_FREE
REQUEST EASYONE_CONSULTACFDI_CREATE
REQUEST EASYONE_CONFIG_SETPREFIRMASCRIPT
REQUEST EASYONE_CONFIG_SETOUTPUTFILEONOK
REQUEST EASYONE_CONFIG_SETLOGDIRECTORY
REQUEST EASYONE_CONFIG_SETCANCELAREINTENTS
REQUEST EASYONE_CONFIG_SETAGROUPNOIDENTIFICACIONCONCEPTS
REQUEST EASYONE_CONFIG_FREE
REQUEST EASYONE_CONFIG_CREATE
REQUEST EASYONE_CFDI33_SETRECEPTOR
REQUEST EASYONE_CFDI33_SETFILENAMES
REQUEST EASYONE_CFDI33_SETEMISOR
REQUEST EASYONE_CFDI33_SETCUENTAPREDIAL
REQUEST EASYONE_CFDI33_SETCOMPROBANTEIMPUESTOS
REQUEST EASYONE_CFDI33_SETCOMPROBANTEFECHA
REQUEST EASYONE_CFDI33_SETCOMPROBANTE
REQUEST EASYONE_CFDI33_SETCFDIRELACIONADOS
REQUEST EASYONE_CFDI33_SETAZUREFILEACCOUNT
REQUEST EASYONE_CFDI33_SETAZUREBLOBACCOUNT
REQUEST EASYONE_CFDI33_SENDMAIL
REQUEST EASYONE_CFDI33_LOADXMLTRANSFORM
REQUEST EASYONE_CFDI33_LOADTXTPIPES
REQUEST EASYONE_CFDI33_FREE
REQUEST EASYONE_CFDI33_CREATE
REQUEST EASYONE_CFDI33_ADDTRASLADOSTRASLADO
REQUEST EASYONE_CFDI33_ADDTRASLADO
REQUEST EASYONE_CFDI33_ADDRETENCIONESRETENCION
REQUEST EASYONE_CFDI33_ADDRETENCION
REQUEST EASYONE_CFDI33_ADDPARTEINFORMACIONADUANERA
REQUEST EASYONE_CFDI33_ADDPARTE
REQUEST EASYONE_CFDI33_ADDNS
REQUEST EASYONE_CFDI33_ADDINFORMACIONADUANERA
REQUEST EASYONE_CFDI33_ADDCONCEPTOIMP
REQUEST EASYONE_CFDI33_ADDCONCEPTO
REQUEST EASYONE_CFDI33_ADDCFDIRELACIONADO
REQUEST EASYONE_CERKEYMATCHFIEL_FREE
REQUEST EASYONE_CERKEYMATCHFIEL
REQUEST EASYONE_CERKEYMATCHCSD_FREE
REQUEST EASYONE_CERKEYMATCHCSD
REQUEST EASYONE_CCE11_SETRECEPTORDOMICILIO
REQUEST EASYONE_CCE11_SETRECEPTOR
REQUEST EASYONE_CCE11_SETEMISORDOMICILIO
REQUEST EASYONE_CCE11_SETEMISOR
REQUEST EASYONE_CCE11_SETCOMERCIOEXTERIOR
REQUEST EASYONE_CCE11_FREE
REQUEST EASYONE_CCE11_CREATE
REQUEST EASYONE_CCE11_ADDPROPIETARIO
REQUEST EASYONE_CCE11_ADDMERCANCIA
REQUEST EASYONE_CCE11_ADDDESTINATARIODOMICILIO
REQUEST EASYONE_CCE11_ADDDESTINATARIO
REQUEST EASYONE_CCE11_ADDDESCRIPCIONESESPECIFICAS
REQUEST EASYONE_CANCELAIO_V2_SETINFOPRUEBASPREF
REQUEST EASYONE_CANCELAIO_V2_SETINFOPRUEBAS
REQUEST EASYONE_CANCELAIO_V2_SETINFOPREF
REQUEST EASYONE_CANCELAIO_V2_SETINFO
REQUEST EASYONE_CANCELAIO_V2_PROCESA
REQUEST EASYONE_CANCELAIO_V2_FREE
REQUEST EASYONE_CANCELAIO_V2_CREATE
REQUEST EASYONE_CANCELACOMPROBANTE_V2_FREE
REQUEST EASYONE_CANCELACOMPROBANTE_V2
// REQUEST EASYONE_ARRENDAMIENTOENFIDEICOMISO10_SETARRENDAMIENTOENFIDEICOMISO
// REQUEST EASYONE_ARRENDAMIENTOENFIDEICOMISO10_FREE
// REQUEST EASYONE_ARRENDAMIENTOENFIDEICOMISO10_CREATE
REQUEST EASYONE_AEROLINEAS10_SETOTROSCARGOS
REQUEST EASYONE_AEROLINEAS10_SETAEROLINEAS
REQUEST EASYONE_AEROLINEAS10_FREE
REQUEST EASYONE_AEROLINEAS10_CREATE
REQUEST EASYONE_AEROLINEAS10_ADDCARGO
REQUEST EASYONE_ADDRESULTADO
REQUEST EASYONE_ADDCOMPLEMENTOCONCEPTO
REQUEST EASYONE_ADDCOMPLEMENTO
REQUEST EASYONE_ADDADDENDATEXT
REQUEST EASYONE_ADDADDENDA

/*
Turbo Incremental Link 5.00 Copyright (c) 1997, 2000 Borland
Error: Unresolved external '_HB_FUN_EASYONE_DETALLISTA_SET_REQUESTFORPAYMENTIDENTIFICATION_ENTITYTY' referenced from C:\DPSGEV51\SOURCE\OBJ\LIB32.OBJ
Error: Unresolved external '_HB_FUN_EASYONE_DETALLISTA_ADD_ORDERIDENTIFICATION_REFERENCEIDENTIFICAT' referenced from C:\DPSGEV51\SOURCE\OBJ\LIB32.OBJ
Error: Unresolved external '_HB_FUN_EASYONE_DETALLISTA_ADD_LINEITEM_ALTERNATETRADEITEMIDENTIFICATIO' referenced from C:\DPSGEV51\SOURCE\OBJ\LIB32.OBJ
Error: Unresolved external '_HB_FUN_EASYONE_DETALLISTA_ADD_ADDITIONALINFORMATION_REFERENCEIDENTIFIC' referenced from C:\DPSGEV51\SOURCE\OBJ\LIB32.OBJ
Error: Unresolved external '_HB_FUN_EASYONE_DECRETO10_SETDECRETOSUSTITVEHICULARVEHICULONUVOSEMENAJE' referenced from C:\DPSGEV51\SOURCE\OBJ\LIB32.OBJ
Error: Unresolved external '_HB_FUN_EASYONE_ARRENDAMIENTOENFIDEICOMISO10_SETARRENDAMIENTOENFIDEICOM' referenced from C:\DPSGEV51\SOURCE\OBJ\LIB32.OBJ
*/

#pragma BEGINDUM
   #include <hbapi.h>
   #include <windows.h>

   typedef void(__cdecl * __EasyOne_XmlToPdf_SetMailInfoEx)(long p, LPSTR security_None_TLS_SSL, LPSTR BodyAsHtml_True_False, LPSTR reply);
   HB_FUNC(EASYONE_XMLTOPDF_SETMAILINFOEX)
   {
      (((__EasyOne_XmlToPdf_SetMailInfoEx)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_SetMailInfoEx"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_XmlToPdf_SetMailInfo)(long p, LPSTR host, LPSTR port, LPSTR username, LPSTR password, LPSTR fromName, LPSTR fromMail, LPSTR toMail, LPSTR Subject, LPSTR Body);
   HB_FUNC(EASYONE_XMLTOPDF_SETMAILINFO)
   {
      (((__EasyOne_XmlToPdf_SetMailInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_SetMailInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10)));
   }

   typedef void(__cdecl * __EasyOne_XmlToPdf_SetLicence)(long p, LPSTR lic);
   HB_FUNC(EASYONE_XMLTOPDF_SETLICENCE)
   {
      (((__EasyOne_XmlToPdf_SetLicence)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_SetLicence"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_XmlToPdf_SetFiles)(long p, LPSTR XmlIn, LPSTR XmlOut);
   HB_FUNC(EASYONE_XMLTOPDF_SETFILES)
   {
      (((__EasyOne_XmlToPdf_SetFiles)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_SetFiles"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef LPSTR(__cdecl * __EasyOne_XmlToPdf_SendMail)(long p);
   HB_FUNC(EASYONE_XMLTOPDF_SENDMAIL)
   {
      hb_retc(((__EasyOne_XmlToPdf_SendMail)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_SendMail"))(hb_parnl(1)));
   }

   typedef LPSTR(__cdecl * __EasyOne_XmlToPdf_QueryXml)(long p, long XPath_0_Or_XQuery_1_, LPSTR exp);
   HB_FUNC(EASYONE_XMLTOPDF_QUERYXML)
   {
      hb_retc(((__EasyOne_XmlToPdf_QueryXml)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_QueryXml"))(hb_parnl(1), hb_parnl(2), (LPSTR) hb_parc(3)));
   }

   typedef LPSTR(__cdecl * __EasyOne_XmlToPdf_GetError)(long p);
   HB_FUNC(EASYONE_XMLTOPDF_GETERROR)
   {
      hb_retc(((__EasyOne_XmlToPdf_GetError)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_GetError"))(hb_parnl(1)));
   }

   typedef void(__cdecl * __EasyOne_XmlToPdf_Free)(long p);
   HB_FUNC(EASYONE_XMLTOPDF_FREE)
   {
      (((__EasyOne_XmlToPdf_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_Free"))(hb_parnl(1)));
   }

   typedef void(__cdecl * __EasyOne_XmlToPdf_CreatePdf)(long p, LPSTR XslFo);
   HB_FUNC(EASYONE_XMLTOPDF_CREATEPDF)
   {
      (((__EasyOne_XmlToPdf_CreatePdf)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_CreatePdf"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef long(__cdecl * __EasyOne_XmlToPdf_Create)(void);
   HB_FUNC(EASYONE_XMLTOPDF_CREATE)
   {
      hb_retnl(((__EasyOne_XmlToPdf_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_Create"))());
   }

   typedef void(__cdecl * __EasyOne_XmlToPdf_AddLabel)(long p, LPSTR label, LPSTR value);
   HB_FUNC(EASYONE_XMLTOPDF_ADDLABEL)
   {
      (((__EasyOne_XmlToPdf_AddLabel)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_XmlToPdf_AddLabel"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_xml33_Free)(long p);
   HB_FUNC(EASYONE_XML33_FREE)
   {
      (((__EasyOne_xml33_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_xml33_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_xml33_Create)(LPSTR xmlFile, LPSTR OutFile);
   HB_FUNC(EASYONE_XML33_CREATE)
   {
      hb_retnl(((__EasyOne_xml33_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_xml33_Create"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ventavehiculos11_SetVentaVehiculos)(long p, LPSTR ClaveVehicular, LPSTR Niv);
   HB_FUNC(EASYONE_VENTAVEHICULOS11_SETVENTAVEHICULOS)
   {
      (((__EasyOne_ventavehiculos11_SetVentaVehiculos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ventavehiculos11_SetVentaVehiculos"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ventavehiculos11_Free)(long p);
   HB_FUNC(EASYONE_VENTAVEHICULOS11_FREE)
   {
      (((__EasyOne_ventavehiculos11_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ventavehiculos11_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_ventavehiculos11_Create)(void);
   HB_FUNC(EASYONE_VENTAVEHICULOS11_CREATE)
   {
      hb_retnl(((__EasyOne_ventavehiculos11_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ventavehiculos11_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ventavehiculos11_AddParteInformacionAduanera)(long p, LPSTR numero, LPSTR fecha, LPSTR aduana);
   HB_FUNC(EASYONE_VENTAVEHICULOS11_ADDPARTEINFORMACIONADUANERA)
   {
      (((__EasyOne_ventavehiculos11_AddParteInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ventavehiculos11_AddParteInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_ventavehiculos11_AddParte)(long p, LPSTR cantidad, LPSTR unidad, LPSTR noIdentificacion, LPSTR descripcion, LPSTR valorUnitario, LPSTR importe);
   HB_FUNC(EASYONE_VENTAVEHICULOS11_ADDPARTE)
   {
      (((__EasyOne_ventavehiculos11_AddParte)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ventavehiculos11_AddParte"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_ventavehiculos11_AddInformacionAduanera)(long p, LPSTR numero, LPSTR fecha, LPSTR aduana);
   HB_FUNC(EASYONE_VENTAVEHICULOS11_ADDINFORMACIONADUANERA)
   {
      (((__EasyOne_ventavehiculos11_AddInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ventavehiculos11_AddInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_vehiculousado10_SetVehiculoUsado)(long p, LPSTR montoAdquisicion, LPSTR montoEnajenacion, LPSTR claveVehicular, LPSTR marca, LPSTR tipo, LPSTR modelo, LPSTR numeroMotor, LPSTR numeroSerie, LPSTR NIV, LPSTR valor);
   HB_FUNC(EASYONE_VEHICULOUSADO10_SETVEHICULOUSADO)
   {
      (((__EasyOne_vehiculousado10_SetVehiculoUsado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_vehiculousado10_SetVehiculoUsado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11)));
   }

   typedef void(__cdecl * __EasyOne_vehiculousado10_Free)(long p);
   HB_FUNC(EASYONE_VEHICULOUSADO10_FREE)
   {
      (((__EasyOne_vehiculousado10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_vehiculousado10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_vehiculousado10_Create)(void);
   HB_FUNC(EASYONE_VEHICULOUSADO10_CREATE)
   {
      hb_retnl(((__EasyOne_vehiculousado10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_vehiculousado10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_vehiculousado10_AddInformacionAduanera)(long p, LPSTR numero, LPSTR fecha, LPSTR aduana);
   HB_FUNC(EASYONE_VEHICULOUSADO10_ADDINFORMACIONADUANERA)
   {
      (((__EasyOne_vehiculousado10_AddInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_vehiculousado10_AddInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_valesdedespensa10_SetValesDeDespensa)(long p, LPSTR registroPatronal, LPSTR numeroDeCuenta, LPSTR total);
   HB_FUNC(EASYONE_VALESDEDESPENSA10_SETVALESDEDESPENSA)
   {
      (((__EasyOne_valesdedespensa10_SetValesDeDespensa)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_valesdedespensa10_SetValesDeDespensa"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_valesdedespensa10_Free)(long p);
   HB_FUNC(EASYONE_VALESDEDESPENSA10_FREE)
   {
      (((__EasyOne_valesdedespensa10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_valesdedespensa10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_valesdedespensa10_Create)(void);
   HB_FUNC(EASYONE_VALESDEDESPENSA10_CREATE)
   {
      hb_retnl(((__EasyOne_valesdedespensa10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_valesdedespensa10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_valesdedespensa10_AddConcepto)(long p, LPSTR identificador, LPSTR fecha, LPSTR rfc, LPSTR curp, LPSTR nombre, LPSTR numSeguridadSocial, LPSTR importe);
   HB_FUNC(EASYONE_VALESDEDESPENSA10_ADDCONCEPTO)
   {
      (((__EasyOne_valesdedespensa10_AddConcepto)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_valesdedespensa10_AddConcepto"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_UseCurl)(long p, LPSTR use);
   HB_FUNC(EASYONE_USECURL)
   {
      (((__EasyOne_UseCurl)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_UseCurl"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_tpe10_SetTuristaPasajeroExtranjero)(long p, LPSTR fechadeTransito, LPSTR tipoTransito);
   HB_FUNC(EASYONE_TPE10_SETTURISTAPASAJEROEXTRANJERO)
   {
      (((__EasyOne_tpe10_SetTuristaPasajeroExtranjero)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_tpe10_SetTuristaPasajeroExtranjero"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_tpe10_SetdatosTransito)(long p, LPSTR Via, LPSTR TipoId, LPSTR NumeroId, LPSTR Nacionalidad, LPSTR EmpresaTransporte, LPSTR IdTransporte);
   HB_FUNC(EASYONE_TPE10_SETDATOSTRANSITO)
   {
      (((__EasyOne_tpe10_SetdatosTransito)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_tpe10_SetdatosTransito"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_tpe10_Free)(long p);
   HB_FUNC(EASYONE_TPE10_FREE)
   {
      (((__EasyOne_tpe10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_tpe10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_tpe10_Create)(void);
   HB_FUNC(EASYONE_TPE10_CREATE)
   {
      hb_retnl(((__EasyOne_tpe10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_tpe10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_TimbradoMasivo_SetFachadaCorePruebas)(long p, LPSTR noting);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_SETFACHADACOREPRUEBAS)
   {
      (((__EasyOne_TimbradoMasivo_SetFachadaCorePruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_SetFachadaCorePruebas"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_TimbradoMasivo_SetConectorPruebas)(long p, LPSTR noting);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_SETCONECTORPRUEBAS)
   {
      (((__EasyOne_TimbradoMasivo_SetConectorPruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_SetConectorPruebas"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_TimbradoMasivo_SetCertificado)(long p, LPSTR csd, LPSTR key, LPSTR pwd);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_SETCERTIFICADO)
   {
      (((__EasyOne_TimbradoMasivo_SetCertificado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_SetCertificado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_TimbradoMasivo_ProcesaComprobantes)(long p, LPSTR user, LPSTR pwd);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_PROCESACOMPROBANTES)
   {
      (((__EasyOne_TimbradoMasivo_ProcesaComprobantes)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_ProcesaComprobantes"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_TimbradoMasivo_Free)(long p);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_FREE)
   {
      (((__EasyOne_TimbradoMasivo_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_TimbradoMasivo_Create)(void);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_CREATE)
   {
      hb_retnl(((__EasyOne_TimbradoMasivo_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_Create"))());
   }

   typedef void(__cdecl * __EasyOne_TimbradoMasivo_AddComprobante)(long p, long pComp);
   HB_FUNC(EASYONE_TIMBRADOMASIVO_ADDCOMPROBANTE)
   {
      (((__EasyOne_TimbradoMasivo_AddComprobante)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_TimbradoMasivo_AddComprobante"))(hb_parnl(1), hb_parnl(2)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_SetPorCuentadeTerceros)(long p, LPSTR rfc, LPSTR nombre);
   HB_FUNC(EASYONE_TERCEROS11_SETPORCUENTADETERCEROS)
   {
      (((__EasyOne_terceros11_SetPorCuentadeTerceros)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_SetPorCuentadeTerceros"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_SetInformacionFiscalTercero)(long p, LPSTR calle, LPSTR noExterior, LPSTR noInterior, LPSTR colonia, LPSTR localidad, LPSTR referencia, LPSTR municipio, LPSTR estado, LPSTR pais, LPSTR codigoPostal);
   HB_FUNC(EASYONE_TERCEROS11_SETINFORMACIONFISCALTERCERO)
   {
      (((__EasyOne_terceros11_SetInformacionFiscalTercero)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_SetInformacionFiscalTercero"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_SetInformacionAduanera)(long p, LPSTR numero, LPSTR fecha, LPSTR aduana);
   HB_FUNC(EASYONE_TERCEROS11_SETINFORMACIONADUANERA)
   {
      (((__EasyOne_terceros11_SetInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_SetInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_SetCuentaPredial)(long p, LPSTR numero);
   HB_FUNC(EASYONE_TERCEROS11_SETCUENTAPREDIAL)
   {
      (((__EasyOne_terceros11_SetCuentaPredial)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_SetCuentaPredial"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_Free)(long p);
   HB_FUNC(EASYONE_TERCEROS11_FREE)
   {
      (((__EasyOne_terceros11_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_terceros11_Create)(void);
   HB_FUNC(EASYONE_TERCEROS11_CREATE)
   {
      hb_retnl(((__EasyOne_terceros11_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_Create"))());
   }

   typedef void(__cdecl * __EasyOne_terceros11_AddTraslado)(long p, LPSTR impuesto, LPSTR tasa, LPSTR importe);
   HB_FUNC(EASYONE_TERCEROS11_ADDTRASLADO)
   {
      (((__EasyOne_terceros11_AddTraslado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_AddTraslado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_AddRetencion)(long p, LPSTR impuesto, LPSTR importe);
   HB_FUNC(EASYONE_TERCEROS11_ADDRETENCION)
   {
      (((__EasyOne_terceros11_AddRetencion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_AddRetencion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_AddParteInformacionAduanera)(long p, LPSTR numero, LPSTR fecha, LPSTR aduana);
   HB_FUNC(EASYONE_TERCEROS11_ADDPARTEINFORMACIONADUANERA)
   {
      (((__EasyOne_terceros11_AddParteInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_AddParteInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_terceros11_AddParte)(long p, LPSTR cantidad, LPSTR unidad, LPSTR noIdentificacion, LPSTR descripcion, LPSTR valorUnitario, LPSTR importe);
   HB_FUNC(EASYONE_TERCEROS11_ADDPARTE)
   {
      (((__EasyOne_terceros11_AddParte)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_terceros11_AddParte"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_ShowResultado)(long p, long resultado);
   HB_FUNC(EASYONE_SHOWRESULTADO)
   {
      (((__EasyOne_ShowResultado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ShowResultado"))(hb_parnl(1), hb_parnl(2)));
   }

   typedef void(__cdecl * __EasyOne_SetQrCodeFormat)(long p, LPSTR newFormat);
   HB_FUNC(EASYONE_SETQRCODEFORMAT)
   {
      (((__EasyOne_SetQrCodeFormat)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetQrCodeFormat"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_SetLogFile)(long p, LPSTR logfile);
   HB_FUNC(EASYONE_SETLOGFILE)
   {
      (((__EasyOne_SetLogFile)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetLogFile"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef LPSTR(__cdecl * __EasyOne_SetLogAs)(LPSTR lang);
   HB_FUNC(EASYONE_SETLOGAS)
   {
      hb_retc(((__EasyOne_SetLogAs)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetLogAs"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_SetInvoiceOneFachadaInfoPrueba)(long p, LPSTR AgenteId, LPSTR SoftwareId);
   HB_FUNC(EASYONE_SETINVOICEONEFACHADAINFOPRUEBA)
   {
      (((__EasyOne_SetInvoiceOneFachadaInfoPrueba)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetInvoiceOneFachadaInfoPrueba"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetInvoiceOneFachadaInfo)(long p, LPSTR AgenteId, LPSTR SoftwareId);
   HB_FUNC(EASYONE_SETINVOICEONEFACHADAINFO)
   {
      (((__EasyOne_SetInvoiceOneFachadaInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetInvoiceOneFachadaInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetFel)(long p, LPSTR url, LPSTR ref, LPSTR user, LPSTR pwd);
   HB_FUNC(EASYONE_SETFEL)
   {
      (((__EasyOne_SetFel)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetFel"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_SetFachadaIOPruebas)(long p, LPSTR UserName, LPSTR PassWord);
   HB_FUNC(EASYONE_SETFACHADAIOPRUEBAS)
   {
      (((__EasyOne_SetFachadaIOPruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetFachadaIOPruebas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetFachadaIOProduccion)(long p, LPSTR UserName, LPSTR PassWord);
   HB_FUNC(EASYONE_SETFACHADAIOPRODUCCION)
   {
      (((__EasyOne_SetFachadaIOProduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetFachadaIOProduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetDesarrolloPruebas)(long p, LPSTR usuario, LPSTR password);
   HB_FUNC(EASYONE_SETDESARROLLOPRUEBAS)
   {
      (((__EasyOne_SetDesarrolloPruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetDesarrolloPruebas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetDesarrolloProduccion)(long p, LPSTR usuario, LPSTR password);
   HB_FUNC(EASYONE_SETDESARROLLOPRODUCCION)
   {
      (((__EasyOne_SetDesarrolloProduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetDesarrolloProduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetDesarrolloFachadaCorePruebas)(long p, LPSTR agente, LPSTR softwareId);
   HB_FUNC(EASYONE_SETDESARROLLOFACHADACOREPRUEBAS)
   {
      (((__EasyOne_SetDesarrolloFachadaCorePruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetDesarrolloFachadaCorePruebas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetDesarrolloFachadaCoreProduccion)(long p, LPSTR agente, LPSTR softwareId);
   HB_FUNC(EASYONE_SETDESARROLLOFACHADACOREPRODUCCION)
   {
      (((__EasyOne_SetDesarrolloFachadaCoreProduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetDesarrolloFachadaCoreProduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetCurlProxy)(long p, LPSTR ProxyUrl);
   HB_FUNC(EASYONE_SETCURLPROXY)
   {
      (((__EasyOne_SetCurlProxy)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetCurlProxy"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_SetCOREPruebas)(long p, LPSTR usuario, LPSTR password);
   HB_FUNC(EASYONE_SETCOREPRUEBAS)
   {
      (((__EasyOne_SetCOREPruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetCOREPruebas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetCOREProduccion)(long p, LPSTR usuario, LPSTR password);
   HB_FUNC(EASYONE_SETCOREPRODUCCION)
   {
      (((__EasyOne_SetCOREProduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetCOREProduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetConectorPruebas)(long p, LPSTR Usuario, LPSTR Contrasena);
   HB_FUNC(EASYONE_SETCONECTORPRUEBAS)
   {
      (((__EasyOne_SetConectorPruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetConectorPruebas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetConectorProduccion)(long p, LPSTR Usuario, LPSTR Contrasena);
   HB_FUNC(EASYONE_SETCONECTORPRODUCCION)
   {
      (((__EasyOne_SetConectorProduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetConectorProduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetConectorCFDI)(long p, LPSTR url, LPSTR agenteId);
   HB_FUNC(EASYONE_SETCONECTORCFDI)
   {
      (((__EasyOne_SetConectorCFDI)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetConectorCFDI"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_SetAddendaXml)(long p, LPSTR AddendaXml);
   HB_FUNC(EASYONE_SETADDENDAXML)
   {
      (((__EasyOne_SetAddendaXml)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetAddendaXml"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_SetAddendaText)(long p, LPSTR AddendaText);
   HB_FUNC(EASYONE_SETADDENDATEXT)
   {
      (((__EasyOne_SetAddendaText)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_SetAddendaText"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef LPSTR(__cdecl * __EasyOne_ServicioSAT_ValidarDescarga)(long p, long cuantos);
   HB_FUNC(EASYONE_SERVICIOSAT_VALIDARDESCARGA)
   {
      hb_retc(((__EasyOne_ServicioSAT_ValidarDescarga)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ValidarDescarga"))(hb_parnl(1), hb_parnl(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_ValidaLicencia)(long p, LPSTR Rfc, LPSTR Lic);
   HB_FUNC(EASYONE_SERVICIOSAT_VALIDALICENCIA)
   {
      (((__EasyOne_ServicioSAT_ValidaLicencia)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ValidaLicencia"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_SetSaveFileName)(long p, LPSTR newFileName);
   HB_FUNC(EASYONE_SERVICIOSAT_SETSAVEFILENAME)
   {
      (((__EasyOne_ServicioSAT_SetSaveFileName)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_SetSaveFileName"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_SetLogFile)(long p, LPSTR fileName);
   HB_FUNC(EASYONE_SERVICIOSAT_SETLOGFILE)
   {
      (((__EasyOne_ServicioSAT_SetLogFile)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_SetLogFile"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_SetDownPath)(long p, LPSTR ruta);
   HB_FUNC(EASYONE_SERVICIOSAT_SETDOWNPATH)
   {
      (((__EasyOne_ServicioSAT_SetDownPath)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_SetDownPath"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_SaveLog)(long p, LPSTR fileName);
   HB_FUNC(EASYONE_SERVICIOSAT_SAVELOG)
   {
      (((__EasyOne_ServicioSAT_SaveLog)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_SaveLog"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef LPSTR(__cdecl * __EasyOne_ServicioSAT_ResolverCaptcha)(long p, long reserved, LPSTR CaptchaFile);
   HB_FUNC(EASYONE_SERVICIOSAT_RESOLVERCAPTCHA)
   {
      hb_retc(((__EasyOne_ServicioSAT_ResolverCaptcha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ResolverCaptcha"))(hb_parnl(1), hb_parnl(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_ObtenerCaptcha)(long p, LPSTR rfc, LPSTR CaptchaFile, LPSTR Licencia);
   HB_FUNC(EASYONE_SERVICIOSAT_OBTENERCAPTCHA)
   {
      (((__EasyOne_ServicioSAT_ObtenerCaptcha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ObtenerCaptcha"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef LPSTR(__cdecl * __EasyOne_ServicioSAT_Obtener)(long p, long NumDocto, LPSTR AttName);
   HB_FUNC(EASYONE_SERVICIOSAT_OBTENER)
   {
      hb_retc(((__EasyOne_ServicioSAT_Obtener)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_Obtener"))(hb_parnl(1), hb_parnl(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_MsgBox)(long p, LPSTR text, LPSTR title);
   HB_FUNC(EASYONE_SERVICIOSAT_MSGBOX)
   {
      (((__EasyOne_ServicioSAT_MsgBox)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_MsgBox"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_LoginC)(long p, LPSTR ciec, LPSTR CaptchaText);
   HB_FUNC(EASYONE_SERVICIOSAT_LOGINC)
   {
      (((__EasyOne_ServicioSAT_LoginC)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_LoginC"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_Login)(long p, LPSTR rfc, LPSTR ciec);
   HB_FUNC(EASYONE_SERVICIOSAT_LOGIN)
   {
      (((__EasyOne_ServicioSAT_Login)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_Login"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_LoadAddInfo)(long p, LPSTR reserved);
   HB_FUNC(EASYONE_SERVICIOSAT_LOADADDINFO)
   {
      (((__EasyOne_ServicioSAT_LoadAddInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_LoadAddInfo"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_GeneralLicenciaRC)(long p, LPSTR usuario, LPSTR password, LPSTR rfcContrtribuyente, LPSTR anios);
   HB_FUNC(EASYONE_SERVICIOSAT_GENERALLICENCIARC)
   {
      (((__EasyOne_ServicioSAT_GeneralLicenciaRC)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_GeneralLicenciaRC"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_Free)(long p);
   HB_FUNC(EASYONE_SERVICIOSAT_FREE)
   {
      (((__EasyOne_ServicioSAT_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_Free"))(hb_parnl(1)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_DownloadAll)(long p, LPSTR path);
   HB_FUNC(EASYONE_SERVICIOSAT_DOWNLOADALL)
   {
      (((__EasyOne_ServicioSAT_DownloadAll)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_DownloadAll"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_Download)(long p, long NumDocto, LPSTR FileName);
   HB_FUNC(EASYONE_SERVICIOSAT_DOWNLOAD)
   {
      (((__EasyOne_ServicioSAT_Download)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_Download"))(hb_parnl(1), hb_parnl(2), (LPSTR) hb_parc(3)));
   }

   typedef long(__cdecl * __EasyOne_ServicioSAT_CuantosHay)(long p);
   HB_FUNC(EASYONE_SERVICIOSAT_CUANTOSHAY)
   {
      hb_retnl(((__EasyOne_ServicioSAT_CuantosHay)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_CuantosHay"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_ServicioSAT_Create)(void);
   HB_FUNC(EASYONE_SERVICIOSAT_CREATE)
   {
      hb_retnl(((__EasyOne_ServicioSAT_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_ConsultaRecibidosPorUUID)(long p, LPSTR UUID);
   HB_FUNC(EASYONE_SERVICIOSAT_CONSULTARECIBIDOSPORUUID)
   {
      (((__EasyOne_ServicioSAT_ConsultaRecibidosPorUUID)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ConsultaRecibidosPorUUID"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_ConsultaRecibidosPorFecha)(long p, LPSTR anioMes, LPSTR dia, LPSTR horaInicial, LPSTR horaFinal, LPSTR rfcEmisor, LPSTR estadoComprobante, LPSTR tipoComplemento);
   HB_FUNC(EASYONE_SERVICIOSAT_CONSULTARECIBIDOSPORFECHA)
   {
      (((__EasyOne_ServicioSAT_ConsultaRecibidosPorFecha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ConsultaRecibidosPorFecha"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_ConsultaEmitidosPorUUID)(long p, LPSTR UUID);
   HB_FUNC(EASYONE_SERVICIOSAT_CONSULTAEMITIDOSPORUUID)
   {
      (((__EasyOne_ServicioSAT_ConsultaEmitidosPorUUID)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ConsultaEmitidosPorUUID"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_ConsultaEmitidosPorFecha)(long p, LPSTR fechaInicial, LPSTR fechaFinal, LPSTR rfcReceptor, LPSTR estadoComprobante, LPSTR tipoComplemento);
   HB_FUNC(EASYONE_SERVICIOSAT_CONSULTAEMITIDOSPORFECHA)
   {
      (((__EasyOne_ServicioSAT_ConsultaEmitidosPorFecha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_ConsultaEmitidosPorFecha"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_CLR_MostrarCaptcha)(long p, LPSTR in, LPSTR out);
   HB_FUNC(EASYONE_SERVICIOSAT_CLR_MOSTRARCAPTCHA)
   {
      (((__EasyOne_ServicioSAT_CLR_MostrarCaptcha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_CLR_MostrarCaptcha"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_CambiarCaptcha)(long p, LPSTR file);
   HB_FUNC(EASYONE_SERVICIOSAT_CAMBIARCAPTCHA)
   {
      (((__EasyOne_ServicioSAT_CambiarCaptcha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_CambiarCaptcha"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_AddXsltInfo)(long p, LPSTR nombre, LPSTR xslt);
   HB_FUNC(EASYONE_SERVICIOSAT_ADDXSLTINFO)
   {
      (((__EasyOne_ServicioSAT_AddXsltInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_AddXsltInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_AddXQueryInfo)(long p, LPSTR nombre, LPSTR xquery);
   HB_FUNC(EASYONE_SERVICIOSAT_ADDXQUERYINFO)
   {
      (((__EasyOne_ServicioSAT_AddXQueryInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_AddXQueryInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_AddXPathInfo)(long p, LPSTR nombre, LPSTR xpath);
   HB_FUNC(EASYONE_SERVICIOSAT_ADDXPATHINFO)
   {
      (((__EasyOne_ServicioSAT_AddXPathInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_AddXPathInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_AddLicence)(long p, LPSTR distId, LPSTR rfcCont, LPSTR anio, LPSTR lic);
   HB_FUNC(EASYONE_SERVICIOSAT_ADDLICENCE)
   {
      (((__EasyOne_ServicioSAT_AddLicence)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_AddLicence"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_ServicioSAT_AddConsultaEmitidosPorEjercicio)(long p, LPSTR anio, LPSTR rfcReceptor, LPSTR estadoComprobante, LPSTR tipoComplemento);
   HB_FUNC(EASYONE_SERVICIOSAT_ADDCONSULTAEMITIDOSPOREJERCICIO)
   {
      (((__EasyOne_ServicioSAT_AddConsultaEmitidosPorEjercicio)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ServicioSAT_AddConsultaEmitidosPorEjercicio"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_servicioparcial10_Setparcialesconstruccion)(long p, LPSTR NumPerLicoAut);
   HB_FUNC(EASYONE_SERVICIOPARCIAL10_SETPARCIALESCONSTRUCCION)
   {
      (((__EasyOne_servicioparcial10_Setparcialesconstruccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_servicioparcial10_Setparcialesconstruccion"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_servicioparcial10_SetInmueble)(long p, LPSTR Calle, LPSTR NoExterior, LPSTR NoInterior, LPSTR Colonia, LPSTR Localidad, LPSTR Referencia, LPSTR Municipio, LPSTR Estado, LPSTR CodigoPostal);
   HB_FUNC(EASYONE_SERVICIOPARCIAL10_SETINMUEBLE)
   {
      (((__EasyOne_servicioparcial10_SetInmueble)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_servicioparcial10_SetInmueble"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10)));
   }

   typedef void(__cdecl * __EasyOne_servicioparcial10_Free)(long p);
   HB_FUNC(EASYONE_SERVICIOPARCIAL10_FREE)
   {
      (((__EasyOne_servicioparcial10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_servicioparcial10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_servicioparcial10_Create)(void);
   HB_FUNC(EASYONE_SERVICIOPARCIAL10_CREATE)
   {
      hb_retnl(((__EasyOne_servicioparcial10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_servicioparcial10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_sectorfinanciero10_SetSectorFinanciero)(long p, LPSTR IdFideicom, LPSTR NomFideicom, LPSTR DescripFideicom);
   HB_FUNC(EASYONE_SECTORFINANCIERO10_SETSECTORFINANCIERO)
   {
      (((__EasyOne_sectorfinanciero10_SetSectorFinanciero)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_sectorfinanciero10_SetSectorFinanciero"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_sectorfinanciero10_Free)(long p);
   HB_FUNC(EASYONE_SECTORFINANCIERO10_FREE)
   {
      (((__EasyOne_sectorfinanciero10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_sectorfinanciero10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_sectorfinanciero10_Create)(void);
   HB_FUNC(EASYONE_SECTORFINANCIERO10_CREATE)
   {
      hb_retnl(((__EasyOne_sectorfinanciero10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_sectorfinanciero10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetTotales)(long p, LPSTR montoTotOperacion, LPSTR montoTotGrav, LPSTR montoTotExent, LPSTR montoTotRet);
   HB_FUNC(EASYONE_RETENCIONES10_SETTOTALES)
   {
      (((__EasyOne_retenciones10_SetTotales)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetTotales"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetRetenciones)(long p, LPSTR FolioInt, LPSTR FechaExp, LPSTR CveRetenc, LPSTR DescRetenc);
   HB_FUNC(EASYONE_RETENCIONES10_SETRETENCIONES)
   {
      (((__EasyOne_retenciones10_SetRetenciones)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetRetenciones"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetReceptor)(long p, LPSTR Nacionalidad);
   HB_FUNC(EASYONE_RETENCIONES10_SETRECEPTOR)
   {
      (((__EasyOne_retenciones10_SetReceptor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetReceptor"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetPeriodo)(long p, LPSTR MesIni, LPSTR MesFin, LPSTR Ejerc);
   HB_FUNC(EASYONE_RETENCIONES10_SETPERIODO)
   {
      (((__EasyOne_retenciones10_SetPeriodo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetPeriodo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetNacional)(long p, LPSTR RFCRecep, LPSTR NomDenRazSocR, LPSTR CURPR);
   HB_FUNC(EASYONE_RETENCIONES10_SETNACIONAL)
   {
      (((__EasyOne_retenciones10_SetNacional)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetNacional"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetExtranjero)(long p, LPSTR NumRegIdTrib, LPSTR NomDenRazSocR);
   HB_FUNC(EASYONE_RETENCIONES10_SETEXTRANJERO)
   {
      (((__EasyOne_retenciones10_SetExtranjero)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetExtranjero"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_SetEmisor)(long p, LPSTR RFCEmisor, LPSTR NomDenRazSocE, LPSTR CURPE);
   HB_FUNC(EASYONE_RETENCIONES10_SETEMISOR)
   {
      (((__EasyOne_retenciones10_SetEmisor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_SetEmisor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_Free)(long p);
   HB_FUNC(EASYONE_RETENCIONES10_FREE)
   {
      (((__EasyOne_retenciones10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_retenciones10_Create)(LPSTR OutFile);
   HB_FUNC(EASYONE_RETENCIONES10_CREATE)
   {
      hb_retnl(((__EasyOne_retenciones10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_Create"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_retenciones10_AddImpRetenidos)(long p, LPSTR BaseRet, LPSTR Impuesto, LPSTR montoRet, LPSTR TipoPagoRet);
   HB_FUNC(EASYONE_RETENCIONES10_ADDIMPRETENIDOS)
   {
      (((__EasyOne_retenciones10_AddImpRetenidos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_retenciones10_AddImpRetenidos"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef LPSTR(__cdecl * __EasyOne_ReadFromFile)(LPSTR FileName);
   HB_FUNC(EASYONE_READFROMFILE)
   {
      hb_retc(((__EasyOne_ReadFromFile)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ReadFromFile"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_premios10_SetPremios)(long p, LPSTR EntidadFederativa, LPSTR MontTotPago, LPSTR MontTotPagoGrav, LPSTR MontTotPagoExent);
   HB_FUNC(EASYONE_PREMIOS10_SETPREMIOS)
   {
      (((__EasyOne_premios10_SetPremios)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_premios10_SetPremios"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_premios10_Free)(long p);
   HB_FUNC(EASYONE_PREMIOS10_FREE)
   {
      (((__EasyOne_premios10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_premios10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_premios10_Create)(void);
   HB_FUNC(EASYONE_PREMIOS10_CREATE)
   {
      hb_retnl(((__EasyOne_premios10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_premios10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_planesderetiro10_SetPlanesderetiro)(long p, LPSTR SistemaFinanc, LPSTR MontTotAportAnioInmAnterior, LPSTR MontIntRealesDevengAniooInmAnt, LPSTR HuboRetirosAnioInmAntPer, LPSTR MontTotRetiradoAnioInmAntPer, LPSTR MontTotExentRetiradoAnioInmAnt, LPSTR MontTotExedenteAnioInmAnt, LPSTR HuboRetirosAnioInmAnt, LPSTR MontTotRetiradoAnioInmAnt);
   HB_FUNC(EASYONE_PLANESDERETIRO10_SETPLANESDERETIRO)
   {
      (((__EasyOne_planesderetiro10_SetPlanesderetiro)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_planesderetiro10_SetPlanesderetiro"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10)));
   }

   typedef void(__cdecl * __EasyOne_planesderetiro10_Free)(long p);
   HB_FUNC(EASYONE_PLANESDERETIRO10_FREE)
   {
      (((__EasyOne_planesderetiro10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_planesderetiro10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_planesderetiro10_Create)(void);
   HB_FUNC(EASYONE_PLANESDERETIRO10_CREATE)
   {
      hb_retnl(((__EasyOne_planesderetiro10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_planesderetiro10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_pfic10_SetPFintegranteCoordinado)(long p, LPSTR ClaveVehicular, LPSTR Placa, LPSTR RFCPF);
   HB_FUNC(EASYONE_PFIC10_SETPFINTEGRANTECOORDINADO)
   {
      (((__EasyOne_pfic10_SetPFintegranteCoordinado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pfic10_SetPFintegranteCoordinado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_pfic10_Free)(long p);
   HB_FUNC(EASYONE_PFIC10_FREE)
   {
      (((__EasyOne_pfic10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pfic10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_pfic10_Create)(void);
   HB_FUNC(EASYONE_PFIC10_CREATE)
   {
      hb_retnl(((__EasyOne_pfic10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pfic10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_pagosaextranjeros10_SetPagosaextranjeros)(long p, LPSTR EsBenefEfectDelCobro);
   HB_FUNC(EASYONE_PAGOSAEXTRANJEROS10_SETPAGOSAEXTRANJEROS)
   {
      (((__EasyOne_pagosaextranjeros10_SetPagosaextranjeros)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagosaextranjeros10_SetPagosaextranjeros"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_pagosaextranjeros10_SetNoBeneficiario)(long p, LPSTR PaisDeResidParaEfecFisc, LPSTR ConceptoPago, LPSTR DescripcionConcepto);
   HB_FUNC(EASYONE_PAGOSAEXTRANJEROS10_SETNOBENEFICIARIO)
   {
      (((__EasyOne_pagosaextranjeros10_SetNoBeneficiario)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagosaextranjeros10_SetNoBeneficiario"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_pagosaextranjeros10_SetBeneficiario)(long p, LPSTR RFC, LPSTR CURP, LPSTR NomDenRazSocB, LPSTR ConceptoPago, LPSTR DescripcionConcepto);
   HB_FUNC(EASYONE_PAGOSAEXTRANJEROS10_SETBENEFICIARIO)
   {
      (((__EasyOne_pagosaextranjeros10_SetBeneficiario)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagosaextranjeros10_SetBeneficiario"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_pagosaextranjeros10_Free)(long p);
   HB_FUNC(EASYONE_PAGOSAEXTRANJEROS10_FREE)
   {
      (((__EasyOne_pagosaextranjeros10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagosaextranjeros10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_pagosaextranjeros10_Create)(void);
   HB_FUNC(EASYONE_PAGOSAEXTRANJEROS10_CREATE)
   {
      hb_retnl(((__EasyOne_pagosaextranjeros10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagosaextranjeros10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_pagoenespecie10_SetPagoEnEspecie)(long p, LPSTR CvePIC, LPSTR FolioSolDon, LPSTR PzaArtNombre, LPSTR PzaArtTecn, LPSTR PzaArtAProd, LPSTR PzaArtDim);
   HB_FUNC(EASYONE_PAGOENESPECIE10_SETPAGOENESPECIE)
   {
      (((__EasyOne_pagoenespecie10_SetPagoEnEspecie)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagoenespecie10_SetPagoEnEspecie"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_pagoenespecie10_Free)(long p);
   HB_FUNC(EASYONE_PAGOENESPECIE10_FREE)
   {
      (((__EasyOne_pagoenespecie10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagoenespecie10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_pagoenespecie10_Create)(void);
   HB_FUNC(EASYONE_PAGOENESPECIE10_CREATE)
   {
      hb_retnl(((__EasyOne_pagoenespecie10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pagoenespecie10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_pago10_Free)(long p);
   HB_FUNC(EASYONE_PAGO10_FREE)
   {
      (((__EasyOne_pago10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pago10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_pago10_Create)(void);
   HB_FUNC(EASYONE_PAGO10_CREATE)
   {
      hb_retnl(((__EasyOne_pago10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pago10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_pago10_AddPago)(long p, LPSTR FechaPago, LPSTR FormaDePagoP, LPSTR MonedaP, LPSTR TipoCambioP, LPSTR Monto, LPSTR NumOperacion, LPSTR RfcEmisorCtaOrd, LPSTR NomBancoOrdExt, LPSTR CtaOrdenante, LPSTR RfcEmisorCtaBen, LPSTR CtaBeneficiario, LPSTR TipoCadPago, LPSTR CertPago, LPSTR CadPago, LPSTR SelloPago);
   HB_FUNC(EASYONE_PAGO10_ADDPAGO)
   {
      (((__EasyOne_pago10_AddPago)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pago10_AddPago"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13), (LPSTR) hb_parc(14), (LPSTR) hb_parc(15), (LPSTR) hb_parc(16)));
   }

   typedef void(__cdecl * __EasyOne_pago10_AddDoctoRelacionado)(long p, LPSTR IdDocumento, LPSTR Serie, LPSTR Folio, LPSTR MonedaDR, LPSTR TipoCambioDR, LPSTR MetodoDePagoDR, LPSTR NumParcialidad, LPSTR ImpSaldoAnt, LPSTR ImpPagado, LPSTR ImpSaldoInsoluto);
   HB_FUNC(EASYONE_PAGO10_ADDDOCTORELACIONADO)
   {
      (((__EasyOne_pago10_AddDoctoRelacionado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_pago10_AddDoctoRelacionado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11)));
   }

   typedef void(__cdecl * __EasyOne_operacionesconderivados10_SetOperacionesconderivados)(long p, LPSTR MontGanAcum, LPSTR MontPerdDed);
   HB_FUNC(EASYONE_OPERACIONESCONDERIVADOS10_SETOPERACIONESCONDERIVADOS)
   {
      (((__EasyOne_operacionesconderivados10_SetOperacionesconderivados)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_operacionesconderivados10_SetOperacionesconderivados"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_operacionesconderivados10_Free)(long p);
   HB_FUNC(EASYONE_OPERACIONESCONDERIVADOS10_FREE)
   {
      (((__EasyOne_operacionesconderivados10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_operacionesconderivados10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_operacionesconderivados10_Create)(void);
   HB_FUNC(EASYONE_OPERACIONESCONDERIVADOS10_CREATE)
   {
      hb_retnl(((__EasyOne_operacionesconderivados10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_operacionesconderivados10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_obrasarte10_Setobrasarteantiguedades)(long p, LPSTR TipoBien, LPSTR OtrosTipoBien, LPSTR TituloAdquirido, LPSTR OtrosTituloAdquirido, LPSTR Subtotal, LPSTR IVA, LPSTR FechaAdquisicion, LPSTR CaracteristicasDeObraoPieza);
   HB_FUNC(EASYONE_OBRASARTE10_SETOBRASARTEANTIGUEDADES)
   {
      (((__EasyOne_obrasarte10_Setobrasarteantiguedades)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_obrasarte10_Setobrasarteantiguedades"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9)));
   }

   typedef void(__cdecl * __EasyOne_obrasarte10_Free)(long p);
   HB_FUNC(EASYONE_OBRASARTE10_FREE)
   {
      (((__EasyOne_obrasarte10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_obrasarte10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_obrasarte10_Create)(void);
   HB_FUNC(EASYONE_OBRASARTE10_CREATE)
   {
      hb_retnl(((__EasyOne_obrasarte10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_obrasarte10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_SetDatosUnEnajenante)(long p, LPSTR Nombre, LPSTR ApellidoPaterno, LPSTR ApellidoMaterno, LPSTR RFC, LPSTR CURP);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_SETDATOSUNENAJENANTE)
   {
      (((__EasyOne_notariospublicos10_SetDatosUnEnajenante)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_SetDatosUnEnajenante"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_SetDatosUnAdquiriente)(long p, LPSTR Nombre, LPSTR ApellidoPaterno, LPSTR ApellidoMaterno, LPSTR RFC, LPSTR CURP);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_SETDATOSUNADQUIRIENTE)
   {
      (((__EasyOne_notariospublicos10_SetDatosUnAdquiriente)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_SetDatosUnAdquiriente"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_SetDatosOperacion)(long p, LPSTR NumInstrumentoNotarial, LPSTR FechaInstNotarial, LPSTR MontoOperacion, LPSTR Subtotal, LPSTR IVA);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_SETDATOSOPERACION)
   {
      (((__EasyOne_notariospublicos10_SetDatosOperacion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_SetDatosOperacion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_SetDatosNotario)(long p, LPSTR CURP, LPSTR NumNotaria, LPSTR EntidadFederativa, LPSTR Adscripcion);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_SETDATOSNOTARIO)
   {
      (((__EasyOne_notariospublicos10_SetDatosNotario)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_SetDatosNotario"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_SetDatosEnajenante)(long p, LPSTR CoproSocConyugalE);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_SETDATOSENAJENANTE)
   {
      (((__EasyOne_notariospublicos10_SetDatosEnajenante)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_SetDatosEnajenante"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_SetDatosAdquiriente)(long p, LPSTR CoproSocConyugalE);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_SETDATOSADQUIRIENTE)
   {
      (((__EasyOne_notariospublicos10_SetDatosAdquiriente)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_SetDatosAdquiriente"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_Free)(long p);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_FREE)
   {
      (((__EasyOne_notariospublicos10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_notariospublicos10_Create)(void);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_CREATE)
   {
      hb_retnl(((__EasyOne_notariospublicos10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_AddDescInmueble)(long p, LPSTR TipoInmueble, LPSTR Calle, LPSTR NoExterior, LPSTR NoInterior, LPSTR Colonia, LPSTR Localidad, LPSTR Referencia, LPSTR Municipio, LPSTR Estado, LPSTR Pais, LPSTR CodigoPostal);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_ADDDESCINMUEBLE)
   {
      (((__EasyOne_notariospublicos10_AddDescInmueble)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_AddDescInmueble"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_AddDatosEnajenanteCopSC)(long p, LPSTR Nombre, LPSTR ApellidoPaterno, LPSTR ApellidoMaterno, LPSTR RFC, LPSTR CURP, LPSTR Porcentaje);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_ADDDATOSENAJENANTECOPSC)
   {
      (((__EasyOne_notariospublicos10_AddDatosEnajenanteCopSC)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_AddDatosEnajenanteCopSC"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_notariospublicos10_AddDatosAdquirienteCopSC)(long p, LPSTR Nombre, LPSTR ApellidoPaterno, LPSTR ApellidoMaterno, LPSTR RFC, LPSTR CURP, LPSTR Porcentaje);
   HB_FUNC(EASYONE_NOTARIOSPUBLICOS10_ADDDATOSADQUIRIENTECOPSC)
   {
      (((__EasyOne_notariospublicos10_AddDatosAdquirienteCopSC)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_notariospublicos10_AddDatosAdquirienteCopSC"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetSubsidioAlEmpleo)(long p, LPSTR SubsidioCausado);
   HB_FUNC(EASYONE_NOMINA12_SETSUBSIDIOALEMPLEO)
   {
      (((__EasyOne_nomina12_SetSubsidioAlEmpleo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetSubsidioAlEmpleo"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetSeparacionIndemnizacion)(long p, LPSTR TotalPagado, LPSTR NumAniosServicio, LPSTR UltimoSueldoMensOrd, LPSTR IngresoAcumulable, LPSTR IngresoNoAcumulable);
   HB_FUNC(EASYONE_NOMINA12_SETSEPARACIONINDEMNIZACION)
   {
      (((__EasyOne_nomina12_SetSeparacionIndemnizacion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetSeparacionIndemnizacion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetReceptor)(long p, LPSTR Curp, LPSTR NumSeguridadSocial, LPSTR FechaInicioRelLaboral, LPSTR Antiguedad, LPSTR TipoContrato, LPSTR Sindicalizado, LPSTR TipoJornada, LPSTR TipoRegimen, LPSTR NumEmpleado, LPSTR Departamento, LPSTR Puesto, LPSTR RiesgoPuesto, LPSTR PeriodicidadPago, LPSTR Banco, LPSTR CuentaBancaria, LPSTR SalarioBaseCotApor, LPSTR SalarioDiarioIntegrado, LPSTR ClaveEntFed);
   HB_FUNC(EASYONE_NOMINA12_SETRECEPTOR)
   {
      (((__EasyOne_nomina12_SetReceptor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetReceptor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13), (LPSTR) hb_parc(14), (LPSTR) hb_parc(15), (LPSTR) hb_parc(16), (LPSTR) hb_parc(17), (LPSTR) hb_parc(18), (LPSTR) hb_parc(19)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetPercepciones)(long p, LPSTR TotalSueldos, LPSTR TotalSeparacionIndemnizacion, LPSTR TotalJubilacionPensionRetiro, LPSTR TotalGravado, LPSTR TotalExento);
   HB_FUNC(EASYONE_NOMINA12_SETPERCEPCIONES)
   {
      (((__EasyOne_nomina12_SetPercepciones)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetPercepciones"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetNomina)(long p, LPSTR TipoNomina, LPSTR FechaPago, LPSTR FechaInicialPago, LPSTR FechaFinalPago, LPSTR NumDiasPagados, LPSTR TotalPercepciones, LPSTR TotalDeducciones, LPSTR TotalOtrosPagos);
   HB_FUNC(EASYONE_NOMINA12_SETNOMINA)
   {
      (((__EasyOne_nomina12_SetNomina)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetNomina"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetJubilacionPensionRetiro)(long p, LPSTR TotalUnaExhibicion, LPSTR TotalParcialidad, LPSTR MontoDiario, LPSTR IngresoAcumulable, LPSTR IngresoNoAcumulable);
   HB_FUNC(EASYONE_NOMINA12_SETJUBILACIONPENSIONRETIRO)
   {
      (((__EasyOne_nomina12_SetJubilacionPensionRetiro)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetJubilacionPensionRetiro"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetEntidadSNCF)(long p, LPSTR OrigenRecurso, LPSTR MontoRecursoPropio);
   HB_FUNC(EASYONE_NOMINA12_SETENTIDADSNCF)
   {
      (((__EasyOne_nomina12_SetEntidadSNCF)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetEntidadSNCF"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetEmisor)(long p, LPSTR Curp, LPSTR RegistroPatronal, LPSTR RfcPatronOrigen);
   HB_FUNC(EASYONE_NOMINA12_SETEMISOR)
   {
      (((__EasyOne_nomina12_SetEmisor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetEmisor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetDeducciones)(long p, LPSTR TotalOtrasDeducciones, LPSTR TotalImpuestosRetenidos);
   HB_FUNC(EASYONE_NOMINA12_SETDEDUCCIONES)
   {
      (((__EasyOne_nomina12_SetDeducciones)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetDeducciones"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetCompensacionSaldosAFavor)(long p, LPSTR SaldoAFavor, LPSTR Anio, LPSTR RemanenteSalFav);
   HB_FUNC(EASYONE_NOMINA12_SETCOMPENSACIONSALDOSAFAVOR)
   {
      (((__EasyOne_nomina12_SetCompensacionSaldosAFavor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetCompensacionSaldosAFavor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_SetAccionesOTitulos)(long p, LPSTR ValorMercado, LPSTR PrecioAlOtorgarse);
   HB_FUNC(EASYONE_NOMINA12_SETACCIONESOTITULOS)
   {
      (((__EasyOne_nomina12_SetAccionesOTitulos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_SetAccionesOTitulos"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_Free)(long p);
   HB_FUNC(EASYONE_NOMINA12_FREE)
   {
      (((__EasyOne_nomina12_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_nomina12_Create)(void);
   HB_FUNC(EASYONE_NOMINA12_CREATE)
   {
      hb_retnl(((__EasyOne_nomina12_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_Create"))());
   }

   typedef void(__cdecl * __EasyOne_nomina12_AddSubContratacion)(long p, LPSTR RfcLabora, LPSTR PorcentajeTiempo);
   HB_FUNC(EASYONE_NOMINA12_ADDSUBCONTRATACION)
   {
      (((__EasyOne_nomina12_AddSubContratacion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_AddSubContratacion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_AddPercepcion)(long p, LPSTR TipoPercepcion, LPSTR Clave, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExento);
   HB_FUNC(EASYONE_NOMINA12_ADDPERCEPCION)
   {
      (((__EasyOne_nomina12_AddPercepcion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_AddPercepcion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_AddOtroPago)(long p, LPSTR TipoOtroPago, LPSTR Clave, LPSTR Concepto, LPSTR Importe);
   HB_FUNC(EASYONE_NOMINA12_ADDOTROPAGO)
   {
      (((__EasyOne_nomina12_AddOtroPago)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_AddOtroPago"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_AddIncapacidad)(long p, LPSTR DiasIncapacidad, LPSTR TipoIncapacidad, LPSTR ImporteMonetario);
   HB_FUNC(EASYONE_NOMINA12_ADDINCAPACIDAD)
   {
      (((__EasyOne_nomina12_AddIncapacidad)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_AddIncapacidad"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_AddHorasExtra)(long p, LPSTR Dias, LPSTR TipoHoras, LPSTR HorasExtra, LPSTR ImportePagado);
   HB_FUNC(EASYONE_NOMINA12_ADDHORASEXTRA)
   {
      (((__EasyOne_nomina12_AddHorasExtra)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_AddHorasExtra"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_nomina12_AddDeduccion)(long p, LPSTR TipoDeduccion, LPSTR Clave, LPSTR Concepto, LPSTR Importe);
   HB_FUNC(EASYONE_NOMINA12_ADDDEDUCCION)
   {
      (((__EasyOne_nomina12_AddDeduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nomina12_AddDeduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetSeparacionInfo)(long p, LPSTR TotalPagado, LPSTR NumAniosServicio, LPSTR UltimoSueldoMensOrd, LPSTR IngresoAcumulable, LPSTR IngresoNoAcumulable);
   HB_FUNC(EASYONE_ND_SETSEPARACIONINFO)
   {
      (((__EasyOne_nd_SetSeparacionInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetSeparacionInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetReceptor)(long p, LPSTR rfc, LPSTR nombre, LPSTR NumEmpleado, LPSTR Departamento, LPSTR Puesto, LPSTR ClaveEntFed, LPSTR Curp);
   HB_FUNC(EASYONE_ND_SETRECEPTOR)
   {
      (((__EasyOne_nd_SetReceptor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetReceptor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetPercepcionJubilacionTotal)(long p, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento, LPSTR TotalUnaExibicion, LPSTR IngresoAcumulable, LPSTR IngresoNoAcumulable);
   HB_FUNC(EASYONE_ND_SETPERCEPCIONJUBILACIONTOTAL)
   {
      (((__EasyOne_nd_SetPercepcionJubilacionTotal)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetPercepcionJubilacionTotal"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetPercepcionJubilacionParcial)(long p, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento, LPSTR TotalParcialidad, LPSTR MontoDiario, LPSTR IngresoAcumulable, LPSTR IngresoNoAcumulable);
   HB_FUNC(EASYONE_ND_SETPERCEPCIONJUBILACIONPARCIAL)
   {
      (((__EasyOne_nd_SetPercepcionJubilacionParcial)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetPercepcionJubilacionParcial"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetOtrosIngresos)(long p, LPSTR OrigenRecurso);
   HB_FUNC(EASYONE_ND_SETOTROSINGRESOS)
   {
      (((__EasyOne_nd_SetOtrosIngresos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetOtrosIngresos"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetOtro)(long p, LPSTR Banco, LPSTR CuentaBancaria);
   HB_FUNC(EASYONE_ND_SETOTRO)
   {
      (((__EasyOne_nd_SetOtro)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetOtro"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetOrdinariaNoLaboral)(long p, LPSTR TipoContrato, LPSTR PeriodicidadPago, LPSTR TipoRegimen);
   HB_FUNC(EASYONE_ND_SETORDINARIANOLABORAL)
   {
      (((__EasyOne_nd_SetOrdinariaNoLaboral)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetOrdinariaNoLaboral"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetOrdinariaLaboral)(long p, LPSTR TipoContrato, LPSTR PeriodicidadPago, LPSTR NumSeguridadSocial, LPSTR FechaInicioRelLaboral, LPSTR Antiguedad, LPSTR RiesgoPuesto, LPSTR SalarioDiarioIntegrado, LPSTR SalarioBaseCotApor, LPSTR Sindicalizado, LPSTR TipoJornada, LPSTR TipoRegimen, LPSTR RegistroPatronal);
   HB_FUNC(EASYONE_ND_SETORDINARIALABORAL)
   {
      (((__EasyOne_nd_SetOrdinariaLaboral)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetOrdinariaLaboral"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetNominaDigital)(long p, LPSTR serie, LPSTR folio, LPSTR fecha, LPSTR LugarExpedicion, LPSTR RfcPatronOrigen, LPSTR FechaPago, LPSTR FechaInicialPago, LPSTR FechaFinalPago, LPSTR NumDiasPagados);
   HB_FUNC(EASYONE_ND_SETNOMINADIGITAL)
   {
      (((__EasyOne_nd_SetNominaDigital)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetNominaDigital"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetIngresosMixtos)(long p, LPSTR MontoRecursoPropio);
   HB_FUNC(EASYONE_ND_SETINGRESOSMIXTOS)
   {
      (((__EasyOne_nd_SetIngresosMixtos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetIngresosMixtos"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetExtraordinaria)(long p, LPSTR PeriodicidadPago, LPSTR TipoRegimen);
   HB_FUNC(EASYONE_ND_SETEXTRAORDINARIA)
   {
      (((__EasyOne_nd_SetExtraordinaria)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetExtraordinaria"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetEmisorPersonaMoral)(long p, LPSTR rfc, LPSTR nombre, LPSTR Regimen);
   HB_FUNC(EASYONE_ND_SETEMISORPERSONAMORAL)
   {
      (((__EasyOne_nd_SetEmisorPersonaMoral)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetEmisorPersonaMoral"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetEmisorPersonaFisica)(long p, LPSTR rfc, LPSTR Curp, LPSTR nombre, LPSTR Regimen);
   HB_FUNC(EASYONE_ND_SETEMISORPERSONAFISICA)
   {
      (((__EasyOne_nd_SetEmisorPersonaFisica)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetEmisorPersonaFisica"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_nd_SetClabe)(long p, LPSTR CuentaBancaria);
   HB_FUNC(EASYONE_ND_SETCLABE)
   {
      (((__EasyOne_nd_SetClabe)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_SetClabe"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_nd_Free)(long p);
   HB_FUNC(EASYONE_ND_FREE)
   {
      (((__EasyOne_nd_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_nd_Create)(LPSTR OutFile);
   HB_FUNC(EASYONE_ND_CREATE)
   {
      hb_retnl(((__EasyOne_nd_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_Create"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddSubContratacionTercero)(long p, LPSTR RfcLabora, LPSTR PorcentajeTiempo);
   HB_FUNC(EASYONE_ND_ADDSUBCONTRATACIONTERCERO)
   {
      (((__EasyOne_nd_AddSubContratacionTercero)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddSubContratacionTercero"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddPercepcionSubsidioIncapacidad)(long p, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento, LPSTR DiasIncapacidad, LPSTR Tipo);
   HB_FUNC(EASYONE_ND_ADDPERCEPCIONSUBSIDIOINCAPACIDAD)
   {
      (((__EasyOne_nd_AddPercepcionSubsidioIncapacidad)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddPercepcionSubsidioIncapacidad"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddPercepcionSeparacion)(long p, LPSTR ClaveSAT, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento);
   HB_FUNC(EASYONE_ND_ADDPERCEPCIONSEPARACION)
   {
      (((__EasyOne_nd_AddPercepcionSeparacion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddPercepcionSeparacion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddPercepcionHorasExtra)(long p, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento, LPSTR Dias, LPSTR TipoHoras, LPSTR HorasExtra);
   HB_FUNC(EASYONE_ND_ADDPERCEPCIONHORASEXTRA)
   {
      (((__EasyOne_nd_AddPercepcionHorasExtra)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddPercepcionHorasExtra"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddPercepcionAccionesOTitulos)(long p, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento, LPSTR ValorMercado, LPSTR PrecioAlOtorgarse);
   HB_FUNC(EASYONE_ND_ADDPERCEPCIONACCIONESOTITULOS)
   {
      (((__EasyOne_nd_AddPercepcionAccionesOTitulos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddPercepcionAccionesOTitulos"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddPercepcion)(long p, LPSTR ClaveSAT, LPSTR ClaveContable, LPSTR Concepto, LPSTR ImporteGravado, LPSTR ImporteExcento);
   HB_FUNC(EASYONE_ND_ADDPERCEPCION)
   {
      (((__EasyOne_nd_AddPercepcion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddPercepcion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddOtrosSubsidioParaEmpleo)(long p, LPSTR Clave, LPSTR Concepto, LPSTR Importe, LPSTR SubsidioCausado);
   HB_FUNC(EASYONE_ND_ADDOTROSSUBSIDIOPARAEMPLEO)
   {
      (((__EasyOne_nd_AddOtrosSubsidioParaEmpleo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddOtrosSubsidioParaEmpleo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddOtrosCompensacion)(long p, LPSTR Clave, LPSTR Concepto, LPSTR Importe, LPSTR SaldoAFavor, LPSTR Anio, LPSTR RemanenteSalFav);
   HB_FUNC(EASYONE_ND_ADDOTROSCOMPENSACION)
   {
      (((__EasyOne_nd_AddOtrosCompensacion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddOtrosCompensacion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddOtros)(long p, LPSTR TipoOtroPago, LPSTR Clave, LPSTR Concepto, LPSTR Importe);
   HB_FUNC(EASYONE_ND_ADDOTROS)
   {
      (((__EasyOne_nd_AddOtros)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddOtros"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddDeduccionIncapacidad)(long p, LPSTR Clave, LPSTR Concepto, LPSTR Importe, LPSTR DiasIncapacidad, LPSTR TipoIncapacidad);
   HB_FUNC(EASYONE_ND_ADDDEDUCCIONINCAPACIDAD)
   {
      (((__EasyOne_nd_AddDeduccionIncapacidad)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddDeduccionIncapacidad"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_nd_AddDeduccion)(long p, LPSTR TipoDeduccion, LPSTR Clave, LPSTR Concepto, LPSTR Importe);
   HB_FUNC(EASYONE_ND_ADDDEDUCCION)
   {
      (((__EasyOne_nd_AddDeduccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_nd_AddDeduccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef long(__cdecl * __EasyOne_MsgBox)(LPSTR msg, LPSTR title);
   HB_FUNC(EASYONE_MSGBOX)
   {
      hb_retnl(((__EasyOne_MsgBox)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MsgBox"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetTo)(long p, LPSTR ToMail);
   HB_FUNC(EASYONE_MAILSERVICE_SETTO)
   {
      (((__EasyOne_MailService_SetTo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetTo"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetSecurityType)(long p, LPSTR None_TLS_SSL);
   HB_FUNC(EASYONE_MAILSERVICE_SETSECURITYTYPE)
   {
      (((__EasyOne_MailService_SetSecurityType)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetSecurityType"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetPriority)(long p, LPSTR High_Normal_Low);
   HB_FUNC(EASYONE_MAILSERVICE_SETPRIORITY)
   {
      (((__EasyOne_MailService_SetPriority)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetPriority"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetMsg)(long p, LPSTR Subject, LPSTR BodyMsg);
   HB_FUNC(EASYONE_MAILSERVICE_SETMSG)
   {
      (((__EasyOne_MailService_SetMsg)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetMsg"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetIsBodyHtml)(long p, LPSTR True_False);
   HB_FUNC(EASYONE_MAILSERVICE_SETISBODYHTML)
   {
      (((__EasyOne_MailService_SetIsBodyHtml)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetIsBodyHtml"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetHost)(long p, LPSTR host, LPSTR port);
   HB_FUNC(EASYONE_MAILSERVICE_SETHOST)
   {
      (((__EasyOne_MailService_SetHost)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetHost"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetFrom)(long p, LPSTR Email, LPSTR Name);
   HB_FUNC(EASYONE_MAILSERVICE_SETFROM)
   {
      (((__EasyOne_MailService_SetFrom)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetFrom"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetErrorFile)(long p, LPSTR ErrorFilePath);
   HB_FUNC(EASYONE_MAILSERVICE_SETERRORFILE)
   {
      (((__EasyOne_MailService_SetErrorFile)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetErrorFile"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetCredentials)(long p, LPSTR UserName, LPSTR Password);
   HB_FUNC(EASYONE_MAILSERVICE_SETCREDENTIALS)
   {
      (((__EasyOne_MailService_SetCredentials)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetCredentials"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_MailService_SetAttachment)(long p, LPSTR AttachmentPath);
   HB_FUNC(EASYONE_MAILSERVICE_SETATTACHMENT)
   {
      (((__EasyOne_MailService_SetAttachment)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_SetAttachment"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef LPSTR(__cdecl * __EasyOne_MailService_Send)(long p);
   HB_FUNC(EASYONE_MAILSERVICE_SEND)
   {
      hb_retc(((__EasyOne_MailService_Send)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_Send"))(hb_parnl(1)));
   }

   typedef void(__cdecl * __EasyOne_MailService_Free)(long p);
   HB_FUNC(EASYONE_MAILSERVICE_FREE)
   {
      (((__EasyOne_MailService_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_MailService_Create)(void);
   HB_FUNC(EASYONE_MAILSERVICE_CREATE)
   {
      hb_retnl(((__EasyOne_MailService_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_MailService_Create"))());
   }

   typedef void(__cdecl * __EasyOne_LoadXml)(long p, LPSTR xmlText);
   HB_FUNC(EASYONE_LOADXML)
   {
      (((__EasyOne_LoadXml)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_LoadXml"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_leyendasFisc10_Free)(long p);
   HB_FUNC(EASYONE_LEYENDASFISC10_FREE)
   {
      (((__EasyOne_leyendasFisc10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_leyendasFisc10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_leyendasFisc10_Create)(void);
   HB_FUNC(EASYONE_LEYENDASFISC10_CREATE)
   {
      hb_retnl(((__EasyOne_leyendasFisc10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_leyendasFisc10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_leyendasFisc10_AddLeyenda)(long p, LPSTR disposicionFiscal, LPSTR norma, LPSTR textoLeyenda);
   HB_FUNC(EASYONE_LEYENDASFISC10_ADDLEYENDA)
   {
      (((__EasyOne_leyendasFisc10_AddLeyenda)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_leyendasFisc10_AddLeyenda"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef LPSTR(__cdecl * __EasyOne_LeeResultado)(long p, long valor);
   HB_FUNC(EASYONE_LEERESULTADO)
   {
      hb_retc(((__EasyOne_LeeResultado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_LeeResultado"))(hb_parnl(1), hb_parnl(2)));
   }

   typedef LPSTR(__cdecl * __EasyOne_IsValidRfc)(LPSTR rfc);
   HB_FUNC(EASYONE_ISVALIDRFC)
   {
      hb_retc(((__EasyOne_IsValidRfc)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_IsValidRfc"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_intereseshipotecarios10_SetIntereseshipotecarios)(long p, LPSTR CreditoDeInstFinanc, LPSTR SaldoInsoluto, LPSTR PropDeducDelCredit, LPSTR MontTotIntNominalesDev, LPSTR MontTotIntNominalesDevYPag, LPSTR MontTotIntRealPagDeduc, LPSTR NumContrato);
   HB_FUNC(EASYONE_INTERESESHIPOTECARIOS10_SETINTERESESHIPOTECARIOS)
   {
      (((__EasyOne_intereseshipotecarios10_SetIntereseshipotecarios)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_intereseshipotecarios10_SetIntereseshipotecarios"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_intereseshipotecarios10_Free)(long p);
   HB_FUNC(EASYONE_INTERESESHIPOTECARIOS10_FREE)
   {
      (((__EasyOne_intereseshipotecarios10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_intereseshipotecarios10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_intereseshipotecarios10_Create)(void);
   HB_FUNC(EASYONE_INTERESESHIPOTECARIOS10_CREATE)
   {
      hb_retnl(((__EasyOne_intereseshipotecarios10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_intereseshipotecarios10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_intereses10_SetIntereses)(long p, LPSTR SistFinanciero, LPSTR RetiroAORESRetInt, LPSTR OperFinancDerivad, LPSTR MontIntNominal, LPSTR MontIntReal, LPSTR Perdida);
   HB_FUNC(EASYONE_INTERESES10_SETINTERESES)
   {
      (((__EasyOne_intereses10_SetIntereses)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_intereses10_SetIntereses"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_intereses10_Free)(long p);
   HB_FUNC(EASYONE_INTERESES10_FREE)
   {
      (((__EasyOne_intereses10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_intereses10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_intereses10_Create)(void);
   HB_FUNC(EASYONE_INTERESES10_CREATE)
   {
      hb_retnl(((__EasyOne_intereses10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_intereses10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ine11_SetINE)(long p, LPSTR TipoProceso, LPSTR TipoComite, LPSTR IdContabilidad);
   HB_FUNC(EASYONE_INE11_SETINE)
   {
      (((__EasyOne_ine11_SetINE)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ine11_SetINE"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_ine11_Free)(long p);
   HB_FUNC(EASYONE_INE11_FREE)
   {
      (((__EasyOne_ine11_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ine11_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_ine11_Create)(void);
   HB_FUNC(EASYONE_INE11_CREATE)
   {
      hb_retnl(((__EasyOne_ine11_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ine11_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ine11_AddEntidad)(long p, LPSTR ClaveEntidad, LPSTR Ambito);
   HB_FUNC(EASYONE_INE11_ADDENTIDAD)
   {
      (((__EasyOne_ine11_AddEntidad)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ine11_AddEntidad"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_ine11_AddContabilidad)(long p, LPSTR IdContabilidad);
   HB_FUNC(EASYONE_INE11_ADDCONTABILIDAD)
   {
      (((__EasyOne_ine11_AddContabilidad)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ine11_AddContabilidad"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_implocal10_SetImpuestosLocales)(long p, LPSTR TotaldeRetenciones, LPSTR TotaldeTraslados);
   HB_FUNC(EASYONE_IMPLOCAL10_SETIMPUESTOSLOCALES)
   {
      (((__EasyOne_implocal10_SetImpuestosLocales)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_implocal10_SetImpuestosLocales"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_implocal10_Free)(long p);
   HB_FUNC(EASYONE_IMPLOCAL10_FREE)
   {
      (((__EasyOne_implocal10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_implocal10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_implocal10_Create)(void);
   HB_FUNC(EASYONE_IMPLOCAL10_CREATE)
   {
      hb_retnl(((__EasyOne_implocal10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_implocal10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_implocal10_AddTrasladosLocales)(long p, LPSTR ImpLocTrasladado, LPSTR TasadeTraslado, LPSTR Importe);
   HB_FUNC(EASYONE_IMPLOCAL10_ADDTRASLADOSLOCALES)
   {
      (((__EasyOne_implocal10_AddTrasladosLocales)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_implocal10_AddTrasladosLocales"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_implocal10_AddRetencionesLocales)(long p, LPSTR ImpLocRetenido, LPSTR TasadeRetencion, LPSTR Importe);
   HB_FUNC(EASYONE_IMPLOCAL10_ADDRETENCIONESLOCALES)
   {
      (((__EasyOne_implocal10_AddRetencionesLocales)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_implocal10_AddRetencionesLocales"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_iedu10_SetinstEducativas)(long p, LPSTR nombreAlumno, LPSTR CURP, LPSTR nivelEducativo, LPSTR autRVOE, LPSTR rfcPago);
   HB_FUNC(EASYONE_IEDU10_SETINSTEDUCATIVAS)
   {
      (((__EasyOne_iedu10_SetinstEducativas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_iedu10_SetinstEducativas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_iedu10_Free)(long p);
   HB_FUNC(EASYONE_IEDU10_FREE)
   {
      (((__EasyOne_iedu10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_iedu10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_iedu10_Create)(void);
   HB_FUNC(EASYONE_IEDU10_CREATE)
   {
      hb_retnl(((__EasyOne_iedu10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_iedu10_Create"))());
   }

   typedef LPSTR(__cdecl * __EasyOne_GetVersionValue)(LPSTR value);
   HB_FUNC(EASYONE_GETVERSIONVALUE)
   {
      hb_retc(((__EasyOne_GetVersionValue)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_GetVersionValue"))((LPSTR) hb_parc(1)));
   }

   typedef long(__cdecl * __EasyOne_FirmaComprobante)(long p, LPSTR csd, LPSTR key, LPSTR pwd);
   HB_FUNC(EASYONE_FIRMACOMPROBANTE)
   {
      hb_retnl(((__EasyOne_FirmaComprobante)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_FirmaComprobante"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_fideicomisonoempresarial10_SetRetEfectFideicomiso)(long p, LPSTR MontRetRelPagFideic, LPSTR DescRetRelPagFideic);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_SETRETEFECTFIDEICOMISO)
   {
      (((__EasyOne_fideicomisonoempresarial10_SetRetEfectFideicomiso)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_SetRetEfectFideicomiso"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_fideicomisonoempresarial10_SetIntegracIngresos)(long p, LPSTR Concepto);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_SETINTEGRACINGRESOS)
   {
      (((__EasyOne_fideicomisonoempresarial10_SetIntegracIngresos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_SetIntegracIngresos"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_fideicomisonoempresarial10_SetIntegracEgresos)(long p, LPSTR ConceptoS);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_SETINTEGRACEGRESOS)
   {
      (((__EasyOne_fideicomisonoempresarial10_SetIntegracEgresos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_SetIntegracEgresos"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_fideicomisonoempresarial10_SetIngresosOEntradas)(long p, LPSTR MontTotEntradasPeriodo, LPSTR PartPropAcumDelFideicom, LPSTR PropDelMontTot);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_SETINGRESOSOENTRADAS)
   {
      (((__EasyOne_fideicomisonoempresarial10_SetIngresosOEntradas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_SetIngresosOEntradas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_fideicomisonoempresarial10_SetDeduccOSalidas)(long p, LPSTR MontTotEgresPeriodo, LPSTR PartPropDelFideicom, LPSTR PropDelMontTot);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_SETDEDUCCOSALIDAS)
   {
      (((__EasyOne_fideicomisonoempresarial10_SetDeduccOSalidas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_SetDeduccOSalidas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_fideicomisonoempresarial10_Free)(long p);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_FREE)
   {
      (((__EasyOne_fideicomisonoempresarial10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_fideicomisonoempresarial10_Create)(void);
   HB_FUNC(EASYONE_FIDEICOMISONOEMPRESARIAL10_CREATE)
   {
      hb_retnl(((__EasyOne_fideicomisonoempresarial10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_fideicomisonoempresarial10_Create"))());
   }

   typedef long(__cdecl * __EasyOne_Execute)(LPSTR CmdFile, LPSTR CmdParams);
   HB_FUNC(EASYONE_EXECUTE)
   {
      hb_retnl(((__EasyOne_Execute)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Execute"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_EscribeResultados)(long p, LPSTR OurFile, long Format);
   HB_FUNC(EASYONE_ESCRIBERESULTADOS)
   {
      (((__EasyOne_EscribeResultados)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_EscribeResultados"))(hb_parnl(1), (LPSTR) hb_parc(2), hb_parnl(3)));
   }

   typedef long(__cdecl * __EasyOne_EscribeResultado)(long p, LPSTR file, long resultado);
   HB_FUNC(EASYONE_ESCRIBERESULTADO)
   {
      hb_retnl(((__EasyOne_EscribeResultado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_EscribeResultado"))(hb_parnl(1), (LPSTR) hb_parc(2), hb_parnl(3)));
   }

   typedef void(__cdecl * __EasyOne_EnviarAddenda)(long p, LPSTR enviar);
   HB_FUNC(EASYONE_ENVIARADDENDA)
   {
      (((__EasyOne_EnviarAddenda)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_EnviarAddenda"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_enajenaciondeacciones10_SetEnajenaciondeAcciones)(long p, LPSTR ContratoIntermediacion, LPSTR Ganancia, LPSTR Perdida);
   HB_FUNC(EASYONE_ENAJENACIONDEACCIONES10_SETENAJENACIONDEACCIONES)
   {
      (((__EasyOne_enajenaciondeacciones10_SetEnajenaciondeAcciones)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_enajenaciondeacciones10_SetEnajenaciondeAcciones"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_enajenaciondeacciones10_Free)(long p);
   HB_FUNC(EASYONE_ENAJENACIONDEACCIONES10_FREE)
   {
      (((__EasyOne_enajenaciondeacciones10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_enajenaciondeacciones10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_enajenaciondeacciones10_Create)(void);
   HB_FUNC(EASYONE_ENAJENACIONDEACCIONES10_CREATE)
   {
      hb_retnl(((__EasyOne_enajenaciondeacciones10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_enajenaciondeacciones10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ecc11_SetEstadoDeCuentaCombustible)(long p, LPSTR NumeroDeCuenta, LPSTR SubTotal, LPSTR Total);
   HB_FUNC(EASYONE_ECC11_SETESTADODECUENTACOMBUSTIBLE)
   {
      (((__EasyOne_ecc11_SetEstadoDeCuentaCombustible)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ecc11_SetEstadoDeCuentaCombustible"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_ecc11_Free)(long p);
   HB_FUNC(EASYONE_ECC11_FREE)
   {
      (((__EasyOne_ecc11_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ecc11_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_ecc11_Create)(void);
   HB_FUNC(EASYONE_ECC11_CREATE)
   {
      hb_retnl(((__EasyOne_ecc11_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ecc11_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ecc11_AddTraslado)(long p, LPSTR Impuesto, LPSTR TasaoCuota, LPSTR Importe);
   HB_FUNC(EASYONE_ECC11_ADDTRASLADO)
   {
      (((__EasyOne_ecc11_AddTraslado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ecc11_AddTraslado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_ecc11_AddConceptoEstadoDeCuentaCombustible)(long p, LPSTR Identificador, LPSTR Fecha, LPSTR Rfc, LPSTR ClaveEstacion, LPSTR TAR, LPSTR Cantidad, LPSTR NoIdentificacion, LPSTR Unidad, LPSTR NombreCombustible, LPSTR FolioOperacion, LPSTR ValorUnitario, LPSTR Importe);
   HB_FUNC(EASYONE_ECC11_ADDCONCEPTOESTADODECUENTACOMBUSTIBLE)
   {
      (((__EasyOne_ecc11_AddConceptoEstadoDeCuentaCombustible)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ecc11_AddConceptoEstadoDeCuentaCombustible"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13)));
   }

   typedef void(__cdecl * __EasyOne_donat11_SetDonatarias)(long p, LPSTR noAutorizacion, LPSTR fechaAutorizacion, LPSTR leyenda);
   HB_FUNC(EASYONE_DONAT11_SETDONATARIAS)
   {
      (((__EasyOne_donat11_SetDonatarias)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_donat11_SetDonatarias"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_donat11_Free)(long p);
   HB_FUNC(EASYONE_DONAT11_FREE)
   {
      (((__EasyOne_donat11_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_donat11_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_donat11_Create)(void);
   HB_FUNC(EASYONE_DONAT11_CREATE)
   {
      hb_retnl(((__EasyOne_donat11_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_donat11_Create"))());
   }

   typedef void(__cdecl * __EasyOne_divisas10_SetDivisas)(long p, LPSTR tipoOperacion);
   HB_FUNC(EASYONE_DIVISAS10_SETDIVISAS)
   {
      (((__EasyOne_divisas10_SetDivisas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_divisas10_SetDivisas"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_divisas10_Free)(long p);
   HB_FUNC(EASYONE_DIVISAS10_FREE)
   {
      (((__EasyOne_divisas10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_divisas10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_divisas10_Create)(void);
   HB_FUNC(EASYONE_DIVISAS10_CREATE)
   {
      hb_retnl(((__EasyOne_divisas10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_divisas10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_dividendos10_SetRemanente)(long p, LPSTR ProporcionRem);
   HB_FUNC(EASYONE_DIVIDENDOS10_SETREMANENTE)
   {
      (((__EasyOne_dividendos10_SetRemanente)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_dividendos10_SetRemanente"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_dividendos10_SetDividOUtil)(long p, LPSTR CveTipDivOUtil, LPSTR MontISRAcredRetMexico, LPSTR MontISRAcredRetExtranjero, LPSTR MontRetExtDivExt, LPSTR TipoSocDistrDiv, LPSTR MontISRAcredNal, LPSTR MontDivAcumNal, LPSTR MontDivAcumExt);
   HB_FUNC(EASYONE_DIVIDENDOS10_SETDIVIDOUTIL)
   {
      (((__EasyOne_dividendos10_SetDividOUtil)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_dividendos10_SetDividOUtil"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9)));
   }

   typedef void(__cdecl * __EasyOne_dividendos10_Free)(long p);
   HB_FUNC(EASYONE_DIVIDENDOS10_FREE)
   {
      (((__EasyOne_dividendos10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_dividendos10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_dividendos10_Create)(void);
   HB_FUNC(EASYONE_DIVIDENDOS10_CREATE)
   {
      hb_retnl(((__EasyOne_dividendos10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_dividendos10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_totalAmount)(long p, LPSTR Amount);
   HB_FUNC(EASYONE_DETALLISTA_SET_TOTALAMOUNT)
   {
      (((__EasyOne_detallista_Set_totalAmount)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_totalAmount"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_shipTo)(long p, LPSTR gln);
   HB_FUNC(EASYONE_DETALLISTA_SET_SHIPTO)
   {
      (((__EasyOne_detallista_Set_shipTo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_shipTo"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_seller)(long p, LPSTR gln, LPSTR alternatePartyIdentification, LPSTR type);
   HB_FUNC(EASYONE_DETALLISTA_SET_SELLER)
   {
      (((__EasyOne_detallista_Set_seller)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_seller"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_requestForPaymentIdentification_entityType)(long p, LPSTR entityType);
   HB_FUNC(EASYONE_DETALLISTA_SET_REQUESTFORPAYMENTIDENTIFICATION_ENTITYTYPE)
   {
      (((__EasyOne_detallista_Set_requestForPaymentIdentification_entityType)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_requestForPaymentIdentification_entityType"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_paymentTerms)(long p, LPSTR Event, LPSTR RelationTime, LPSTR netPaymentTermsType, LPSTR paymentTimePeriod, LPSTR paymentTimePeriodValue, LPSTR discountType, LPSTR discountPercentage);
   HB_FUNC(EASYONE_DETALLISTA_SET_PAYMENTTERMS)
   {
      (((__EasyOne_detallista_Set_paymentTerms)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_paymentTerms"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_orderIdentification_ReferenceDate)(long p, LPSTR ReferenceDate);
   HB_FUNC(EASYONE_DETALLISTA_SET_ORDERIDENTIFICATION_REFERENCEDATE)
   {
      (((__EasyOne_detallista_Set_orderIdentification_ReferenceDate)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_orderIdentification_ReferenceDate"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_nameAndAddress)(long p, LPSTR name, LPSTR streetAddressOne, LPSTR city, LPSTR postalCode);
   HB_FUNC(EASYONE_DETALLISTA_SET_NAMEANDADDRESS)
   {
      (((__EasyOne_detallista_Set_nameAndAddress)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_nameAndAddress"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_lineItem_tradeItemIdentification)(long p, LPSTR gtin);
   HB_FUNC(EASYONE_DETALLISTA_SET_LINEITEM_TRADEITEMIDENTIFICATION)
   {
      (((__EasyOne_detallista_Set_lineItem_tradeItemIdentification)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_lineItem_tradeItemIdentification"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_lineItem_tradeItemDescriptionInformation)(long p, LPSTR language, LPSTR longText);
   HB_FUNC(EASYONE_DETALLISTA_SET_LINEITEM_TRADEITEMDESCRIPTIONINFORMATION)
   {
      (((__EasyOne_detallista_Set_lineItem_tradeItemDescriptionInformation)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_lineItem_tradeItemDescriptionInformation"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_lineItem_totalLineAmount)(long p, LPSTR grossAmount, LPSTR netAmount);
   HB_FUNC(EASYONE_DETALLISTA_SET_LINEITEM_TOTALLINEAMOUNT)
   {
      (((__EasyOne_detallista_Set_lineItem_totalLineAmount)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_lineItem_totalLineAmount"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_lineItem_netPrice)(long p, LPSTR Amount);
   HB_FUNC(EASYONE_DETALLISTA_SET_LINEITEM_NETPRICE)
   {
      (((__EasyOne_detallista_Set_lineItem_netPrice)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_lineItem_netPrice"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_lineItem_invoicedQuantity)(long p, LPSTR invoicedQuantity, LPSTR unitOfMeasure);
   HB_FUNC(EASYONE_DETALLISTA_SET_LINEITEM_INVOICEDQUANTITY)
   {
      (((__EasyOne_detallista_Set_lineItem_invoicedQuantity)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_lineItem_invoicedQuantity"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_lineItem_grossPrice)(long p, LPSTR Amount);
   HB_FUNC(EASYONE_DETALLISTA_SET_LINEITEM_GROSSPRICE)
   {
      (((__EasyOne_detallista_Set_lineItem_grossPrice)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_lineItem_grossPrice"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_InvoiceCreator)(long p, LPSTR gln, LPSTR alternatePartyIdentification, LPSTR name, LPSTR streetAddressOne, LPSTR city, LPSTR postalCode);
   HB_FUNC(EASYONE_DETALLISTA_SET_INVOICECREATOR)
   {
      (((__EasyOne_detallista_Set_InvoiceCreator)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_InvoiceCreator"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_detallista)(long p, LPSTR documentStatus);
   HB_FUNC(EASYONE_DETALLISTA_SET_DETALLISTA)
   {
      (((__EasyOne_detallista_Set_detallista)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_detallista"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_DeliveryNote_ReferenceDate)(long p, LPSTR ReferenceDate);
   HB_FUNC(EASYONE_DETALLISTA_SET_DELIVERYNOTE_REFERENCEDATE)
   {
      (((__EasyOne_detallista_Set_DeliveryNote_ReferenceDate)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_DeliveryNote_ReferenceDate"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Set_buyer)(long p, LPSTR gln, LPSTR personOrDepartmentName);
   HB_FUNC(EASYONE_DETALLISTA_SET_BUYER)
   {
      (((__EasyOne_detallista_Set_buyer)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Set_buyer"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Free)(long p);
   HB_FUNC(EASYONE_DETALLISTA_FREE)
   {
      (((__EasyOne_detallista_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_detallista_Create)(void);
   HB_FUNC(EASYONE_DETALLISTA_CREATE)
   {
      hb_retnl(((__EasyOne_detallista_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Create"))());
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_TotalAllowanceCharge)(long p, LPSTR allowanceOrChargeType, LPSTR specialServicesType, LPSTR Amount);
   HB_FUNC(EASYONE_DETALLISTA_ADD_TOTALALLOWANCECHARGE)
   {
      (((__EasyOne_detallista_Add_TotalAllowanceCharge)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_TotalAllowanceCharge"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_specialInstruction_text)(long p, LPSTR text);
   HB_FUNC(EASYONE_DETALLISTA_ADD_SPECIALINSTRUCTION_TEXT)
   {
      (((__EasyOne_detallista_Add_specialInstruction_text)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_specialInstruction_text"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_specialInstruction)(long p, LPSTR code);
   HB_FUNC(EASYONE_DETALLISTA_ADD_SPECIALINSTRUCTION)
   {
      (((__EasyOne_detallista_Add_specialInstruction)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_specialInstruction"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_orderIdentification_referenceIdentification)(long p, LPSTR type, LPSTR referenceIdentification);
   HB_FUNC(EASYONE_DETALLISTA_ADD_ORDERIDENTIFICATION_REFERENCEIDENTIFICATION)
   {
      (((__EasyOne_detallista_Add_orderIdentification_referenceIdentification)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_orderIdentification_referenceIdentification"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_orderIdentification)(long p, LPSTR none);
   HB_FUNC(EASYONE_DETALLISTA_ADD_ORDERIDENTIFICATION)
   {
      (((__EasyOne_detallista_Add_orderIdentification)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_orderIdentification"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_lineItem_alternateTradeItemIdentification)(long p, LPSTR type, LPSTR alternateTradeItemIdentification);
   HB_FUNC(EASYONE_DETALLISTA_ADD_LINEITEM_ALTERNATETRADEITEMIDENTIFICATION)
   {
      (((__EasyOne_detallista_Add_lineItem_alternateTradeItemIdentification)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_lineItem_alternateTradeItemIdentification"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_lineItem)(long p, LPSTR type, LPSTR number);
   HB_FUNC(EASYONE_DETALLISTA_ADD_LINEITEM)
   {
      (((__EasyOne_detallista_Add_lineItem)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_lineItem"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_DeliveryNote_referenceIdentification)(long p, LPSTR referenceIdentification);
   HB_FUNC(EASYONE_DETALLISTA_ADD_DELIVERYNOTE_REFERENCEIDENTIFICATION)
   {
      (((__EasyOne_detallista_Add_DeliveryNote_referenceIdentification)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_DeliveryNote_referenceIdentification"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_DeliveryNote)(long p, LPSTR none);
   HB_FUNC(EASYONE_DETALLISTA_ADD_DELIVERYNOTE)
   {
      (((__EasyOne_detallista_Add_DeliveryNote)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_DeliveryNote"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_Customs)(long p, LPSTR gln);
   HB_FUNC(EASYONE_DETALLISTA_ADD_CUSTOMS)
   {
      (((__EasyOne_detallista_Add_Customs)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_Customs"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_currency)(long p, LPSTR ISOCode, LPSTR Function1, LPSTR Function2, LPSTR Function3, LPSTR rateOfChange);
   HB_FUNC(EASYONE_DETALLISTA_ADD_CURRENCY)
   {
      (((__EasyOne_detallista_Add_currency)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_currency"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_allowanceCharge)(long p, LPSTR allowanceChargeType, LPSTR settlementType, LPSTR sequenceNumber, LPSTR specialServicesType, LPSTR base, LPSTR percentage);
   HB_FUNC(EASYONE_DETALLISTA_ADD_ALLOWANCECHARGE)
   {
      (((__EasyOne_detallista_Add_allowanceCharge)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_allowanceCharge"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_AdditionalInformation_referenceIdentification)(long p, LPSTR type, LPSTR referenceIdentification);
   HB_FUNC(EASYONE_DETALLISTA_ADD_ADDITIONALINFORMATION_REFERENCEIDENTIFICATION)
   {
      (((__EasyOne_detallista_Add_AdditionalInformation_referenceIdentification)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_AdditionalInformation_referenceIdentification"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_detallista_Add_AdditionalInformation)(long p, LPSTR none);
   HB_FUNC(EASYONE_DETALLISTA_ADD_ADDITIONALINFORMATION)
   {
      (((__EasyOne_detallista_Add_AdditionalInformation)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_detallista_Add_AdditionalInformation"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_destruccion10_SetVehiculoDestruido)(long p, LPSTR Marca, LPSTR TipooClase, LPSTR Anio, LPSTR Modelo, LPSTR NIV, LPSTR NumSerie, LPSTR NumPlacas, LPSTR NumMotor, LPSTR NumFolTarjCir);
   HB_FUNC(EASYONE_DESTRUCCION10_SETVEHICULODESTRUIDO)
   {
      (((__EasyOne_destruccion10_SetVehiculoDestruido)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_destruccion10_SetVehiculoDestruido"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10)));
   }

   typedef void(__cdecl * __EasyOne_destruccion10_SetInformacionAduanera)(long p, LPSTR NumPedImp, LPSTR Fecha, LPSTR Aduana);
   HB_FUNC(EASYONE_DESTRUCCION10_SETINFORMACIONADUANERA)
   {
      (((__EasyOne_destruccion10_SetInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_destruccion10_SetInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_destruccion10_Setcertificadodedestruccion)(long p, LPSTR Serie, LPSTR NumFolDesVeh);
   HB_FUNC(EASYONE_DESTRUCCION10_SETCERTIFICADODEDESTRUCCION)
   {
      (((__EasyOne_destruccion10_Setcertificadodedestruccion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_destruccion10_Setcertificadodedestruccion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_destruccion10_Free)(long p);
   HB_FUNC(EASYONE_DESTRUCCION10_FREE)
   {
      (((__EasyOne_destruccion10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_destruccion10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_destruccion10_Create)(void);
   HB_FUNC(EASYONE_DESTRUCCION10_CREATE)
   {
      hb_retnl(((__EasyOne_destruccion10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_destruccion10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_decreto10_SetVehiculoUsadoEnajenadoPermAlFab)(long p, LPSTR PrecioVehUsado, LPSTR TipoVeh, LPSTR Marca, LPSTR TipooClase, LPSTR Anio, LPSTR Modelo, LPSTR NIV, LPSTR NumSerie, LPSTR NumPlacas, LPSTR NumMotor, LPSTR NumFolTarjCir, LPSTR NumFolAvisoint, LPSTR NumPedIm, LPSTR Aduana, LPSTR FechaRegulVeh, LPSTR Foliofiscal);
   HB_FUNC(EASYONE_DECRETO10_SETVEHICULOUSADOENAJENADOPERMALFAB)
   {
      (((__EasyOne_decreto10_SetVehiculoUsadoEnajenadoPermAlFab)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_SetVehiculoUsadoEnajenadoPermAlFab"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13), (LPSTR) hb_parc(14), (LPSTR) hb_parc(15), (LPSTR) hb_parc(16), (LPSTR) hb_parc(17)));
   }

   typedef void(__cdecl * __EasyOne_decreto10_SetVehiculoNuvoSemEnajenadoFabAlPerm)(long p, LPSTR Anio, LPSTR Modelo, LPSTR NumPlacas, LPSTR RFC);
   HB_FUNC(EASYONE_DECRETO10_SETVEHICULONUVOSEMENAJENADOFABALPERM)
   {
      (((__EasyOne_decreto10_SetVehiculoNuvoSemEnajenadoFabAlPerm)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_SetVehiculoNuvoSemEnajenadoFabAlPerm"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_decreto10_Setrenovacionysustitucionvehiculos)(long p, LPSTR TipoDeDecreto);
   HB_FUNC(EASYONE_DECRETO10_SETRENOVACIONYSUSTITUCIONVEHICULOS)
   {
      (((__EasyOne_decreto10_Setrenovacionysustitucionvehiculos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_Setrenovacionysustitucionvehiculos"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_decreto10_SetDecretoSustitVehicularVehiculoNuvoSemEnajenadoFabAlPerm)(long p, LPSTR Anio, LPSTR Modelo, LPSTR NumPlacas, LPSTR RFC);
   HB_FUNC(EASYONE_DECRETO10_SETDECRETOSUSTITVEHICULARVEHICULONUVOSEMENAJENADOFABALPERM)
   {
      (((__EasyOne_decreto10_SetDecretoSustitVehicularVehiculoNuvoSemEnajenadoFabAlPerm)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_SetDecretoSustitVehicularVehiculoNuvoSemEnajenadoFabAlPerm"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_decreto10_SetDecretoSustitVehicular)(long p, LPSTR VehEnaj);
   HB_FUNC(EASYONE_DECRETO10_SETDECRETOSUSTITVEHICULAR)
   {
      (((__EasyOne_decreto10_SetDecretoSustitVehicular)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_SetDecretoSustitVehicular"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_decreto10_SetDecretoRenovVehicular)(long p, LPSTR VehEnaj);
   HB_FUNC(EASYONE_DECRETO10_SETDECRETORENOVVEHICULAR)
   {
      (((__EasyOne_decreto10_SetDecretoRenovVehicular)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_SetDecretoRenovVehicular"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_decreto10_Free)(long p);
   HB_FUNC(EASYONE_DECRETO10_FREE)
   {
      (((__EasyOne_decreto10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_decreto10_Create)(void);
   HB_FUNC(EASYONE_DECRETO10_CREATE)
   {
      hb_retnl(((__EasyOne_decreto10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_decreto10_AddVehiculosUsadosEnajenadoPermAlFab)(long p, LPSTR PrecioVehUsado, LPSTR TipoVeh, LPSTR Marca, LPSTR TipooClase, LPSTR Anio, LPSTR Modelo, LPSTR NIV, LPSTR NumSerie, LPSTR NumPlacas, LPSTR NumMotor, LPSTR NumFolTarjCir, LPSTR NumPedIm, LPSTR Aduana, LPSTR FechaRegulVeh, LPSTR Foliofiscal);
   HB_FUNC(EASYONE_DECRETO10_ADDVEHICULOSUSADOSENAJENADOPERMALFAB)
   {
      (((__EasyOne_decreto10_AddVehiculosUsadosEnajenadoPermAlFab)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_decreto10_AddVehiculosUsadosEnajenadoPermAlFab"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13), (LPSTR) hb_parc(14), (LPSTR) hb_parc(15), (LPSTR) hb_parc(16)));
   }

   typedef void(__cdecl * __EasyOne_Crypto_LoadCsd)(long p, LPSTR csd, LPSTR key, LPSTR pwd);
   HB_FUNC(EASYONE_CRYPTO_LOADCSD)
   {
      (((__EasyOne_Crypto_LoadCsd)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Crypto_LoadCsd"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_Crypto_GetUTCTime)(long p, LPSTR delay);
   HB_FUNC(EASYONE_CRYPTO_GETUTCTIME)
   {
      (((__EasyOne_Crypto_GetUTCTime)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Crypto_GetUTCTime"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_Crypto_GeneraPeticion)(long p, LPSTR CreatedToken, LPSTR ExpiredToken, LPSTR csd, LPSTR key, LPSTR pwd);
   HB_FUNC(EASYONE_CRYPTO_GENERAPETICION)
   {
      (((__EasyOne_Crypto_GeneraPeticion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Crypto_GeneraPeticion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_Crypto_Free)(long p);
   HB_FUNC(EASYONE_CRYPTO_FREE)
   {
      (((__EasyOne_Crypto_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Crypto_Free"))(hb_parnl(1)));
   }

   typedef void(__cdecl * __EasyOne_Crypto_Export2Pfx)(long p, LPSTR file, LPSTR pwdPfx);
   HB_FUNC(EASYONE_CRYPTO_EXPORT2PFX)
   {
      (((__EasyOne_Crypto_Export2Pfx)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Crypto_Export2Pfx"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef long(__cdecl * __EasyOne_Crypto_Create)(void);
   HB_FUNC(EASYONE_CRYPTO_CREATE)
   {
      hb_retnl(((__EasyOne_Crypto_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Crypto_Create"))());
   }

   typedef void(__cdecl * __EasyOne_ConsultaCFDI_ObtenerEstado)(long p, LPSTR rfcEmisor, LPSTR rfcReceptor, LPSTR total6Dec, LPSTR uuid);
   HB_FUNC(EASYONE_CONSULTACFDI_OBTENERESTADO)
   {
      (((__EasyOne_ConsultaCFDI_ObtenerEstado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ConsultaCFDI_ObtenerEstado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_ConsultaCFDI_Free)(long p);
   HB_FUNC(EASYONE_CONSULTACFDI_FREE)
   {
      (((__EasyOne_ConsultaCFDI_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ConsultaCFDI_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_ConsultaCFDI_Create)(void);
   HB_FUNC(EASYONE_CONSULTACFDI_CREATE)
   {
      hb_retnl(((__EasyOne_ConsultaCFDI_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_ConsultaCFDI_Create"))());
   }

   typedef void(__cdecl * __EasyOne_Config_SetPreFirmaScript)(long p, LPSTR ScriptFile);
   HB_FUNC(EASYONE_CONFIG_SETPREFIRMASCRIPT)
   {
      (((__EasyOne_Config_SetPreFirmaScript)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_SetPreFirmaScript"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_Config_SetOutputFileOnOk)(long p, LPSTR DynamicFilePath);
   HB_FUNC(EASYONE_CONFIG_SETOUTPUTFILEONOK)
   {
      (((__EasyOne_Config_SetOutputFileOnOk)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_SetOutputFileOnOk"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_Config_SetLogDirectory)(long p, LPSTR LogDirectory);
   HB_FUNC(EASYONE_CONFIG_SETLOGDIRECTORY)
   {
      (((__EasyOne_Config_SetLogDirectory)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_SetLogDirectory"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_Config_SetCancelaReintents)(long p, LPSTR NumReintents, LPSTR IntervalTime);
   HB_FUNC(EASYONE_CONFIG_SETCANCELAREINTENTS)
   {
      (((__EasyOne_Config_SetCancelaReintents)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_SetCancelaReintents"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_Config_SetAgroupNoIdentificacionConcepts)(long p, LPSTR TrueOrFalse);
   HB_FUNC(EASYONE_CONFIG_SETAGROUPNOIDENTIFICACIONCONCEPTS)
   {
      (((__EasyOne_Config_SetAgroupNoIdentificacionConcepts)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_SetAgroupNoIdentificacionConcepts"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_Config_Free)(long p);
   HB_FUNC(EASYONE_CONFIG_FREE)
   {
      (((__EasyOne_Config_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_Config_Create)(void);
   HB_FUNC(EASYONE_CONFIG_CREATE)
   {
      hb_retnl(((__EasyOne_Config_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_Config_Create"))());
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetReceptor)(long p, LPSTR Rfc, LPSTR Nombre, LPSTR ResidenciaFiscal, LPSTR NumRegIdTrib, LPSTR UsoCFDI);
   HB_FUNC(EASYONE_CFDI33_SETRECEPTOR)
   {
      (((__EasyOne_cfdi33_SetReceptor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetReceptor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetFileNames)(long p, LPSTR FileOk, LPSTR FileError, LPSTR fopFile, LPSTR fopOutFile);
   HB_FUNC(EASYONE_CFDI33_SETFILENAMES)
   {
      (((__EasyOne_cfdi33_SetFileNames)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetFileNames"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetEmisor)(long p, LPSTR Rfc, LPSTR Nombre, LPSTR RegimenFiscal);
   HB_FUNC(EASYONE_CFDI33_SETEMISOR)
   {
      (((__EasyOne_cfdi33_SetEmisor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetEmisor"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetCuentaPredial)(long p, LPSTR Numero);
   HB_FUNC(EASYONE_CFDI33_SETCUENTAPREDIAL)
   {
      (((__EasyOne_cfdi33_SetCuentaPredial)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetCuentaPredial"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetComprobanteImpuestos)(long p, LPSTR TotalImpuestosRetenidos, LPSTR TotalImpuestosTrasladados);
   HB_FUNC(EASYONE_CFDI33_SETCOMPROBANTEIMPUESTOS)
   {
      (((__EasyOne_cfdi33_SetComprobanteImpuestos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetComprobanteImpuestos"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetComprobanteFecha)(long p, LPSTR Fecha);
   HB_FUNC(EASYONE_CFDI33_SETCOMPROBANTEFECHA)
   {
      (((__EasyOne_cfdi33_SetComprobanteFecha)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetComprobanteFecha"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetComprobante)(long p, LPSTR Serie, LPSTR Folio, LPSTR Fecha, LPSTR FormaPago, LPSTR CondicionesDePago, LPSTR SubTotal, LPSTR Descuento, LPSTR Moneda, LPSTR TipoCambio, LPSTR Total, LPSTR TipoDeComprobante, LPSTR MetodoPago, LPSTR LugarExpedicion, LPSTR Confirmacion);
   HB_FUNC(EASYONE_CFDI33_SETCOMPROBANTE)
   {
      (((__EasyOne_cfdi33_SetComprobante)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetComprobante"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13), (LPSTR) hb_parc(14), (LPSTR) hb_parc(15)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetCfdiRelacionados)(long p, LPSTR TipoRelacion);
   HB_FUNC(EASYONE_CFDI33_SETCFDIRELACIONADOS)
   {
      (((__EasyOne_cfdi33_SetCfdiRelacionados)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetCfdiRelacionados"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetAzureFileAccount)(long p, LPSTR StorageAccount, LPSTR BlobContainer, LPSTR FilePathOk, LPSTR filePathError, LPSTR SASToken);
   HB_FUNC(EASYONE_CFDI33_SETAZUREFILEACCOUNT)
   {
      (((__EasyOne_cfdi33_SetAzureFileAccount)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetAzureFileAccount"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SetAzureBlobAccount)(long p, LPSTR StorageAccount, LPSTR BlobContainer, LPSTR FilePathOk, LPSTR filePathError, LPSTR SASToken);
   HB_FUNC(EASYONE_CFDI33_SETAZUREBLOBACCOUNT)
   {
      (((__EasyOne_cfdi33_SetAzureBlobAccount)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SetAzureBlobAccount"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_SendMail)(long p, LPSTR host, LPSTR port, LPSTR username, LPSTR pwd, LPSTR fromMail, LPSTR ToMail, LPSTR subject, LPSTR BodyMsg);
   HB_FUNC(EASYONE_CFDI33_SENDMAIL)
   {
      (((__EasyOne_cfdi33_SendMail)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_SendMail"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_LoadXmlTransform)(long p, LPSTR XmlFile, LPSTR XslFile);
   HB_FUNC(EASYONE_CFDI33_LOADXMLTRANSFORM)
   {
      (((__EasyOne_cfdi33_LoadXmlTransform)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_LoadXmlTransform"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_LoadTxtPipes)(long p, LPSTR File, LPSTR XslFile);
   HB_FUNC(EASYONE_CFDI33_LOADTXTPIPES)
   {
      (((__EasyOne_cfdi33_LoadTxtPipes)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_LoadTxtPipes"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_Free)(long p);
   HB_FUNC(EASYONE_CFDI33_FREE)
   {
      (((__EasyOne_cfdi33_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_cfdi33_Create)(LPSTR OutFile);
   HB_FUNC(EASYONE_CFDI33_CREATE)
   {
      hb_retnl(((__EasyOne_cfdi33_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_Create"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddTrasladosTraslado)(long p, LPSTR Impuesto, LPSTR TipoFactor, LPSTR TasaOCuota, LPSTR Importe);
   HB_FUNC(EASYONE_CFDI33_ADDTRASLADOSTRASLADO)
   {
      (((__EasyOne_cfdi33_AddTrasladosTraslado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddTrasladosTraslado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddTraslado)(long p, LPSTR Base, LPSTR Impuesto, LPSTR TipoFactor, LPSTR TasaOCuota, LPSTR Importe);
   HB_FUNC(EASYONE_CFDI33_ADDTRASLADO)
   {
      (((__EasyOne_cfdi33_AddTraslado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddTraslado"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddRetencionesRetencion)(long p, LPSTR Impuesto, LPSTR Importe);
   HB_FUNC(EASYONE_CFDI33_ADDRETENCIONESRETENCION)
   {
      (((__EasyOne_cfdi33_AddRetencionesRetencion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddRetencionesRetencion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddRetencion)(long p, LPSTR Base, LPSTR Impuesto, LPSTR TipoFactor, LPSTR TasaOCuota, LPSTR Importe);
   HB_FUNC(EASYONE_CFDI33_ADDRETENCION)
   {
      (((__EasyOne_cfdi33_AddRetencion)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddRetencion"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddParteInformacionAduanera)(long p, LPSTR NumeroPedimento);
   HB_FUNC(EASYONE_CFDI33_ADDPARTEINFORMACIONADUANERA)
   {
      (((__EasyOne_cfdi33_AddParteInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddParteInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddParte)(long p, LPSTR ClaveProdServ, LPSTR NoIdentificacion, LPSTR Cantidad, LPSTR Unidad, LPSTR Descripcion, LPSTR ValorUnitario, LPSTR Importe);
   HB_FUNC(EASYONE_CFDI33_ADDPARTE)
   {
      (((__EasyOne_cfdi33_AddParte)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddParte"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddNS)(long p, LPSTR prefix, LPSTR url);
   HB_FUNC(EASYONE_CFDI33_ADDNS)
   {
      (((__EasyOne_cfdi33_AddNS)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddNS"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddInformacionAduanera)(long p, LPSTR NumeroPedimento);
   HB_FUNC(EASYONE_CFDI33_ADDINFORMACIONADUANERA)
   {
      (((__EasyOne_cfdi33_AddInformacionAduanera)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddInformacionAduanera"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddConceptoImp)(long p, LPSTR ClaveProdServ, LPSTR NoIdentificacion, LPSTR Cantidad, LPSTR ClaveUnidad, LPSTR Unidad, LPSTR Descripcion, LPSTR ValorUnitario, LPSTR Descuento, LPSTR tasaIVA, LPSTR tasaIEPS, LPSTR impLocal, LPSTR tasaImpLocal, LPSTR RetIva, LPSTR RetISR, LPSTR TipoCalculo);
   HB_FUNC(EASYONE_CFDI33_ADDCONCEPTOIMP)
   {
      (((__EasyOne_cfdi33_AddConceptoImp)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddConceptoImp"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12), (LPSTR) hb_parc(13), (LPSTR) hb_parc(14), (LPSTR) hb_parc(15), (LPSTR) hb_parc(16)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddConcepto)(long p, LPSTR ClaveProdServ, LPSTR NoIdentificacion, LPSTR Cantidad, LPSTR ClaveUnidad, LPSTR Unidad, LPSTR Descripcion, LPSTR ValorUnitario, LPSTR Importe, LPSTR Descuento);
   HB_FUNC(EASYONE_CFDI33_ADDCONCEPTO)
   {
      (((__EasyOne_cfdi33_AddConcepto)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddConcepto"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10)));
   }

   typedef void(__cdecl * __EasyOne_cfdi33_AddCfdiRelacionado)(long p, LPSTR UUID);
   HB_FUNC(EASYONE_CFDI33_ADDCFDIRELACIONADO)
   {
      (((__EasyOne_cfdi33_AddCfdiRelacionado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cfdi33_AddCfdiRelacionado"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_CerKeyMatchFIEL_Free)(long p);
   HB_FUNC(EASYONE_CERKEYMATCHFIEL_FREE)
   {
      (((__EasyOne_CerKeyMatchFIEL_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CerKeyMatchFIEL_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_CerKeyMatchFIEL)(LPSTR Csd, LPSTR Key, LPSTR pwd, LPSTR emisor);
   HB_FUNC(EASYONE_CERKEYMATCHFIEL)
   {
      hb_retnl(((__EasyOne_CerKeyMatchFIEL)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CerKeyMatchFIEL"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_CerKeyMatchCSD_Free)(long p);
   HB_FUNC(EASYONE_CERKEYMATCHCSD_FREE)
   {
      (((__EasyOne_CerKeyMatchCSD_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CerKeyMatchCSD_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_CerKeyMatchCSD)(LPSTR Csd, LPSTR Key, LPSTR pwd, LPSTR emisor);
   HB_FUNC(EASYONE_CERKEYMATCHCSD)
   {
      hb_retnl(((__EasyOne_CerKeyMatchCSD)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CerKeyMatchCSD"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_cce11_SetReceptorDomicilio)(long p, LPSTR Calle, LPSTR NumeroExterior, LPSTR NumeroInterior, LPSTR Colonia, LPSTR Localidad, LPSTR Referencia, LPSTR Municipio, LPSTR Estado, LPSTR Pais, LPSTR CodigoPostal);
   HB_FUNC(EASYONE_CCE11_SETRECEPTORDOMICILIO)
   {
      (((__EasyOne_cce11_SetReceptorDomicilio)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_SetReceptorDomicilio"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11)));
   }

   typedef void(__cdecl * __EasyOne_cce11_SetReceptor)(long p, LPSTR NumRegIdTrib);
   HB_FUNC(EASYONE_CCE11_SETRECEPTOR)
   {
      (((__EasyOne_cce11_SetReceptor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_SetReceptor"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cce11_SetEmisorDomicilio)(long p, LPSTR Calle, LPSTR NumeroExterior, LPSTR NumeroInterior, LPSTR Colonia, LPSTR Localidad, LPSTR Referencia, LPSTR Municipio, LPSTR Estado, LPSTR Pais, LPSTR CodigoPostal);
   HB_FUNC(EASYONE_CCE11_SETEMISORDOMICILIO)
   {
      (((__EasyOne_cce11_SetEmisorDomicilio)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_SetEmisorDomicilio"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11)));
   }

   typedef void(__cdecl * __EasyOne_cce11_SetEmisor)(long p, LPSTR Curp);
   HB_FUNC(EASYONE_CCE11_SETEMISOR)
   {
      (((__EasyOne_cce11_SetEmisor)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_SetEmisor"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_cce11_SetComercioExterior)(long p, LPSTR MotivoTraslado, LPSTR TipoOperacion, LPSTR ClaveDePedimento, LPSTR CertificadoOrigen, LPSTR NumCertificadoOrigen, LPSTR NumeroExportadorConfiable, LPSTR Incoterm, LPSTR Subdivision, LPSTR Observaciones, LPSTR TipoCambioUSD, LPSTR TotalUSD);
   HB_FUNC(EASYONE_CCE11_SETCOMERCIOEXTERIOR)
   {
      (((__EasyOne_cce11_SetComercioExterior)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_SetComercioExterior"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11), (LPSTR) hb_parc(12)));
   }

   typedef void(__cdecl * __EasyOne_cce11_Free)(long p);
   HB_FUNC(EASYONE_CCE11_FREE)
   {
      (((__EasyOne_cce11_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_cce11_Create)(void);
   HB_FUNC(EASYONE_CCE11_CREATE)
   {
      hb_retnl(((__EasyOne_cce11_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_Create"))());
   }

   typedef void(__cdecl * __EasyOne_cce11_AddPropietario)(long p, LPSTR NumRegIdTrib, LPSTR ResidenciaFiscal);
   HB_FUNC(EASYONE_CCE11_ADDPROPIETARIO)
   {
      (((__EasyOne_cce11_AddPropietario)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_AddPropietario"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cce11_AddMercancia)(long p, LPSTR NoIdentificacion, LPSTR FraccionArancelaria, LPSTR CantidadAduana, LPSTR UnidadAduana, LPSTR ValorUnitarioAduana, LPSTR ValorDolares);
   HB_FUNC(EASYONE_CCE11_ADDMERCANCIA)
   {
      (((__EasyOne_cce11_AddMercancia)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_AddMercancia"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_cce11_AddDestinatarioDomicilio)(long p, LPSTR Calle, LPSTR NumeroExterior, LPSTR NumeroInterior, LPSTR Colonia, LPSTR Localidad, LPSTR Referencia, LPSTR Municipio, LPSTR Estado, LPSTR Pais, LPSTR CodigoPostal);
   HB_FUNC(EASYONE_CCE11_ADDDESTINATARIODOMICILIO)
   {
      (((__EasyOne_cce11_AddDestinatarioDomicilio)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_AddDestinatarioDomicilio"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9), (LPSTR) hb_parc(10), (LPSTR) hb_parc(11)));
   }

   typedef void(__cdecl * __EasyOne_cce11_AddDestinatario)(long p, LPSTR NumRegIdTrib, LPSTR Nombre);
   HB_FUNC(EASYONE_CCE11_ADDDESTINATARIO)
   {
      (((__EasyOne_cce11_AddDestinatario)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_AddDestinatario"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_cce11_AddDescripcionesEspecificas)(long p, LPSTR Marca, LPSTR Modelo, LPSTR SubModelo, LPSTR NumeroSerie);
   HB_FUNC(EASYONE_CCE11_ADDDESCRIPCIONESESPECIFICAS)
   {
      (((__EasyOne_cce11_AddDescripcionesEspecificas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_cce11_AddDescripcionesEspecificas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5)));
   }

   typedef void(__cdecl * __EasyOne_CancelaIO_v2_SetInfoPruebasPref)(long p, LPSTR UserName, LPSTR Password, LPSTR RfcEmisor, LPSTR RfcReceptor, LPSTR Total, LPSTR uuid);
   HB_FUNC(EASYONE_CANCELAIO_V2_SETINFOPRUEBASPREF)
   {
      (((__EasyOne_CancelaIO_v2_SetInfoPruebasPref)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_SetInfoPruebasPref"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_CancelaIO_v2_SetInfoPruebas)(long p, LPSTR UserName, LPSTR Password, LPSTR RfcEmisor, LPSTR RfcReceptor, LPSTR Total, LPSTR uuid);
   HB_FUNC(EASYONE_CANCELAIO_V2_SETINFOPRUEBAS)
   {
      (((__EasyOne_CancelaIO_v2_SetInfoPruebas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_SetInfoPruebas"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_CancelaIO_v2_SetInfoPref)(long p, LPSTR UserName, LPSTR Password, LPSTR RfcEmisor, LPSTR RfcReceptor, LPSTR Total, LPSTR uuid);
   HB_FUNC(EASYONE_CANCELAIO_V2_SETINFOPREF)
   {
      (((__EasyOne_CancelaIO_v2_SetInfoPref)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_SetInfoPref"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef void(__cdecl * __EasyOne_CancelaIO_v2_SetInfo)(long p, LPSTR UserName, LPSTR Password, LPSTR RfcEmisor, LPSTR RfcReceptor, LPSTR Total, LPSTR uuid);
   HB_FUNC(EASYONE_CANCELAIO_V2_SETINFO)
   {
      (((__EasyOne_CancelaIO_v2_SetInfo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_SetInfo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7)));
   }

   typedef long(__cdecl * __EasyOne_CancelaIO_v2_Procesa)(long p, LPSTR csd, LPSTR key, LPSTR pwd);
   HB_FUNC(EASYONE_CANCELAIO_V2_PROCESA)
   {
      hb_retnl(((__EasyOne_CancelaIO_v2_Procesa)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_Procesa"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4)));
   }

   typedef void(__cdecl * __EasyOne_CancelaIO_v2_Free)(long p);
   HB_FUNC(EASYONE_CANCELAIO_V2_FREE)
   {
      (((__EasyOne_CancelaIO_v2_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_CancelaIO_v2_Create)(LPSTR OutFile);
   HB_FUNC(EASYONE_CANCELAIO_V2_CREATE)
   {
      hb_retnl(((__EasyOne_CancelaIO_v2_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaIO_v2_Create"))((LPSTR) hb_parc(1)));
   }

   typedef void(__cdecl * __EasyOne_CancelaComprobante_v2_Free)(long p);
   HB_FUNC(EASYONE_CANCELACOMPROBANTE_V2_FREE)
   {
      (((__EasyOne_CancelaComprobante_v2_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaComprobante_v2_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_CancelaComprobante_v2)(LPSTR csd, LPSTR key, LPSTR pwd, LPSTR uuid, LPSTR rfcEmisor, LPSTR agenteId, LPSTR softwareId, LPSTR rfcReceptor, LPSTR total);
   HB_FUNC(EASYONE_CANCELACOMPROBANTE_V2)
   {
      hb_retnl(((__EasyOne_CancelaComprobante_v2)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_CancelaComprobante_v2"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8), (LPSTR) hb_parc(9)));
   }

   typedef void(__cdecl * __EasyOne_arrendamientoenfideicomiso10_SetArrendamientoenfideicomiso)(long p, LPSTR PagProvEfecPorFiduc, LPSTR RendimFideicom, LPSTR DeduccCorresp, LPSTR MontTotRet, LPSTR MontResFiscDistFibras, LPSTR MontOtrosConceptDistr, LPSTR DescrMontOtrosConceptDistr);
   HB_FUNC(EASYONE_ARRENDAMIENTOENFIDEICOMISO10_SETARRENDAMIENTOENFIDEICOMISO)
   {
      (((__EasyOne_arrendamientoenfideicomiso10_SetArrendamientoenfideicomiso)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_arrendamientoenfideicomiso10_SetArrendamientoenfideicomiso"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3), (LPSTR) hb_parc(4), (LPSTR) hb_parc(5), (LPSTR) hb_parc(6), (LPSTR) hb_parc(7), (LPSTR) hb_parc(8)));
   }

   typedef void(__cdecl * __EasyOne_arrendamientoenfideicomiso10_Free)(long p);
   HB_FUNC(EASYONE_ARRENDAMIENTOENFIDEICOMISO10_FREE)
   {
      (((__EasyOne_arrendamientoenfideicomiso10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_arrendamientoenfideicomiso10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_arrendamientoenfideicomiso10_Create)(void);
   HB_FUNC(EASYONE_ARRENDAMIENTOENFIDEICOMISO10_CREATE)
   {
      hb_retnl(((__EasyOne_arrendamientoenfideicomiso10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_arrendamientoenfideicomiso10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_aerolineas10_SetOtrosCargos)(long p, LPSTR TotalCargos);
   HB_FUNC(EASYONE_AEROLINEAS10_SETOTROSCARGOS)
   {
      (((__EasyOne_aerolineas10_SetOtrosCargos)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_aerolineas10_SetOtrosCargos"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_aerolineas10_SetAerolineas)(long p, LPSTR TUA);
   HB_FUNC(EASYONE_AEROLINEAS10_SETAEROLINEAS)
   {
      (((__EasyOne_aerolineas10_SetAerolineas)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_aerolineas10_SetAerolineas"))(hb_parnl(1), (LPSTR) hb_parc(2)));
   }

   typedef void(__cdecl * __EasyOne_aerolineas10_Free)(long p);
   HB_FUNC(EASYONE_AEROLINEAS10_FREE)
   {
      (((__EasyOne_aerolineas10_Free)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_aerolineas10_Free"))(hb_parnl(1)));
   }

   typedef long(__cdecl * __EasyOne_aerolineas10_Create)(void);
   HB_FUNC(EASYONE_AEROLINEAS10_CREATE)
   {
      hb_retnl(((__EasyOne_aerolineas10_Create)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_aerolineas10_Create"))());
   }

   typedef void(__cdecl * __EasyOne_aerolineas10_AddCargo)(long p, LPSTR CodigoCargo, LPSTR Importe);
   HB_FUNC(EASYONE_AEROLINEAS10_ADDCARGO)
   {
      (((__EasyOne_aerolineas10_AddCargo)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_aerolineas10_AddCargo"))(hb_parnl(1), (LPSTR) hb_parc(2), (LPSTR) hb_parc(3)));
   }

   typedef void(__cdecl * __EasyOne_AddResultado)(long p, LPSTR tagName, long resultado);
   HB_FUNC(EASYONE_ADDRESULTADO)
   {
      (((__EasyOne_AddResultado)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_AddResultado"))(hb_parnl(1), (LPSTR) hb_parc(2), hb_parnl(3)));
   }

   typedef void(__cdecl * __EasyOne_AddComplementoConcepto)(long p, long pComplemento);
   HB_FUNC(EASYONE_ADDCOMPLEMENTOCONCEPTO)
   {
      (((__EasyOne_AddComplementoConcepto)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_AddComplementoConcepto"))(hb_parnl(1), hb_parnl(2)));
   }

   typedef void(__cdecl * __EasyOne_AddComplemento)(long p, long pComplemento);
   HB_FUNC(EASYONE_ADDCOMPLEMENTO)
   {
      (((__EasyOne_AddComplemento)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_AddComplemento"))(hb_parnl(1), hb_parnl(2)));
   }

   typedef long(__cdecl * __EasyOne_AddAddendaText)(LPSTR xmlFile, LPSTR xmlAddenda);
   HB_FUNC(EASYONE_ADDADDENDATEXT)
   {
      hb_retnl(((__EasyOne_AddAddendaText)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_AddAddendaText"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2)));
   }

   typedef long(__cdecl * __EasyOne_AddAddenda)(LPSTR xmlFile, LPSTR xmlAddenda);
   HB_FUNC(EASYONE_ADDADDENDA)
   {
      hb_retnl(((__EasyOne_AddAddenda)GetProcAddress(LoadLibrary("EasyOne.DLL"), "EasyOne_AddAddenda"))((LPSTR) hb_parc(1), (LPSTR) hb_parc(2)));
   }

#pragma ENDDUMP

#pragma BEGINDUM
// runtime vc++ 2017 https://go.microsoft.com/fwlink/?LinkId=746571
#include <hbapi.h>
#include <windows.h>

typedef long (_cdecl * _BuscarRif_Create )(void);
typedef void (_cdecl * _BuscarRif_SetCaptcha )(long p, LPSTR captcha);
typedef LPSTR (_cdecl * _BuscarRif_Buscar )(long p, LPSTR rif);
typedef void (_cdecl * _BuscarRif_Free )(long p);


static FARPROC GetProc(LPSTR fnName)
{
HMODULE hmod = LoadLibrary("EasyConnect.Dll");
return GetProcAddress(hmod,fnName);
}

HB_FUNC(BUSCARRIF_CREATE)
{
hb_retnl(((_BuscarRif_Create)GetProc("BuscarRif_Create"))());
}


HB_FUNC(BUSCARRIF_SETCAPTCHA){
((_BuscarRif_SetCaptcha)GetProc("BuscarRif_SetCaptcha"))(hb_parnl(1),(LPSTR)hb_parc(2));
}


HB_FUNC(BUSCARRIF_BUSCAR)
{
hb_retc(((_BuscarRif_Buscar)GetProc("BuscarRif_Buscar"))(hb_parnl(1),(LPSTR)hb_parc(2)));
}

HB_FUNC(BUSCARRIF_FREE){
((_BuscarRif_Free)GetProc("BuscarRif_Free"))(hb_parnl(1));
}


#pragma ENDDUMP


#pragma BEGINDUM
   #include <hbapi.h>
   #include <windows.h>

   typedef long (__cdecl * __Email_Create)(void);
   typedef void (__cdecl * __Email_SetServer)(long p, LPSTR server, int port, int security);
   typedef void (__cdecl * __Email_SetAuth)(long p, LPSTR userName, LPSTR Pwd);
   typedef void (__cdecl * __Email_SetSender)(long p, LPSTR email, LPSTR name);
   typedef void (__cdecl * __Email_AddTo)(long p, LPSTR email);
   typedef void (__cdecl * __Email_AddToCC)(long p, LPSTR emailCC);
   typedef void (__cdecl * __Email_AddToBCC)(long p, LPSTR emailBCC);
   typedef void (__cdecl * __Email_SetMessage)(long p, LPSTR subject, LPSTR body, long isHtlm);
   typedef void (__cdecl * __Email_SetConfigEx)(long p, long priority, long confirmRead);
   typedef long (__cdecl * __Email_SendMail)(long p);
   typedef void (__cdecl * __Email_AddAttachment)(long p, LPSTR file);

   #define Q(x) #x
   #define QUOTE(x) Q(x)
   #define EasyConnect(name) ((__##name)GetProcAddress(LoadLibrary("EasyConnect_mail.dll"), QUOTE(name)))
   #define _hb_parc(x) (LPSTR)hb_parc(x)

   HB_FUNC(EMAIL_CREATE)
   {
      hb_retnl(EasyConnect(Email_Create)());
   }

   HB_FUNC(EMAIL_SETSERVER)
   {
      EasyConnect(Email_SetServer)(hb_parnl(1),_hb_parc(2),hb_parni(3),hb_parni(4));
   }

   HB_FUNC(EMAIL_SETAUTH)
   {
      EasyConnect(Email_SetAuth)(hb_parnl(1),_hb_parc(2),_hb_parc(3));
   }

   HB_FUNC(EMAIL_SETSENDER)
   {
      EasyConnect(Email_SetSender)(hb_parnl(1),_hb_parc(2),_hb_parc(3));
   }

   HB_FUNC(EMAIL_ADDTO)
   {
      EasyConnect(Email_AddTo)(hb_parnl(1),_hb_parc(2));
   }

   HB_FUNC(EMAIL_SETMESSAGE)
   {
      EasyConnect(Email_SetMessage)(hb_parnl(1),_hb_parc(2),_hb_parc(3),hb_parnl(4));
   }

   HB_FUNC(EMAIL_SETCONFIGEX)
   {
      EasyConnect(Email_SetConfigEx)(hb_parnl(1),hb_parnl(2),hb_parnl(3));
   }

   HB_FUNC(EMAIL_SENDMAIL)
   {
      EasyConnect(Email_SendMail)(hb_parnl(1));
   }


#pragma ENDDUMP


// #include "FiveWin.ch"
/*
function leerpdf()

   local cPDF := MemoRead( "c:\test.pdf" )
   local nStart := At( "stream", cPDF )
   local nEnd := At( "endstream", cPDF )
   local cBuf := Replicate( Chr( 0 ), ( nEnd - nStart ) * 10 )
   local cText, nResult
   local hFile

   while nStart <= Len( cPDF )
      nStart = At( "stream", cPDF )
      nEnd = At( "endstream", cPDF )
      cBuf = Replicate( Chr( 0 ), ( nEnd - nStart ) * 10 )
      cText = SubStr( cPDF, nStart + 6, nEnd - nStart )

      if Left( cText, 1 ) == Chr( 0x0d ) .and. ;
         SubStr( cText, 2, 1 ) == Chr( 0x0a )
         nStart += 2
      elseif Left( cText, 1 ) == Chr( 0x0a )
         nStart++
      endif

      if SubStr( cText, nEnd - 2, 1 ) == Chr( 0x0d ) .and. ;
         SubStr( cText, nEnd - 1, 1 ) == Chr( 0x0a )
         nEnd -= 2
      elseif SubStr( cText, nEnd - 1, 1 ) == Chr( 0x0a )
         nEnd--
      endif

      HB_ZUNCOMPRESS( SubStr( cPDF, nStart + 6, nEnd - nStart ), @cBuf, @nResult )

      cPDF = SubStr( cPDF, nEnd + Len( "endstream" ) + 1 )

      ProcessOutput( hFile := fcreate( "c:\test.out", "wb" ), cBuf )
      FClose( hFile )

      if ! Empty( MemoRead( "c:\test.out" ) )
         // MsgInfo( MemoRead( "c:\test.out" ), nResult )
      endif
   end

return nil
*/

/*
https://forums.fivetechsupport.com/viewtopic.php?f=6&t=42141&sid=3b0aa479676b928aea9971c37d5cfdd2
*/

/*
#pragma BEGINDUMP

#include <hbapi.h>
#define HB_LONGLONG LONGLONG

HB_FUNC( STRTOPTR )
{
   hb_retnll( ( HB_LONGLONG ) hb_parc( 1 ) );
}

#pragma ENDDUMP
*/
