// ==================================PEGetTabLocation=========================================
// TCRW.PRG                                               (C) Tom Groeger 1998
// ===========================================================================
//
// 16 & 32 Bit Wrapper Classes for Crystal Reports gives you access to
// Crystal Reports 32Bit Dll. The class constructor automatically checks if
// 32Bit Dll's are available, and falls back to 16Bit if not.
// Thanks to Matthew Cullen/Adsystems for his help !
//
//         downloaded from http://www.fivedb.com
// ===========================================================================
// Algunos metodos han sido creados o modificados por Manuel Cabrera
//
//              lolo@redestb.es
//
// ===========================================================================

#define  FW_LEAN_AND_MEAN
#include "fivewin.CH"
#include "struct.CH"

#define TRUE   1
#define FALSE  0

Static cTemp
Static lscreen
Static nX

MEMVAR cQuerySql,oDp
/*
FUNCTION RunCrystalDbf(cModelRpt,cPath,nSalida,lchangeDbf,cFiltros,cFileSql)

   // OJ0 cPath Puede ser Utilizado como Servidor cuando Se requiereSQL
   PRIVATE cQuerySql :=""

   ? "AQUI ESTOY"

   // ALERT("INICIO MAIN(")
   // ALERT(cModelRpt)

   IF EMPTY(cModelRpt)
     nSalida   :="1"
     cModelRpt :="\DP19\CRYSTAL\COTIZASQL.rpt"
     cPath     :="\DP19\CRYSTAL\00\"
     lchangeDbf:=.F.
     cFileSql  :=""
   ENDIF

   IF !EMPTY(cFileSql).and.FILE(cFileSql)
      ALERT("REQUIER ADAPTAR SQL COMANDO")
   ENDIF

   nSalida   :=IIF( Valtype(nSalida)="C", VAL(nSalida) , nSalida )
   lChangeDbf:=IIF( ValType(lChangeDbf) ="C",&lChangeDbf. ,lChangeDbf )
   lChangeDbf:=IIF( ValType(lChangeDbf)!="L",.F.          ,lChangeDbf )

// ALERT(lChangeDbf)

   RUNRPT(cModelRpt,cPath,nSalida,lChangeDbf,cFiltros)

   CANCEL

RETURN .T.
*/
Function RUNRPT(cModelRpt,aFiles,nSalida,cTitle)

Local oDlg,oTimerGen
Local cFileDll:=oDp:cFileDll // GetWinDir()+"\System32\CRPE32.DLL"
Local  oCrw
lScreen        := TRUE

If nSalida=2
  lScreen        := FALSE
Endif

nX := 0

// WINDOWS 2003 Server crea carpeta por usuario, a
// IF !File(cFileDll)
//   cFileDll:="\WINDOWS\System32\CRPE32.DLL"
// ENDIF

IF !File(cFileDll)
   MensajeErr("Archivo :"+cFileDll+" de Crystal Report no Existe","Advertencia")
   Return .F.
ENDIF

IF !File(cModelRpt)
   MensajeErr("No existe "+cModelRpt)
   Return .F.
ENDIF

DEFAULT cTitle:=cModelRpt

oCrw    := Crystal32():New()

   // IF EMPTY(cModelRpt)
     // nSalida   :="1"
     //cModelRpt :="\DPWIN32\CRYSTAL\PRENOMINA.rpt"
     //cPath     :="\DPWIN32\CRYSTAL\"
//     lchangeDbf:=.F.
//     cFileSql  :=""
   // ENDIF

// ERRORSYS(.T.)

 /* DEFINE  WINDOW oDlg from 5,15 to 30,115 title 'Collegamento' OF oDp:oFrameDp

  DEFINE TIMER  oTimerGen OF oDlg INTERVAL 1000 ACTION ChekCRW(oDlg,oCrw)
  ACTIVATE TIMER oTimerGen

  ACTIVATE WINDOW oDlg ICONIZED  ON init (Parti(oDlg,oCrw,cModelRpt,cPath))
  */

  DEFINE TIMER  oTimerGen OF oDp:oFrameDp INTERVAL 100 ACTION ChekCRW(oDp:oFrameDp,oCrw,oTimerGen,aFiles)
  ACTIVATE TIMER oTimerGen

  // ? cTitle,"cTitle"

  Parti(oDp:oFrameDp,oCrw,cModelRpt,aFiles,nSalida,cTitle,aFiles)

//  IF ValType(oTimerGen)="O"
//     oTimerGen:End()
//  ENDIF

 // oCrw:PeClosePrintJob()
 // oCrw:PECloseEngine()
 // oCrw:FreeLibrary()

Return TRUE

Function ChekCRW(oDlg,oCrw,oTimer,aFiles)
  Local nRet :=  oCrw:PEGetJobStatus

//  oDlg:Settext(STR(nRet,10)+" "+;
//       STR(oCrw:PEGetWindowHandle))
//
//  oDlg:Refresh()

  IF  lScreen == FALSE
      nRet :=  oCrw:PEGetJobStatus

      if nRet  == 3 .OR. nRet == 4  .OR.  nRet == 5
         CLOSECRW(oCrw,oTimer,aFiles)
//         oDlg:End()
      endif

      Return TRUE

  *  IF nX == 10
  *     MsgInfo(oCrw:PEGetJobStatus)
  *     nX := 0
  *  ELSE
  *     nX++
  *  ENDIF
ENDIF

  if oCrw:PEGetWindowHandle == 0 .AND. ( nRet == 3 .OR. nRet == 4  .OR.  nRet == 5 )
     CLOSECRW(oCrw,oTimer,aFiles)
   //  oDlg:End()
  endif

Return TRUE

/*
// Cierra el Reporte Crystal
*/
FUNCTION CLOSECRW(oCrw,oTimer,aFiles)

    IF ValType(oTimer)="O"
      oTimer:End()
    ENDIF

    oCrw:PeClosePrintJob()
    oCrw:PECloseEngine()
    oCrw:FreeLibrary()

//    Debe Borrar los DBF
//    AEVAL(aFiles,{|cFile,i|FERASE(cFile)})

RETURN .T.

Function PArti(oDlg,oCrw,cReport,cPath,nSalida,cTitle,aFiles)
// LOCAL lScreen   :=(nSalida=1),nX,cFilter:="",cIndex
// LOCAL aStringhe := {}
// LOCAL aPathFile := {}
// Local  oCrw    := Crystal32():New()
// Local lScreen := (nSalida=1) // TRUE
* Local oDlg
// Local nHandle := FOPEN('RPTDATI.TXT')
 LOCAL I,cFile
 Local cRec := space(1)
 Local nRec := 1
 Local cStringa := ''
 // Local cReport  := ''
 Local cVar := ''
 Local cStr1 := ''
 Local cStr2 := ''
 Local cFilter := ''

 Local aStringhe := {}
 Local aPathFile := {}
 Local nX := 1
 Local nPos
 Local cFiltroReport
 Local nCopie := 1
 LOcal cIndex
 Local nCambiaSta := 0
 LOCAL nTable,nFile,nI //,cFile

 // oDlg:=oDp:oFrameDp

/* cReport := "\dp19\crystal\cotizasqllogo.rpt"


    do while .F. .AND. FREAD(nHandle,@cRec,nRec) ==  1
       IF ASC(cRec) == 10
          FREAD(nHandle,@cRec,nRec)
          do case
             case alltrim(substr(cStringa,1,10)) == 'REPORT'
                  cStringa := alltrim(cStringa)
                  cReport := substr(cStringa,12,len(cStringa)-12)
                  cReport := alltrim(cReport)
             case alltrim(substr(cStringa,1,10)) == 'WINDOWS'
                  IF substr(cStringa,12,3)= 'Yes'
                     lScreen := TRUE
                  ELSE
                     lScreen := FALSE
                  ENDIF
             case alltrim(substr(cStringa,1,10)) == 'FORMULA'
                  cStringa := alltrim(cStringa)
                  cVar  := substr(cStringa,12,len(cStringa)-12)
                  cVar  := alltrim(cVar)
                  IF AT(";" ,cVar) > 0
                     cStr1 := SUBSTR(cVar, 1, AT(";", cVar) - 1)
                     cStr2 := SUBSTR(cVar, AT(";", cVar) + 1)
                     aadd(aStringhe,{ cStr1    ,  cStr2    })

                  ENDIF
             case alltrim(substr(cStringa,1,10)) == 'FILTER'
                  cStringa := alltrim(cStringa)
                  cVar  := substr(cStringa,12,len(cStringa)-12)
                  cFilter  := alltrim(cVar)
             case alltrim(substr(cStringa,1,10)) == 'PATH'
                  cStringa := alltrim(cStringa)
                  cVar  := substr(cStringa,12,len(cStringa)-12)
                  cVar  := alltrim(cVar)
                  IF AT(";" ,cVar) > 0
                     cStr1 := SUBSTR(cVar, 1, AT(";", cVar) - 1)
                     cStr2 := SUBSTR(cVar, AT(";", cVar) + 1)
                     aadd(aPathFile,{ cStr1    ,  cStr2    })

                  ENDIF
             case alltrim(substr(cStringa,1,10)) == 'INDEX'
                  cStringa := alltrim(cStringa)
                  cVar  := substr(cStringa,12,len(cStringa)-12)
                  cIndex  := alltrim(cVar)

             case alltrim(substr(cStringa,1,10)) == 'COPIES'
                  cStringa := alltrim(cStringa)
                  cVar  := substr(cStringa,12,len(cStringa)-12)
                  cVar  := alltrim(cVar)
                  nCopie := val(cVar)
             case alltrim(substr(cStringa,1,10)) == 'SELECTPRT'
                  cStringa := alltrim(cStringa)
                  cVar  := substr(cStringa,12,len(cStringa)-12)
                  cVar  := alltrim(cVar)
                  nCambiaSta := val(cVar)

          endcase
          cStringa := ''
       ENDIF
       cStringa += cRec

    enddo

//  IF ! FCLOSE(nHandle)
//      MsgInfo('File RPTDATI.TXT non chiuso correttamente')
//  ENDIF
*/

  if oCrw:PEOpenPrintJob( cReport )

//   ? cReport

   // ? oCrw:PEGetTabLocation(1)," DE NUEVO"
//   FOR I := 1 TO LEN(aFiles)
//     ? aFiles[i],FILE(aFiles[I])
//       oCrw:PESetTabLocation(I-1,alltrim(aFiles[I]))
//       cFile:=cFileNoExt(oCrw:PEGetTabLocation(I-1))
//      cFile:=cFileNoPath(cFile)
//      IF UPPE(cFile)$UPPE(aFiles[I])
//         ? "AQUI ESTA",I,cFile,aFiles[I]
//      ENDIF
//   NEXT
   // ? oCrw:PESetTabLocation(0,"c:\dp19\dpinv.dbf")

/* nTable := oCrw:PEGetNTables()

   ? nTable,cReport

   FOR nI := 1 TO 10
      cFile:= oCrw:PEGetTabLocation(nI)
      ? cFile
   //   ?  nI,cFile
   NEXT */

   *  oCrw:lScreen := lScreen

     oCrw:PeSetWindowOptions()             // Modifica los Botones de la Ventana
     IF lScreen == TRUE
         oCrw:PEOutputToWindow (oDlg,cTitle)
      else
         oCrw:PEOutToPrinter(nCopie)              // Numero di copia
      ENDIF
  else
      msginfo('Errore peopenprintjob ')
  endif

   *oCrw:PeSetFormula("cTitolo", ['] + 'PROVA DELLA STRINGA ' +  ['] )
   *oCrw:PeSetFormula("cTitolo", ['] + cTitolo +  ['] )
   *oCrw:PeSetFormula("dData"  , ['] + dToc(date()) + ['] )
   *oCrw:PeSetFormula("cRiga1", ['] + m->RAG1 + ['] )
   *oCrw:PeSetFormula("cRiga2", ['] + m->RAG2 + ['] )
   *oCrw:PeSetFormula("cRiga3", ['] + m->RAG3 + ['] )

   if aStringhe # nil .AND. valtype(aStringhe) == 'A'
      for nX := 1 to len(aStringhe)
        aStringhe[nX,2]  := strtran(aStringhe[nX,2],['] , [ ] )
        aStringhe[nX,2]  := strtran(aStringhe[nX,2],["] , [ ] )
        oCrw:PeSetFormula(aStringhe[nX,1], ['] + aStringhe[nX,2] + ['] )
      next
   endif

   *   if  lSelePRN
   *    SelectPrn(oCrw:nJobHandle)
   *endif

    if cFilter # nil
        oCrw:PESetSelFormula(cFilter)
    endif
    // ==========   Selezione solo conto

   if cIndex # nil
     oCrw:ImpostaIndex( cIndex )
   endif
   // ----------   Stabilisci posizione record
   if aPathFile # nil .AND. valType(aPathFile) == 'A'  .AND. len(aPathFile) >= 1
      oCrw:SetPercorso(aPathFile)
   endif
   //----------- Vedi se cambio stampante
   IF nCAmbiaSta == 1

   ENDIF


//   ? "SETPERCORSO"

//   oCrw:SetPercorso({"C:\DPWIN32\CRYSTALL\"})


//   nTable := oCrw:PEGetNTables()
//   ? nTable,cReport,ValType(nTable)
//   FOR nI := 1 TO 10
//     cFile:= oCrw:PEGetTabLocation(nI)
//     ? cFile,nI
//     ?  nI,cFile
//   NEXT

   oCrw:PeStartPrintJob() // Inicia la Impresión

  * IF lScreen
  *   MSGINFO('visto')
  * ENDIF

*  oCrw:PeClosePrintJob()
*  oCrw:PECloseEngine()
*  oCrw:FreeLibrary()


Return nil



// ==========================================================================
// CLASS TCRW   // Crystal Reports 16/32Bit Wrapper
// ==========================================================================

  CLASS Crystal32
     DATA  hDll   AS NUM
     DATA  nJobHandle
     DATA  lError
     DATA  cReport
     // DATA  cTitle // JN
     DATA  oJobInfo
     DATA  oTabLocation
     DATA  oWinOptions
     DATA  lScreen
     DATA  l32


     MESSAGE New                METHOD NEW_ CONSTRUCTOR
     MESSAGE FreeLibrary        METHOD FREECRPE_
     MESSAGE GetModuleHandle    METHOD GETMOD_

     MESSAGE PEOpenEngine       METHOD PEOPEN_
     MESSAGE PEErrorText        METHOD ERRORT_
     MESSAGE PEGetErrorCode     METHOD ERRORN_
     MESSAGE PEGetHandleString  METHOD GETSTR_
     MESSAGE PEOpenPrintJob     METHOD OPENJOB_
     MESSAGE PEOutputToWindow   METHOD OUTPUTW_
     MESSAGE PEPrintWindow      METHOD PRINTWND_
     MESSAGE PEOutToPrinter     METHOD OUTPUTP_
     MESSAGE PEStartPrintJob    METHOD STARTPR_
     MESSAGE PEGetWindowHandle  METHOD GETHWND_
     MESSAGE PEGetJobStatus     METHOD GETJOBS_
     MESSAGE PESetSelFormula    METHOD SETSELE_
     MESSAGE PEGetSelFormula    METHOD GETSELE_
     MESSAGE PECheckSelFormula  METHOD CHECKSEL_
     MESSAGE PESetFormula       METHOD SETFORM_
     MESSAGE PECheckFormula     METHOD CHECKFOR_
     MESSAGE PEGetNTables       METHOD GETNTAB_
     MESSAGE PEGetTabLocation   METHOD GETTLOC_
     MESSAGE PESetTabLocation   METHOD SETTLOC_
     MESSAGE PEGetSqlQuery      METHOD GETSQLQ_
     MESSAGE PESetSqlQuery      METHOD SETSQLQ_
     MESSAGE PETestNTable       METHOD TESTTAB_

     MESSAGE PEClosePrintJob    METHOD CLOSEJOB_
     MESSAGE PECloseEngine      METHOD PECLOSE_
     MESSAGE PeSetWindowOptions METHOD PESETWIN_


     Method ImpostaIndex
     METHOD SetPercorso
     Method SetPrinter32
     Method IfFinish

     METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                         WndHandleEvent( Self, nMsg, nWParam, nLParam )



 END CLASS



// ===========================================================================
// METHOD TCrw::New()
//
// Purpose:
//  16/32Bit Crystal Reports Class Constructor
//
// ===========================================================================
METHOD NEW_()   CLASS Crystal32

    ::hDll       := 0
    ::nJobHandle := 0
    ::lError     := .F.

    DEFAULT oDp:cPathCrystal:=""

    ::hDll :=   LoadLibrary( oDp:cPathCrystal+"CRPE32.DLL" )
    ::l32  := .T.

    // Still no Dll, so bail out
    //
    IF ABS( ::hDll ) <= 32
       ::lError := .T.
       IF ::hDll == 2
          cTemp := "find"
       ELSE
          cTemp := "load"
       ENDIF
       MsgAlert( "Could not " + cTemp + " CRPE or CRPE32.DLL")
    ELSE

   ::PEOpenEngine()
   // Create JobInfo Class

       STRUCT ::oJobInfo
       //  MEMBER structSize   AS WORD  LEN 2
       // my_modi
         MEMBER structSize   AS DWORD  LEN 2

         MEMBER NumRecords   AS DWORD LEN 4
         MEMBER NumSelect    AS DWORD LEN 4
         MEMBER NumPrinted   AS DWORD LEN 4
         MEMBER DispPageN    AS WORD  LEN 2
         MEMBER LatestPage   AS WORD  LEN 2
         MEMBER StartPageN   AS WORD  LEN 2
         MEMBER PrintEnded   AS BOOL  LEN 2

       ENDSTRUCT

       ::oJobInfo:SetMember(1, 22 )

       STRUCT ::oTabLocation

       //  MEMBER structSize   AS WORD   LEN 2
       // my_modi
         MEMBER structSize   AS DWORD   LEN 2

         MEMBER location     AS STRING LEN 256

       ENDSTRUCT

       ::oTabLocation:SetMember(1, 258 )

       // Crea un array que permite modificar los botones que presenta
       // la ventana de preview

       STRUCT ::oWinOptions
      //   MEMBER structSize             AS WORD  LEN 2
       // my_modi
         MEMBER structSize             AS DWORD  LEN 2

         MEMBER hasGroupTree           AS BOOL  LEN 2
         MEMBER CanDrillDown           AS BOOL  LEN 2
         MEMBER hasNavigationControls  AS BOOL  LEN 2
         MEMBER hasCancelButton        AS BOOL  LEN 2
         MEMBER hasPrintButton         AS BOOL  LEN 2
         MEMBER hasExportButton        AS BOOL  LEN 2
         MEMBER hasZoomControl         AS BOOL  LEN 2
         MEMBER hasCloseButton         AS BOOL  LEN 2
         MEMBER hasProgressControls    AS BOOL  LEN 2
         MEMBER hasSearchButton        AS BOOL  LEN 2
         MEMBER hasPrintSetupButton    AS BOOL  LEN 2
         MEMBER hasRefreshButton       AS BOOL  LEN 2
       ENDSTRUCT
       ::oWinOptions:SetMember(1, 26 )

    ENDIF

RETURN Self



// ==========================================================================
// Method TCrw::FreeLibrary()
//
// ========================================================================== */
METHOD FREECRPE_()    CLASS Crystal32

     FreeLibrary( ::hDll )

    ::hDll := 0

 RETURN .T.

// ==========================================================================
// METHOD TCrw::PEOpenEngine() -> lError
//
//  Prepares the Report Engine for requests. This method is a necessary
//  part of any custom-print link. It is also required for any print-only
//  link in which you want the report to print to a window that is to remain
//  visible after the report is printed.
//  It is not necessary to use this method with a print-only link where
//  you are directing the report to a printer
// ========================================================================== */
METHOD PEOPEN_()   CLASS Crystal32

 LOCAL cFarProc
 LOCAL cFunc := "PEOpenEngine"

       cFarProc := GetProcAddress( ::hDLL, cFunc, .T., LONG )
       ::lError := ( CallDll( cFarProc ) == FALSE )

    IF ::lError
       MsgAlert( "Could not open the Print Engine!" + CRLF + ::PeErrorText())
    ENDIF

 RETURN ! ::lError
// ==========================================================================
// Method TCrw::PEOpenPrintJob() -> lError
//
// Prepares to print a report and sets a number which identifies the
// particular print job, called a print job handle. This Handle is
// used in all subsequent calls related to the new print job
// (where a print job handle is required). This method is used as a
//
// reportFilePath	Specifies the file name and path of the report
//                      you want to open
//
// ========================================================================== */
METHOD  OPENJOB_( cReportName ,hWnd , lScreen ) CLASS Crystal32
 LOCAL cFarProc  := ''
 LOCAL cFunc    := "PEOpenPrintJob"

    ::cReport := cReportName
    ::lScreen :=  lScreen



     cFarProc := GetProcAddress( ::hDll, cFunc,.T., _INT, LPSTR )
     ::nJobHandle := CallDll( cFarProc, ::cReport )

    ::lError := .F.
    IF ::nJobHandle == 0
       MsgAlert( "Could not open Print Job!" + CRLF + '[' + ::cReport + ']' )
       ::lError := .T.
    ENDIF

 RETURN ! ::lError
// ==========================================================================
// Method TCrw::PeOutputToWindow( oWnd ) -> lError
//
// Prepares to direct printed output to a print window
//
// He modificado este metodo para que el report aparezca en una ventana a pantalla
// completa.
// ========================================================================== */
METHOD OUTPUTW_( vWnd, cTitle , lMdi )      CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc
 *LOCAL oRect :=  vWnd:GetCliRect()
 LOCAL cFunc := "PEOutputToWindow"

 DEFAULT cTitle:=::cReport
 DEFAULT lMdi  :=.F.

 // ? cTitle,"cTitle"

    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFunc, .T., _INT,;        // nRetVar
                                           LONG,;               // nHandle
                                           LPSTR,;              // cReport
                                           LONG,;               // nTop
                                           LONG,;               // nLeft
                                           LONG,;               // nWidth
                                           LONG,;               // nHeigth
                                           LONG,;               // nStyle
                                           LONG )               // hWnd

// Esto es una variante que coloca el listado dentro de la ventana mdi.
// Probarlo y lo vereis.
    IF lMdi
      nRet := CallDll( cFarProc, ::nJobHandle, cTitle, 0, 0, vWnd:nWidth, vWnd:nHeight, 0, vWnd:hWnd )
    ELSE
     nRet := CallDll( cFarProc, ::nJobHandle, cTitle  , 0, 0,3330, 2210, 0,  )
    ENDIF

    IF nRet == FALSE
       MsgAlert( 'Could not output to Window!' )
       ::lError := .T.
    ENDIF

 RETURN ! ::lError


// ==========================================================================
// Method TCrw::PeStartPrintJob( ) -> lError
// Purpose: Starts the printing of a report.
// ========================================================================== */
METHOD STARTPR_()         CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc
 LOCAL cFunc := "PEStartPrintJob"

    ::lError := .F.
   * Sysrefresh()
       cFarProc := GetProcAddress( ::hDll, cFunc,.T., _INT, LONG, LONG )
       nRet := CallDll( cFarProc , ::nJobHandle, .T. )

    IF nRet == FALSE
       ::lError := .T.
       MsgAlert( "Could not start Report!" + CRLF + ::PEErrorText())
    ENDIF
 RETURN ! ::lError
 // ==========================================================================
// Method TCrw::PEClosePrintJob()
//
// PEClosePrintJob closes the print job. If printing has not yet finished,
// it continues; if the print window is open, it stays open.
// This method is used as a mandatory part of each custom-print link
// to shut down the print job once it has finished printing to screen
// or to window.
// ========================================================================== */
METHOD CLOSEJOB_()     CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc := "PEClosePrintJob"
    ::lError := .F.

    IF ::nJobHandle != 0

          cFarProc := GetProcAddress( ::hDll, cFarPRoc, .T., _INT, LONG )
          nRet := CallDll( cFarProc, ::nJobHandle )

       IF nRet == FALSE
          ::lError := .T.
       ENDIF
    ENDIF
 RETURN ::lError


// ==========================================================================
// Method TCrw::PECloseEngine()
//
// PECloseEngine terminates the Report Engine. All printing is stopped
// and all windows are closed.
//
// NOTE:  This will only happen if the calling application is the
//        last one using CRPE !!!
//
// This method stops the Report Engine from sending output, but the
// report may continue to print from data remaining in the spooler.
// This method is a necessary part of any custom-print link. It is
// also required for any print-only link in which you want the report
// to print to a window that is to remain visible after the report is
// printed. It is not necessary to use this method with a print-only link
// where you are directing the report to a printer.
// ========================================================================== */
METHOD PECLOSE_()      CLASS Crystal32

 LOCAL cFarProc := "PECloseEngine"

    IF ::hDll != 0

          cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT )
          CallDll( cFarProc )
    ENDIF

RETURN .T.

// ==========================================================================
// Method TCrw::PEErrorText()
//
// Purpose:
//  Returns a string describing the status of the most recent Report
//  Engine method called. These method can be used in a custom-print
//  link to display the error string to the user as part of an error message.
// ========================================================================== */

METHOD ERRORT_()       CLASS Crystal32


   LOCAL cFarProc
   LOCAL cBuffer
   LOCAL cError
   LOCAL hError := 0
   LOCAL nLen   := 0
   LOCAL nRet
   LOCAL cFunc := "PEGetErrorText"

     // Get ErrorCode
     //
     cError := "Error Code " + ALLTRIM( STR( ::PEGetErrorCode())) + ":" + CRLF

     // Get StringHandle of ErrorText
     //
        cFarProc := GetProcAddress( ::hDLL, cFunc, .T., _INT, LONG, PTR, PTR )
        nRet := CallDll( cFarProc , ::nJobHandle, @hError, @nLen )


     // Now get the String
     //
     IF nRet == TRUE
        cError += ::PEGetHandleString( hError, nLen )
     ENDIF
     */
 return cError


// ==========================================================================
// Method TCrw::PEGetErrorCode() -> nCode
//
// Returns a number that indicates the status of the most recent Report
// Engine method called. When a call to another method fails, this call
// gets the error code that was generated so you can take some action based
// on that error code. If no Error occured, the method returns 0
// ========================================================================== */
METHOD ERRORN_()  CLASS Crystal32

 LOCAL cFarProc
 LOCAL nCode := 0
 LOCAL cFunc := "PEGetErrorCode"
       cFarProc := GetProcAddress( ::hDLL, cFunc, .T., _INT, LONG )
       nCode    := CallDll( cFarProc, ::nJobHandle )
 RETURN nCode

// ==========================================================================
// Method TCrw::PEGetHandleString( nHandle )
//
// This method will return the text that the string handle is pointing to.
//
// ========================================================================== */
METHOD GETSTR_( nHandle, nLen )   CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc := "PEGetHandleString"
 LOCAL cBuffer := SPACE( nLen )
       cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG,LPSTR,LONG )
       nRet := CallDll( cFarProc , nHandle, @cBuffer, nLen )

    IF nRet == FALSE
       cBuffer := ""
    ELSE
       cBuffer := SUBSTR( cBuffer, 1, nLen -1 )
    ENDIF
    */
 RETURN cBuffer







// ==========================================================================
// Method TCrw::GetModuleHandle( lpName )
//
// This method will return the Handle to a Module( Dll ) that is mapped
// into our address space
//
// ========================================================================== */
METHOD GETMOD_( cModule )  CLASS Crystal32

 LOCAL hDll := LoadLib32( "Kernel32.dll" )
 LOCAL cFarProc
 LOCAL nRet

    cFarProc := GetProcAddress( hDll, "GetModuleHandleA", .T., LONG, LPSTR )
    nRet := CallDll( cFarProc ,cModule )

 RETURN nRet

 // ==========================================================================
// Method TCrw::PEPrintWindow() -> lError
//
// Prints the report displayed in the print window. This method can be
// used in a custom-print link to enable the user to preview the report
// in the print window, and then, if everything looks satisfactory, to
// print the report to the printer (in response to a user event).
//
// ========================================================================== */
METHOD PRINTWND_( lWait )  CLASS Crystal32

 LOCAL cFarProc
 LOCAL cFunc := "PEPrintWindow"
 LOCAL nRet

    DEFAULT lWait := .T.
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFunc, .T., _INT, LONG, LONG )
       nRet := CallDll( cFarProc, ::nJobHandle, IIF(lWait,1,0))


    IF nRet == FALSE
       ::lError := .T.
    ENDIF
    */
 RETURN ! ::lError

  // ==========================================================================
// Method TCrw::PeOutToPrinter( nCopies ) -> lError
//
// Prepares to direct printed output to a printer
//
// ========================================================================== */
METHOD OUTPUTP_( nCopies)  CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc
 LOCAL cFunc := "PEOutputToPrinter"
    DEFAULT nCopies := 1
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFunc, .T., _INT, LONG, LONG )
       nRet :=  CallDll( cFarProc, ::nJobHandle, nCopies )


    IF nRet == FALSE
       MsgAlert( 'Could not output to Printer!' )
       ::lError := .T.
    ENDIF
 RETURN ! ::lError
// ==========================================================================
// Method TCrw::PEGetWindowHandle() -> hWnd
//
// Purpose:
//  Returns the handle of the print window . This method can be used in
//  a custom-print link if you want to do something with the print window
//  (move it, change its size, etc.).
//  PEGetWindowHandle can also be used to determine if the user has already
//  closed the print window.
//
// ========================================================================== */
METHOD GETHWND_()       CLASS Crystal32

 LOCAL hWnd
 LOCAL cFarProc
 LOCAL cFunc := "PEGetWindowHandle"
       cFarProc := GetProcAddress( ::hDll, cFunc,.T.,_INT, LONG )
       hWnd := CallDll( cFarProc , ::nJobHandle )
 RETURN hWnd



// ==========================================================================
// Method TCrw::PEGetGetJobStatus() -> nStatus
//
// Evaluates the status of a print job. You can use this method in a
// number of programming situations, for example:
// -  to trigger error messages, for example, when a print job fails
//   (due to insufficient memory, insufficient disk space, etc.),
// -  to trigger screen displays (hourglass, series of graphics, etc.)
//    that confirm to the user that work is in progress, or
// —  to find out whether a job was canceled by the user after
//    PEStartPrintJob returns.
//
// ========================================================================== */
METHOD GETJOBS_()      CLASS Crystal32

 LOCAL cStruct
 LOCAL nStatus := 0
 LOCAL cFarProc := "PEGetJobStatus"
    cStruct := ::oJobInfo:cBuffer

    cFarProc := GetProcAddress( ::hDll, cFarProc,.T.,_INT, LONG, LPSTR )
    nStatus := CallDll( cFarProc, ::nJobHandle, @cStruct )

 RETURN nStatus
 // ==========================================================================
// Method TCrw::PEGetSelFormula()  // PEGetSelectionFormula
//
// Returns the string for the selection formula used in the specified report.
// This method is typically used as one of a series of methods
// (PEGetSelectionFormula, PEGetHandleString, PESetSelectionFormula).
// The series can be used in a custom-print link to identify and then
// change an existing record selection formula at print time in response
// to a user selection.
//
// ========================================================================== */
METHOD GETSELE_()     CLASS Crystal32

 LOCAL cFarProc
 LOCAL nRet
 LOCAL nText := 0
 LOCAL nLen := 0
 LOCAL cForm := ""
 LOCAL cFunc := "PEGetSelectionFormula"

       cFarProc := GetProcAddress( ::hDll,cFunc,.T., _INT,LONG,PTR,PTR )
       nRet := CallDll( cFarProc, ::nJobHandle, @nText, @nLen )


    IF nRet == FALSE
       ::lError := .T.
    ELSE
       cForm := ::PEGetHandleString( nText, nLen )
    ENDIF

 RETURN cForm

 // ==========================================================================
// Method TCrw::PESetSelFormula()
//
// Changes the selection formula to the formula string you supply as a
// parameter. This method can be used by itself to replace a known
// record selection formula.
// The method can also be used as one of a series of methods
// (PEGetSelectionFormula, PEGetHandleString, PESetSelectionFormula).
// The series can be used in a custom-print link to identify and then
// change an existing record selection formula at print time in response
// to a user selection.
// ========================================================================== */
METHOD SETSELE_( cFormula )     CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc := "PESetSelectionFormula"
    ::lError := .F.
       cFarProc := GetProcAddress( ::hDll, cFarProc,.T.,_INT,LONG, LPSTR )
       nRet := CallDll( cFarProc, ::nJobHandle,  cFormula  )
    IF nRet == FALSE
       MsgAlert( "Invalid Selection!" + CRLF + "[" + cFormula + "]" )
       ::lError := .T.
    ENDIF
    */
RETURN ! ::lError



// ==========================================================================
// Method TCrw::PECheckSelFormula()
//
// Checks the text of the report's record selection formula for errors.
// Use this method whenever the record selection formula has been changed
// and you wish to check the formula for syntax errors.
// If the record selection formula contains an error, this method returns
// False
// ========================================================================== */
METHOD CHECKSEL_()    CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc := "PECheckSelectionFormula"
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFarProc,.T.,_INT, LONG )
       nRet :=  CallDll( cFarProc, ::nJobHandle  )

    IF nRet == FALSE
       ::lError := .T.
    ENDIF
RETURN ! ::lError



// ==========================================================================
// Method TCrw::PESetFormula( cformulaName, cFormulaString )
//
// Changes the specified formula to the formula string you supply as a
// parameter. This method will only change the text of a formula which
// already exists in the report; you can not use it to add a formula.
// This method can be used by itself to replace the formula string for
// a known formula.
// It can also be used as one of a series of methods (PEGetFormula,
// PEGetHandleString, PESetFormula). The series can be used in a
// custom-print link to identify and then change an existing formula at
// print time in response to a user selection.
//
// ==========================================================================
METHOD SETFORM_( cName, cString )     CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc := "PESetFormula"
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll,cFarProc ,.T.,_INT, LONG, LPSTR, LPSTR )
       nRet := CallDll( cFarProc, ::nJobHandle,  cName, cString )

    IF nret == FALSE
       //MsgAlert( "Could not set Formula!" + CRLF + "[" + cName + "]" + ;
       //               + CRLF + "[" + cString + "]" )
       ::lError := .T.
    ENDIF
    */
RETURN ! ::lError


// ==========================================================================
// Method TCrw::PECheckFormula( cFormualName )
//
// The PECheckFormula method checks the text of a named formula for
// validity. Use this method to check a named formula for errors. This
// method works like the Check button in the Formula Editor. If the
// named formula contains an error, the method returns False
//
// ========================================================================== */
METHOD CHECKFOR_( cformulaName )   CLASS Crystal32
 LOCAL nRet
 LOCAL cFarProc := "PECheckFormula"
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFarProc,.T.,_INT,LONG,LPSTR )
       nRet := CallDll( cFarProc, ::nJobHandle,  cformulaName )


    IF nRet == FALSE
       MsgAlert("Invalid Formula!" + CRLF + "[" + cformulaName + "]" )
       ::lError := .T.
    ENDIF

RETURN ! ::lError

// ==========================================================================
// Method TCrw::PEGetNTables()
//
// The PEGetNTables method retrieves the number of tables in the open report.
// It counts both PC and SQL databases. This method is one of a series of
// methods that enable you to retrieve and update database information in an
// opened report so that the report can be printed using different server,
// database, user, and/or table location settings.
//
// ========================================================================== */
METHOD GETNTAB_()    CLASS Crystal32

 LOCAL nTables
 LOCAL cFarProc:= "PEGetNTables"

// LOCAL cFarProc  := GetProcAddress( ::hDll, cFarProc,.T.,_INT, LONG )
//       nTables   := CallDll( cFarProc, ::nJobHandle )

cFarProc  := GetProcAddress( ::hDll, cFarProc,.T.,_INT, _INT) // LONG )
nTables   := CallDll( cFarProc, ::nJobHandle )

//    IF .T. // ::l32
//     cFarProc := GetProc32( ::hDll, cFarProc,.T.,_INT, LONG )
//     ** nTables  := CallDll32( cFarProc, ::nJobHandle )
//    // ELSE
//     // cFarProc := GetProcAddress( ::hDll, cFarProc ,.T.,_INT, _INT )
//    //  cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, LONG, LPSTR )
//     nTables  := CallDll( cFarProc, ::nJobHandle )
//   ENDIF

 RETURN nTables


// ==========================================================================
// Method TCrw::PEGetTabLocation  // PEGetNthTableLocation()
//
// The PEGetNthTableLocation method determines the location of a selected
// table used in the specified print job. This method is typically combined
// with PESetNthTableLocation to identify the location of a table and then
// to change it.
// ========================================================================== */
METHOD GETTLOC_( nPos ) CLASS Crystal32

 LOCAL cBuffer
 LOCAL nRet
 LOCAL cFarProc := "PEGetNthTableLocation"
    // Clear Filename-Buffer
    //
    ::lError := .F.
    ::oTabLocation:SetMember( 2, CHR(0) )

    cBuffer := ::oTabLocation:cBuffer


    cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, LONG, LPSTR )

    nRet :=     CallDll( cFarProc, ::nJobHandle, nPos, @cBuffer )


    ::oTabLocation:cBuffer := cBuffer
    cBuffer := ::oTabLocation:GetMember( 2 )
    *MsgInfo("Buffer"+ cBuffer)

    IF nRet == FALSE .OR. EMPTY( cBuffer )
       MsgAlert( "Error locating Table", ::PEGetErrorCode())
       ::lError := .T.
    ENDIF


 RETURN IIF( ::lError, "", TRIM( cBuffer ))



// ==========================================================================
// Method TCrw::PESetTabLocation  // peSetNthTableLocation()
//
// The PESetNthTableLocation method sets the location for a selected table
// in the specified print job. This method is typically combined with
// PEGetTableLocation to identify the location of a table and then to
// change it.
// ==========================================================================
METHOD SETTLOC_( nPos, cName )   CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc := "PESetNthTableLocation"
    // Set Filename-Buffer
    //
    ::oTabLocation:SetMember( 2, TRIM( cName ) + CHR(0) )
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, LONG, LPSTR )
       nRet     := CallDll( cFarProc, ::nJobHandle, nPos, ::oTabLocation:cBuffer )

    IF nRet == FALSE
       ::lError := .T.
       MsgAlert( "Error setting Location " + cName, ::PEGetErrorText())
    ENDIF
 RETURN ! ::lError
// ==========================================================================
// Method TCrw::PEIsPrintFinished() -> lYesNo // PEIsPrintJobFinished
//
// Monitors the print job to see if it is finished or still in progress.
// You can use this method any time you have a call that is contingent
// on a print job being finished.
//
// ========================================================================== */
*METHOD ISPRTRDY_()
METHOD IfFinish()        CLASS Crystal32

 LOCAL nRet
 LOCAL cFarProc
 LOCAL cFunc := "PEIsPrintJobFinished"

       cFarProc := GetProcAddress( ::hDll, cFunc, .T., _INT, LONG )
       nRet := CallDll( cFarProc, ::nJobHandle )

RETURN ( nRet == TRUE )
// ==========================================================================
// Method TCrw::PESetWindowOptions  // PESetWindowOptions()
//
// Este Metodo es llamado para modificar los botones de la ventana preview
// ========================================================================== */
METHOD PESETWIN_()     CLASS Crystal32

 LOCAL cStruc
 LOCAL nRet
 LOCAL cFarProc := "PESetWindowOptions"

    ::lError := .F.

// restaurado 04/12/2020
    ::oWinOptions:SetMember( 1, TRUE )
    ::oWinOptions:SetMember( 2, TRUE )
    ::oWinOptions:SetMember( 3, TRUE )
//

    ::oWinOptions:SetMember( 4, TRUE )
    ::oWinOptions:SetMember( 5, TRUE )
    ::oWinOptions:SetMember( 6, TRUE )
    ::oWinOptions:SetMember( 7, TRUE )
    ::oWinOptions:SetMember( 8, TRUE )
    ::oWinOptions:SetMember( 9, TRUE )
    ::oWinOptions:SetMember( 10, TRUE )
    ::oWinOptions:SetMember( 11, TRUE )
    ::oWinOptions:SetMember( 12, TRUE )
    ::oWinOptions:SetMember( 13, TRUE )

    cStruc := ::oWinOptions:cBuffer

    cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, LPSTR )
     nRet :=     CallDll( cFarProc, ::nJobHandle, @cStruc )

RETURN self
// ==========================================================================
// Method TCrw::PEGetSqlQuery
//
// You can use this method to retrieve the SQL query that will be generated
// to print the report, and you can update the query using PESetSQLQuery.
//
// ========================================================================== */
METHOD GETSQLQ_()  CLASS Crystal32
 LOCAL cFarProc := "PEGetSqlQuery"
 LOCAL nHandle := 0
 LOCAL nLen := 0
 LOCAL cBuffer := ""
 LOCAL nRet
    ::lError := .F.

       cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, PTR, PTR )
       nRet     := CallDll( cFarProc, ::nJobHandle, @nHandle, @nLen )


    IF nRet == FALSE
       MsgAlert("Could not get SQL query!" + CRLF )
       ::lError := .T.

    ELSE
       cBuffer := ::PEGetHandleString( nHandle, nLen )

    ENDIF
    */
 RETURN cBuffer


// ==========================================================================
// Method TCrw::PESetSqlQuery
//
// You can use this method to set a SQL query
//
// ========================================================================== */
METHOD SETSQLQ_( cQuery )   CLASS Crystal32
 LOCAL nRet
 LOCAL cFarProc := "PESetSqlQuery"

    ::lError := .F.
       cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, LPSTR )
       nRet     := CallDll( cFarProc, ::nJobHandle, cQuery )


    IF nRet == FALSE
       MsgAlert('Could not set query!' + CRLF + cQuery )
       ::lError := .T.
    ENDIF
 RETURN ! ::lError



// ==========================================================================
// Method TCrw::PETestNTable( n )  // PETestNthTableConnectivity( nPos )
//
// Purpose:
// The PETestNthTableConnectivity method tests whether a database
// table's settings are valid and ready to be reported on.
// This method is typically used if you plan to print at a later time
// but you want to test now to make sure everything is in order for
// logging on.
//
// ========================================================================== */
METHOD TESTTAB_( nPos )       CLASS Crystal32
 LOCAL nRet
 LOCAL cFarProc := "PETestNthTableConnectivity"
 LOCAL cError := ""
 LOCAL cBuffer
    DEFAULT nPos := 0

       cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT, LONG, LONG )
       nRet     := CallDll( cFarProc, ::nJobHandle, nPos )


    IF nRet == FALSE
       cError  := ::PEErrorText()
       cBuffer := ::PEGetTabLocation( nPos )
       cError += CRLF + "File: " + cBuffer

    ENDIF
    */
 RETURN cError
// ========================================================================== */
METHOD  ImpostaIndex( cCampo )   CLASS Crystal32
 Local nRet
 LOCAL cFarProc := "PESetNthSortField"
       cFarProc := GetProcAddress( ::hDll, cFarProc, .T., _INT,  LONG, LONG,LPSTR,LONG )
       nRet := CallDll( cFarProc , ::nJobHandle, 0, cCampo, 1 )

 *nRet := peSetNthSortField( ::nHandle, 0 , cCampo , 1 )
 //CRWDLL    FUNCTION peSetNthSortField(nHandle AS _INT, nOrdi AS _INT, cCampo AS LPSTR , nInd AS _INT ) AS BOOL PASCAL
  *CRWDLL    FUNCTION PEGetHandleString(nHandle AS _INT, cVar AS LPSTR, nLen AS _INT) AS BOOL PASCAL



Return nil

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
Method SetPercorso(aPathFile) CLASS Crystal32
Local nTable := ::PEGetNTables()
LOcal nI,cVar ,yVar ,cNew ,cTable ,nX ,lRet

FOR nI := 0 TO nTable - 1
   cVar := Space(258)
   cVar := ::PEGetTabLocation(nI)
   *yVar := Upper( SubStr( cVar, 3 ) )
   yVar := cVar
   cNew := ""
   // ----- Mi dice quale file è nella tabella
   cTable := substr(yVar,rat('\',yVar)+1, len(yVar))
   cTable := upper(cTable)
   for nX := 1 to len(aPathFile)
      if upper(aPathFile[nX,2]) $ cTable
      *if aPathFile[nX] $ cTable
         cNew := aPathFile[nX,1] + cTable
         exit
      endif
   next
   cNew := Upper( cNew )

   if ( ! Empty( cNew ) ) .AND. ! ( cNew = yVar )
      *cNew := cNew + chr(0)
      *cVar := I2BIN( 258 ) + Padr( cNew, 256 )
      cVAr := cNew
      if ! ::PESetTabLocation(nI,cVar)
         MsgAlert( 'Non Posso settare nuovo percorso !' + CRLF + '[' + cNew + ']' )
      endif
   endif
NEXT


Return Self

Method SetPrinter32( cDriver, cPrinter, cPort) CLASS Crystal32


*local aDevmode := devmode(cDriver)

*if ! Select32( ::nHandle, cDriver, cPrinter, cPort, aDevmode )
*   MsgAlert('Could not select Printer!' + CRLF + '[' + cPrinter + ']' )
*endif

Return NIL

































