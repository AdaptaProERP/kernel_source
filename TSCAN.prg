//  $Id: TSCAN.prg,v 1.1 2004/03/22 12:20:43 xthefull Exp $
// Example scanner, Class TSCAN32 and Wrappers for [x]Harbour
//    This program is free software; you can redistribute it and/or modify
//    it under the terms of the GNU General Public License as published by
//    the Free Software Foundation; either version 2 of the License, or
//    (at your option) any later version.

//    This program is distributed in the hope that it will be useful,
//    but WITHOUT ANY WARRANTY; without even the implied warranty of
//    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//    GNU General Public License for more details.

//    You should have received a copy of the GNU General Public License
//    along with this software; see the file COPYING.  If not, write to
//    the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
//    Boston, MA 02111-1307 USA (or visit the web site http://www.gnu.org/).

//    The exception is that, if you link the class Tscan32 and Wrappers C with other
//    files to produce an executable, this does not by itself cause the
//    resulting executable to be covered by the GNU General Public License.
//    Your use of that executable is in no way restricted on account of
//    linking the Tscan32 and Wrappers library code into it.

// 2003(c)- Rafa Carmona( Thefull )
//
// interface to Dosadi EZTWAIN library by TheFull
// Interface para la libreria EZTWAIN de Dosadi por TheFull
// 32 bits

// ATENTION. THIS LICENSE NOT APLICATTE A EZTWAIN.
// YOUR VIEW LICENSE EZTW32.DLL IN WWW.DOSADI.COM

#INCLUDE "HBCLASS.CH"


// Returns the TWAIN Protocol State per the spec.
#define TWAIN_PRESESSION		1	// source manager not loaded
#define TWAIN_SM_LOADED			2	// source manager loaded
#define TWAIN_SM_OPEN			3	// source manager open
#define TWAIN_SOURCE_OPEN		4	// source open but not enabled
#define TWAIN_SOURCE_ENABLED	5	// source enabled to acquire
#define TWAIN_TRANSFER_READY	6	// image ready to transfer
#define TWAIN_TRANSFERRING		7	// image in transit


#define TWPT_BW          0  Blanco/Negro
#define TWPT_GRAY        1  Escala de Grises
#define TWPT_RGB         2  Color Real
#define TWPT_PALETTE     3  Color Paleta.


***************************************************************************
*
* CLASS TSCAN32
* 2003(c)- by Rafa Carmona ( TheFull )
* Beta 1.0
* Beta 2.0  New. Support Get ClipBoard to hDib.
*           New. ClipboardToFile( cFile ) -> Save File Jpg into Clipboard.
*
* Beta 3.0  New. ReWrite Method End, conflict GPF's  ;)
*
* Beta 4.0  Return parameters in format Logical. Change hb_retni() with hb_retl()
*
****************************************************************************

CLASS TSCAN32

      DATA hWnd
      DATA cFile
      DATA hDib
      DATA lError INIT .F.
      DATA nState
      DATA lBmp   INIT .F.

      METHOD New( cFile, hWnd ) CONSTRUCTOR
      METHOD End()
      METHOD Register()
      MESSAGE FreeNative METHOD End()
      MESSAGE Close      METHOD End()

      METHOD DigiToFile( cFile )
      METHOD DibToJpeg( cFile )  INLINE TW_DibWJpg( ::hDib, cFile )
      METHOD DibToBmp( cFile )   INLINE TW_DibWBmp( ::hDib, cFile )
      METHOD SetJpegQ( nQuality) INLINE TW_SetJpegQuality( nQuality )// 1..100 Default 75

      METHOD Acquire()           INLINE TW_Acquire( ::hWnd )
      METHOD AcquireClipBoard()  INLINE TW_AcquireToClipboard( ::hWnd , 0 )

      METHOD OpenDefault()
      METHOD OpenSource()
      METHOD EnableSource()     INLINE TW_EnableSource( ::hWnd )
      METHOD Choose()           INLINE TW_SelectImageSource()

      // **********    Basic TWAIN Inquiries **********************************
      METHOD IsAvailable()          INLINE TW_Avail()
      METHOD Version()              INLINE ( Str( TW_EasyVersion() / 100 ) )
      METHOD State()                INLINE (::nState := TW_State() )
      METHOD SourceName()           INLINE TW_SourceName()
      METHOD GetSourceName( cName ) INLINE TW_GetSourceName( cName )


      //************   Global Options *****************************************
      METHOD SetHide( lHide )     INLINE IIF( lHide , TW_SetHideUI(1), TW_SetHideUI(0) )


      //-*********** High-level Capability Negotiation Functions **************
      // These functions should only be called in State 4 (TWAIN_SOURCE_OPEN)
      METHOD PixelType( nType ) INLINE ( ::State(),;
                                if( ::nState >= TWAIN_SOURCE_OPEN,;
                                                TW_SetCurrentPixelType( nType ),0 ) )
      METHOD SetRes( dRes )     INLINE ( ::State(),;
                                if( ::nState >= TWAIN_SOURCE_OPEN,;
                                                TW_SetCurrentResolution( dRes ),0) )
      METHOD SetXRes( dRes )            INLINE TW_SetXResolution( dRes )
      METHOD SetYRes( dRes )            INLINE TW_SetYResolution( dRes )
      METHOD SetUnits( nUnits )         INLINE TW_SETCURRENTUNITS( nUnits )
      METHOD SetContrats( dContrats)    INLINE TW_SETCONTRATS( dContrats  )
      METHOD SetBright( dBright )       INLINE TW_SETBRIGHTNESS( dBright )
      METHOD SetPaper( nPaper )         INLINE TW_SETPAPERSIZE( nPaper )
      METHOD GetResolution()            INLINE TW_GETCURRENTRESOLUTION()


      //******** Image Layout (Region of Interest) **********************
      METHOD RegionSet( nLeft,nTop, nRigth, nBottom )  INLINE TW_SetImageLayout( nLeft,nTop, nRigth, nBottom )
      METHOD RegionReset()                             INLINE TW_ResetImageLayout()
      METHOD RegionGet( L,T,R,B )                      INLINE TW_GetImageLayout(@L,@T,@R,@B)
      METHOD RegionGetDefault( L,T,R,B )               INLINE TW_GetDefaultImageLayout(@L,@T,@R,@B)

      METHOD ClipBoardToFile( cFile )
      METHOD DIB_GetFromClipboard() INLINE TW_DIB_GetFromClipboard()

      // feeader
      METHOD HasFeeder()    INLINE TW_HasFeeder()
      METHOD IsFeederLoad() INLINE TW_IsFeederLoaded()
      METHOD IsAutoFeedOn() INLINE TW_IsAutoFeedon()
      METHOD SetAutoFedd( lOnOff )  INLINE TW_SetAutoFedd( lOnOff )
      METHOD SelectFeeder( lOnOff )  INLINE TW_SelectFeeder( lOnOff )

ENDCLASS

***********************************************************
***********************************************************
METHOD New( cFile, hWnd ) CLASS TSCAN32

       IF Empty( hWnd )
          ::hWnd := GetActiveWindow()
       ENDIF

       IF Empty( cFile )
          ::cFile := "TestTscan32.jpg"
       ELSE
          ::cFile := cFile
       ENDIF


       IF !::IsAvailable()
          ::lError := .T.
         // MsgStop( "TWAIN no found","Attention!") // Only Fivewin
       ENDIF

       IF !::lError
          ::Register( 1,0,0,0,"1.0 Beta","by Rafa Carmona","Scan by Thefull","Scan For [x]Harbour" )
       ENDIF
       ::OpenSource()
       ::OpenDefault()

RETURN Self

**********************************************************************
* Comprueba que esta disponible TWAIN y cargalo
**********************************************************************
METHOD OpenSource() CLASS TSCAN32
   Local nResult := TW_LoadSourceManager()

   DO CASE
      CASE nResult = -1
           ::lError := .T.
           // MsgStop("Error Driver TWAIN"," ¿ Insufficient Memory ?")
           Return Self
   ENDCASE

Return Self

***********************************************************************
*  Abre TWAIN el ultimo que fue seleccionado desde Select ..Dialog
* Si no puede cargarlo solicita seleccionar Source.
***********************************************************************
METHOD OpenDefault() CLASS TSCAN32
       Local nResult := TW_OpenDefaultSource()

       DO CASE
          CASE nResult = 0
               ::Choose()
          CASE nResult = -1
               ::lError := .T.
          OTHERWISE
               ::lError := .F.
       ENDCASE

Return Self

**************************************************************************
* Liberamos hDib y cerramos el dispositivo
**************************************************************************
METHOD End() CLASS TSCAN32

   if !Empty( ::hDib )
      TW_Free( ::hDib )
   endif

   TW_CloseSourceManager( ::hWnd ) // Close para evitar GPF

Return NIL

***************************************************************************
* Registra la aplicacion
***************************************************************************
METHOD Register( nMajor ,nMinor,nLanguage,nCountry,cVersion,cName,cFamily,cProduct ) CLASS TSCAN32

       TW_REGISTERAPP( nMajor ,nMinor,nLanguage,nCountry,;
                       cVersion,cName,cFamily,cProduct )

Return Self

***********************************************************
* Digitaliza hacia un fichero .jpg
***********************************************************
METHOD DigiToFile( cFile )

       if EMPTY( cFile )
          cFile := ::cFile
       endif

       IF !::lError
          ::hDib := ::Acquire()         // Importar
       ENDIF

       IF !Empty( ::hDib )
          IF ::lBmp                      // Format bmp
             ::DibToBmp( cFile )       // Si no hacemos uso de BMP
          ELSE
             IF TW_IsJpg()              // Si puede hacer uso del JPG
                ::DibToJpeg( cFile )
             ELSE
                ::DibToBmp( cFile )       // Si no hacemos uso de BMP
             ENDIF
          ENDIF
       ENDIF

Return Self


************************************************************************
* Save content clipboard into to file.jpg
************************************************************************
METHOD ClipBoardToFile( cFile ) CLASS TSCAN32
       Local hDib, nError

       IF Empty( cFile )
          ::cFile := "TestTscan32.jpg"
       ELSE
          ::cFile := cFile
       ENDIF

        hDib := ::DIB_GetFromClipboard()


       IF !Empty( hDib )

           IF ::lBmp
              TW_DibWBmp(  hDib, ::cFile )
           ELSE
              IF TW_IsJpg()              // Si puede hacer uso del JPG
                 TW_DibWJpg( hDib,  ::cFile )
              ELSE
                 TW_DibWBmp(  hDib, ::cFile )
              ENDIF
           ENDIF

           TW_Free( hDib )

       ENDIF

RETURN Self
/* -----------------------------------------------------------------
  Implementation Language C for [x]Harbour(WIN32)
  2003(c)- by Rafa Carmona ( TheFull )

  Wrappers for EZTW32.DLL the DosAdi.

-------------------------------------------------------------------- */
#pragma BEGINDUMP

 #include <windows.h>
 #include "c:\dwh\prg\eztwain.h"
 #include "hbapi.h"


/*--------- Top-Level Calls -------------------------------------*/

 HB_FUNC( TW_ACQUIRE )  // hWnd
 {
  hb_retnl( ( LONG )TWAIN_Acquire( ( HWND ) hb_parnl( 1 ) ) );
 }

 HB_FUNC( TW_FREE )     // hDib
 {
  TWAIN_FreeNative( ( HANDLE ) hb_parnl( 1 ) );
  hb_ret();
 }

 HB_FUNC( TW_SELECTIMAGESOURCE )  // hWnd
 {
  hb_retni( TWAIN_SelectImageSource( ( HWND ) hb_parnl( 1 ) ) );
 }

 HB_FUNC( TW_ACQUIRENATIVE )  // hWnd, nPixTypes
 {
  hb_retnl( ( LONG )TWAIN_AcquireNative( ( HWND ) hb_parnl( 1 ), (unsigned) hb_parni(2) ) );
 }

 HB_FUNC( TW_ACQUIRETOCLIPBOARD )  // hWnd, nPixTypes
 {
  hb_retl( TWAIN_AcquireToClipboard( ( HWND ) hb_parnl( 1 ), (unsigned) hb_parni(2) ) );
 }

 HB_FUNC( TW_ACQUIREMEMORY )  // hWnd
 {
  hb_retnl( ( LONG )TWAIN_AcquireMemory( ( HWND ) hb_parnl( 1 ) ) );
 }

 HB_FUNC( TW_ACQUIRETOFILENAME )  // hWnd, cFileName
 {
  hb_retni( TWAIN_AcquireToFilename( ( HWND ) hb_parnl( 1 ), hb_parc( 2 ) ) );
 }

 HB_FUNC( TW_ACQUIREFILE )  // hWnd, nFF, cFileName
 {
  hb_retni( TWAIN_AcquireFile( ( HWND ) hb_parnl( 1 ), hb_parni( 2 ) ,hb_parc( 3 ) ) );
 }


//--------- Basic TWAIN Inquiries

 HB_FUNC( TW_AVAIL )
 {
  hb_retl( TWAIN_IsAvailable()  );
 }

 HB_FUNC( TW_EASYVERSION)
 {
  hb_retni( TWAIN_EasyVersion()  );
 }

 HB_FUNC( TW_STATE )
 {
  hb_retni( TWAIN_State() );
 }

 HB_FUNC( TW_SOURCENAME )
 {
  hb_retc( TWAIN_SourceName() );
 }

 HB_FUNC( TW_GETSOURCENAME )   // pzName
 {
  TWAIN_GetSourceName( (LPSTR) hb_parc( 1 ) );
  hb_ret();
 }


//--------- DIB handling utilities ---------

 HB_FUNC( TW_DIBWJPG ) // hDib, cName
 {
  hb_parni( DIB_WriteToJpeg( ( HANDLE ) hb_parnl(1), hb_parc( 2 ) ) );
 }

 HB_FUNC( TW_DIBWBMP ) // hDib, cName
 {
  hb_parni( DIB_WriteToBmp( ( HANDLE ) hb_parnl(1), hb_parc( 2 ) ) );
 }


//--------- File Read/Write

 HB_FUNC( TW_ISJPG )
 {
  hb_retl( TWAIN_IsJpegAvailable() );
 }

 HB_FUNC( TW_SETSAVEFORMAT )
 {
  hb_retni( TWAIN_SetSaveFormat( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_GETSAVEFORMAT )
 {
  hb_retni( TWAIN_GetSaveFormat() );
 }

 HB_FUNC( TW_SETJPEGQUALITY ) // nQuality 1...100
 {
  TWAIN_SetJpegQuality( hb_parni( 1 ) );
  hb_ret();
 }

 HB_FUNC( TW_GETJPEGQUALITY )
 {
  hb_retni( TWAIN_GetJpegQuality() );
 }

 HB_FUNC( TW_WRITENATIVETOFILENAME )
 {
  hb_retni( TWAIN_WriteNativeToFilename( (HANDLE) hb_parnl(1), hb_parc(2) ));
 }

 HB_FUNC( TW_LOADNATIVEFROMFILENAME )
 {
  hb_retnl( (LONG) TWAIN_LoadNativeFromFilename( hb_parc( 1 ) ) );
 }



//--------- Global Options ----------------------------------------------
 HB_FUNC( TW_SETMULTITRANSFER )
 {
  TWAIN_SetMultiTransfer( hb_parni( 1 ) );
  hb_ret();
 }

 HB_FUNC( TW_GETMULTITRANSFER )
 {
  hb_retni( TWAIN_GetMultiTransfer() );
 }

 HB_FUNC( TW_SETHIDEUI  ) // nHide
 {
  TWAIN_SetHideUI( hb_parni( 1) );
  hb_ret();
 }

 HB_FUNC( TW_GETHIDEUI  )
 {
  hb_retni( TWAIN_GetHideUI() );
 }


 HB_FUNC( TW_DISABLEPARENT )
 {
  TWAIN_DisableParent( hb_parni( 1 ) );
  hb_ret();
 }

 HB_FUNC( TW_GETDISABLEPARENT )
 {
  hb_retni( TWAIN_GetDisableParent() );
 }

 HB_FUNC( TW_REGISTERAPP )
 {

 TWAIN_RegisterApp( hb_parni(1),hb_parni(2),hb_parni(3),hb_parni(4),
                    hb_parc(5), hb_parc(6),
                    hb_parc(7), hb_parc(8) );
 hb_ret();
 }

 HB_FUNC( TW_SETAPPTITLE )
 {
  TWAIN_SetAppTitle( hb_parc( 1 ) );
  hb_ret();
 }




//--------- TWAIN State Control ---------------------------------------

 HB_FUNC( TW_LOADSOURCEMANAGER )
 {
  hb_retni( TWAIN_LoadSourceManager() );
 }

 HB_FUNC( TW_OPENSOURCEMANAGER )  // hWnd
 {
  hb_retni( TWAIN_OpenSourceManager( ( HWND ) hb_parnl( 1 ) ) );
 }

 HB_FUNC( TW_OPENDEFAULTSOURCE )
 {
  hb_retni( TWAIN_OpenDefaultSource() );
 }

 HB_FUNC( TW_GETSOURCELIST )
 {
  hb_retni( TWAIN_GetSourceList() );
 }

 HB_FUNC( TW_GETNEXTSOURCENAME )
 {
  hb_retni( TWAIN_GetNextSourceName( hb_parc( 1 ) ) );
 }

 HB_FUNC( TW_GETDEFAULTSOURCENAME )
 {
  hb_retni( TWAIN_GetDefaultSourceName( hb_parc( 1 ) ));
 }

 HB_FUNC( TW_OPENSOURCE )
 {
  hb_retni( TWAIN_OpenSource( hb_parc( 1 ) ) );
 }

 HB_FUNC( TW_ENABLESOURCE )       // hWnd
 {
  hb_retni( TWAIN_EnableSource( ( HWND ) hb_parnl( 1 ) ) );
 }

 HB_FUNC( TW_DISABLESOURCE )
 {
  hb_retni( TWAIN_DisableSource( ) );

 }

 HB_FUNC( TW_CLOSESOURCE )
 {
  hb_retni( TWAIN_CloseSource() );
 }

 HB_FUNC( TW_CLOSESOURCEMANAGER )
 {
  hb_retni( TWAIN_CloseSourceManager( (HWND) hb_parnl( 1 ) ) );
 }

 HB_FUNC( TW_UNLOADSOURCEMANEGER )
 {
  hb_retni( TWAIN_UnloadSourceManager() );
 }



//--------- High-level Capability Negotiation Functions --------------
// These functions should only be called in State 4 (TWAIN_SOURCE_OPEN)

 HB_FUNC( TW_GETCURRENTUNITS )
 {
  hb_retni( TWAIN_GetCurrentUnits() );
 }

 HB_FUNC( TW_SETCURRENTUNITS ) // nUnits
 {
  hb_retni( TWAIN_SetCurrentUnits( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_GETBITDEPTH )
 {
  hb_retni( TWAIN_GetBitDepth() );
 }

 HB_FUNC( TW_SETBITDEPTH )
 {
  hb_retni( TWAIN_SetBitDepth( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_GETPIXELTYPE )
 {
  hb_retni( TWAIN_GetPixelType() );
 }

 HB_FUNC( TW_SETCURRENTPIXELTYPE )  // nBits
 {
  hb_retni( TWAIN_SetCurrentPixelType( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_GETCURRENTRESOLUTION )
 {
  hb_retnd( TWAIN_GetCurrentResolution());
 }

 HB_FUNC( TW_GETYRESOLUTION )
 {
  hb_retnd( TWAIN_GetYResolution());
 }

 HB_FUNC( TW_SETCURRENTRESOLUTION )  // dRes
 {
  hb_retni( TWAIN_SetCurrentResolution( hb_parnd( 1 ) ) );
 }

 HB_FUNC( TW_SETXRESOLUTION )
 {
  hb_retni( TWAIN_SetXResolution( hb_parnd( 1 ) ) );
 }

 HB_FUNC( TW_SETYRESOLUTION )
 {
  hb_retni( TWAIN_SetYResolution( hb_parnd( 1 ) ) );
 }

 HB_FUNC( TW_SETCONTRATS ) //dCon
 {
  hb_retni( TWAIN_SetContrast( hb_parnd( 1 ) ) ); // -1000....+1000
 }

 HB_FUNC( TW_SETBRIGHTNESS ) //dBri
 {
  hb_retni( TWAIN_SetBrightness( hb_parnd( 1 ) ) ); // -1000....+1000
  }

 HB_FUNC( TW_SETTHRESHOLD )
 {
  hb_retni( TWAIN_SetThreshold( hb_parnd( 1 ) ) );
 }

 HB_FUNC( TW_GETCURRENTTHRESHOLD )
 {
  hb_retnd( TWAIN_GetCurrentThreshold() );
 }

 HB_FUNC( TW_SETXFERMECH )
 {
  hb_retni( TWAIN_SetXferMech( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_XFERMECH )
 {
  hb_retni( TWAIN_XferMech() );
 }

 HB_FUNC( TW_SUPPORTSFILEXFER )
 {
  hb_retni( TWAIN_SupportsFileXfer() );
 }

 HB_FUNC( TW_SETPAPERSIZE ) // nTypePaper
 {
  hb_retni( TWAIN_SetPaperSize( hb_parni( 1 ) ) );
 }

//-------- Document Feeder ---------------------------------

 HB_FUNC( TW_HASFEEDER )
 {
  hb_retl( TWAIN_HasFeeder() );
 }

 HB_FUNC( TW_ISFEEDERSELECTED )
 {
  hb_retl( TWAIN_IsFeederSelected() );
 }

 HB_FUNC( TW_SELECTFEEDER )
 {
  hb_retl( TWAIN_SelectFeeder( hb_parl( 1 ) ) );
 }

 HB_FUNC( TW_ISAUTOFEEDON )
 {
  hb_retl( TWAIN_IsAutoFeedOn() );
 }

 HB_FUNC( TW_SETAUTOFEDD )
 {
  hb_retl( TWAIN_SetAutoFeed( hb_parl( 1 ) ) );
 }

 HB_FUNC( TW_ISFEEDERLOADED )
 {
  hb_retl( TWAIN_IsFeederLoaded() );
 }

//-------- Duplex Scanning ------------------------------------------
 HB_FUNC( TW_GETDUPLEXSUPPORT )
 {
  hb_retni( TWAIN_GetDuplexSupport() );
 }

 HB_FUNC( TW_ENABLEDUPLEX )
 {
  hb_retni( TWAIN_EnableDuplex( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_ISDUPLEXENABLED )
 {
  hb_retl( TWAIN_IsDuplexEnabled() );
 }

//--------- Other 'exotic' capabilities --------

 HB_FUNC( TW_HASCONTROLLABLEUI )
 {
  hb_retni( TWAIN_HasControllableUI() );
 }

 HB_FUNC( TW_SETINDICATORS )
 {
  hb_retni( TWAIN_SetIndicators( hb_parl( 1 ) ) );
 }

 HB_FUNC( TW_COMPRESSION )
 {
  hb_retni( TWAIN_Compression() );
 }

 HB_FUNC( TW_SETCOMPRESSION )
 {
  hb_retni( TWAIN_SetCompression( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_TILED )
 {
  hb_retl( TWAIN_Tiled() );
 }

 HB_FUNC( TW_SETTILED )
 {
  hb_retni( TWAIN_SetTiled( hb_parl( 1 ) ) );
 }

 HB_FUNC( TW_PLANARCHUNKY )
 {
  hb_retni( TWAIN_PlanarChunky() );
 }

 HB_FUNC( TW_SETPLANARCHUNKY )
 {
  hb_retni( TWAIN_SetPlanarChunky( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_PIXELFLAVOR )
 {
  hb_retni( TWAIN_PixelFlavor() );
 }

 HB_FUNC( TW_SETPIXELFLAVOR )
 {
  hb_retni( TWAIN_SetPixelFlavor( hb_parni( 1 ) ) );
 }

 HB_FUNC( TW_SETLIGHTPATH )
 {
  hb_retni( TWAIN_SetLightPath( hb_parl( 1 ) ) );
 }

 HB_FUNC( TW_SETAUTOBRIGHT )
 {
  hb_retni( TWAIN_SetAutoBright( hb_parl( 1 ) ) );
 }

 HB_FUNC( TW_SETGAMMA )
 {
  hb_retni( TWAIN_SetGamma( hb_parnd( 1 ) ) );
 }

 HB_FUNC( TW_SETSHADOW )
 {
  hb_retni( TWAIN_SetShadow( hb_parnd( 1 ) ) );
 }

 HB_FUNC( TW_SETHIGHLIGHT )
 {
  hb_retni( TWAIN_SetHighlight( hb_parnd( 1 ) ) );
 }
//--------- Image Layout (Region of Interest) --------


 HB_FUNC( TW_SETIMAGELAYOUT )   // left, top, right, bottom
 {
  hb_retni( TWAIN_SetImageLayout( hb_parnd( 1 ),hb_parnd( 2 ),hb_parnd( 3 ),hb_parnd( 4 ) ) );
 }

 HB_FUNC( TW_RESETIMAGELAYOUT )
 {
  TWAIN_ResetImageLayout();
  hb_ret();
 }

 HB_FUNC( TW_GETIMAGELAYOUT )
 {
  double L,T,R,B;
  int nRet;

  nRet = TWAIN_GetImageLayout( &L,&T,&R,&B );

  hb_stornd( L, 1 );
  hb_stornd( T, 2 );
  hb_stornd( R, 3 );
  hb_stornd( B, 4 );
  hb_retni( nRet );
 }

 HB_FUNC( TW_GETDEFAULTIMAGELAYOUT )
 {
  double L,T,R,B;
  int nRet;

  nRet = TWAIN_GetDefaultImageLayout( &L, &T, &R, &B);

  hb_stornd( L, 1 );
  hb_stornd( T, 2 );
  hb_stornd( R, 3 );
  hb_stornd( B, 4 );
  hb_retni( nRet );
 }

//HANDLE EZTAPI DIB_GetFromClipboard(void);
HB_FUNC( TW_DIB_GETFROMCLIPBOARD )
{
 hb_retnl( (LONG) DIB_GetFromClipboard() );
}

// para no depender de Five, y harbour poder el solito trabajar ;)
 HB_FUNC ( GETACTIVEWINDOW )
 {
  hb_retnl( ( LONG ) GetActiveWindow() );
 }

#pragma ENDDUMP


/*
  TWAIN.
  Bajo estas siglas se esconde la tecnologica de adquisicion de imagen
  desde cualquier tipo de dispositivo que cumpla dicho standar, como
  puede ser el dispositivo mas comun, el scanner, a otros como las camaras
  WebCam.

  En este capitulo veremos las posibilidades qe tenemos nosotros , vil
  progrmador de Harbour&Fivewin, de aprovecharnos.

  He de decir que nosotros usaremos un 'atajo'. Dicho 'atajo' sera hacer
  uso de un par de DLL que a parte de cumplir nuestro requisito , que es
  ni mas ni menos que controlar un dispositivo que cumpla la norma TWAIN,
  nos hara la programacion muchisimo mas sencilla.

  La primera DLL a usar sera EZTW32.DLL, de la compañia dosadi.
  Dicha DLL nos brinda la posibilidad de conectar con el standard TWAIN
  de  una forma mucho mas simple.
  He de decir que es perfectamente posible programar el TWAIN directamente,
  ahora bien, no creo que el esfuerzo invertido merezca la pena teniendo
  dicha DLL a nuestro alcance.

  La segunda es ij11.dll de la casa Intel. Dicha libreria es usada por
  la dll EZTW32.DLL para guardar la imagen obtenido a un fichero JPEG.
  Si no tuvieramos dicha ij11.dll, simplemente se guardara en formato BMP.

  Teniendo en cuenta todo esto, empezaremos a explicar la clase de mi
  creacion: TSCAN32.

  El motivo principal a que me ha llevado a crear la interface con el
  driver TWAIN son dos:

   1.- La clase TScan de 16 bits que viene con Fivewin, en determinados
       momentos no funciona como deberia de funcionar.

   2.- La salida no era posible guardala como formato JPEG.

   Asi que os podeis imaginar las mejoras de la nueva clase con respecto
   a la de 16 bits:

   1.- 32 Bits
   2.- Interface en lenguaje C.
   3.- Graba en JPEGS o directamente al portapapeles!!!
   4.- Realmente funciona!!! ;-)

   Veamos pues todo el entramado en codigo fuente.

   Creacion de un objeto TScan y scaneando:

      oScan := TScan32():New( cFile )
      oScan:DigiToFile()

   Y esto es todo lo que tienes que escribir!!
   Realmente fantastico, no crees ?

   Ahora bien, como es logico de suponer , quizas nos interese escoger
   que tipo de salida queremos, dimensiones, etc..., como por ejemplo:

      oScan:PixelType( TWPT_GRAY  )  // TWPT_GRAY  = 1

   con esta simple instruccion estamos definiendo que el dispositivo
   capture en Escala de Grises!
   Realmente esto no podria ser mas simple!

   Teneis disponible un ejemplo donde vereis como podemos escanear y
   visualizar lo que hemos escaneado para que veais que con cuatro lineas
   de codigo podemos hacer cosas muy potente.

   Ahora bien, os preguntareis como funciona el chiringuito, pues pasemos
   a ver como esta construido y el porque.

   CLASE TSCAN32. Variables de Instancia y metodos.

   La clase TScan32 te encapsula todo el funcionamiento interno para que
   a la hora de escribir tu aplicacion , el coste de implementacion sea
   minimo, y ya lo creo que es minimo!!! 2 Lineas de programacion y
   tienes tu aplicacion disponible para escanner desde scanners,webcam,etc...

   Aqui detallare los metodos mas interesantes o mas comunes, aunque en
   la implementacion en C realice un largo y tedioso trabajo y esta el
   95% portada para que puedas hacer uso si ese es tu deseo.

   DATAs:
      DATA hWnd
           Handle de la ventana padre.
      DATA cFile
           Nombre del fichero resultante.
      DATA hDib
           Handle del DIB
      DATA lError INIT .F.
           Indica si se ha producido un error
      DATA nState
           Indica en que estado se encuentra el dispositivo TWAIN
      DATA lBmp   INIT .F.
           Indica qe quiero salida BMP si es .T.


   METHODs:

      New( cFile, hWnd ) CONSTRUCTOR
      Inicializa el dispositivo TWAIN.
         cFile := Nombre del Fichero resultante
          hWnd := Handle de la ventana padre

      End(), FreeNative , Close
      Cierra el dispositivo TWAIN y libera hDib.

      Register()
      Registra la aplicacion.

      DigiToFile( cFile )
      Digitaliza directamente a un fichero.
         cFile := Fichero resultante opcional.
                  Por defecto es ::cFile

      DibToJpeg( cFile )  y DibToBmp( cFile )
      Guarda el contenido del resultado de la adquisicion, hDib,
      en un fichero JPEG o BMP

      SetJpegQ( nQuality)
      Calidad del JPEG. Por defecto es 75.
      El baremo es entre 1...100 , siendo 100 maxima calidad.

      Acquire()
      Digilitaliza. Devuelve un DIB.

      AcquireClipBoard()
      Digilitaliza directamente al portapapeles.
      Devuelve .T. o .F. dependiendo si hay error o no.

      OpenDefault()
      Abre TWAIN el ultimo que fue seleccionado desde Select ..Dialog
      Si no puede cargarlo solicita seleccionar Source.

      OpenSource()
      Comprueba que esta disponible TWAIN y cargalo si no estuviera cargado

      Choose()
      Permite seleccionar la fuente desde donde vamos a adquirir.


      IsAvailable()
      Devuelve si esta o no disponible TWAIN en el sistema

      Version()
      Devuelve Version de la DLL de DosAdi.

      State()
      Devuelve el estado del dispositivo.


      SetHide( lHide )
      Oculta UI del escanner.


      Estos metodos solamente pueden ser llamados cuando el dispositvo TWAIN
      se encuentro en un Estado 4  (TWAIN_SOURCE_OPEN)

      PixelType( nType )
      Tipo de scaneo del dispositivo.
        #define TWPT_BW          0  Blanco/Negro
        #define TWPT_GRAY        1  Escala de Grises
        #define TWPT_RGB         2  Color Real
        #define TWPT_PALETTE     3  Color Paleta.


      SetRes( dRes )
      Prepara resolucion a capturar en el dispositivo.

      SetXRes( dRes )  y SetYRes( dRes )
      Prepara resolucion Horizontal y Vertical del dispositivo.

      SetUnits( nUnits )
      Sistema de Medida. Atencion, no todos los dispositivos admiten
      alguna medida de estas, pero todos si que admiten inches.
        #define TWUN_INCHES      0
        #define TWUN_CENTIMETERS 1
        #define TWUN_PICAS       2
        #define TWUN_POINTS      3
        #define TWUN_TWIPS       4
        #define TWUN_PIXELS      5

      SetContrats( dContrats)
      Contraste del dispositivo.

      SetBright( dBright )
      Brillo del dispositivo.

      SetPaper( nPaper )
      Seleccionar tipo de papel , tamaño, del dispositivo,
      Por poner unos pocos, si quieres mas tipo mirate  TWSS_* en TWAIN.H
         #define PAPER_A4LETTER    1
         #define PAPER_B5LETTER    2
         #define PAPER_USLETTER    3
         #define PAPER_USLEGAL     4
         #define PAPER_A5          5
         #define PAPER_B4          6
         #define PAPER_B6          7
         #define PAPER_USLEDGER    9
         #define PAPER_USEXECUTIVE 10
         #define PAPER_A3          11
         #define PAPER_B3          12
         #define PAPER_A6          13
         #define PAPER_C4          14
         #define PAPER_C5          15
         #define PAPER_C6          16

      GetResolution()
      Devuelve que resolucion actualmente tiene el dispositivo.


      Con estos metodos que acontinuacion veremos , podemos seleccionar
      una porcion de la region a adquirir.

      RegionSet( nLeft,nTop, nRigth, nBottom )
      Selecciona una region para adquirir.

      METHOD RegionReset()
      Restaura a la region que habia por defecto, toda la superficie.

      RegionGet( L,T,R,B )
      Devuelve en las variables la porciones de la region actualmente definidas.
      El uso es muy simple:

      Local L,T,R,B  // Left,Top,Rigth y Bottom, logicamente ;)
      oScan := oScan:New()
      oScan:RegionGet(@L,@T,@R,@B), obteniendo el resultado en las misma variables
      pasadas por referencia.

      RegionGetDefault( L,T,R,B )
      Devuelve los valores de la region por defecto. El mismo ejemplo del method
      RegionGet es aplicable a este.


      Y para finalizar , un par de metodos muy utiles:

      ClipBoardToFile( cFile )
      Guarda el contenido del portapapeles en un fichero.

      DIB_GetFromClipboard()
      Coge el contenido del portapapeles ,DIB., y lo asigna a hDib.

      Como podeis observar , esto es mas que suficiente para dotar a nuestras
      aplicaciones Fivewin&Harbour de capacidades de TWAIN.

      Otro aspecto a tener em cuenta son las funciones en C, pero que se
      escapan a la teoria de este libro.

      Quien realemente esta mas interesado en saber sobre TWAIN, vaya a
      internet y realice una busqueda.
      Creo que para nosotros , programadores de Fivewin, esto es mucho
      mas que suficiente y ya les gustaria a mas de uno de otros lenguajes
      de programacion tener esto a su alcance tan facil como aqui he explicado.

      Los wrappers en C los teneis disponibles junto en el fichero de la clase
      TSCAN32 para los que quieran ver el interior o modificarlo a su gusto.

      Teneis portadas 90 funciones desde C para que hagais uso desde harbour.

      Como comentario final, he de decir que a sido probado y con exito en
      scanners paralelos, usb y SCSI, asi como camaras WebCam.


   */








*/
