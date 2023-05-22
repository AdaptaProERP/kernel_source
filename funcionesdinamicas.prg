/*
hbmk2.exe create.prg -cflag=-D_HB_API_INTERNAL_
Por Antonio Linares 05/05/2023
Desde el comienzo de Clipper 5 deciamos que los codeblocks eran "funciones sin nombre" y ahora resulta
que podemos darles nombre :-)
*/

function Main()

   hb_CreateFunction( "xyz", { || QOut( "from XYZ()" ) } )
   hb_CreateFunction( "abcd", { || QOut( "from ABDC()" ) } )
   hb_CreateFunction( "another", { | c | QOut( ProcName( 1 ) ), QOut( c ) } )

   &( "XYZ()" )
   &( "ABCD()" )
   &( "another( 'Hello world' )")

return nil

#pragma BEGINDUMP

#include <hbapi.h>
#include <hbapiitm.h>
#include <hbvm.h>
#include <hbapicdp.h>

static PHB_ITEM pHash = NULL;

static void evalBlock( void )
{
   HB_USHORT ui, uiParams = hb_pcount();
   char szProcName[ HB_SYMBOL_NAME_LEN + HB_SYMBOL_NAME_LEN + 5 ];
   PHB_ITEM pKey, pValue;

   hb_procname( 0, szProcName, HB_FALSE );
   pKey = hb_itemPutC( NULL, szProcName );
   pValue = hb_hashGetItemPtr( pHash, pKey, HB_HASH_AUTOADD_ACCESS );

   if( pValue )
   {
      hb_vmPushEvalSym();
      hb_vmPush( pValue );

      for( ui = 0; ui < uiParams; ui++ )
         hb_vmPush( hb_param( ui + 1, HB_IT_ANY ) );

      hb_vmSend( uiParams );
   }
}

HB_FUNC( HB_CREATEFUNCTION )
{
   const char * szSymbolName = hb_parvc( 1 );
   PHB_ITEM pText = hb_param( 1, HB_IT_STRING );
   HB_SIZE nLen;
   char * pszBuffer;
   PHB_DYNS pDynSym = hb_dynsymGet( szSymbolName );

   if( ! pHash )
      pHash = hb_hashNew( NULL );

   nLen  = hb_itemGetCLen( pText );
   pszBuffer = hb_cdpnDupUpper( hb_vmCDP(),
                                hb_itemGetCPtr( pText ), &nLen );
   hb_hashAdd( pHash, hb_itemPutC( NULL, pszBuffer ), hb_itemNew( hb_param( 2, HB_IT_BLOCK ) ) );

   pDynSym->pSymbol->value.pFunPtr = evalBlock;
}

#pragma ENDDUMP
