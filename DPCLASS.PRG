//
// DpClass
// Clase standar Datapro
// Genera variables y metodos virtuales
// JN 05/07/2003
//
#INCLUDE "FIVEWIN.CH"

CLASS TDpClass

// DATA  aVars       // Control de los Campos
// DATA  cName       // Variable
   DATA  cScript     // Programa que Genera los Methods Virtuales
   DATA  cVarName    // Variable que representa el Methodo
   DATA  lIsDef    INIT .F.
   DATA  lMsgError INIT .F.

   DATA  aTablesH  INIT {}

// DATA  nPos        // Posición
// DATA  lAutomatic  // Modo de Asignación de Valores
   DATA  oScript     // Script según Programa DpXbase
   DATA  cClassName  // Nombre de la Clase

   METHOD New(cScript,cVarName)

   METHOD Add( cName, xValue ) INLINE (__objAddData( Self, cName ),;
                                       oSend(Self , cName , xValue))

   METHOD Get( cName )         INLINE oSend(Self,cName)

   METHOD Set( cName, xValue ) INLINE IF(!::ISDEF(cName),::ADD(cName,xValue),oSend(Self , cName , xValue))

   METHOD IsDef( cName ) INLINE (::lIsDef:=.T.,;
                                 __objHasMsg( self, cName ),;
                                 ::lIsDef:=.F.)

   METHOD ClassName() INLINE ::cClassName

   METHOD Close() INLINE ::End()

   METHOD End()

   ERROR HANDLER OnError( cMsg,nPar1,nPar2,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11,nPar12,nError )

ENDCLASS

/*
 *  TDpClass:New()
 */

METHOD New( cScript , cVarName ) CLASS TDpClass

//  ::lAutomatic:=.T.      // Campo no declarado lo asume por defecto
//  ::aVars     :={}
//  ::nPos      :=0
//  ::cName     :=""
  ::cVarName  :=cVarName
  ::cClassName:="TDPCLASS"

  IF !EMPTY(cScript)
     EJECUTAR(cScript,SELF) // Iniciación del Programa DpXbase
     ::oScript:=GetScript()
     ::cScript:=cScript
  ELSE
     ::oScript:=GetScript()
     ::cScript:=::oScript
  ENDIF

  IF !EMPTY(cVarName)
    PUBLICO(cVarName,SELF)
  ENDIF

RETURN SELF

/*
// Culminar la Clase
*/
METHOD End() CLASS TDpClass
RETURN NIL

/*
 *  OnError()
 */

METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TDpClass


  LOCAL cErrorLog,cMsg,lScript:=.F.,J,N //,oFrameDp
  LOCAL lResp:=.F.

  cMsg   := ALLTRIM(UPPE(__GetMessage()))

// MensajeErr(cMsg)
//  #endif

  // cMsg := Upper( AllTrim( cMsg ))
  // MsgAlert(ctoo(uValue,"C"),cMsg)

  if Left( cMsg, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2,Len(cMsg))

      // ::add(cMsg,uValue)
      __objAddData( Self, cMsg )
      oSend(Self ,cMsg , uValue)

   else

   IF ::lIsDef   // Llamada por ::IsDef()
      RETURN NIL
   ENDIF

    __QQPUB(::cVarName)
    PUBLICO(::cVarName,Self)

         //? ::oScript:IsFunction(cMsg),cMsg

         IF !ValType(::oScript)="O" .OR. !::oScript:IsFunction(cMsg)
            COMPILA(::cScript,.T.)
            ::oScript:=GetScript()
            MensajeErr(MI("REQUIERE COMPILAR ",1020)+::cScript+CRLF+MI("Funcion ",1021)+cMsg,GetProce())
            MensajeErr("FUNCTION "+cMsg+CRLF+GETPROCE(),MI("Funcion no existe: ",1022)+::oScript:IsFunction(cMsg))
         ENDIF

         IF ValType(::oScript)="O" // busca la función en el Script
//          RestoreEdit(::nNumEdit,Self)
            lScript:=::oScript:IsFunction(cMsg)
            IF lScript
                // IF "SCR"$::cScript
                //   ? ::cScript,cMsg
                // ENDIF
//                oDp:oScript:=::oScript
                ::oScript:lNoError:=!::lMsgError
                lResp:=::oScript:RUN(cMsg,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)
                PUBLICO(::cVarName,Self)
//                oDp:oScript:=NIL
                IF !Empty(::oScript:cError) .AND. ::lMsgError
                   MensajeErr(::oScript:cError,MI("Programa",1023)+::cScript+CRLF+"Function: "+cMsg)
                ENDIF

                RETURN lResp

            ENDIF
         ENDIF

         IF !lScript

           MsgAlert(cMsg,MI("Programa no está en ejecución",1024)+::cScript)

           // ? lScript,"lScript",::cScript
           // _ClsSetError( _GenError( nError, ::ClassName(), cMsg ) )

         ENDIF

//    endif

   endif
RETURN NIL

/*
// Clase para Generar HTML
*/
CLASS THTML FROM TDpClass

   DATA  aTablesH

ENDCLASS

















































































































































































