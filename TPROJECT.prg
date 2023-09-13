/*
 *  TPROJECT()
 *  Permite crear variables de manera dinámica reemplazando el clasico esquema para crear variables Virtuales
 *  Creada por Juan Navas 29/08/2020 03:02:28
*/

# include "FiveWin.ch"

MEMVAR oDp

/*
 * TPROJECT()
 */
CLASS TPROJECT

  DATA  cName       // AS CHARACTER   INIT ""  // READONLY // [ER]
  DATA  cProgram
  DATA  cScript    AS "TPROJECT" // Programa DpXbase para ejecutas las funciones UDF
  DATA  oScript

  DATA  aCodigos INIT {}
  DATA  aVars    INIT {}  // Objetos Variables
  DATA  nCantid  INIT 1
  DATA  nPrecio  INIT 0
  DATA  nTotal   INIT 0
  DATA  nCosto   INIT 0

  DATA  cTipo  INIT "Producto"

  DATA   nNivel INIT 1

  DATA   dDesde INIT CTOD("")
  DATA   dHasta INIT CTOD("")


  DATA  aAct INIT {}  // Proyecto esta compuesto de Actividades y las Actividades compuestas por Tareas. Permite Concatenacion Infinita

  METHOD New( cName )
  METHOD End()  INLINE NIL

  METHOD Get( cName ) INLINE oSend(Self,cName)
  METHOD Set( cName, xValue )
  METHOD Add( cName, xValue )
  METHOD IsDef( cName )
  METHOD Inspect() INLINE EJECUTAR("INSPECT",Self)
  METHOD RunScript(cFunction,nPar1,nPar2,nPar3,nPar4)
  METHOD IsFunction(cFunction) INLINE ::oScript:IsFunction(cFunction)

  METHOD ADDACTIVIDAD(oAct)

  ERROR HANDLER OnError( cMsg, nError )

ENDCLASS

/*
 *  TPROJECT:New()
 */
METHOD New( cName ) CLASS TPROJECT // [WA]

    DEFAULT cName:="PYR"

    ::cName:=cName

    oDp:oPry:=Self

RETURN Self

METHOD ADDACTIVIDAD( oAct ) CLASS TPROJECT

   AADD(::aAct,oAct)

RETURN oAct

/*
 *  TPROJECT:Add()
 */
METHOD Add( cName, xValue ) CLASS TPROJECT // [ER]

   IF !::IsDef(cName)
      __objAddData( Self, cName )
      __objSendMsg( Self, "_" + cName , xValue )                  //  WG
   ENDIF

RETURN Self

/*
 *  TPROJECT:Set()
 */
 METHOD Set( cName, xValue ) CLASS TPROJECT

     IF oSend(Self,cName)=NIL
        __objAddData( Self, cName )
     ENDIF

     __objSendMsg( Self, "_" + cName , xValue )                 // WG

RETURN Self

/*
 *  TPROJECT:IsDef()
 * Facilita comprobar si existe una variable
 */
METHOD IsDef( cName ) CLASS TPROJECT                // [by ER]
RETURN oSend(Self,cName)!=NIL

/*
// Ejecuta Función del Script
*/
METHOD RunScript(cFunction,uPar1,uPar2,uPar3,uPar4,uPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)  CLASS TPROJECT

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

     ::MensajeErr("Función : "+cFunction+" No Existe ")

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
 *  OnError()
 */

METHOD OnError( uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11 ) CLASS TPROJECT
  LOCAL cMsg   := UPPE(ALLTRIM(__GetMessage()))

  if SubStr( cMsg, 1, 1 ) == "_" // Asignar Valor

      cMsg:=Subs(cMsg,2)
      __objAddData( Self, cMsg )
      __objSendMsg( Self, "_" + cMsg, uValue )

      RETURN uValue

  ENDIF

  IF !ValType(::oScript)="O"
      ::oScript:=XCOMPILA(::cScript)
  ENDIF

  IF !::oScript:IsFunction(cMsg)
     MensajeErr("FUNCTION "+cMsg+CRLF+GETPROCE(),"Función no existe ")
     RETURN NIL
  ENDIF

  ::oScript:lNoError:=!::lMsgError

  uValue:=::RunScript(cMsg,Self,uValue,nError,nPar3,nPar4,nPar5,nPar6,nPar7,nPar8,nPar9,nPar10,nPar11)

RETURN NIL

/*
// SubProyecto Hereda la Misma Funcionalidad del Proyecto
*/

CLASS TACTIVIDAD FROM TPROJECT

  DATA  oPry
  DATA  nNivel  INIT 2
  DATA  aTareas INIT {}
  DATA  cScript INIT "TACTIVIDAD"

  METHOD New( cName )
  METHOD End()  INLINE NIL

ENDCLASS

/*
 *  TPROJECT:New()
 */
METHOD New(  cName,oPry ) CLASS TACTIVIDAD

    DEFAULT cName:="PYR",;
            oPry :=oDp:oPry

    ::cName  :=cName
    ::oPry   :=oPry
    ::aTareas:={}

    AADD(::oPry:aAct,Self)

RETURN Self

CLASS TTAREA FROM TPROJECT

  DATA  oAct
  DATA  nNivel  INIT 3
  DATA  cName   INIT "TAREA"
  DATA  cScript INIT "TTAREA"

  METHOD New( cName )
  METHOD End()  INLINE NIL

ENDCLASS

/*
 *  TTAREA()
 */
METHOD New( cName , oAct ) CLASS TTAREA

    DEFAULT cName:="TAREA"

    oDp:oTarea:=Self

// ? "AQUI ES TAREA",oAct:ClassName()

    ::cName:=cName

EJECUTAR("INSPECT",Self)

// ? ValType(oAct:aTareas)
// AADD(::oAct:aTareas,Self)

RETURN Self


CLASS TVARS FROM TPROJECT

  DATA  oProject
  DATA  nNivel INIT 2
  DATA  cName  INIT "TAREA"

  METHOD New( cName )
  METHOD End()  INLINE NIL

ENDCLASS

/*
 *  TVARS
 */
METHOD New( oPry, cName ) CLASS TVARS

    DEFAULT cName:="A1"

    oDp:oVar:=Self

// ? "AQUI ES VARS",oAct:ClassName()

    ::cName:=cName

//    AADD(::oPry:aVars,Self)

RETURN Self

