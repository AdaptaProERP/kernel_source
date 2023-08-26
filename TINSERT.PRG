/*
// Clase TINSERTINTO
// Diseñada para Incluir Datos
*/
#INCLUDE "FIVEWIN.CH"

MEMVAR oDp

FUNCTION INSERTINTO(cTable,oOdbc,nInsert)
RETURN TINSERTINTO():New(cTable,oOdbc,nInsert)


CLASS TINSERTINTO

   DATA   oOdbc,oTable

   DATA   nCount,nInsert

   DATA   cTable,cInsert,lEnd
   DATA   aFields

   DATA  aFieldsN INIT {} // Campos Numericos, su valor sera CERO Para evitar valores NULL

   DATA aDefault INIT {}  // 25/08/2023 Asume valores por Defecto
   DATA lFileLog INIT .F. // 25/08/2023
   DATA lAuditar INIT .F. // 25/08/2023

   METHOD New( cTable,oOdbc,nInsert ) CONSTRUCTOR
   METHOD SetDefault()  INLINE ::aDefault:=EJECUTAR("GETDEFAULT",::cTable) //25/08/2023
   METHOD AppendBlank() // INLINE NIL
   METHOD Replace(cField , uValue)
   METHOD FieldPos(cField)
   METHOD Commit()
   METHOD Execute(cSql,cCommand)

   METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                      WndHandleEvent( Self, nMsg, nWParam, nLParam )

   METHOD  End()

ENDCLASS

//----------------------------------------------------------------------------//

METHOD New( cTable, oODBC,nInsert ) CLASS TINSERTINTO

   LOCAL oTable,I

   oTable:=OpenTable("SELECT * FROM "+cTable,.F.)

   ::aFields:=ACLONE(oTable:aFields)

   DEFAULT oODBC  :=oTable:oOdbc ,;
           nInsert:=1

   ::oOdbc  :=oODBC   // oTable:oOdbc
   ::cTable :=oTable:cTable
   ::nInsert:=nInsert  // Cantidad de Registros el Bloque
   ::nCount :=0
   ::cInsert:=""
   ::lEnd   :=.F.

   ::oTable :=oTable // 25/08/2023

   oTable:End()

   IF LEN(::aFields[1])=4
      FOR I := 1 TO LEN(::aFields)
          WHILE LEN(::aFields[I])<=10
            AADD(::aFields[I],NIL)
          ENDDO
      NEXT
   ENDIF

   FOR I=1 TO LEN(::aFields)
      IF ::aFields[I,2]="N"
        AADD(::aFieldsN,::aFields[I,1]) // campos numéricos
      ENDIF
   NEXT I

   // ? LEN(::aFields)
   // ? LEN(::aFields[1])

   AEVAL(::aFields,{|a,n| AADD(::aFields[n],NIL) , ::aFields[n,10]:=NIL })

RETURN  SELF

METHOD FieldPos( cField, uValue ) CLASS TINSERTINTO
     LOCAL nAt
     cField:=ALLTRIM(UPPE(cField))
     nAt   :=ASCAN(::aFields,{|a,n|a[1]=cField})
RETURN nAt

/*
// Reemplazo de Datos
*/
METHOD Replace( cField, uValue ) CLASS TINSERTINTO
    LOCAL nAt:=::FieldPos(cField)
    IF nAt>0
       ::aFields[nAt,10]:=uValue
    ENDIF
RETURN nAt>0

/*
// Asume los Valores por Defecto
*/
METHOD AppendBlank() CLASS TINSERTINTO
 LOCAL I,uValue

 FOR I=1 TO LEN(::aFieldsN)
   ::Replace(::aFieldsN[I],0)
 NEXT I

 FOR I=1 TO LEN(::aDefault)

    uValue:=::aDefault[I,2]

    IF LEFT(uValue,1)="&"
      uValue:=MACROEJE(uValue)
    ENDIF

    ::Replace(::aDefault[I,1],uValue)

 NEXT I

RETURN .T.

/*
// Ejecuta Comando SQL sobre el Dsn 25/08/2023
*/
METHOD Execute(cSql,cCommand) CLASS TINSERTINTO
   LOCAL lResp

   DEFAULT cCommand:="Execute"

   IF oDp:lMySqlNativo
     lResp:=::oOdbc:ExecSQL(cSql,cCommand)
   ELSE
     lResp:=::oOdbc:Execute(cSql,cCommand)
   ENDIF

RETURN lResp


/*
// Insert INTO
*/
METHOD COMMIT() CLASS TINSERTINTO
   LOCAL lResp:=.T.,cSql,I,cInsert:="",cValues:=""

   FOR I=1 TO LEN(::aFields)
      IF ::aFields[I,10]<>NIL
        cInsert:=cInsert+IIF(Empty(cInsert),"",",")+::aFields[I,1]
        cValues:=cValues+IIF(Empty(cValues),"",",")+GetWhere("",::aFields[I,10])
      ENDIF
   NEXT I

   IF !::lEnd

     IF ::nCount=0

        ::cInsert:="INSERT INTO "+::cTable+" ("+cInsert+") VALUES ("+cValues+")"

     ELSE

        ::cInsert:=::cInsert+","+;
                   "("+cValues+")"
     ENDIF

     ::nCount++

     IF ::nCount<::nInsert
       RETURN .T.
      // cSql:=::cInsert
      // cSql:="INSERT INTO "+::cTable+" ("+cInsert+") VALUES ("+cValues+")"

     ENDIF

   ENDIF

   IF !Empty(::cInsert)
      cSql:=::cInsert

//      cSql:="INSERT INTO "+::cTable+" ("+cInsert+") VALUES ("+cValues+")"
      lResp:=::oOdbc:Execute(cSql)
      IF !lResp .OR. !EMPTY(oDp:cMsgOdbc)
         MemoWrit("ERROR.SQL",cSql)
         CLPCOPY(cSql)
         MensajeErr(cSql,MI("Registro no Incluido ",812)+oDp:cScript+"]DSN="+::oOdbc:cDsn+" "+::cTable)
      ELSE
         ::cInsert:=""
         ::nCount :=0
      ENDIF

   ENDIF

   // ? cInsert,cValues,CLPCOPY(cSql)

RETURN lResp

METHOD End( cTable ) CLASS TINSERTINTO

   ::lEnd:=.T.

   IF !Empty(::cInsert)
      ::COMMIT()
   ENDIF

RETURN  NIL
