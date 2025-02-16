
/*
// Clase MyTable para Cursores Mysql en Forma Nativa
*/
#INCLUDE "FIVEWIN.CH"

STATIC aDataBase
STATIC aTableOpen
STATIC aTablesDB
STATIC cDataBase_
STATIC aStruct
STATIC nTime

// STATIC aLogico

MEMVAR oDp,aData,bBlq
MEMVAR _MycIp,_MycPass,_MycLoging,_MySqlDate,_MySqlPort


FUNCTION MyTable(cSql,lArray,cDataBase,lChecktable,oDb)
   LOCAL cTable,nAt:=0
   LOCAL aExcluye :=GetExcluye()

   // ? "AQUI ES MyTable",GetProce()
   // ValType(aTablesDB)
   // ? "AQUI ES",oDp:cTypeBD,cDataBase," TMYTABLE",cSql,GETPROCE()

   DEFAULT lChecktable:=.T.

   IF oDp:cTypeBD!="MYSQL" .AND. !oDp:lNativo
     RETURN OpenTable(cSql,.T.)
   ENDIF

   DEFAULT aDataBase  :={},;
           aTableOpen :={},;
           cDataBase_ :="",; // Ultima DataBase Abierta
           aTablesDb  :=GetTables() ,;
           oDp:lLogicField:=.T.

   IF !Empty(aDataBase) .AND. oDp:lMYSQLCHKCONN
     MYSQLCHKCONN()
   ENDIF

   // Cuando se Instala y no Tiene Tablas no puede buscar DPCAMPOS JN 27/05/2016
   IF oDp:aLogico=NIL .AND. LEN(GetTables())>1  .AND. oDp:lLogicField

      oDp:aLogico:={}

      // ? ValType(oDp:aLogicos),"AQUI OBTIENE LOGICOS",GETPROCE()
      // EJECUTAR("GETFIELDLOGICAL")
      // MensajeErr("AQUI ES LECTURA DE CAMPOS LOGICOS")

      ADDTABLE("0000","DPCAMPOS",oDp:cDsnConfig,.T.) // Evita quedarse en LOOP

      oDp:aLogico:=ASQL("SELECT RTRIM(CAM_TABLE),CAM_NAME FROM DPCAMPOS WHERE CAM_TYPE='L' ORDER BY CAM_TYPE",OPENODBC(oDp:cDsnConfig))
      AEVAL(oDp:aLogico,{|a,n| oDp:aLogico[n,1]:=ALLTRIM(a[1]),;
                               oDp:aLogico[n,2]:=ALLTRIM(a[2])})
   ENDIF

   // DEFAULT aLogico:=ASQL("SELECT RTRIM(CAM_TABLE),CAM_NAME FROM DPCAMPOS WHERE CAM_TYPE='L'")
   // ? LEN(aLogico)

   cTable   :=ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql)))
   cTable   :=ALLTRIM(IIF(" "$cTable ,LEFT(cTable,AT(" ",cTable)),cTable))
   cTable   :=STRTRAN(cTable,"(","")
   cTable   :=STRTRAN(cTable,CHR(10),"")
   cTable   :=STRTRAN(cTable,CHR(13 ),"")

   nAt   :=ASCAN(aTablesDb,{|aVal| aVal[2] == ALLTRIM(cTable) })
   nAt   :=IIF( nAt=0 , ASCAN(aTablesDb,{|aVal| aVal[2] == ALLTRIM(cTable) }) ,nAt)

   IF nAt>0 .AND. LEN(aTablesDb[nAt])>=9 .AND.  aTablesDb[nAt,9]!=NIL

      cSql:=STRTRAN(cSql+" "," "+ALLTRIM(UPPE(cTable))+" "," "+ALLTRIM(aTablesDb[nAt,9])+" ")
      cSql:=SETFIELDAS(cSql,aTablesDb[nAt,11])
      cDataBase:=aTablesDb[nAt,03]

      // oOdbc  :=OPENODBC(aTablas[nAt,3]) // Apertura del DSN
      //? "TABLAS COMPATIBLES",cTable,cSql
      // AEVAL(aTablas[nAt,11],{|a,i|cSql:=STRTRAN(cSql,a[2],a[1]+" AS "+a[2])})

   ENDIF

   IF oDp:lExcluye .AND. (nAt:=ASCAN(aExcluye,{|a,n|a[1]==cTable}),nAt>0)
      cSql:=BuildExcluye(cSql,aExcluye[nAt,2],aExcluye[nAt,3])
   ENDIF

   // 25/01/2025, solo si esta configurado, evitar lentitud
   IF oDp:lSaveSqlFile
     DpWrite("temp\query_"+STRTRAN(TIME(),":","")+"_"+alltrim(cTable)+".sql",cSql+CRLF+GETPROCE())
   ENDIF

RETURN MySqlTable():New(cSql,lArray,cDataBase,cTable,lChecktable,oDb)


CLASS MySqlTable

   DATA   aFields,aBuffer,aIsNull,aScanFields
   DATA   aFldPrimary INIT {}
   DATA   aDataFill

   DATA   oDataBase
   DATA   oTable

   DATA   cSql
   DATA   cTable
   DATA   cFileName,cSingular

   DATA   nOpen
   DATA   nRecno
   DATA   nFields
   DATA   hStmt    // Compatibilidad FWH TDBODBC
   DATA   nHandler
   DATA   lFound,lMySqlNativo,_lClosed
   DATA   lChecktable // No revisa tablas

   METHOD New( cSql , lArray , cDataBase , cTable ,lChecktable ,oDb ) CONSTRUCTOR

   METHOD GoTop()       INLINE ::use(),::oTable:GoTo( 1 ),::SetData()
   METHOD Eof()         INLINE ::use(),::oTable:Eof()
   METHOD DbSkip(nSkip) INLINE nSkip:=IIF( nSkip=NIL, 1 , nSkip ),;
                               ::use(),::oTable:Skip(nSkip),::SetData()

   METHOD Skip(nSkip)   INLINE ::DbSkip(nSkip)

   METHOD SetData()
   METHOD RecCount()      INLINE ::oTable:RecCount() // ::oTable:nRowCount
   METHOD Recno()         INLINE ::oTable:RecNo()
   METHOD nRecCount()     INLINE ::oTable:RecCount()
   METHOD GotoId(nRecno)
   METHOD FieldName(nField)
   METHOD FieldGet(nField)
   METHOD Refresh()       INLINE ::Use(),::oTable:Refresh()
   METHOD Use()
   METHOD SetOrder(cField)

   METHOD SeekBin(uValue,cField)
   METHOD CursorFields()  INLINE ::aFields
   METHOD Load()          INLINE ::aDataFill
   METHOD aFill()         INLINE ::aDataFill

   METHOD GetRow(aRow)


   METHOD FieldPos(cField)
   METHOD Find(nCol,uValue,lTop ) INLINE ::oTable:Find( nCol , uValue, lTop )

// ::nRecNo := if( n > ::nRowCount, ( ::lEof := .t., ::nRowCount ), n )


   METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                      WndHandleEvent( Self, nMsg, nWParam, nLParam )


   METHOD End()

ENDCLASS

METHOD New( cSql , lArray , cDataBase , cTable , lChecktable ,oDb) CLASS MySqlTable
  LOCAL nAt,oCursor,I,uValue ,cConvert:="",nT1:=SECONDS()

  DEFAULT lArray:=.F.

  ::oDataBase:=oDb

   // QUITAR

   IF !ValType(oDb)="O"
     // JN 074/09/2013
     MySqlStart()
     // ? "JN INNECESARIO"
   ENDIF

   IF !"SELECT "$cSql
       cSql:="SELECT * FROM "+cSql
   ENDIF

   nAt:=AT(" FROM ",cSql)

   ::_lClosed :=.F.
   ::cSql    :=cSql

   IF Empty(cTable)
     ::cTable   :=cTable(::cTable)
   ELSE
     ::cTable   :=cTable
   ENDIF


   ::cFileName   :=::cTable
   ::lFound      :=.F.
   ::lMySqlNativo:=.T.

   // Revisar Nombre de la tabla
   ::lChecktable :=lChecktable

//IF "VIEW_"$cTable
//    ? ::oDatabase:cName,"Nombre de la Vista",GETPROCE()
//ENDIF


   IF !Empty(cDataBase) .AND. ::oDataBase=NIL .AND. lChecktable // lChecktable 29/04/2020
      ::oDataBase:=MyOpenDataBase(cDataBase,.F.)
   ENDIF
/*


   ELSEIF ::oDataBase=NIL

      ::oDataBase:=MyOpenDataBase(::cTable ,.T.) // Busca la Conexion con la BD

   ENDIF

ENDIF
*/

      IF ::oDataBase=NIL
         ::oDataBase:=MyOpenDataBase(::cTable ,.T.) // Busca la Conexion con la BD
      ENDIF

   cDataBase  :=::oDataBase:cName

   ::oDatabase:Use()

//?  oDp:lSayTable

   DEFAULT oDp:cMemoSql:=""

   IF oDp:lSayTable
     oDp:cMemoSql:=oDp:cMemoSql + CRLF + ::oDataBase:cName+" "+::cTable+" DB:"+cDataBase+cSql+CRLF
     DpWrite("TRACERSQL.TXT",oDp:cMemoSql)
     oDp:oFrameDp:SetText(::oDataBase:cName+" "+::cTable+" DB:"+cDataBase+" SQL:"+cSql)
   ENDIF

   nT1:=SECONDS()

   ::Use() // 10/03/2015
   // ::oDataBase:Use() // JN 10/03/2015


//   oDp:oFrameDp:SetText("TMSTABLE "+::cTable)

   ::oTable:=TMSTable():New( ::oDataBase, ::cTable ,,  )  // ,"1" Basado en Array
   ::oTable:oDataBase:=::oDataBase
   ::Use()

   ::oTable:cStatement:=::cSql

   IF !::oTable:Open(cSql)

      IF ValType(::oDataBase)="O"
         MySqlStart()
         ::oDataBase:oConnect:oError:Show( .T. )
      ENDIF

      IF .T. //  11/02/2021 oDp:lSqlSayErr

         EJECUTAR("SQLMSGERR",cSql,::cTable,::oDataBase:cDsn,SECONDS()-nT1,GETPROCE(), oDp:cDpXbaseLine)

      ENDIF

      ::End()

      RETURN Self

   ENDIF

   ::aFields:= ACLONE(::oTable:aStruct)  // :( ::oTable:hMySt )

//   oDp:oFrameDp:SetText("TMSTABLE "+::cTable+" LEN "+LSTR(LEN(::aFields)))

   IF ASCAN(::aFields,{|a,n|Empty(a[1]) .OR. Empty(a[3])})>0
      MensajeErr("Versión de MySql Incompatible con "+oDp:cDpSys,"Contáctenos www.adaptaproerp.com")
      SALIR()
   ENDIF

   // ? ::oTable:lExist

   IF ::oTable:hMySt=0
      ? "Error Desde "+::ClassName( ),::cSql
   ENDIF

   // ::oTable:Read()

//   oDp:oFrameDp:SetText("MMXSTRUCT "+::cTable+" LEN "+LSTR(LEN(::aFields)))

   IF Empty(::aFields)
      ::aFields:= MMxStruct( ::oTable:hMySt )
   ENDIF

//   oDp:oFrameDp:SetText("POST MMXSTRUCT "+::cTable+" LEN "+LSTR(LEN(::aFields)))

   AEVAL(::aFields,{|a,n|::aFields[n,3]:=a[3] - IF( a[2]<>"N" .AND. a[3]>1 , 0 ,  a[4] ) ,;
                         ::aFields[n,3]:=a[3] - IF( a[2]="N" .AND. a[3]>1 .AND. a[4]=0 , 1 , 0) })


// ? "COñO AHORA SI"

   //::oTable:aBuffer := Array( ::oTable:FCount() )

   ::aFields     :=SetLogico(::aFields,::cTable)
   ::nFields     := LEN(::aFields)
   ::aBuffer     := Array(::nFields)
   ::aIsNull     := Array(::nFields)
   ::aScanFields := Array(::nFields)
   ::nOpen       := 0

//   oDp:oFrameDp:SetText("SELOGICOO"+::cTable+" LEN "+LSTR(LEN(::aFields)))

   // Rellena el Buffers
   AEVAL( ::aFields, { |a,i,uValue|  uValue:=CTOEMPTY(NIL ,::aFields[I,2] , ::aFields[I,3]) ,;
                                     AADD(::aBuffer , {uValue,::aFields[I,1],::aFields[I,2],::aFields[I,3]} )})
   IF lArray


//  IF oDp:lTracer
//    ErrorSys(.T.)
//  ENDIF

  //   oDp:oFrameDp:SetText("MSCURSOR "+::cTable+" LEN "+LSTR(LEN(::aFields)))

     oCursor:= TMSACursor():New( ::oTable ) // Sin datafields

     oCursor:Open()

   //  oDp:oFrameDp:SetText("POST-MSCURSOR "+::cTable+" LEN "+LSTR(LEN(::aFields)))

      ::aDataFill:=ACLONE(oCursor:Cursor)

//     oDp:oFrameDp:SetText("::aDataFill "+::cTable+" LEN "+LSTR(LEN(::aFields)))

     ::lFound:=!Empty(::aDataFill)

     IF ::lFound

//        oDp:oFrameDp:SetText("CTODXBASE "+::cTable)

       ::aDataFill:=CTOXBASE(::aDataFill,::aFields,::cTable)
/*
       FOR I=1 TO LEN(::aFields)

       IF ::aFields[I,2]="L"

         cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                 "aData[n,"+LSTR(I)+"]:=(a["+LSTR(I)+"]='1')"

       ENDIF

       IF ::aFields[I,2]="C"

         ::aDataFill[1,I]:=PADR( ::aDataFill[1,I],::aFields[I,3])

//         cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
//                 "aData[n,"+LSTR(I)+"]:=PADR(a["+LSTR(I)+"],"+;
//                 LSTR(::aFields[I,3])+")"

       ENDIF

       IF ::aFields[I,2]="N"

         cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                 "aData[n,"+LSTR(I)+"]:=ValToX(a["+LSTR(I)+"])"

       ENDIF

         IF ::aFields[I,2]="D"

           cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                  "aData[n,"+LSTR(I)+"]:=SQLTODATE(a["+LSTR(I)+"])"

         ENDIF

       NEXT

       IF !Empty(cConvert)
          xData(::aDataFill, scr_MacEval( "{|a,n|"+cConvert+"}"))
       ENDIF
*/
     ENDIF


     oCursor:Free()
     ::oTable:free()

 //oDp:oFrameDp:SetText("TMYSQL REGRESA "+::cTable)


     RETURN SELF

    ELSE

    ::oTable:Read()

    IIF( ::oTable:RecCount()>0 ,  ::oTable:xLoad() ,  NIL)

    AEVAL(::aFields,{|a,n|__objAddData( Self,   a[1] ),    ;
                           __objSendMsg(Self, a[1] , ::FieldGet( n )) } )

    ::Gotop()
    ::lFound:=(::Recno()>0)

   ENDIF

   ::nOpen:=LEN(aTableOpen)+1

   AADD( aTableOpen , {Self,::nOpen} )

   /*
   // No tiene Registros, Campos Lógicos, su Valor es .F.
   */
   IF Empty(::aDataFill)

       FOR I=1 TO LEN(::aFields)

         IF ::aFields[I,2]="L"
            __objSendMsg(Self, ::aFields[I,1] , .F.)
         ENDIF

       NEXT I

   ENDIF

RETURN Self

METHOD GotoID(nRecno) CLASS MySqlTable

     IF ::_lClosed
        RETURN NIL
     ENDIF

     nRecno:=MAX(nRecno,1)
     nRecno:=MIN(nRecno,::RecCount())

     // oDp:oFrameDp:SetText(::cTable+" / "+STR(nRecno)+" "+STR(::oTable:hMySt))

     ::oTable:Goto(nRecno)
     ::SetData()

RETURN NIL

METHOD SetOrder(cField) CLASS MySqlTable
   LOCAL nAt

//   ? cField,::cSql

   nAt:=AT(" ORDER BY ",::cSql)

   IF nAt>0
     ::cSql:=LEFT(::cSql,nAt+09)+" "+cField
   ENDIF

   // ? ::cSql

   ::oTable:cStatement:=::cSql

   IF ::oTable:Open(::cSql)
      ::oTable:Read()
     // ? "OK",::cSql
   ENDIF

RETURN .T.

/*
// Nombre del Campo
*/
METHOD FieldName(nField) CLASS MySqlTable

    // ? nField,"nField"

    IF ValType(nField)="C"
       nField:=::FieldPos(nField)
    ENDIF

    IF nField>LEN(::aFields)
       MensajeErr("Campo no Existe" , ::ClassName() )
       RETURN ""
    ENDIF

RETURN ::aFields[nField,1]

/*
// Revisa la BD
*/
METHOD use() CLASS MySqlTable

   DEFAULT ::oDataBase:=MyOpenDataBase(::cTable ,.T.) // Busca la Conexion con la BD

   // oDp:lTracer:=.t.
   // oDp:lTracer:=.t.

   IF oDp:lTracerSql
     // 13/12/2013 Aqui Genera Error y Sale Del Sistema
     // oDp:oFrameDp:SetText("DB:Actual="+::oDatabase:cName+" / DB:Server="+::oDatabase:oConnet:cHost+" Data:"+oDp:cDsnData+" Empresa="+oDp:cEmpresa)
   ENDIF

   IF !(oDp:oMySqlCon:oDataBase:cName==::oDatabase:cName)
      ::oDatabase:Use()
   ENDIF

RETURN .T.

METHOD GetRow(aRow) CLASS MySqlTable

RETURN (aRow:={},;
        AEVAL(::aFields,{|a,n| AADD(aRow,::FieldGet(n))}),;
        aRow)

METHOD SeekBin(uValue,cField) CLASS MySqlTable

   LOCAL nField :=::FieldPos(cField),nRec:=1,uData,bData,nLen:=0,nRecord:=0
   LOCAL nVeces :=0,nOldRec:=::Recno(),bWhile:={||.T.}
   LOCAL uValueOrg:=uValue
   LOCAL bCtoData:={|uValue|uValue},bExp,I
   LOCAL aFields :={}

   IF ","$cField

      aFields:=_VECTOR(cField)
      bExp   :=cTokenWhere(cField,uValue,::cTable,.F.)

      // ? uValue,"UVALUE"
      // ? bExp

      FOR I=1 TO LEN(aFields)

         //IF ValType(::FieldGet(aFields[I]))="D"
         //  bExp:=STRTRAN(bExp,aFields[I]+"=","oCursor:"+aFields[I]+"=")
         //ELSE
         bExp:=STRTRAN(bExp,aFields[I]+"=","oCursor:"+aFields[I]+"=")
         //ENDIF

      NEXT I

//    bExp:=STRTRAN(bExp,::cTable," oCursor")
      bExp:=STRTRAN(bExp," AND "," .AND. ")

      // ? bExp

      bExp:="{|oCursor|"+bExp+"}"
// ? bExp
      bExp:=BloqueCod(bExp)

      ::GoTop()

      // oDp:oCursor:=Self
      // ? ::TBR_TIPPER

      nRecord:=0
      nRec   :=0

      WHILE !::oTable:Eof()
         nRecord++
        // oDp:oCursor:=Self

         IF Eval(bExp,Self)
            nRec:=nRecord
            EXIT
         ENDIF

         ::DbSkip(1)

//         ::GotoId(nRecord)
//         ::oTable:Skip(1):Read()

      ENDDO

      // oDp:oCursor:=NIL

      IF nRec>0
        ::nRecno:=nRec
        ::GotoId(nRec)
        RETURN (nRec>0)
      ELSE
        ::GotoId(nOldRec)
      ENDIF

      RETURN .T.

   ENDIF

   IF nField=0
      MensajeErr("Campo "+cField + " no Existe ","Clase : "+::ClassName())
      RETURN .F.
   ENDIF

   bData :={|uData| uData=uValue  }

   IF ValType(uValue)="C"

      uValue:=ALLTRIM(uValue)
      nLen  :=LEN(uValue)

      bData :={|uData| ALLTRIM(uData)==uValue  }
      // bWhile:={|uData| uValue<=uData}

   ENDIF

   WHILE .T. // nVeces<2

     nRec   :=0
     nRecord:=0

     ::Gotop()

     uData:=::oTable:FieldGet(nField)

     IF ValType(uValue)="D" .AND. ValType(uData)="C"
        bCtoData:={|uValue|SQLTODATE(uValue)}
/*
        bCtoData:= {|v| ctod(Strzero(Bin2I(SubStr(v, 5, 2)),2)+"/"+; // jn
                        StrZero(Bin2I(SubStr(v, 3, 2)),2)+"/"+;
                        StrZero(Bin2I(SubStr(v, 1, 2)),4))}
*/
      //   ? "CTODATA",uData,EVAL(bCtoData,uData)
     ENDIF

     uData:=EVAL(bCtoData,uData)

     // ? uData,ValType(uData),cField,nField,uValue,ValType(uValue)

     // IF ValType(uData)="C" .AND. ValType(uData)="D"
     //   uValue:=SQLTODATE(uValue)
     // ENDIF
     // IF uValue<uData // Menor que el Primero
     //   EXIT
     // ENDIF

     WHILE !::oTable:Eof()

        uData:=::oTable:FieldGet(nField)
        uData:=EVAL(bCtoData,uData)

        // ? uData,ValType(uData),cField,nField,uValue,ValType(uValue)

        IF ValType(uData)<>ValType(uValue)
           ? "Error SEEKBIN", uData,uValue,nField,GetProce()
           ::oTable:gobottom()
           RETURN .F.
        ENDIF


        IF uData>uValue
           EXIT
        ENDIF

        nRecord++
        // ::GotoId(nRecord)
        // uData:=::oTable:FieldGet(nField)

        IF Eval(bData,EVAL(bCtoData,::oTable:FieldGet(nField)))
           nRec:=nRecord
           EXIT
         ENDIF

         // ::oTable:Skip(1):Read()
         ::DbSkip(1)

       ENDDO

       IF nRec>0
         ::GotoId(nRec)
         RETURN (nRec>0)
       ELSE
         ::GotoId(nOldRec)
       ENDIF

       IF ValType(uValue)<>"C"
         ::GotoId(nRec)
         RETURN .F.
       ENDIF

       IF nVeces=0 .AND. !Empty(uValue)
         uValue:=LEFT(uValue,Len(uValue)-1)
         nLen  :=LEN(uValue)
         // ? "AHORA BUSCAMOS",uValue,nLen
         LOOP
          ENDIF

       IF Empty(uValue) .AND. nVeces=0
         uValue:=ALLTRIM(uValueOrg)
         nLen  :=LEN(uValue)
         bData :={|uData|LEFT(uData,nLen)==uValue }
         LOOP
       ENDIF

       nVeces++

       EXIT

/*
       IF nVeces=1
         uValue:=uValueOrg
         bData :={|uData,nLen|nLen:=ALLTRIM(uData),uData==LEFT(uValue ,nLen)) }
         // uValue:=LEFT(uValue,Len(uValue)-1)
         // nLen  :=LEN(uValue)
         LOOP
       ENDIF
*/
    ENDDO


RETURN nRec>0


METHOD FieldGet(nField)
   LOCAL cField:=nField,uValue

   IF ValType(nField)="C"

      nField:=ASCAN(::aFields,{|a,n|a[1]:=nField})

      IF nField=0
         MensajeErr("Campo "+cField+ " no Existe en "+::cSql,::ClassName())
      ENDIF

   ENDIF

   IF ::oTable:RecCount()=0

      uValue:=CTOO("",::aFields[nField,2])

      IF ::aFields[nField,2]="C"
         uValue:=PADR(uValue,::aFields[nField,3])
         RETURN uValue
      ENDIF

      uValue:=::oTable:xFieldGet(nField)

      IF ::aFields[nField,2]="L" .AND. ValType(uValue)="N" // Logico
        uValue:=(uValue=1)
      ENDIF

      RETURN uValue

   ENDIF

   IF ::_lClosed
      //? nField,::aFields[nField,1]
      RETURN __objSendMsg(Self,::aFields[nField,1])
   ENDIF

   uValue:=::oTable:xFieldGet(nField)

   // ? ::aFields[nField,2],uValue // Fecha

   IF ::aFields[nField,2]="D" .AND. ValType(uValue)<>"D" // Fecha
     uValue:=SQLTODATE(uValue)
   ENDIF

   IF ::aFields[nField,2]="L" .AND. ValType(uValue)="N" // Logico
     uValue:=(uValue=1)
   ENDIF

//   IF ::aFields[nField,3]=1 .AND. ::aFields[nField,3]
//   ENDIF

RETURN uValue


METHOD FieldPos(cField)

     IF Valtype(cField)="N"
        RETURN cField
     ENDIF

     cField:=ALLTRIM(UPPE(cField))

RETURN ASCAN(::aFields,{|aVal| ALLTRIM(UPPE(aVal[1])) == cField })

/*
// Asigna los Datos
*/
METHOD SetData() CLASS MySqlTable

   IF ::_lClosed .OR. ::oTable:RecCount()=0 //.OR. ::_lClosed
      RETURN NIL
   ENDIF

   ::nRecno:=::oTable:RecNo()

   ::oTable:Read()

   //? ::nRecno,::oTable:xFieldget(1)
   // __objSendMsg(Self, ::aFields[1,1] ,::oTable:xFieldget(1))

   AEVAL(::aFields,{ | a , n | __objSendMsg(Self, a[1] , ::oTable:xFieldGet( n )) } )

RETURN NIL

METHOD End(lEnd) CLASS MySqlTable

   LOCAL nAt:=ASCAN(aTableOpen,{|a,n| a[2]=::nOpen})

   DEFAULT lEnd:=.F.

   // ::oCursor:Freen()

   IF nAt>0

//    ? aTableOpen[nAt,1]:ClassName(),nAt
     aTableOpen[nAt,1]:oTable:Free()
     aTableOpen:=ARREDUCE(aTableOpen ,nAt)

   ENDIF

   IF lEnd
     ::aFields    :={}
     ::aBuffer    :={}
     ::aIsNull    :={}
     ::aScanFields:={}
     ::aFldPrimary:={}
     ::aDataFill  :={}
   ENDIF

RETURN NIL

FUNCTION MyOpenDataBase(cTable,lTable,oServer,lSelect)
   LOCAL nAt,cDataBase,oDataBase

   DEFAULT oServer:=oDp:oMySqlCon,;
           lTable :=.T.,;
           lSelect:=.F.

   IF oServer=NIL
      MySqlStart()
   ENDIF

   oDp:lViewArrayMdi:=.F.

   IF lTable

     aTablesDb:=GetTables(.T.)

     nAt    :=ASCAN(aTablesDb,{|aVal| aVal[2] == ALLTRIM(cTable) })

     IF nAt=0
       MensajeErr("Tabla no Existe "+cTable,"MyOpenDataBase")
       RETURN NIL
     ENDIF

     cDataBase:=ALLTRIM(aTablesDb[nAt,3]) // Nombre de la Base de Datos
     cDataBase:=iif( Empty(cDataBase),  oDp:cDsnData , cDataBase)

  ELSE

     cDataBase:=cTable

  ENDIF

  cDataBase:=ALLTRIM(UPPE(cDataBase))

  nAt:=ASCAN(aDataBase,{|a,n|a[1]==cDataBase})

  IF nAt>0

      oDataBase:=aDataBase[nAt,2]

      IF !oDataBase:oConnect:lConnected
         // MensajeErr("aqui MySQL no esta conectado")
         oDataBase:oConnect:ReConnect()
      ENDIF

      oDataBase:Use()
      oDataBase:Select()


      // JN 04/04/2018  Evitar cerrar el sistema por usuario dormido
      __objAddData(oDataBase, "nSeconds" )
      __objSendMsg(oDataBase, "nSeconds",SECONDS())

      __objAddData(oDataBase, "nTimeMax" )
      __objSendMsg(oDataBase, "nTimeMax",5*(60*60))

/*
      IF cTable="DPBOTBAR"  // .AND. cDataBase="DPADMWIN"
         ErrorSys(.T.)
         ViewArray(aTablesDb)
         ViewArray(aDataBase)
         ViewArray(oDataBase:GetTables())
         ? cTAble,cDataBase,aDataBase[nAt,1],oDataBase:cName,aDataBase[nAt,2]:ClassName() ,nAt // Nombre de la Base de Datos
     ENDIF
*/
     RETURN oDataBase

  ENDIF

  // cDataBase:=UPPE(cDataBase)
  // ? cDataBase

  // oDp:oFrameDp:SetText(oDp:cIp+"MYSQLSTART")

  // JN 14/09/2016 oServer:ReConnect()

  // Si no Existe, se crea la DB
  oDp:lNewDb:=.T.

  IF !oServer:ExistDb(cDataBase)
     oServer:CreateDB(cDataBase)
     oDp:lNewDb:=.T.
//   MensajeErr("aqui se Crea la Base de Datos y  oDp:lNewDb:=.T.  "+cDataBase+CRLF+GetProce())
  ENDIF

// 13/09/2016
// ADD_DSN(cDatabase,NIL,NIL,NIL,NIL,oServer:cHost,cDataBase)
// ? oServer:cHost,"AQUI ES oServer MyOpenDataBase(cTable,lTable,oServer)",oServer:ClassName(),cDatabase,lSelect,GETPROCE()

  oDataBase:=TMSDataBase():New( oServer, cDataBase, lSelect )

  IF !Valtype(oDataBase)="O"
    RETURN NIL
  ENDIF

//    ? oDataBase:ClassName(),oServer:Classname(),cDataBase
  oDataBase:Use()
  oDataBase:Select()

  DEFAULT aDataBase:={}

  AADD(aDataBase,{cDataBase,oDataBase})

  oDp:aDataBase:=aDataBase // Cantidad de Bases de Datos Abiertos

RETURN ATAIL(aDataBase)[2]


FUNCTION MyCloseAll()

   aTableOPen:={}

   AEVAL(aTableOpen,{|a,n| a[1]:oTable:Free()})

   aTableOpen:={}

RETURN .T.

EXIT PROCEDURE MySqlCloseDataBase()

   MySqlCloseAll()

   // ? ValType(oDp:oMySqlCon)

   IF ValType(oDp:oMySqlCon)="O"
      oDp:oMySqlCon:Close()
   ENDIF

   oDp:oMySqlCon:=NIL

RETURN

FUNCTION MyCloseDataBase(cDatabase,lDelete)
   LOCAL nAt

   DEFAULT lDelete:=.T.
// ? "AQUI MYCLOSEDATABASE",cDataBase,GETPROCE(),oDp:cDpXbaseLine

   IF Empty(aDatabase)
      RETURN .F.
   ENDIF

   nAt:=ASCAN(aDataBase,{|a,n|a[1]=cDatabase})

   IF nAt>0

      aTablesDb:={}
      aDataBase[nAt,2]:End()
      aDataBase[nAt,2]:Destroy()


      IF lDelete
        aDataBase:=ARREDUCE(aDataBase,nAt)
      ENDIF

   ENDIF

RETURN nAt>0

FUNCTION MySqlCloseAll()

   DEFAULT aDataBase :={},;
           aTableOPen:={}

   MyCloseAll()

   AEVAL(aDataBase ,{|a,n| a[2]:End(),a[2]:Destroy()})

   SysRefresh()
   aTablesDb:={}
   aDataBase:={}

RETURN NIL

FUNCTION MyGetDataBases()
RETURN aDataBase

FUNCTION MySqlCursor(cSql)
/*
  LOCAL oTAble

  oTable:= TMSTable():New( oDp:oMySqlAdm, "DPINV" )
  oTable:cStatement:="SELECT * FROM DPINV ORDER BY INV_CODIGO"

  oCursor:= TMSACursor():New( oTable ) // Sin datafields
  oCursor:Open()
  aData:=oCursor:Cursor
  ? oTable:ClassName()	, SECONDS()-T1,LEN(aData)
*/
RETURN .T.

/*
// Cursor en Arreglo
*/
FUNCTION My_SqlArray(cSql)
   LOCAL oCursor,aData,oTable
   LOCAL nAt,cTable,oDataBase

   cTable   :=ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql)))
   cTable   :=ALLTRIM(IIF(" "$cTable ,LEFT(cTable,AT(" ",cTable)),cTable))

   oDataBase:=MyOpenDataBase(cTable)

   oTable:=TMSTable():New( oDataBase, cTable )

   oCursor:= TMSACursor():New( oTable ) // Sin datafields
   oCursor:Open()
   aData:=ACLONE(oCursor:Cursor)
   oCursor:Free()
   oTable:free()

RETURN aData

FUNCTION MySqlStart(lError)
   LOCAL lHayError:=.F. ,aFiles:={}

   DEFAULT lError:=.T.


   IF ValType(oDp:oMySqlCon)="O"

 // JN 17/07/2018
        oDp:oMySqlCon:oError:lAutoError:=lError

 //     oDp:oFrameDp:SetText(LSTR(oDp:oMySqlCon:oError:GetErrNo()))

      IF oDp:oMySqlCon:oError:GetErrNo()=2003 .AND. UPPE(oDp:cIp)="LOCALHOST"
         lHayError:=.T.
         LMKDIR("MYSQL")
         MsgRun("Instalando Servicio","Por Favor Espere",{||WaitRun("MYSQL\MYSQLSTART.BAT",0)})
      ENDIF

      IF oDp:oMySqlCon:oError:GetErrNo()=2013
        // oDp:oMySqlCon:Close() //03/03/2016
         oDp:oMySqlCon:ReConnect()
         // oDp:oFrameDp:SetText("MySql Reiniciado")
      ENDIF
/*
      IF !lHayError .AND. !Empty(oDp:oMySqlCon:oError:GetErrNo())
         oDp:oMySqlCon:oError:lAutoError:=.F.
         MensajeErr(oDp:oMySqlCon:oError:Show())
      ENDIF
*/

      IF !lHayError .AND. !oDp:oMySqlCon:lConnected
         // oDp:oMySqlCon:ReConnect()
         lHayError:=.T.
      ENDIF

      IF !lHayError
        RETURN .T. // oDp:oMySqlCon
      ENDIF

      // MySqlCloseAll()
      oDp:oMySqlCon:Close()
      oDp:oMySqlCon:=NIL

//? "AQUI SE CIERRA MYSQL",oDp:cDpXbaseLine

   ENDIF


   IF Empty(oDp:cIp)

      RESTOREMYSQLMEM()

      IF Empty(oDp:cIp)
        MensajeErr("Es necesario indicar DIRECCION IP, function mysqlstart(), clase TMYSQL"+CRLF+GETPROCE())
        SALIR()
      ENDIF

   ENDIF

   oDp:cIp      := ALLTRIM(oDp:cIp   )
   oDp:cLogin   := ALLTRIM(oDp:cLogin)
   oDp:cPass    := ALLTRIM(oDp:cPass )

   CursorWait()

   oDp:oMySqlCon:= TMSConnect():New()   // Inicia el objeto Conexion

 // lError:=.F.

  // ? oDp:oMySqlCon,"oDp:oMySqlCon, AQUI ES",lError,oDp:cIp,oDp:cLogin ,oDp:cPass,oDp:nPort,lError,GETPROCE()

   oDp:oMySqlCon:SetAutoError( lError )

   // Asignación dinamica de classdata
   // ? "POR AHORA QUITAMOS EL TIEMPO DE DORMIR"


   __objAddData(oDp:oMySqlCon, "nSeconds" )
   __objSendMsg(oDp:oMySqlCon, "nSeconds",SECONDS())

    __objAddData(oDp:oMySqlCon, "nTimeMax" )
   __objSendMsg(oDp:oMySqlCon, "nTimeMax",5*(60*60))

   __objAddData(oDp:oMySqlCon, "lRemote" )
   __objSendMsg(oDp:oMySqlCon, "lRemote",.F.)


//? oDp:oMySqlCon:Connect( oDp:cIp, oDp:cLogin , oDp:cPass , NIL, IF( oDp:nPort=3306 , NIL , oDp:nPort  ))

  // IF oDp:oMySqlCon:Connect( oDp:cIp, oDp:cLogin , oDp:cPass , NIL, IF( oDp:nPort=3306 , NIL , oDp:nPort  ))
  IF oDp:oMySqlCon:Connect( oDp:cIp, oDp:cLogin , oDp:cPass ,NIL, IF( oDp:nPort=3306 , NIL , oDp:nPort  )) // Indicamos la BD para envitar incidencias

//? "SI CONECTO, PARA RECONECTAR REQUIERE BD USADA"

      oDp:oMySqlCon:Reconnect() // QUITAR POR AHORA
      oDp:oMySQL:=oDp:oMySqlCon

// ? "SI CONECTO"

   ELSE
      oDp:oMySqlCon:Close()
      MensajeErr( "No hay conexion con "+oDp:cIp )
      oDp:oMySqlCon:=NIL
      RETURN .F.
   ENDIF

   IF !oDp:oMySqlCon:ExistDb(oDp:cDsnConfig)
      oDp:oMySqlCon:CreateDB(oDp:cDsnConfig)
      ferase(oDp:cDsnConfig+".chk")
      ferase("DPWIN32.CHK")
   ENDIF

   IF !oDp:oMySqlCon:ExistDb(oDp:cDsnData)
      oDp:oMySqlCon:CreateDB(oDp:cDsnData)
      oDp:lCrearTablas:=.T.
      ferase(oDp:cDsnData+".chk")
      ferase("DPWIN32.CHK")
   ENDIF

  // oDp:oMySqlCon:SetAutoError( .F. )
  // ? "aqui se Conecta con mysql Server",GETPROCE(),oDp:cDpXbaseLine,"APAGAMOS oDp:oMySqlCon:SetAutoError( .F. )"

RETURN .T. // oDp:oMySqlCon
/*
// Asigna Campos Lógicos
*/
FUNCTION SetLogico(aFields,cTable)
//   LOCAL nAt
/*
RETURN {}

   cTable:=UPPER(cTable)
   nAt:=ASCAN(oDp:aLogico,{|a,n|a[1]==cTable})

? nAt,cTable,"cTable"

   IF nAt>0
      RETURN oDp:aLogico[nAt,2]
   ENDIF

RETURN {}
*/

   LOCAL nAt,I,n

   IF !Empty(cTable)
     cTable:=UPPER(ALLTRIM(cTable)) // Requiere Comparacion ==
   ENDIF

   // nAt   :=ASCAN(oDp:aLogico,{|a,n|a[1]==cTable})
   // IF nAt=0
   //    RETURN aFields
   // ENDIF

   DEFAULT oDp:aLogico:={}

   IF Empty(oDp:aLogico)
//      oDp:aLogico:=ASQL("SELECT RTRIM(CAM_TABLE),CAM_NAME FROM DPCAMPOS WHERE CAM_TYPE='L' ORDER BY CAM_NAME")
   ENDIF

   // JN 19/03/2014
   // n:=ASCAN(aFields,{|a,n|  a[3]=1 .AND. a[2]="N" })
   n:=ASCAN(aFields,{|a,n|  a[4]=0 .AND. a[2]="N" }) // Busca sin Decimales

   IF n=0
     n:=1 // RETURN aFields
   ENDIF

   n:=1
   FOR I:=n TO LEN(aFields)

      IF aFields[I,3]=1 .AND. aFields[I,2]="N"

         // IF aFields[I,4]=0 .AND. aFields[I,2]="N"  // Solo compara sin Decimales JN 01/06/2016

         // 17/11/2020
         // IF "LOGIC"$aFields[I,1] // Campos Declarados como logicos en la sentencia SELECT 0 AS LOGICO,LOGICO1,LOGICO2
         IF Empty( oDp:aLogico )
           nAt:=1 // Todos son Logicos
         ELSE
           // JN 20/03/2018 Precisa la Busqueda de cTable
           nAt:=ASCAN(oDp:aLogico,{|a,n|ALLTRIM(a[2])==ALLTRIM(aFields[I,1])}) // 01/06/2016 Debe Precisar la Tabla a[1]==cTable, si aplica este criterio las tablas inner join no va a convertir
         ENDIF

         IF nAt>0
            aFields[I,2]="L"
         ENDIF

      ENDIF

   NEXT


   /*
   // JN 01/06/2016, Campos Logicos con Mayor Longitud a 1
   */

/*
   FOR I:=1 TO LEN(aFields)

      IF aFields[I,2]="N"

         // nAt:=ASCAN(oDp:aLogico,{|a,n|a[1]==cTable .AND. a[2]=aFields[I,1]})
         nAt:=ASCAN(oDp:aLogico,{|a,n| ALLTRIM(a[1])==cTable .AND. a[2]=aFields[I,1]})

         // IF cTable=="DPPCLOG"
         //    ? cTable,nAt,aFields[I,1]
         // ENDIF

         IF nAt>0
            aFields[I,2]="L"
            aFields[I,3]=1
         ENDIF

      ENDIF

   NEXT
*/

RETURN aFields

FUNCTION MYSQLGET(cTable,cField,cWhere,lMsg,bRun)
   LOCAL cSql,oTable,uValue

   DEFAULT lMsg:=.F.,cWhere:="",bRun:={||NIL}

   bRun:=BloqueCod(bRun)

   IF !Empty(cWhere)
      cWhere:=IIF( " WHERE "$cWhere , " " , " WHERE " )+cWhere
   ENDIF

   oDp:nRecCount:=0 // Registros Leidos

RETURN SQLGET(cTable,cField,cWhere,lMsg,bRun)
/*
   IF !oDp:lNativo .OR. oDp:cTypeBD!="MYSQL"
     RETURN SQLGET(cTable,cField,cWhere,lMsg,bRun)
   ENDIF

   cSql      :="SELECT "+cField+" FROM "+cTable+cWhere

   oDp:aQuery:={}
   oDp:aRow  :={}
   oDp:cSql  :=cSql

   //? cSql,"SQLGET"
   oTable:=MyTable(cSql)

   IF !Empty(oTable:aFields) // ChkSql(cSql)
     uValue       :=oTable:Fieldget(1)
     oDp:nRecCount:=oTable:RecCount()
     oDp:aRow     :=oTable:GetRow()
     IF !Empty(oDp:aRow)
        uValue:=oDp:aRow[1]
     ENDIF
     // ? uValue,LEN(oDp:aRow)
     //oDp:aQuery:=ACLONE(oTable:aDataFill)
     //IF !Empty(oTable:aDataFill)
     // oDp:aRow  :=ACLONE(oTable:aDataFill[1])
     //ENDIF
   ELSE
     lMsg:=.T.
   ENDIF

   IF lMsg
     CLPCOPY(cSql)
     MemoWrit("ERROR.SQL",cSql)
     MensajeErr(cSql+CRLF+GetProce(),"SQLGET()")
   ENDIF

   oTable:End()

   IIF( ValType(bRun)="B" , Eval(bRun,uValue) , NIL )

RETURN uValue
*/


/*
// Cuenta la Cantidad de Registros
*/
FUNCTION MYCOUNT(cTable,cWhere)
   LOCAL nCount

   DEFAULT cWhere:=""

   cWhere:=" "+cWhere

   IF !Empty(cWhere) .AND. !" WHERE "$cWhere
      cWhere:=" WHERE "+cWhere
   ENDIF

   nCount:=SQLGET(cTable,"COUNT(*)",cWhere)
   nCount:=CTOO(nCount,"N")

RETURN nCount

/*
// Lee la Estructura de Datos
*/
FUNCTION MyStruct(cSql,aStruct)

   LOCAL nAt,oTable,I,cTable,aFields:={},oDataBase

   IF Empty(cSql) // Hacer Release
      aStruct:={}
      RETURN {}
   ENDIF

   DEFAULT aStruct:={}

   IF !"SELECT "$cSql
       cSql:="SELECT * FROM "+cSql
   ENDIF

   nAt:=AT(" FROM ",cSql)

   cTable   :=ALLTRIM(SUBS(cSql,AT(" FROM ",UPPE(cSql))+5,LEN(cSql)))
   cTable   :=ALLTRIM(IIF(" "$cTable ,LEFT(cTable,AT(" ",cTable)),cTable))

   IF " * "$cSql  .AND. (nAt:=ASCAN(aStruct,{|a,n|a[1]=cTable}), nAt>0)
      RETURN aStruct[nAt,2]
   ENDIF

   oDataBase:=MyOpenDataBase(cTable) // Busca la Conexion con la BD

   IF !oDataBase:ExistTable( cTable )
      RETURN aStruct
   ENDIF

   oTable:=TMSTable():New( oDataBase, cTable )

   oTable:cStatement:=cSql

   IF !oTable:Open(cSql)

      oDataBase:oError:Show( .T. )
      MensajeErr(cSql+CRLF+oDataBase:ClassName()+CRLF+GetProce(),"NO ABRIO")

   ELSE

      // aFields:= ACLONE(oTable:aStruct)  // :( ::oTable:hMySt )
      // ViewArray(aFields)

//      IF Empty(aFields) .OR. .T.
        aFields:= MMxStruct( oTable:hMySt )
//        ViewArray(aFields)
//      ENDIF

      // ? LEN(oTable:aStruct),"MYSTRUC",LEN(aFields)

      IF " * "$cSql
        AADD(aStruct,{cTable,aFields})
      ENDIF

   ENDIF

   oTable:Free()

RETURN aFields


FUNCTION ISMYSQLGET(cTable,cField,uValue,bRun)
/*
  LOCAL nT1:=Seconds()
  LOCAL uResp:=MYSQLGET(cTable,cField,cField+GetWhere("=",uValue))
  LOCAL lFound:=oDp:nRecCount>0
*/
RETURN MYCOUNT(cTable,cField+GetWhere("=",uValue))>0
/*
  DEFAULT bRun:={||NIL}

  bRun:=BloqueCod(bRun)

  IF ValType(uValue)="C"
     uValue:=ALLTRIM(uValue)
     uResp :=ALLTRIM(uResp)
  ENDIF

  EVAL(bRun,lFound) // uResp==uValue)

  // ? uResp,uValue,CLPCOPY(oDp:cSql)

RETURN lFound // (uResp==uValue)
*/

/*
FUNCTION AMYSQL(cSql,cDataBase)
   LOCAL oCursor,aData

   oCursor:=MyTable(cSql,.T.,cDataBase)
   aData:=ACLONE(oCursor:aDataFill)
   oCursor:End()

RETURN aData
*/

/*
// Ejecutar SQL MySql
*/
FUNCTION MYEXECUTE(cTable,cSql,oDataBase,lError)

   DEFAULT oDataBase:=MyOpenDataBase(cTable)

   oDataBase:Use()

RETURN oDataBase:ExecSQL( cSql,lError )

/*
// Carga las Tablas
*/
FUNCTION MYLOADTABLES()
   LOCAL aTablas:={}

   MySqlStart(.T.) // Revisa la Conexión

   AADD(aTablas,{"0001","DPTABLAS",oDp:cDsnConfig ,"","",NIL,"TABLAS","Tablas",NIL,.T.})

// ? "AQUI DEBE LEER LAS TABLAS",LEN(aTablas)

RETURN aTablas

STATIC FUNCTION xData()
// aData,bBlq)

    PARAM aData,bBlq

   //bBlq:=scr_MacEval(bBlq)

   AEVAL(aData,bBlq)

RETURN NIL

FUNCTION VALTOX(uValue,cTable)

   IF ValType(uValue)="U"
     RETURN 0
   ENDIF

   IF ValType(uValue)<>"C"
      MensajeErr(ValType(uValue),"UVALUE")
   ENDIF

RETURN VAL(uValue)

FUNCTION CTOXBASE(aDataFill,aFields,cTable)
  LOCAL I,cConvert:=""

  FOR I=1 TO LEN(aFields)

       IF aFields[I,2]="L"

         cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                 "aData[n,"+LSTR(I)+"]:=(a["+LSTR(I)+"]='1')"

       ENDIF

       IF aFields[I,2]="C"

//         aDataFill[1,I]:=PADR( aDataFill[1,I],aFields[I,3])
//        "aData[n,"+LSTR(I)+"]:=IF(Empty(a["+LSTR(I)+"],[],a["+LSTR(i)+"])"

         cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                 "aData[n,"+LSTR(I)+"]:=IF(Empty(a["+LSTR(I)+"]),[],a["+LSTR(i)+"]),"+;
                 "aData[n,"+LSTR(I)+"]:=PADR(a["+LSTR(I)+"],"+;
                  LSTR(aFields[I,3])+")"

//? cConvert,"cConvert"

       ENDIF

       IF aFields[I,2]="N"

         cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                 "aData[n,"+LSTR(I)+"]:=ValToX(a["+LSTR(I)+"])"

       ENDIF

       IF aFields[I,2]="D"

           cConvert:=cConvert + IF( Empty(cConvert) , "" , "," )+;
                  "aData[n,"+LSTR(I)+"]:=SQLTODATE(a["+LSTR(I)+"])"

         ENDIF

    NEXT


/*
    IF "JOR_"$aFields[1,1]
? cConvert,"cConvert"
       ViewArray(aFields)
       ViewArray(oDp:aLogico)
    ENDIF
*/

    IF !Empty(cTable) .AND. oDp:lSaveSqlFile
       DPWRITE("TEMP\CTOXBASE_"+cTable+".TXT",cConvert)
    ENDIF

    IF !Empty(cConvert)
       xData(aDataFill, scr_MacEval( "{|a,n|"+cConvert+"}"))
    ENDIF

    // Debe Revisar los valores NULL en los CHAR de left join no genere incidencias en el browse cuando se ordena

    FOR I=1 TO LEN(aFields)

    NEXT I

RETURN aDataFill

/*
// Devuelve un Arreglo desde Consulta SQL, Con o sin la conversión Dpxbase
*/
FUNCTION AQUERY(cSql,oDb,lXbase,aFields)
   LOCAL cTable:=cTable(cSql),aData:={}
   LOCAL oCursor

   DEFAULT lXBase:=.T.,;
           oDb:=GetOdbc(cTable)

   oDb:Use()

   oDp:cSql:=cSql

   oCursor:=TMSQuery():New(oDb)

   IF oCursor:Open(cSQL)

     aFields:= MMxStruct( oCursor:hMySt )
     aData  :=oCursor:FillCursor()

   ELSE

     aData  :={}
     lXBase :=.F.

   ENDIF

   IF lXbase
     aFields:= SETLOGICO(aFields,cTable)
     aData  :=CTOXBASE(aData,aFields)
   ENDIF
   // oCursor:Free()
   // oCursor:Destroy()
   oCursor:Close()

RETURN aData

FUNCTION RESTOREMYSQLMEM()
    LOCAL cFileMem:="MYSQL.MEM"

    PRIVATE _MycIp    :="",;
            _MycPass  :="",;
            _MycLoging:="",;
            _MySqlDate:=""
            _MySqlPort:=0

    IF !FILE(cFileMem)
       MensajeErr("Es necesario Archivo "+cFileMem)
       SALIR()
    ENDIF

    REST FROM (cFileMem) ADDI

    oDp:cIp     :=PADR(ENCRIPT(_MycIp    ,.F.),40)
    oDp:cPass   :=PADR(ENCRIPT(_MycPass  ,.F.),40)
    oDp:cLogin  :=PADR(ENCRIPT(_MycLoging,.F.),40)
    oDp:cSqlDate:=PADR(ENCRIPT(_MySqlDate,.F.),40)
    oDp:nPort   :=_MySqlPort

RETURN NIL

FUNCTION MYSQLCHKCONN(lRun,lSay)
RETURN EJECUTAR("MYSQLCHKCONN",lRun,lSay,aDataBase)
/*
  LOCAL I,oCon,aLine:={}
  LOCAL cText:=""

  DEFAULT lRun:=.F.,;
          lSay:=.F.

  FOR I=1 TO LEN(aDataBase)

   oCon:=aDataBase[I,2]:oConnect

//   IF lSay
//     // ? oCon:nTimeM   ax,oCon:nSeconds,ABS(Seconds()-oCon:nSeconds) >= (oCon:nTimeMax),"oCon:nTimeMax,ABS(Seconds()-oCon:nSeconds) >= (oCon:nTimeMax)"
//   ENDIF

   IF oCon:nTimeMax>0 .AND. ABS(Seconds()-oCon:nSeconds) >= (oCon:nTimeMax) .OR. lRun

     MsgRun("ReConectando con BD "+ aDataBase[I,1])
     CLOSEODBC(aDataBase[I,1],.F.)
     MyOpenDataBase(aDataBase[I,1],.F.):oConnect:ReConnect()

   ENDIF

   aDataBase[I,2]:oConnect:nSeconds:=Seconds()

  NEXT I

RETURN .T.
*/

/*
// 13/09/2016 Agregar BD en Arreglo aDatabase para refrescar sus conexiones
*/
FUNCTION MYADD_DATABASE(oDataBase)
  LOCAL nAt,cDataBase

  cDataBase:=oDataBase:cName

  nAt:=ASCAN(aDataBase,{|a,n|a[1]==cDataBase})

  IF nAt=0
     AADD(aDataBase,{cDataBase,oDataBase}) //:oConnect})
  ENDIF

RETURN aDataBase

/*
// Cerrar la BD
*/
// FUNCTION MYCLOSEDB(cDataBase)
// RETURN NIL

// EOF
