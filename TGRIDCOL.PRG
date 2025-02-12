*
// Columna de Cada  Grid
*/
#include "FiveWin.ch"
#include "InKey.ch"
#include "xbrowse.ch"
#include "\DWH\LIBS\TSBUTTON.CH"
#include "Obj2Hb.ch"

MEMVAR oDp,oHead,oGrid // Validación de la Cabeza

CLASS TDOCGRIDCOL

   DATA uOldValue

   DATA oGrid
   DATA oDb
   DATA oDpLbx
   DATA oWnd
   DATA oCol       // oCol DEL BROWSE 19/08/2023
   DATA aItems     // Data que Muestra
   DATA aItemsData // Data que Devuelve

   DATA cField
   DATA cType
   DATA cPicture
   DATA cSayPicture
   DATA cTitle
   DATA cMsgValid
   DATA cListBox
   DATA cWhereListBox
   DATA cTitleListBox
   DATA cFieldItem // Campo Item del Grid
   DATA uValueFind
   DATA uDefault   // En caso de esta Vacio, toman este Valor
   DATA cFooter
   DATA cMemoRun INIT "" // Programa Ejecución DpXbase por Columna


   DATA uValue

   DATA nEditType
   DATA nDataStrAlign  // Alineación de Datos
   DATA nHeadStrAlign  // Alineación Encabezado
   DATA nKeyEdit       // Tecla para Edición
   DATA nWidth
   DATA nCol
   DATA nTotal
   DATA nItemAdd
   DATA nPos INIT 0

   // Color de la Columna
   DATA nClrPaneH
   DATA nClrTextH

   DATA bValid
   DATA bWhen
   DATA bWhenGetValue
   DATA bWhenGetCol   // Solo se Aplica cuando se Edita la Columna
   DATA bAction       // si no es NIL, el Get es un Boton
   DATA bCalc         // Determina el Valor de si Mismo
   DATA bExecute      // Ejecuta una Acción en Forma Cíclica
   DATA bPostEdit     // PostEdición
   DATA bListBox
   DATA bFind
   DATA bRunOff       // Ejecución por Columna Cuando esta OFF
   DATA bEditBlock    // Ejecución Directa del Boton
   DATA bStrData      // Visualización de Columna

   DATA bClrHeader,;    // default color pair for header
        bClrFooter,;    // default color pair for footer
        bClrStd,;       // default color pair for normal rows
        bClrSel,;       // default color pair for selected row
        bClrSelFocus,;  // default color pair for selected row when control has focus
        bClrEdit        // default color pair for edition

   DATA lRepeat       // Repite el Valor Anterior
   DATA lChange       // Cambio
   DATA lValid        // Indica si Ejecuta la Validacion F6, Repite VALID Y POSTEDIT
   DATA lPrimary      // Indica si la Columna es Clave Primaria
   DATA lTotal        // Genera Totales
   DATA lMaxSize      init .F. // Obtiene el Mayor Tamaño Posible
// DATA lRepeat       // Repite el Valor Anterior
   DATA lRunValid     // ya ejecutó Valida
   DATA lMsg          // Mensaje Error
   DATA lZero         // Si Rellena de Ceros
   DATA lListBox      // Ejecutar ListBox
   DATA lTry
   DATA lEmpty        // Campo no puede quedar Vacio
   DATA lItems        // Indica si Muestra la Cantidad de Items
   DATA lViewEmpty    INIT .F.
   DATA lAUTOLIST     INIT .F. // 16/01/2024

   DATA cItems        // Indica el texto
   DATA cNameAs       // Le Cambia el Nombre al Campo

METHOD New( cField , cNameAs, oGrid)
METHOD PostEdit()
METHOD When()
METHOD Valid(uValue,nLastKey, oColBrw)
METHOD Edit(lEdit)
METHOD PrepareCol()
METHOD VarPut(uValue)
METHOD BuildPicture()
METHOD RunCalc()
METHOD SetKey(nKey,bAction)
METHOD ListBox(lNext)
// METHOD BuildItems()
METHOD GoNextCol()
METHOD CalCuleRow(bExp)
METHOD GetDefault(uValue)
METHOD SetEditType(nEditType)
METHOD GetEditGet() INLINE ::oGrid:oBrw:aCols[::nCol]:oEditGet
METHOD BuildItems(aItems , aItemsData)
METHOD Find(uValue)
METHOD ShowTotal()   // Muestra el Total
METHOD Import(uValue)
METHOD RunOff(uValue) INLINE EVAL(::bRunOff,uValue)
METHOD GotFocus()
// EJECUCION DE CAMPOS MEMOS
METHOD RunMemo(nPar1,uPar2,uPar3,uPar4,uPar5,uPar6,uPar7,uPar8,uPar9,uPar10)    INLINE EJECUTAR("RUNMEMO",::cMemoRun,::oGrid,nPar1,uPar2,uPar3,uPar4,uPar5,uPar6,uPar7,uPar8,uPar9,uPar10) // 12/02/2025

/*
   ,;
                        ::oGrid:oBrw:aCols[::nCol]:PostEdit( ::oGrid:oBrw:aCols[::nCol] , uValue ),;
                        ::aCols[::nCol]:GoNextCol(::nCol)
*/
METHOD MensajeErr(cMsg)

METHOD HandleEvent( nMsg, nWParam, nLParam ) EXTERN ;
                               WndHandleEvent( Self, nMsg, nWParam, nLParam )

METHOD End()

ENDCLASS

METHOD New( cField , cNameAs , oGrid ) CLASS TDOCGRIDCOL

     LOCAL nAt:=0

     nAt:=ASCAN(oGrid:aFields,{|a,n|a[1]=cField})

     IF nAt=0
        MensajeErr("Campo "+cField+" no Existe")
     ENDIF


     ::oDb      :=oGrid:oDb
     ::cField   :=cField
     ::cNameAs  :=cNameAs
     ::nPos     :=LEN(oGrid:aCols)+1   // Posición en el Arreglo Columna

     ::nTotal       :=0
     ::oGrid        :=oGrid
     ::nEditType    :=1
     ::bValid       :={||.T.}
     ::bWhen        :={||.T.}
     ::bRunOff      :={||.T.}
     ::bCalc        :=NIL
     ::cTitle       :=cField
     ::nWidth       :=100
     ::lRepeat      :=.F.
     ::lChange      :=.F.
     ::lValid       :=.T.
     ::lRunValid    :=.F.
     ::lTotal       :=.F.
     ::cType        :=ValType(__objSendMsg(oGrid,cField))
     ::lPrimary     :=.F.
     ::nItemAdd     :=1 // Incrementador de Item
     ::lRepeat      :=.F.
     ::lMsg         :=.F. // Mensaje no Enviado
     ::lZero        :=.F.
     ::lListBox     :=.T.
     ::lEmpty       :=.T.
     ::uValueFind   :=NIL
     ::nDataStrAlign:=AL_LEFT
     ::nHeadStrAlign:=AL_LEFT
     ::oWnd         :=oGrid:oBrw
     ::lItems       :=.F.
     ::cItems       :="Items:" // Texto del Items

     IF ::cType="N"
       ::nDataStrAlign:=AL_RIGHT
       ::nHeadStrAlign:=AL_RIGHT
     ENDIF

     // ::lShowMsgErr:=.F.

RETURN SELF

/*
// Valida Post/Edit
*/
METHOD PostEdit(o,uValue) CLASS TDOCGRIDCOL
   LOCAL nCol:=::nCol // ::oGrid:oBrw:nGetColSel() // 21/11/2024
   LOCAL uOld:=::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol],I
   // LOCAL nCol //,oEditGet
   LOCAL bOnPostEdit:=o:bOnPostEdit

   o:bOnPostEdit:={||NIL}  //  JN 17/06/2018 Repetia la ejecucion de Seriales Entradas dos veces y se quedaba en un LOOP

// ? "AQUI ES POSTEDIT EN TDOCGRIDCOL",uValue
//   LOCAL oGet :=  ::oGrid:oBrw:aCols[::nCol]:oEditGet
/*
   IF ValType(oGet)="O" .AND. oGet:nLastKey=17
      oGet:bValid:={||.T.}
      oGet:bKeyDown:={||.T.}
      ::oGrid:oBrw:aCols[::nCol]:bKeyAction:={||oGrid:Edit()}
      // ::oGrid:oBrw:aCols[::nCol]:nKey      :=0
      oGet:End()
      // MensajeErr("MUERTO")
      RETURN .F.
   ENDIF

   MensajeErr(::oGrid:oBrw:aCols[::nCol]:oEditGet:nLastKey,"clase")

MensajeErr(GetProce(),oGrid:cKey)
*/

   ::uValue:=uValue

   IF ::cType="N" .AND. ValType(uValue)="C"
      uValue:=VAL(uValue)
      ::uValue:=uValue
      // 21/11/2024 ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=uValue
      ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,nCol]:=uValue
   ENDIF

   IF ::lZero .AND. ValType(uValue)="C"
      ::uValue:=STRZERO(VAL(uValue) , LEN(uValue))
      uValue:=::uValue
      // ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=uValue
      ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,nCol]:=uValue

//    ::oGrid:oBrw:DrawLine(.F.) // 03/12/2015
   ENDIF

   // ? o,ValType(o)+"DEBE SER OBJ",uValue,ValType(uValue)+"DEBE SER DATA"
   // ,"Se dispara despues del EditBtn"
   // RETURN
   // ? o:ClassName(),o:nLastKey
   // ? "POSEDIT,::nEditType",::nEditType

   ::oDpLbx:=NIL

   // Accionada por el Boton
   IF ValType(uValue)="L" .AND. ::nEditType=EDIT_GET_BUTTON
      RETURN .F.
   ENDIF

// ? ValType(uValue),o:ClassName(),GetProce(),::lValid

   IF ::uValue=NIL .OR. o=NIL .OR. !::lValid
      RETURN .F.
   ENDIF

// ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=uValue
// ::oGrid:Set(::cField,uValue)
// __objSendMsg(::oGrid,::cField,uValue)
// ? "POSTEDIT",getproce()
// IF !Empty(::aItems) // Es ComboBox

      ::lValid:=.T.

      IF !::Valid( @::uValue , NIL , o)

         ::oGrid:oBrw:SelectCol( ::nCol )

         IF ValType(::oDpLbx)="O" .AND. ::oDpLbx:lSqlOk
           ::oDpLbx:GotFocus()
           oDp:oFocus:=::oDpLbx:oBrw // Utilizado por BRWCOL y Recuperar Focus
           //SysRefresh(.T.)
         ENDIF

         RETURN .F.

      ENDIF

//   ENDIF

//   ? uValue,"Devuelto desde valid()"

   IF ValType(uOld)=ValType(::uValue) .AND. !(uOld==::uValue)
      ::lChange:=.T.
   ENDIF

   // Asigna el Valor
   // ? ::uValue,"EN POSTEDIT"

   // 21/11/2024 ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=::uValue
   ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,nCol]:=::uValue

   ::oGrid:Set(::cField,::uValue)
   ::uOldValue:=::uValue // Necesario para ser Editado

   // ? ::cField,uValue
   // ? ::oGrid:MCO_CODIGO,"ESE ES EL CODIGO"

   ::oGrid:nColEdit:=nCol // 21/11/2024 ::nCol  // Ultima Columna Editada

   IF !Empty(::bPostEdit) // PostEdición
      ::bPostEdit:=BloqueCod(::bPostEdit)
      EVAL(::bPostEdit)
   ENDIF

   //IF o:oEditGet!=NIL // .AND. o:oEditGet:nLastKey=13
   //? ::nCol,LEN(oGrid:aCols)

    FOR I := ::nCol+1 TO LEN(::oGrid:aCols)
       IF ::oGrid:aCols[I]:When() // ,"SE PUEDE????EDITAR, GO NEXT"
         ::GoNextCol(I) // Busca la Siguiente Columna que Acepte Get
         EXIT
       ENDIF
    NEXT

    // IF ::nCol<LEN(oGrid:aCols) .AND. !::oGrid:aCols[::nCol+1]:When()
    //  ? "DEBE BUSCAR AL SIGUIENTE"
    //   ::GoNextCol() // Busca la Siguiente Columna que Acepte Get
    // ENDIF

//   ELSE //IF !Empty(::aItems) // Es ComboBox

//      IF ::nCol<LEN(oGrid:aCols) .AND. !::oGrid:aCols[::nCol+1]:When()
//         ::GoNextCol() // Busca la Siguiente Columna que Acepte Get
//      ENDIF

//   ENDIF

/*
   nCol:=::nCol
   WHILE nCol<LEN(oGrid:aCols)

      nCol++

      IF !::oGrid:aCols[nCol]:When()
        ::oGrid:oBrw:GoRight()
      ELSE
        EXIT
      ENDIF

   ENDDO
*/

// ::RunCalc() // Calcula Cada Columna

/*
   oBrw:aCols[nCol]:bOnPostEdit   := {|o,x| oBrw:aArrayData[ oBrw:nArrayAt, nCol ] := x,;
                                            oBody:Set(cField,x),;
                                            IIF( o:nEditType=2 .OR. o:nEditType=5 .OR. o:nEditType=4,(lResp:=oBody:RunScrCol(NIL , nCol , o , cField ,x , .T. ,o:cValid),;
                                            oBrw:aCols[nCol]:lValid:=lResp ),NIL),;
                                            oBody:CheckCol(o,nCol),VP("lRunBtn",.F.),lResp}
*/

RETURN .T.
/*
// Importar
*/
METHOD Import(uValue) CLASS TDOCGRIDCOL

   // ? ValType(uValue),uValue

   ::oGrid:Prepare()

   IF !Empty(uValue)
     ::oGrid:Set(::cField,uValue,.T.)
   ENDIF

   // ::oGrid:oBrw:oWnd:nLastkey:=0
   // ::oGrid:oBrw:SetFocus()
   // ? ::oGrid:nOption,::oGrid:lEdit
   // ::oGrid:Edit(.T.)
   DpFocus(::oGrid:oBrw)
   ::oGrid:Edit(.T.)
   // ::oGrid:KeyBoard(13) // JN 08/04/2016

   //oDp:oFocus:=::oGrid:oBrw

RETURN .T.

METHOD MensajeErr(cMsg) CLASS TDOCGRIDCOL

   IF !::lMsg

     IF ::oGrid:oHead:oSayMsgErr=NIL

       ::oGrid:MensajeErr(cMsg) // MensajeErr(cMsg)

     ELSE
/*
// JN 04/06/2017
       IF ::oGrid:lShowMsgErr
         ::oGrid:oHead:oSayMsgErr:Hide()
       ENDIF

       // SndPlaySound( ::oGrid:cFileSound, 1 )

       ::oGrid:oHead:cSayMsgErr:=cMsg
       ::oGrid:oHead:oSayMsgErr:Show()
       ::oGrid:oHead:oSayMsgErr:Refresh(.T.)
       ::oGrid:lShowMsgErr:=.T.
*/
       ::oGrid:MensajeErr(cMsg)

     ENDIF
       // ::oGrid:MensajeErr(cMsg)
   ENDIF

   ::lMsg:=.T.

RETURN .T.

METHOD ShowTotal(cTotal) CLASS TDOCGRIDCOL
    LOCAL nTotal  :=0
    LOCAL cPicture:=::cPicture
    LOCAL nItems  :=LEN(::oGrid:oBrw:aArrayData)

    IF ::lItems
      ::oGrid:oBrw:aCols[::nCol]:cFooter      := ::cItems+LSTR(nItems)+IF(Empty(::oGrid:nMaxItems),"","/"+LSTR(::oGrid:nMaxItems))
      RETURN .F.
    ENDIF

    IF !Empty(cTotal)
      ::cFooter:=cTotal
    ENDIF

    IF !Empty(::cFooter)
       ::oGrid:oBrw:aCols[::nCol]:cFooter:= ::cFooter
    ENDIF

    IF !::lTotal
      RETURN .F.
    ENDIF

    nTotal  :=::oGrid:GetTotal(::cField)

    // ? "TOTAL",::cField,nTotal

    DEFAULT cPicture:="9,999,999,999,999.99"

    ::oGrid:oBrw:aCols[::nCol]:nFootStrAlign:= AL_RIGHT
    ::oGrid:oBrw:aCols[::nCol]:cFooter      := TRAN(nTotal,cPicture)

    // ? ::oGrid:oBrw:aCols[::nCol]:cFooter

RETURN .T.
/*
// Realiza la búsqueda
*/
METHOD Find(uValue,lMore) CLASS TDOCGRIDCOL
   LOCAL lFound:=.T.,oCol:=SELF,nAt:=0,nAtIni

   DEFAULT lMore:=.F.

   IF ::nCol=NIL
     // ? "Buscar",uValue
     ::bFind:={||oCol:Find(uValue,.F.)}
     RETURN .F.
   ENDIF

   ::uValueFind:=uValue

   IF Valtype(uValue)<>"N" .AND. Empty(uValue)
      RETURN .F.
   ENDIF

   nAtIni:=IIF( lMore, ::oGrid:oBrw:nArrayAt , 0)

// ? uValue

   WHILE .T.  //ValType(uValue)="C"

//     ? ::lPrimary,lMore
     IF ValType(uValue)<>"C"
        nAt:=ASCAN(::oGrid:oBrw:aArrayData,{|a,n|n>nAtInI .AND. a[::nCol]=uValue})
     ENDIF

     IF ValType(uValue)="C"
        uValue:=ALLTRIM(uValue)
        nAt:=ASCAN(::oGrid:oBrw:aArrayData,{|a,n|n>nAtInI .AND. Alltrim(a[::nCol])=uValue})
     ENDIF

     IF nAt=0 .AND. !::lPrimary .AND. ValType(uValue)="C"
        nAt:=ASCAN(::oGrid:oBrw:aArrayData,{|a,n|n>nAtIni .AND. UPPE(uValue)$UPPE(Alltrim(a[::nCol]))})
     ENDIF

     IF nAt=0 .AND. ValType(uValue)="C" .AND. !Empty(uValue) .AND. !lMore
       uValue      :=ALLTRIM(uValue)
       uValue      :=Left(uValue,Len(uValue)-1)
       IF !Empty(uValue)
          ::uValueFind:=uValue
       ENDIF
       LOOP
     ENDIF

     EXIT

   ENDDO

   // ? nAt,uValue,LEN(::oGrid:oBrw:aArrayData)

   IF nAt>0
     ::oGrid:oBrw:Refresh(.t.)
     ::oGrid:oBrw:nArrayAt:=nAt

// MensajeErr(getproce(),"aqui")

//     ::oGrid:oBrw:DrawLine(.T.)
//     ::oGrid:oBrw:Paint()
   ENDIF

   // ::oGrid:SetFind(.F.) // Quita Buscar

   // ViewArray(::oGrid:oBrw:aArrayData)
   // ? "Buscar",uValue,::cField,::nCol,nAt

RETURN lFound

/*
// Prepara la Lista
*/
// METHOD BuildItems() CLASS TDOCGRIDCOL
//   LOCAL aItems:={}
//
//   IF Empty(::aItems)
//      RETURN .F.
//   ENDIF
//RETURN .F.
/*
   IF ValType(::aItems)="B"
      aItems:=Eval(::aItems)
   ELSEIF ValType(::aItems)="A"
      aItems:=::aItems
   ENDIF

   IF !Empty(aItems)
      ::oGrid:oBrw:aCols[::nCol]:aEditListTxt  :=ACLONE(aItems)
      ::oGrid:oBrw:aCols[::nCol]:aEditListBound:=ACLONE(aItems)
   ENDIF
*/
// RETURN .T.

/*
// Valor por Defecto de la Columna
*/
METHOD GetDefault(uValue)

   IF ::uDefault=NIL
      RETURN uValue
   ENDIF

   IF ValType(::uDefault)="B"
      uValue:=EVAL(::uDefault,uValue)
   ELSE
      uValue:=::uDefault
      // ? ::uDefault,"::uDefault",uValue
   ENDIF

RETURN uValue

/*
// Llamado desde DPLBX para Asignar el Registro Deseado
*/
METHOD VarPut(uValue)

// ? "VARPUT DSDE DPLBX",GETPROCE()

   IF !(::oGrid:nOption=1.OR.::oGrid:nOption=3) .OR. ValType(::oGrid:oBrw)!="O" .OR. ::oGrid:oBrw:oWnd:hWnd=0 // ya fué Apagado
      RETURN NIL
   ENDIF

//? "VARPUT DSDE DPLBX"

   ::oGrid:Set(::cField,uValue)
   ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=uValue
// 03/12/2015
   ::oGrid:oBrw:DrawLine(.F.) //03/12/2015
//   ::oGrid:oBrw:SetFocus()
   ::PostEdit(::oGrid:oBrw:aCols[::nCol],uValue)

   ::oGrid:oBrw:SetFocus() // 9/3/2017

   PostMessage(::oGrid:oBrw:hWnd, 1024+1024 ) // 9/3/2017 Simula Presionar Click, dibuja el recuadro 9/3/2017

   // "AQUI ES EL CAMBIO",GETPROCE()

   // ::Edit(.T.)

RETURN NIL

/*
// Realiza la Validación de Cada Columna
//
*/
METHOD Valid(uValue,nLastKey, oColBrw )  CLASS TDOCGRIDCOL

    LOCAL lResp:=.F.

    ::lMsg:=.F. // Mensaje no Enviado
    ::oGrid:lMsgError:=.F. // No Emitio Mensaje de Error

//     MensajeErr("AQUI ME VALIDAN")

    IF !::lValid
       ::lRunValid:=.F.
       RETURN .F.
    ENDIF

    IF !::lEmpty .AND. Empty(uValue) // no puede estar VaciO
       RETURN .F.
    ENDIF

    IF !Empty(uValue) .AND. ValType(uValue)="D" .AND. DTOC(uValue)=DTOC(CTOD(""))
       ::oGrid:MensajeErr("Fecha Inválida")
       RETURN .F.
    ENDIF

//    IF !Empty(uValue) .AND. "("$::cListBox
//
//    ENDIF

    IF Empty(uValue) .AND. !EMPTY(::cListBox)

       uValue:=::ListBox(.F.)

       IF Empty(uValue)
          RETURN .F.
       ENDIF

    ENDIF

    ::oGrid:Set(::cField,uValue)
    ::uValue:=uValue

    __objSendMsg(::oGrid,::cField,uValue)  //  ::oGrid:nLastKey:=nLastKey

    IF !Empty(::cNameAs)
       ::oGrid:Set(::cNameAs,uValue)
    ENDIF

    IF Empty(::oGrid:aDataGet)
       ::oGrid:aDataGet:=ARRAY(LEN(::oGrid:aCols))
    ENDIF

    ::oGrid:aDataGet[::nCol]:=uValue

    ::PrepareCol()

    // ? ::bValid,"VALIDACION"

//MensajeErr( ::oGrid:cScript,::oGrid:oScript:cProgram )
// Errorsys(.t.)

    ::bValid:=BloqueCod(::bValid)

    // ? "DESPUES DEL VALID",::uValue,uValue
    // ::oGrid:Set(::cField,uValue,.T.)
    //    ? ::oGrid:MOC_CUENTA,"MOC_CUENTA"

    ::lListBox:=.T.

    WHILE .T.

      ::lTry:=.F.

      ::oGrid:Prepare()
      lResp:=EVAL(::bValid)

      IF ValType(lResp)<>"L"
         MensajeErr("Error en Validación en Campo "+::cField,::ClassName())
         lResp:=.F.
      ENDIF


      //? "DESPUES DEL VALID",::uValue,uValue,::lTry

      IF !::lTry
         EXIT
      ENDIF

      IF ValType(::GetEditGet())="O"
         // ? ::uValue,::GetEditGet():ClassName(),"AQUI ES TRY"
         ::GetEditGet():VarPut(::uValue,.T.)
      ENDIF

      ::oGrid:Set(::cField,uValue,.T.) // JN 07/04/2016 .t. pinta y parpadea, solo debe colocar su valor

    ENDDO

// ? "DESPUES DEL VALID",::uValue,uValue

    IF !lResp .AND. !Empty(::cMsgValid) .AND. ::lListBox

       ::oGrid:oBrw:GotFocus()

       IF !Empty(::cListBox)

          uValue:=::ListBox(.F.)

          IF uValue=NIL
              DpFocus(::oGrid:oFocus)
              // ? "AQUI DEBE PONER EL FOCO",::oGrid:ClassName()
              RETURN .F.
          ENDIF


          IF !Empty(uValue)

             ::oGrid:Set(::cField,uValue)

              // ? "LEYO, DEBE HACER NUEVO VALID"
              //
              // lResp:=EVAL(::bValid) //

          ENDIF

       ELSE

          IF !::oGrid:lMsgError
             ::MensajeErr(::cMsgValid)
          ENDIF

          ::oGrid:oBrw:GotFocus()

       // IIF( !Empty(::cListBox) , ::ListBox(.F.) , NIL)

       ENDIF

    ENDIF

//    ? "AHORA DEBE BUSCAR SI EXISTE ALGUN PRIMARY"
//
    IF ::lPrimary .AND. !::oGrid:ChkRecord()
      // Busca que las siguientes Columnas no sean Primary
      lResp:=.F.
    ENDIF

    ::lRunValid:=lResp

//    ? lResp,::oGrid:lShowMsgErr,::oGrid:oHead:oSayMsgErr

    IF lResp .AND. ::oGrid:lShowMsgErr .AND.  ::oGrid:oHead:oSayMsgErr!=NIL
       ::oGrid:oHead:oSayMsgErr:Hide()
       ::oGrid:lShowMsgErr:=.F.
    ENDIF


//    ::oGrid:oBrw:GotFocus()

RETURN lResp

/*
// Siguiente Columna
*/
METHOD GoNextCol(nCol)  CLASS TDOCGRIDCOL

   DEFAULT nCol:=::nCol+1

   WHILE nCol<LEN(oGrid:aCols) .AND. !::oGrid:aCols[nCol]:When()
      nCol++
   ENDDO

   ::oGrid:oBrw:nColSel:=nCol

   // ::oGrid:aCols[nCol]:When()
   // ::oGrid:oBrw:SelectCol( nCol ) // JN Causante del Pestañeo, ademas refresca completo el browse lo repinta

   ::oGrid:nColSel:=nCol
//   ::oGrid:oBrw:Refresh(.F.) //
   ::oGrid:aCols[nCol]:Edit(.T.)
   // ::oGrid:oBrw:keyBoard(13) // JN Debe Activar Enter para Editar la Casilla

RETURN NIL

/*
// Editar Archivo ListBox
*/
METHOD ListBox(lNext,cListBox,cWhere) CLASS TDOCGRIDCOL

     LOCAL lResp,oGet,nAt,nRow,cWhereListBox
     LOCAL oCol   :=::oGrid:oBrw:aCols[::nCol]
     LOCAL uValue :=::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]

     DEFAULT cListBox:=::cListBox

     IF Empty(cListBox)
        MensajeErr("Es necesario Indicar Formulario LBX")
        RETURN NIL
     ENDIF

     ::oDpLbx:=NIL
     ::oGrid:Prepare()

     DEFAULT lNext:=.T.

     IF ValType(oCol:oEditGet)="O"
        uValue:=Eval(oCol:oEditGet:bSetGet)
        // oCol:oEditGet:nLastKey:=13
     ENDIF

     IF ValType(cListBox)="B"
        oGet:=Eval(cListBox,Self,uValue)
        IF ValType(oGet)="O"
           // ? oGet:nColSel
           // oGet:SelectCol( 1 )
           // oGet:Paint()
           // oGet:DrawLine(.t.)
           // oGet:Refresh(.T.)
           // oGet:Paint()
           // DpFocus(oGet)
           //::oGrid:oHead:oControl:=oGet
           //::oGrid:oBrw:aCols[::nCol]:KillGet()
           //::oGrid:oBrw:aCols[::nCol]:lOnPostEdit:=.T. // Asi no Valida Otra vez
           // ? oGet:ClassName(),"AQUI DEBE ESTAR",GETPROCE()
           oDp:oFocus:=oGet // :oWnd
           // ::oGrid:oFocus:=oGet // :oWnd
        ENDIF

        RETURN NIL

     ENDIF

     IF ".LBX"$UPPE(cListBox)

        IF Empty(uValue) .AND. !Empty(::uOldValue)
           uValue:=::uOldValue // Necesario para ser Editado
        ENDIF

        cWhereListBox:=::cWhereListBox

        IF cWhereListBox<>NIL .AND. (["]$cWhereListBox)
           cWhereListBox:=MacroEje(::cWhereListBox)
        ENDIF

        // 03/04/2014
        IF !Empty(cWhere) .AND. !Empty(cWhereListBox)
           cWhereListBox:=cWhereListBox+" AND "+cWhere
        ENDIF

        IF !Empty(cWhere) .AND. Empty(cWhereListBox)
           cWhereListBox:=cWhere
        ENDIF

        SETDBSERVER(::oGrid:oDb)

// ? ::oDb,"::oGrid:oDb",::oGrid:oDb:ClassName(),::oDb:oConnect:cUser

        ::oDpLbx:=DpLbx(cListBox,::cTitleListBox,cWhereListBox,NIL,NIL,NIL,NIL,NIL,::oDb)
//? cFileLbx,cTitle,cWhere,lPage,cField,uValue,aParam,lDialog,oDb

        SETDBSERVER(NIL)

        IF ::oDpLbx:GetValue(NIL , Self , NIL , NIL , uValue)

           nAt :=::oDpLbx:oBrw:nArrayAt
           nRow:=::oDpLbx:oBrw:nRowSel

           ::oDpLbx:oBrw:DrawLine(.T.) // JN 22/04/2016 Refresh(.F.)

           ::oDpLbx:oBrw:nArrayAt:=nAt
           ::oDpLbx:oBrw:nRowSel :=nRow


           // ? "SI ENCONTRO"
        ENDIF

        IF ValType(::oDpLbx)="O" .AND. ValType(::oDpLbx:oBrw)="O"
          ::oDpLbx:oBrw:GotFocus()
          DpFocus(::oDpLbx:oBrw) // :GotFocus()

        ENDIF

        // GetValue(cField,oGet,bBlq,uAdd,uValue)

        RETURN NIL

     ENDIF


     lResp:=DPBRWPAG(cListBox,NIL,@uValue,::cField,.T.)

     ::oGrid:oBrw:SetFocus()

     IF !Empty(uValue)

         ::oGrid:Set(::cField,uValue)
         // Mejorar la Vision
         ::oGrid:oBrw:CancelEdit()
         ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=uValue
         ::oGrid:oBrw:DrawLine(.T.)

//         SysRefresh(.T.)

     ENDIF

RETURN uValue
/*
// Asigna Tecla de Función
*/
METHOD SetKey(nKey) CLASS TDOCGRIDCOL

//     oCol:SetKey(VK_F6,{|oGet|DPBRWPAG("DPINV.BRW",oGet,NIL,NIL,.T.)})

RETURN .T.

/*
// Calculo de Cada Columna
*/
   METHOD RunCalc()  CLASS TDOCGRIDCOL

      LOCAL uValue

      IF Empty(::bCalc)
         return nil
      ENDIF

      ::PrepareCol()
      ::bCalc:=BloqueCod(::bCalc)

      uValue :=EVAL(::bCalc)

      IF ValType(uValue)="N"
        uValue :=VALSTRTRAN(uValue,::cPicture)
      ENDIF

      ::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]:=uValue
      ::oGrid:oBrw:DrawLine()
      __objSendMsg(::oGrid,::cField,uValue)

RETURN NIL

/*
// Calculo de Cada Columna
*/
METHOD BuildItems(aItems , aItemsData , nEditType )  CLASS TDOCGRIDCOL
//   LOCAL nEditType

   aItems    :={}
   aItemsData:={}

   IF Empty(::aItems) .OR. Empty(::oGrid:oBrw:aArrayData)
      RETURN NIL
   ENDIF

   nEditType:=EDIT_LISTBOX

   IF ValType(::aItems)="A"
      aItems:=::aItems
   ELSEIF ValType(::aItems)="B"
      aItems:=EVAL(::aItems)
   ENDIF

   aItemsData:=CTOITEM(::aItemsData)

   IF Empty(aItemsData)

      Aeval(aItems,{|a,n|Aadd(aItemsData,CTOO(a,::cType))})

   ELSE

      Aeval(aItemsData,{|a,n|aItemsData[n]:=CTOO(a,::cType)})

   ENDIF

   Aeval(aItems,{|a,n|aItems[n]:=CTOO(a,"C")})

   ::oGrid:oBrw:aCols[::nCol]:aEditListTxt  :=aItems
   ::oGrid:oBrw:aCols[::nCol]:aEditListBound:=aItemsData

   IF Empty(::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]) .AND. !Empty(aItemsData)

      ::oGrid:Set(::cField,aItemsData[1],.T.)

   ENDIF

   IF LEN(aItemsData)=1 // Un solo Elemento no Hace Box
      nEditType:=0
   ENDIF

RETURN NIL


/*
// Indica si la Columna puede Tener Datos
*/
METHOD When() CLASS TDOCGRIDCOL
   LOCAL lResp :=.T.
   LOCAL aItems:={} , aItemsData:={}

   // RETURN .T.

   ::bWhen:=BloqueCod(::bWhen)

   IF !Empty(::bEditBlock)
      ::oGrid:oBrw:aCols[::nCol]:bEditBlock:=::bEditBlock
   ENDIF

   ::PrepareCol() // Crea Todas las Variables Publicas

   // oDp:oFrameDp:SetText(LSTR(::oGrid:GetColEdit()))

   IF ::bWhenGetCol<>NIL .AND. ::nCol=::oGrid:GetColEdit()

      ::oGrid:oBrw:CancelEdit()

      IF !Eval(::bWhenGetCol) //  "AHORA ME TOCA A MI COL"
         RETURN .F.
      ENDIF

   ENDIF

   IF !Eval(::oGrid:bWhen)  // Condición del Grid
      RETURN .F.
   ENDIF

//oDp:oFrameDp:SetText(::oGrid:cTable+","+::cField+","+LSTR(::oGrid:nOption)+",RESP"+CTOO(!AccessField(::oGrid:cTable,::cField,::oGrid:nOption,"C")))

   IF !AccessField(::oGrid:cTable,::cField,::oGrid:nOption)
      RETURN .F.
   ENDIF

   IF !Empty(::aItems)

     ::BuildItems(@aItems , @aItemsData , ::nEditType )

     IF !Empty(aItems) .AND. LEN(aItems)=1 // Un Solo Item no hace Get
        RETURN .F.
     ENDIF

   ENDIF

//   IF ::BuildItems() // Prepara la Lista de Opciones
//      ::nEditType:=EDIT_LISTBOX
//   ENDIF

   lResp:=Eval(::bWhen)

RETURN lResp

/*
// Asigna Tipo de Get
*/
METHOD SetEditType(nEditType)

   ::oGrid:oBrw:aCols[::nCol]:nEditType:=nEditType

   ::nEditType :=1

   IF nEditType<>2 // Debe Matar a Items

      ::aItems    :={}
      ::aItemsData:={}

      ::oGrid:oBrw:aCols[::nCol]:aEditListTxt  :=NIL
      ::oGrid:oBrw:aCols[::nCol]:aEditListBound:=NIL

   ENDIF

RETURN NIL


/*
// Activa el get
*/
METHOD Edit(lEdit) CLASS TDOCGRIDCOL

   LOCAL lWhen      :=.T.
   LOCAL nEditType  :=::nEditType
   LOCAL aItems     :={}
   LOCAL aItemsData :={}

   DEFAULT lEdit:=.T.

   // MensajeErr(::oGrid:oBrw:nLastKey,"aquie debe ser")
//   oDp:oFrameDp:SetText(LSTR(::oGrid:oBrw:aCols[::nCol]:nkey))
//   IF  ::oGrid:oBrw:aCols[::nCol]:nkey=117
      // MensajeErr("aqui no")
//      return .F.
//   ENDIF

   Aeval(::oGrid:oBrw:aCols,{|oCol|oCol:KillGet()})

   IF lEdit

      IF ::When() // Revisa si está o no Apta

         IF !Empty(::cListBox)

            // nEditType:= EDIT_GET_BUTTON // EDIT_BUTTON

            // ::bAction:=BLOQUECOD(::bAction)
            // ::oGrid:oBrw:aCols[::nCol]:bEditBlock:=::bAction

            DEFAULT ::nKeyEdit:=VK_F6

// OJO este quita la bKeyaction de runkey
//            ::oGrid:oBrw:aCols[::nCol]:SetKey(::nKeyEdit,{||::lValid:=.F.,::ListBox(),::lValid:=.T.}) // MsgAlert("OK,Editar")})

            IF ::nEditType=EDIT_GET_BUTTON
              ::oGrid:oBrw:aCols[::nCol]:bEditBlock:={|| ::lValid:=.F.,::ListBox(),::lValid:=.T. }
            ENDIF

            // {|oGet|DPBRWPAG("DPINV.BRW",oGet,NIL,NIL,.T.)})

         ENDIF

         IF !Empty(::aItems)

            nEditType:=EDIT_LISTBOX
//            ErrorSys(.T.)
            ::BuildItems(@aItems , @aItemsData , @nEditType )

/*

            nEditType:=EDIT_LISTBOX

            IF ValType(::aItems)="A"
               aItems:=::aItems
            ELSEIF ValType(::aItems)="B"
               aItems:=EVAL(::aItems)
            ENDIF

            aItemsData:=CTOITEM(::aItemsData)

            IF Empty(aItemsData)

              Aeval(aItems,{|a,n|Aadd(aItemsData,CTOO(a,::cType))})

            ELSE
              // Todos deben Ser el Mismo Tipo de Data
              Aeval(aItemsData,{|a,n|aItemsData[n]:=CTOO(a,::cType)})

            ENDIF

            // Todos deben Ser Caracteres
            Aeval(aItems,{|a,n|aItems[n]:=CTOO(a,"C")})

            ::oGrid:oBrw:aCols[::nCol]:aEditListTxt  :=aItems
            ::oGrid:oBrw:aCols[::nCol]:aEditListBound:=aItemsData

            IF Empty(::oGrid:oBrw:aArrayData[::oGrid:oBrw:nArrayAt,::nCol]) .AND. !Empty(aItemsData)

               ::oGrid:Set(::cField,aItemsData[1],.T.)

            ENDIF

            IF LEN(aItemsData)=1 // Un solo Elemento no Hace Box
               nEditType:=0
            ENDIF
*/

         ENDIF

         // JN 12/05/2023, boton ACTION Caso del calendario u otra Ejecución
         IF ValType(::bAction)="B" .AND. ::oGrid:oBrw:aCols[::nCol]:bEditBlock=NIL
            ::oGrid:oBrw:aCols[::nCol]:bEditBlock:={|| ::lValid:=.F.,EVAL(::bAction),::lValid:=.T. }
         ENDIF

         ::oGrid:oBrw:aCols[::nCol]:nEditType  :=nEditType
         ::oGrid:oBrw:aCols[::nCol]:bWhenGetValue   :=::bWhenGetValue
         ::oGrid:oBrw:aCols[::nCol]:lEditBorder:=.F.
         ::oGrid:oBrw:aCols[::nCol]:bClrEdit :={||{oDp:Get_nCltText,oDp:Get_nClrPane}}

         // ::oGrid:oBrw:aCols[::nCol]:Edit() // jn 07/04/2016 Debe activa el Foco

         ::oGrid:oBrw:oWnd:lCancel:=.T.


         // ? "AQUI DEBE SER, PARA CANCELAR"

      ELSE

         ::oGrid:oBrw:aCols[::nCol]:nEditType:=0
         lWhen:=.F.

      ENDIF

   ELSE

      ::oGrid:oBrw:aCols[::nCol]:nEditType:=0
      lWhen:=.F.

   ENDIF

RETURN lWhen

/*
// Prepara la Columna
*/
METHOD PrepareCol() CLASS TDOCGRIDCOL

   ::oGrid:Prepare()

   // PUBLICO("OGRID",Self)   // El Cuerpo se llama oGRID
   // PUBLICO(::oGrid:oHead:cVarName,::oGrid:oHead)

RETURN .T.

/*
// Realiza el Calculo en Todo el Browse
*/
METHOD CalCuleRow(bCalc) CLASS TDOCGRIDCOL
   LOCAL I,U,nValue:=0

   bCalc:=BloqueCod(bCalc)

// ? ::oGrid:MOC_MONTO,"MOC_MONTO",ValType(::oGrid:MOC_MONTO)
// ViewArray(::oGrid:oBrw:aArrayData)

   FOR I := 1 TO LEN(::oGrid:oBrw:aArrayData)

      FOR U := 1 TO LEN(::oGrid:oBrw:aArrayData[I])
         // TransFiere los Datos
         ::oGrid:Set(::oGrid:aCols[U]:cField,::oGrid:oBrw:aArrayData[I,U])
      NEXT

      nValue+=EVAL(bCalc)

   NEXT

RETURN nValue

/*
// Construye el Picture
*/

METHOD BuildPicture() CLASS TDOCGRIDCOL

     LOCAL cType:=ValType(::oGrid:aData[1,::nCol])

     DEFAULT ::cSayPicture:=::cPicture

     IF Empty(::cSayPicture) .AND. cType="N"

        ::nDataStrAlign:=AL_RIGHT
        ::cSayPicture  :=FIELDPICTURE(::oGrid:cTable,::cField,.T.)

        DEFAULT ::cPicture     :=::cSayPicture

     ENDIF

RETURN ::cSayPicture

METHOD GotFocus() CLASS TDOCGRIDCOL

   IF ::oGrid:lEdit
      oGrid:BrwGotFocus()
   ENDIF

RETURN .T.

METHOD End() CLASS TDOCGRIDCOL
RETURN NIL

STATIC FUNCTION CTOITEM(aData)

    DEFAULT aData:={}

    IF ValType(aData)="B"
       aData:=EVAL(aData)
    ENDIF

RETURN aData


