// Programa   : TODOSLOSACENTOS
// Fecha/Hora : 11/08/2023 14:00:06
// Prop�sito  : Recuperar Todos los Acentos Distorcionados
// Creado Por : Juan Navas
// Llamado por:
// Aplicaci�n :
// Tabla      :

#INCLUDE "DPXBASE.CH"

PROCE MAIN(lData)
  LOCAL cWhere,cWhereT,oTable,I,cField,cDataO,cDataD,cSql,cTable,aData:={},oDb

  DEFAULT lData:=.T.

  cWhereT:=IF(lData,[LEFT(TAB_DSN,1)="<"],[LEFT(TAB_DSN,1)="."])+" AND NOT LEFT(TAB_NOMBRE,5)"+GetWhere("=","VIEW_")

  oDb    :=OpenOdbc(IF(lData,oDp:cDsnData,oDp:cDsnConfig))

  cWhere :="CAM_NAME "+GetWhere(" LIKE ","%_DESCRI%")+" OR "+;
           "CAM_NAME "+GetWhere(" LIKE ","%_NOMBRE%")+" OR "+;
           "CAM_NAME "+GetWhere(" LIKE ","%_PRESEN%")

  oTable :=OpenTable(" SELECT CAM_TABLE,CAM_NAME,TAB_PRIMAR FROM DPCAMPOS "+;
                     " INNER JOIN DPTABLAS ON TAB_NOMBRE=CAM_TABLE AND "+cWhereT+;
                     " WHERE "+cWhere,.T.)
  AADD(aData,{"ó","�"})
  AADD(aData,{"ía","�"})
  AADD(aData,{"é","�"})
  AADD(aData,{"ú","�"})
  AADD(aData,{"ñ","�"})
  AADD(aData,{"�"+CHR(173),"�"})
  AADD(aData,{"á","�"})
  AADD(aData,{"�"+CHR(226),"�"})
  AADD(aData,{"Ñ","�"})

  WHILE !oTable:EOF()

    cField:=ALLTRIM(oTable:CAM_NAME)
    cTable:=ALLTRIM(oTable:CAM_TABLE)

    IF !EJECUTAR("ISFIELDMYSQL",oDb,cTable,cField)
       Checktable(cTable)
    ENDIF


    FOR I=1 TO LEN(aData)

      cWhere:=[ ]+cField+ " LIKE "+GetWhere("","%"+aData[I,1]+"%")

      cSql:=[ UPDATE ]+cTable+;
            [ SET    ]+cField+[= REPLACE(]+cField+[,]+GetWhere("",aData[I,1])+[,]+GetWhere("",aData[I,2])+[)]+;
            [ WHERE  ]+cWhere

      oDb:EXECUTE(cSql)

    NEXT I

    oTable:DbSkip()

  ENDDO

  oTable:End()

RETURN
