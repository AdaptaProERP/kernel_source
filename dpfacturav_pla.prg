// Programa   : DPFACTURAV_PLA
// Fecha/Hora : 20/04/2020 17:02:11
// Propósito  : Encabezado de la Plantilla
// Creado Por : Juan Navas
// Llamado por:
// Aplicación :
// Tabla      :

#INCLUDE "DPXBASE.CH"

PROCE MAIN(oDocCli)
   LOCAL cTitle:=" TITULO"

   IF oDocCli=NIL
      RETURN NIL
   ENDIF

   oDocCli:cDescri:="oDocCli:cDescri"

   @ 1, 1 SAY "Número: "           RIGHT
   @ 2, 1 SAY "Descripción:"       RIGHT
   @ 3, 1 SAY "Fecha:"             RIGHT
   @ 3, 1 SAY oDp:xDPTABMON+":"    RIGHT
   @ 4, 1 SAY oDp:xDPPRECIOTIP+":" RIGHT

   @ 5,20 SAY oDocCli:oDOC_SAYMON PROMPT SQLGET("DPTABMON"    ,"MON_DESCRI","MON_CODIGO"+GetWhere("=",oDocCli:DOC_CODMON))
   @ 6,20 SAY oDocCli:oDOC_SAYLIS PROMPT SQLGET("DPPRECIOTIP" ,"TPP_DESCRI","TPP_CODIGO"+GetWhere("=",oDocCli:DOC_DESTIN))

   @ 4, 1 SAY "Destino:"           RIGHT
   @ 5,20 SAY oDocCli:oDOC_SAYDES  PROMPT SQLGET("DPTIPDOCCLI","TDC_DESCRI","TDC_TIPO"+GetWhere("=",oDocCli:DOC_TIPAFE))

   @ 0.0,43 GET oDocCli:oDOC_NUMERO VAR oDocCli:DOC_NUMERO;
            PICTURE oDocCli:cPicDoc;
            VALID CERO(oDocCli:DOC_NUMERO) .AND. EJECUTAR("DPDOCCLIVALNUM",oDocCli);
            WHEN (AccessField("DPDOCCLI","DOC_NUMERO",oDocCli:nOption);
                .AND. oDocCli:lValCodCli ;
                .AND. oDocCli:nOption!=0 .AND. oDocCli:lPar_EditNum);
            SIZE 35,10

   @ 10,43 GET oDocCli:oDescri VAR oDocCli:cDescri;
           SIZE 135,10;
           WHEN !oDocCli:nOption=0

   @ 0.9,70 BMPGET oDocCli:oDOC_FECHA  VAR oDocCli:DOC_FECHA ;
            PICTURE oDp:cFormatoFecha;
            NAME "BITMAPS\Calendar.bmp";
            ACTION LbxDate(oDocCli:oDOC_FECHA ,oDocCli:DOC_FECHA);
            VALID (EJECUTAR("DPVALFECHA",oDocCli:DOC_FECHA,oDocCli:lPar_ValFch,.T.) .AND. ;
                  EJECUTAR("DPDOCCLIVALCAM",oDocCli));
            WHEN (AccessField("DPDOCMOV","DOC_FECHA",oDocCli:nOption);
                .AND. oDocCli:lValCodCli ;
                .AND. oDocCli:nOption!=0.AND. oDocCli:lPar_Fecha);
            SIZE 45,10

 @ 7.0,06 BMPGET oDocCli:oDOC_CODMON VAR oDocCli:DOC_CODMON;
            VALID oDocCli:VALCODMON();
            NAME "BITMAPS\FIND.BMP";
            ACTION (oDpLbx:=DpLbx("DPTABMON",NIL,"MON_ACTIVO=1",NIL,NIL,NIL,NIL,NIL,NIL,oDocCli:oDOC_CODMON,NIL),;
                    oDpLbx:GetValue("MON_CODIGO",oDocCli:oDOC_CODMON));
            WHEN (AccessField("DPTABMON","DOC_CODMON",oDocCli:nOption);
                  .AND. oDocCli:nOption!=0);
             SIZE 30,10

  oDocCli:oDOC_CODMON:cToolTip:="Código del "+oDp:xDPTABMON

 @ 7.0,06 BMPGET oDocCli:oDOC_DESTIN VAR oDocCli:DOC_DESTIN;
            VALID oDocCli:VALLISTA();
            NAME "BITMAPS\FIND.BMP";
            ACTION (oDpLbx:=DpLbx("DPPRECIOTIP",NIL,"TPP_ACTIVO=1",NIL,NIL,NIL,NIL,NIL,NIL,oDocCli:oDOC_DESTIN,NIL),;
                    oDpLbx:GetValue("TPP_CODIGO",oDocCli:oDOC_DESTIN));
            WHEN (AccessField("DPDOCCLI","DOC_DESTIN",oDocCli:nOption);
                  .AND. oDocCli:nOption!=0);
             SIZE 30,10

  oDocCli:oDOC_DESTIN:cToolTip:="Código del "+oDp:xDPPRECIOTIP

  @ 7.0,06 BMPGET oDocCli:oDOC_TIPAFE VAR oDocCli:DOC_TIPAFE;
            VALID oDocCli:VALTIPDOC();
            NAME "BITMAPS\FIND.BMP";
            ACTION (oDpLbx:=DpLbx("DPTIPDOCCLI",NIL,"TDC_ACTIVO=1 AND TDC_PRODUC=1",NIL,NIL,NIL,NIL,NIL,NIL,oDocCli:oDOC_TIPAFE,NIL),;
                    oDpLbx:GetValue("TDC_TIPO",oDocCli:oDOC_TIPAFE));
            WHEN (AccessField("DPDOCCLI","DOC_TIPAFE",oDocCli:nOption);
                  .AND. oDocCli:nOption!=0);
             SIZE 30,10

  oDocCli:oDOC_DESTIN:cToolTip:="Código del "+oDp:xDPPRECIOTIP

  @ 2,80 CHECKBOX oDocCli:oDOC_VTAANT  VAR  oDocCli:DOC_VTAANT PROMPT ANSITOOEM(" Dinámico ")

 
RETURN NIL
// EOF

