      SUBROUTINE OP0141 ( IER )
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER             IER
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 23/05/2003   AUTEUR MCOURTOI M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR   
C (AT YOUR OPTION) ANY LATER VERSION.                                 
C
C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT 
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF          
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU    
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.                            
C
C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE   
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,       
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.      
C ======================================================================
C     PROCEDURE D'IMPRESSION DE COURBES ( IMPR_COURBE )
C     ----------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     ----- FIN COMMUNS NORMALISES  JEVEUX  ----------------------------
      INTEGER       NSST,LEXT
      CHARACTER*4   KIOCC, PARTIE
      CHARACTER*8   K8B, NOPARA, NOMTAB, INTITU, NOEUD, SST, BASEMO,
     +              RAIDE, NOMGR, NOMA, INTERP, PROLGD
      CHARACTER*16  NOMCMD, FILE , FORMAT, PARAX, PARAY
      CHARACTER*19  NOMFON, NOMFX, NOMFY, LISTR, LISTRS, RESU
      CHARACTER*1   K1BID
      CHARACTER*24  LISFON
C     ----------------------------------------------------------------
C
C     --- VERIFICATION SUPPLEMENTAIRE ---
      CALL JEMARQ()
      CALL INFMAJ()
C
      CALL GETRES(K8B,K8B,NOMCMD)
      INTERP = 'NON NON '
      PROLGD = 'EE      '
C
      CALL GETFAC('COURBE',NBCOUR)
      IMPR = 3
C
C     --- FORMAT ET FICHIER DE SORTIE DEMANDE ---
      CALL GETVTX(' ','FICHIER',1,1,1,FILE  ,N)
      FORMAT = FILE
      CALL GETVTX(' ','FORMAT' ,1,1,1,FORMAT,N)
      IUL = IUNIFI(FILE)
      IF ( IUL .LE. 0 ) THEN
         LG = MAX(1,LXLGUT(FILE))
         CALL UTMESS('A',NOMCMD//' (ERREUR 01)',
     +                   'LE FICHIER "'//FILE(1:LG)//'" N''EST RELIE '//
     +                   'A AUCUNE UNITE LOGIQUE.')
         GOTO 9999
      ENDIF
C
      IF ( FORMAT .EQ. 'EXCEL' ) THEN
C       TABLEAU TRAVAIL POUR EXCEL
        LISFON='&&OP0141.TABEXCEL'
        CALL WKVECT(LISFON,'V V K24',NBCOUR,LEXT)
      ENDIF
C
C     PARTIE IMPRESSION DES DIRECTIVES
      IF ( FORMAT.EQ.'GNUPLOT' .OR. FORMAT.EQ.'POSTSCRIPT' .OR.
     +     FORMAT.EQ.'AGRAF'   ) THEN
         IPS = 0
         IF (FORMAT.EQ.'POSTSCRIPT' ) IPS = 1
         DO 100 IC = 1,NBCOUR
            CALL CODENT(IC,'G',KIOCC)
            CALL GETVID('COURBE','FONCTION' ,IC,1,0,NOMFON,NF)
            CALL GETVID('COURBE','FONC_X'   ,IC,1,0,NOMFX ,NX)
            CALL GETVID('COURBE','TABLE'    ,IC,1,0,NOMTAB,NT)
            CALL GETVID('COURBE','LIST_RESU',IC,1,0,LISTRS,NL)
            CALL GETVID('COURBE','RESU_GENE',IC,1,0,RESU  ,NR)
            CALL GETVID('COURBE','LIST_PARA',IC,1,0,LISTR ,IND)
            IF (NF.NE.0) THEN
               CALL GETVID('COURBE','FONCTION',IC,1,1,NOMFX ,N)
               CALL JEVEUO(NOMFX//'.PROL','L',LPROX)
               IF (ZK16(LPROX).EQ.'FONCT_C ') THEN
                  CALL GETVTX('COURBE','PARTIE',IC,1,1,PARTIE,N)
                  IF (N.EQ.0) PARTIE = 'REEL'
                  NOMFON = '&&PARTIE_'//PARTIE
                  CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
                  ZK16(LPRO)   = 'FONCTION'
                  ZK16(LPRO+1) = ZK16(LPROX+1)
                  ZK16(LPRO+2) = ZK16(LPROX+2)
                  ZK16(LPRO+3) = ZK16(LPROX+3)
                  ZK16(LPRO+4) = ZK16(LPROX+4)
               ELSE
                  NOMFON = NOMFX
               ENDIF
            ELSEIF (NX.NE.0) THEN
               CALL GETVID('COURBE','FONC_X',IC,1,1,NOMFX ,N)
               CALL GETVID('COURBE','FONC_Y',IC,1,1,NOMFY ,N)
               CALL GETVTX('COURBE','PARA'  ,IC,1,1,NOPARA,N)
               IF (NOPARA(1:6).EQ.'FONC_Y') THEN
                  NOMFON = NOMFY
                  NOMFY = NOMFX
                  NOMFX = NOMFON
               ENDIF
               CALL JEVEUO(NOMFX//'.PROL','L',LPROX)
               CALL JEVEUO(NOMFY//'.PROL','L',LPROY)
               NOMFON = '&&COURBE_'//KIOCC
               CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
               ZK16(LPRO)   = 'FONCTION'
               ZK16(LPRO+1) = INTERP
               ZK16(LPRO+2) = ZK16(LPROX+2)
               ZK16(LPRO+3) = ZK16(LPROY+3)
               ZK16(LPRO+4) = PROLGD
            ELSEIF (NT.NE.0) THEN
               CALL GETVTX('COURBE','PARA_X'  ,IC,1,1,PARAX ,N)
               CALL GETVTX('COURBE','PARA_Y'  ,IC,1,1,PARAY ,N)
               NOMFON = '&&COURBE_'//KIOCC
               CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
               ZK16(LPRO)   = 'FONCTION'
               ZK16(LPRO+1) = INTERP
               ZK16(LPRO+2) = PARAX
               ZK16(LPRO+3) = PARAY
               ZK16(LPRO+4) = PROLGD
            ELSEIF (NR.NE.0) THEN
               CALL GETVTX('COURBE','PARA_X'  ,IC,1,1,PARAX ,N)
               CALL GETVTX('COURBE','PARA_Y'  ,IC,1,1,PARAY ,N)
               NOMFON = '&&COURBE_'//KIOCC
               CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
               ZK16(LPRO)   = 'FONCTION'
               ZK16(LPRO+1) = 'LIN LIN '
               ZK16(LPRO+2) = PARAX
               ZK16(LPRO+3) = PARAY
               ZK16(LPRO+4) = PROLGD
            ELSE
               NOMFON = '&&COURBE_'//KIOCC
               CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
               ZK16(LPRO)   = 'FONCTION'
               ZK16(LPRO+1) = INTERP
               ZK16(LPRO+2) = '  '
               ZK16(LPRO+3) = '  '
               ZK16(LPRO+4) = PROLGD
            ENDIF
            IF ( FORMAT.EQ.'AGRAF'   ) THEN
               CALL FOECAG('COURBE',NBCOUR,IC,NOMFON,IUL,IPS,IND,IRET)
            ELSE
               CALL FOECGN('COURBE',NBCOUR,IC,NOMFON,IUL,IPS,IND,IRET)
            ENDIF
            CALL JEDETC('V','&&COURBE',1)
            CALL JEDETC('V','&&PARTIE',1)
            IF (IRET.NE.0) GOTO 9999
 100     CONTINUE
      ENDIF
C
      DO 200 IC = 1,NBCOUR
         CALL CODENT(IC,'G',KIOCC)
C
         CALL GETVID('COURBE','FONCTION' ,IC,1,0,NOMFON,NF)
         CALL GETVID('COURBE','FONC_X'   ,IC,1,0,NOMFX ,NX)
         CALL GETVID('COURBE','TABLE'    ,IC,1,0,NOMTAB,NT)
         CALL GETVID('COURBE','LIST_RESU',IC,1,0,LISTRS,NL)
         CALL GETVID('COURBE','RESU_GENE',IC,1,0,LISTRS,NR)
C
         IF (NF.NE.0) THEN
            CALL GETVID('COURBE','FONCTION' ,IC,1,1,NOMFX ,N)
            CALL GETVTX('COURBE','PARTIE'   ,IC,1,1,PARTIE,NPA)
            CALL JEVEUO(NOMFX//'.PROL','L',LPROX)
            IF (ZK16(LPROX).EQ.'FONCT_C ') THEN
               IF (NPA.EQ.0) THEN
                  IF ( FORMAT.EQ.'GNUPLOT'    .OR.
     +                 FORMAT.EQ.'AGRAF'      .OR.
     +                 FORMAT.EQ.'POSTSCRIPT' ) THEN
                     PARTIE = 'REEL'
                     NOMFON = '&&PARTIE_'//PARTIE
                  ELSE
                     NOMFON = NOMFX
                     GOTO 8
                  ENDIF
               ELSE
                  NOMFON = '&&PARTIE_'//PARTIE
               ENDIF
               CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
               ZK16(LPRO)   = 'FONCTION'
               ZK16(LPRO+1) = ZK16(LPROX+1)
               ZK16(LPRO+2) = ZK16(LPROX+2)
               ZK16(LPRO+3) = ZK16(LPROX+3)
               ZK16(LPRO+4) = ZK16(LPROX+4)
               CALL JELIRA(NOMFX//'.VALE' ,'LONMAX',NBCOUP,K1BID)
               NBINST = NBCOUP / 3
               NBVAL = 2 * NBINST
               CALL WKVECT(NOMFON//'.VALE','V V R',NBVAL,LVAL)
               CALL JEVEUO(NOMFX//'.VALE' ,'L',JPAR)
               LFON = LVAL + NBINST
               JFON = JPAR + NBINST
               IF (PARTIE.EQ.'REEL') THEN
                  DO 10 I = 0,NBINST-1
                     ZR(LVAL+I) = ZR(JPAR+I)
                     ZR(LFON+I) = ZR(JFON+(2*I))
 10               CONTINUE
               ELSE
                  DO 12 I = 0,NBINST-1
                     ZR(LVAL+I) = ZR(JPAR+I)
                     ZR(LFON+I) = ZR(JFON+(2*I)+1)
 12               CONTINUE
               ENDIF
            ELSE
               NOMFON = NOMFX
            ENDIF
 8          CONTINUE
            CALL GETVID('COURBE','LIST_PARA',IC,1,1,LISTR ,IND)
         ELSEIF (NX.NE.0) THEN
            CALL GETVID('COURBE','FONC_X'   ,IC,1,1,NOMFX ,N)
            CALL GETVID('COURBE','FONC_Y'   ,IC,1,1,NOMFY ,N)
            CALL GETVTX('COURBE','PARA'     ,IC,1,1,NOPARA,N)
            CALL GETVID('COURBE','LIST_PARA',IC,1,1,LISTR ,IND)
            NOMFON = '&&COURBE_'//KIOCC
            CALL FOTRAJ(NOPARA,NOMFX,NOMFY,NOMFON,IND,LISTR,IRET)
            IF (IRET.NE.0) GOTO 200
            IND = 0
         ELSEIF (NL.NE.0) THEN
            CALL GETVID('COURBE','LIST_RESU',IC,1,1,LISTRS,N)
            CALL GETVID('COURBE','LIST_PARA',IC,1,1,LISTR ,IND)
            NOMFON = '&&COURBE_'//KIOCC
            CALL WKVECT(NOMFON//'.PROL','V V K16',5,LPRO)
            ZK16(LPRO)   = 'FONCTION'
            ZK16(LPRO+1) = INTERP
            ZK16(LPRO+2) = ' '
            ZK16(LPRO+3) = ' '
            ZK16(LPRO+4) = PROLGD
            CALL JELIRA(LISTR//'.VALE' ,'LONMAX',NBCOUP,K1BID)
            CALL JELIRA(LISTRS//'.VALE','LONMAX',NBCOU2,K1BID)
            IF (NBCOU2.NE.NBCOUP) THEN
               CALL UTMESS('E',NOMCMD,'IL MANQUE DES VALEURS DANS '//
     +                       LISTRS//' ,LISTE PLUS PETITE QUE '//LISTR)
               GOTO 200
            ENDIF
            NBVAL = NBCOUP * 2
            CALL WKVECT(NOMFON//'.VALE','V V R',NBVAL,LVAL)
            CALL JEVEUO(LISTR//'.VALE' ,'L',JPAR)
            CALL JEVEUO(LISTRS//'.VALE','L',JFON)
            LFON = LVAL + NBCOUP
            DO 20 IVAL = 0,NBCOUP-1
               ZR(LVAL+IVAL) = ZR(JPAR+IVAL)
               ZR(LFON+IVAL) = ZR(JFON+IVAL)
 20         CONTINUE
            CALL JELIBE(LISTR//'.VALE')
            CALL JELIBE(LISTRS//'.VALE')
            IND = 0
         ELSEIF (NT.NE.0) THEN
            CALL GETVID('COURBE','TABLE'   ,IC,1,1,NOMTAB,N)
            CALL GETVTX('COURBE','PARA_X'  ,IC,1,1,PARAX ,N)
            CALL GETVTX('COURBE','PARA_Y'  ,IC,1,1,PARAY ,N)
            NOMFON = '&&COURBE_'//KIOCC
            IND = 0
            CALL TBEXFO ( NOMTAB, PARAX,PARAY,NOMFON,INTERP,PROLGD,'V')
         ELSEIF (NR.NE.0) THEN
            CALL GETVID('COURBE','RESU_GENE'    ,IC,1,1, RESU  , N   )
            CALL GETVTX('COURBE','INTITULE'     ,IC,1,1, INTITU, INT )
            CALL GETVID('COURBE','NOEUD_CHOC'   ,IC,1,1, NOEUD , N   )
            CALL GETVID('COURBE','GROUP_NO_CHOC',IC,1,1, NOMGR , NG  )
            IF ( NG .NE. 0 ) THEN
               CALL JEVEUO(RESU//'.REFE','L',JREFE)
               BASEMO = ZK24(JREFE)(1:8)
               CALL JEVEUO(BASEMO//'           .REFE','L',JREFE)
               RAIDE = ZK24(JREFE+2)(1:8)
            CALL DISMOI('F','NOM_MAILLA',RAIDE,'MATR_ASSE',IBID,NOMA,IE)
               CALL UTNONO(' ',NOMA,'NOEUD',NOMGR,NOEUD,IRET)
               IF (IRET.EQ.10) THEN
                  CALL UTMESS('F','OP0141',
     +                        'LE GROUP_NO : '//NOMGR//'N''EXISTE PAS.')
               ELSEIF (IRET.EQ.1) THEN
                  CALL UTDEBM('A','OP0141',
     +                        'TROP DE NOEUDS DANS LE GROUP_NO')
                  CALL UTIMPK('L','  NOEUD UTILISE: ',1,NOEUD)
                  CALL UTFINM( )
               ENDIF
            ENDIF
            CALL GETVTX('COURBE','PARA_X'    ,IC,1,1,PARAX ,N)
            CALL GETVTX('COURBE','PARA_Y'    ,IC,1,1,PARAY ,N)
            CALL GETVID('COURBE','LIST_PARA' ,IC,1,1,LISTR ,IND)
            CALL GETVTX('COURBE','SOUS_STRUC',IC,1,1,SST,NSST)
            NOMFON = '&&COURBE_'//KIOCC
            CALL FOCRCH(NOMFON,RESU,NOEUD,PARAX,PARAY,'V',
     +                  INT,INTITU,IND,LISTR,SST,NSST,IRET)
            IF (IRET.NE.0) GOTO 200
            IND = 0
         ENDIF
C
         IF     ( FORMAT .EQ. 'RESULTAT' ) THEN
            CALL FOIMPR(NOMFON,IMPR,FILE,IND,LISTR)
         ELSEIF ( FORMAT .EQ. 'COMMANDE' ) THEN
            CALL FOECCF(NOMFON,IUL,IND,LISTR)
         ELSEIF ( FORMAT .EQ. 'SEISME'   ) THEN
            CALL FOECFD(NOMFON,IUL,IND,LISTR)
         ELSEIF ( FORMAT .EQ. 'AGRAF'    ) THEN
            CALL FOECGV(NOMFON,IUL,IND,LISTR,0)
         ELSEIF ( FORMAT .EQ. 'EXCEL'    ) THEN
C           STOCKE LA LISTE DES FONCTIONS A IMPRIMER
            ZK24(LEXT+IC-1)=NOMFON
         ELSEIF ( FORMAT .EQ. 'GNUPLOT'  ) THEN
            CALL FOECGV(NOMFON,IUL,IND,LISTR,0)
         ELSEIF ( FORMAT .EQ. 'POSTSCRIPT' ) THEN
         CALL UTMESS('I',NOMCMD,'ATTENTION, IL FAUT UTILISER "ASTERIX"'
     +   //' POUR TRANSFORMER LES COMMANDES "GNUPLOT" EN "POSTSCRIPT".')
            CALL FOECGV(NOMFON,IUL,IND,LISTR,1)
         ELSE
            LG = MAX(1,LXLGUT(FORMAT))
            CALL UTMESS('A',NOMCMD//' (ERREUR 02)',
     +                   'LE FORMAT "'//FORMAT(1:LG)//'" EST INCONNU.')
            GOTO 200
         ENDIF
C
C        SI EXCEL ON UTILISE LES NOMS TEMPORAIRES DANS FOIEXC
         IF(FORMAT.NE.'EXCEL')THEN
            CALL JEDETC('V','&&COURBE',1)
            CALL JEDETC('V','&&PARTIE',1)
         ENDIF
C
 200  CONTINUE
C
      IF(FORMAT.EQ.'EXCEL')THEN
C       FORMAT EXCEL, ON IMPRIME TOUTES LES FONCTIONS EN UNE SEULE FOIS
C        DO 333 IC=1,NBCOUR
C          WRITE(6,*) 'IMPR_COURBE :',ZK24(LEXT+IC-1)
C333     CONTINUE
        CALL FOIEXC(LISFON,FILE,IND,LISTR)
        CALL JEDETR('&&OP0141.TABEXCEL')
        CALL JEDETC('V','&&COURBE',1)
        CALL JEDETC('V','&&PARTIE',1)
      ENDIF
C
 9999 CONTINUE
      CALL JEDEMA()
      END
