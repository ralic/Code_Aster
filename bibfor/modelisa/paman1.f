      SUBROUTINE PAMAN1(MOTFAZ, MOCLEZ, NOMAZ, IOCC, LISN1Z, LONLI1,
     +                  LISN2Z, LONLI2)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     MOTFAZ, MOCLEZ, NOMAZ, LISN1Z, LISN2Z
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 16/05/2001   AUTEUR CIBHHBC N.SELLALI 
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
C
C     CREATION DES VECTEURS DE K8 :
C                        .DE NOM LISNO1 ET DE LONGUEUR LONLI1
C                        .DE NOM LISNO2 ET DE LONGUEUR LONLI2
C
C     LISNO1 CONTIENT LA LISTE DES NOMS DES NOEUDS SOMMETS
C     LISNO2 CONTIENT LA LISTE DES NOMS DES NOEUDS MILIEUX
C        DES MAILLES DEFINIES PAR LE MOT-CLE :
C                   GROUP_MA_1 OU GROUP_MA_2 OU MAILLE_1
C        OU MAILLE_2 APRES LE MOT-FACTEUR LIAISON_UNIL_NO
C
C     CES LISTES NE CONTIENNENT QU'UNE OCCURENCE DES NOEUDS.
C
C IN       : MOTFAZ : MOT-CLE FACTEUR 'LIAISON_GROUP'
C IN       : MOCLEZ : MOT-CLE GROUP_MA_1 OU GROUP_MA_2
C                     OU      MAILLE_1   OU MAILLE_2
C                     OU      GROUP_NO_1 OU GROUP_NO_2
C                     OU      NOEUD_1    OU NOEUD_2
C IN       : NOMAZ  : NOM DU MAILLAGE
C IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
C OUT      : LISN1Z : NOM DE LA LISTE DES NOEUDS SOMMETS
C OUT      : LONLI1 : LONGUEUR DE LA LISTE DES NOEUDS SOMMETS
C OUT      : LISN2Z : NOM DE LA LISTE DES NOEUDS MILIEUX
C OUT      : LONLI2 : LONGUEUR DE LA LISTE DES NOEUDS MILIEUX
C ----------------------------------------------------------------------
C     ----------- COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER           ZI
      COMMON / IVARJE / ZI(1)
      REAL*8            ZR
      COMMON / RVARJE / ZR(1)
      COMPLEX*16        ZC
      COMMON / CVARJE / ZC(1)
      LOGICAL           ZL
      COMMON / LVARJE / ZL(1)
      CHARACTER*8       ZK8
      CHARACTER*16              ZK16
      CHARACTER*24                       ZK24
      CHARACTER*32                                ZK32
      CHARACTER*80                                         ZK80
      COMMON / KVARJE / ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
      CHARACTER*32      JEXNOM, JEXNUM
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      CHARACTER*8   K8BID, NOMA, NOMNOE, NOMAIL, NOMTYP
      CHARACTER*16  MOTFAC, MOTCLE
      CHARACTER*16  MGRMA1, MGRMA2
      CHARACTER*16  MMAIL1, MMAIL2
      CHARACTER*24  NOEUMA, MAILMA, GRMAMA, LISNO1, LISNO2
      CHARACTER*1   K1BID
      INTEGER       NBNOMI
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      MOTFAC = MOTFAZ
      NOMA   = NOMAZ
      LISNO1 = LISN1Z
      LISNO2 = LISN2Z
      MOTCLE = MOCLEZ
C
      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GOTO 9999
C
      MGRMA1 = 'GROUP_MA_1'
      MGRMA2 = 'GROUP_MA_2'
      MMAIL1 = 'MAILLE_1'
      MMAIL2 = 'MAILLE_2'
C
      NOEUMA = NOMA//'.NOMNOE'
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
C
      IDIMA1 = 0
      IDIMA2 = 0
      IDIM1  = 0
      IDIM2  = 0
      IDIM3  = 0
      IDIM4  = 0
C
C     -- TRAITEMENT DES MOTS-CLES GROUP_MA_1 ET GROUP_MA_2
C        -------------------------------------------------
      IF (MOTCLE.EQ.MGRMA1.OR.MOTCLE.EQ.MGRMA2) THEN
C
C           -- CALCUL DE
C              IDIM1=NB_NOEUD/MAILLE*NB_MAILLE/GROUP_MA*NB_GROUP_MA
C              ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
C              AUX GROUP_MA DU MAILLAGE
C              ------------------------
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             NG = -NG
             CALL WKVECT ('&&PAMANO.TRAV','V V K8',NG,JJJ)
             CALL GETVEM (NOMA,'GROUP_MA',
     .                    MOTFAC,MOTCLE,IOCC,1,NG,ZK8(JJJ),NGR)
             DO 10 IGR = 1, NGR
                CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'L',JGRO)
                CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      NBMAIL,K1BID)
                DO 20 M = 1, NBMAIL
                  NUMAIL = ZI(JGRO-1+M)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JEVEUO (NOMA//'.TYPMAIL','L',IATYMA)
                  JTYP=IATYMA-1+IBID
                  ITYP = ZI(JTYP)
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
                  IF (NOMTYP.EQ.'QUAD8'.OR.NOMTYP.EQ.'TRIA6') THEN
                      CALL UTMESS('F','PAMAN1_1','LE MOT-CLE'//
     +                ' N EST PAS VALIDE EN PRESENCE D ELEMENT'//
     +                ' DE TYPE '//NOMTYP)
                  ENDIF
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',
     +                         N1,K1BID)
                  NBNOM1 = NBNOMI(NOMTYP)
                  NBNOS1 = N1 - NBNOM1
                  IDIM1  = IDIM1 + NBNOS1
                  IDIM2  = IDIM2 + NBNOM1
 20            CONTINUE
 10          CONTINUE
         ENDIF
C
C     -- TRAITEMENT DES MOTS-CLES MAILLE_1 ET MAILLE_2
C        ---------------------------------------------
      ELSEIF (MOTCLE.EQ.MMAIL1.OR.MOTCLE.EQ.MMAIL2) THEN
C
C           -- CALCUL DE
C              IDIM2=NB_NOEUD/MAILLE*NB_MAILLE DE LISTE DE MAILLES
C              ET VERIFICATION DE L'APPARTENANCE DES MAILLES
C              AUX MAILLES DU MAILLAGE
C              -----------------------
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
             NBMA = -NBMA
             CALL WKVECT ('&&PAMANO.TRAV','V V K8',NBMA,JJJ)
             CALL GETVEM (NOMA,'MAILLE',
     .                    MOTFAC,MOTCLE,IOCC,1,NBMA,ZK8(JJJ),NMAI)
             DO 30 IMA = 1, NMAI
                 CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+IMA-1)),
     &                       IBID)
                 CALL JEVEUO (NOMA//'.TYPMAIL','L',IATYMA)
                 JTYP=IATYMA-1+IBID
                 ITYP = ZI(JTYP)
                 CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
                 IF (NOMTYP.EQ.'QUAD8'.OR.NOMTYP.EQ.'TRIA6') THEN
                     CALL UTMESS('F','PAMAN1_2','LE MOT-CLE'//
     +               ' N EST PAS VALIDE EN PRESENCE D ELEMENT'//
     +               ' DE TYPE '//NOMTYP)
                 ENDIF
                 CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+IMA-1)),
     &                       IBID)
                 CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),
     +                       'LONMAX',  N2,K1BID)
                 NBNOM2 = NBNOMI(NOMTYP)
                 NBNOS2 = N2 - NBNOM2
                 IDIM3  = IDIM3 + NBNOS2
                 IDIM4  = IDIM4 + NBNOM2
 30          CONTINUE
         ENDIF
C
C     -- MOTCLE NON ADMIS
C        ----------------
      ELSE
         CALL UTMESS('F','PAMAN1_3',' MOT CLE NON ADMIS :'//MOTCLE//
     +                ' LES MOTS-CLES ADMISSIBLES SONT : '//MGRMA1//
     +                ' OU '//MGRMA2//' OU '//MMAIL1//' OU '//
     +                MMAIL2)
      ENDIF
C
C     -- IDIMA1 = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS SOMMETS
C     -- IDIMA2 = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS MILIEUX
C        --------------------------------------------------------------
      IDIMA1 = MAX(IDIM1,IDIMA1)
      IDIMA2 = MAX(IDIM2,IDIMA2)
      IDIMA1 = MAX(IDIM3,IDIMA1)
      IDIMA2 = MAX(IDIM4,IDIMA2)
C
C     -- ALLOCATION DES TABLEAUX DES NOMS DE NOEUDS
C    ----------------------------------------------
      CALL JEEXIN(LISNO1, IER)
      IF (IER.NE.0) THEN
          CALL JEDETR(LISNO1)
      ENDIF
      CALL JEEXIN(LISNO2, IER)
      IF (IER.NE.0) THEN
          CALL JEDETR(LISNO2)
      ENDIF
      CALL WKVECT (LISNO1,'V V K8',IDIMA1,JLIST1)
      CALL WKVECT (LISNO2,'V V K8',IDIMA2,JLIST2)
C
      INDNO1 = 0
      INDNO2 = 0
C
C     --  MOTS-CLES GROUP_MA_1 ET GROUP_MA_2
C        -----------------------------------
      IF (MOTCLE.EQ.MGRMA1.OR.MOTCLE.EQ.MGRMA2) THEN
C
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             NG = -NG
             CALL GETVID (MOTFAC,MOTCLE,IOCC,1,NG,ZK8(JJJ),NGR)
             DO 40 IGR = 1, NGR
               CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'L',JGRO)
               CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      NBMAIL,K1BID)
               DO 50 M = 1, NBMAIL
                  NUMAIL = ZI(JGRO-1+M)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JEVEUO (JEXNUM(NOMA//'.CONNEX',IBID),'L',JDES)
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JEVEUO (NOMA//'.TYPMAIL','L',IATYMA)
                  JTYP=IATYMA-1+IBID
                  ITYP = ZI(JTYP)
                  CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',
     +                         N1,K1BID)
                  NBNOML = NBNOMI(NOMTYP)
                  NBNOSO = N1 - NBNOML
                  DO 60 INO = 1, NBNOSO
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(JDES+INO-1)),NOMNOE)
                    INDNO1 = INDNO1 + 1
                    ZK8(JLIST1+INDNO1-1) = NOMNOE
 60              CONTINUE
                  DO 70 INO = NBNOSO+1, N1
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(JDES+INO-1)),NOMNOE)
                    INDNO2 = INDNO2 + 1
                    ZK8(JLIST2+INDNO2-1) = NOMNOE
 70              CONTINUE
 50            CONTINUE
 40         CONTINUE
         ENDIF
C
C     --  MOTS-CLES MAILLE_1 ET MAILLE_2
C        -------------------------------
      ELSEIF (MOTCLE.EQ.MMAIL1.OR.MOTCLE.EQ.MMAIL2) THEN
C
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
             NBMA = -NBMA
             CALL GETVID (MOTFAC,MOTCLE,IOCC,1,NBMA,ZK8(JJJ),NMAI)
             DO 80 IMA = 1, NMAI
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+IMA-1)),IBID)
                CALL JEVEUO (JEXNUM(NOMA//'.CONNEX',IBID),
     +                       'L',JDES)
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+IMA-1)),IBID)
                CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),
     +                       'LONMAX',  N2,K1BID)
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ+IMA-1)),IBID)
                CALL JEVEUO (NOMA//'.TYPMAIL','L',IATYMA)
                JTYP=IATYMA-1+IBID
                ITYP = ZI(JTYP)
                CALL JENUNO(JEXNUM('&CATA.TM.NOMTM',ITYP),NOMTYP)
                NBNOML = NBNOMI(NOMTYP)
                NBNOSO = N2 - NBNOML
                DO 90 INO = 1, NBNOSO
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(JDES+INO-1)),NOMNOE)
                    INDNO1 = INDNO1 + 1
                    ZK8(JLIST1+INDNO1-1) = NOMNOE
 90             CONTINUE
                DO 100 INO = NBNOSO + 1, N2
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(JDES+INO-1)),NOMNOE)
                    INDNO2 = INDNO2 + 1
                    ZK8(JLIST2+INDNO2-1) = NOMNOE
 100            CONTINUE
 80         CONTINUE
         ENDIF
C
      ENDIF
C
C     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
C        DES LISTES LISNO1 (NOEUDS SOMMETS) ET LISNO2 (NOEUDS MILIEUX)
C    -----------------------------------------------------------------
      IDIMAX = 0
      IDIMAX = MAX(IDIMA1,IDIMAX)
      IDIMAX = MAX(IDIMA2,IDIMAX)
      CALL WKVECT ('&&PAMANO.INDICE','V V I',IDIMAX,JIND)
C
      DO 110 INO = 1, IDIMA1
          DO 120 IN1 = INO+1, IDIMA1
                IF (ZK8(JLIST1+IN1-1).EQ.ZK8(JLIST1+INO-1)) THEN
                      ZI(JIND+IN1-1) = 1
                ENDIF
 120      CONTINUE
 110   CONTINUE
C
      INDLIS = 0
      DO 130 INO = 1, IDIMA1
         IF (ZI(JIND+INO-1).EQ.0) THEN
              INDLIS = INDLIS + 1
              ZK8(JLIST1+INDLIS-1) = ZK8(JLIST1+INO-1)
         ENDIF
 130  CONTINUE
C
      LONLI1 = INDLIS
C
      DO 140 I = 1, IDIMAX
          ZI(JIND+I-1) = 0
 140  CONTINUE
C
      DO 150 INO = 1, IDIMA2
          DO 160 IN1 = INO+1, IDIMA2
                IF (ZK8(JLIST2+IN1-1).EQ.ZK8(JLIST2+INO-1)) THEN
                      ZI(JIND+IN1-1) = 1
                ENDIF
 160      CONTINUE
 150   CONTINUE
C
      INDLIS = 0
      DO 170 INO = 1, IDIMA2
         IF (ZI(JIND+INO-1).EQ.0) THEN
              INDLIS = INDLIS + 1
              ZK8(JLIST2+INDLIS-1) = ZK8(JLIST2+INO-1)
         ENDIF
 170  CONTINUE
C
      LONLI2 = INDLIS
C
      CALL JEDETR ('&&PAMANO.TRAV')
      CALL JEDETR ('&&PAMANO.INDICE')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
