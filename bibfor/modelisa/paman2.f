      SUBROUTINE PAMAN2(CHARZ, MOTFAZ, MOCLEZ, NOMAZ, LISTYZ, IOCC,
     +                  LISNOZ, LONLIS, LISQUA, NNOQUA )
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*8      CHARZ
      CHARACTER*(*)    MOTFAZ, MOCLEZ, NOMAZ, LISTYZ, LISNOZ,LISQUA
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 31/07/2001   AUTEUR CIBHHBC R.FERNANDES 
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
C TOLE CRP_20
C
C     CREATION DU VECTEUR DE K8 DE NOM LISNOZ ET DE LONGUEUR
C     LONLIS.
C     CE VECTEUR CONTIENT LA LISTE DES NOMS DES NOEUDS DEFINIS
C     PAR LE MOT-CLE : GROUP_MA_1 OU GROUP_MA_2 OU MAILLE_1
C     OU MAILLE_2 APRES LE MOT-FACTEUR LIAISON_GROUP OU
C     LIAISON_UNIL_NO ,
C                   OU GROUP_NO_1 OU GROUP_NO_2
C     APRES LE MOT-FACTEUR LIAISON_GROUP.
C     CETTE LISTE NE CONTIENT QU'UNE OCCURENCE DES NOEUDS.
C
C IN       : MOTFAZ : MOT-CLE FACTEUR 'LIAISON_GROUP'
C IN       : MOCLEZ : MOT-CLE GROUP_MA_1 OU GROUP_MA_2
C                     OU      MAILLE_1   OU MAILLE_2
C                     OU      GROUP_NO_1 OU GROUP_NO_2
C                     OU      NOEUD_1    OU NOEUD_2
C IN       : NOMAZ  : NOM DU MAILLAGE
C IN       : LISTYZ : LISTE DE NOMS DE TYPES (GEOMETRIQUES) D'ELEMENTS
C                     SI CETTE LISTE N'EST PAS VIDE ON TESTE SI LE
C                     DES ELEMENTS DONNES APRES LES MOTS CLES 
C                     MAILLE_I ET GROUP_MA_I APPARTIENT A CETTE LISTE
C                     SI CE N'EST PAS LE CAS ON S'ARRETE EN ERREUR
C                     FATALE
C IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
C OUT      : LISNOZ : NOM DE LA LISTE DES NOEUDS
C OUT      : LONLIS : LONGUEUR DE LA LISTE DES NOEUDS
C          : LONLIS : <0 SI CONTACT ET POI1
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
      CHARACTER*1   K1BID
      CHARACTER*8   K8BID, NOMA, NOMNOE, NOMAIL, NOMTYP, CARACT, CHAR
      CHARACTER*8   NOMTM, NOMOB, MOTKEY
      CHARACTER*16  NOMTE, MOTFAC, MOTCLE
      CHARACTER*16  MGRMA1, MGRMA2, MGRNO1, MGRNO2
      CHARACTER*19  NOLIG
      CHARACTER*16  MMAIL1, MMAIL2, MNOEU1, MNOEU2
      CHARACTER*24  NOEUMA, GRNOMA, MAILMA, GRMAMA, LISTYP, LISNOE
      INTEGER IOCC,LONLIS,NLIAI,IDIMAX,IDIM1,IDIM2,IDIM3,IDIM4,IDIMP1
      INTEGER N1MAX,NG,JJJ,NGR,IGR,JGRO,NBMAIL,M,NUMAIL,N1,NBMA,NMAI
      INTEGER IMA,N2,N3,NBNO,NNO,IER,JLIST,INDNOE,JDES,INO,IN,JIND,IN1
      INTEGER INDLIS, ELIMQU, I, IATYMA,IBID,II,K,NBNOMI
      INTEGER NUTYMA,NUTYP,NNOQUA,NNOQU0,INDIC,IRET,JQU,JJ2,IPMA
      INTEGER IPQU,JLISQU,ITYP,JTYP,LONLIT,INDICE
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CHAR   = CHARZ
      MOTFAC = MOTFAZ
      NOMA   = NOMAZ
      LISNOE = LISNOZ
      LISTYP = LISTYZ
      MOTCLE = MOCLEZ
      LONLIT = 0
      NNOQUA = 0
C
      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GOTO 9999
C
      CALL JEEXIN(LISTYP,IRET)
      IF (IRET.NE.0) THEN
        CALL JELIRA(LISTYP, 'LONMAX', LONLIT, K1BID)
        CALL JEVEUO(LISTYP, 'L', IDTYPK)
        IF (LONLIT.NE.0) THEN
          CALL JEDETR ('&&PAMAN2.LISTYP')
          CALL WKVECT ('&&PAMAN2.LISTYP','V V I',LONLIT,IDTYPI)
          DO 1 I = 1, LONLIT
            CALL JENONU(JEXNOM('&CATA.TM.NOMTM',ZK8(IDTYPK+I-1)),
     +                  ZI(IDTYPI+I-1))
  1       CONTINUE
          CALL JEVEUO(NOMA//'.TYPMAIL','L',IDTYMA)
        ENDIF
      ENDIF
C
      MGRMA1 = 'GROUP_MA_1'
      MGRMA2 = 'GROUP_MA_2'
      MGRNO1 = 'GROUP_NO_1'
      MGRNO2 = 'GROUP_NO_2'
      MMAIL1 = 'MAILLE_1'
      MMAIL2 = 'MAILLE_2'
      MNOEU1 = 'NOEUD_1'
      MNOEU2 = 'NOEUD_2'
C
      NOEUMA = NOMA//'.NOMNOE'
      GRNOMA = NOMA//'.GROUPENO'
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
C
      LONLIS = 0
      IDIMAX = 0
      IDIM1  = 0
      IDIM2  = 0
      IDIM3  = 0
      IDIM4  = 0
      IDIMP1 = 0
      N1MAX  = 0
      IPQU   = 0
      INDICE = 0

      IF (MOTFAC.EQ.'LIAISON_UNIL_NO') THEN
         CALL GETVTX(MOTFAC,'CONTACT',1,1,1,CARACT, IBID)
         IF (CARACT.EQ.'MAINTENU') INDICE = 1
      ENDIF
C
C     -- TRAITEMENT DES MOTS-CLES GROUP_MA_1 ET GROUP_MA_2
C        -------------------------------------------------------
      IF (MOTCLE.EQ.MGRMA1.OR.MOTCLE.EQ.MGRMA2) THEN
C
C           -- CALCUL DE
C              IDIM1=NB_NOEUD/MAILLE*NB_MAILLE/GROUP_MA*NB_GROUP_MA
C              ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
C              AUX GROUP_MA DU MAILLAGE
C              -----------------------------------------------------
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             NG = -NG
             CALL WKVECT ('&&PAMAN2.TRAV','V V K8',NG,JJJ)
             CALL GETVEM (NOMA,'GROUP_MA',
     .                    MOTFAC,MOTCLE,IOCC,1,NG,ZK8(JJJ),NGR)
             DO 10 IGR = 1, NGR
                CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'L',JGRO)
                CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      NBMAIL,K1BID)
                DO 20 M = 1, NBMAIL
                  NUMAIL = ZI(JGRO-1+M)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  IF (LONLIT.NE.0) THEN
                    NUTYMA = ZI(IDTYMA+NUMAIL-1)
                    INDIC = 0
                    DO 21 I = 1, LONLIT
                     IF (NUTYMA.EQ.ZI(IDTYPI+I-1)) THEN
                       INDIC = 1
                       GO TO 22
                     ENDIF
 21                 CONTINUE
 22                 CONTINUE
                    IF (INDIC.EQ.0) THEN
                      CALL UTMESS('F','PAMAN2_1','LA MAILLE '//NOMAIL
     +               //' DU GROUP_MA '//ZK8(JJJ+IGR-1)
     +               //'DONNE APRES LE MOT CLE '//MOTCLE//'N''A '
     +               //'PAS UN TYPE GEOMETRIQUE AUTORISE.')
                    ENDIF
                  ENDIF
 20            CONTINUE
 10          CONTINUE
             MOTKEY = 'GROUP_MA'
             CALL NBNOEL(CHAR,NOMA,MOTKEY,NGR,ZK8(JJJ),INDICE,
     +                   NBMA,NBNO,NNOQUA)
             IDIM1 = IDIM1 + NBNO
             N1MAX = MAX ( N1MAX, NBNO)
             CALL WKVECT ('&&PAMAN2.TRAV2','V V I' ,NBMA,JJ2)
         ENDIF
         IF ((N1MAX.EQ.1).AND.(MOTFAC.EQ.'LIAISON_UNIL_NO')) THEN
            IDIMP1 = 1
         ENDIF
C
C     -- TRAITEMENT DES MOTS-CLES MAILLE_1 ET MAILLE_2
C        -------------------------------------------------------
      ELSEIF (MOTCLE.EQ.MMAIL1.OR.MOTCLE.EQ.MMAIL2) THEN
C
C           -- CALCUL DE
C              IDIM2=NB_NOEUD/MAILLE*NB_MAILLE DE LISTE DE MAILLES
C              ET VERIFICATION DE L'APPARTENANCE DES MAILLES
C              AUX MAILLES DU MAILLAGE
C              ----------------------------------------------------
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
             NBMA = -NBMA
             CALL WKVECT ('&&PAMAN2.TRAV' ,'V V K8',NBMA,JJJ)
             CALL WKVECT ('&&PAMAN2.TRAV2','V V I' ,NBMA,JJ2)
             CALL GETVEM (NOMA,'MAILLE',
     .                    MOTFAC,MOTCLE,IOCC,1,NBMA,ZK8(JJJ),NMAI)
             DO 30 IMA = 1, NMAI
               IF (LONLIT.NE.0) THEN
                 CALL JENONU(JEXNOM(MAILMA,ZK8(JJJ+IMA-1)),NUMAIL)
                 NUTYMA = ZI(IDTYMA+NUMAIL-1)
                 INDIC = 0
                 DO 31 I = 1, LONLIT
                   IF (NUTYMA.EQ.ZI(IDTYPI+I-1)) THEN
                     INDIC = 1
                     GO TO 32
                   ENDIF
 31              CONTINUE
 32              CONTINUE
                 IF (INDIC.EQ.0) THEN
                   CALL UTMESS('F','PAMAN2_2','LA MAILLE '//
     +                  ZK8(JJJ+IMA-1)
     +               //'DONNE APRES LE MOT CLE '//MOTCLE//'N''A '
     +               //'PAS UN TYPE GEOMETRIQUE AUTORISE.')
                  ENDIF
               ENDIF
 30          CONTINUE
             MOTKEY = 'MAILLE'
             CALL NBNOEL(CHAR,NOMA,MOTKEY,0,ZK8(JJJ),INDICE,
     +                   NMAI,NBNO,NNOQUA)
             NBMA = NMAI
             IDIM2 = IDIM2 + NBNO
             N1MAX = MAX ( N1MAX, NBNO)
         ENDIF
         IF ((N1MAX.EQ.1).AND.(MOTFAC.EQ.'LIAISON_UNIL_NO')) THEN
            IDIMP1 = 1
         ENDIF
C
C     -- TRAITEMENT DES MOTS-CLES GROUP_NO_1 ET GROUP_NO_2
C        -------------------------------------------------------
      ELSEIF (MOTCLE.EQ.MGRNO1.OR.MOTCLE.EQ.MGRNO2) THEN
C
C           -- CALCUL DE
C              IDIM3 = NB_NOEUD/GROUP_NO*NB_GROUP_NO
C              ET VERIFICATION DE L'APPARTENANCE DES GROUP_NO
C              AUX GROUP_NO DU MAILLAGE
C              ------------------------------------------------
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             NG = -NG
             CALL WKVECT ('&&PAMAN2.TRAV','V V K8',NG,JJJ)
             CALL GETVEM (NOMA,'GROUP_NO',
     .                    MOTFAC,MOTCLE,IOCC,1,NG,ZK8(JJJ),NGR)
             DO 40 IGR = 1, NGR
               CALL JELIRA (JEXNOM(GRNOMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      N3,K1BID)
               IDIM3 = IDIM3 + N3
 40          CONTINUE
         ENDIF
C
C     -- TRAITEMENT DES MOTS-CLES NOEUD_1 ET NOEUD_2
C        -------------------------------------------------------
      ELSEIF (MOTCLE.EQ.MNOEU1.OR.MOTCLE.EQ.MNOEU2) THEN
C
C           -- CALCUL DE
C              IDIM4 = NB_NOEUD DE LA LISTE DE NOEUDS
C              ET VERIFICATION DE L'APPARTENANCE DES NOEUDS
C              AUX NOEUDS DU MAILLAGE
C              ---------------------------------------------
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NBNO)
         IF (NBNO.NE.0) THEN
             NBNO = -NBNO
             CALL WKVECT ('&&PAMAN2.TRAV','V V K8',NBNO,JJJ)
             CALL GETVEM (NOMA,'NOEUD',
     .                    MOTFAC,MOTCLE,IOCC,1,NBNO,ZK8(JJJ),NNO)
             IDIM4 = IDIM4 + NNO
      ENDIF
C
C     -- MOTCLE NON ADMIS
C        -------------------------------------------------------
      ELSE
         CALL UTMESS('F','PAMAN2_3',' MOT CLE NON ADMIS :'//MOTCLE//
     +                ' LES MOTS-CLES ADMISSIBLES SONT : '//MGRMA1//
     +                ' OU '//MGRMA2//' OU '//MGRNO1//' OU '//
     +                MGRNO2//' OU '//MMAIL1//' OU '//MMAIL2//
     +                ' OU '//MNOEU1// 'OU '//MNOEU2)
      ENDIF
C
C     -- IDIMAX = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS
C    -----------------------------------------------------------
      IDIMAX = MAX(IDIM1,IDIMAX)
      IDIMAX = MAX(IDIM2,IDIMAX)
      IDIMAX = MAX(IDIM3,IDIMAX)
      IDIMAX = MAX(IDIM4,IDIMAX)
C
      IF (IDIMAX.EQ.0) GOTO 9999
C
C     -- ALLOCATION DES TABLEAUX DES NOMS DE NOEUDS
C    ----------------------------------------------
      CALL JEEXIN(LISNOE, IER)
      IF (IER.NE.0) THEN
          CALL JEDETR(LISNOE)
      ENDIF
      CALL WKVECT ('&&PAMAN2.LISTE','V V I',IDIMAX,ILIST)
      CALL WKVECT (LISNOE,'V V K8',IDIMAX,JLIST)
C
      IF (NNOQUA.NE.0) THEN
          CALL WKVECT ('&&PAMAN2.NOQU','V V I',3*NNOQUA,JLISQU)
      ENDIF
C
      INDNOE = 0
      IPNO = 0
      IPQU = 0
C
C     --  MOTS-CLES GROUP_MA_1 ET GROUP_MA_2
C        -------------------------------------------------------
      IF (MOTCLE.EQ.MGRMA1.OR.MOTCLE.EQ.MGRMA2) THEN
C
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
            NG = -NG
            MOTKEY = 'GROUP_MA'
            IBID = 0
            IPMA = 0
            CALL GETVID (MOTFAC,MOTCLE,IOCC,1,NG,ZK8(JJJ),NGR)
            CALL EXNOEL(CHAR,NOMA,MOTKEY,NGR,ZK8(JJJ),
     +                  IBID,NBNO,NNOQUA,ZI(JJ2),ZI(ILIST),
     +                  ZI(JLISQU), IPMA, IPNO, IPQU )
      IF (IPNO.NE.NBNO)   CALL UTMESS('F','PAMAN2_01','ERREUR SUR IPNO')
      IF (IPQU.NE.NNOQUA) CALL UTMESS('F','PAMAN2_02','ERREUR SUR IPQU')
            DO 70 INO = 1, IPNO
               CALL JENUNO(JEXNUM(NOEUMA,ZI(ILIST-1+INO)),NOMNOE)
               INDNOE = INDNOE + 1
               ZK8(JLIST+INDNOE-1) = NOMNOE
 70         CONTINUE
         ENDIF
C
C     --  MOTS-CLES MAILLE_1 ET MAILLE_2
C        -------------------------------------------------------
      ELSEIF (MOTCLE.EQ.MMAIL1.OR.MOTCLE.EQ.MMAIL2) THEN
C
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NBMA)
         IF (NBMA.NE.0) THEN
            MOTKEY = 'MAILLE'
            IBID = 0
            IPMA = 0
            NBMA = -NBMA
            CALL GETVID (MOTFAC,MOTCLE,IOCC,1,NBMA,ZK8(JJJ),NMAI)
            CALL EXNOEL(CHAR,NOMA,MOTKEY,IBID,ZK8(JJJ),
     +                   NMAI,NBNO,NNOQUA,ZI(JJ2),ZI(ILIST),
     +                   ZI(JLISQU), IPMA, IPNO, IPQU )
      IF (IPNO.NE.NBNO)   CALL UTMESS('F','PAMAN2_03','ERREUR SUR IPNO')
      IF (IPQU.NE.NNOQUA) CALL UTMESS('F','PAMAN2_04','ERREUR SUR IPQU')
            DO 90 INO = 1, IPNO
               CALL JENUNO(JEXNUM(NOEUMA,ZI(ILIST-1+INO)),NOMNOE)
               INDNOE = INDNOE + 1
               ZK8(JLIST+INDNOE-1) = NOMNOE
 90         CONTINUE
         ENDIF
C
C     --  MOTS-CLES GROUP_NO_1 ET GROUP_NO_2
C        -------------------------------------------------------
      ELSEIF (MOTCLE.EQ.MGRNO1.OR.MOTCLE.EQ.MGRNO2) THEN
C
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NG)
         IF (NG.NE.0) THEN
             NG = -NG
             CALL GETVID (MOTFAC,MOTCLE,IOCC,1,NG,ZK8(JJJ),NGR)
             DO 100 IGR = 1, NGR
               CALL JEVEUO (JEXNOM(GRNOMA,ZK8(JJJ+IGR-1)),'L',JGRO)
               CALL JELIRA (JEXNOM(GRNOMA,ZK8(JJJ+IGR-1)),'LONMAX',
     +                      N3,K1BID)
               DO 110 INO = 1, N3
                  IN = ZI(JGRO+INO-1)
                  INDNOE = INDNOE + 1
                  CALL JENUNO(JEXNUM(NOEUMA,IN),NOMNOE)
                  ZK8(JLIST+INDNOE-1) = NOMNOE
 110           CONTINUE
 100         CONTINUE
         ENDIF
C
C     --  MOTS-CLES NOEUD_1 ET NOEUD_2
C        -------------------------------------------------------
      ELSEIF (MOTCLE.EQ.MNOEU1.OR.MOTCLE.EQ.MNOEU2) THEN
C
         CALL GETVID (MOTFAC,MOTCLE,IOCC,1,0,K8BID,NBNO)
         IF (NBNO.NE.0) THEN
             NBNO = -NBNO
             CALL GETVID (MOTFAC,MOTCLE,IOCC,1,NBNO,ZK8(JJJ),NNO)
             DO 120 INO = 1, NNO
                  INDNOE = INDNOE + 1
                  ZK8(JLIST+INDNOE-1) = ZK8(JJJ+INO-1)
 120         CONTINUE
         ENDIF
C
      ENDIF
C
C     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
C        DE LA LISTE
C    -------------------------------------------------------------
      CALL WKVECT ('&&PAMAN2.INDICE','V V I',IDIMAX,JIND)
C
      DO 130 INO = 1, IDIMAX
          DO 140 IN1 = INO+1, IDIMAX
                IF (ZK8(JLIST+IN1-1).EQ.ZK8(JLIST+INO-1)) THEN
                      ZI(JIND+IN1-1) = 1
                ENDIF
 140      CONTINUE
 130   CONTINUE
C
      INDLIS = 0
      DO 150 INO = 1, IDIMAX
         IF (ZI(JIND+INO-1).EQ.0) THEN
              INDLIS = INDLIS + 1
              ZK8(JLIST+INDLIS-1) = ZK8(JLIST+INO-1)
         ENDIF
 150  CONTINUE
C
      LONLIS = INDLIS
      IF (IDIMP1.GT.0) THEN
         LONLIS = - LONLIS
      ENDIF
C
      IF (NNOQUA.NE.0) THEN
        CALL WKVECT ('&&PAMAN2.INDQU','V V I',NNOQUA,JINDQ)
        ELIMQU = 0
      IF (IPQU.NE.NNOQUA) CALL UTMESS('F','PAMAN2_4','ERREUR SUR IPQU')
        IPQU = 0
        DO 160 INO = 1,NNOQUA
           ZI(JINDQ+INO-1) = 0
           DO 170 II = 1,INO-1
               IF(ZI(JLISQU+3*(INO-1)).EQ.ZI(JLISQU+3*(II-1))) THEN
                  ELIMQU = ELIMQU + 1
                  ZI(JINDQ+INO-1) = 1
               ENDIF
 170       CONTINUE
 160    CONTINUE
        NNOQU0 = NNOQUA - ELIMQU
        CALL WKVECT (LISQUA,'V V I',3*NNOQU0,JQU)
        DO 180 INO = 1, NNOQUA
           IF (ZI(JINDQ+INO-1).NE.1) THEN
           IPQU = IPQU + 1
           DO 190 K = 1,3
              ZI(JQU+3*(IPQU-1)+K-1) = ZI(JLISQU+3*(INO-1)+K-1)
 190       CONTINUE
           ENDIF
 180    CONTINUE
       IF(IPQU.NE.NNOQU0) CALL UTMESS('F','PAMAN2_5','ERREUR SUR IPQU')
        NNOQUA = NNOQU0
      ENDIF
C
 9999 CONTINUE
C
      CALL JEDETR ('&&PAMAN2.NOQU'  )
      CALL JEDETR ('&&PAMAN2.LISTE' )
      CALL JEDETR ('&&PAMAN2.INDQU' )
      CALL JEDETR ('&&PAMAN2.TRAV'  )
      CALL JEDETR ('&&PAMAN2.TRAV2' )
      CALL JEDETR ('&&PAMAN2.INDICE')
      CALL JEDETR ('&&PAMAN2.LISTYP')
C
      CALL JEDEMA()
      END
