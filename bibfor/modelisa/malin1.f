      SUBROUTINE MALIN1(MOTFAZ, CHARGZ, IOCC, INDMOT, LISNOZ, LONLIS)
      IMPLICIT REAL*8 (A-H,O-Z)
      CHARACTER*(*)     MOTFAZ, CHARGZ, LISNOZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 21/09/2011   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
C     CREATION DU VECTEUR DE K8 DE NOM LISNOZ ET DE LONGUEUR
C     LONLIS.
C     CE VECTEUR CONTIENT LA LISTE DES NOMS DES NOEUDS DEFINIS
C     PAR LES MOTS-CLES : GROUP_MA OU MAILLE
C     APRES LE MOT-FACTEUR LIAISON_ELEM.
C     CETTE LISTE NE CONTIENT QU'UNE OCCURENCE DES NOEUDS.
C
C IN       : MOTFAZ : MOT-CLE FACTEUR 'LIAISON_ELEM'
C IN       : CHARGZ : NOM D'UNE SD CHARGE
C IN       : IOCC   : NUMERO D'OCCURENCE DU MOT-FACTEUR
C IN       : INDMOT : INDICE = 0 --> TRAITEMENT DES MOTS-CLES
C                                    'GROUP_MA' OU 'MAILLE'
C                            = 1 --> TRAITEMENT DES MOTS-CLES
C                                     'GROUP_MA_1' OU 'MAILLE_1'
C                            = 2 --> TRAITEMENT DES MOTS-CLES
C                                     'GROUP_MA_2' OU 'MAILLE_2
C OUT      : LISNOZ : NOM DE LA LISTE DES NOEUDS
C OUT      : LONLIS : LONGUEUR DE LA LISTE DES NOEUDS
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
      CHARACTER*8   CHARGE
      CHARACTER*8   K8BID, NOMA, NOMNOE, NOMAIL
      CHARACTER*16  MOMAIL, MOGRMA
      CHARACTER*16  MOTFAC
      CHARACTER*24   NOEUMA, MAILMA, GRMAMA, LISNOE
      CHARACTER*1 K1BID
      INTEGER      IARG
C ----------------------------------------------------------------------
C
      CALL JEMARQ()
      CHARGE = CHARGZ
      MOTFAC = MOTFAZ
      LISNOE = LISNOZ
C
      IF (INDMOT.EQ.0) THEN
         MOMAIL = 'MAILLE'
         MOGRMA = 'GROUP_MA'
      ELSEIF (INDMOT.EQ.1) THEN
         MOMAIL = 'MAILLE_1'
         MOGRMA = 'GROUP_MA_1'
      ELSEIF (INDMOT.EQ.2) THEN
         MOMAIL = 'MAILLE_2'
         MOGRMA = 'GROUP_MA_2'
      ENDIF
C
      CALL GETFAC(MOTFAC,NLIAI)
      IF (NLIAI.EQ.0) GOTO 9999
C
      CALL DISMOI('F','NOM_MAILLA',CHARGE,'CHARGE',IBID,NOMA,IER)
C
      NOEUMA = NOMA//'.NOMNOE'
      MAILMA = NOMA//'.NOMMAI'
      GRMAMA = NOMA//'.GROUPEMA'
C
      IDIMAX = 0
      IDIM1  = 0
      IDIM2  = 0
C
C     -- CALCUL DE IDIM1=NB_NOEUD/MAILLE*NB_MAILLE/GROUP_MA*NB_GROUP_MA
C        ET VERIFICATION DE L'APPARTENANCE DES GROUP_MA
C        AUX GROUP_MA DU MAILLAGE
C        -------------------------------------------------------
      CALL GETVTX (MOTFAC,MOGRMA,IOCC,IARG,0,K8BID,NG)
      IF (NG.NE.0) THEN
          NG = -NG
          CALL WKVECT ('&&MALIN1.TRAV1','V V K8',NG,JJJ1)
          CALL GETVEM (NOMA,'GROUP_MA',
     .                 MOTFAC,MOGRMA,IOCC,IARG,NG,ZK8(JJJ1),NGR)
          DO 10 IGR = 1, NGR
                CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'L',JGRO)
                CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'LONUTI',
     +                      NBMAIL,K1BID)
                DO 20 M = 1, NBMAIL
                  NUMAIL = ZI(JGRO-1+M)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',
     +                         N1,K1BID)
                  IDIM1 = IDIM1 + N1
 20            CONTINUE
 10         CONTINUE
      ENDIF
C
C     -- CALCUL DE IDIM2=NB_NOEUD/MAILLE*NB_MAILLE DE LISTE DE MAILLES
C        ET VERIFICATION DE L'APPARTENANCE DES MAILLES
C        AUX MAILLES DU MAILLAGE
C        -------------------------------------------------------
      CALL GETVTX (MOTFAC,MOMAIL,IOCC,IARG,0,K8BID,NBMA)
      IF (NBMA.NE.0) THEN
          NBMA = -NBMA
          CALL WKVECT ('&&MALIN1.TRAV2','V V K8',NBMA,JJJ2)
          CALL GETVEM (NOMA,'MAILLE',
     .                 MOTFAC,MOMAIL,IOCC,IARG,NBMA,ZK8(JJJ2),NMAI)
          DO 30 IMA = 1, NMAI
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ2+IMA-1)),
     &                      IBID)
                CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),
     +                       'LONMAX',  N2,K1BID)
                IDIM2 = IDIM2 + N2
 30      CONTINUE
      ENDIF
C
C     -- IDIMAX = MAJORANT DE LA LONGUEUR DE LA LISTE DE NOEUDS
C    ----------------------------------------------------------
      IDIMAX = IDIM1 + IDIM2
C
C     -- ALLOCATION DU TABLEAU DES NOMS DE NOEUDS
C    ----------------------------------------------
      CALL WKVECT (LISNOE,'V V K8',IDIMAX,JLIST)
C
      INDNOE = 0
C
      CALL GETVTX (MOTFAC,MOGRMA,IOCC,IARG,0,K8BID,NG)
      IF (NG.NE.0) THEN
          NG = -NG
          CALL GETVTX (MOTFAC,MOGRMA,IOCC,IARG,NG,ZK8(JJJ1),NGR)
          DO 40 IGR = 1, NGR
               CALL JEVEUO (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'L',JGRO)
               CALL JELIRA (JEXNOM(GRMAMA,ZK8(JJJ1+IGR-1)),'LONUTI',
     +                      NBMAIL,K1BID)
               DO 50 M = 1, NBMAIL
                  NUMAIL = ZI(JGRO-1+M)
                  CALL JENUNO(JEXNUM(MAILMA,NUMAIL),NOMAIL)
                  CALL JENONU(JEXNOM(NOMA//'.NOMMAI',NOMAIL),IBID)
                  CALL JEVEUO (JEXNUM(NOMA//'.CONNEX',IBID),'L',JDES)
                  CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),'LONMAX',
     +                         N1,K1BID)
                  DO 60 INO = 1, N1
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(JDES+INO-1)),NOMNOE)
                    INDNOE = INDNOE + 1
                    ZK8(JLIST+INDNOE-1) = NOMNOE
 60              CONTINUE
 50           CONTINUE
 40       CONTINUE
      ENDIF
C
      CALL GETVTX (MOTFAC,MOMAIL,IOCC,IARG,0,K8BID,NBMA)
      IF (NBMA.NE.0) THEN
          NBMA = -NBMA
          CALL GETVTX (MOTFAC,MOMAIL,IOCC,IARG,NBMA,ZK8(JJJ2),NMAI)
          DO 70 IMA = 1, NMAI
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ2+IMA-1)),
     &                      IBID)
                CALL JEVEUO (JEXNUM(NOMA//'.CONNEX',IBID),
     +                       'L',JDES)
                CALL JENONU(JEXNOM(NOMA//'.NOMMAI',ZK8(JJJ2+IMA-1)),
     &                      IBID)
                CALL JELIRA (JEXNUM(NOMA//'.CONNEX',IBID),
     +                       'LONMAX',  N2,K1BID)
                DO 80 INO = 1, N2
                    CALL JENUNO(JEXNUM(NOEUMA,ZI(JDES+INO-1)),NOMNOE)
                    INDNOE = INDNOE + 1
                    ZK8(JLIST+INDNOE-1) = NOMNOE
 80            CONTINUE
 70       CONTINUE
      ENDIF
C
C     -- ELIMINATION DES REDONDANCES EVENTUELLES DES NOEUDS
C        DE LA LISTE
C    -------------------------------------------------------------
      CALL WKVECT ('&&MALIN1.TRAV3','V V I',IDIMAX,JIND)
C
      DO 90 INO = 1, IDIMAX
          DO 100 IN1 = INO+1, IDIMAX
                IF (ZK8(JLIST+IN1-1).EQ.ZK8(JLIST+INO-1)) THEN
                      ZI(JIND+IN1-1) = 1
                ENDIF
 100      CONTINUE
  90  CONTINUE
C
      INDLIS = 0
C
      DO 110 INO = 1, IDIMAX
         IF (ZI(JIND+INO-1).EQ.0) THEN
              INDLIS = INDLIS + 1
              ZK8(JLIST+INDLIS-1) = ZK8(JLIST+INO-1)
         ENDIF
 110  CONTINUE
C
      LONLIS = INDLIS
C
      CALL JEDETR ('&&MALIN1.TRAV1')
      CALL JEDETR ('&&MALIN1.TRAV2')
      CALL JEDETR ('&&MALIN1.TRAV3')
C
 9999 CONTINUE
      CALL JEDEMA()
      END
