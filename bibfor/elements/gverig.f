      SUBROUTINE GVERIG ( NOMA, NOCC, CHFOND, LOBJ2, NOMNO, COORN,
     &                    TRAV1, TRAV2, TRAV3, TRAV4 )
      IMPLICIT REAL*8 (A-H,O-Z)
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 05/03/2007   AUTEUR GALENNE E.GALENNE 
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
C FONCTION REALISEE:
C
C     MOTS CLE FACTEUR THETA:
C
C     POUR CHAQUE NOEUD DU FOND DE FISSURE GAMM0 ON RECUPERE
C     LE TRIPLET ( MODULE(THETA), R_INF, R_SUP )
C
C     PUIS ON VERIFIE:
C                     QUE LES NOMS DE GROUPE OU D'ELEMENTS (NOEUD)
C                     APPARTIENNENT BIEN AU MAILLAGE ET A GAMMA0
C
C                     QU'IL N'Y A PAS DUPLICATION DES ENTITES
C
C                     QUE GAMM0 EST COMPLET
C
C                  ---------------------------------
C
C
C     ------------------------------------------------------------------
C ENTREE:
C        NOMA   : NOM DU MAILLAGE
C        NOCC   : NOMBRE D'OCCURENCES
C        NOMNO  : NOM DE L'OBJET CONTENANT LES NOMS DES NOEUDS
C        CHFOND : NOMS DES NOEUDS
C        COORN  : NOM DE L'OBJET CONTENANT LES COORDONNEES DES NOEUDS
C        LOBJ2  : NOMBRE DE NOEUDS DE GAMM0
C
C SORTIE:
C        R_INF         ( OBJET TRAV1 )
C        R_SUP         ( OBJET TRAV2 )
C        MODULE(THETA) ( OBJET TRAV3 )
C        ABSC_CURV     ( OBJET TRAV4 )
C     ------------------------------------------------------------------
C
      CHARACTER*24      OBJ1, OBJ2, CHFOND, GRPNO, NOMNO, COORN
      CHARACTER*24      TRAV, TRAV0, TRAV1, TRAV2, TRAV3, TRAV4
      CHARACTER*8       NOMA,NOEUD,MODELE,NOEUD1,K8B
      CHARACTER*8       NOMPAR(1),RINFF,RSUPF,THETF
      CHARACTER*6       CHAINE
      CHARACTER*16      MOTFAC,NOMCMD,K16B
C
      INTEGER           JJJ,NGRO,NENT,NSOM,IOCC,NOCC,NDIM,LOBJ2,NBMOF
      INTEGER           IGR,NGR,INO,NNO,IRET,COMPT,NBPAR,ITO,NTO,NOUI
      INTEGER           NBM,NBMF,IADRNO,IADRCO,IADRT0,IADRT1,IADRT2
      INTEGER           CANOEU,NBRE,IADRT3,IADABS,I,IADR,IER,J,L,N1,NUM
C
      REAL*8            RINF,RSUP,THET,XL,VALPAR(1),VALRES
C
C---------------- COMMUNS NORMALISES  JEVEUX  --------------------------
      COMMON/IVARJE/ZI(1)
      COMMON/RVARJE/ZR(1)
      COMMON/CVARJE/ZC(1)
      COMMON/LVARJE/ZL(1)
      COMMON/KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      INTEGER ZI
      REAL*8 ZR
      COMPLEX*16 ZC
      LOGICAL ZL
      CHARACTER*8  ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*24 VALK(2)
      CHARACTER*32 ZK32,JEXNOM,JEXNUM
      CHARACTER*80 ZK80
      CHARACTER*8  K8BID
C---------------- FIN COMMUNS NORMALISES  JEVEUX  ----------------------
C
      CALL JEMARQ()
C
      CALL JEVEUO ( CHFOND, 'L', IADRNO )
      CALL JEVEUO ( COORN , 'L', IADRCO )

      CALL GETRES(K8B,K16B,NOMCMD)
      IF(NOMCMD.EQ.'CALC_G')THEN
        MOTFAC = 'THETA'
      ELSE
        MOTFAC = 'THETA_3D'
      ENDIF
      L = LEN(MOTFAC)
      NBRE = 0
C
C OBJET DEFINISSANT LES GROUP_NO DU MAILLAGE
C
      GRPNO = NOMA//'.GROUPENO'
C
C ALLOCATION DE 3 OBJETS DE TRAVAIL
C
      TRAV0 = '&&VERIFG.GAM0'//'           '
      TRAV1 = '&&VERIFG.RINF'//'           '
      TRAV2 = '&&VERIFG.RSUP'//'           '
      TRAV3 = '&&VERIFG.THET'//'           '
      CALL WKVECT(TRAV0,'V V K8',LOBJ2,IADRT0)
      CALL WKVECT(TRAV1,'V V R',LOBJ2,IADRT1)
      CALL WKVECT(TRAV2,'V V R',LOBJ2,IADRT2)
      CALL WKVECT(TRAV3,'V V R',(NBRE+1)*LOBJ2,IADRT3)
C
C
      IF(NOMCMD.NE.'CALC_G')THEN
        DO 1 IOCC=1,NOCC
C
         CALL GETVTX (MOTFAC(1:L),'TOUT',IOCC,1,0,K8BID,NOUI)
         CALL GETVEM(NOMA,'GROUP_NO',MOTFAC(1:L),'GROUP_NO',
     &               IOCC,1,0,K8BID,NGRO)
         CALL GETVEM(NOMA,'NOEUD',MOTFAC(1:L),'NOEUD',
     &            IOCC,1,0,K8BID,NENT)
         NSOM = NGRO + NENT + NOUI
         IF (NSOM.EQ.NGRO) THEN
            NGRO = -NGRO
         ELSE IF (NSOM.EQ.NENT) THEN
            NENT = -NENT
         ELSE IF (NSOM.EQ.NOUI) THEN
            NOUI = -NOUI
         ENDIF
C
1       CONTINUE
C
        NDIM = MAX(NGRO,NENT)
        NDIM = MAX(NDIM,NOUI)
      ELSE
        NDIM=1
      ENDIF
C
C ALLOCATION D'UN AUTRE OBJET DE TRAVAIL
C
      TRAV = '&&VERIFG.'//MOTFAC(1:L)
      CALL WKVECT(TRAV,'V V K8',NDIM,JJJ)
C
      NBPAR = 1
      NOMPAR(1) = 'ABSC'
C
C     CALCUL DES ABSCISSES CURVILIGNES LE LONG DU FOND DE FISSURE
C
      CALL GABSCU(LOBJ2,COORN,NOMNO,CHFOND,XL,TRAV4)
      CALL JEVEUO(TRAV4,'L',IADABS)
      DO 2 IOCC=1,NOCC
C
         CALL GETVR8(MOTFAC(1:L),'MODULE',IOCC,1,NDIM,THET,NBM)
         IF(NOMCMD.EQ.'CALC_G' .AND. NBM.NE.1)THEN
           THET = 1.D0
         ENDIF
         CALL GETVR8(MOTFAC(1:L),'R_INF',IOCC,1,NDIM,RINF,NBM)
         CALL GETVR8(MOTFAC(1:L),'R_SUP',IOCC,1,NDIM,RSUP,NBM)
         IF (NBM.NE.0 .AND. RSUP .LE. RINF) THEN
           CALL U2MESS('F','ELEMENTS5_11')
         ENDIF
         CALL GETVID(MOTFAC(1:L),'MODULE_FO',IOCC,1,NDIM,THETF,NBMOF)
         CALL GETVID(MOTFAC(1:L),'R_INF_FO',IOCC,1,NDIM,RINFF,NBMF)
         CALL GETVID(MOTFAC(1:L),'R_SUP_FO',IOCC,1,NDIM,RSUPF,NBMF)

C
C
C MOT CLE TOUT OU COMMANDE 'CALC_G'
C
         IF(NOMCMD.EQ.'CALC_G')THEN
           NTO=1
         ELSE
           CALL GETVTX (MOTFAC(1:L),'TOUT',IOCC,1,NDIM,ZK8(JJJ),NTO)
         ENDIF
C
         DO 100 ITO=1,NTO
            DO 150 J=1,LOBJ2
                   ZK8(IADRT0 + J - 1) = ZK8(IADRNO + J - 1)
                   NOEUD = ZK8(IADRNO + J - 1)
                   IF(NBM.NE.0) THEN
                      ZR(IADRT1 + J - 1) = RINF
                      ZR(IADRT2 + J - 1) = RSUP
                      ZR(IADRT3 + J - 1) = THET
                   ELSE
                      CALL JENONU(JEXNOM(NOMNO,ZK8(IADRNO+J-1)),NUM)
                      VALPAR(1) = ZR(IADABS + J - 1)
                  CALL FOINTE('FM',RINFF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT1 + J - 1) = VALRES
                  CALL FOINTE('FM',RSUPF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT2 + J - 1) = VALRES
                  IF (ZR(IADRT2 + J - 1) .LE. ZR(IADRT1 + J - 1)) THEN
                    CALL U2MESS('F','ELEMENTS5_11')
                  ENDIF
                  IF (NBMOF .NE. 0 ) THEN
                    CALL FOINTE('FM',THETF,NBPAR,NOMPAR,VALPAR,
     &                    VALRES,IER)
                  ELSE
                    VALRES = 1.D0
                  ENDIF
                  ZR(IADRT3 + J - 1) = VALRES
                   ENDIF
150         CONTINUE
100      CONTINUE
         IF(NOMCMD.EQ.'CALC_G')GOTO 2
C
C
C
C MOT CLE GROUP_NO
C
C LE GROUP_NO DOIT APPARTENIR AU MAILLAGE
C
         CALL GETVEM(NOMA,'GROUP_NO',MOTFAC(1:L),'GROUP_NO',
     &               IOCC,1,NDIM,ZK8(JJJ),NGR)
C
         DO 3 IGR=1,NGR
C
            CALL JEEXIN(JEXNOM(GRPNO,ZK8(JJJ+IGR-1)),IRET)
            IF(IRET.EQ.0) THEN
                VALK(1) = ZK8(JJJ+IGR-1)
                VALK(2) = NOMA
                CALL U2MESK('F','ELEMENTS2_23', 2 ,VALK)
            ELSE
C LES NOEUDS DE CE GROUP_NO DOIVENT APPARTENIR A GAMMO
C
              CALL JELIRA (JEXNOM(GRPNO,ZK8(JJJ+IGR-1)),'LONMAX',
     &                     N1,K8BID)
              CALL JEVEUO (JEXNOM(GRPNO,ZK8(JJJ+IGR-1)),'L',IADR)
              DO 4 J=1,N1
                CALL JENUNO(JEXNUM(NOMNO,ZI(IADR+J-1)),NOEUD1)
                CANOEU = 0
                DO 5 I=1,LOBJ2
                 NOEUD = ZK8(IADRNO + I - 1)
                 IF(NOEUD.EQ.NOEUD1) THEN
                   CANOEU = CANOEU + 1
                   ZK8(IADRT0 + I - 1) = NOEUD1
                   IF(NBM.NE.0) THEN
                      ZR(IADRT1 + I - 1) = RINF
                      ZR(IADRT2 + I- 1) = RSUP
                      ZR(IADRT3 + I - 1) = THET
                   ELSE
                      CALL JENONU(JEXNOM(NOMNO,ZK8(IADRNO+I-1)),NUM)
                      VALPAR(1) = ZR(IADABS + J - 1)
                  CALL FOINTE('FM',RINFF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT1 + I - 1) = VALRES
                  CALL FOINTE('FM',RSUPF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT2 + I - 1) = VALRES
                  IF (ZR(IADRT2 + J - 1) .LE. ZR(IADRT1 + J - 1)) THEN
                    CALL U2MESS('F','ELEMENTS5_11')
                  ENDIF
                  CALL FOINTE('FM',THETF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT3 + I - 1) = VALRES
                   ENDIF
                 ENDIF
5               CONTINUE
                IF(CANOEU.EQ.0) THEN
                  CALL U2MESK('F','ELEMENTS2_24',1,NOEUD1)
                ENDIF
4             CONTINUE
            ENDIF
3      CONTINUE
C
C MOT CLE NOEUD
         CALL GETVEM(NOMA,'NOEUD',MOTFAC(1:L),'NOEUD',
     &            IOCC,1,NDIM,ZK8(JJJ),NNO)
C
         DO 6 I=1,NNO
C
            CALL JENONU(JEXNOM(NOMNO,ZK8(JJJ+I-1)),IRET)
            IF(IRET.EQ.0) THEN
                 VALK(1) = ZK8(JJJ+I-1)
                 VALK(2) = NOMA
                 CALL U2MESK('F','ELEMENTS_90', 2 ,VALK)
            ELSE
C LES NOEUDS DOIVENT APPARTENIR A GAMMO
              CALL JENUNO(JEXNUM(NOMNO,IRET),NOEUD1)
              CANOEU = 0
              DO 7 J=1,LOBJ2
                   NOEUD = ZK8(IADRNO+J-1)
                   IF(NOEUD1.EQ.NOEUD) THEN
                     CANOEU = CANOEU + 1
                     ZK8(IADRT0 + J - 1) = NOEUD1
                     IF(NBM.NE.0) THEN
                        ZR(IADRT1 + J - 1) = RINF
                        ZR(IADRT2 + J - 1) = RSUP
                        ZR(IADRT3 + J - 1) = THET
                     ELSE
                      CALL JENONU(JEXNOM(NOMNO,ZK8(IADRNO+J-1)),NUM)
                      VALPAR(1) = ZR(IADABS + J - 1)
                  CALL FOINTE('FM',RINFF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT1 + J - 1) = VALRES
                  CALL FOINTE('FM',RSUPF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT2 + J - 1) = VALRES
                  IF (ZR(IADRT2 + J - 1) .LE. ZR(IADRT1 + J - 1)) THEN
                    CALL U2MESS('F','ELEMENTS5_11')
                  ENDIF
                  CALL FOINTE('FM',THETF,NBPAR,NOMPAR,VALPAR,VALRES,IER)
                      ZR(IADRT3 + J - 1) = VALRES
                     ENDIF
                   ENDIF
7             CONTINUE
              IF(CANOEU.EQ.0) THEN
                  CALL U2MESK('F','ELEMENTS2_24',1,ZK8(IADRNO+J-1))
              ENDIF
            ENDIF
6     CONTINUE
2     CONTINUE
C
C
C VERIFICATION QUE GAMM0 EST COMPLET
C
      DO 8 I=1,LOBJ2
         IF(ZK8(IADRNO+ I -1).NE.ZK8(IADRT0+I-1)) THEN
            CALL U2MESS('F','ELEMENTS2_25')
         ENDIF
8     CONTINUE
C
C DESTRUCTION D'OBJETS DE TRAVAIL
C
      CALL JEDETR (TRAV)
      CALL JEDETR (TRAV0)
C
      CALL JEDEMA()
      END
