      SUBROUTINE BOITE(MAIL,NGRMZ,NNORMZ,DIME,NTM,NBOITZ)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C A_UTIL
C ----------------------------------------------------------------------
C     CONSTRUCTION DE BOITES ENGLOBANTES POUR UN ENSEMBLE DE MAILLES
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8       MAIL     : SD MAILLAGE
C CHARACTER*(10)    NGRMZ    : GROUPE DE MAILLES
C CHARACTER*(10)    NNORMZ   : NORMALES LISSEES COQUES (CF LISNOR)
C INTEGER           DIME     : DIMENSION DE L'ESPACE
C CHARACTER*8       NTM(*)   : VECTEUR NOMS TYPES DE MAILLE
C CHARACTER*(16)    NBOITZ   : SD BOITE
C
C SD PRODUITE
C BOITE.DIME   : DIMENSIONS ET INDEX DE LA SD BOITE
C               ( DIME, NMA, PAN1, SOM1, PAN2, ...)
C                 DIME : DIMENSION DE L'ESPACE
C                 NMA  : NOMBRE DE MAILLES
C                 PAN* : INDEX DES PANS DE MA* DANS BOITE.PAN
C                 SOM* : INDEX DES SOMMETS DE MA* DANS BOITE.SOMMET
C BOITE.MINMAX : BOITES ENGLOBANT LES MAILLES SUIVANT X, Y, [Z]
C                (X1MIN,X1MAX,Y1MIN,Y1MAX,[Z1MIN],[Z1MAX],X2MIN,...)
C BOITE.PAN    : PANS (2D = ARETES, 3D = FACES) DES CONVEXES
C                ENGLOBANTS ET INSCRITS DES MAILLES
C                (A1,B1,[C1],D1,E1,A2...)
C                   TELS QUE AX+BY+[CZ]+D<=0 (CONVEXE ENGLOBANT)
C                   ET       AX+BY+[CZ]+E<=0 (CONVEXE INSCRIT)
C BOITE.SOMMET : SOMMETS DES CONVEXES ENGLOBANT LES MAILLES
C                (X1,Y1,[Z1],X2,...)
C BOITE.MMGLOB : BOITE ENGLOBANT TOUTES LES MAILLES DE NGRMA
C                (XMIN,XMAX,YMIN,YMAX,[ZMIN,ZMAX])
C BOITE.H      : TAILLE MOYENNE DES MAILLES
C                (H1,H2,...)
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --------- DEBUT DECLARATIONS NORMALISEES  JEVEUX ---------------------
      CHARACTER*32       JEXNUM , JEXNOM , JEXR8 , JEXATR
      INTEGER            ZI
      COMMON  / IVARJE / ZI(1)
      REAL*8             ZR
      COMMON  / RVARJE / ZR(1)
      COMPLEX*16         ZC
      COMMON  / CVARJE / ZC(1)
      LOGICAL            ZL
      COMMON  / LVARJE / ZL(1)
      CHARACTER*8        ZK8
      CHARACTER*16                ZK16
      CHARACTER*24                          ZK24
      CHARACTER*32                                    ZK32
      CHARACTER*80                                              ZK80
      COMMON  / KVARJE / ZK8(1) , ZK16(1) , ZK24(1) , ZK32(1) , ZK80(1)
C --- FIN DECLARATIONS NORMALISEES JEVEUX ------------------------------

C --- PARAMETRES
      REAL*8        R8MAX, PREC
      PARAMETER     (R8MAX = 1.D92)
      PARAMETER     (PREC = 1.D-8)

C --- VARIABLES
      CHARACTER*(*) NGRMZ,NBOITZ,NNORMZ
      CHARACTER*24  NGRMA,NNORMA
      CHARACTER*16  NBOITE
      CHARACTER*8   MAIL,TYPEMA,NTM(*)
      INTEGER       NMA,DIME,NPAN,NSOM,NNO,NARE,MA,I,J,L,LCPAN,LCSOM
      INTEGER       P0,P1,P2,P3,P4,P5,P6,Q0,Q1,Q2,Q3,Q4,Q5,Q6
      INTEGER       NOEPAN(60),NOAR(36),PANNOE(24),PAAR(24)
      REAL*8        CNO(81),H,R

      NGRMA = NGRMZ
      NBOITE = NBOITZ
      NNORMA = NNORMZ

      CALL JEMARQ()

C --- LECTURE DONNEES

      CALL JEVEUO(MAIL//'.TYPMAIL','L',P1)
      CALL JEVEUO(MAIL//'.CONNEX','L',P2)
      CALL JEVEUO(JEXATR(MAIL//'.CONNEX','LONCUM'),'L',P3)
      CALL JEVEUO(MAIL//'.COORDO    .VALE','L', P4)
      CALL JELIRA(NGRMA,'LONMAX',NMA,ZK8)
      CALL JEVEUO(NGRMA,'L',P5)
      CALL JEEXIN(NNORMA,I)
      IF (I.NE.0) CALL JEVEUO(NNORMA,'L',P6)

C --- COMPTE NOMBRE DE PANS ET DE SOMMETS

      P0 = P5
      NPAN = 0
      NSOM = 0
      IF (DIME.EQ.2) THEN

        DO 10 I = 1, NMA
          TYPEMA = NTM(ZI(P1-1+ZI(P0)))
          IF ((TYPEMA(1:4).EQ.'QUAD').OR.(TYPEMA(1:3).EQ.'SEG')) THEN
            NPAN = NPAN + 4
            NSOM = NSOM + 4
          ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN
            NPAN = NPAN + 3
            NSOM = NSOM + 3
          ELSE
            CALL U2MESK('F','CALCULEL_28',1,TYPEMA)
          ENDIF
          P0 = P0 + 1
 10     CONTINUE

      ELSE

        DO 20 I = 1, NMA
          TYPEMA = NTM(ZI(P1-1+ZI(P0)))
          IF ((TYPEMA(1:5).EQ.'PENTA').OR.(TYPEMA(1:4).EQ.'TRIA')) THEN
            NPAN = NPAN + 5
            NSOM = NSOM + 6
          ELSEIF((TYPEMA(1:4).EQ.'HEXA').OR.(TYPEMA(1:4).EQ.'QUAD'))THEN
            NPAN = NPAN + 6
            NSOM = NSOM + 8
          ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
            NPAN = NPAN + 4
            NSOM = NSOM + 4
          ELSE
            CALL U2MESK('F','CALCULEL_28',1,TYPEMA)
          ENDIF
          P0 = P0 + 1
 20     CONTINUE

      ENDIF

C --- ALLOCATIONS SD BOITE

      CALL WKVECT(NBOITE//'.MMGLOB','V V R',2*DIME,Q0)
      CALL WKVECT(NBOITE//'.MINMAX','V V R',2*DIME*NMA,Q1)
      CALL WKVECT(NBOITE//'.DIME','V V I',4+2*NMA,Q2)
      CALL WKVECT(NBOITE//'.PAN','V V R',(2+DIME)*NPAN,Q3)
      CALL WKVECT(NBOITE//'.SOMMET','V V R',DIME*NSOM,Q4)
      CALL WKVECT(NBOITE//'.H','V V R',NMA,Q5)

      Q6 = Q0
      DO 30 I = 1, DIME
        ZR(Q6) = R8MAX
        ZR(Q6+1) = -R8MAX
        Q6 = Q6 + 2
 30   CONTINUE

      LCPAN = 1
      LCSOM = 1
      ZI(Q2) = DIME
      ZI(Q2+1) = NMA
      ZI(Q2+2) = LCPAN
      ZI(Q2+3) = LCSOM
      Q2 = Q2 + 4

C --- CALCUL DES BOITES ET DES PANS

      P0 = P5

      IF (DIME.EQ.2) THEN

C ----- MAILLES 2D

        DO 40 I = 1, NMA

          MA = ZI(P0)
          TYPEMA = NTM(ZI(P1-1+MA))
          CALL TMACOQ(TYPEMA,2,L)
          CALL NOPAN(TYPEMA,NOEPAN,NPAN)

C ------- TRIANGLE

          IF (TYPEMA(1:4).EQ.'TRIA') THEN

C --------- TRIA3

            IF (TYPEMA(5:5).EQ.'3') THEN

              CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),H,DIME,0,ZR(Q4),NSOM)
              CALL BOITEL(ZR(Q4),TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))

C --------- TRIA6

            ELSE

              CALL PANNO(TYPEMA,PANNOE,NSOM)
              CALL NOAREQ(TYPEMA,NOAR,NARE)
              CALL PANARQ(TYPEMA,PAAR,NARE)
              CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),H,DIME,0,CNO,NNO)
              CALL BOITEL(CNO,TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL BOITEA(CNO,NOAR,PAAR,NARE,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL SOMMET(PANNOE,DIME,ZR(Q3),NSOM,ZR(Q4))

            ENDIF

            Q4 = Q4 + 6

          ELSE

C ------- QUADRANGLE

C --------- QUAD4

            IF (TYPEMA(5:5).EQ.'4') THEN

              CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),H,DIME,0,ZR(Q4),NSOM)
              CALL BOITEL(ZR(Q4),TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))

C --------- QUAD6/QUAD8/QUAD9

            ELSE

              CALL PANNO(TYPEMA,PANNOE,NSOM)
              CALL NOAREQ(TYPEMA,NOAR,NARE)
              CALL PANARQ(TYPEMA,PAAR,NARE)
              CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),ZR(P6),DIME,L,CNO,NNO)
              CALL BOITEL(CNO,TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL BOITEA(CNO,NOAR,PAAR,NARE,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL SOMMET(PANNOE,DIME,ZR(Q3),NSOM,ZR(Q4))

            ENDIF

            Q4 = Q4 + 8

          ENDIF

          Q6 = Q0
          H = 1.D0
          DO 50 J = 1,DIME
            R = ZR(Q1+1)-ZR(Q1)
            H = H*R
            R = PREC*R
            ZR(Q1) = ZR(Q1) - R
            ZR(Q1+1) = ZR(Q1+1) + R
            IF (ZR(Q1).LT.ZR(Q6)) ZR(Q6) = ZR(Q1)
            IF (ZR(Q1+1).GT.ZR(Q6+1)) ZR(Q6+1) = ZR(Q1+1)
            Q1 = Q1 + 2
            Q6 = Q6 + 2
 50       CONTINUE

          DO 60 J = 1, NPAN
            DO 70 L = 1, 4
              ZR(Q3) = ZR(Q3)/H
              Q3 = Q3 + 1
 70         CONTINUE
            ZR(Q3-2) = ZR(Q3-2) - PREC
            ZR(Q3-1) = ZR(Q3-1) + PREC
 60       CONTINUE

          H = H**0.5D0
          LCPAN = LCPAN + NPAN
          LCSOM = LCSOM + NSOM
          ZI(Q2) = LCPAN
          ZI(Q2+1) = LCSOM
          Q2 = Q2 + 2
          P0 = P0 + 1

          ZR(Q5) = H
          Q5 = Q5 + 1

 40     CONTINUE

      ELSE

C ----- MAILLES 3D

        DO 80 I = 1, NMA

          MA = ZI(P0)
          TYPEMA = NTM(ZI(P1-1+MA))
          CALL TMACOQ(TYPEMA,3,L)
          CALL NOPAN(TYPEMA,NOEPAN,NPAN)

C ------- TETRAEDRE

          IF (TYPEMA(1:5).EQ.'TETRA') THEN

C --------- TETRA4

            IF (TYPEMA(6:6).EQ.'4') THEN

              CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),H,DIME,0,ZR(Q4),NSOM)
              CALL BOITEL(ZR(Q4),TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))

C --------- TETRA10

            ELSE

              CALL PANNO(TYPEMA,PANNOE,NSOM)
              CALL NOAREQ(TYPEMA,NOAR,NARE)
              CALL PANARQ(TYPEMA,PAAR,NARE)
              CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),H,DIME,0,CNO,NNO)
              CALL BOITEL(CNO,TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL BOITEA(CNO,NOAR,PAAR,NARE,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL BOITEQ(CNO,NOEPAN,NPAN,ZR(Q1),ZR(Q3))
              CALL SOMMET(PANNOE,DIME,ZR(Q3),NSOM,ZR(Q4))

            ENDIF

            Q4 = Q4 + 12

C ------- PENTAEDRE

          ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN

            CALL PANNO(TYPEMA,PANNOE,NSOM)
            CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),ZR(P6),DIME,L,CNO,NNO)
            CALL BOITEL(CNO,TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))

C --------- PENTA12 / PENTA14 / PENTA15

            IF (TYPEMA(6:6).NE.'6') THEN

              CALL NOAREQ(TYPEMA,NOAR,NARE)
              CALL PANARQ(TYPEMA,PAAR,NARE)
              CALL BOITEA(CNO,NOAR,PAAR,NARE,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL BOITEQ(CNO,NOEPAN,NPAN,ZR(Q1),ZR(Q3))

            ENDIF

            CALL SOMMET(PANNOE,DIME,ZR(Q3),NSOM,ZR(Q4))
            Q4 = Q4 + 18

C ------- HEXAEDRE

          ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN

            CALL PANNO(TYPEMA,PANNOE,NSOM)
            CALL CONOEU(MA,ZI(P2),ZI(P3),ZR(P4),ZR(P6),DIME,L,CNO,NNO)
            CALL BOITEL(CNO,TYPEMA,NOEPAN,NPAN,DIME,ZR(Q1),ZR(Q3))

C --------- HEXA16 / HEXA18 / HEXA20 / HEXA27

            IF (TYPEMA(5:5).NE.'8') THEN

              CALL NOAREQ(TYPEMA,NOAR,NARE)
              CALL PANARQ(TYPEMA,PAAR,NARE)
              CALL BOITEA(CNO,NOAR,PAAR,NARE,NPAN,DIME,ZR(Q1),ZR(Q3))
              CALL BOITEQ(CNO,NOEPAN,NPAN,ZR(Q1),ZR(Q3))

            ENDIF

            CALL SOMMET(PANNOE,DIME,ZR(Q3),NSOM,ZR(Q4))
            Q4 = Q4 + 24

          ENDIF

          Q6 = Q0
          H = 1.D0
          DO 90 J = 1,DIME
            R = ZR(Q1+1)-ZR(Q1)
            H = H*R
            R = R*PREC
            ZR(Q1) = ZR(Q1) - R
            ZR(Q1+1) = ZR(Q1+1) + R
            IF (ZR(Q1).LT.ZR(Q6)) ZR(Q6) = ZR(Q1)
            IF (ZR(Q1+1).GT.ZR(Q6+1)) ZR(Q6+1) = ZR(Q1+1)
            Q1 = Q1 + 2
            Q6 = Q6 + 2
 90       CONTINUE

          DO 100 J = 1, NPAN
            DO 110 L = 1, 5
              ZR(Q3) = ZR(Q3)/H
              Q3 = Q3 + 1
 110        CONTINUE
            ZR(Q3-2) = ZR(Q3-2) - PREC
            ZR(Q3-1) = ZR(Q3-1) + PREC
 100      CONTINUE

          H = H**(1.D0/3.D0)
          LCPAN = LCPAN + NPAN
          LCSOM = LCSOM + NSOM
          ZI(Q2) = LCPAN
          ZI(Q2+1) = LCSOM
          Q2 = Q2 + 2
          P0 = P0 + 1

          ZR(Q5) = H
          Q5 = Q5 + 1

 80     CONTINUE

      ENDIF

      CALL JEDEMA()

      END
