      SUBROUTINE RECI2D(LIRELA,MAILLA,NNOECA,NOEBE,NBCNX,CXMA,NORMAL,
     &                  ITRIA,XBAR,IPROJ,EXCENT)
      IMPLICIT NONE
C-----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 14/03/2005   AUTEUR VABHHTS J.PELLET 
C TOLE CRP_20
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C-----------------------------------------------------------------------
C  DESCRIPTION : DETERMINATION DES RELATIONS CINEMATIQUES ENTRE LES DDLS
C  -----------   D'UN NOEUD DU CABLE ET LES DDLS DES NOEUDS VOISINS DE
C                LA STRUCTURE BETON
C                CAS OU LA STRUCTURE BETON EST MODELISEE PAR DES
C                ELEMENTS 2D
C                APPELANT : PROJCA

C  IN     : LIRELA : CHARACTER*19 , SCALAIRE
C                    NOM DE LA SD DE TYPE LISTE_DE_RELATIONS
C  IN     : MAILLA : CHARACTER*8 , SCALAIRE
C                    NOM DU CONCEPT MAILLAGE ASSOCIE A L'ETUDE
C  IN     : NNOECA : CHARACTER*8 , SCALAIRE
C                    NOM DU NOEUD DU CABLE
C  IN     : NOEBE  : INTEGER , SCALAIRE
C                    NUMERO DU NOEUD VOISIN DE LA STRUCTURE BETON LE
C                    PLUS PROCHE DU NOEUD DU CABLE
C  IN     : NBCNX  : INTEGER , SCALAIRE
C                    NOMBRE DE NOEUDS DE LA MAILLE VOISINE DE LA
C                    STRUCTURE BETON
C  IN     : CXMA   : INTEGER , VECTEUR DE DIMENSION AU PLUS NNOMAX
C                    CONTIENT LES NUMEROS DES NOEUDS DE LA MAILLE
C                    VOISINE DE LA STRUCTURE BETON
C                    (TABLE DE CONNECTIVITE)
C  IN     : NORMAL : REAL*8 , VECTEUR DE DIMENSION 3
C                    COORDONNEES DANS LE REPERE GLOBAL DU VECTEUR NORMAL
C                    AU PLAN MOYEN DE LA MAILLE VOISINE DE LA STRUCTURE
C                    BETON
C  IN     : ITRIA  : INTEGER , SCALAIRE
C                    INDICATEUR DU SOUS-DOMAINE AUQUEL APPARTIENT LE
C                    POINT PROJETE :
C                    ITRIA = 1 : TRIANGLE 1-2-3
C                    ITRIA = 2 : TRIANGLE 3-4-1
C  IN     : XBAR   : REAL*8 , VECTEUR DE DIMENSION 3
C                    SI IPROJ.NE.2 : COORDONNEES BARYCENTRIQUES DU POINT
C                    PROJETE (BARYCENTRE DES SOMMETS DU TRIANGLE 1-2-3
C                    OU 3-4-1)
C  IN     : IPROJ  : INTEGER , SCALAIRE
C                    INDICE DE PROJECTION
C                    IPROJ =  0  LE POINT PROJETE EST A L'INTERIEUR
C                                DE LA MAILLE VOISINE
C                    IPROJ =  1X LE POINT PROJETE EST SUR UNE FRONTIERE
C                                DE LA MAILLE VOISINE
C                    IPROJ =  2  LE POINT PROJETE COINCIDE AVEC UN DES
C                                NOEUDS DE LA MAILLE VOISINE
C  IN     : EXCENT : REAL*8 , SCALAIRE
C                    EXCENTRICITE DU NOEUD DU CABLE PAR RAPPORT A LA
C                    MAILLE VOISINE DE LA STRUCTURE BETON

C-------------------   DECLARATION DES VARIABLES   ---------------------

C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER ZI
      COMMON /IVARJE/ZI(1)
      REAL*8 ZR
      COMMON /RVARJE/ZR(1)
      COMPLEX*16 ZC
      COMMON /CVARJE/ZC(1)
      LOGICAL ZL
      COMMON /LVARJE/ZL(1)
      CHARACTER*8 ZK8
      CHARACTER*16 ZK16
      CHARACTER*24 ZK24
      CHARACTER*32 ZK32
      CHARACTER*80 ZK80
      COMMON /KVARJE/ZK8(1),ZK16(1),ZK24(1),ZK32(1),ZK80(1)
      CHARACTER*32 JEXNOM,JEXNUM,JEXATR
C     ----- FIN   COMMUNS NORMALISES  JEVEUX  --------------------------

C ARGUMENTS
C ---------
      CHARACTER*19 LIRELA
      CHARACTER*8 MAILLA,NNOECA
      INTEGER NOEBE,NBCNX,CXMA(*),ITRIA,IPROJ,NNO
      REAL*8 NORMAL(*),XBAR(*),EXCENT

C VARIABLES LOCALES
C -----------------
      INTEGER I1,I2,I3,IBLOC,ICNX,ITERM,JCMUR,JDDL,JDIME,JDIREC,JNOMNO,
     &        NBBLOC,NBSOM,NBTERM,NBTMAX,NNOMAX,NOECA
      REAL*8 KSI1,KSI2,ZERO
      COMPLEX*16 CBID
      CHARACTER*8 K8B
      CHARACTER*24 NONOMA
      LOGICAL NOTLIN

      REAL*8 FFEL2D,X(2),FF(9)

C-------------------   DEBUT DU CODE EXECUTABLE    ---------------------

      CALL JEMARQ()

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 1   CREATION DES OBJETS DE TRAVAIL - INITIALISATIONS
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      NNOMAX = 9
      NBTMAX = 1 + 2*NNOMAX
      CALL WKVECT('&&RECI2D.COEMUR','V V R',NBTMAX,JCMUR)
      CALL WKVECT('&&RECI2D.NOMDDL','V V K8',NBTMAX,JDDL)
      CALL WKVECT('&&RECI2D.NOMNOE','V V K8',NBTMAX,JNOMNO)
      CALL WKVECT('&&RECI2D.DIMENS','V V I',NBTMAX,JDIME)
      CALL WKVECT('&&RECI2D.DIRECT','V V R',3*NBTMAX,JDIREC)

      NOTLIN = (NBCNX.GT.4)
      IF ((NBCNX.EQ.3) .OR. (NBCNX.EQ.6)) THEN
        NBSOM = 3
      ELSE
        NBSOM = 4
      END IF

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 2   DETERMINATION DE L'ANTECEDENT DU POINT PROJETE DANS L'ELEMENT
C     DE REFERENCE ASSOCIE A L'ELEMENT REEL
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      IF (IPROJ.NE.2) CALL ANTE2D(ITRIA,XBAR(1),KSI1,KSI2)

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
C 3   DETERMINATION DES RELATIONS CINEMATIQUES
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

      ZERO = 0.0D0

      NONOMA = MAILLA//'.NOMNOE'

      ZK8(JNOMNO) = NNOECA
      ZK8(JDDL) = 'DEPL'
      ZR(JCMUR) = 1.0D0

C 3.1 DETERMINATION DES RELATIONS CINEMATIQUES DANS LE CAS OU
C --- L'EXCENTRICITE EST NULLE

      IF (EXCENT.EQ.0.0D0) THEN

        IF (IPROJ.EQ.2) THEN

C.......... PAS DE RELATIONS CINEMATIQUES SI LES NOEUDS SONT
C.......... IDENTIQUES TOPOLOGIQUEMENT

          CALL JENONU(JEXNOM(NONOMA,NNOECA),NOECA)
          IF (NOECA.EQ.NOEBE) GO TO 110

          NBTERM = 2
          CALL JENUNO(JEXNUM(NONOMA,NOEBE),ZK8(JNOMNO+1))
          ZK8(JDDL+1) = 'DEPL'
          ZR(JCMUR+1) = -1.0D0

        ELSE

          IF (IPROJ.GT.10) THEN

            NBTERM = 3
            I1 = IPROJ - 10
            I2 = I1 + 1
            IF (I2.GT.NBSOM) I2 = 1
            CALL JENUNO(JEXNUM(NONOMA,CXMA(I1)),ZK8(JNOMNO+1))
            CALL JENUNO(JEXNUM(NONOMA,CXMA(I2)),ZK8(JNOMNO+2))
            ZK8(JDDL+1) = 'DEPL'
            ZK8(JDDL+2) = 'DEPL'
            IF (NBCNX.EQ.3) THEN
              IF (I1.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)
              ELSE IF (I1.EQ.2) THEN
                FFEL2D = -0.5D0* (KSI1+KSI2)
              ELSE
                FFEL2D = 0.5D0* (1.0D0+KSI1)
              END IF
            ELSE IF (NBCNX.EQ.6) THEN
              IF (I1.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
              ELSE IF (I1.EQ.2) THEN
                FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
              ELSE IF (I1.EQ.3) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
              ELSE IF (I1.EQ.4) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
              ELSE IF (I1.EQ.5) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
              ELSE
                FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
              END IF
            ELSE IF (NBCNX.EQ.4) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU4',X,4,FF,NNO)
              IF (I1.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE
                FFEL2D = FF(I1-1)
              END IF
            ELSE IF (NBCNX.EQ.8) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU8',X,8,FF,NNO)
              IF (I1.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I1.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE
                FFEL2D = FF(I1-1)
              END IF
            ELSE IF (NBCNX.EQ.9) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU9',X,9,FF,NNO)
              IF (I1.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I1.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE IF (I1.EQ.9) THEN
                FFEL2D = FF(9)
              ELSE
                FFEL2D = FF(I1-1)
              END IF
            END IF

            ZR(JCMUR+1) = -FFEL2D

            IF (NBCNX.EQ.3) THEN
              IF (I2.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)
              ELSE IF (I2.EQ.2) THEN
                FFEL2D = -0.5D0* (KSI1+KSI2)
              ELSE
                FFEL2D = 0.5D0* (1.0D0+KSI1)
              END IF
            ELSE IF (NBCNX.EQ.6) THEN
              IF (I2.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
              ELSE IF (I2.EQ.2) THEN
                FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
              ELSE IF (I2.EQ.3) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
              ELSE IF (I2.EQ.4) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
              ELSE IF (I2.EQ.5) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
              ELSE
                FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
              END IF
            ELSE IF (NBCNX.EQ.4) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU4',X,4,FF,NNO)
              IF (I2.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE
                FFEL2D = FF(I2-1)
              END IF
            ELSE IF (NBCNX.EQ.8) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU8',X,8,FF,NNO)
              IF (I2.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I2.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE
                FFEL2D = FF(I2-1)
              END IF
            ELSE IF (NBCNX.EQ.9) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU9',X,9,FF,NNO)
              IF (I2.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I2.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE IF (I2.EQ.9) THEN
                FFEL2D = FF(9)
              ELSE
                FFEL2D = FF(I2-1)
              END IF
            END IF
            ZR(JCMUR+2) = -FFEL2D
C               ZR(JCMUR+1) = -FFEL2D(NBCNX,I1,KSI1,KSI2)
C               ZR(JCMUR+2) = -FFEL2D(NBCNX,I2,KSI1,KSI2)
            IF (NOTLIN) THEN
              NBTERM = 4
              I3 = I1 + NBSOM
              CALL JENUNO(JEXNUM(NONOMA,CXMA(I3)),ZK8(JNOMNO+3))
              ZK8(JDDL+3) = 'DEPL'
              IF (NBCNX.EQ.3) THEN
                IF (I3.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)
                ELSE IF (I3.EQ.2) THEN
                  FFEL2D = -0.5D0* (KSI1+KSI2)
                ELSE
                  FFEL2D = 0.5D0* (1.0D0+KSI1)
                END IF
              ELSE IF (NBCNX.EQ.6) THEN
                IF (I3.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
                ELSE IF (I3.EQ.2) THEN
                  FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
                ELSE IF (I3.EQ.3) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
                ELSE IF (I3.EQ.4) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
                ELSE IF (I3.EQ.5) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
                ELSE
                  FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
                END IF
              ELSE IF (NBCNX.EQ.4) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU4',X,4,FF,NNO)
                IF (I3.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE
                  FFEL2D = FF(I3-1)
                END IF
              ELSE IF (NBCNX.EQ.8) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU8',X,8,FF,NNO)
                IF (I3.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (I3.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE
                  FFEL2D = FF(I3-1)
                END IF
              ELSE IF (NBCNX.EQ.9) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU9',X,9,FF,NNO)
                IF (I3.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (I3.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE IF (I3.EQ.9) THEN
                  FFEL2D = FF(9)
                ELSE
                  FFEL2D = FF(I3-1)
                END IF
              END IF
              ZR(JCMUR+3) = -FFEL2D
C                  ZR(JCMUR+3) = -FFEL2D(NBCNX,I3,KSI1,KSI2)
            END IF

          ELSE

            NBTERM = 1 + NBCNX
            DO 10 ICNX = 1,NBCNX
              CALL JENUNO(JEXNUM(NONOMA,CXMA(ICNX)),ZK8(JNOMNO+ICNX))
              ZK8(JDDL+ICNX) = 'DEPL'

              IF (NBCNX.EQ.3) THEN
                IF (ICNX.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)
                ELSE IF (ICNX.EQ.2) THEN
                  FFEL2D = -0.5D0* (KSI1+KSI2)
                ELSE
                  FFEL2D = 0.5D0* (1.0D0+KSI1)
                END IF
              ELSE IF (NBCNX.EQ.6) THEN
                IF (ICNX.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
                ELSE IF (ICNX.EQ.2) THEN
                  FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
                ELSE IF (ICNX.EQ.3) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
                ELSE IF (ICNX.EQ.4) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
                ELSE IF (ICNX.EQ.5) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
                ELSE
                  FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
                END IF
              ELSE IF (NBCNX.EQ.4) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU4',X,4,FF,NNO)
                IF (ICNX.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE
                  FFEL2D = FF(ICNX-1)
                END IF
              ELSE IF (NBCNX.EQ.8) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU8',X,8,FF,NNO)
                IF (ICNX.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (ICNX.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE
                  FFEL2D = FF(ICNX-1)
                END IF
              ELSE IF (NBCNX.EQ.9) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU9',X,9,FF,NNO)
                IF (ICNX.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (ICNX.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE IF (ICNX.EQ.9) THEN
                  FFEL2D = FF(9)
                ELSE
                  FFEL2D = FF(ICNX-1)
                END IF
              END IF
              ZR(JCMUR+ICNX) = -FFEL2D
C                  ZR(JCMUR+ICNX) = -FFEL2D(NBCNX,ICNX,KSI1,KSI2)
   10       CONTINUE

          END IF

        END IF

C....... UNE RELATION PAR DDL DE TRANSLATION DU NOEUD DU CABLE
C        .....................................................

C....... LE VECTEUR ZI(JDIME) DOIT ETRE REINITIALISE AFIN DE PRENDRE
C....... EN COMPTE LES DIFFERENTS COEFFICIENTS PAR DIRECTION DEFINIS
C....... DANS LE VECTEUR ZR(JDIREC)

        DO 20 ITERM = 1,NBTERM
          ZI(JDIME+ITERM-1) = 3
   20   CONTINUE

C....... COEFFICIENTS PAR DIRECTIONS POUR LA PREMIERE RELATION (DDL DX)
C....... PUIS AFFECTATION

        DO 30 ITERM = 1,NBTERM
          ZR(JDIREC+3* (ITERM-1)) = 1.0D0
          ZR(JDIREC+3* (ITERM-1)+1) = 0.0D0
          ZR(JDIREC+3* (ITERM-1)+2) = 0.0D0
   30   CONTINUE

        CALL AFRELA(ZR(JCMUR),CBID,ZK8(JDDL),ZK8(JNOMNO),ZI(JDIME),
     &              ZR(JDIREC),NBTERM,ZERO,CBID,K8B,'REEL','REEL','12',
     &              0.D0,LIRELA)

C....... COEFFICIENTS PAR DIRECTIONS POUR LA DEUXIEME RELATION (DDL DY)
C....... PUIS AFFECTATION

        DO 40 ITERM = 1,NBTERM
          ZR(JDIREC+3* (ITERM-1)) = 0.0D0
          ZR(JDIREC+3* (ITERM-1)+1) = 1.0D0
          ZR(JDIREC+3* (ITERM-1)+2) = 0.0D0
   40   CONTINUE

        CALL AFRELA(ZR(JCMUR),CBID,ZK8(JDDL),ZK8(JNOMNO),ZI(JDIME),
     &              ZR(JDIREC),NBTERM,ZERO,CBID,K8B,'REEL','REEL','12',
     &              0.D0,LIRELA)

C....... COEFFICIENTS PAR DIRECTIONS POUR LA TROISIEME RELATION (DDL DZ)
C....... PUIS AFFECTATION

        DO 50 ITERM = 1,NBTERM
          ZR(JDIREC+3* (ITERM-1)) = 0.0D0
          ZR(JDIREC+3* (ITERM-1)+1) = 0.0D0
          ZR(JDIREC+3* (ITERM-1)+2) = 1.0D0
   50   CONTINUE

        CALL AFRELA(ZR(JCMUR),CBID,ZK8(JDDL),ZK8(JNOMNO),ZI(JDIME),
     &              ZR(JDIREC),NBTERM,ZERO,CBID,K8B,'REEL','REEL','12',
     &              0.D0,LIRELA)

C 3.2 DETERMINATION DES RELATIONS CINEMATIQUES DANS LE CAS GENERAL
C ---
      ELSE

        IF (IPROJ.EQ.2) THEN

          NBTERM = 3
          CALL JENUNO(JEXNUM(NONOMA,NOEBE),ZK8(JNOMNO+1))
          ZK8(JNOMNO+2) = ZK8(JNOMNO+1)
          ZK8(JDDL+1) = 'DEPL'
          ZK8(JDDL+2) = 'ROTA'
          ZR(JCMUR+1) = -1.0D0
          ZR(JCMUR+2) = -EXCENT

        ELSE

          IF (IPROJ.GT.10) THEN

            NBTERM = 5
            I1 = IPROJ - 10
            I2 = I1 + 1
            IF (I2.GT.NBSOM) I2 = 1
            CALL JENUNO(JEXNUM(NONOMA,CXMA(I1)),ZK8(JNOMNO+1))
            ZK8(JNOMNO+2) = ZK8(JNOMNO+1)
            CALL JENUNO(JEXNUM(NONOMA,CXMA(I2)),ZK8(JNOMNO+3))
            ZK8(JNOMNO+4) = ZK8(JNOMNO+3)
            ZK8(JDDL+1) = 'DEPL'
            ZK8(JDDL+2) = 'ROTA'
            ZK8(JDDL+3) = 'DEPL'
            ZK8(JDDL+4) = 'ROTA'

            IF (NBCNX.EQ.3) THEN
              IF (I1.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)
              ELSE IF (I1.EQ.2) THEN
                FFEL2D = -0.5D0* (KSI1+KSI2)
              ELSE
                FFEL2D = 0.5D0* (1.0D0+KSI1)
              END IF
            ELSE IF (NBCNX.EQ.6) THEN
              IF (I1.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
              ELSE IF (I1.EQ.2) THEN
                FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
              ELSE IF (I1.EQ.3) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
              ELSE IF (I1.EQ.4) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
              ELSE IF (I1.EQ.5) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
              ELSE
                FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
              END IF
            ELSE IF (NBCNX.EQ.4) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU4',X,4,FF,NNO)
              IF (I1.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE
                FFEL2D = FF(I1-1)
              END IF
            ELSE IF (NBCNX.EQ.8) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU8',X,8,FF,NNO)
              IF (I1.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I1.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE
                FFEL2D = FF(I1-1)
              END IF
            ELSE IF (NBCNX.EQ.9) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU9',X,9,FF,NNO)
              IF (I1.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I1.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE IF (I1.EQ.9) THEN
                FFEL2D = FF(9)
              ELSE
                FFEL2D = FF(I1-1)
              END IF
            END IF
            ZR(JCMUR+1) = -FFEL2D
C               ZR(JCMUR+1) = -FFEL2D(NBCNX,I1,KSI1,KSI2)
            ZR(JCMUR+2) = EXCENT*ZR(JCMUR+1)

            IF (NBCNX.EQ.3) THEN
              IF (I2.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)
              ELSE IF (I2.EQ.2) THEN
                FFEL2D = -0.5D0* (KSI1+KSI2)
              ELSE
                FFEL2D = 0.5D0* (1.0D0+KSI1)
              END IF
            ELSE IF (NBCNX.EQ.6) THEN
              IF (I2.EQ.1) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
              ELSE IF (I2.EQ.2) THEN
                FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
              ELSE IF (I2.EQ.3) THEN
                FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
              ELSE IF (I2.EQ.4) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
              ELSE IF (I2.EQ.5) THEN
                FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
              ELSE
                FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
              END IF
            ELSE IF (NBCNX.EQ.4) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU4',X,4,FF,NNO)
              IF (I2.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE
                FFEL2D = FF(I2-1)
              END IF
            ELSE IF (NBCNX.EQ.8) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU8',X,8,FF,NNO)
              IF (I2.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I2.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE
                FFEL2D = FF(I2-1)
              END IF
            ELSE IF (NBCNX.EQ.9) THEN
              X(1) = KSI1
              X(2) = KSI2
              CALL ELRFVF('QU9',X,9,FF,NNO)
              IF (I2.EQ.1) THEN
                FFEL2D = FF(4)
              ELSE IF (I2.EQ.5) THEN
                FFEL2D = FF(8)
              ELSE IF (I2.EQ.9) THEN
                FFEL2D = FF(9)
              ELSE
                FFEL2D = FF(I2-1)
              END IF
            END IF
            ZR(JCMUR+3) = -FFEL2D
C               ZR(JCMUR+3) = -FFEL2D(NBCNX,I2,KSI1,KSI2)
            ZR(JCMUR+4) = EXCENT*ZR(JCMUR+3)
            IF (NOTLIN) THEN
              NBTERM = 7
              I3 = I1 + NBSOM
              CALL JENUNO(JEXNUM(NONOMA,CXMA(I3)),ZK8(JNOMNO+5))
              ZK8(JNOMNO+6) = ZK8(JNOMNO+5)
              ZK8(JDDL+5) = 'DEPL'
              ZK8(JDDL+6) = 'ROTA'

              IF (NBCNX.EQ.3) THEN
                IF (I3.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)
                ELSE IF (I3.EQ.2) THEN
                  FFEL2D = -0.5D0* (KSI1+KSI2)
                ELSE
                  FFEL2D = 0.5D0* (1.0D0+KSI1)
                END IF
              ELSE IF (NBCNX.EQ.6) THEN
                IF (I3.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
                ELSE IF (I3.EQ.2) THEN
                  FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
                ELSE IF (I3.EQ.3) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
                ELSE IF (I3.EQ.4) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
                ELSE IF (I3.EQ.5) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
                ELSE
                  FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
                END IF
              ELSE IF (NBCNX.EQ.4) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU4',X,4,FF,NNO)
                IF (I3.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE
                  FFEL2D = FF(I3-1)
                END IF
              ELSE IF (NBCNX.EQ.8) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU8',X,8,FF,NNO)
                IF (I3.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (I3.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE
                  FFEL2D = FF(I3-1)
                END IF
              ELSE IF (NBCNX.EQ.9) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU9',X,9,FF,NNO)
                IF (I3.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (I3.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE IF (I3.EQ.9) THEN
                  FFEL2D = FF(9)
                ELSE
                  FFEL2D = FF(I3-1)
                END IF
              END IF
              ZR(JCMUR+5) = -FFEL2D
C                  ZR(JCMUR+5) = -FFEL2D(NBCNX,I3,KSI1,KSI2)
              ZR(JCMUR+6) = EXCENT*ZR(JCMUR+5)
            END IF

          ELSE

            NBTERM = 1 + 2*NBCNX
            DO 60 ICNX = 1,NBCNX
              CALL JENUNO(JEXNUM(NONOMA,CXMA(ICNX)),
     &                    ZK8(JNOMNO+2* (ICNX-1)+1))
              ZK8(JNOMNO+2*ICNX) = ZK8(JNOMNO+2* (ICNX-1)+1)
              ZK8(JDDL+2* (ICNX-1)+1) = 'DEPL'
              ZK8(JDDL+2*ICNX) = 'ROTA'

              IF (NBCNX.EQ.3) THEN
                IF (ICNX.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)
                ELSE IF (ICNX.EQ.2) THEN
                  FFEL2D = -0.5D0* (KSI1+KSI2)
                ELSE
                  FFEL2D = 0.5D0* (1.0D0+KSI1)
                END IF
              ELSE IF (NBCNX.EQ.6) THEN
                IF (ICNX.EQ.1) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI2)*KSI2
                ELSE IF (ICNX.EQ.2) THEN
                  FFEL2D = 0.5D0* (KSI1+KSI2)* (KSI1+KSI2+1.0D0)
                ELSE IF (ICNX.EQ.3) THEN
                  FFEL2D = 0.5D0* (1.0D0+KSI1)*KSI1
                ELSE IF (ICNX.EQ.4) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI2)* (KSI1+KSI2)
                ELSE IF (ICNX.EQ.5) THEN
                  FFEL2D = -1.0D0* (1.0D0+KSI1)* (KSI1+KSI2)
                ELSE
                  FFEL2D = (1.0D0+KSI1)* (1.0D0+KSI2)
                END IF
              ELSE IF (NBCNX.EQ.4) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU4',X,4,FF,NNO)
                IF (ICNX.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE
                  FFEL2D = FF(ICNX-1)
                END IF
              ELSE IF (NBCNX.EQ.8) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU8',X,8,FF,NNO)
                IF (ICNX.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (ICNX.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE
                  FFEL2D = FF(ICNX-1)
                END IF
              ELSE IF (NBCNX.EQ.9) THEN
                X(1) = KSI1
                X(2) = KSI2
                CALL ELRFVF('QU9',X,9,FF,NNO)
                IF (ICNX.EQ.1) THEN
                  FFEL2D = FF(4)
                ELSE IF (ICNX.EQ.5) THEN
                  FFEL2D = FF(8)
                ELSE IF (ICNX.EQ.9) THEN
                  FFEL2D = FF(9)
                ELSE
                  FFEL2D = FF(ICNX-1)
                END IF
              END IF
              ZR(JCMUR+2* (ICNX-1)+1) = -FFEL2D
C            ZR(JCMUR+2*(ICNX-1)+1) = -FFEL2D(NBCNX,ICNX,KSI1,KSI2)
              ZR(JCMUR+2*ICNX) = EXCENT*ZR(JCMUR+2* (ICNX-1)+1)
   60       CONTINUE

          END IF

        END IF

C....... UNE RELATION PAR DDL DE TRANSLATION DU NOEUD DU CABLE
C        .....................................................

C....... LE VECTEUR ZI(JDIME) DOIT ETRE REINITIALISE AFIN DE PRENDRE
C....... EN COMPTE LES DIFFERENTS COEFFICIENTS PAR DIRECTION DEFINIS
C....... DANS LE VECTEUR ZR(JDIREC)

        DO 70 ITERM = 1,NBTERM
          ZI(JDIME+ITERM-1) = 3
   70   CONTINUE

        NBBLOC = (NBTERM-1)/2

C....... COEFFICIENTS PAR DIRECTIONS POUR LA PREMIERE RELATION (DDL DX)
C....... PUIS AFFECTATION

        ZR(JDIREC) = 1.0D0
        ZR(JDIREC+1) = 0.0D0
        ZR(JDIREC+2) = 0.0D0
        DO 80 IBLOC = 1,NBBLOC
          ZR(JDIREC+3+6* (IBLOC-1)) = 1.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+1) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+2) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+3) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+4) = NORMAL(3)
          ZR(JDIREC+3+6* (IBLOC-1)+5) = -NORMAL(2)
   80   CONTINUE

        CALL AFRELA(ZR(JCMUR),CBID,ZK8(JDDL),ZK8(JNOMNO),ZI(JDIME),
     &              ZR(JDIREC),NBTERM,ZERO,CBID,K8B,'REEL','REEL','12',
     &              0.D0,LIRELA)

C....... COEFFICIENTS PAR DIRECTIONS POUR LA DEUXIEME RELATION (DDL DY)
C....... PUIS AFFECTATION

        ZR(JDIREC) = 0.0D0
        ZR(JDIREC+1) = 1.0D0
        ZR(JDIREC+2) = 0.0D0
        DO 90 IBLOC = 1,NBBLOC
          ZR(JDIREC+3+6* (IBLOC-1)) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+1) = 1.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+2) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+3) = -NORMAL(3)
          ZR(JDIREC+3+6* (IBLOC-1)+4) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+5) = NORMAL(1)
   90   CONTINUE

        CALL AFRELA(ZR(JCMUR),CBID,ZK8(JDDL),ZK8(JNOMNO),ZI(JDIME),
     &              ZR(JDIREC),NBTERM,ZERO,CBID,K8B,'REEL','REEL','12',
     &              0.D0,LIRELA)

C....... COEFFICIENTS PAR DIRECTIONS POUR LA TROISIEME RELATION (DDL DZ)
C....... PUIS AFFECTATION

        ZR(JDIREC) = 0.0D0
        ZR(JDIREC+1) = 0.0D0
        ZR(JDIREC+2) = 1.0D0
        DO 100 IBLOC = 1,NBBLOC
          ZR(JDIREC+3+6* (IBLOC-1)) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+1) = 0.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+2) = 1.0D0
          ZR(JDIREC+3+6* (IBLOC-1)+3) = NORMAL(2)
          ZR(JDIREC+3+6* (IBLOC-1)+4) = -NORMAL(1)
          ZR(JDIREC+3+6* (IBLOC-1)+5) = 0.0D0
  100   CONTINUE

        CALL AFRELA(ZR(JCMUR),CBID,ZK8(JDDL),ZK8(JNOMNO),ZI(JDIME),
     &              ZR(JDIREC),NBTERM,ZERO,CBID,K8B,'REEL','REEL','12',
     &              0.D0,LIRELA)

      END IF

  110 CONTINUE
      CALL JEDETC('V','&&RECI2D',1)
      CALL JEDEMA()

C --- FIN DE RECI2D.
      END
