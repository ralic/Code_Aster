      SUBROUTINE NMASF3(NNO,NBPG1,IPOIDS,IVF,IDFDE,IMATE,GEOM,
     &                  DEPLM,SIGM,VECTU,COMPOR)
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C TOLE CRS_1404
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER NNO,NBPG1,IMATE
      INTEGER IPOIDS,IVF,IDFDE
      INTEGER IPOID2,IVF2,IDFDE2
      CHARACTER*16  COMPOR(4)
      REAL*8 GEOM(3,NNO)
      REAL*8 DEPLM(3,NNO),DFDI(NNO,3)
      REAL*8 DEF(6,3,NNO)
      REAL*8 SIGM(78,NBPG1)
      REAL*8 VECTU(3,NNO)
C.......................................................................
C
C     BUT:  CALCUL  DE L' OPTION FORC_NODA
C           EN HYPO-ELASTICITE EN 3D POUR LE HEXA8 SOUS INTEGRE
C           STABILITE PAR ASSUMED STRAIN
C.......................................................................
C IN  NNO     : NOMBRE DE NOEUDS DE L'ELEMENT
C IN  NBPG1   : NOMBRE DE POINTS DE GAUSS
C IN  POIDSG  : POIDS DES POINTS DE GAUSS
C IN  VFF     : VALEUR  DES FONCTIONS DE FORME
C IN  DFDE    : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
C IN  IMATE   : ADRESSE MATERIAU CODE
C IN  GEOM    : COORDONEES DES NOEUDS
C IN  DEPLM   : DEPLACEMENT A L'INSTANT PRECEDENT
C IN  SIGM    : CONTRAINTES A L'INSTANT PRECEDENT
C OUT VECTU   : FORCES NODALES
C.......................................................................
C

      LOGICAL GRAND,CALBN,AXI
      INTEGER CODRE
      CHARACTER*8 NOMRES(2)
      CHARACTER*16 PHENOM
      INTEGER KPG,I,II,INO,IA,J,K,KL,PROJ,NBPG2
      INTEGER NDIM,NNOS,JGANO,KP,IAA
      REAL*8 D(6,6),F(3,3),EPS(6),R,S
      REAL*8 POIDS,POIPG2(8)
      REAL*8 JAC,SIGAS(6,8),INVJA(3,3),BI(3,8),HX(3,4)
      REAL*8 GAM(4,8),COOPG2(24),H(8,4),DH(4,24)
      REAL*8 QPLUS(72)
      REAL*8 BN(6,3,8)
      REAL*8 DFDX(8),DFDY(8),DFDZ(8)
      REAL*8 NU,NUB,RAC2,DEN,BID
      REAL*8 VALRES(2)
      DATA H/ 1.D0, 1.D0, -1.D0,-1.D0,-1.D0,-1.D0, 1.D0, 1.D0,
     &        1.D0,-1.D0, -1.D0, 1.D0,-1.D0, 1.D0, 1.D0,-1.D0,
     &        1.D0,-1.D0,  1.D0,-1.D0, 1.D0,-1.D0, 1.D0,-1.D0,
     &       -1.D0, 1.D0, -1.D0, 1.D0, 1.D0,-1.D0, 1.D0,-1.D0/

C - INITIALISATION
C   ==============

C    PROJ : INDICATEUR DE LA PROJECTION
C           0 AUCUNE
C           1 ADS
C           2 ASQBI
C
      IF(COMPOR(1).EQ.'ELAS            ') THEN
        PROJ= 2
      ELSE
        PROJ= 1
      ENDIF
      RAC2 = SQRT(2.D0)
      GRAND = .FALSE.

C - INITIALISATION HEXAS8
      CALL ELRAGA ( 'HE8', 'FPG8    ', NDIM, NBPG2, COOPG2, POIPG2)
      CALL ELREF4 ( 'HE8', 'MASS', NDIM, NNO, NNOS, NBPG2, IPOID2,
     &                                           IVF2, IDFDE2, JGANO )

C - CALCUL DES COEFFICIENTS BI (MOYENNE DES DERIVEES DES FCTS DE FORME)
      CALL R8INIR(3*NNO,0.D0,BI,1)
      DEN = 0.D0
      DO 2 KPG = 1,NBPG2
        CALL DFDM3D ( NNO, KPG, IPOID2, IDFDE2, GEOM,
     &                    DFDX, DFDY, DFDZ, JAC )
        DEN = DEN + JAC
        DO 3 INO = 1,NNO
          BI(1,INO) = BI(1,INO) + JAC * DFDX(INO)
          BI(2,INO) = BI(2,INO) + JAC * DFDY(INO)
          BI(3,INO) = BI(3,INO) + JAC * DFDZ(INO)
   3    CONTINUE
   2  CONTINUE
      DO 4 I = 1,3
        DO 5 INO = 1,NNO
          BI(I,INO) = BI(I,INO)/ DEN
   5    CONTINUE
   4  CONTINUE

C - CALCUL DES COEFFICIENTS GAMMA

      DO 6 I = 1,4
        DO 7 K = 1,3
          HX(K,I) = 0.D0
          DO 8 J = 1,NNO
            HX(K,I) = HX(K,I) + H(J,I) * GEOM(K,J)
   8      CONTINUE
   7    CONTINUE
   6  CONTINUE

      DO 9 I = 1,4
        DO 10 J = 1,NNO
          S = 0.D0
          DO 11 K = 1,3
            S = S + HX(K,I) * BI(K,J)
   11     CONTINUE
        GAM(I,J) = 0.125D0 * (H(J,I) - S)
   10   CONTINUE
   9  CONTINUE

C - CALCUL POUR LE POINT DE GAUSS CENTRAL
      KPG = 1
C
C  RECUP DU COEF DE POISSON POUR ASQBI
C
      CALL RCCOMA(IMATE,'ELAS',PHENOM,CODRE)
      NOMRES(1)='E'
      IF(PHENOM.EQ.'ELAS') THEN
         NOMRES(2)='NU'
      ELSE IF (PHENOM.EQ.'ELAS_ISTR') THEN
         NOMRES(2)='NU_LT'
      ELSE IF (PHENOM.EQ.'ELAS_ORTH') THEN
         NOMRES(2)='NU_LT'
      ELSE
         CALL ASSERT (.FALSE.)
      ENDIF
C
      CALL RCVALB('FPG1',1,1,'+',IMATE,' ',PHENOM,0,' ',0.D0,1,
     &                 NOMRES(2),VALRES(2),CODRE, 1)
      IF(CODRE.EQ.0) THEN
         NU = VALRES(2)
      ELSE
        CALL U2MESS('F','ELEMENTS4_72')
      ENDIF

      NUB = NU/(1.D0-NU)

      AXI = .FALSE.
      CALL NMGEOM(3,NNO,AXI,GRAND,GEOM,KPG,
     &              IPOIDS,IVF,IDFDE,DEPLM,.TRUE.,POIDS,DFDI,F,EPS,R)

C      CALCUL DES PRODUITS SYMETR. DE F PAR N,
        DO 41 I=1,NNO
          DO 31 J=1,3
            DEF(1,J,I) =  F(J,1)*DFDI(I,1)
            DEF(2,J,I) =  F(J,2)*DFDI(I,2)
            DEF(3,J,I) =  F(J,3)*DFDI(I,3)
            DEF(4,J,I) = (F(J,1)*DFDI(I,2) + F(J,2)*DFDI(I,1))/RAC2
            DEF(5,J,I) = (F(J,1)*DFDI(I,3) + F(J,3)*DFDI(I,1))/RAC2
            DEF(6,J,I) = (F(J,2)*DFDI(I,3) + F(J,3)*DFDI(I,2))/RAC2
 31       CONTINUE
 41     CONTINUE

        DO 180 I = 1,72
          QPLUS(I) = SIGM(I+6,KPG)
  180   CONTINUE

        CALL R8INIR(3*NNO,0.D0,VECTU,1)
        CALL R8INIR(6*NBPG2,0.D0,SIGAS,1)

        CALBN = .TRUE.

C      OPERATEUR DE STABILISATION DU GRADIENT AUX 8 POINTS DE GAUSS

        DO 290 KPG = 1,NBPG2
          KP = 3*(KPG-1)
          CALL INVJAC ( NNO, KPG, IPOID2, IDFDE2, GEOM,
     &                  INVJA, JAC )

          DO 165 I = 1,3
            DH(1,3*(KPG-1)+I) = COOPG2(3*KPG-1) * INVJA(3,I) +
     &                          COOPG2(3*KPG)   * INVJA(2,I)
  165     CONTINUE

          DO 166 I = 1,3
            DH(2,3*(KPG-1)+I) = COOPG2(3*KPG-2) * INVJA(3,I) +
     &                          COOPG2(3*KPG)   * INVJA(1,I)
  166     CONTINUE

          DO 167 I = 1,3
            DH(3,3*(KPG-1)+I) = COOPG2(3*KPG-2) * INVJA(2,I) +
     &                          COOPG2(3*KPG-1) * INVJA(1,I)
  167     CONTINUE

          DO 168 I = 1,3
            DH(4,3*(KPG-1)+I) =
     &       COOPG2(3*KPG-2) * COOPG2(3*KPG-1) * INVJA(3,I) +
     &       COOPG2(3*KPG-1) * COOPG2(3*KPG)   * INVJA(1,I) +
     &       COOPG2(3*KPG-2) * COOPG2(3*KPG)   * INVJA(2,I)
  168     CONTINUE

C
C  CALCUL DE BN AU POINT DE GAUSS KPG
C
          CALL CAST3D(PROJ,GAM,DH,DEF,NNO,KPG,NUB,NU,
     &                D,CALBN,BN,JAC,BID)

C    CONTRAINTES DE HOURGLASS

         DO 32 I = 1,6
           II = 12*(I-1)
           DO 34 IA = 1,4
             IAA = 3*(IA-1)
             DO 35 J= 1,3
               SIGAS(I,KPG) = SIGAS(I,KPG) + QPLUS(II+IAA+J)
     &                                       * DH(IA,KP+J)
  35         CONTINUE
  34       CONTINUE
  32     CONTINUE

C     CALCUL DES FORCES INTERNES

          DO 250 I = 1,NNO
            DO 240 J = 1,3
              DO 230 KL = 1,3
                VECTU(J,I) = VECTU(J,I) + (DEF(KL,J,I)+ BN(KL,J,I))*
     &                       (SIGAS(KL,KPG)+SIGM(KL,1))*JAC
     &                       + (RAC2*DEF(KL+3,J,I)+ BN(KL+3,J,I))*
     &                       (SIGAS(KL+3,KPG)+SIGM(KL+3,1))*JAC
  230         CONTINUE
  240       CONTINUE
  250     CONTINUE

  290   CONTINUE

      END
