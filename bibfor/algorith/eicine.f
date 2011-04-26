      SUBROUTINE EICINE(NDIM,AXI,NNO1,NNO2,VFF1,VFF2,WREF,DFFR2,GEOM,
     &                  ANG,WG,B)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================

      IMPLICIT NONE
      LOGICAL AXI
      INTEGER NDIM,NNO1,NNO2
      REAL*8  WREF,VFF1(NNO1),VFF2(NNO2),GEOM(NDIM,NNO2),ANG(*)
      REAL*8  DFFR2(NDIM-1,NNO2),WG,B(3,3,2*NNO1)
C-----------------------------------------------------------------------
C  MATRICE CINEMATIQUE POUR LES ELEMENTS D'INTERFACE (EN UN PG DONNE)
C-----------------------------------------------------------------------
C IN  NDIM   DIMENSION DE L'ESPACE
C IN  AXI    .TRUE. SI AXISYMETRIQUE
C IN  NNO1   NB DE NOEUDS DE LA FACE POUR LES DEPLACEMENTS
C IN  NNO2   NB DE NOEUDS DE LA FACE POUR LES LAGRANGES L ET LA GEOM X
C IN  VFF1   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR U
C IN  VFF2   VALEUR DES FONCTIONS DE FORME (DE LA FACE) POUR L ET X
C IN  WREF   POIDS DE REFERENCE DU POINT DE GAUSS
C IN  DFFR2  DERIVEE DES FONCTIONS DE FORME DE REFERENCE DE L ET X EN G
C IN  GEOM   COORDONNEES DES NOEUDS (X)
C IN  ANG    ANGLES NAUTIQUES NODAUX (FAMILLE X)
C OUT WG     POIDS REEL DU POINT DE GAUSS (AVEC DISTORSION)
C OUT B      MATRICE DE PASSAGE UNODAL -> SAUT DE U LOCAL
C-----------------------------------------------------------------------
      INTEGER N,I,J,NANG
      REAL*8 COVA(3,3),METR(2,2),DFDX(9),COUR,JAC,COSA,SINA
      REAL*8 ANGLOC(3),ROT(3,3),R,RMAX
      REAL*8 DDOT
C-----------------------------------------------------------------------

C    CALCUL DU JACOBIEN

      IF (NDIM.EQ.3) THEN
        CALL SUBACO(NNO2,DFFR2,GEOM,COVA)
        CALL SUMETR(COVA,METR,JAC)
        WG = WREF*JAC
      ELSE IF (NDIM.EQ.2) THEN
        CALL DFDM1D(NNO2,WREF,DFFR2,GEOM,DFDX,COUR,WG,COSA,SINA)
      END IF

      IF (AXI) THEN
        R = DDOT(NNO2,GEOM,2,VFF2,1)
C ----------------------------------------------------------------------
C POUR LES ELEMENTS AVEC COUPLAGE HM, DANS LE CAS OU R EGAL 0, ON A UN
C JACOBIEN NUL EN UN PG. ON PRENDS LE MAX DU RAYON MULTIPLIE PAR 1.E-3
C ----------------------------------------------------------------------
        IF (R .EQ. 0.D0) THEN
            RMAX=GEOM(1,1)
            DO 11 N=2,NNO2
               RMAX=MAX(GEOM(1,N),RMAX)
 11         CONTINUE
            WG = WG*1.D-03*RMAX
         ELSE
            WG = R*WG
         ENDIF
      END IF

C    CALCUL DES ANGLES NAUTIQUES AU POINT D'INTEGRATION

      IF (NDIM.EQ.2) NANG = 1
      IF (NDIM.EQ.3) NANG = 3
      CALL R8INIR(3,0.D0,ANGLOC,1)
      DO 10 I = 1,NANG
        ANGLOC(I) = DDOT(NNO2,ANG(I),NANG,VFF2,1)
 10   CONTINUE

C    CALCUL DE LA MATRICE DE ROTATION GLOBAL -> LOCAL

      CALL MATROT(ANGLOC,ROT)

C    CONSTRUCTION DE LA MATRICE B

      DO 20 I = 1,NDIM
        DO 30 J = 1,NDIM
          DO 40 N = 1, NNO1
            B(I,J,N)      = - ROT(I,J)*VFF1(N)
            B(I,J,N+NNO1) =   ROT(I,J)*VFF1(N)
 40       CONTINUE
 30     CONTINUE
 20   CONTINUE

      END
