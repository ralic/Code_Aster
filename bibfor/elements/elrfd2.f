      SUBROUTINE ELRFD2(ELREFZ,X,DIMD,DFF2,NNO,NDIM)
      IMPLICIT NONE
      INTEGER DIMD,NNO,NDIM
      REAL*8 X(*),DFF2(3,3,*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 02/05/2011   AUTEUR DELMAS J.DELMAS 
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
C RESPONSABLE VABHHTS J.PELLET
C ======================================================================
C

C BUT:   CALCUL DES DERIVEES 2EMES DES FONCTIONS DE FORME
C        AU POINT DE COORDONNEES X

C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFE (K8)
C        X      : COORDONNEES DU POINT DE CALCUL : X(IDIM)
C        DIMD   : DIMENSION DE DFF2
C   OUT  DFF2   : DERIVEES 2EMES DES FONCTIONS DE FORME :
C                 DFF2(IDIM,JDIM,INO)
C        NNO    : NOMBRE DE NOEUDS
C        NDIM   : DIMENSION DE L'ESPACE DE L'ELREFE : 1,2 OU 3
C   -------------------------------------------------------------------
      CHARACTER*8 ELREFE
      INTEGER IDIM,JDIM,INO
      REAL*8 X1,X2,X3,X4,D1,D2,D3,D4,X0,Y0
      REAL*8 ZERO,UNDEMI,UN,DEUX,TROIS,QUATRE,SIX,SEPT,HUIT,UNS4

C DEB ------------------------------------------------------------------
      ELREFE = ELREFZ
      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
      SIX    = 6.0D0
      SEPT   = 7.0D0
      HUIT   = 8.0D0
      UNS4 = UN/QUATRE

C     -- POUR LES ELEMENTS LINEAIRES : C'EST FACILE : 0.
C     ------------------------------------------------------------------
      IF ((ELREFE.EQ.'SE2') .OR. (ELREFE.EQ.'TR3') .OR.
     &    (ELREFE.EQ.'TE4') ) THEN
        IF (ELREFE.EQ.'SE2') THEN
          NNO = 2
          NDIM = 1
        ELSE IF (ELREFE.EQ.'TR3') THEN
          NNO = 3
          NDIM = 2
        ELSE IF (ELREFE.EQ.'TE4') THEN
          NNO = 4
          NDIM = 3
        END IF

        DO 30,IDIM = 1,NDIM
          DO 20,JDIM = 1,NDIM
            DO 10,INO = 1,NNO
              DFF2(IDIM,JDIM,INO) = 0.D0
   10       CONTINUE
   20     CONTINUE
   30   CONTINUE

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TR6') THEN
        NNO = 6
        NDIM = 2

        DFF2(1,1,1) = QUATRE
        DFF2(2,1,1) = QUATRE
        DFF2(1,2,1) = QUATRE
        DFF2(2,2,1) = QUATRE

        DFF2(1,1,2) = QUATRE
        DFF2(2,1,2) = ZERO
        DFF2(1,2,2) = ZERO
        DFF2(2,2,2) = ZERO

        DFF2(1,1,3) = ZERO
        DFF2(2,1,3) = ZERO
        DFF2(1,2,3) = ZERO
        DFF2(2,2,3) = QUATRE

        DFF2(1,1,4) = -HUIT
        DFF2(2,1,4) = -QUATRE
        DFF2(1,2,4) = -QUATRE
        DFF2(2,2,4) =  ZERO

        DFF2(1,1,5) = ZERO
        DFF2(2,1,5) = QUATRE
        DFF2(1,2,5) = QUATRE
        DFF2(2,2,5) = ZERO

        DFF2(1,1,6) =  ZERO
        DFF2(2,1,6) = -QUATRE
        DFF2(1,2,6) = -QUATRE
        DFF2(2,2,6) = -HUIT

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TW6') THEN
        NNO = 6
        NDIM = 2

        DFF2(1,1,1) = -UN
        DFF2(2,1,1) = -UN
        DFF2(1,2,1) = -UN
        DFF2(2,2,1) =  UN

        DFF2(1,1,2) = ZERO
        DFF2(2,1,2) = DEUX
        DFF2(1,2,2) = DEUX
        DFF2(2,2,2) = ZERO

        DFF2(1,1,3) = +UN
        DFF2(2,1,3) = -UN
        DFF2(1,2,3) = -UN
        DFF2(2,2,3) = -UN

        DFF2(1,1,4) = QUATRE
        DFF2(2,1,4) = ZERO
        DFF2(1,2,4) = ZERO
        DFF2(2,2,4) = ZERO

        DFF2(1,1,5) = ZERO
        DFF2(2,1,5) = ZERO
        DFF2(1,2,5) = ZERO
        DFF2(2,2,5) = QUATRE

        DFF2(1,1,6) = 2.8284271247461901D0
        DFF2(2,1,6) = 2.8284271247461901D0
        DFF2(1,2,6) = 2.8284271247461901D0
        DFF2(2,2,6) = 2.8284271247461901D0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TR7') THEN
        X0 = X(1)
        Y0 = X(2)
        NNO = 7
        NDIM = 2

        DFF2(1,1,1) = QUATRE - SIX * Y0
        DFF2(2,1,1) = SEPT - SIX * ( X0 + Y0 )
        DFF2(1,2,1) = SEPT - SIX * ( X0 + Y0 )
        DFF2(2,2,1) = QUATRE - SIX * X0

        DFF2(1,1,2) = QUATRE - SIX * Y0
        DFF2(2,1,2) = TROIS - SIX * ( X0 + Y0 )
        DFF2(1,2,2) = TROIS - SIX * ( X0 + Y0 )
        DFF2(2,2,2) = - SIX * X0

        DFF2(1,1,3) =  - SIX * Y0
        DFF2(2,1,3) = TROIS - SIX * ( X0 + Y0 )
        DFF2(1,2,3) = TROIS - SIX * ( X0 + Y0 )
        DFF2(2,2,3) = QUATRE - SIX * X0

        DFF2(1,1,4) = HUIT * ( -UN + TROIS * Y0 )
        DFF2(2,1,4) = QUATRE * ( -QUATRE + SIX * ( X0 + Y0 ))
        DFF2(1,2,4) = QUATRE * ( -QUATRE + SIX * ( X0 + Y0 ))
        DFF2(2,2,4) = 24.0D0 * X0

        DFF2(1,1,5) = 24.0D0 * Y0
        DFF2(2,1,5) = QUATRE * ( -DEUX + SIX * ( X0 + Y0 ))
        DFF2(1,2,5) = QUATRE * ( -DEUX + SIX * ( X0 + Y0 ))
        DFF2(2,2,5) = 24.0D0 * X0

        DFF2(1,1,6) = 24.0D0 * Y0
        DFF2(2,1,6) = QUATRE * ( -QUATRE + SIX * ( X0 + Y0 ))
        DFF2(1,2,6) = QUATRE * ( -QUATRE + SIX * ( X0 + Y0 ))
        DFF2(2,2,6) = HUIT * ( -UN + TROIS * X0 )

        DFF2(1,1,7) = -54.0D0 * Y0
        DFF2(2,1,7) = 27.0D0 * ( UN - DEUX * ( X0 + Y0 ))
        DFF2(1,2,7) = 27.0D0 * ( UN - DEUX * ( X0 + Y0 ))
        DFF2(2,2,7) = -54.0D0 * X0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU4') THEN
        X0 = X(1)
        Y0 = X(2)
        NNO = 4
        NDIM = 2

        DFF2(1,1,1) = ZERO
        DFF2(2,1,1) = UNS4
        DFF2(1,2,1) = UNS4
        DFF2(2,2,1) = ZERO

        DFF2(1,1,2) = ZERO
        DFF2(2,1,2) = -UNS4
        DFF2(1,2,2) = -UNS4
        DFF2(2,2,2) = ZERO

        DFF2(1,1,3) = ZERO
        DFF2(2,1,3) = UNS4
        DFF2(1,2,3) = UNS4
        DFF2(2,2,3) = ZERO

        DFF2(1,1,4) = ZERO
        DFF2(2,1,4) = -UNS4
        DFF2(1,2,4) = -UNS4
        DFF2(2,2,4) = ZERO

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU6') THEN
        X0 = X(1)
        Y0 = X(2)
        NNO = 6
        NDIM = 2

        DFF2(1,1,1) = UNDEMI*(UN - Y0)
        DFF2(2,1,1) = ZERO
        DFF2(1,2,1) = ZERO
        DFF2(2,2,1) = UNDEMI*(UN - X0) - 0.25D0

        DFF2(1,1,2) = UNDEMI*(UN - Y0)
        DFF2(2,1,2) = ZERO
        DFF2(1,2,2) = ZERO
        DFF2(2,2,2) = 0.25D0 - UNDEMI*(UN + X0)

        DFF2(1,1,3) = UNDEMI*(UN + Y0)
        DFF2(2,1,3) = ZERO
        DFF2(1,2,3) = ZERO
        DFF2(2,2,3) = UNDEMI*(UN + X0) - 0.25D0

        DFF2(1,1,4) = UNDEMI*(UN + Y0)
        DFF2(2,1,4) = ZERO
        DFF2(1,2,4) = ZERO
        DFF2(2,2,4) = 0.25D0 - UNDEMI*(UN - X0)

        DFF2(1,1,5) = Y0 - UN
        DFF2(2,1,5) = ZERO
        DFF2(1,2,5) = ZERO
        DFF2(2,2,5) = X0

        DFF2(1,1,6) = -Y0 - UN
        DFF2(2,1,6) = ZERO
        DFF2(1,2,6) = ZERO
        DFF2(2,2,6) = -X0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU8') THEN
        X0 = X(1)
        Y0 = X(2)
        NNO = 8
        NDIM = 2

        DFF2(1,1,1) = (UN - Y0) * UNDEMI
        DFF2(2,1,1) = (UN - DEUX*X0 - DEUX*Y0) * UNS4
        DFF2(1,2,1) = (UN - DEUX*X0 - DEUX*Y0) * UNS4
        DFF2(2,2,1) = (UN - X0) * UNDEMI

        DFF2(1,1,2) = (UN - Y0) * UNDEMI
        DFF2(2,1,2) = -(UN + DEUX*X0 - DEUX*Y0) * UNS4
        DFF2(1,2,2) = -(UN + DEUX*X0 - DEUX*Y0) * UNS4
        DFF2(2,2,2) = (UN + X0) * UNDEMI

        DFF2(1,1,3) = (UN + Y0) * UNDEMI
        DFF2(2,1,3) = (UN + DEUX*X0 + DEUX*Y0) * UNS4
        DFF2(1,2,3) = (UN + DEUX*X0 + DEUX*Y0) * UNS4
        DFF2(2,2,3) = (UN + X0) * UNDEMI

        DFF2(1,1,4) = (UN + Y0) * UNDEMI
        DFF2(2,1,4) = -(UN - DEUX*X0 + DEUX*Y0) * UNS4
        DFF2(1,2,4) = -(UN - DEUX*X0 + DEUX*Y0) * UNS4
        DFF2(2,2,4) = (UN - X0) * UNDEMI

        DFF2(1,1,5) = -UN + Y0
        DFF2(2,1,5) = X0
        DFF2(1,2,5) = X0
        DFF2(2,2,5) = ZERO

        DFF2(1,1,6) = ZERO
        DFF2(2,1,6) = -Y0
        DFF2(1,2,6) = -Y0
        DFF2(2,2,6) = -UN - X0

        DFF2(1,1,7) = -UN - Y0
        DFF2(2,1,7) = -X0
        DFF2(1,2,7) = -X0
        DFF2(2,2,7) = ZERO

        DFF2(1,1,8) = ZERO
        DFF2(2,1,8) = Y0
        DFF2(1,2,8) = Y0
        DFF2(2,2,8) = -UN + X0
C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU9') THEN
        X0 = X(1)
        Y0 = X(2)
        NNO = 9
        NDIM = 2

        DFF2(1,1,1) = Y0 * (Y0 - UN) * UNDEMI
        DFF2(2,1,1) = (X0 - UNDEMI) * (Y0 - UNDEMI)
        DFF2(1,2,1) = (X0 - UNDEMI) * (Y0 - UNDEMI)
        DFF2(2,2,1) = X0 * (X0 - UN) * UNDEMI

        DFF2(1,1,2) = Y0 * (Y0 - UN) * UNDEMI
        DFF2(2,1,2) = (X0 + UNDEMI) * (Y0 - UNDEMI)
        DFF2(1,2,2) = (X0 + UNDEMI) * (Y0 - UNDEMI)
        DFF2(2,2,2) = X0 * (X0 + UN) * UNDEMI

        DFF2(1,1,3) = Y0 * (Y0 + UN) * UNDEMI
        DFF2(2,1,3) = (X0 + UNDEMI) * (Y0 + UNDEMI)
        DFF2(1,2,3) = (X0 + UNDEMI) * (Y0 + UNDEMI)
        DFF2(2,2,3) = X0 * (X0 + UN) * UNDEMI

        DFF2(1,1,4) = Y0 * (Y0 + UN) * UNDEMI
        DFF2(2,1,4) = (X0 - UNDEMI) * (Y0 + UNDEMI)
        DFF2(1,2,4) = (X0 - UNDEMI) * (Y0 + UNDEMI)
        DFF2(2,2,4) = X0 * (X0 - UN) * UNDEMI

        DFF2(1,1,5) = -Y0 * (Y0 - UN)
        DFF2(2,1,5) = -DEUX * X0 * (Y0 - UNDEMI)
        DFF2(1,2,5) = -DEUX * X0 * (Y0 - UNDEMI)
        DFF2(2,2,5) = -(X0 + UN) * (X0 - UN)

        DFF2(1,1,6) = -(Y0 + UN) * (Y0 - UN)
        DFF2(2,1,6) = -DEUX * Y0 * (X0 + UNDEMI)
        DFF2(1,2,6) = -DEUX * Y0 * (X0 + UNDEMI)
        DFF2(2,2,6) = -X0 * (X0 + UN)

        DFF2(1,1,7) = -Y0 * (Y0 + UN)
        DFF2(2,1,7) = -DEUX * X0 * (Y0 + UNDEMI)
        DFF2(1,2,7) = -DEUX * X0 * (Y0 + UNDEMI)
        DFF2(2,2,7) = -(X0 + UN) * (X0 - UN)

        DFF2(1,1,8) = -(Y0 + UN) * (Y0 - UN)
        DFF2(2,1,8) = -DEUX * Y0 * (X0 - UNDEMI)
        DFF2(1,2,8) = -DEUX * Y0 * (X0 - UNDEMI)
        DFF2(2,2,8) =  -X0 * (X0 - UN)

        DFF2(1,1,9) = DEUX * (Y0 + UN) * (Y0 - UN)
        DFF2(2,1,9) = QUATRE * X0 * Y0
        DFF2(1,2,9) = QUATRE * X0 * Y0
        DFF2(2,2,9) = DEUX * (X0 + UN) * (X0 - UN)
C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SE3') THEN
        NNO = 3
        NDIM = 1

        DFF2(1,1,1) = UN
        DFF2(1,1,2) = UN
        DFF2(1,1,3) = -DEUX

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SE4') THEN
        NNO = 4
        NDIM = 1

        X1 = -UN
        X2 =  UN
        X3 = -UN/3.D0
        X4 =  UN/3.D0

        D1 = (X1-X2)* (X1-X3)* (X1-X4)
        D2 = (X2-X3)* (X2-X4)* (X2-X1)
        D3 = (X3-X4)* (X3-X1)* (X3-X2)
        D4 = (X4-X1)* (X4-X2)* (X4-X3)

        DFF2(1,1,1) = 2* ((X(1)-X2)+ (X(1)-X3)+ (X(1)-X4))/D1
        DFF2(1,1,2) = 2* ((X(1)-X3)+ (X(1)-X4)+ (X(1)-X1))/D2
        DFF2(1,1,3) = 2* ((X(1)-X4)+ (X(1)-X1)+ (X(1)-X2))/D3
        DFF2(1,1,4) = 2* ((X(1)-X1)+ (X(1)-X2)+ (X(1)-X3))/D4

C     ------------------------------------------------------------------
C     -- POUR LES ELEREFE NON ENCORE RENSEIGNES, ON REND NDIM=NNO=0
      ELSE
        NNO = 0
        NDIM = 0

      END IF

C     ------------------------------------------------------------------

      CALL ASSERT(DIMD.GE. (NNO*NDIM*NDIM))

      END
