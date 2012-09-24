      SUBROUTINE FGEQUI ( TZ, TYPZ, NDIM, EQUI )
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/09/2012   AUTEUR DELMAS J.DELMAS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C ----------------------------------------------------------------------
      IMPLICIT NONE
C     CALCUL DES GRANDEURS EQUIVALENTES EN CONTRAINTE ET DEFORMATION
C     SOIT DANS L ORDRE :
C                . CONTRAINTES EQUIVALENTES  :
C                        . VON MISES                    (= 1 VALEUR)
C                        . TRESCA                       (= 1 VALEUR)
C                        . CONTRAINTES PRINCIPALES      (= 3 VALEURS)
C                        . VON-MISES * SIGNE (PRESSION) (= 1 VALEUR)
C                        . DIRECTION DES CONTRAINTES PRINCIPALES
C                                                       (=3*3 VALEURS)
C                        . TRACE                        (= 1 VALEUR)
C                        . TAUX DE TRIAXIALITE          (= 1 VALEUR)
C               . DEFORMATIONS EQUIVALENTES  :
C                        . SECOND INVARIANT             (= 1 VALEUR)
C                        . DEFORMATIONS PRINCIPALES     (= 3 VALEURS)
C                        . 2EME INV. * SIGNE (1ER.INV.) (= 1 VALEUR)
C                        . DIRECTION DES CONTRAINTES PRINCIPALES
C                                                       (=3*3 VALEURS)
C ----------------------------------------------------------------------
C     IN     TZ    TENSEUR CONTRAINTE OU DEFORMATION (XX YY ZZ XY XZ YZ)
C            TYPZ  TYPE DU TENSEUR 'SIGM' OU 'EPSI' SUIVI EVENTUELLEMENT
C            DES CARACTERES _DIR POUR CALCULER LES VECTEURS DIRECTEURS
C            NDIM  DIMENSION ESPACE 3 OU 2
C     OUT    EQUI  VECTEUR DES GRANDEURS EQUIVALENTES
C ----------------------------------------------------------------------
      REAL*8         TZ(*),T(6),TN(6),TR(6),TU(6),VECP(3,3), NUL(6)
      REAL*8         EQUI(*) , RAC2  , HYD, JACAUX(3)
      REAL*8         LCIV2S,   LCIV2E
      REAL*8         TOL, TOLDYN
      INTEGER        NBVEC, NPERM
      INTEGER        NDIM, TYPE, IORDRE
      CHARACTER*(*)    TYPZ
      CHARACTER*8 TYP
      CHARACTER*3    LCQEQV
      COMMON /TDIM/  NT,ND
C-----------------------------------------------------------------------
      INTEGER I ,J ,ND ,NITJAC ,NT 
C-----------------------------------------------------------------------
      DATA   NUL     /6*0.D0/
      DATA   NPERM ,TOL,TOLDYN    /12,1.D-10,1.D-2/
C ----------------------------------------------------------------------
      TYP=TYPZ
      
      IF      ( NDIM .EQ. 3 )THEN
         NT = 6
         ND = 3
      ELSE IF ( NDIM .EQ. 2 ) THEN
         NT = 4
         ND = 3
      ELSE IF ( NDIM .EQ. 1 ) THEN
         NT = 1
         ND = 1
      ENDIF

      CALL R8INIR(6,0.D0,T,1)
      DO 30 I = 1,NT
         T(I) = TZ(I)
 30   CONTINUE
C
C --- TENSEUR TN = (XX YY ZZ RAC2.XY RAC2.XZ RAC2.YZ) (POUR LCIV2E)
C
      RAC2 = SQRT (2.D0)
      DO 10 I  = 1,ND
         TN(I)   = T(I)
 10   CONTINUE
      DO 12 I  = ND+1 , NT
         TN(I)   = RAC2 * T(I)
 12   CONTINUE
C
C --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR JACOBI)
C
      TR(1) = T(1)
      TR(2) = T(4)
      TR(3) = T(5)
      TR(4) = T(2)
      TR(5) = T(6)
      TR(6) = T(3)
C
C --- MATRICE UNITE = (1 0 0 1 0 1) (POUR JACOBI)
C
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
C
C --- VALEURS PRINCIPALES
      NBVEC = 3
C
      DO 20 I = 1, NBVEC
         DO 22 J = 1, NBVEC
            VECP(J,I) = 0.0D0
 22      CONTINUE
 20   CONTINUE
C
C --- DEFORMATIONS
C
      IF      ( TYP(1:4) .EQ. 'EPSI' ) THEN
C ------ SECOND INVARIANT
         EQUI(1) =  LCIV2E (TN)
         IF ( LCQEQV(TR,NUL) .EQ. 'OUI' ) THEN
            EQUI(2) = 0.D0
            EQUI(3) = 0.D0
            EQUI(4) = 0.D0
         ELSE
            TYPE = 0
            IORDRE = 0
            CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,TR,TU,VECP,EQUI(2),
     &                  JACAUX,NITJAC,TYPE,IORDRE)
         ENDIF
C ------ PREMIER INVARIANT
         CALL LCHYDR (TN,HYD)
C ------ EQUIVALENT FATIGUE = SECOND INVARIANT * SIGNE(PREMIER INV)
         IF ( HYD .GE. 0.D0 )  EQUI(5) =   EQUI(1)
         IF ( HYD .LT. 0.D0 )  EQUI(5) = - EQUI(1)
C
C ------ DIRECTION DES DEFORMATIONS PRINCIPALES DANS EQUI
C -      DANS L ORDRE LES COORDONNEES DU VECTEUR PUIS PASSAGE
C -      A L AUTRE VECTEUR
         IF (TYP(5:8).EQ.'_DIR') THEN
            DO 100 I=1,NBVEC
               DO 102 J=1,NBVEC
                  EQUI(5+((I-1)*NBVEC)+J) = VECP(J,I)
 102           CONTINUE
 100        CONTINUE
         ENDIF
C
C --- CONTRAINTES
C
      ELSE IF ( TYP(1:4) .EQ. 'SIGM' ) THEN
C ------ VON MISES = SECOND INVARIANT
         EQUI(1) =  LCIV2S (TN)
C
         IF ( LCQEQV(TR,NUL) .EQ. 'OUI' ) THEN
            EQUI(3) = 0.D0
            EQUI(4) = 0.D0
            EQUI(5) = 0.D0
         ELSE
            TYPE = 0
            IORDRE = 0
            CALL JACOBI(NBVEC,NPERM,TOL,TOLDYN,TR,TU,VECP,EQUI(3),
     &                  JACAUX,NITJAC,TYPE,IORDRE)
         ENDIF
C ------ TRESCA = MAX DIFF VALEURS PRINCIPALES
         EQUI(2) = MAX ( ABS(EQUI(3)-EQUI(4)),
     +                   ABS(EQUI(3)-EQUI(5)),
     +                   ABS(EQUI(4)-EQUI(5)) )

C ------ PREMIER INVARIANT
         CALL LCHYDR (TN,HYD)
C ------ EQUIVALENT FATIGUE = SECOND INVARIANT * SIGNE(PREMIER INV)
         IF ( HYD .GE. 0.D0 )EQUI(6) =   EQUI(1)
         IF ( HYD .LT. 0.D0 )EQUI(6) = - EQUI(1)
C
C ------ DIRECTION DES CONTRAINTES PRINCIPALES DANS EQUI
C -      DANS L ORDRE LES COORDONNEES DU VECTEUR PUIS PASSAGE
C -      A L AUTRE VECTEUR
         IF (TYP(5:8).EQ.'_DIR') THEN
            DO 200 I=1,NBVEC
               DO 202 J=1,NBVEC
                  EQUI(6+((I-1)*NBVEC)+J) = VECP(J,I)
 202           CONTINUE
 200        CONTINUE

C ------    TRACE DES CONTRAINTES : TRSIG
            EQUI(16) = T(1)+T(2)+T(3)

C ------    TRIAXIALITE DES CONTRAINTES : TRIAX

            IF(EQUI(1).GT.TOL*ABS(EQUI(16))) THEN
               EQUI(17) = EQUI(16)/3.D0/EQUI(1)
            ELSE
               EQUI(17) = 0.D0
            ENDIF
         ENDIF
      ENDIF
C
      END
