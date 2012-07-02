      SUBROUTINE RVPSTD(VALEE,TYPE,CODIR,VALDIR,VALEQ)
      IMPLICIT NONE
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
C
      REAL*8       VALEE(*),VALEQ(*),VALDIR(*)
      CHARACTER*2  TYPE
      INTEGER      CODIR, INDIR1(3), INDIR2(4), INDIR3(3)
C
C-----------------------------------------------------------------------
      INTEGER I 
      REAL*8 R8VIDE 
C-----------------------------------------------------------------------
      DATA INDIR1 / 2 , 1 , 3 /
      DATA INDIR2 / 2 , 1 , 4 , 3 /
      DATA INDIR3 / 2 , 3 , 1  /
C
C**********************************************************************
C
C  OPERATION REALISEE
C  ------------------
C
C     CALCUL DE LA TRACE NORMALE EN UN POINT
C
C  ARGUMENTS EN ENTREE
C  -------------------
C
C     VALEE  : TABLE DES VALEUR DES CMP (REPERE GLOBAL)
C     TYPE     VAUT 'V3' POUR LES VECTEURS ET
C              'T2' POUR LES TENSEUR 2X2 ET 'T3' POUR LES 3X3
C     CODIR  : CODE LES DIRECTIONS ACTIVES
C     VALDIR : VALEURS DU VECTEUR DIRECTION (TJS X, Y, Z)
C
C  ARGUMENTS EN SORTIE
C  -------------------
C
C     VALEE  : TABLE DES VALEUR DES TRACES(REPERE GLOBAL)
C
C**********************************************************************
C
C==================== CORPS DE LA ROUTINE =============================
C
      IF ( CODIR .EQ. 1 ) THEN
C
C     /* DIRECTION ACTIVE : X */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            IF ( VALEE(1) .EQ. R8VIDE() ) THEN
               VALEQ(1) = 0.D0
            ELSE
               VALEQ(1) = VALEE(1)*VALDIR(1)
            ENDIF
C
         ELSE IF ( TYPE .EQ. 'T3' ) THEN
C
            DO 10 I = 1 , 3
               IF ( VALEE(I) .EQ. R8VIDE() ) THEN
                  VALEQ(I) = 0.D0
               ELSE
                  VALEQ(I) = VALEE(I)*VALDIR(1)
               ENDIF
 10         CONTINUE
C
         ELSE
C
            DO 12 I = 1 , 4
               IF ( VALEE(I) .EQ. R8VIDE() ) THEN
                  VALEQ(I) = 0.D0
               ELSE
                  VALEQ(I) = VALEE(I)*VALDIR(1)
               ENDIF
 12         CONTINUE
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 2 ) THEN
C
C     /* DIRECTION ACTIVE : Y */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            IF ( VALEE(1) .EQ. R8VIDE() ) THEN
               VALEQ(1) = 0.D0
            ELSE
               VALEQ(1) = VALEE(1)*VALDIR(2)
            ENDIF
C
         ELSE IF ( TYPE .EQ. 'T3' ) THEN
C
            DO 22 I = 1 , 3
               IF ( VALEE(INDIR1(I)) .EQ. R8VIDE() ) THEN
                  VALEQ(I) = 0.D0
               ELSE
                  VALEQ(I) = VALEE(INDIR1(I))*VALDIR(2)
               ENDIF
 22         CONTINUE
C
         ELSE
C
            DO 24 I = 1 , 4
               IF ( VALEE(INDIR2(I)) .EQ. R8VIDE() ) THEN
                  VALEQ(I) = 0.D0
               ELSE
                  VALEQ(I) = VALEE(INDIR2(I))*VALDIR(2)
               ENDIF
 24         CONTINUE
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 3 ) THEN
C
C     /* DIRECTION ACTIVE : Z */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            IF ( VALEE(1) .EQ. R8VIDE() ) THEN
               VALEQ(1) = 0.D0
            ELSE
               VALEQ(1) = VALEE(1)*VALDIR(3)
            ENDIF
C
         ELSE
C
            DO 30 I = 1 , 3
               IF ( VALEE(INDIR3(I)) .EQ. R8VIDE() ) THEN
                  VALEQ(I) = 0.D0
               ELSE
                  VALEQ(I) = VALEE(INDIR3(I))*VALDIR(3)
               ENDIF
 30         CONTINUE
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 4 ) THEN
C
C     /* DIRECTION ACTIVE : X,Y */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            DO 40 I = 1 , 2
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 40         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(2)*VALDIR(2)
C
         ELSE IF (TYPE .EQ. 'T3' ) THEN
C
            DO 42 I = 1 , 5
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 42         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(3)*VALDIR(2)
            VALEQ(2) = VALEE(3)*VALDIR(1) + VALEE(2)*VALDIR(2)
            VALEQ(3) = VALEE(4)*VALDIR(1) + VALEE(5)*VALDIR(2)
C
         ELSE
C
            DO 44 I = 1 , 6
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 44         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(3)*VALDIR(2)
            VALEQ(2) = VALEE(3)*VALDIR(1) + VALEE(2)*VALDIR(2)
            VALEQ(3) = VALEE(4)*VALDIR(1) + VALEE(6)*VALDIR(2)
            VALEQ(4) = VALEE(6)*VALDIR(1) + VALEE(5)*VALDIR(2)
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 5 ) THEN
C
C     /* DIRECTION ACTIVE : X,Z */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            DO 50 I = 1 , 2
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 50         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(2)*VALDIR(3)
C
         ELSE
C
            DO 52 I = 1 , 5
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 52         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(4)*VALDIR(3)
            VALEQ(2) = VALEE(3)*VALDIR(1) + VALEE(5)*VALDIR(3)
            VALEQ(3) = VALEE(4)*VALDIR(1) + VALEE(2)*VALDIR(3)
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 6 ) THEN
C
C     /* DIRECTION ACTIVE : Y,Z */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            DO 60 I = 1 , 2
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 60         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(2) + VALEE(2)*VALDIR(3)
C
         ELSE
C
            DO 62 I = 1 , 5
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 62         CONTINUE
            VALEQ(1) = VALEE(3)*VALDIR(2) + VALEE(4)*VALDIR(3)
            VALEQ(2) = VALEE(1)*VALDIR(2) + VALEE(5)*VALDIR(3)
            VALEQ(3) = VALEE(5)*VALDIR(2) + VALEE(2)*VALDIR(3)
C
         ENDIF
C
      ELSE
C
C     /* DIRECTION ACTIVE : X,Y,Z */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            DO 70 I = 1 , 3
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 70         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(2)*VALDIR(2) +
     +                 VALEE(3)*VALDIR(3)
C
         ELSE
C
            DO 72 I = 1 , 6
               IF ( VALEE(I) .EQ. R8VIDE() )  VALEE(I) = 0.D0
 72         CONTINUE
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(4)*VALDIR(2) +
     +                 VALEE(5)*VALDIR(3)
            VALEQ(2) = VALEE(4)*VALDIR(1) + VALEE(2)*VALDIR(2) +
     +                 VALEE(6)*VALDIR(3)
            VALEQ(3) = VALEE(5)*VALDIR(1) + VALEE(6)*VALDIR(2) +
     +                 VALEE(3)*VALDIR(3)
C
         ENDIF
C
      ENDIF
C
      END
