      SUBROUTINE RVPSTD(VALEE,TYPE,CODIR,VALDIR,VALEQ)
      IMPLICIT REAL*8 (A-H,O-Z)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF POSTRELE  DATE 06/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
      REAL*8       VALEE(*),VALEQ(*),VALDIR(*)
      CHARACTER*2  TYPE
      INTEGER      CODIR
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
            VALEQ(1) = VALEE(1)*VALDIR(1)
C
         ELSE IF ( TYPE .EQ. 'T3' ) THEN
C
            VALEQ(1) = VALEE(1)*VALDIR(1)
            VALEQ(2) = VALEE(2)*VALDIR(1)
            VALEQ(3) = VALEE(3)*VALDIR(1)
C
         ELSE
C
            VALEQ(1) = VALEE(1)*VALDIR(1)
            VALEQ(2) = VALEE(3)*VALDIR(1)
            VALEQ(3) = VALEE(2)*VALDIR(1)
            VALEQ(4) = VALEE(4)*VALDIR(1)
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 2 ) THEN
C
C     /* DIRECTION ACTIVE : Y */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            VALEQ(1) = VALEE(1)*VALDIR(2)
C
         ELSE IF ( TYPE .EQ. 'T3' ) THEN
C
            VALEQ(1) = VALEE(2)*VALDIR(2)
            VALEQ(2) = VALEE(1)*VALDIR(2)
            VALEQ(3) = VALEE(3)*VALDIR(2)
C
         ELSE
C
            VALEQ(1) = VALEE(2)*VALDIR(2)
            VALEQ(2) = VALEE(1)*VALDIR(2)
            VALEQ(3) = VALEE(4)*VALDIR(2)
            VALEQ(4) = VALEE(3)*VALDIR(2)
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 3 ) THEN
C
C     /* DIRECTION ACTIVE : Z */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            VALEQ(1) = VALEE(1)*VALDIR(3)
C
         ELSE
C
            VALEQ(1) = VALEE(2)*VALDIR(3)
            VALEQ(2) = VALEE(3)*VALDIR(3)
            VALEQ(3) = VALEE(1)*VALDIR(3)
C
         ENDIF
C
      ELSE IF ( CODIR .EQ. 4 ) THEN
C
C     /* DIRECTION ACTIVE : X,Y */
C
         IF ( TYPE.EQ. 'V3' ) THEN
C
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(2)*VALDIR(2)
C
         ELSE IF (TYPE .EQ. 'T3' ) THEN
C
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(3)*VALDIR(2)
            VALEQ(2) = VALEE(3)*VALDIR(1) + VALEE(2)*VALDIR(2)
            VALEQ(3) = VALEE(4)*VALDIR(1) + VALEE(5)*VALDIR(2)
C
         ELSE
C
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
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(2)*VALDIR(3)
C
         ELSE
C
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
            VALEQ(1) = VALEE(1)*VALDIR(2) + VALEE(2)*VALDIR(3)
C
         ELSE
C
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
            VALEQ(1) = VALEE(1)*VALDIR(1) + VALEE(2)*VALDIR(2) +
     +                 VALEE(3)*VALDIR(3)
C
         ELSE
C
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
