      SUBROUTINE MMMMPB(RESE  ,NRESE ,NDIM  ,MATPRB)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 17/10/2011   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      REAL*8   RESE(3),NRESE
      INTEGER  NDIM
      REAL*8   MATPRB(3,3)
C      
C ----------------------------------------------------------------------
C
C ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
C
C CALCUL DE LA MATRICE DE PROJECTION SUR LA BOULE UNITE
C
C ----------------------------------------------------------------------
C
C
C IN  RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT (x)
C               GTK = LAMBDAF + COEFAF*VITESSE
C IN  NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
C IN  NDIM   : DIMENSION DU PROBLEME
C IN  VEC    : LE VECTEUR A MULTIPLIER
C OUT MATPRB : MATRICE DE PROJECTION SUR LA BOULE UNITE
C                 K(x) = (Id-x*xt/!!x!!**)1/!!x!!
C
C ----------------------------------------------------------------------
C
      REAL*8  NORME,THETA
      INTEGER I,J
C
C ----------------------------------------------------------------------
C
C --- INITIALISATIONS
C
      CALL MATINI(3,3,0.D0,MATPRB)
      THETA = 1.D0
      IF (NRESE.EQ.0.D0) CALL ASSERT(.FALSE.)
C
C --- CALCUL DE LA NORME DE LAMBDA +RHO[[U]]_TAU
C
      NORME = NRESE*NRESE
C
C --- CALCUL DE LA MATRICE
C
      DO 10 I = 1,NDIM
        DO 15 J = 1,NDIM      
          MATPRB(I,J) = -THETA*RESE(I)*RESE(J)/NORME
 15     CONTINUE
 10   CONTINUE
C
      DO 20 J = 1,NDIM
        MATPRB(J,J) = 1.D0+MATPRB(J,J)
 20   CONTINUE
C
      DO 30 I = 1,NDIM
        DO 35 J = 1,NDIM      
          MATPRB(I,J) = MATPRB(I,J)/NRESE
 35     CONTINUE
 30   CONTINUE
C
      END
