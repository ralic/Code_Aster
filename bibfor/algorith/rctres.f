      SUBROUTINE RCTRES ( SIGM, TRESCA )
      IMPLICIT   NONE
      REAL*8              SIGM(*), TRESCA
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/06/2007   AUTEUR VIVAN L.VIVAN 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C ----------------------------------------------------------------------
C     CALCUL DU TRESCA 
C ----------------------------------------------------------------------
C     IN    T       TENSEUR CONTRAINTE 
C     OUT   TRESCA  LE TRESCA
C ----------------------------------------------------------------------
      REAL*8         TR(6) , TU(6) , NUL(6)
      REAL*8         EQUI(3)
      INTEGER        NT, ND
      CHARACTER*3    LCQEQV
      COMMON /TDIM/  NT, ND
      DATA   NUL     /6*0.D0/
C ----------------------------------------------------------------------
      NT = 6
      ND = 3
C
      TU(1) = 1.D0
      TU(2) = 0.D0
      TU(3) = 0.D0
      TU(4) = 1.D0
      TU(5) = 0.D0
      TU(6) = 1.D0
C
C --- MATRICE TR = (XX XY XZ YY YZ ZZ) (POUR JACOBI)
C
      TR(1) = SIGM(1)
      TR(2) = SIGM(4)
      TR(3) = SIGM(5)
      TR(4) = SIGM(2)
      TR(5) = SIGM(6)
      TR(6) = SIGM(3)
C
      IF ( LCQEQV(TR,NUL) .EQ. 'OUI' ) THEN
         TRESCA = 0.D0
      ELSE
         CALL RCJACO ( TR, TU, EQUI )
C ------ TRESCA = MAX DIFF VALEURS PRINCIPALES
         TRESCA = MAX ( ABS(EQUI(1)-EQUI(2)),
     +                  ABS(EQUI(1)-EQUI(3)),
     +                  ABS(EQUI(2)-EQUI(3)) )
      ENDIF
C
      END
