      SUBROUTINE EF213D(NPG, A,
     &                  NNO2, VFF2, DFF2,
     &                  NNO1, VFF1, DFF1)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 28/08/2000   AUTEUR GJBHHEL E.LORENTZ 
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

      IMPLICIT NONE
      INTEGER  A(8, 3), NNO1, NNO2, NPG
      REAL*8   VFF2(NNO2, NPG), DFF2(3, NNO2, NPG)
      REAL*8   VFF1(NNO1, NPG), DFF1(3, NNO1, NPG)

C ----------------------------------------------------------------------
C    CALCUL DES FONCTIONS DE FORME P1 A PARTIR DES FONCTIONS P2  3D
C ----------------------------------------------------------------------
C IN  NPG    NOMBRE DE POINTS DE GAUSS
C IN  A      TABLEAU DE CORRESPONDANCE SOMMET -> NOEUDS MILIEUX
C IN  NNO2   NOMBRE DE NOEUDS P2
C IN  VFF2   VALEUR  DES FONCTIONS DE FORME P2
C IN  DFF2   DERIVEE DES FONCTIONS DE FORME P2
C IN  NNO1   NOMBRE DE NOEUDS P1
C OUT VFF1   VALEUR  DES FONCTIONS DE FORME P1
C OUT DFF1   DERIVEE DES FONCTIONS DE FORME P1
C ----------------------------------------------------------------------

      INTEGER K, N


      DO 10 K = 1, NPG
        DO 20 N = 1, NNO1

          VFF1(N,K)    = VFF2(N,K)
     &                 + VFF2(A(N,1),K)/2.D0
     &                 + VFF2(A(N,2),K)/2.D0
     &                 + VFF2(A(N,3),K)/2.D0

          DFF1(1,N,K)  = DFF2(1,N,K)
     &                 + DFF2(1,A(N,1),K)/2.D0
     &                 + DFF2(1,A(N,2),K)/2.D0
     &                 + DFF2(1,A(N,3),K)/2.D0

          DFF1(2,N,K)  = DFF2(2,N,K)
     &                 + DFF2(2,A(N,1),K)/2.D0
     &                 + DFF2(2,A(N,2),K)/2.D0
     &                 + DFF2(2,A(N,3),K)/2.D0

          DFF1(3,N,K)  = DFF2(3,N,K)
     &                 + DFF2(3,A(N,1),K)/2.D0
     &                 + DFF2(3,A(N,2),K)/2.D0
     &                 + DFF2(3,A(N,3),K)/2.D0


 20     CONTINUE
 10   CONTINUE

      END
