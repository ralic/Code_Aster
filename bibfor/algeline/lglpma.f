        SUBROUTINE LGLPMA(N,DIM,A,B,C)
C
        IMPLICIT    NONE
        INTEGER     N, DIM
        REAL*8      A(DIM,DIM), B(DIM,DIM), C(DIM,DIM)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 27/03/2002   AUTEUR CIBHHBC R.FERNANDES 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                                                                       
C                                                                       
C ======================================================================
C ======================================================================
C --- BUT : CALCUL DU PRODUIT MATRICIEL --------------------------------
C ======================================================================
C IN  : N      : DIMENSION REELLE DES MATRICES -------------------------
C --- : DIM    : DIMENSION EFFECTIVE DES MATRICES ----------------------
C --- : A      : MATRICE A ---------------------------------------------
C --- : B      : MATRICE B ---------------------------------------------
C OUT : C      : MATRICE C = A * B -------------------------------------
C ======================================================================
        INTEGER   I, J, K
C ======================================================================
        REAL*8    V
        DO 1 I = 1 , N
          DO 2 J = 1 , N
          V = 0.D0
            DO 3 K = 1 , N
               V = V + A(I,K)*B(K,J)
 3          CONTINUE
            C(I,J) = V
 2        CONTINUE
 1      CONTINUE
C ======================================================================
        END
