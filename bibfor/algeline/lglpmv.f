      SUBROUTINE LGLPMV(CUMUL,N,A,X,Y)
C
        IMPLICIT       NONE
        CHARACTER*(*)  CUMUL
        INTEGER        N
        REAL*8         A(6,6), X(6),Y(6)
C ======================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGELINE  DATE 17/06/2003   AUTEUR CIBHHBC R.FERNANDES 
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
C --- BUT : CALCUL DU PRODUIT MATRICE-VECTEUR --------------------------
C ======================================================================
C IN  : CUMUL  : K* :   ON CUMULE OU NON DANS LE VECTEUR RESULTAT Y ----
C --- : CUMUL = 'ZERO' ON MET Y A ZERO AVANT DE COMMENCER --------------
C --- : CUMUL = 'CUMU' ON ACCUMULE DANS Y ------------------------------
C --- : N      : DIMENSION REELLE DES MATRICES -------------------------
C --- : DIM    : DIMENSION EFFECTIVE DES MATRICES ----------------------
C --- : A      : MATRICE A ---------------------------------------------
C --- : X      : VECTEUR X ---------------------------------------------
C OUT : Y      : VECTEUR Y = A * X -------------------------------------
C ======================================================================
        INTEGER I, J
C ======================================================================
        IF ( CUMUL .EQ. 'ZERO' ) THEN
           DO 1 I = 1 , N
              Y(I) = 0.0D0
 1         CONTINUE
        ENDIF
C ======================================================================
        DO 3 J = 1 , N
           DO 2 I = 1 , N
              Y(I) = Y(I) + A(I,J) * X(J)
 2         CONTINUE
 3      CONTINUE
C ======================================================================
        END
