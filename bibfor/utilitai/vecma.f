        SUBROUTINE VECMA(MV,N,MP,M)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILITAI  DATE 29/09/95   AUTEUR GIBHHAY A.Y.PORTABILITE 
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
C       ----------------------------------------------------------------
C       PASSAGE DEMI-MATRICE COLONNE VECTEUR (N)  > MATRICE PLEINE (M*M)
C       IN      MV = VECTEUR DEMI - MATRICE STOCKE COLONNE , LONGUEUR N
C       OUT     MP = MATRICE PLEINE (M*M)
C       ----------------------------------------------------------------
        REAL*8  MV(N)  , MP(M,M)
C
        K = 0
        DO 10 I = 1 , M
          DO 20 J = 1 , M
          K = K + 1
          MP(I,J) = MV(K)
          MP(J,I) = MV(K)
          IF(J.EQ.I)GOTO 10
 20       CONTINUE
 10     CONTINUE
C
 9999   CONTINUE
        END
