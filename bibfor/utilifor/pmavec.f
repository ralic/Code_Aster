      SUBROUTINE PMAVEC(CUMUL,N,A,X,Y)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
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
      IMPLICIT NONE
        CHARACTER*(*)     CUMUL
        INTEGER                 N
        REAL*8  A(N,N), X(N),Y(N)
C       ----------------------------------------------------------------
C       PRODUIT MATRICE CARREE PLEINE PAR UN VECTEUR
C          Y(N) = 0.  + A(N,N)*X(N)
C       OU Y(N) = Y(N)+ A(N,N)*X(N)
C       ----------------------------------------------------------------
C IN    N     : I  :   DIMENSION DE LA MATRICE ET DES VECTEURS X ET Y
C IN    A(N,N): R  :   MATRICE REELLE
C IN    X(N)  : R  :   VECTEUR REEL
C IN    CUMUL : K* :   ON CUMULE OU NON DANS LE VECTEUR RESULTAT Y
C       CUMUL = 'ZERO' ON MET Y A ZERO AVANT DE COMMENCER
C       CUMUL = 'CUMU' ON ACCUMULE DANS Y
C OUT   Y(N)  : R  :   VECTEUR REEL
C       ----------------------------------------------------------------
C-----------------------------------------------------------------------
      INTEGER I ,J 
C-----------------------------------------------------------------------
        IF ( CUMUL .EQ. 'ZERO' ) THEN
           DO 1 I = 1 , N
              Y(I) = 0.D0
 1         CONTINUE
        ENDIF
        DO 3 J = 1 , N
           DO 2 I = 1 , N
              Y(I) = Y(I) + A(I,J) * X(J)
 2         CONTINUE
 3      CONTINUE
        END
