        SUBROUTINE HUJPRJ (K, TIN, TOUD, P, Q)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/11/2007   AUTEUR KHAM M.KHAM 
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
C  ------------------------------------------------------
C  LOI DE HUJEUX: PROJECTION DANS LE PLAN DEVIATEUR K
C  IN  K        :  DIRECTION K=1 A 3
C      TIN( )   :  TENSEUR A PROJETER (NDIM), EGAL A:
C                     - TENSEUR DES CONTRAINTES DE CAUCHY
C                     - TENSEUR DES DEFORMATIONS
C
C  OUT 
C      TOUD  :  DEVIATEUR K (NDIM/2)
C      P     :  COMPOSANTE ISOTROPE K
C      Q     :  NORME DEVIATEUR K
C  ------------------------------------------------------
        INTEGER   NDT, NDI, I, J, K
        REAL*8    D12, DD, DEUX
        REAL*8    TIN(6), TOU(3), TOUD(3), P, Q

        COMMON /TDIM/ NDT  , NDI

        DATA   D12, DEUX /0.5D0, 2.D0/

        J = 1
        DO 10 I = 1, NDI
          IF (I .NE. K) THEN
            TOU(J) = TIN(I)
            J = J+1
          ENDIF
  10     CONTINUE

        TOU(3) = TIN(NDT+1-K)

        DD      = D12*( TOU(1)-TOU(2) )
        TOUD(1) = DD
        TOUD(2) = -DD
        TOUD(3) = TOU(3)

        P = D12*( TOU(1)+TOU(2) )
        Q = DD**DEUX + ((TOU(3))**DEUX)/DEUX
        Q = SQRT(Q)
        
        END
