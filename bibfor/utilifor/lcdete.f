        SUBROUTINE LCDETE ( A, DETA )
      IMPLICIT NONE
C       ----------------------------------------------------------------
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
C       ----------------------------------------------------------------
C       DETERMINANT D UNE MATRICE EN FONCTION DE LA MODELISATION
C       3D   : A = A11, A22, A33, RAC2 A12, RAC2 A13, RAC2 A23
C       D_PLAN OU AXIS A = A11, A22, A33, RAC2 A12
C       IN  A      :  MATRICE
C       OUT LCDETE :  DETERMINANT
C       ----------------------------------------------------------------
        INTEGER         N , ND
        REAL*8          A(6), DETA, INVRC2
        COMMON /TDIM/   N , ND
C
C-----------------------------------------------------------------------
C-----------------------------------------------------------------------
        INVRC2 = 1.D0 / SQRT(2.D0)

        IF(N .EQ. 6) THEN
        DETA = A(1)*A(2)*A(3) + INVRC2*A(4)*A(5)*A(6) -
     &         ( A(3)*A(4)*A(4)+A(2)*A(5)*A(5)+A(1)*A(6)*A(6) )/2.D0
        ENDIF

        IF(N .EQ. 4) THEN
        DETA = A(1)*A(2)*A(3) - A(3)*A(4)*A(4)/2.D0
        ENDIF

        END
