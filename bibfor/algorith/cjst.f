              SUBROUTINE CJST ( S, T )
        IMPLICIT NONE
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C      CALCUL DE T = DET(S)*INV(S)
C       3D   : A = A11, A22, A33, RAC2 A12, RAC2 A13, RAC2 A23
C       D_PLAN OU AXIS A = A11, A22, A33, RAC2 A12
C       IN  S      :  MATRICE
C       OUT T      :  T (SOUS FORME VECTORIELLE AVEC RAC2)
C       ----------------------------------------------------------------
        INTEGER         N , ND
        REAL*8          S(6), T(6),  INVRC2
        COMMON /TDIM/   N , ND
C
        INVRC2 = 1.D0 / SQRT(2.D0)
        IF(N .EQ. 6) THEN
        T(1) = ( S(2)*S(3)-0.5D0*S(6)*S(6) )
        T(2) = ( S(1)*S(3)-0.5D0*S(5)*S(5) )
        T(3) = ( S(1)*S(2)-0.5D0*S(4)*S(4) )
        T(4) = ( INVRC2*S(5)*S(6)-S(4)*S(3) )
        T(5) = ( INVRC2*S(4)*S(6)-S(5)*S(2) )
        T(6) = ( INVRC2*S(4)*S(5)-S(6)*S(1) )
        ENDIF

        IF(N .EQ. 4) THEN
        T(1) = S(2)*S(3)
        T(2) = S(1)*S(3)
        T(3) = ( S(1)*S(2)-0.5D0*S(4)*S(4) )
        T(4) = -S(4)*S(3)
        ENDIF
        END
