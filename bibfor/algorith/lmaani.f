        SUBROUTINE LMAANI ( DIAG , M )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 07/07/95   AUTEUR H2BAXPG P.GEYER 
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C                  CONSTRUCTION DES MATRICES D'ANISOTROPIE A PARTIR DES
C                  COEFFICIENTS DIAGONAUX M11, M22, M33, M66
C                  M12 = 0.5( - M11 - M22 + M33 )  = M21
C                  M13 = 0.5( - M11 + M22 - M33 )  = M31
C                  M23 = 0.5(   M11 - M22 - M33 )  = M32
C                  M44 = M55 = 1.
C                  LES AUTRES TERMES SONT NULS
C       ----------------------------------------------------------------
C       IN  DIAG   :  M11, M22, M33, M66
C       OUT M      :  MATRICE D'ANISOTROPIE
C       ----------------------------------------------------------------
        REAL*8          M(6,6)  
        REAL*8          DIAG(4)
C
C       ----------------------------------------------------------------
C
        CALL LCINMA ( 0.D0 , M )
        M(1,1) = DIAG(1)
        M(2,2) = DIAG(2)
        M(3,3) = DIAG(3)
        M(4,4) = 1.D0
        M(5,5) = 1.D0
        M(6,6) = DIAG(4)
        M(1,2) = .5D0 * ( - DIAG(1) - DIAG(2) + DIAG(3) )
        M(2,1) = M(1,2)
        M(1,3) = .5D0 * ( - DIAG(1) + DIAG(2) - DIAG(3) )
        M(3,1) = M(1,3)
        M(3,2) = .5D0 * (   DIAG(1) - DIAG(2) - DIAG(3) )
        M(2,3) = M(3,2)
C        
        END
