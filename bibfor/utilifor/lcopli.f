        SUBROUTINE LCOPLI ( TYP, MOD, MATER, HOOK)
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 27/03/2002   AUTEUR VABHHTS J.PELLET 
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
C       OPERATEUR DE RIGIDITE POUR COMPORTEMENT ELASTIQUE LINEAIRE
C       IN  TYP    :  TYPE OPERATEUR
C                     'ISOTROPE'
C                     'ORTHOTRO'
C                     'ANISOTRO'
C           MOD    :  MODELISATION
C           MATER  :  COEFFICIENTS MATERIAU ELASTIQUE
C       OUT HOOK   :  OPERATEUR RIGIDITE ELASTIQUE LINEAIRE
C       ----------------------------------------------------------------
C
        INTEGER         NDT , NDI
        REAL*8          UN  , D12 , ZERO , DEUX
        PARAMETER       ( D12  = .5D0   )
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
        PARAMETER       ( DEUX = 2.D0   )
C
        REAL*8          HOOK(6,6)
        REAL*8          MATER(*) , E ,   NU , AL , LA , MU
C
        CHARACTER*8     MOD , TYP
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
C
        CALL LCINMA ( ZERO , HOOK )
C
        IF ( TYP .EQ. 'ISOTROPE' ) THEN
        E  = MATER(1)
        NU = MATER(2)
        AL = E  * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E       / (UN+NU) / (UN-DEUX*NU)
        MU = E  * D12     / (UN+NU)
C
C - 3D/DP/AX/CP
C
            IF ( MOD(1:2) .EQ. '3D'     .OR.
     1           MOD(1:6) .EQ. 'D_PLAN' .OR.
     1           MOD(1:6) .EQ. 'C_PLAN' .OR.
     2           MOD(1:4) .EQ. 'AXIS'        )THEN
                DO 40 I = 1,NDI
                DO 40 J = 1,NDI
                        IF(I.EQ.J) HOOK(I,J) = AL
                        IF(I.NE.J) HOOK(I,J) = LA
 40             CONTINUE
                DO 45 I = NDI+1 , NDT
                DO 45 J = NDI+1 , NDT
                        IF(I.EQ.J) HOOK(I,J) = DEUX* MU
 45             CONTINUE
C
C - 1D
C
            ELSE IF ( MOD(1:2) .EQ. '1D' )THEN
                HOOK(1,1) = E
            ENDIF
        ENDIF
        END
