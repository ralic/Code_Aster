        SUBROUTINE RSLPLI ( TYP, MOD, MATER, HOOK, NMAT, VIN)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF UTILIFOR  DATE 28/01/98   AUTEUR JMBHH01 J.M.PROIX 
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
        INTEGER         NMAT
C
        REAL*8          UN  , RHO, F, F0
        REAL*8          HOOK(6,6)
        REAL*8          MATER(NMAT,2), VIN(*)
C
        PARAMETER       ( UN   = 1.D0   )
C
        CHARACTER*8     MOD , TYP
C       ----------------------------------------------------------------
C
C --    CALCUL DE RHO
C
        F   = VIN(2)
        F0  = MATER(3,2)
        RHO = (UN-F)/(UN-F0)
C
        CALL LCOPLI ( TYP , MOD , MATER(1,1) , HOOK )
        CALL LCPRSM ( RHO , HOOK, HOOK )
        END
