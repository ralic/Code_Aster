        SUBROUTINE INSJPL ( MOD,IMAT,NMAT,MATER,TEMP,SIG,VIN,DSDE )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ================================================================
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
C       NADAI_BETON   :  MATRICE SYMETRIQUE DE COMPORTEMENT TANGENT
C                        ELASTO_PLASTIQUE EN VITESSE A T OU T+DT
C       ----------------------------------------------------------------
C       IN  MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           TEMP   :  TEMPERATURE
C           MATER  :  COEFFICIENTS MATERIAU
C           SIG    :  CONTRAINTES
C           VIN    :  VARIABLES INTERNES
C       OUT DSDE   :  MATRICE DE COMPORTEMENT TANGENT = DSIG/DEPS
C       --------------------------------------------------------------
        INTEGER         IMAT, NDT , NDI , NMAT
        REAL*8          UN
        PARAMETER       ( UN     =  1.D0   )
C
        REAL*8          VIN(*)
        REAL*8          SIG(6) ,     DEV(6)
        REAL*8          HOOK(6,6),   DSDE(6,6), DFDS(6)
        REAL*8          VTMP(6),     VTMP1(6)
C
        REAL*8          MATER(NMAT,2) , TEMP
        REAL*8          KAPA  , DFDP , HP, TAU , R0 , KRUP
        REAL*8          LCS, KPIC, H3, D
C
        CHARACTER*8     MOD
C       ---------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ---------------------------------------------------------------
C
        KAPA = VIN(1)
C
        LCS     = MATER(1,2)
        R0      = MATER(3,2)
        KPIC    = MATER(4,2)
        KRUP    = MATER(5,2)
C
        CALL LCOPLI ( 'ISOTROPE' , MOD , MATER(1,1) , HOOK )
C
        CALL INSISO (LCS, KPIC, R0, KRUP, KAPA, HP, TAU)
        CALL INSFS (SIG , DFDS  , DEV )
        CALL LCPRMV ( HOOK   , DFDS  , VTMP1 )
        CALL LCPRSC ( DFDS   , VTMP1 , H3    )
        DFDP = - HP
        D = H3 - DFDP
        CALL LCPRMV ( HOOK   , DFDS  , VTMP  )
        CALL LCPRTE ( VTMP   , VTMP  , DSDE  )
        CALL LCPRSM ( - UN/D , DSDE  , DSDE  )
        CALL LCSOMA ( HOOK   , DSDE  , DSDE  )
C
        END
