        SUBROUTINE LCVVSX ( DSX , DDVDSX )
        IMPLICIT REAL*8 (A-H,O-Z)
C       ----------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 05/01/95   AUTEUR G8BHHAC A.Y.PORTABILITE 
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
C       3D / 1D / DP / CP
C       DERIVEE SECONDE / SIG / X DE LA NORME DE VON MISES DE (SIG-X) 
C       IN  DSX    :   CONTRAINTE - CINEMATIQUE
C                                                       T
C       OUT  DDVDSX:  DDVDSX = -1/S ( 3/2 I4 - DVDS DVDS )
C                     DVDS   = 3/2  DEV/S
C                                       T    1/2
C                     S      = (3 /2 DEV DEV )
C                     DEV    = DSX - 1/3 TR(DSX) I
C                     I4     = TENSEUR UNITE ORDRE 4
C       ----------------------------------------------------------------
        REAL*8          UN    , ZERO , D32
        PARAMETER       ( UN   = 1.D0   )
        PARAMETER       ( ZERO = 0.D0   )
        PARAMETER       ( D32  = 1.5D0  )
        REAL*8          DVDS(6)     , DSX(6)  , DEV(6) , S
        REAL*8          DDVDSX(6,6) , DVDS2(6,6)
        REAL*8          I4(6,6)
        REAL*8          LCNRTS
C
        DATA  I4        /UN     , ZERO  , ZERO  , ZERO  ,ZERO  ,ZERO,
     1                   ZERO   , UN    , ZERO  , ZERO  ,ZERO  ,ZERO,
     2                   ZERO   , ZERO  , UN    , ZERO  ,ZERO  ,ZERO,
     3                   ZERO   , ZERO  , ZERO  , UN    ,ZERO  ,ZERO,
     4                   ZERO   , ZERO  , ZERO  , ZERO  ,UN    ,ZERO,
     5                   ZERO   , ZERO  , ZERO  , ZERO  ,ZERO  ,UN/
C       ----------------------------------------------------------------
C
        CALL LCDEVI ( DSX , DEV )
        S =  LCNRTS ( DEV )
                IF ( S .EQ. ZERO ) THEN
                CALL LCINMA ( ZERO , DDVDSX )
                ELSE
                CALL LCVS   ( DSX , DVDS )
                CALL LCPRTE ( DVDS    , DVDS  , DVDS2 )
                CALL LCPRSM ( D32     , I4    , DDVDSX )
                CALL LCDIMA ( DDVDSX  , DVDS2 , DDVDSX )
                CALL LCPRSM ( -UN / S , DDVDSX, DDVDSX )
                ENDIF
C
        END
