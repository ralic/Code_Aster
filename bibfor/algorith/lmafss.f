        SUBROUTINE LMAFSS ( IMAT, NMAT, MATER, SIG, X, ID, DDFDDS )
        IMPLICIT REAL*8 (A-H,O-Z)
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C       3D / 1D / DP / CP
C       DERIVEE / S / S DE LA FONCTION SEUIL A (SIG , X) DONNES
C
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C       IN  NMAT   :  DIMENSION MATER
C       IN  MATER  :  COEFFICIENTS MATERIAU
C       IN  SIG    :  TENSEUR CONTRAINTE
C       IN  X      :  TENSEUR CINEMATIQUE 1
C                                                   T
C       OUT DDFDDS :  DDFDDS = 1/S ( 3/2 M ID - DFDS DFDS )
C                     DFDS   = 3 M (D-X) / 2 S
C                                            T
C                     ID     = I4 - 1/3 I2 I2
C                                         T      1/2
C                     S      = (3/2(D-X) M (D-X))
C                     D      = SIG - 1/3 TR(SIG) I
C                     M    = MATRICE D'ANISOTROPIE = MATER(16,1)
C       ----------------------------------------------------------------
        INTEGER         N , ND
        INTEGER         NMAT , IMAT
        REAL*8          DFDS(6) ,  SIG(6) , X(6) , DEV(6) , S
        REAL*8          DDFDDS(6,6) , DFDS2(6,6)
        REAL*8          ID(6,6)
        REAL*8          LCNRTS
        REAL*8          MATER(NMAT,2)
        REAL*8          TEM
C       ----------------------------------------------------------------
        COMMON /TDIM/   N , ND
C       ----------------------------------------------------------------
C
        CALL LMAFS  ( IMAT  , NMAT , MATER , SIG , X , DFDS )
        CALL LMACVX ( IMAT  , NMAT , MATER , TEM , SIG , X , S )
C
        CALL LCPRSM ( 1.5D0   , ID         , DFDS2  )
        CALL LCPRMM ( DFDS2 , MATER(16,1), DDFDDS )
        CALL LCPRTE ( DFDS  , DFDS       , DFDS2  )
        CALL LCDIMA ( DDFDDS, DFDS2      , DDFDDS )
        IF ( S .EQ. 0.D0 ) S = 1.D0
        CALL LCPRSM ( 1.D0/ S , DDFDDS     , DDFDDS )
C
C
        END
