        SUBROUTINE LMAFS ( IMAT , NMAT , MATER , SIG , X , DFDS )
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
C       DERIVEE / SIG DE   LA FONCTION SEUIL A (SIG , X ) DONNES
C
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU
C           SIG    :  TENSEUR CONTRAINTE
C           X      :  TENSEUR CINEMATIQUE 1
C       OUT DFDS   :  NORMALE DFDS = 3 M(D-X) / 2 S
C
C                                             T       1/2
C                             S    = (3/2(D-X) M (D-X))
C                             D    = SIG - 1/3 TR(SIG) I
C                       ET    M    = MATRICE D'ANISOTROPIE = MATER(16,1)
C       ----------------------------------------------------------------
        INTEGER         N , ND
        INTEGER         IMAT, NMAT
        REAL*8          MATER(NMAT,2) , VTMP(6)
        REAL*8          DFDS(6) , SIG(6) , X(6),  DEV(6) , S
        REAL*8          LCNRTS
C       ----------------------------------------------------------------
        COMMON /TDIM/   N , ND
C       ----------------------------------------------------------------
C
        CALL LCDEVI ( SIG        , DEV         )
        CALL LCDIVE ( DEV        , X    , DEV  )
        CALL LCPRMV ( MATER(16,1), DEV  , VTMP )
        CALL LCPRSC ( DEV        , VTMP , S    )
        S =  SQRT   ( 1.5D0 * S                  )
        IF ( S .EQ. 0.D0 ) S = 1.D0
        CALL LCPRSV ( 1.5D0 / S    , VTMP , DFDS )
C
C
        END
