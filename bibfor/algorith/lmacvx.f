        SUBROUTINE LMACVX ( IMAT, NMAT, MATER, TEMP, SIG ,VIN, SEUIL )
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
C       MODELE VISCOPLASTIQUE DE BESANCON EN VITESSE
C       CALCUL DE LA FONCTION SEUIL POUR (MATER,SIG,X,X1,X2,V)
C
C                     SEUIL   F    = S   -  R0
C                                             T        1/2
C                     SEUIL   S    = (3/2(D-X) M (D-X))
C                       ET    D    = SIG - 1/3 TR(SIG) I
C                             M    = MATRICE D'ANISOTROPIE
C       ----------------------------------------------------------------
C       IN  SIG    :  CONTRAINTE
C       IN  VIN    :  VARIABLES INTERNES = ( X X1 X2 V )
C       IN  IMAT   :  ADRESSE DU MATERIAU CODE
C       IN  NMAT   :  DIMENSION MATER
C       IN  TEMP   :  TEMPERATURE
C       IN  MATER  :  COEFFICIENTS MATERIAU A TEMP
C       OUT SEUIL  :  SEUIL  ELASTICITE
C       ----------------------------------------------------------------
        INTEGER         IMAT,  NDT   , NDI  , NMAT
        REAL*8          SIG(6)       , X(6) , DEV(6) , VIN(*) , VTMP(6)
        REAL*8          MATER(NMAT,2) , TEMP , SEUIL
        REAL*8          LCNRTS
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT , NDI
C       ----------------------------------------------------------------
C
        R0   = MATER(2,2)
C
        CALL LCEQVN ( NDT          , VIN(1), X    )
C
        CALL LCDEVI ( SIG          , DEV          )
        CALL LCDIVE ( DEV          , X     , DEV  )
        CALL LCPRMV ( MATER(16,1) , DEV   , VTMP )
        CALL LCPRSC ( DEV          , VTMP  , SEUIL)
        SEUIL  = SQRT( 1.5D0 * SEUIL ) - R0
C
C
        END
