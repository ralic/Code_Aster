        SUBROUTINE LCPLMA ( LOI,   MOD,   IMAT, NMAT, MATERF, TEMPD,
     1                      TEMPF,TIMED,TIMEF,DEPS,EPSD,YD,YF,NMOD,DRDY)
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
C       INTEGRATION ELASTO-PLASTIQUE SUR DT DE Y = ( SIG , VIN )
C
C       CALCUL DES TERMES DU SYSTEME A T+DT = R
C       ----------------------------------------------------------------
C       IN  LOI    :  MODELE DE COMPORTEMENT
C           NMOD   :  DIMENSION  DRDY
C           MOD    :  TYPE DE MODELISATION
C           IMAT   :  ADRESSE DU MATERIAU CODE
C           NMAT   :  DIMENSION MATER
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           TEMPD  :  TEMPERATURE A T
C           TEMPF  :  TEMPERATURE A T+DT
C           TIMED  :  INSTANT  T
C           TIMEF  :  INSTANT T+DT
C           DEPS   :  INCREMENT DE DEFORMATION
C           EPSD   :  DEFORMATION A T
C           YD     :  VARIABLES A T   = ( SIGD  VIND  (EPSD3)   )
C           YF     :  VARIABLES A T+DT= ( SIGF  VINF  (EPSF3)   )
C       OUT DRDY   :  TERMES OPERATEUR LINEAIRE A T+DT
C       ----------------------------------------------------------------
        INTEGER         IMAT, NMAT,    NMOD
C
        INTEGER         NDT,    NDI
C
        REAL*8          TEMPD,          TEMPF, TIMED, TIMEF
        REAL*8          EPSD(6),   DEPS(6)
        REAL*8          DRDY(NMOD,NMOD)
        REAL*8          YD(NMOD) , YF(NMOD)
        REAL*8          MATERF(NMAT,2)
C
        CHARACTER*8     MOD
        CHARACTER*16    LOI
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
 9999   CONTINUE
        END
