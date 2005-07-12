        SUBROUTINE LCPLLI ( MOD, NMAT, NR, NVI,EPSD,
     2                      SIGD, VIND, SIGF, VINF)
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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
C       LE SYSTEME A RESOUDRE EN DY ETANT  LINEAIRE
C
C       ON RESOUD DONC          DRDY DY - R = 0
C       ET ON REACTUALISE       YF = YD + DY
C       ----------------------------------------------------------------
C       IN  MOD    :  TYPE DE MODELISATION
C           NMAT   :  DIMENSION MATER
C           EPSD   :  DEFORMATION A T
C           SIGD   :  CONTRAINTE A T
C           VIND   :  VARIABLES INTERNES A T
C           NR     :  NB EQUATION DU SYSTEME R(DY)
C           NVI    :  NB VARIABLES INTERNES
C       OUT SIGF   :  CONTRAINTE A T+DT
C           VINF   :  VARIABLES INTERNES A T+DT
C       ----------------------------------------------------------------
C           NR   :  DIMENSION R , DRDY
C           R      :  VECTEUR SECOND MEMBRE
C           DRDY   :  OPERATEUR LINEAIRE
C           DY     :  INCREMENT DES VARIABLES = ( DSIG  DVIN  (DEPS3)  )
C           YD     :  VARIABLES A T   = ( SIGD  VIND  (EPSD3)   )
C           YF     :  VARIABLES A T+DT= ( SIGF  VINF  (EPSF3)   )
C       ----------------------------------------------------------------
        INTEGER         NMAT,    NR, IRET
        REAL*8          ZERO   , UN
C
        PARAMETER       ( ZERO = 0.D0   )
        PARAMETER       ( UN   = 1.D0   )
C
        INTEGER         NDT,    NDI,    NVI
C
        REAL*8          HOOK(6,6),      EPSD(6)
        REAL*8          SIGD(6),        SIGF(6)
        REAL*8          VIND(*),        VINF(*)
        REAL*8          R(NR),        DRDY(NR,NR)
        REAL*8          DY(NR),       YD(NR) , YF(NR), DET
C
        CHARACTER*8     MOD
C       ----------------------------------------------------------------
        COMMON /TDIM/   NDT  , NDI
C       ----------------------------------------------------------------
        INTEGER I,J
C
        DO 200  I = 1 , NR
        R( I ) = 0.D0
        DO 210  J = 1 , NR
        DRDY( I , J ) = 0.D0
  210           CONTINUE
        DY( I ) = 0.D0
        YD( I ) = 0.D0
        YF( I ) = 0.D0
  200           CONTINUE

C       ----------------------------------------------------------------
C
C --    INITIALISATION YD = ( SIGD , VIND , (EPSD(3)) )
C
        CALL LCEQVN ( NDT  ,  SIGD , YD )
        CALL LCEQVN ( NVI-1,  VIND , YD(NDT+1) )
        IF(MOD.EQ.'C_PLAN') YD (NDT+NVI) = EPSD(3)
C
C         RESOLUTION DU SYSTEME LINEAIRE DRDY DY  = R
C
C --      CALCUL DES TERMES DU SYSTEME A T+DT = R
C
          CALL LCPL2M (NMAT,NR)
C
C --      CALCUL DE L OPERATEUR LINEAIRE DU SYSTEME  = DRDY
C
          CALL LCPLMA (NMAT,NR)
C
C --      RESOLUTION DU SYSTEME LINEAIRE DRDY.DY = R
C
                IF(MOD.EQ.'C_PLAN')THEN
                R(3) = ZERO
                DO 110 I  = 1 , NR
                DRDY(I,3) = ZERO
                DRDY(3,I) = ZERO
  110           CONTINUE
                DRDY(3,3) = UN
                DY(3)    = ZERO
                ENDIF
C
          CALL LCEQVN ( NR ,   R ,  DY )
          CALL MGAUSS ('NFVP',DRDY , DY , NR , NR , 1, DET, IRET)
C
C --      INCREMENTATION DE YF = YD + DY
C
          CALL LCSOVN ( NR , YD , DY , YF )
C
C --      MISE A JOUR DE SIGF , VINF
C
          CALL LCEQVN ( NDT ,   YF(1)     , SIGF )
          CALL LCEQVN ( NVI-1 , YF(NDT+1) , VINF )
          VINF (NVI) = 1.D0
C
        END
