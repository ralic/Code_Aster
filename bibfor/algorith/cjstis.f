       SUBROUTINE CJSTIS ( MOD, MATER, SIG, VIN, DSDE )
        IMPLICIT NONE
C       ================================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C     CALCUL DE LA MATRICE TANGENTE DU PROBLEME CONTINU DE LA LOI CJS
C     POUR LE MECANISME PLASTIQUE ISOTROPE
C     IN   MOD     :  MODELISATION
C          MATER   :  COEFFICIENTS MATERIAU
C          SIG     :  CONTRAINTES
C          VIN     :  VARIABLES INTERNES
C     OUT  DSDE    :  MATRICE TANGENTE
C       ----------------------------------------------------------------

        INTEGER       NDT, NDI

        REAL*8      SIG(6), DSDE(6,6), MATER(14,2), VIN(*), I1
        REAL*8      E, NU, AL, LA, MU, KE, KP, COEF1, COEF2
        REAL*8      ZERO, D12, UN, DEUX, TROIS,QINIT,PA
        INTEGER     I,J

        CHARACTER*8 MOD

        COMMON /TDIM/   NDT, NDI

        DATA          ZERO  / 0.D0 /
        DATA          D12   / .5D0 /
        DATA          UN    / 1.D0 /
        DATA          DEUX  / 2.D0 /
        DATA          TROIS / 3.D0 /

C       ----------------------------------------------------------------

C--->   CALCUL PREMIER INVARIANT DES CONTRAINTES
        QINIT = MATER(13,2)
        PA =    MATER(12,2)
        I1 = ZERO
        DO 10 I = 1,NDI
        I1 = I1 + SIG(I)
  10    CONTINUE

         IF((I1+QINIT)  .EQ. 0.D0 ) THEN
          I1  = -QINIT+1.D-12 * PA
         ENDIF

C--->   CALCUL DES COEF. UTILES
        COEF1 = ((I1+QINIT)/TROIS/MATER(12,2))**MATER(3,2)
        E = MATER(1,1) * COEF1
        NU = MATER(2,1)
        KE   = E/TROIS/( UN-DEUX*NU )
        KP   = MATER(4,2)*(VIN(1)/MATER(12,2))**MATER(3,2)
        COEF2 = KE * KE  / (KE+KP)
        AL = E * (UN-NU) / (UN+NU) / (UN-DEUX*NU) - COEF2
        LA = NU * E  / (UN+NU) / (UN-DEUX*NU)  - COEF2
        MU = E  * D12     / (UN+NU)


C - EN FAIT ON NE TIENT PAS COMPTE DE COEF2, SINON NON CONVERGENCE

        AL = E * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E  / (UN+NU) / (UN-DEUX*NU)



C--->   OPERATEUR DE RIGIDITE

C - 3D/DP/AX
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS'        )THEN
             DO 20 I = 1,NDI
             DO 20 J = 1,NDI
                 IF(I.EQ.J) DSDE(I,J) = AL
                 IF(I.NE.J) DSDE(I,J) = LA
 20          CONTINUE
             DO 30 I = NDI+1 , NDT
             DO 30 J = NDI+1 , NDT
                 IF(I.EQ.J) DSDE(I,J) = DEUX* MU
 30          CONTINUE

C - CP/1D
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' )THEN
             CALL U2MESS('F','ALGORITH2_15')
        ENDIF

        END
