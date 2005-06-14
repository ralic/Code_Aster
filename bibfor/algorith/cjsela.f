        SUBROUTINE CJSELA ( MOD, CRIT, MATERF, DEPS, SIGD, SIGF,
     &                      NVI, VIND, VINF)
        IMPLICIT NONE
C       ===============================================================
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 08/03/2004   AUTEUR REZETTE C.REZETTE 
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
C       ---------------------------------------------------------------
C       INTEGRATION ELASTIQUE NON LINEAIRE DE LA LOI CJS
C       IN  MOD    :  MODELISATION
C           CRIT   : CRITERES DE CONVERGENCE
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           SIGD   :  CONTRAINTE  A T
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT SIGF   :  CONTRAINTE A T+DT
C       ---------------------------------------------------------------
        INTEGER       NDT, NDI, NVI
        REAL*8        COEF, E, NU, AL, LA, MU, HOOK(6,6), I1
        REAL*8        DEPS(6), DSIG(6), SIGD(6), SIGF(6)
        REAL*8        VIND(*), VINF(*)
        REAL*8        MATERF(14,2), CRIT(*)
        CHARACTER*8   MOD
        REAL*8        ZERO, UN, D12, DEUX, TROIS,PA,QINIT
        LOGICAL       TRACT
        INTEGER       I,J

        COMMON /TDIM/   NDT  , NDI

        DATA          ZERO  / 0.D0 /
        DATA          D12   / .5D0 /
        DATA          UN    / 1.D0 /
        DATA          DEUX  / 2.D0 /
        DATA          TROIS / 3.D0 /

C       ---------------------------------------------------------------
        PA = MATERF(12,2)
        QINIT = MATERF(13,2)

C--->   CALCUL DE I1=TR(SIG) A T+DT PAR METHODE DE LA SECANTE
C       OU EXPLICITEMENT SI NIVEAU CJS1

        CALL CJSCI1 ( CRIT, MATERF, DEPS, SIGD, I1, TRACT )

C
C--->   EN CAS D'ENTREE EN TRACTION, LES CONTRAINTES SONT
C       RAMENEES SUR L'AXE HYDROSTATIQUE A DES VALEURS FAIBLES
C       ( EGALES A PA/100.0 SOIT -1 KPA )
C
        IF( TRACT ) THEN
           DO 10 I=1, NDI
           SIGF(I) = -QINIT/3.D0+PA/100.0D0
 10        CONTINUE
           DO 20 I=NDI+1, NDT
           SIGF(I) = ZERO
 20        CONTINUE
           GOTO 9999
        ENDIF


C                         I1+QINIT
C--->   CALCUL DU COEF  (-----------)**N ET MODULE_YOUNG A T+DT
C                        3 PA


        COEF = ((I1+QINIT)/TROIS/PA)**MATERF(3,2)
        E = MATERF(1,1)* COEF
        NU = MATERF(2,1)
        AL = E  * (UN-NU) / (UN+NU) / (UN-DEUX*NU)
        LA = NU * E       / (UN+NU) / (UN-DEUX*NU)
        MU = E  * D12     / (UN+NU)

C--->   OPERATEUR DE RIGIDITE

        CALL LCINMA(ZERO,HOOK)

C - 3D/DP/AX
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS'        )THEN
             DO 40 I = 1,NDI
             DO 40 J = 1,NDI
                 IF(I.EQ.J) HOOK(I,J) = AL
                 IF(I.NE.J) HOOK(I,J) = LA
 40          CONTINUE
             DO 45 I = NDI+1 , NDT
             DO 45 J = NDI+1 , NDT
                 IF(I.EQ.J) HOOK(I,J) = DEUX* MU
 45          CONTINUE

C - CP/1D
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' )THEN
             CALL UTMESS('F','CJS','LES MODELISATIONS AUTORISEES'//
     &                       ' SONT 3D ET D_PLAN ET AXIS')
        ENDIF


C--->   INCREMENTATION DES CONTRAINTES  SIGF = SIGD + HOOK DEPS

        CALL LCPRMV ( HOOK, DEPS, DSIG )
        CALL LCSOVE ( SIGD, DSIG, SIGF )


 9999   CONTINUE

C--->   VINF = VIND, ETAT A T+DT = ELASTIQUE = 0

        CALL LCEQVN ( NVI-1, VIND, VINF )
        VINF(NVI) = 0.D0

        END
