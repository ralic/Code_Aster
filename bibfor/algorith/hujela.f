        SUBROUTINE HUJELA (MOD, CRIT, MATERF, DEPS, SIGD, SIGF, IRET)
        IMPLICIT NONE
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 22/05/2007   AUTEUR KHAM M.KHAM 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
C   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.         
C ======================================================================
C       ---------------------------------------------------------------
C       INTEGRATION ELASTIQUE NON LINEAIRE DE LA LOI DE HUJEUX
C       IN  MOD    :  MODELISATION
C           CRIT   :  CRITERES DE CONVERGENCE
C           MATERF :  COEFFICIENTS MATERIAU A T+DT
C           SIGD   :  CONTRAINTE  A T
C           DEPS   :  INCREMENT DE DEFORMATION
C       OUT SIGF   :  CONTRAINTE A T+DT
C           IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
C                         IRET=0 => PAS DE PROBLEME
C                         IRET=1 => ECHEC 
C       ---------------------------------------------------------------
        INTEGER       NDT, NDI, IRET, I, J
        REAL*8        COEF, E, NU, AL, DEMU, I1, N, PREF
        REAL*8        DEPS(6), DSIG(6), SIGD(6), SIGF(6)
        REAL*8        HOOK(6,6), MATERF(20,2), CRIT(*)
        REAL*8        ZERO, UN, DEUX
        CHARACTER*8   MOD
        LOGICAL       TRACT

        COMMON /TDIM/ NDT, NDI

        DATA          ZERO  / 0.D0 /
        DATA          UN    / 1.D0 /
        DATA          DEUX  / 2.D0 /

C       ---------------------------------------------------------------
        PREF = MATERF(8,2)
        N = MATERF(1,2)


C--->  CALCUL DE I1=TR(SIG) A T+DT PAR METHODE DE LA SECANTE
C      OU EXPLICITEMENT SI NIVEAU HUJEUX
        CALL HUJCI1 (CRIT, MATERF, DEPS, SIGD, I1, TRACT, IRET)
        IF (IRET.EQ.1) GOTO 9999
        

C--->  EN CAS D'ENTREE EN TRACTION, LES CONTRAINTES SONT
C      RAMENEES SUR L'AXE HYDROSTATIQUE A DES VALEURS FAIBLES
C      ( EGALES A PA/100.0 SOIT -1 KPA )
C

CKH A REVOIR....
        IF (TRACT) THEN
           DO 10 I = 1, NDI
             SIGF(I) = PREF/100.D0
 10          CONTINUE
           DO 20 I = NDI+1, NDT
             SIGF(I) = ZERO
 20          CONTINUE
             GOTO 9999
        ENDIF


C---> CALCUL DU COEF  (-----------)**N ET MODULE_YOUNG A T+DT
        COEF = (I1/PREF)**N
        E    = MATERF(1,1)*COEF
        NU   = MATERF(2,1)
        AL   = E*(UN-NU) /(UN+NU) /(UN-DEUX*NU)
        DEMU = E         / (UN+NU)


C--->   OPERATEUR DE RIGIDITE
        CALL LCINMA (ZERO, HOOK)

C - 3D/DP/AX
        IF ( MOD(1:2) .EQ. '3D'     .OR.
     &       MOD(1:6) .EQ. 'D_PLAN' .OR.
     &       MOD(1:4) .EQ. 'AXIS' ) THEN
           DO 40 I = 1, NDI
             DO 40 J = 1, NDI
               IF(I.EQ.J) HOOK(I,J) = AL
               IF(I.NE.J) HOOK(I,J) = DEMU
 40          CONTINUE
           DO 45 I = NDI+1, NDT
             HOOK(I,I) = DEMU
 45          CONTINUE

C - CP/1D
        ELSE IF ( MOD(1:6) .EQ. 'C_PLAN' .OR.
     &            MOD(1:2) .EQ. '1D' ) THEN
           CALL U2MESS('F','COMPOR1_4')
        ENDIF


C--->   INCREMENTATION DES CONTRAINTES  SIGF = SIGD + HOOK DEPS
        CALL LCPRMV (HOOK, DEPS, DSIG)
        CALL LCSOVE (SIGD, DSIG, SIGF)

 9999   CONTINUE
        END
