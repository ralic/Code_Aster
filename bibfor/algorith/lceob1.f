       SUBROUTINE LCEOB1 (INTMAX,TOLE,EPS,BM,DM,
     &                     LAMBDA,MU,ALPHA,ECROB,ECROD,
     &                     SEUIL,BDIM,B,D,MULT,ELAS,DBLOQ,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 17/01/2011   AUTEUR IDOUX L.IDOUX 
C ======================================================================
C COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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

      IMPLICIT NONE
      REAL*8             EPS(6)
      REAL*8             BM(6),DM,B(6),D,MULT
      REAL*8             LAMBDA,MU,ALPHA,SEUIL,ECROB,ECROD
      REAL*8             TOLE

      INTEGER            INTMAX, IRET, BDIM

      LOGICAL            ELAS,DBLOQ
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
C     ROUTINE DE RESOLUTION DU SYSTEME NON LINEAIRE
C     ALGORITHME DE NEWTON
C
C
C
C  IN INTMAX  : NBRE D'ITERATION MAX POUR LE NEWTON LOCAL
C  IN TOLE    : RESIDU TOLERE POUR LE NEWTON LOCAL
C  IN  BDIM   : DIMENSION DE L'ESPACE
C  IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
C  IN  EPSM   : DEFORMATION EN T- REPERE GLOBAL
C  IN  DEPS   : INCREMENT DE DEFORMATION
C  IN  BM DM  : VARIABLES INTERNES EN T-
C  IN  LAMBDA : /
C  IN  MU     : / COEFFICIENTS DE LAME
C  IN  ALPHA  : /
C  IN  ECROB  : /
C  IN  ECROD  : / PARAMETRES DU MODELE
C  IN  SEUIL  : SEUIL DU CRITERE D'ENDOMMAGEMENT
C  IN  BDIM   : DIMENSION DE L ESPACE
C
C OUT  B D    : VARIABLES INTERNES EN T+
C OUT MULT    : MULTIPLICATEUR PLASTIQUE DU PRINCIPE DE NORMALITE
C OUT ELAS    : ELASTIQUE OU DISSIPATION?
C OUT DBLOQ   : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
C OUT IRET    : CODE RETOUR
C ----------------------------------------------------------------------

      INTEGER     I,COMPTE

      REAL*8      BS,BMS,DEUX,UN,TOLC
      REAL*8      FB(6),DBS,FD,DD,FBM(6)
      REAL*8      RESB,DELTA1,DELTA2,DDG
      REAL*8      NORMRB,RTEMP,CRIT
      REAL*8      MTE1,MTE2(6,6),MTE2S
      REAL*8      FBS,FBSM,KSI,IKSI,COUPL
      REAL*8      RESD,DFDDD,PSI
      REAL*8      INTER1,INTER2,INTER3,INTER4

      TOLC=SEUIL*TOLE

      COMPTE=0
      MULT=0.D0
      UN=1.D0
      DEUX=2.D0
      DO 100 I=1,6
        B(I)=BM(I)
 100  CONTINUE
      D=DM
C-------------------------------------------------------
C-------------------------------------------------------
C----CALCUL DE FB: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION

       CALL CEOBFB(B,EPS,LAMBDA,MU,ECROB,BDIM,FB,RTEMP,FBM)

       FBS=FB(1)
       FBSM=FBM(1)
       BS=B(1)
       BMS=BM(1)

C----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION

      IF (DBLOQ) THEN
        FD=0.D0
      ELSE
        CALL CEOBFD(D,EPS,LAMBDA,MU,ECROD,FD)
      ENDIF

C----CALCUL DU CRITERE-------------------------------------
      COUPL=SQRT(ALPHA*RTEMP+(1-ALPHA)*FD**2)
      CRIT=COUPL-SEUIL

      ELAS=.FALSE.

      IF (CRIT.LE.TOLC) THEN
        ELAS=.TRUE.
        GOTO 999

      ELSE

        RESB=-BS+BMS+ALPHA*MULT*FBSM
        RESD=-D+DM+(1-ALPHA)*MULT*FD

        NORMRB=ABS(RESB)

        DDG=0.D0

C--------------------------------------------------------
C--BOUCLE DU NEWTON SUR LES VARIABLES INTERNES-----------
C--------------------------------------------------------

38      CONTINUE
        IF(((CRIT.GT.TOLC).OR.(NORMRB.GT.TOLE).OR.(ABS(RESD).GT.TOLE)))
     &      THEN
          IF ((COMPTE.LT.INTMAX).AND.(COUPL.NE.0.D0)) THEN
C Rajout du test sur COUPL (fiche 15020) : lorsque c'est le cas,
C la derivee du residu est une matrice singuliere et le systeme ne
C peut etre resolu. On sort pour enclencher la decoupe du pas de temps
            IF (FBS.GT.0.D0) THEN
              MTE1=0.D0
            ELSE
              MTE1=1.D0
            ENDIF

            CALL DFBDB(3,B,EPS,DEUX*MU,LAMBDA,ECROB,MTE2)

            MTE2S=MTE2(1,1)

            DFDDD=0.D0

            IF ((.NOT.DBLOQ).AND.(FD.NE.0.D0)) THEN
              DFDDD=-(FD+DEUX*ECROD)/(UN-D)
            ENDIF

            KSI=-MULT*ALPHA*MTE1*MTE2S+UN

            IF (KSI.NE.0.D0) THEN
              IKSI=UN/KSI
            ELSE
              CALL U2MESS('F','ALGORITH4_54')
            ENDIF

            PSI=1-MULT*(1-ALPHA)*DFDDD

            DELTA1=ALPHA*FBSM*MTE2S

            DELTA2=(1-ALPHA)*FD*DFDDD

            INTER1=DELTA1*IKSI*RESB
            INTER3=ALPHA*DELTA1*IKSI*FBSM

            INTER2=DELTA2/PSI*RESD
            INTER4=DELTA2/PSI*(1-ALPHA)*FD

            DDG=-(CRIT*COUPL+INTER1+INTER2)/(INTER3+INTER4)

            DD=RESD/PSI+DDG*(1-ALPHA)*FD/PSI
            DBS=IKSI*(RESB+DDG*ALPHA*FBSM)

            BS=BS+DBS
            D=D+DD

            COMPTE=COMPTE+1
            MULT=MULT+DDG

C----CALCUL DE FB DANS NEWTON---------------------------

            CALL R8INIR(6,0.D0,B,1)

            B(1)=BS

            CALL CEOBFB(B,EPS,LAMBDA,MU,ECROB,BDIM,FB,RTEMP,FBM)
            FBS=FB(1)
            FBSM=FBM(1)

C----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION

            IF (DBLOQ) THEN
              FD=0.D0
            ELSE
              CALL CEOBFD(D,EPS,LAMBDA,MU,ECROD,FD)
            ENDIF

C----CALCUL DU CRITERE-------------------------------------
           COUPL=SQRT(ALPHA*RTEMP+(1-ALPHA)*FD**2)
           CRIT=COUPL-SEUIL

           RESB=-BS+BMS+ALPHA*MULT*FBSM
           RESD=-D+DM+(1-ALPHA)*MULT*FD

           NORMRB=ABS(RESB)

       GOTO 38
           ELSE
             IRET = 1
             GOTO 999
           ENDIF
           ENDIF

          ENDIF
 999      CONTINUE

      END
