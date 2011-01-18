       SUBROUTINE LCEOB3 (INTMAX,TOLE,EPS,BM,DM,
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

      INTEGER            INTMAX,IRET,BDIM

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
C  IN LAMBDA  : /
C  IN MU      : / COEFFICIENTS DE LAME
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

      INTEGER     I,J,K,COMPTE,IRET1

      REAL*8      FB(6),DB(6),FD,DD,FBM(6),RESB(6)
      REAL*8      RAC2,UN,DEUX
      REAL*8      RTEMP2,RTEMP3,DELTA1(6),DELTA2
      REAL*8      DDG,TOLC,DET,TATA,NORMRB,RTEMP,CRIT
      REAL*8      MTE1(6,6),MTE2(6,6)
      REAL*8      KSI(6,6),IKSI(6,6),TOTI(6,6),IDE(6,6)
      REAL*8      TEME(6,6),COUPL
      REAL*8      RESD,DFDDD,PSI
      REAL*8      INTER1,INTER2,INTER3,INTER4

      DEUX=2.D0
      RAC2=SQRT(DEUX)
      TOLC=SEUIL*TOLE
      UN=1.D0
      COMPTE=0
      MULT=0.D0

      DO 100 I=1,6
        B(I)=BM(I)
 100  CONTINUE

      D=DM

C-------------------------------------------------------
C-------------------------------------------------------
C----CALCUL DE FB: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION

      CALL CEOBFB(B,EPS,LAMBDA,MU,ECROB,BDIM,FB,RTEMP,FBM)

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

        DO 32 I=1,6
          RESB(I)=-B(I)+BM(I)+ALPHA*MULT*FBM(I)
32      CONTINUE
        RESD=-D+DM+(1-ALPHA)*MULT*FD
        DO 33 I=4,6
          RESB(I)=RAC2*RESB(I)
33      CONTINUE

        TATA=0.D0
        DO 37 I=1,6
          TATA=TATA+RESB(I)*RESB(I)
37      CONTINUE

        NORMRB=SQRT(TATA)

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

            CALL DFMDF(6,FB,MTE1)
            CALL DFBDB(3,B,EPS,DEUX*MU,LAMBDA,ECROB,MTE2)

            DFDDD=0.D0

            IF ((.NOT.DBLOQ).AND.(FD.NE.0.D0)) THEN
              DFDDD=-(FD+DEUX*ECROD)/(UN-D)
            ENDIF

            CALL R8INIR(36,0.D0,KSI,1)
            DO 40 I=1,6
              DO 41 J=1,6
                DO 42 K=1,6
                  KSI(I,J)=KSI(I,J)-MULT*ALPHA*MTE1(I,K)*MTE2(K,J)
 42             CONTINUE
 41           CONTINUE
 40         CONTINUE

            DO 43 I=1,6
              KSI(I,I)=KSI(I,I)+1
 43         CONTINUE

            DO 44 I=1,6
              DO 45 J=1,6
                TOTI(I,J)=KSI(I,J)
 45           CONTINUE
 44         CONTINUE
            DO 46 I=1,6
              DO 47 J=1,6
                IF (I.EQ.J) THEN
                  IDE(I,J)=1.D0
                ELSE
                  IDE(I,J)=0.D0
                ENDIF
 47           CONTINUE
 46         CONTINUE
            CALL R8INIR(36,0.D0,TEME,1)
            DO 48 I=1,6
              DO 49 J=1,6
                TEME(I,J)=IDE(I,J)
 49           CONTINUE
 48         CONTINUE
            CALL MGAUSS('NFVP',TOTI,TEME,6,6,6,DET,IRET1)
            CALL R8INIR(36,0.D0,IKSI,1)
            DO 51 I=1,6
              DO 52 J=1,6
                IKSI(I,J)=TEME(I,J)
 52           CONTINUE
 51         CONTINUE

            PSI=1-MULT*(1-ALPHA)*DFDDD

            CALL R8INIR(6,0.D0,DELTA1,1)
            DO 53 I=1,6
              DO 54 J=1,6
                IF (J.GE.4) THEN
                  RTEMP2=RAC2
                ELSE
                  RTEMP2=1.D0
                ENDIF
               DELTA1(I)=DELTA1(I)+ALPHA/COUPL*RTEMP2*FBM(J)*MTE2(J,I)
 54           CONTINUE
 53         CONTINUE

            DELTA2=(1-ALPHA)/COUPL*FD*DFDDD

            INTER1=0.D0
            INTER3=0.D0
            DO 55 I=1,6
              DO 56 J=1,6
                IF (J.GE.4) THEN
                  RTEMP2=RAC2
                ELSE
                  RTEMP2=1.D0
                ENDIF
                INTER1=INTER1+DELTA1(I)*IKSI(I,J)*RESB(J)
                INTER3=INTER3+ALPHA*RTEMP2*DELTA1(I)
     &                            *IKSI(I,J)*FBM(J)
 56           CONTINUE
 55         CONTINUE

            INTER2=DELTA2/PSI*RESD
            INTER4=DELTA2/PSI*(1-ALPHA)*FD

            DDG=-(CRIT+INTER1+INTER2)/(INTER3+INTER4)

            CALL R8INIR(6,0.D0,DB,1)

            DD=RESD/PSI+DDG*(1-ALPHA)*FD/PSI
            DO 57 I=1,6
              DO 58 J=1,6
                IF (I.GE.4) THEN
                  RTEMP2=1/RAC2
                ELSE
                  RTEMP2=1.D0
                ENDIF
                IF (J.GE.4) THEN
                  RTEMP3=RAC2
                ELSE
                  RTEMP3=1.D0
                ENDIF
                DB(I)=DB(I)+RTEMP2*IKSI(I,J)*
     &                     (RESB(J)+DDG*ALPHA*FBM(J)*RTEMP3)
 58           CONTINUE
 57         CONTINUE

            DO 59 I=1,6
              B(I)=B(I)+DB(I)
 59         CONTINUE
            D=D+DD

            COMPTE=COMPTE+1
            MULT=MULT+DDG

C----CALCUL DE FB DANS NEWTON---------------------------

            CALL CEOBFB(B,EPS,LAMBDA,MU,ECROB,BDIM,FB,RTEMP,FBM)

C----CALCUL DE FD DANS NEWTON----------------------------
            IF (DBLOQ) THEN
              FD=0.D0
            ELSE
              CALL CEOBFD(D,EPS,LAMBDA,MU,ECROD,FD)
            ENDIF

C----CALCUL DU CRITERE-------------------------------------
            COUPL=SQRT(ALPHA*RTEMP+(1-ALPHA)*FD**2)
            CRIT=COUPL-SEUIL

            DO 132 I=1,6
              RESB(I)=-B(I)+BM(I)+ALPHA*MULT*FBM(I)
132         CONTINUE
            RESD=-D+DM+(1-ALPHA)*MULT*FD
            DO 133 I=4,6
              RESB(I)=RAC2*RESB(I)
133         CONTINUE

            TATA=0.D0
            DO 137 I=1,6
              TATA=TATA+RESB(I)*RESB(I)
137          CONTINUE

            NORMRB=SQRT(TATA)

       GOTO 38
           ELSE
             IRET = 1
             GOTO 999
           ENDIF
           ENDIF

          ENDIF
 999      CONTINUE
      
      END
