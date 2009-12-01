       SUBROUTINE LCEOB1 (INTMAX,TOLE,EPS,BM,DM,
     &                     LAMBDA,MU,ALPHA,ECROB,ECROD,
     &                     SEUIL,B,D,MULT,ELAS,DBLOQ,IRET)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 24/03/2009   AUTEUR REZETTE C.REZETTE 
C ======================================================================
C COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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

      INTEGER            INTMAX, IRET

      LOGICAL            ELAS,DBLOQ
C ----------------------------------------------------------------------
C     LOI DE COMPORTEMENT DU MODELE D'ENDOMMAGEMENT ANISOTROPE
C     ROUTINE DE RESOLUTION DU SYSTEME NON LINEAIRE
C     ALGORITHME DE NEWTON
C
C
C
C  IN INTMAX  : NBRE D'ITERATION MAX POUR LE NEWTON LOCAL
C  IN TOLE   : RESIDU TOLERE POUR LE NEWTON LOCAL
C  IN  NDIM    : DIMENSION DE L'ESPACE
C  IN  CRIT   : CRITERES DE CONVERGENCE LOCAUX
C  IN  EPSM    : DEFORMATION EN T- REPERE GLOBAL
C  IN  DEPS    : INCREMENT DE DEFORMATION
C  IN  BM DM     : VARIABLES INTERNES EN T-
C  IN LAMBDA     : /
C  IN MU        : / COEFFICIENTS DE LAME
C  IN  ALPHA    : /
C  IN  ECROB    : /
C  IN  ECROD    : / PARAMETRES DU MODELE
C  IN  SEUIL    : SEUIL DU CRITERE D'ENDOMMAGEMENT
C
C OUT  B D     : VARIABLES INTERNES EN T+
C OUT MULT     : MULTIPLICATEUR PLASTIQUE DU PRINCIPE DE NORMALITE
C OUT ELAS     : ELASTIQUE OU DISSIPATION?
C OUT DBLOQ  : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
C OUT IRET   : CODE RETOUR
C ----------------------------------------------------------------------

      INTEGER     I,J,K,COMPTE,T(3,3)

      REAL*8      BS,BMS
      REAL*8      FB(6),DBS,FD,DD
      REAL*8      TREPS,TREB,TREM
      REAL*8      CC(6),CPE(6),CCP(6),FBM(6),RESB
      REAL*8      RTEMP2,RTEMP3,DELTA1,DELTA2
      REAL*8      DDG,FBEN(1)
      REAL*8      TOLC
      REAL*8      NORMRB,RTEMP,CRIT
      REAL*8      MTE1,MTE2(6,6),MTE2S
      REAL*8      FBS,FBSM
      REAL*8      KSI,IKSI
      REAL*8      VECC(3,3),VALCC(3)
      REAL*8      COUPL,KRON(6)
      REAL*8      RESD,ENE,DCOEFD,DDCOED,DFDDD,PSI
      REAL*8      INTER1,INTER2,INTER3,INTER4

      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3

      TOLC=SEUIL*TOLE

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

      CALL R8INIR(6,0.D0,CC,1)

      DO 9 I=1,3
        DO 10 J=I,3
          DO 11 K=1,3
            CC(T(I,J))=CC(T(I,J))+B(T(I,K))*EPS(T(K,J))+
     &                 B(T(J,K))*EPS(T(K,I))
 11       CONTINUE
 10     CONTINUE
 9    CONTINUE
      CALL DIAGO3(CC,VECC,VALCC)
      CALL R8INIR(6,0.D0,CCP,1)
      CALL R8INIR(6,0.D0,CPE,1)
      DO 12 I=1,3
        IF (VALCC(I).LT.0.D0) THEN
          VALCC(I)=0.D0
        ENDIF
 12   CONTINUE
      DO 13 I=1,3
        DO 14 J=I,3
          DO 15 K=1,3
          CCP(T(I,J))=CCP(T(I,J))+VECC(I,K)*VALCC(K)*VECC(J,K)
 15       CONTINUE
 14     CONTINUE
 13   CONTINUE
      DO 16 I=1,3
        DO 17 J=I,3
          DO 18 K=1,3
            CPE(T(I,J))=CPE(T(I,J))+ CCP(T(I,K))*EPS(T(K,J))+
     &                    CCP(T(J,K))*EPS(T(K,I))
  18      CONTINUE
  17    CONTINUE
  16  CONTINUE

      CALL R8INIR(6,0.D0,FB,1)
      TREB=0.D0
      DO 301 I=1,3
      TREB=TREB+CC(I)/2
 301  CONTINUE
      IF (TREB.GT.0.D0) THEN
        DO 19 I=1,6
          FB(I)=-LAMBDA*TREB*EPS(I)
  19    CONTINUE
      ENDIF
      DO 20 I=1,6
        FB(I)=FB(I)-MU/2.D0*CPE(I)+ECROB*(KRON(I)-B(I))
  20  CONTINUE

       FBS=FB(1)
       BS=B(1)
       BMS=BM(1)

        IF (FBS.GT.0.D0) THEN
          FBSM=0.D0
        ELSE
          FBSM=FBS
        ENDIF
        RTEMP=FBS**2

C----CALCUL DE FD: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION

      IF (DBLOQ) THEN
        FD=0.D0
      ELSE
        TREPS=EPS(1)+EPS(2)+EPS(3)
        CALL DIAGO3(EPS,VECC,VALCC)
        DO 22 I=1,3
          IF (VALCC(I).GT.0.D0) THEN
            VALCC(I)=0.D0
          ENDIF
 22     CONTINUE

        CALL R8INIR(6,0.D0,CCP,1)

        DO 23 I=1,3
          DO 24 J=I,3
            DO 25 K=1,3
            CCP(T(I,J))=CCP(T(I,J))+VECC(I,K)*VALCC(K)*VECC(J,K)
 25         CONTINUE
 24       CONTINUE
 23     CONTINUE

        TREM=VALCC(1)**2+VALCC(2)**2+VALCC(3)**2
        IF (TREPS.GT.0.D0) THEN
          TREPS=0.D0
        ENDIF
        DCOEFD=2.D0*(1.D0-D)
        ENE=LAMBDA/2*TREPS**2+MU*TREM
        FD=DCOEFD*ENE-2.D0*D*ECROD
        IF (FD.LT.0.D0) THEN
          FD=0.D0
        ENDIF
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
        IF(((CRIT.GT.TOLC).OR.(NORMRB.GT.TOLE).OR.(RESD.GT.TOLE)))
     &      THEN
          IF (COMPTE.LT.INTMAX) THEN

            IF (FBS.GT.0.D0) THEN
              MTE1=0.D0
            ELSE
              MTE1=1.D0
            ENDIF

            CALL DFBDB(3,B,EPS,2.D0*MU,LAMBDA,ECROB,MTE2)

            MTE2S=MTE2(1,1)

            DDCOED=0.D0
            DFDDD=0.D0

            IF ((.NOT.DBLOQ).AND.(FD.NE.0.D0)) THEN
            DDCOED=-2.D0
            DFDDD=DDCOED*ENE-2.D0*ECROD
            ENDIF

            KSI=-MULT*ALPHA*MTE1*MTE2S+1.D0

            IF (KSI.NE.0.D0) THEN
              IKSI=1.D0/KSI
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

            CALL R8INIR(6,0.D0,CC,1)

            DO 109 I=1,3
              DO 110 J=I,3
                DO 111 K=1,3
                  CC(T(I,J))=CC(T(I,J))+B(T(I,K))*EPS(T(K,J))+
     &                      B(T(J,K))*EPS(T(K,I))
 111            CONTINUE
 110          CONTINUE
 109        CONTINUE

            CALL DIAGO3(CC,VECC,VALCC)
            CALL R8INIR(6,0.D0,CCP,1)
            CALL R8INIR(6,0.D0,CPE,1)
            DO 112 I=1,3
              IF (VALCC(I).LT.0.D0) THEN
                VALCC(I)=0.D0
              ENDIF
 112        CONTINUE
            DO 113 I=1,3
              DO 114 J=I,3
                DO 115 K=1,3
             CCP(T(I,J))=CCP(T(I,J))+VECC(I,K)*VALCC(K)*VECC(J,K)
 115            CONTINUE
 114          CONTINUE
 113        CONTINUE
            DO 116 I=1,3
              DO 117 J=I,3
                DO 118 K=1,3
             CPE(T(I,J))=CPE(T(I,J))+ CCP(T(I,K))*EPS(T(K,J))+
     &                     CCP(T(J,K))*EPS(T(K,I))
 118            CONTINUE
 117          CONTINUE
 116        CONTINUE

            CALL R8INIR(6,0.D0,FB,1)
            TREB=(CC(1)+CC(2)+CC(3))/2
            IF (TREB.GT.0.D0) THEN
              DO 119 I=1,6
                FB(I)=-LAMBDA*TREB*EPS(I)
 119          CONTINUE
            ENDIF
            DO 120 I=1,6
              FB(I)=FB(I)-MU/2.D0*CPE(I)+ECROB*(KRON(I)-B(I))
 120        CONTINUE

            FBS=FB(1)
            IF (FBS.GT.0.D0) THEN
              FBSM=0.D0
            ELSE
              FBSM=FBS
            ENDIF
            RTEMP=FBS**2
C----CALCUL DE FD: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION

            IF (DBLOQ) THEN
              FD=0.D0
            ELSE
              DCOEFD=2.D0*(1.D0-D)
              FD=DCOEFD*ENE-2.D0*D*ECROD
              IF (FD.LT.0.D0) THEN
                FD=0.D0
              ENDIF
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
