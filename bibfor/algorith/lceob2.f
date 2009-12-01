       SUBROUTINE LCEOB2 (INTMAX,TOLE,EPS,BM,DM,
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
C OUT DBLOQ    : BLOQUAGE DE L'ENDOMMAGEMENT DE COMPRESSION
C OUT IRET     : CODE RETOUR
C ----------------------------------------------------------------------
C TOLE CRP_20

      INTEGER     I,J,K,COMPTE,T(3,3),R(2,2),IRET1

      REAL*8      BS(3),BMS(3),DBS(3)
      REAL*8      FB(6),FD,DD,DDG
      REAL*8      TREPS,TREB,TREM
      REAL*8      CC(6),CPE(6),CCP(6),FBM(6),RESB(3)
      REAL*8      RAC2
      REAL*8      RTEMP2,RTEMP3,DELTA1(3),DELTA2
      REAL*8      TOLC,DET
      REAL*8      TATA,NORMRB,RTEMP,CRIT
      REAL*8      MTE1(3,3),MTE2(6,6),MTE2S(3,3)
      REAL*8      VECFBS(2,2),VALFBS(2),FBS(3),FBSM(3)
      REAL*8      KSI(3,3),IKSI(3,3),TOTI(3,3),IDE(3,3)
      REAL*8      VECC(3,3),VALCC(3)
      REAL*8      TEME(3,3),COUPL
      REAL*8      RESD,ENE,DCOEFD,DDCOED,DFDDD,PSI
      REAL*8      INTER1,INTER2,INTER3,INTER4
      REAL*8      INTER,TOTO
      REAL*8      KRON(6)

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

      R(1,1)=1
      R(2,2)=2
      R(1,2)=3
      R(2,1)=3

      RAC2=SQRT(2.D0)
      TOLC= SEUIL*TOLE

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

       FBS(1)=FB(1)
       FBS(2)=FB(2)
       FBS(3)=FB(4)

       BS(1)=B(1)
       BS(2)=B(2)
       BS(3)=B(4)

       BMS(1)=BM(1)
       BMS(2)=BM(2)
       BMS(3)=BM(4)

      CALL DIAGO2(FBS,VECFBS,VALFBS)

      RTEMP=0.D0
      DO 29 I=1,2
        IF (VALFBS(I).GT.0.D0) THEN
          VALFBS(I)=0.D0
        ENDIF
        RTEMP=RTEMP+VALFBS(I)*VALFBS(I)
  29  CONTINUE

      CALL R8INIR(3,0.D0,FBSM,1)
      DO 26 I=1,2
        DO 27 J=I,2
          DO 28 K=1,2
        FBSM(R(I,J))=FBSM(R(I,J))+VECFBS(I,K)*VALFBS(K)*VECFBS(J,K)
  28      CONTINUE
  27    CONTINUE
  26  CONTINUE


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
      COUPL=SQRT(ALPHA*RTEMP+(1.D0-ALPHA)*FD**2)
      CRIT=COUPL-SEUIL


      ELAS=.FALSE.

      IF (CRIT.LE.TOLC) THEN
        ELAS=.TRUE.
        GOTO 999

      ELSE

        DO 32 I=1,3
          RESB(I)=-BS(I)+BMS(I)+ALPHA*MULT*FBSM(I)
32      CONTINUE
        RESD=-D+DM+(1.D0-ALPHA)*MULT*FD
        RESB(3)=RAC2*RESB(3)

        TATA=0.D0
        DO 37 I=1,3
          TATA=TATA+RESB(I)*RESB(I)
37      CONTINUE

        NORMRB=SQRT(TATA)

        DDG=0.D0

C--------------------------------------------------------
C--BOUCLE DU NEWTON SUR LES VARIABLES INTERNES-----------
C--------------------------------------------------------


38      CONTINUE
        IF(((CRIT.GT.TOLC).OR.(NORMRB.GT.TOLE).OR.(RESD.GT.TOLE)))
     &      THEN
          IF (COMPTE.LT.INTMAX) THEN

            CALL DFMDF(3,FBS,MTE1)

            CALL DFBDB(3,B,EPS,2.D0*MU,LAMBDA,ECROB,MTE2)

            MTE2S(1,1)=MTE2(1,1)
            MTE2S(1,2)=MTE2(1,2)
            MTE2S(1,3)=MTE2(1,4)
            MTE2S(2,1)=MTE2(2,1)
            MTE2S(2,2)=MTE2(2,2)
            MTE2S(2,3)=MTE2(2,4)
            MTE2S(3,1)=MTE2(4,1)
            MTE2S(3,2)=MTE2(4,2)
            MTE2S(3,3)=MTE2(4,4)

            DDCOED=0.D0
            DFDDD=0.D0

            IF ((.NOT.DBLOQ).AND.(FD.NE.0.D0)) THEN
            DDCOED=-2.D0
            DFDDD=DDCOED*ENE-2.D0*ECROD
            ENDIF

            CALL R8INIR(9,0.D0,KSI,1)
            DO 40 I=1,3
              DO 41 J=1,3
                DO 42 K=1,3
                  KSI(I,J)=KSI(I,J)-MULT*ALPHA*MTE1(I,K)*MTE2S(K,J)
 42             CONTINUE
 41           CONTINUE
 40         CONTINUE

            DO 43 I=1,3
              KSI(I,I)=KSI(I,I)+1.D0
 43         CONTINUE

            DO 44 I=1,3
              DO 45 J=1,3
                TOTI(I,J)=KSI(I,J)
 45           CONTINUE
 44         CONTINUE

            DO 46 I=1,3
              DO 47 J=1,3
                IF (I.EQ.J) THEN
                  IDE(I,J)=1.D0
                ELSE
                  IDE(I,J)=0.D0
                ENDIF
 47           CONTINUE
 46         CONTINUE
            CALL R8INIR(9,0.D0,TEME,1)
            DO 48 I=1,3
              DO 49 J=1,3
                TEME(I,J)=IDE(I,J)
 49           CONTINUE
 48         CONTINUE
            CALL MGAUSS('NFVP',TOTI,TEME,3,3,3,DET,IRET1)
            CALL R8INIR(9,0.D0,IKSI,1)
            DO 51 I=1,3
              DO 52 J=1,3
                IKSI(I,J)=TEME(I,J)
 52           CONTINUE
 51         CONTINUE

            PSI=1.D0-MULT*(1.D0-ALPHA)*DFDDD

            CALL R8INIR(3,0.D0,DELTA1,1)
            DO 53 I=1,3
              DO 54 J=1,3
                IF (J.EQ.3) THEN
                  RTEMP2=RAC2
                ELSE
                  RTEMP2=1.D0
                ENDIF
              DELTA1(I)=DELTA1(I)+ALPHA/COUPL*RTEMP2*FBSM(J)*MTE2S(J,I)
 54           CONTINUE
 53         CONTINUE

            DELTA2=(1.D0-ALPHA)/COUPL*FD*DFDDD

            INTER1=0.D0
            INTER3=0.D0
            DO 55 I=1,3
              DO 56 J=1,3
                IF (J.EQ.3) THEN
                  RTEMP2=RAC2
                ELSE
                  RTEMP2=1.D0
                ENDIF
                INTER1=INTER1+DELTA1(I)*IKSI(I,J)*RESB(J)
                INTER3=INTER3+ALPHA*RTEMP2*DELTA1(I)
     &                            *IKSI(I,J)*FBSM(J)
 56           CONTINUE
 55         CONTINUE

            INTER2=DELTA2/PSI*RESD
            INTER4=DELTA2/PSI*(1.D0-ALPHA)*FD

            DDG = -(CRIT+INTER1+INTER2)/(INTER3+INTER4)

            DD=RESD/PSI+DDG*(1.D0-ALPHA)*FD/PSI

            CALL R8INIR(3,0.D0,DBS,1)
            DO 57 I=1,3
              DO 58 J=1,3
                IF (I.EQ.3) THEN
                  RTEMP2=1/RAC2
                ELSE
                  RTEMP2=1.D0
                ENDIF
                IF (J.EQ.3) THEN
                  RTEMP3=RAC2
                ELSE
                  RTEMP3=1.D0
                ENDIF
                DBS(I)=DBS(I)+RTEMP2*IKSI(I,J)*
     &                     (RESB(J)+DDG*ALPHA*FBSM(J)*RTEMP3)
 58           CONTINUE
 57         CONTINUE

            DO 59 I=1,3
              BS(I)=BS(I)+DBS(I)
 59         CONTINUE
            D=D+DD

            COMPTE=COMPTE+1
            MULT=MULT+DDG

C----CALCUL DE FB DANS NEWTON---------------------------

           CALL R8INIR(6,0.D0,B,1)

           B(1)=BS(1)
           B(2)=BS(2)
           B(4)=BS(3)

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

            FBS(1)=FB(1)
            FBS(2)=FB(2)
            FBS(3)=FB(4)

            CALL DIAGO2(FBS,VECFBS,VALFBS)

            RTEMP=0.D0
            DO 129 I=1,2
              IF (VALFBS(I).GT.0.D0) THEN
                VALFBS(I)=0.D0
              ENDIF
              RTEMP=RTEMP+VALFBS(I)*VALFBS(I)
  129      CONTINUE

           CALL R8INIR(3,0.D0,FBSM,1)

           DO 126 I=1,2
             DO 127 J=I,2
               DO 128 K=1,2
         FBSM(R(I,J))=FBSM(R(I,J))+VECFBS(I,K)*VALFBS(K)*VECFBS(J,K)
  128          CONTINUE
  127        CONTINUE
  126      CONTINUE


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
           COUPL=SQRT(ALPHA*RTEMP+(1.D0-ALPHA)*FD**2.D0)
           CRIT=COUPL-SEUIL

            DO 132 I=1,3
              RESB(I)=-BS(I)+BMS(I)+ALPHA*MULT*FBSM(I)
132         CONTINUE
            RESD=-D+DM+(1.D0-ALPHA)*MULT*FD
            RESB(3)=RAC2*RESB(3)

            TATA=0.D0
            DO 137 I=1,3
              TATA=TATA+RESB(I)*RESB(I)
137         CONTINUE

            NORMRB=SQRT(TATA)

            GOTO 38
           ELSE
            IRET=1
            GOTO 999
           ENDIF
           ENDIF

          ENDIF
 999      CONTINUE


      END
