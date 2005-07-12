      SUBROUTINE MEOBL2 (EPS,B,D,DELTAB,DELTAD,MULT,LAMBDA,
     &                    MU,ECROB,ECROD,ALPHA,K1,K2,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 11/07/2005   AUTEUR VABHHTS J.PELLET 
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

      REAL*8            EPS(6),B(6),D,DSIDEP(6,6)
      REAL*8            DELTAB(6),DELTAD,MULT
      REAL*8            LAMBDA,MU,ALPHA,K1,K2,ECROB,ECROD

C--CALCUL DE LA MATRICE TANGENTE POUR LA LOI ENDO_ORTHO_BETON
C
C
C
C
C
C-------------------------------------------------------------

      INTEGER            I,J,K,T(3,3),T2(2,2),IRET
      REAL*8             RAC2,KRON(6),NOFBM,UN,DET
      REAL*8             CC(6),VECC(3,3),VALCC(3),CCP(6),CPE(6)
      REAL*8             VECEPS(3,3),VALEPS(3),TOTO
      REAL*8             FB(6),VECFB(3,3),VALFB(3),TREB
      REAL*8             TREPS,DCOEFD,ENE,FD,TREM
      REAL*8             DFMF(3,3),TDFBDB(6,6),TDFBDE(6,6)
      REAL*8             TDFDDE(6),TDFDDD
      REAL*8             INTERD(3),INTERT(6),INTERG(3)
      REAL*8             PSI(3,6),KSI(3,3),IKSI(3,3)
      REAL*8             MATB(3,6),MATD(6)
      REAL*8             FBS(3),VECFBS(2,2),VALFBS(2),DELTAS(3)
      REAL*8             FBSM(3),SDFBDB(3,3),SDFBDE(3,6)
      REAL*8             COUPL,DCRIT(6)


      DATA  KRON/1.D0,1.D0,1.D0,0.D0,0.D0,0.D0/

      RAC2=SQRT(2.D0)
      UN=1.D0
      T(1,1)=1
      T(2,2)=2
      T(3,3)=3
      T(1,2)=4
      T(2,1)=4
      T(1,3)=5
      T(3,1)=5
      T(2,3)=6
      T(3,2)=6

      T2(1,1)=1
      T2(2,2)=2
      T2(1,2)=3
      T2(2,1)=3


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

       DELTAS(1)=DELTAB(1)
       DELTAS(2)=DELTAB(2)
       DELTAS(3)=DELTAB(4)


      CALL DIAGO2(FBS,VECFBS,VALFBS)

      DO 29 I=1,2
        IF (VALFBS(I).GT.0.D0) THEN
          VALFBS(I)=0.D0
        ENDIF
  29  CONTINUE
      CALL R8INIR(3,0.D0,FBSM,1)
      DO 26 I=1,2
        DO 27 J=I,2
          DO 28 K=1,2
        FBSM(T2(I,J))=FBSM(T2(I,J))+VECFBS(I,K)*VALFBS(K)*VECFBS(J,K)
  28      CONTINUE
  27    CONTINUE
  26  CONTINUE






C----CALCUL DE FD: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION


        TREPS=EPS(1)+EPS(2)+EPS(3)
        CALL DIAGO3(EPS,VECEPS,VALEPS)
        DO 22 I=1,3
          IF (VALEPS(I).GT.0.D0) THEN
            VALEPS(I)=0.D0
          ENDIF
 22     CONTINUE

        TREM=VALEPS(1)**2+VALEPS(2)**2+VALEPS(3)**2
        IF (TREPS.GT.0.D0) THEN
          TREPS=0.D0
        ENDIF
        DCOEFD=2.D0*(1.D0-D)
        ENE=LAMBDA/2*TREPS**2+MU*TREM
        FD=DCOEFD*ENE-2.D0*D*ECROD
        IF (FD.LT.0.D0) THEN
          FD=0.D0
        ENDIF


C---CALCUL DE DERIVEES UTILES----------------------------------

      CALL DFMDF(3,FBS,DFMF)

C----CALCUL DE LA DERIVEE DU SEUIL---------------------


      DCRIT(1)=-K1*(-TREPS/K2/(1.D0+(-TREPS/K2)**2.D0)
     &           +ATAN2(-TREPS/K2,UN))
      DCRIT(2)=-K1*(-TREPS/K2/(1.D0+(-TREPS/K2)**2.D0)
     &           +ATAN2(-TREPS/K2,UN))
      DCRIT(3)=-K1*(-TREPS/K2/(1.D0+(-TREPS/K2)**2.D0)
     &           +ATAN2(-TREPS/K2,UN))
      DCRIT(4)=0.D0
      DCRIT(5)=0.D0
      DCRIT(6)=0.D0




      CALL DFBDB(3,B,EPS,2.D0*MU,LAMBDA,ECROB,TDFBDB)
      CALL DFBDE(3,B,EPS,2.D0*MU,LAMBDA,TDFBDE)

            SDFBDB(1,1)=TDFBDB(1,1)
            SDFBDB(1,2)=TDFBDB(1,2)
            SDFBDB(1,3)=TDFBDB(1,4)
            SDFBDB(2,1)=TDFBDB(2,1)
            SDFBDB(2,2)=TDFBDB(2,2)
            SDFBDB(2,3)=TDFBDB(2,4)
            SDFBDB(3,1)=TDFBDB(4,1)
            SDFBDB(3,2)=TDFBDB(4,2)
            SDFBDB(3,3)=TDFBDB(4,4)

      DO 381 I=1,6
        SDFBDE(1,I)=TDFBDE(1,I)
        SDFBDE(2,I)=TDFBDE(2,I)
        SDFBDE(3,I)=TDFBDE(4,I)
 381  CONTINUE


        FBSM(3)=RAC2*FBSM(3)
        DELTAS(3)=DELTAS(3)*RAC2

      CALL DFDDE(EPS,D,3,LAMBDA,MU,TDFDDE)
      CALL DFDDD(EPS,D,3,LAMBDA,MU,ECROD,TDFDDD)

         NOFBM=FBSM(1)**2+FBSM(2)**2+FBSM(3)**2

      COUPL=SQRT(ALPHA*NOFBM+(1.D0-ALPHA)*FD**2.D0)

       IF ((FD.NE.0.D0).AND.(NOFBM.NE.0.D0)) THEN


C---CALCUL DE DBDE ET DDDE-------------------------------------

C---CALCUL DE KSI ET PSI

      CALL R8INIR(3,0.D0,INTERD,1)
      CALL R8INIR(3,0.D0,INTERG,1)
      CALL R8INIR(6,0.D0,INTERT,1)
      CALL R8INIR(18,0.D0,PSI,1)
      CALL R8INIR(9,0.D0,KSI,1)

      DO 110 I=1,6
        INTERT(I)=(1.D0-ALPHA)*FD*TDFDDE(I)-COUPL*DCRIT(I)
        DO 111 J=1,3
          DO 112 K=1,3
          INTERT(I)=INTERT(I)+ALPHA*FBSM(K)*DFMF(K,J)*SDFBDE(J,I)
 112      CONTINUE
 111    CONTINUE
 110  CONTINUE



      DO 310 I=1,3
        INTERG(I)=DELTAS(I)/FD-ALPHA*FBSM(I)/(1.D0-ALPHA)/FD/TDFDDD
        DO 311 J=1,3
          DO 312 K=1,3
          KSI(I,J)=KSI(I,J)+ALPHA*DELTAD*DFMF(I,K)*SDFBDB(K,J)
          INTERD(I)=INTERD(I)+ALPHA*FBSM(K)*DFMF(K,J)*SDFBDB(J,I)
 312      CONTINUE
 311    CONTINUE
        DO 313 J=1,6
          DO 314 K=1,3
          PSI(I,J)=PSI(I,J)-ALPHA*DELTAD*DFMF(I,K)*SDFBDE(K,J)
 314      CONTINUE
 313    CONTINUE
 310  CONTINUE


      DO 120 I=1,3
        KSI(I,I)=KSI(I,I)-(1.D0-ALPHA)*FD
 120  CONTINUE

      DO 130 I=1,3
        DO 131 J=1,3
        KSI(I,J)=KSI(I,J)+INTERG(I)*INTERD(J)
 131    CONTINUE
        DO 331 J=1,6
        PSI(I,J)=PSI(I,J)-INTERG(I)*INTERT(J)
     &                  +(1.D0-ALPHA)*DELTAS(I)*TDFDDE(J)
 331    CONTINUE
 130  CONTINUE


      CALL R8INIR(9,0.D0,IKSI,1)
      DO 140 I=1,3
        IKSI(I,I)=1.D0
 140  CONTINUE

      CALL MGAUSS('NFVP',KSI,IKSI,3,3,3,DET,IRET)


C-- ! ksi n est plus disponible

      CALL R8INIR(18,0.D0,MATB,1)
      CALL R8INIR(6,0.D0,MATD,1)

      DO 150 I=1,6
        MATD(I)=-INTERT(I)/(1.D0-ALPHA)/FD/TDFDDD
        DO 151 J=1,3
               DO 152 K=1,3
            MATB(J,I)=MATB(J,I)+IKSI(J,K)*PSI(K,I)
            MATD(I)=MATD(I)-INTERD(J)*IKSI(J,K)*PSI(K,I)
     &                   /(1.D0-ALPHA)/FD/TDFDDD
152          CONTINUE
C            WRITE(6,*) 'MB(',J,',',I,')=',MATB(J,I),';'
151        CONTINUE
150    CONTINUE



      CALL R8INIR(36,0.D0,DSIDEP,1)


       DO 201 I=1,6
         DO 202 J=1,6
           DSIDEP(I,J)=-TDFDDE(I)*MATD(J)
C         WRITE(6,*) 'DID(',I,',',J,')=', DSIDEP(I,J),';'
           DO 203 K=1,3
             DSIDEP(I,J)=DSIDEP(I,J)-SDFBDE(K,I)*MATB(K,J)
 203             CONTINUE
 202           CONTINUE
 201   CONTINUE




       ELSEIF ((FD.EQ.0.D0).AND.(NOFBM.NE.0.D0)) THEN

         CALL R8INIR(9,0.D0,KSI,1)
         CALL R8INIR(18,0.D0,PSI,1)

         DO 500 I=1,3
           DO 501 J=1,3
             KSI(I,J)=-FBSM(I)*FBSM(J)/NOFBM
             DO 502 K=1,3
               KSI(I,J)=KSI(I,J)-ALPHA*MULT*DFMF(I,K)*SDFBDB(K,J)
 502         CONTINUE
 501       CONTINUE
           DO 581 J=1,6
             PSI(I,J)=PSI(I,J)-FBSM(I)*ALPHA*MULT/COUPL*DCRIT(J)
             DO 582 K=1,3
               PSI(I,J)=PSI(I,J)+ALPHA*MULT*DFMF(I,K)*SDFBDE(K,J)
 582         CONTINUE
 581       CONTINUE
 500     CONTINUE

         DO 504 I=1,3
           KSI(I,I)=KSI(I,I)+1
 504     CONTINUE

         CALL R8INIR(9,0.D0,IKSI,1)
         DO 505 I=1,3
           IKSI(I,I)=1.D0
 505     CONTINUE

         CALL MGAUSS('NFVP',KSI,IKSI,3,3,3,DET,IRET)

         CALL R8INIR(18,0.D0,MATB,1)

         DO 550 I=1,3
           DO 551 J=1,6
                  DO 552 K=1,3
               MATB(I,J)=MATB(I,J)+IKSI(I,K)*PSI(K,J)
552               CONTINUE
551             CONTINUE
550      CONTINUE

         CALL R8INIR(36,0.D0,DSIDEP,1)
         DO 561 I=1,6
           DO 562 J=1,6
             DO 563 K=1,3
              DSIDEP(I,J)=DSIDEP(I,J)-SDFBDE(K,I)*MATB(K,J)
 563               CONTINUE
 562             CONTINUE
 561     CONTINUE

        ELSEIF ((FD.NE.0.D0).AND.(NOFBM.EQ.0.D0)) THEN

         CALL R8INIR(36,0.D0,DSIDEP,1)
         DO 661 I=1,6
           DO 662 J=1,6
             DSIDEP(I,J)= -TDFDDE(I)*(-TDFDDE(J)+COUPL/(1.D0-ALPHA)
     &                      *DCRIT(J)/FD)/TDFDDD
 662             CONTINUE
 661     CONTINUE

      ENDIF



      END
