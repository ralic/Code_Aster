      SUBROUTINE MEOBL3 (EPS,B,D,DELTAB,DELTAD,MULT,LAMBDA,
     &                    MU,ECROB,ECROD,ALPHA,K1,K2,DSIDEP)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/05/2010   AUTEUR IDOUX L.IDOUX 
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

      INTEGER            I,J,K,T(3,3),IRET
      REAL*8             RAC2,NOFBM,UN,DET,DEUX
      REAL*8             FB(6),FBM(6),VECFB(3,3),VALFB(3)
      REAL*8             TREPS,FD
      REAL*8             DFBMDF(6,6),TDFBDB(6,6),TDFBDE(6,6)
      REAL*8             TDFDDE(6),TDFDDD
      REAL*8             INTERD(6),INTERT(6),INTERG(6)
      REAL*8             PSI(6,6),KSI(6,6),IKSI(6,6)
      REAL*8             MATB(6,6),MATD(6)
      REAL*8             COUPL,DCRIT(6)

      UN=1.D0
      DEUX=2.D0
      RAC2=SQRT(DEUX)

      T(1,1)=1
      T(2,2)=2
      T(3,3)=3
      T(1,2)=4
      T(2,1)=4
      T(1,3)=5
      T(3,1)=5
      T(2,3)=6
      T(3,2)=6

C-------------------------------------------------------
C-------------------------------------------------------
C----CALCUL DE FB: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION

      CALL CEOBFB(B,EPS,LAMBDA,MU,ECROB,FB)

      CALL DIAGO3(FB,VECFB,VALFB)
      DO 29 I=1,3
        IF (VALFB(I).GT.0.D0) THEN
          VALFB(I)=0.D0
        ENDIF
  29  CONTINUE
      CALL R8INIR(6,0.D0,FBM,1)
      DO 26 I=1,3
        DO 27 J=I,3
          DO 28 K=1,3
            FBM(T(I,J))=FBM(T(I,J))+VECFB(I,K)*VALFB(K)*VECFB(J,K)
  28      CONTINUE
  27    CONTINUE
  26  CONTINUE

C----CALCUL DE FD: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION

        CALL CEOBFD(D,EPS,LAMBDA,MU,ECROD,FD)
        IF (FD.LT.0.D0) THEN
          FD=0.D0
        ENDIF

C---CALCUL DE DERIVEES UTILES----------------------------------

      CALL DFMDF(6,FB,DFBMDF)
      CALL DFBDB(3,B,EPS,DEUX*MU,LAMBDA,ECROB,TDFBDB)
      CALL DFBDE(3,B,EPS,DEUX*MU,LAMBDA,TDFBDE)

C----CALCUL DE LA DERIVEE DU SEUIL---------------------

      TREPS=EPS(1)+EPS(2)+EPS(3)
      IF (TREPS.GT.0.D0) THEN
        TREPS=0.D0
      ENDIF      
      DCRIT(1)=-K1*(-TREPS/K2/(UN+(-TREPS/K2)**DEUX)
     &           +ATAN2(-TREPS/K2,UN))
      DCRIT(2)=-K1*(-TREPS/K2/(UN+(-TREPS/K2)**DEUX)
     &           +ATAN2(-TREPS/K2,UN))
      DCRIT(3)=-K1*(-TREPS/K2/(UN+(-TREPS/K2)**DEUX)
     &           +ATAN2(-TREPS/K2,UN))
      DCRIT(4)=0.D0
      DCRIT(5)=0.D0
      DCRIT(6)=0.D0

      DO 101 I=4,6
        FBM(I)=RAC2*FBM(I)
        DELTAB(I)=DELTAB(I)*RAC2
 101  CONTINUE

      CALL DFDDE(EPS,D,3,LAMBDA,MU,TDFDDE)
      CALL DFDDD(EPS,D,3,LAMBDA,MU,ECROD,TDFDDD)

      NOFBM=FBM(1)**2+FBM(2)**2+FBM(3)**2+FBM(4)**2
     &        +FBM(5)**2+FBM(6)**2

       COUPL=SQRT(ALPHA*NOFBM+(UN-ALPHA)*FD**DEUX)

      CALL R8INIR(36,0.D0,DSIDEP,1)

      IF ((FD.NE.0.D0).AND.(NOFBM.NE.0.D0)) THEN

C---CALCUL DE DBDE ET DDDE-------------------------------------

C---CALCUL DE KSI ET PSI

      CALL R8INIR(6,0.D0,INTERD,1)
      CALL R8INIR(6,0.D0,INTERG,1)
      CALL R8INIR(6,0.D0,INTERT,1)
      CALL R8INIR(36,0.D0,PSI,1)
      CALL R8INIR(36,0.D0,KSI,1)

      DO 110 I=1,6
        INTERG(I)=DELTAB(I)/FD-ALPHA*FBM(I)/(UN-ALPHA)/FD/TDFDDD
        INTERT(I)=(UN-ALPHA)*FD*TDFDDE(I)-COUPL*DCRIT(I)
        DO 111 J=1,6
          DO 112 K=1,6
          KSI(I,J)=KSI(I,J)+ALPHA*DELTAD*DFBMDF(I,K)*TDFBDB(K,J)
          INTERD(I)=INTERD(I)+ALPHA*FBM(K)*DFBMDF(K,J)*TDFBDB(J,I)
          PSI(I,J)=PSI(I,J)-ALPHA*DELTAD*DFBMDF(I,K)*TDFBDE(K,J)
          INTERT(I)=INTERT(I)+ALPHA*FBM(K)*DFBMDF(K,J)*TDFBDE(J,I)
 112      CONTINUE
 111    CONTINUE
 110  CONTINUE

      DO 120 I=1,6
        KSI(I,I)=KSI(I,I)-(UN-ALPHA)*FD
 120  CONTINUE

      DO 130 I=1,6
        DO 131 J=1,6
        KSI(I,J)=KSI(I,J)+INTERG(I)*INTERD(J)
        PSI(I,J)=PSI(I,J)-INTERG(I)*INTERT(J)
     &                  +(UN-ALPHA)*DELTAB(I)*TDFDDE(J)
 131    CONTINUE
 130  CONTINUE

      CALL R8INIR(36,0.D0,IKSI,1)
      DO 140 I=1,6
        IKSI(I,I)=1.D0
 140  CONTINUE

      CALL MGAUSS('NFVP',KSI,IKSI,6,6,6,DET,IRET)

C-- ! ksi n est plus disponible

      CALL R8INIR(36,0.D0,MATB,1)
      CALL R8INIR(6,0.D0,MATD,1)

      DO 150 I=1,6
        MATD(I)=-INTERT(I)/(UN-ALPHA)/FD/TDFDDD
        DO 151 J=1,6
               DO 152 K=1,6
            MATB(I,J)=MATB(I,J)+IKSI(I,K)*PSI(K,J)
            MATD(I)=MATD(I)-INTERD(J)*IKSI(J,K)*PSI(K,I)
     &                   /(UN-ALPHA)/FD/TDFDDD
152          CONTINUE
151        CONTINUE
150    CONTINUE

       DO 201 I=1,6
         DO 202 J=1,6
           DSIDEP(I,J)=-TDFDDE(I)*MATD(J)
           DO 203 K=1,6
             DSIDEP(I,J)=DSIDEP(I,J)-TDFBDE(K,I)*MATB(K,J)
 203             CONTINUE
C             WRITE(6,*) 'tang(',I,',',J,')=',DSIDEP(I,J)
 202           CONTINUE
 201   CONTINUE

       ELSEIF ((FD.EQ.0.D0).AND.(NOFBM.NE.0.D0)) THEN

C 567     CONTINUE

         CALL R8INIR(36,0.D0,KSI,1)
         CALL R8INIR(36,0.D0,PSI,1)

         DO 500 I=1,6
           DO 501 J=1,6
             KSI(I,J)=-FBM(I)*FBM(J)/NOFBM
             PSI(I,J)=PSI(I,J)-FBM(I)*ALPHA*MULT/COUPL*DCRIT(J)
             DO 502 K=1,6
               KSI(I,J)=KSI(I,J)-ALPHA*MULT*DFBMDF(I,K)*TDFBDB(K,J)
               PSI(I,J)=PSI(I,J)+ALPHA*MULT*DFBMDF(I,K)*TDFBDE(K,J)
 502         CONTINUE
 501       CONTINUE
 500     CONTINUE

         DO 504 I=1,6
           KSI(I,I)=KSI(I,I)+1
 504     CONTINUE

         CALL R8INIR(36,0.D0,IKSI,1)
         DO 505 I=1,6
           IKSI(I,I)=1.D0
 505     CONTINUE

         CALL MGAUSS('NFVP',KSI,IKSI,6,6,6,DET,IRET)

         CALL R8INIR(36,0.D0,MATB,1)

         DO 550 I=1,6
           DO 551 J=1,6
                  DO 552 K=1,6
               MATB(I,J)=MATB(I,J)+IKSI(I,K)*PSI(K,J)
552               CONTINUE
551             CONTINUE
550      CONTINUE

         DO 561 I=1,6
           DO 562 J=1,6
             DO 563 K=1,6
              DSIDEP(I,J)=DSIDEP(I,J)-TDFBDE(K,I)*MATB(K,J)
 563               CONTINUE
C             WRITE(6,*) 'tang(',I,',',J,')=',DSIDEP(I,J)
 562             CONTINUE
 561     CONTINUE

        ELSEIF ((FD.NE.0.D0).AND.(NOFBM.EQ.0.D0)) THEN

C 568     CONTINUE

         DO 661 I=1,6
           DO 662 J=1,6
             DSIDEP(I,J)= -TDFDDE(I)*(-TDFDDE(J)+COUPL/(UN-ALPHA)
     &                      *DCRIT(J)/FD)/TDFDDD
C             WRITE(6,*) 'tang(',I,',',J,')=',DSIDEP(I,J)
 662             CONTINUE
 661     CONTINUE

      ENDIF

      END
