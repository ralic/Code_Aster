      SUBROUTINE MEOBG3 (EPS,EPSG,B,D,DELTAB,DELTAD,MULT,
     &                 LAMBDA,MU,ECROB,ECROD,ALPHA,K1,K2,DSIDEP)

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

      REAL*8            EPS(6),EPSG(6),B(6),D,DSIDEP(6,6)
      REAL*8            DELTAB(6),DELTAD,MULT
      REAL*8            LAMBDA,MU,ALPHA,K1,K2,ECROB,ECROD

C--CALCUL DE LA MATRICE TANGENTE POUR LA LOI ENDO_ORTHO_BETON
C  VERSION NON LOCALE
C
C
C
C
C-------------------------------------------------------------

      INTEGER           I,J,K,T(3,3),IRET
      REAL*8             RAC2,KRON(6),NOFBM,UN
      REAL*8             CC(6),VECC(3,3),VALCC(3),CCP(6),CPE(6)
      REAL*8             VECEPG(3,3),VALEPG(3),DET
      REAL*8             FB(6),FBM(6),VECFB(3,3),VALFB(3),TREB
      REAL*8             TREPSG,DCOEFD,ENE,FD,TREM
      REAL*8             DFBMDF(6,6),TDFBDB(6,6),TDFBDE(6,6)
      REAL*8             TDFDDE(6),TDFDDD,TOTO
      REAL*8             INTERD(6),INTERT(6),INTERG(6)
      REAL*8             PSI(6,6),KSI(6,6),IKSI(6,6)
      REAL*8             MATB(6,6),MATD(6)
      REAL*8             DSIGB(6,6),DSIGD(6)
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

      CALL R8INIR(36,0.D0,DSIDEP,1)


C-------------------------------------------------------
C-------------------------------------------------------
C----CALCUL DE FB: FORCE THERMO ASSOCIEE A
C-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION

      CALL R8INIR(6,0.D0,CC,1)

      DO 9 I=1,3
        DO 10 J=I,3
          DO 11 K=1,3
            CC(T(I,J))=CC(T(I,J))+B(T(I,K))*EPSG(T(K,J))+
     &                 B(T(J,K))*EPSG(T(K,I))
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
            CPE(T(I,J))=CPE(T(I,J))+ CCP(T(I,K))*EPSG(T(K,J))+
     &                    CCP(T(J,K))*EPSG(T(K,I))
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
          FB(I)=-LAMBDA*TREB*EPSG(I)
  19    CONTINUE
      ENDIF
      DO 20 I=1,6
        FB(I)=FB(I)-MU/2.D0*CPE(I)+ECROB*(KRON(I)-B(I))
  20  CONTINUE

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


        TREPSG=EPSG(1)+EPSG(2)+EPSG(3)
        CALL DIAGO3(EPSG,VECEPG,VALEPG)
        DO 22 I=1,3
          IF (VALEPG(I).GT.0.D0) THEN
            VALEPG(I)=0.D0
          ENDIF
 22     CONTINUE

        TREM=VALEPG(1)**2+VALEPG(2)**2+VALEPG(3)**2
        IF (TREPSG.GT.0.D0) THEN
          TREPSG=0.D0
        ENDIF
        DCOEFD=2.D0*(1.D0-D)
        ENE=LAMBDA/2*TREPSG**2+MU*TREM
        FD=DCOEFD*ENE-2.D0*D*ECROD

C---CALCUL DE DERIVEES UTILES----------------------------------

      CALL DFMDF(6,FB,DFBMDF)
      CALL DFBDB(3,B,EPSG,2.D0*MU,LAMBDA,ECROB,TDFBDB)
      CALL DFBDE(3,B,EPSG,2.D0*MU,LAMBDA,TDFBDE)
      CALL DFBDE(3,B,EPS,2.D0*MU,LAMBDA,DSIGB)

      CALL DFDDE(EPSG,D,3,LAMBDA,MU,TDFDDE)
      CALL DFDDE(EPS,D,3,LAMBDA,MU,DSIGD)
      CALL DFDDD(EPSG,D,3,LAMBDA,MU,ECROD,TDFDDD)



      DCRIT(1)=-K1*(-TREPSG/K2/(1.D0+(-TREPSG/K2)**2.D0)
     &           +ATAN2(-TREPSG/K2,UN))
      DCRIT(2)=-K1*(-TREPSG/K2/(1.D0+(-TREPSG/K2)**2.D0)
     &           +ATAN2(-TREPSG/K2,UN))
      DCRIT(3)=-K1*(-TREPSG/K2/(1.D0+(-TREPSG/K2)**2.D0)
     &           +ATAN2(-TREPSG/K2,UN))
      DCRIT(4)=0.D0
      DCRIT(5)=0.D0
      DCRIT(6)=0.D0





      DO 101 I=4,6
        FBM(I)=RAC2*FBM(I)
        DELTAB(I)=DELTAB(I)*RAC2
 101  CONTINUE

      NOFBM=FBM(1)**2+FBM(2)**2+FBM(3)**2+FBM(4)**2
     &        +FBM(5)**2+FBM(6)**2

      COUPL=SQRT(ALPHA*NOFBM+(1.D0-ALPHA)*FD**2.D0)

      IF ((FD.NE.0.D0).AND.(NOFBM.NE.0.D0)) THEN

C---CALCUL DE DBDE ET DDDE-------------------------------------

C---CALCUL DE KSI ET PSI

      CALL R8INIR(6,0.D0,INTERD,1)
      CALL R8INIR(6,0.D0,INTERG,1)
      CALL R8INIR(6,0.D0,INTERT,1)
      CALL R8INIR(36,0.D0,PSI,1)
      CALL R8INIR(36,0.D0,KSI,1)

      DO 110 I=1,6
        INTERG(I)=DELTAB(I)/FD-ALPHA*FBM(I)/(1.D0-ALPHA)/FD/TDFDDD
        INTERT(I)=(1.D0-ALPHA)*FD*TDFDDE(I)-COUPL*DCRIT(I)
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
        KSI(I,I)=KSI(I,I)-(1.D0-ALPHA)*FD
 120  CONTINUE

      DO 130 I=1,6
        DO 131 J=1,6
        KSI(I,J)=KSI(I,J)+INTERG(I)*INTERD(J)
        PSI(I,J)=PSI(I,J)-INTERG(I)*INTERT(J)
     &                  +(1.D0-ALPHA)*DELTAB(I)*TDFDDE(J)
 131    CONTINUE
 130  CONTINUE


      CALL R8INIR(36,0.D0,IKSI,1)
      DO 140 I=1,6
        IKSI(I,I)=1.D0
 140  CONTINUE

      CALL MGAUSS('NCVP',KSI,IKSI,6,6,6,DET,IRET)

      IF (IRET.NE.0) GOTO 999


C-- ! ksi n est plus disponible

      CALL R8INIR(36,0.D0,MATB,1)
      CALL R8INIR(6,0.D0,MATD,1)

      DO 150 I=1,6
        MATD(I)=-INTERT(I)/(1.D0-ALPHA)/FD/TDFDDD
        DO 151 J=1,6
               DO 152 K=1,6
            MATB(I,J)=MATB(I,J)+IKSI(I,K)*PSI(K,J)
            MATD(I)=MATD(I)-INTERD(J)*IKSI(J,K)*PSI(K,I)
     &                   /(1.D0-ALPHA)/FD/TDFDDD
152          CONTINUE
151        CONTINUE
150    CONTINUE



      CALL R8INIR(36,0.D0,DSIDEP,1)


       DO 201 I=1,6
         DO 202 J=1,6
           DSIDEP(I,J)=-DSIGD(I)*MATD(J)
           DO 203 K=1,6
             DSIDEP(I,J)=DSIDEP(I,J)-DSIGB(K,I)*MATB(K,J)
 203             CONTINUE
 202           CONTINUE
 201   CONTINUE



       ELSEIF ((FD.EQ.0.D0).AND.(NOFBM.NE.0.D0)) THEN

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

         CALL R8INIR(36,0.D0,DSIDEP,1)
         DO 561 I=1,6
           DO 562 J=1,6
             DO 563 K=1,6
              DSIDEP(I,J)=DSIDEP(I,J)-DSIGB(K,I)*MATB(K,J)
 563               CONTINUE
 562             CONTINUE
 561     CONTINUE

       ELSEIF ((FD.NE.0.D0).AND.(NOFBM.EQ.0.D0)) THEN

         CALL R8INIR(36,0.D0,DSIDEP,1)
         DO 661 I=1,6
           DO 662 J=1,6
              DSIDEP(I,J)= -DSIGD(I)*(-TDFDDE(J)+COUPL/(1.D0-ALPHA)
     &                      *DCRIT(J)/FD)/TDFDDD
 662             CONTINUE
 661     CONTINUE




      ENDIF

 999  CONTINUE

      END
