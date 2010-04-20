      SUBROUTINE SIGEOB(EPS,BT,ENDO,NDIM,LAMBDA,MU,SIGM)
C
      IMPLICIT NONE
      INTEGER           NDIM
      REAL*8            EPS(6), BT(6), LAMBDA, MU
      REAL*8            SIGM(6),ENDO


C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 19/04/2010   AUTEUR IDOUX L.IDOUX 
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


C----------------------------------------------------------------------
C     CALCUL DE LA CONTRAINTE POUR LA LOI DE COMPORTEMENT 
C     ENDO_ORTH_BETON
C
C     IN  EPS      : DEFORMATION
C     IN  B        : TENSEUR D ENDOMMAGEMENT DE TRACTION
C     IN  ENDO     : ENDOMMAGEMENT SCALAIRE DE COMPRESSION
C     IN  NDIM     : DIMENSION 3(3D) OU 2(2D)
C     IN  LAMBDA MU: COEFFICIENT DE LAME
C     OUT SIGM     : CONTRAINTE
C----------------------------------------------------------------------

      REAL*8            RAC2, R8PREM,DEUX,UN
      REAL*8            TREB,TREPS,BE(6),BEEB(6),B(6)
      REAL*8            TO(6),TU(6),VP(3),VPE(3)
      REAL*8            VALBE(3),VECBE(3,3)
      REAL*8            VALEPS(3),VECEPS(3,3),PHID
      REAL*8            VECB(3,3),VB(3),VALB(3)
      INTEGER           I,J,K,T(3,3)

      
      T(1,1)=1
      T(1,2)=4
      T(1,3)=5
      T(2,1)=4
      T(2,2)=2
      T(2,3)=6
      T(3,1)=5
      T(3,2)=6
      T(3,3)=3
      DEUX=2.D0
      RAC2=SQRT(DEUX)
      DEUX=2.D0
      UN=1.D0

      PHID = (UN-ENDO)**DEUX

C      CALL DIAGO3(BT,VECB,VALB)
C      CALL R8INIR(3,0.D0,VB,1)
C      DO 32 I=1,NDIM
C          VB(I)=VALB(I)
C 32   CONTINUE
C
      CALL R8INIR(6,0.D0,B,1)
C      DO 33 I=1,NDIM
C        DO 34 J=I,NDIM
C          DO 35 K=1,NDIM
C            B(T(I,J))=B(T(I,J))+VECB(I,K)*VB(K)*VECB(J,K)
C 35       CONTINUE
C 34     CONTINUE
C 33   CONTINUE
      DO 32 I=1,6
        B(I)=BT(I)
 32   CONTINUE
      CALL R8INIR(6,0.D0,SIGM,1)
      CALL R8INIR(6,0.D0,BE,1)
      DO 1 I=1,NDIM
        DO 2 J=I,NDIM
          DO 3 K=1,NDIM
           BE(T(I,J))=BE(T(I,J))+B(T(I,K))*EPS(T(K,J))
 3        CONTINUE
 2      CONTINUE
 1    CONTINUE
 
      TREB=0.D0
      DO 4 I=1,NDIM
         TREB=TREB+BE(I)
 4    CONTINUE

      TREPS=0.D0
      DO 5 I=1,NDIM
         TREPS=TREPS+EPS(T(I,I))
 5    CONTINUE
      IF (TREB.GE.0.D0) THEN
        DO 6 I=1,NDIM
          DO 7 J=I,NDIM 
            SIGM(T(I,J))=SIGM(T(I,J))+LAMBDA*TREB*B(T(I,J))
 7        CONTINUE
 6      CONTINUE
      ENDIF
      IF (TREPS.LT.0.D0) THEN
        DO 8 I=1,NDIM 
          SIGM(T(I,I))=SIGM(T(I,I))+PHID*LAMBDA*TREPS
 8      CONTINUE
      ENDIF
      CALL R8INIR(6,0.D0,BEEB,1)
      DO 9 I=1,NDIM
        DO 10 J=I,NDIM
          DO 11 K=1,NDIM
           BEEB(T(I,J))=BEEB(T(I,J))+
     &          B(T(I,K))*EPS(T(K,J))+B(T(J,K))*EPS(T(K,I))
 11       CONTINUE
 10     CONTINUE
 9    CONTINUE

      CALL DIAGO3(BEEB,VECBE,VALBE)
      CALL R8INIR(3,0.D0,VP,1)
      DO 12 I=1,NDIM
        IF (VALBE(I).GT.0.D0) THEN
          VP(I)=VALBE(I)
        ELSE
          VP(I)=0.D0
        ENDIF
 12   CONTINUE

      CALL R8INIR(6,0.D0,TO,1)
      DO 13 I=1,NDIM
        DO 14 J=I,NDIM
          DO 15 K=1,NDIM
            TO(T(I,J))=TO(T(I,J))+VECBE(I,K)*VP(K)*VECBE(J,K)
 15       CONTINUE
 14     CONTINUE
 13   CONTINUE
 
      DO 16 I=1,NDIM
        DO 17 J=I,NDIM
          DO 18 K=1,NDIM
            SIGM(T(I,J))=SIGM(T(I,J))+MU/2*(TO(T(I,K))*B(T(K,J))+
     &                   TO(T(J,K))*B(T(K,I)))
  18      CONTINUE
  17    CONTINUE
  16  CONTINUE
      CALL DIAGO3(EPS,VECEPS,VALEPS)
      CALL R8INIR(3,0.D0,VPE,1)

      DO 19 I=1,NDIM
        IF (VALEPS(I).LT.0.D0) THEN
          VPE(I)=VALEPS(I)
        ELSE
          VPE(I)=0.D0
        ENDIF
 19   CONTINUE
 
      CALL R8INIR(6,0.D0,TU,1)
      DO 20 I=1,NDIM
        DO 21 J=I,NDIM
          DO 22 K=1,NDIM
            TU(T(I,J))=TU(T(I,J))+VECEPS(I,K)*VPE(K)*VECEPS(J,K)
 22       CONTINUE
 21     CONTINUE
 20   CONTINUE

      DO 23 I=1,NDIM
        DO 24 J=I,NDIM
            SIGM(T(I,J))=SIGM(T(I,J))+DEUX*MU*PHID*TU(T(I,J))
  24    CONTINUE
  23  CONTINUE


      SIGM(4)=RAC2*SIGM(4)
      SIGM(5)=RAC2*SIGM(5)
      SIGM(6)=RAC2*SIGM(6)

      
      END
