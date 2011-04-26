      SUBROUTINE VECTGT(IND,NB1,XI,KSI3S2,INTSX,ZR,EPAIS,VECTN,
     &                                                 VECTG,VECTT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 26/04/2011   AUTEUR COURTOIS M.COURTOIS 
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
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
C ======================================================================
C
      IMPLICIT REAL*8 (A-H,O-Z)
      INTEGER IND,NB1,INTSX
      REAL*8 XI(3,*),ZR(*),EPAIS,VECTN(9,3)
      REAL*8 VECTG(2,3),VECTT(3,3)
      REAL*8 KSI3S2
C
      IF  (IND.EQ.0) THEN
C
C     CALCULS AUX PTS D'INTEGRATION REDUITE
C
      L1= 12
      L2= 44
      L3= 76
      ELSE IF  (IND.EQ.1) THEN
C
C     CALCULS AUX PTS D'INTEGRATION NORMALE
C
      L1=135
      L2=207
      L3=279
C
      ENDIF
C
C     CONSTRUCTION DU VECTEUR N AUX X PTS DE GAUSS. X = REDUIT OU NORMAL
C     (STOCKE DANS VECTT)
C
         INTSX1=8*(INTSX-1)
         I1=L1+INTSX1
      DO 15 K=1,3
         VECTT(3,K)=0
      DO 25 J=1,NB1
         VECTT(3,K)=VECTT(3,K)+ZR(I1+J)*VECTN(J,K)
 25   CONTINUE
 15   CONTINUE
C
C     CONSTRUCTION DES VECTEURS GA AUX X PTS DE GAUSS
C
         I1=L2+INTSX1
         I2=L3+INTSX1
      DO 40 K=1,3
         VECTG(1,K)=0.D0
         VECTG(2,K)=0.D0
      DO 50 J=1,NB1
         VECTG(1,K)= VECTG(1,K)
     &                     +ZR(I1+J)*(XI(K,J)+KSI3S2*EPAIS*VECTN(J,K))
         VECTG(2,K)= VECTG(2,K)
     &                     +ZR(I2+J)*(XI(K,J)+KSI3S2*EPAIS*VECTN(J,K))
 50   CONTINUE
 40   CONTINUE
C
C     CONSTRUCTION DES VECTEURS TA AUX X PTS DE GAUSS (T3=N)
C
         RNORM=SQRT(VECTG(1,1)*VECTG(1,1)
     &             +VECTG(1,2)*VECTG(1,2)
     &             +VECTG(1,3)*VECTG(1,3))
C
      DO 60 K=1,3
         VECTT(1,K)=VECTG(1,K)/RNORM
 60   CONTINUE
C
         VECTT(2,1)= VECTT(3,2)*VECTT(1,3)-VECTT(3,3)*VECTT(1,2)
         VECTT(2,2)= VECTT(3,3)*VECTT(1,1)-VECTT(3,1)*VECTT(1,3)
         VECTT(2,3)= VECTT(3,1)*VECTT(1,2)-VECTT(3,2)*VECTT(1,1)
C
      END
