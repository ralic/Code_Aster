      SUBROUTINE FORCEN(RNORMC,INTSN,NB1,XI,XR,RHO,EPAIS,VOMEGA,VECL1,
     &                  XA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 03/07/2012   AUTEUR PELLET J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT NONE
C
      INTEGER INTSN,NB1,INTSX,IE(3,3,3)
      REAL*8 WGT,RHO
      REAL*8 XI(3,*),XR(*),VOMEGA(3),VECL1(42),XA(3)
C
C-----------------------------------------------------------------------
      INTEGER I ,I1 ,I2 ,IB ,IK ,IP ,IQ 
      INTEGER IR ,J ,JB ,K ,L1 
      REAL*8 EPAIS ,RNORMC 
C-----------------------------------------------------------------------
      WGT=XR(127-1+INTSN)
C
      DO 30 I=1,3
        DO 20 J=1,3
          DO 10 K=1,3
            IE(I,J,K)=0
   10     CONTINUE
   20   CONTINUE
   30 CONTINUE
      IE(1,2,3)=1
      IE(1,3,2)=-1
      IE(2,1,3)=-1
      IE(2,3,1)=1
      IE(3,1,2)=1
      IE(3,2,1)=-1
C
      L1=135
      INTSX=8*(INTSN-1)
C
      I1=L1+INTSX
C
      DO 90 IB=1,NB1
        I2=5*(IB-1)
        DO 80 IQ=1,3
          DO 70 IP=1,3
            DO 60 IK=1,3
              DO 50 IR=1,3
                DO 40 JB=1,NB1
                  VECL1(I2+1)=VECL1(I2+1)-WGT*RHO*EPAIS*RNORMC*
     &                        IE(IQ,IP,IK)*IE(1,IR,IQ)*VOMEGA(IR)*
     &                        VOMEGA(IP)*(XI(IK,JB)-XA(IK))*XR(I1+IB)*
     &                        XR(I1+JB)
C
                  VECL1(I2+2)=VECL1(I2+2)-WGT*RHO*EPAIS*RNORMC*
     &                        IE(IQ,IP,IK)*IE(2,IR,IQ)*VOMEGA(IR)*
     &                        VOMEGA(IP)*(XI(IK,JB)-XA(IK))*XR(I1+IB)*
     &                        XR(I1+JB)
C
                  VECL1(I2+3)=VECL1(I2+3)-WGT*RHO*EPAIS*RNORMC*
     &                        IE(IQ,IP,IK)*IE(3,IR,IQ)*VOMEGA(IR)*
     &                        VOMEGA(IP)*(XI(IK,JB)-XA(IK))*XR(I1+IB)*
     &                        XR(I1+JB)
   40           CONTINUE
   50         CONTINUE
   60       CONTINUE
   70     CONTINUE
   80   CONTINUE
   90 CONTINUE
      END
