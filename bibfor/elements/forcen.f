      SUBROUTINE FORCEN(RNORMC,INTSN,NB1,XI,XR,RHO,EPAIS,VOMEGA,VECL1)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 15/10/96   AUTEUR SABHHLA A.LAULUSA 
C ======================================================================
C COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
      IMPLICIT REAL*8 (A-H,O-Z)
C
      INTEGER INTSN,NB1,INTSX,IE(3,3,3)
      REAL*8 WGT,RHO,DETJ
      REAL*8 XI(3,*),XR(*),VOMEGA(3),VECL1(42)
C
      WGT=XR(127-1+INTSN)
C
      DO 2 I=1,3
      DO 3 J=1,3
      DO 4 K=1,3
         IE(I,J,K)=0
 4    CONTINUE
 3    CONTINUE
 2    CONTINUE
         IE(1,2,3)= 1
         IE(1,3,2)=-1
         IE(2,1,3)=-1
         IE(2,3,1)= 1
         IE(3,1,2)= 1
         IE(3,2,1)=-1
C
      L1=135
      INTSX=8*(INTSN-1)
C
      I1=L1+INTSX
C
      DO 10 IB=1,NB1
         I2=5*(IB-1)
      DO 20 IQ=1,3
      DO 30 IP=1,3
      DO 40 IK=1,3
      DO 50 IR=1,3
      DO 60 JB=1,NB1
         VECL1(I2+1)=VECL1(I2+1)-WGT*RHO*EPAIS*RNORMC
     &                              *IE(IQ,IP,IK)*IE(1,IR,IQ)
     &                              *VOMEGA(IR)*VOMEGA(IP)*XI(IK,JB)
     &                              *XR(I1+IB)*XR(I1+JB)
C
         VECL1(I2+2)=VECL1(I2+2)-WGT*RHO*EPAIS*RNORMC
     &                              *IE(IQ,IP,IK)*IE(2,IR,IQ)
     &                              *VOMEGA(IR)*VOMEGA(IP)*XI(IK,JB)
     &                              *XR(I1+IB)*XR(I1+JB)
C
         VECL1(I2+3)=VECL1(I2+3)-WGT*RHO*EPAIS*RNORMC
     &                              *IE(IQ,IP,IK)*IE(3,IR,IQ)
     &                              *VOMEGA(IR)*VOMEGA(IP)*XI(IK,JB)
     &                              *XR(I1+IB)*XR(I1+JB)
 60   CONTINUE
 50   CONTINUE
 40   CONTINUE
 30   CONTINUE
 20   CONTINUE
 10   CONTINUE
      END
