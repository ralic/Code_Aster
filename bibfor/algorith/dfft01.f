      SUBROUTINE DFFT01( NXTLT, NTHPO, LENGT, PI2, P7,
     &                   CR0, CR1, CR2, CR3, CR4, CR5, CR6, CR7,
     &                   CI0, CI1, CI2, CI3, CI4, CI5, CI6, CI7 )
C
C *********************************************************************
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 18/07/96   AUTEUR CIBHHLV L.VIVAN 
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
C TOLE  CRP_21
C *********************************************************************
C
C DESCRIPTION : ROUTINE D ITERATIONS EN BASE 8
C -----------
C
C
      IMPLICIT NONE
C
C ARGUMENTS
      INTEGER NXTLT, NTHPO, LENGT
      REAL*8 PI2, P7
      REAL*8 CR0(*),CR1(*),CR2(*),CR3(*)
      REAL*8 CR4(*),CR5(*),CR6(*),CR7(*)
      REAL*8 CI0(*),CI1(*),CI2(*),CI3(*)
      REAL*8 CI4(*),CI5(*),CI6(*),CI7(*)
C
C VARIABLES LOCALES
      INTEGER J, K
      REAL*8 SCALE, ARG, TR, TI
      REAL*8 C1,S1,C2,S2,C3,S3,C4,S4,C5,S5,C6,S6,C7,S7
      REAL*8 AR0,AR1,AR2,AR3,AR4,AR5,AR6,AR7
      REAL*8 AI0,AI1,AI2,AI3,AI4,AI5,AI6,AI7
      REAL*8 BR0,BR1,BR2,BR3,BR4,BR5,BR6,BR7
      REAL*8 BI0,BI1,BI2,BI3,BI4,BI5,BI6,BI7
C
C ******************   DEBUT DU CODE EXECUTABLE   **********************
C
      SCALE = PI2/DBLE(LENGT)
      DO 30 J=1,NXTLT
        ARG = DBLE(J-1)*SCALE
        C1 = COS(ARG)
        S1 = SIN(ARG)
        C2 = C1**2 - S1**2
        S2 = C1*S1 + C1*S1
        C3 = C1*C2 - S1*S2
        S3 = C2*S1 + S2*C1
        C4 = C2**2 - S2**2
        S4 = C2*S2 + C2*S2
        C5 = C2*C3 - S2*S3
        S5 = C3*S2 + S3*C2
        C6 = C3**2 - S3**2
        S6 = C3*S3 + C3*S3
        C7 = C3*C4 - S3*S4
        S7 = C4*S3 + S4*C3
        DO 20 K=J,NTHPO,LENGT
          AR0 = CR0(K) + CR4(K)
          AR1 = CR1(K) + CR5(K)
          AR2 = CR2(K) + CR6(K)
          AR3 = CR3(K) + CR7(K)
          AR4 = CR0(K) - CR4(K)
          AR5 = CR1(K) - CR5(K)
          AR6 = CR2(K) - CR6(K)
          AR7 = CR3(K) - CR7(K)
          AI0 = CI0(K) + CI4(K)
          AI1 = CI1(K) + CI5(K)
          AI2 = CI2(K) + CI6(K)
          AI3 = CI3(K) + CI7(K)
          AI4 = CI0(K) - CI4(K)
          AI5 = CI1(K) - CI5(K)
          AI6 = CI2(K) - CI6(K)
          AI7 = CI3(K) - CI7(K)
          BR0 = AR0 + AR2
          BR1 = AR1 + AR3
          BR2 = AR0 - AR2
          BR3 = AR1 - AR3
          BR4 = AR4 - AI6
          BR5 = AR5 - AI7
          BR6 = AR4 + AI6
          BR7 = AR5 + AI7
          BI0 = AI0 + AI2
          BI1 = AI1 + AI3
          BI2 = AI0 - AI2
          BI3 = AI1 - AI3
          BI4 = AI4 + AR6
          BI5 = AI5 + AR7
          BI6 = AI4 - AR6
          BI7 = AI5 - AR7
          CR0(K) = BR0 + BR1
          CI0(K) = BI0 + BI1
          IF (J.LE.1) GO TO 10
          CR1(K) = C4*(BR0-BR1) - S4*(BI0-BI1)
          CI1(K) = C4*(BI0-BI1) + S4*(BR0-BR1)
          CR2(K) = C2*(BR2-BI3) - S2*(BI2+BR3)
          CI2(K) = C2*(BI2+BR3) + S2*(BR2-BI3)
          CR3(K) = C6*(BR2+BI3) - S6*(BI2-BR3)
          CI3(K) = C6*(BI2-BR3) + S6*(BR2+BI3)
          TR = P7*(BR5-BI5)
          TI = P7*(BR5+BI5)
          CR4(K) = C1*(BR4+TR) - S1*(BI4+TI)
          CI4(K) = C1*(BI4+TI) + S1*(BR4+TR)
          CR5(K) = C5*(BR4-TR) - S5*(BI4-TI)
          CI5(K) = C5*(BI4-TI) + S5*(BR4-TR)
          TR = -P7*(BR7+BI7)
          TI = P7*(BR7-BI7)
          CR6(K) = C3*(BR6+TR) - S3*(BI6+TI)
          CI6(K) = C3*(BI6+TI) + S3*(BR6+TR)
          CR7(K) = C7*(BR6-TR) - S7*(BI6-TI)
          CI7(K) = C7*(BI6-TI) + S7*(BR6-TR)
          GO TO 20
  10      CONTINUE
          CR1(K) = BR0 - BR1
          CI1(K) = BI0 - BI1
          CR2(K) = BR2 - BI3
          CI2(K) = BI2 + BR3
          CR3(K) = BR2 + BI3
          CI3(K) = BI2 - BR3
          TR = P7*(BR5-BI5)
          TI = P7*(BR5+BI5)
          CR4(K) = BR4 + TR
          CI4(K) = BI4 + TI
          CR5(K) = BR4 - TR
          CI5(K) = BI4 - TI
          TR = -P7*(BR7+BI7)
          TI = P7*(BR7-BI7)
          CR6(K) = BR6 + TR
          CI6(K) = BI6 + TI
          CR7(K) = BR6 - TR
          CI7(K) = BI6 - TI
  20    CONTINUE
  30  CONTINUE
C
      END
