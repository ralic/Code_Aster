      SUBROUTINE HYP3DV(C11,C22,C33,C12,C13,C23,
     &                  K,
     &                  CVOL)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
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
      REAL*8 C11
      REAL*8 C22
      REAL*8 C33
      REAL*8 C12
      REAL*8 C13
      REAL*8 C23
      REAL*8 K
      REAL*8 CVOL(6,6)
C
C-----------------------------------------------------------------------
C
C     LOI DE COMPORTEMENT HYPERELASTIQUE - 3D
C     CALCUL DE LA MATRICE TANGENTE - PARTIE VOLUMIQUE
C
C IN  C11,C22,C33,C12,C13,C23: ELONGATIONS
C IN  K     : MODULE DE COMPRESSIBILITE
C OUT CVOL  : MATRICE TANGENTE VOLUMIQUE
C-----------------------------------------------------------------------
C
      REAL*8 DF(14)
      REAL*8 DFF0(14)
      REAL*8 DFF1(14)
      REAL*8 DFF2(14)
      REAL*8 DFF3(14)
      REAL*8 DFF4(14)
      REAL*8 GRD(6,6)
      REAL*8 T1,T3,T5,T7
      REAL*8 T10,T13,T15,T16,T17,T19
      REAL*8 T22,T24,T29,T34,T38,T43,T44,T49
      REAL*8 T50,T54,T64,T68,T78,T92,T96
      REAL*8 T106,T110,T120,T125
      REAL*8 T130,T132,T133,T137,T140
      REAL*8 T155,T160,T163,T178,T184,T199
      REAL*8 T204,T207,T212,T217,T224,T230,T245,T251
C
C-----------------------------------------------------------------------
C
      T1 = C11*C22
      T3 = C23**2
      T5 = C12**2
      T7 = C12*C13
      T10 = C13**2
      T13 = SQRT(T1*C33-C11*T3-T5*C33+2*T7*C23-T10*C22)
      IF ((T13.EQ.0.D0)) THEN
      CALL U2MESS('F','ALGORITH3_93')
      ENDIF
      T15 = K*(-1+T13)
      T16 = 1/T13
      T17 = T15*T16
      T19 = C22*C33-T3
      T22 = C11*C33-T10
      T24 = T1-T5
      T29 = -2*C12*C33+2*C13*C23
      T34 = 2*C12*C23-2*C13*C22
      T38 = -2*C11*C23+2*T7
      DF(8) = T15*T19/2
      DF(7) = T16*T19/2
      T43 = T13**2
      T44 = 1/T43
      DF(6) = -DF(8)*T44+DF(7)*K
      T49 = 1/T13
      T50 = DF(6)*T49
      DF(5) = -T50*C22/2
      DF(4) = T50*C23
      T54 = T50*C33/2
      DF(3) = -T54
      DF(2) = -T17/2-T50*C11/2
      DF(1) = T54
      DFF0(8) = T15*T22/2
      DFF0(7) = T16*T22/2
      DFF0(6) = -DFF0(8)*T44+DFF0(7)*K
      T64 = DFF0(6)*T49
      DFF0(5) = -T17/2-T64*C22/2
      DFF0(4) = T64*C23
      T68 = T64*C33/2
      DFF0(3) = -T68
      DFF0(2) = -T64*C11/2
      DFF0(1) = T68
      DFF1(8) = T15*T24/2
      DFF1(7) = T16*T24/2
      DFF1(6) = -DFF1(8)*T44+DFF1(7)*K
      T78 = DFF1(6)*T49
      DFF1(5) = -T78*C22/2
      DFF1(4) = T78*C23
      DFF1(3) = -T17/2-T78*C33/2
      DFF1(2) = -T78*C11/2
      DFF1(1) = -DFF1(3)
      DFF2(8) = T15*T29/2
      DFF2(7) = T16*T29/2
      DFF2(6) = -DFF2(8)*T44+DFF2(7)*K
      T92 = DFF2(6)*T49
      DFF2(5) = -T92*C22/2
      DFF2(4) = T92*C23
      T96 = T92*C33/2
      DFF2(3) = -T96
      DFF2(2) = -T92*C11/2
      DFF2(1) = T96
      DFF3(8) = T15*T34/2
      DFF3(7) = T16*T34/2
      DFF3(6) = -DFF3(8)*T44+DFF3(7)*K
      T106 = DFF3(6)*T49
      DFF3(5) = -T106*C22/2
      DFF3(4) = T106*C23
      T110 = T106*C33/2
      DFF3(3) = -T110
      DFF3(2) = -T106*C11/2
      DFF3(1) = T110
      DFF4(8) = T15*T38/2
      DFF4(7) = T16*T38/2
      DFF4(6) = -DFF4(8)*T44+DFF4(7)*K
      T120 = DFF4(6)*T49
      DFF4(5) = -T120*C22/2
      DFF4(4) = T17+T120*C23
      T125 = T120*C33/2
      DFF4(3) = -T125
      DFF4(2) = -T120*C11/2
      DFF4(1) = T125
      T130 = DF(1)
      GRD(1,1) = -T50*T3/2+T130*C22
      T132 = T17*C33
      T133 = T132/2
      GRD(1,2) = T133-T50*T10/2+T130*C11
      T137 = T17*C22
      GRD(1,3) = T137/2+T50*T24/2
      T140 = DF(4)
      GRD(1,4) = T140*C13+2*DF(3)*C12
      GRD(1,5) = 2*DF(5)*C13+T140*C12
      GRD(1,6) = T50*T7+2*DF(2)*C23
      T155 = DFF0(1)
      GRD(2,1) = T133-T64*T3/2+T155*C22
      GRD(2,2) = -T64*T10/2+T155*C11
      T160 = T17*C11
      GRD(2,3) = T160/2+T64*T24/2
      T163 = DFF0(4)
      GRD(2,4) = T163*C13+2*DFF0(3)*C12
      GRD(2,5) = 2*DFF0(5)*C13+T163*C12
      GRD(2,6) = T64*T7+2*DFF0(2)*C23
      T178 = DFF1(1)
      GRD(3,1) = -T78*T3/2+T178*C22
      GRD(3,2) = -T78*T10/2+T178*C11
      GRD(3,3) = T78*T24/2
      T184 = DFF1(4)
      GRD(3,4) = T184*C13+2*DFF1(3)*C12
      GRD(3,5) = 2*DFF1(5)*C13+T184*C12
      GRD(3,6) = T78*T7+2*DFF1(2)*C23
      T199 = DFF2(1)
      GRD(4,1) = -T92*T3/2+T199*C22
      GRD(4,2) = -T92*T10/2+T199*C11
      T204 = T17*C12
      GRD(4,3) = -T204+T92*T24/2
      T207 = DFF2(4)
      GRD(4,4) = -T132+T207*C13+2*DFF2(3)*C12
      T212 = T17*C23
      GRD(4,5) = T212+2*DFF2(5)*C13+T207*C12
      T217 = T17*C13
      GRD(4,6) = T217+T92*T7+2*DFF2(2)*C23
      T224 = DFF3(1)
      GRD(5,1) = -T106*T3/2+T224*C22
      GRD(5,2) = -T217-T106*T10/2+T224*C11
      GRD(5,3) = T106*T24/2
      T230 = DFF3(4)
      GRD(5,4) = T212+T230*C13+2*DFF3(3)*C12
      GRD(5,5) = -T137+2*DFF3(5)*C13+T230*C12
      GRD(5,6) = T204+T106*T7+2*DFF3(2)*C23
      T245 = DFF4(1)
      GRD(6,1) = -T212-T120*T3/2+T245*C22
      GRD(6,2) = -T120*T10/2+T245*C11
      GRD(6,3) = T120*T24/2
      T251 = DFF4(4)
      GRD(6,4) = T251*C13+2*DFF4(3)*C12
      GRD(6,5) = 2*DFF4(5)*C13+T251*C12
      GRD(6,6) = -T160+T120*T7+2*DFF4(2)*C23

      CVOL(1,1) = 4.D0*GRD(1,1)
      CVOL(1,2) = 4.D0*GRD(1,2)
      CVOL(1,3) = 4.D0*GRD(1,3)
      CVOL(1,4) = 4.D0*GRD(1,4)
      CVOL(1,5) = 4.D0*GRD(1,5)
      CVOL(1,6) = 4.D0*GRD(1,6)
      CVOL(2,1) = 4.D0*GRD(2,1)
      CVOL(2,2) = 4.D0*GRD(2,2)
      CVOL(2,3) = 4.D0*GRD(2,3)
      CVOL(2,4) = 4.D0*GRD(2,4)
      CVOL(2,5) = 4.D0*GRD(2,5)
      CVOL(2,6) = 4.D0*GRD(2,6)
      CVOL(3,1) = 4.D0*GRD(3,1)
      CVOL(3,2) = 4.D0*GRD(3,2)
      CVOL(3,3) = 4.D0*GRD(3,3)
      CVOL(3,4) = 4.D0*GRD(3,4)
      CVOL(3,5) = 4.D0*GRD(3,5)
      CVOL(3,6) = 4.D0*GRD(3,6)
      CVOL(4,1) = 4.D0*GRD(4,1)
      CVOL(4,2) = 4.D0*GRD(4,2)
      CVOL(4,3) = 4.D0*GRD(4,3)
      CVOL(4,4) = 4.D0*GRD(4,4)
      CVOL(4,5) = 4.D0*GRD(4,5)
      CVOL(4,6) = 4.D0*GRD(4,6)
      CVOL(5,1) = 4.D0*GRD(5,1)
      CVOL(5,2) = 4.D0*GRD(5,2)
      CVOL(5,3) = 4.D0*GRD(5,3)
      CVOL(5,4) = 4.D0*GRD(5,4)
      CVOL(5,5) = 4.D0*GRD(5,5)
      CVOL(5,6) = 4.D0*GRD(5,6)
      CVOL(6,1) = 4.D0*GRD(6,1)
      CVOL(6,2) = 4.D0*GRD(6,2)
      CVOL(6,3) = 4.D0*GRD(6,3)
      CVOL(6,4) = 4.D0*GRD(6,4)
      CVOL(6,5) = 4.D0*GRD(6,5)
      CVOL(6,6) = 4.D0*GRD(6,6)
      END
