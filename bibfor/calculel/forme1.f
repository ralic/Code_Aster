      SUBROUTINE FORME1(M,TYPEMA,W,NNO,DIME)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C TOLE CRP_20
C A_UTIL
C ----------------------------------------------------------------------
C         CALCUL DES DERIVEES PREMIERES DES FONCTIONS DE FORME
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8      M(*)        : POINT SUR MAILLE DE REFERENCE (X,[Y],[Z])
C CHARACTER*8 TYPEMA      : TYPE DE LA MAILLE
C       
C VARIABLES DE SORTIE
C REAL*8      W(DIME,NNO) : DERIVEES 1ERES DES FONCTIONS DE FORME EN M
C                           (DW1/DX, [DW1/DY], [DW1/DZ], DW2/DX ...)
C INTEGER     NNO         : NOMBRE DE NOEUDS
C INTEGER     DIME        : DIMENSION DE LA MAILLE
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE
      REAL*8      ERPYRA
      PARAMETER   (ERPYRA = 1.D-6)

C --- VARIABLES
      CHARACTER*8 TYPEMA
      REAL*8      M(*),W(*),X,Y,Z,A,B,C,D,E,F,G,H,I,J,K,L,N,O,R
      INTEGER     NNO,DIME

      IF (TYPEMA(1:3).EQ.'SEG') THEN
        DIME = 1
        IF (TYPEMA(4:4).EQ.'2') THEN
          W(1) = -0.5D0
          W(2) = 0.5D0
          NNO = 2
        ELSEIF (TYPEMA(4:4).EQ.'3') THEN
          X = M(1)
          W(1) = X - 0.5D0
          W(2) = X + 0.5D0 
          W(3) = -2.D0 * X
          NNO = 3
        ELSE
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:3).EQ.'TRI') THEN
        X = M(1)           
        Y = M(2)
        DIME = 2
        IF (TYPEMA(4:5).EQ.'A3') THEN
          W(1) = -1.D0
          W(2) = -1.D0
          W(3) = 1.D0              
          W(4) = 0.D0
          W(5) = 0.D0
          W(6) = 1.D0
          NNO = 3 
        ELSEIF (TYPEMA(4:5).EQ.'A6') THEN
          X = 4.D0 * M(1)
          Y = 4.D0 * M(2)
          A = 4.D0 - X - Y
          W(1) = 1.D0 - A 
          W(2) = W(1)
          W(3) = X - 1.D0
          W(4) = 0.D0
          W(5) = 0.D0
          W(6) = Y - 1.D0
          W(7) = A - X
          W(8) = -X
          W(9) = Y
          W(10) = X
          W(11) = -Y
          W(12) = A - Y
          NNO = 6
        ELSEIF (TYPEMA(4:5).EQ.'A7') THEN
          X = 4.D0 * M(1) 
          Y = 4.D0 * M(2)
          A = 4.D0 - X - Y
          B = 0.1875D0*Y*(A-X)
          C = 0.1875D0*X*(A-Y)
          W(1) = B + 1.D0 - A
          W(2) = C + 1.D0 - A
          W(3) = B + X - 1.D0
          W(4) = C
          W(5) = B
          W(6) = C + Y - 1.D0
          B = -4.D0 * B
          C = -4.D0 * C
          W(7) = B + A - X
          W(8) = C - X
          W(9) = B + Y
          W(10) = C + X
          W(11) = B - Y
          W(12) = C + A - Y
          W(13) =-2.25D0*B
          W(14) =-2.25D0*C
          NNO = 7
        ELSEIF (TYPEMA(4:5).EQ.'W6') THEN
          X = M(1)
          Y = M(2)
          R = 2.8284271247461901D0
          A = 0.5D0 - X - Y
          X = 2.D0*X
          Y = 2.D0*Y
          W(1) = A
          W(2) = A + Y
          W(3) = Y - 1.D0 
          W(4) = X - 1.D0
          W(5) = A + X
          W(6) = A
          W(7) = 2.D0*X - 2.D0
          W(8) = 0.D0
          W(9) = 0.D0
          W(10) = 2.D0*Y - 2.D0
          W(11) = -R*A
          W(12) = W(11)
          NNO = 6
        ELSEIF (TYPEMA(4:5).EQ.'H3') THEN
          W(1) = -2.D0
          W(2) = -2.D0
          W(3) = 2.D0
          W(4) = 0.D0
          W(5) = 0.D0
          W(6) = 2.D0
          NNO = 3
        ELSE
          GOTO 10
        ENDIF
    
      ELSEIF (TYPEMA(1:3).EQ.'QUA') THEN
        X = M(1)
        Y = M(2)
        DIME = 2
        IF (TYPEMA(4:5).EQ.'D4') THEN
          A = 0.25D0 * (1.D0 - X)
          B = 0.25D0 * (1.D0 - Y)
          C = 0.25D0 * (1.D0 + X)
          D = 0.25D0 * (1.D0 + Y)
          W(1) = -B 
          W(2) = -A
          W(3) = B
          W(4) = -C
          W(5) = D
          W(6) = C
          W(7) = -D
          W(8) = A
          NNO = 4
        ELSEIF (TYPEMA(4:5).EQ.'D6') THEN
          A = 0.25D0*X*(X - 1.D0)
          B = 0.5D0*(1.D0 - Y)
          C = A + 0.5D0*X
          D = B + Y
          E = 0.5D0*(1.D0 - X*X)
          F = X*Y
          W(1) = B*(X-0.5D0)
          W(2) =-A
          W(3) = B*(X+0.5D0)
          W(4) =-C
          W(5) = D*(X+0.5D0)
          W(6) = C
          W(7) = D*(X-0.5D0)
          W(8) = A
          W(9) = F-X
          W(10) =-E
          W(11) =-F-X
          W(12) = E
          NNO = 6
        ELSEIF (TYPEMA(4:5).EQ.'D8') THEN
          A = 0.5D0 * (1.D0 - X)
          B = 0.5D0 * (1.D0 - Y)
          C = A + X
          D = B + Y   
          F = 0.5D0*(1.D0 - X*X)
          G = 0.5D0*(1.D0 - Y*Y)
          H = 0.5D0*X
          I = 0.5D0*Y
          W(1) = B*(X + I)
          W(2) = A*(Y + H)
          W(3) = B*(X - I)
          W(4) = C*(Y - H)
          W(5) = D*(X + I)
          W(6) = C*(Y + H)
          W(7) = D*(X - I)
          W(8) = A*(Y - H)
          X = -2.D0*X
          Y = -2.D0*Y
          W(9) = X*B
          W(10) = -F
          W(11) = G
          W(12) = Y*C
          W(13) = X*D
          W(14) = F
          W(15) = -G
          W(16) = Y*A
          NNO = 8
        ELSEIF (TYPEMA(4:5).EQ.'D9') THEN
          A = 0.5D0*X*(X-1.D0)
          B = 0.5D0*Y*(Y-1.D0)
          C = A + X
          D = B + Y
          E = 1.D0-X*X
          F = 1.D0-Y*Y
          G = -2.D0*X
          H = -2.D0*Y
          W(1) = B*(X-0.5D0)
          W(2) = A*(Y-0.5D0)
          W(3) = B*(X+0.5D0)
          W(4) = C*(Y-0.5D0)
          W(5) = D*(X+0.5D0)
          W(6) = C*(Y+0.5D0)
          W(7) = D*(X-0.5D0)
          W(8) = A*(Y+0.5D0)
          W(9) = G*B
          W(10) = E*(Y-0.5D0)
          W(11) = F*(X+0.5D0)
          W(12) = H*C
          W(13) = G*D
          W(14) = E*(Y+0.5D0)
          W(15) = F*(X-0.5D0)
          W(16) = H*A
          W(17) = G*F
          W(18) = H*E
          NNO = 9
        ELSEIF (TYPEMA(4:5).EQ.'H4') THEN 
          R = 0.4330127018922193D0
          X = X * 0.75D0
          Y = Y * 0.75D0
          A = R - X
          B = R - Y
          C = R + X
          D = R + Y
          W(1) = -B
          W(2) = -A
          W(3) = B
          W(4) = -C
          W(5) = D
          W(6) = C
          W(7) = -D
          W(8) = A
          NNO = 4
        ELSE
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        DIME = 3
        IF (TYPEMA(6:6).EQ.'4') THEN
          W(1) = 0.D0
          W(2) = 1.D0
          W(3) = 0.D0
          W(4) = 0.D0
          W(5) = 0.D0
          W(6) = 1.D0
          W(7) = -1.D0
          W(8) = -1.D0
          W(9) = -1.D0
          W(10) = 1.D0
          W(11) = 0.D0
          W(12) = 0.D0
          NNO = 4
        ELSEIF (TYPEMA(6:7).EQ.'10') THEN
          X = 4.D0*M(1)
          Y = 4.D0*M(2)
          Z = 4.D0*M(3)
          A = 4.D0 - X - Y - Z 
          W(1) = 0.D0
          W(2) = Y - 1.D0
          W(3) = 0.D0 
          W(4) = 0.D0
          W(5) = 0.D0
          W(6) = Z - 1.D0
          W(7) = 1.D0 - A
          W(8) = 1.D0 - A
          W(9) = 1.D0 - A
          W(10) = X - 1.D0
          W(11) = 0.D0
          W(12) = 0.D0
          W(13) = 0.D0
          W(14) = Z
          W(15) = Y
          W(16) = -Z
          W(17) = -Z
          W(18) = A - Z
          W(19) = -Y
          W(20) = A - Y
          W(21) = -Y
          W(22) = Y
          W(23) = X
          W(24) = 0.D0
          W(25) = Z
          W(26) = 0.D0
          W(27) = X
          W(28) = A - X
          W(29) = -X
          W(30) = -X
          NNO = 10
        ELSE
          GOTO 10
        ENDIF
 
      ELSEIF (TYPEMA(1:4).EQ.'PENT') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        DIME = 3
        IF (TYPEMA(5:6).EQ.'A6') THEN
          X = 0.5D0 * X
          Y = 0.5D0 * Y
          Z = 0.5D0 * Z
          A = 0.5D0 - Y - Z
          B = 0.5D0 - X
          C = 0.5D0 + X
          W(1) = -Y 
          W(2) = B
          W(3) = 0.D0 
          W(4) = -Z
          W(5) = 0.D0
          W(6) = B
          W(7) = -A
          W(8) = -B
          W(9) = -B
          W(10) = Y
          W(11) = C
          W(12) = 0.D0
          W(13) = Z
          W(14) = 0.D0
          W(15) = C
          W(16) = A
          W(17) = -C
          W(18) = -C
          NNO = 6
        ELSEIF (TYPEMA(5:7).EQ.'A12') THEN
          A = 1.D0 - Y - Z
          B = 0.5D0*(1.D0 - X)
          C = B + X
          D = 4.D0*A*B
          E = 4.D0*Y*B
          F = 4.D0*Z*B
          G = 4.D0*A*C
          H = 4.D0*Y*C
          I = 4.D0*Z*C 
          W(1) = Y*(0.5D0 - Y)
          W(2) = E - B
          W(3) = 0.D0
          W(4) = Z*(0.5D0 - Z)
          W(5) = 0.D0
          W(6) = F - B
          W(7) = A*(0.5D0 - A)
          W(8) = B - D
          W(9) = W(8)
          W(10) =-W(1)
          W(11) = H - C
          W(12) = 0.D0
          W(13) =-W(4)
          W(14) = 0.D0
          W(15) = I - C
          W(16) =-W(7)
          W(17) = C - G
          W(18) = W(17)
          W(19) =-2.D0*Y*Z
          W(20) = F
          W(21) = E
          W(22) =-2.D0*Z*A
          W(23) =-F
          W(24) = D - F
          W(25) =-2.D0*A*Y
          W(26) = D - E
          W(27) =-E
          W(28) =-W(19)
          W(29) = I
          W(30) = H
          W(31) =-W(22)
          W(32) =-I
          W(33) = G - I
          W(34) =-W(25)
          W(35) = G - H
          W(36) =-H
          NNO = 12     
        ELSEIF (TYPEMA(5:7).EQ.'A14') THEN
          A = 1.D0 - Y - Z
          B = 0.5D0*(1.D0 - X)
          C = B + X
          D = 4.D0*A*B
          E = 4.D0*Y*B
          F = 4.D0*Z*B
          G = 4.D0*A*C
          H = 4.D0*Y*C
          I = 4.D0*Z*C 
          J = 1.5D0*A*Y*Z
          N = 3.D0*Z*(A - Y)
          O = 3.D0*Y*(A - Z)
          K = N*B
          L = O*B
          N = N*C
          O = O*C
          W(1) = Y*(0.5D0 - Y) - J
          W(2) = K + E - B
          W(3) = L
          W(4) = Z*(0.5D0 - Z) - J
          W(5) = K
          W(6) = L + F - B
          W(7) = A*(0.5D0 - A) - J
          W(8) = K + B - D
          W(9) = L + B - D
          W(10) =-W(1)
          W(11) = N + H - C
          W(12) = O
          W(13) =-W(4)
          W(14) = N
          W(15) = O + I - C
          W(16) =-W(7)
          W(17) = N + C - G
          W(18) = O + C - G
          J = 4.D0 * J
          K =-4.D0 * K
          L =-4.D0 * L
          N =-4.D0 * N
          O =-4.D0 * O
          W(19) =-2.D0*Y*Z + J
          W(20) = K + F 
          W(21) = L + E
          W(22) =-2.D0*Z*A + J
          W(23) = K - F
          W(24) = L + D - F
          W(25) =-2.D0*A*Y + J
          W(26) = K + D - E
          W(27) = L - E
          W(28) =-W(19)
          W(29) = N + I
          W(30) = O + H
          W(31) =-W(22)
          W(32) = N - I
          W(33) = O + G - I
          W(34) =-W(25)
          W(35) = N + G - H
          W(36) = O - H
          W(37) =-2.25D0*J 
          W(38) =-2.25D0*K
          W(39) =-2.25D0*L
          W(40) =-W(37)
          W(41) =-2.25D0*N
          W(42) =-2.25D0*O
          NNO = 14
        ELSEIF (TYPEMA(5:7).EQ.'A15') THEN
          A = 1.D0 - Y - Z
          B = 0.5D0 *(1.D0 - X)
          C = B + X
          D = 1.D0 - X*X
          E = 4.D0*Y - 2.D0
          F = 4.D0*Z - 2.D0
          G = 2.D0 - 4.D0*A
          W(1) = Y*(X-Y+0.5D0)
          W(2) = B*(E-X)
          W(3) = 0.D0
          W(4) = Z*(X-Z+0.5D0)
          W(5) = 0.D0
          W(6) = B*(F-X)
          W(7) = A*(X-A+0.5D0)
          W(8) = B*(G+X)
          W(9) = W(8)
          W(10) = Y*(X+Y-0.5D0)
          W(11) = C*(E+X)
          W(12) = 0.D0
          W(13) = Z*(X+Z-0.5D0)
          W(14) = 0.D0
          W(15) = C*(F+X)
          W(16) = A*(X+A-0.5D0)
          W(17) = C*(G-X)
          W(18) = W(17)
          B = 4.D0*B
          C = 4.D0*C
          W(19) = -2.D0*Y*Z
          W(20) = Z*B
          W(21) = Y*B
          W(22) = -2.D0*A*Z
          W(23) = -Z*B
          W(24) = B*(A-Z)
          W(25) = -2.D0*Y*A
          W(26) = B*(A-Y)
          W(27) = -Y*B
          W(28) = -2.D0*X*Y
          W(29) = D
          W(30) = 0.D0
          W(31) = -2.D0*X*Z
          W(32) = 0.D0
          W(33) = D
          W(34) = -2.D0*X*A
          W(35) = -D
          W(36) = -D
          W(37) = 2.D0*Y*Z
          W(38) = Z*C
          W(39) = Y*C
          W(40) = 2.D0*Z*A
          W(41) = -Z*C
          W(42) = C*(A-Z)
          W(43) = 2.D0*Y*A
          W(44) = C*(A-Y)
          W(45) = -Y*C
          NNO = 15         
        ELSEIF (TYPEMA(5:6).EQ.'H6') THEN
          R = 1.7320508075688773D0
          X = R*X
          Y = R*Y
          Z = R*Z
          R = 0.2886751345948129D0
          A = X - R
          B = Y - R
          C = 1.D0 - Z
          D = 1.D0 + Z
          E = 0.86602540378443865D0 - A - B
          W(1) = -C
          W(2) = -C
          W(3) = -E
          W(4) = C
          W(5) = 0.D0
          W(6) = -A
          W(7) = 0.D0
          W(8) = C
          W(9) = -B
          W(10) = -D
          W(11) = -D
          W(12) = E
          W(13) = D
          W(14) = 0.D0
          W(15) = A
          W(16) = 0.D0
          W(17) = D
          W(18) = B
          NNO = 6
        ELSE
          GOTO 10
        ENDIF
         
      ELSEIF (TYPEMA(1:3).EQ.'HEX') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        DIME = 3     
        IF (TYPEMA(4:5).EQ.'A8') THEN         
          A = 0.125D0*(1.D0 - X)
          B = 0.125D0*(1.D0 - Y)
          C = 0.125D0*(1.D0 - Z)
          D = 0.125D0*(1.D0 + X)
          E = 0.125D0*(1.D0 + Y)
          F = 0.125D0*(1.D0 + Z)                             
          W(1) = B*(Z - 1.D0)
          W(2) = A*(Z - 1.D0)
          W(3) = A*(Y - 1.D0)
          W(4) = -W(1)
          W(5) = D*(Z - 1.D0)
          W(6) = D*(Y - 1.D0)
          W(7) = E*(1.D0 - Z)
          W(8) = -W(5)
          W(9) = -D*(1.D0 + Y)
          W(10) = -W(7)
          W(11) = -W(2)
          W(12) = -A*(1.D0 + Y)
          W(13) = -B*(1.D0 + Z)
          W(14) = -A*(1.D0 + Z)
          W(15) = -W(3)
          W(16) = -W(13)
          W(17) = -D*(1.D0 + Z)
          W(18) = -W(6)
          W(19) = E*(1.D0 + Z) 
          W(20) = -W(17)
          W(21) = -W(9)
          W(22) = -W(19)
          W(23) = -W(14)
          W(24) = -W(12)
          NNO = 8
        ELSEIF (TYPEMA(4:6).EQ.'A16') THEN
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*(1.D0 - Y)
          C = 0.5D0*(1.D0 - Z)
          D = A + X
          E = B + Y
          F = C + Z
          G = 0.5D0*(1.D0 - Y*Y)
          H = 0.5D0*(1.D0 - Z*Z)
          I = 0.5D0*Y
          J = 0.5D0*Z
          W(1) = B*C*( E + J )
          W(2) = A*C*( Y + J )
          W(3) = A*B*( Z + I )
          W(4) =-W(1)
          W(5) = D*C*( Y + J )
          W(6) = D*B*( Z + I )
          W(7) = E*C*(-B - J )
          W(8) = D*C*( Y - J )
          W(9) = D*E*( Z - I )
          W(10) =-W(7)
          W(11) = A*C*( Y - J ) 
          W(12) = A*E*( Z - I )
          W(13) = B*F*( E - J )
          W(14) = A*F*( Y - J )
          W(15) = A*B*( Z - I )
          W(16) =-W(13)
          W(17) = D*F*( Y - J )
          W(18) = D*B*( Z - I )
          W(19) = E*F*(-B + J )
          W(20) = D*F*( Y + J )
          W(21) = D*E*( Z + I )
          W(22) =-W(19) 
          W(23) = A*F*( Y + J ) 
          W(24) = A*E*( Z + I )
          Y = -2.D0*Y
          Z = -2.D0*Z
          W(25) = G*C
          W(26) = D*Y*C
          W(27) =-D*G
          W(28) =-W(25)
          W(29) = A*Y*C
          W(30) =-A*G
          W(31) =-B*H
          W(32) =-A*H
          W(33) = A*B*Z
          W(34) =-W(31)
          W(35) =-D*H
          W(36) = D*B*Z
          W(37) = E*H
          W(38) =-W(35)
          W(39) = D*E*Z
          W(40) =-W(37)
          W(41) =-W(32) 
          W(42) = A*E*Z
          W(43) = G*F 
          W(44) = D*Y*F
          W(45) =-W(27)
          W(46) =-W(43)
          W(47) = A*Y*F
          W(48) =-W(30)
          NNO = 16
        ELSEIF (TYPEMA(4:6).EQ.'A18') THEN
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*Y*(Y - 1.D0)
          C = 0.5D0*Z*(Z - 1.D0)
          D = A + X
          E = B + Y
          F = C + Z
          G = 0.5D0*(1.D0 - Y*Y)
          H = 0.5D0*(1.D0 - Z*Z)
          W(1) =-0.5D0*B*C
          W(2) = A*C*(Y-0.5D0)
          W(3) = A*B*(Z-0.5D0)
          W(4) =-W(1)
          W(5) = D*C*(Y-0.5D0)
          W(6) = D*B*(Z-0.5D0)
          W(7) = 0.5D0*E*C
          W(8) = D*C*(Y+0.5D0)
          W(9) = D*E*(Z-0.5D0)
          W(10) =-W(7)
          W(11) = A*C*(Y+0.5D0) 
          W(12) = A*E*(Z-0.5D0)
          W(13) =-0.5D0*B*F
          W(14) = A*F*(Y-0.5D0)
          W(15) = A*B*(Z+0.5D0)
          W(16) =-W(13)
          W(17) = D*F*(Y-0.5D0)
          W(18) = D*B*(Z+0.5D0)
          W(19) = 0.5D0*E*F
          W(20) = D*F*(Y+0.5D0)
          W(21) = D*E*(Z+0.5D0)
          W(22) =-W(19) 
          W(23) = A*F*(Y+0.5D0) 
          W(24) = A*E*(Z+0.5D0)
          Y = 2.D0*Y
          Z = 2.D0*Z
          W(25) = G*C
          W(26) =-D*Y*C 
          W(27) = D*G*(Z-1.D0)
          W(28) =-W(25)
          W(29) =-A*Y*C
          W(30) = A*G*(Z-1.D0)
          W(31) =-B*H
          W(32) = A*H*(Y-1.D0)
          W(33) =-A*B*Z
          W(34) =-W(31)
          W(35) = D*H*(Y-1.D0)
          W(36) =-D*B*Z
          W(37) = E*H
          W(38) = D*H*(Y+1.D0)
          W(39) =-D*E*Z
          W(40) =-W(37)
          W(41) = A*H*(Y+1.D0)
          W(42) =-A*E*Z
          W(43) = G*F
          W(44) =-D*Y*F
          W(45) = D*G*(Z+1.D0)
          W(46) =-W(43)
          W(47) =-A*Y*F
          W(48) = A*G*(Z+1.D0)
          Y = 2.D0*Y
          Z = 2.D0*Z
          W(49) = 2.D0*G*H
          W(50) =-D*Y*H
          W(51) =-D*G*Z
          W(52) =-W(49) 
          W(53) =-A*Y*H
          W(54) =-A*G*Z
          NNO = 18
        ELSEIF (TYPEMA(4:6).EQ.'A20') THEN
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*(1.D0 - Y)
          C = 0.5D0*(1.D0 - Z)
          D = A + X
          E = B + Y
          F = C + Z    
          G = (1.D0 - X*X) * 0.5D0
          H = (1.D0 - Y*Y) * 0.5D0
          I = (1.D0 - Z*Z) * 0.5D0
          W(1) = B*C*(X + 0.5D0*( Y + Z + 1.D0))
          W(2) = A*C*(Y + 0.5D0*( X + Z + 1.D0))
          W(3) = A*B*(Z + 0.5D0*( X + Y + 1.D0))
          W(4) = B*C*(X + 0.5D0*(-Y - Z - 1.D0))
          W(5) = D*C*(Y + 0.5D0*(-X + Z + 1.D0))
          W(6) = D*B*(Z + 0.5D0*(-X + Y + 1.D0))
          W(7) = E*C*(X + 0.5D0*( Y - Z - 1.D0))
          W(8) = D*C*(Y + 0.5D0*( X - Z - 1.D0))
          W(9) = D*E*(Z + 0.5D0*(-X - Y + 1.D0))
          W(10) = E*C*(X + 0.5D0*(-Y + Z + 1.D0))
          W(11) = A*C*(Y + 0.5D0*(-X - Z - 1.D0))
          W(12) = A*E*(Z + 0.5D0*( X - Y + 1.D0))
          W(13) = B*F*(X + 0.5D0*( Y - Z + 1.D0))
          W(14) = A*F*(Y + 0.5D0*( X - Z + 1.D0))
          W(15) = A*B*(Z + 0.5D0*(-X - Y - 1.D0))
          W(16) = B*F*(X + 0.5D0*(-Y + Z - 1.D0))
          W(17) = D*F*(Y + 0.5D0*(-X - Z + 1.D0))
          W(18) = D*B*(Z + 0.5D0*( X - Y - 1.D0))
          W(19) = E*F*(X + 0.5D0*( Y + Z - 1.D0))
          W(20) = D*F*(Y + 0.5D0*( X + Z - 1.D0))
          W(21) = D*E*(Z + 0.5D0*( X + Y - 1.D0))
          W(22) = E*F*(X + 0.5D0*(-Y - Z + 1.D0))
          W(23) = A*F*(Y + 0.5D0*(-X + Z - 1.D0))
          W(24) = A*E*(Z + 0.5D0*(-X + Y - 1.D0))
          X = -2.D0*X
          Y = -2.D0*Y
          Z = -2.D0*Z 
          W(25) = X*B*C
          W(26) = -G*C
          W(27) = -G*B
          W(28) = H*C
          W(29) = Y*D*C
          W(30) = -D*H
          W(31) = X*E*C
          W(32) = -W(26)
          W(33) = -G*E
          W(34) = -W(28)
          W(35) = Y*A*C
          W(36) = -A*H
          W(37) = -B*I
          W(38) = -A*I
          W(39) = Z*A*B
          W(40) = -W(37)
          W(41) = -D*I
          W(42) = Z*D*B
          W(43) = E*I
          W(44) = -W(41)
          W(45) = Z*D*E
          W(46) = -W(43)
          W(47) = -W(38)
          W(48) = Z*A*E
          W(49) = X*B*F
          W(50) = -G*F
          W(51) = -W(27)
          W(52) = H*F
          W(53) = Y*D*F
          W(54) = -W(30)
          W(55) = X*E*F
          W(56) = -W(50)
          W(57) = -W(33)
          W(58) = -W(52)
          W(59) = Y*A*F
          W(60) = -W(36)
          NNO = 20
        ELSEIF (TYPEMA(4:6).EQ.'A27') THEN
          A = 0.5D0*X*(X - 1.D0)
          B = 0.5D0*Y*(Y - 1.D0)
          C = 0.5D0*Z*(Z - 1.D0)
          D = A + X
          E = B + Y
          F = C + Z
          G = 1.D0 - X*X
          H = 1.D0 - Y*Y
          I = 1.D0 - Z*Z
          W(1) = B*C*(X-0.5D0)
          W(2) = A*C*(Y-0.5D0)
          W(3) = A*B*(Z-0.5D0)
          W(4) = B*C*(X+0.5D0)
          W(5) = D*C*(Y-0.5D0)
          W(6) = D*B*(Z-0.5D0)
          W(7) = E*C*(X+0.5D0)
          W(8) = D*C*(Y+0.5D0)
          W(9) = D*E*(Z-0.5D0)
          W(10) = E*C*(X-0.5D0)
          W(11) = A*C*(Y+0.5D0)
          W(12) = A*E*(Z-0.5D0)
          W(13) = B*F*(X-0.5D0)
          W(14) = A*F*(Y-0.5D0)
          W(15) = A*B*(Z+0.5D0)
          W(16) = B*F*(X+0.5D0)
          W(17) = D*F*(Y-0.5D0)
          W(18) = D*B*(Z+0.5D0)
          W(19) = E*F*(X+0.5D0)
          W(20) = D*F*(Y+0.5D0)
          W(21) = D*E*(Z+0.5D0)
          W(22) = E*F*(X-0.5D0)
          W(23) = A*F*(Y+0.5D0)
          W(24) = A*E*(Z+0.5D0)
          W(25) = -2.D0*X*B*C
          W(26) = G*C*(Y-0.5D0)
          W(27) = G*B*(Z-0.5D0)
          W(28) = H*C*(X+0.5D0)
          W(29) = -2.D0*Y*D*C
          W(30) = D*H*(Z-0.5D0)
          W(31) = -2.D0*X*E*C
          W(32) = G*C*(Y+0.5D0)
          W(33) = G*E*(Z-0.5D0)
          W(34) = H*C*(X-0.5D0)
          W(35) = -2.D0*Y*A*C
          W(36) = A*H*(Z-0.5D0)
          W(37) = B*I*(X-0.5D0)
          W(38) = A*I*(Y-0.5D0)
          W(39) = -2.D0*Z*A*B
          W(40) = B*I*(X+0.5D0)
          W(41) = D*I*(Y-0.5D0)
          W(42) = -2.D0*Z*D*B
          W(43) = E*I*(X+0.5D0)
          W(44) = D*I*(Y+0.5D0)
          W(45) = -2.D0*Z*D*E
          W(46) = E*I*(X-0.5D0)
          W(47) = A*I*(Y+0.5D0)
          W(48) = -2.D0*Z*A*E
          W(49) = -2.D0*X*B*F
          W(50) = G*F*(Y-0.5D0)
          W(51) = G*B*(Z+0.5D0)
          W(52) = H*F*(X+0.5D0)
          W(53) = -2.D0*Y*D*F
          W(54) = D*H*(Z+0.5D0)
          W(55) = -2.D0*X*E*F
          W(56) = G*F*(Y+0.5D0)
          W(57) = G*E*(Z+0.5D0)
          W(58) = H*F*(X-0.5D0)
          W(59) = -2.D0*Y*A*F
          W(60) = A*H*(Z+0.5D0)
          W(61) = -2.D0*X*H*C
          W(62) = -2.D0*Y*G*C
          W(63) = G*H*(Z-0.5D0)
          W(64) = -2.D0*X*B*I
          W(65) = G*I*(Y-0.5D0)
          W(66) = -2.D0*Z*G*B
          W(67) = H*I*(X+0.5D0)
          W(68) = -2.D0*Y*D*I
          W(69) = -2.D0*Z*D*H
          W(70) = -2.D0*X*E*I
          W(71) = G*I*(Y+0.5D0) 
          W(72) = -2.D0*Z*G*E
          W(73) = H*I*(X-0.5D0)
          W(74) = -2.D0*Y*A*I
          W(75) = -2.D0*Z*A*H
          W(76) = -2.D0*X*H*F
          W(77) = -2.D0*Y*G*F
          W(78) = G*H*(Z+0.5D0)
          W(79) = -2.D0*X*H*I
          W(80) = -2.D0*Y*G*I
          W(81) = -2.D0*Z*G*H
          NNO = 27
       ELSEIF (TYPEMA(5:6).EQ.'H8') THEN
          R = 0.2165063509461097D0
          A = R - 0.375D0 * X
          B = R - 0.375D0 * Y
          C = R - 0.375D0 * Z
          D = R + 0.375D0 * X
          E = R + 0.375D0 * Y
          F = R + 0.375D0 * Z
          R = 1.7320508075688773D0
          X = X * R
          Y = Y * R
          Z = Z * R
          W(1) = B*(Z - 1.D0)
          W(2) = A*(Z - 1.D0)
          W(3) = A*(Y - 1.D0)
          W(4) = -W(1)
          W(5) = D*(Z - 1.D0)
          W(6) = D*(Y - 1.D0)
          W(7) = E*(1.D0 - Z)
          W(8) = -W(5)
          W(9) = -D*(1.D0 + Y)
          W(10) = -W(7)
          W(11) = -W(2)
          W(12) = -A*(1.D0 + Y)
          W(13) = -B*(1.D0 + Z)
          W(14) = -A*(1.D0 + Z)
          W(15) = -W(3)
          W(16) = -W(13)
          W(17) = -D*(1.D0 + Z)
          W(18) = -W(6)
          W(19) = E*(1.D0 + Z)
          W(20) = -W(17)
          W(21) = -W(9)
          W(22) = -W(19)
          W(23) = -W(14)
          W(24) = -W(12)
          NNO = 8
        ELSE 
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'PYRAM') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        DIME = 3         
        IF (TYPEMA(6:6).EQ.'5') THEN 
          IF (ABS(Z - 1.D0) .LT. ERPYRA) THEN
C           ATTENTION : NON DIFFERENTIABLE EN (0,0,1) 
            W(1) = 0.5D0
            W(2) = 0.D0
            W(3) = -0.5D0
            W(4) = 0.D0
            W(5) = 0.5D0
            W(6) = -0.5D0
            W(7) = -0.5D0
            W(8) = 0.D0
            W(9) = -0.5D0
            W(10) = 0.D0
            W(11) = -0.5D0
            W(12) = -0.5D0
          ELSE
            E = 2.D0*(1.D0 - M(3))
            X = M(1)/E
            Y = M(2)/E
            F = X*X - Y*Y
            W(1) = 0.5D0 + X
            W(2) = -Y
            W(3) = -0.25D0 + F
            W(4) = -X
            W(5) = 0.5D0 + Y
            W(6) = -0.25D0 - F
            W(7) = -0.5D0 + X
            W(8) = -Y
            W(9) = -0.25D0 + F
            W(10) = -X
            W(11) = -0.5D0 + Y
            W(12) = -0.25D0 - F
          ENDIF
            W(13) = 0.D0
            W(14) = 0.D0
            W(15) = 1.D0
            NNO = 5
        ELSEIF (TYPEMA(6:7).EQ.'13') THEN
          IF (ABS(Z - 1.D0) .LT. ERPYRA) THEN
C           ATTENTION : NON DIFFERENTIABLE EN (0,0,1) 
            W(1) = -0.5D0
            W(2) = 0.D0
            W(3) = 0.25D0
            W(4) = 0.D0
            W(5) = -0.5D0
            W(6) = 0.25D0
            W(7) = 0.5D0
            W(8) = 0.D0
            W(9) = 0.25D0
            W(10) = 0.D0
            W(11) = 0.5D0
            W(12) = 0.25D0
            W(13) = 0.D0
            W(14) = 0.D0
            W(15) = 3.D0
            W(16) = 0.D0
            W(17) = 0.D0
            W(18) = 0.D0
            W(19) = 0.D0
            W(20) = 0.D0
            W(21) = 0.D0
            W(22) = 0.D0
            W(23) = 0.D0
            W(24) = 0.D0
            W(25) = 2.D0
            W(26) = 0.D0
            W(27) = 0.D0
            W(28) = 0.D0
            W(29) = 2.D0
            W(30) = -1.D0
            W(31) = -2.D0
            W(32) = 0.D0
            W(33) = -1.D0
            W(34) = 0.D0
            W(35) = -2.D0
            W(36) = -1.D0
            W(37) = 0.D0
            W(38) = 0.D0
            W(39) = -1.D0
          ELSE
            R = 1.D0 - Z
            E = 1.D0 / R
            A = (1.5D0 - 2.D0*Z)*E
            B = 0.5D0*(3.D0*X*X - Y*Y - Z*R)*E
            C = 0.5D0*(3.D0*Y*Y - X*X - Z*R)*E                   
            F = 0.5D0*(X*X - Y*Y)*E*E
            H = 0.5D0*(X+Y)*((X-Y)*(X-Y)*E*E+1.D0)
            I = 0.5D0*(X-Y)*((X+Y)*(X+Y)*E*E+1.D0)
            W(1) = X*A + B
            W(2) = Y*(0.5D0 - X)*E
            W(3) = (0.5D0 - F)*(0.5D0 - X)
            W(4) = X*(0.5D0 - Y)*E
            W(5) = Y*A + C
            W(6) = (0.5D0 + F)*(0.5D0 - Y)
            W(7) = X*A - B
            W(8) = Y*(0.5D0 + X)*E
            W(9) = (0.5D0 - F)*(0.5D0 + X)
            W(10) = X*(0.5D0 + Y)*E
            W(11) = Y*A - C
            W(12) = (0.5D0 + F)*(0.5D0 + Y)
            W(13) = 0.D0
            W(14) = 0.D0
            W(15) = 4.D0*Z - 1.D0
            A = -X + Y + Z - 1.D0
            B = -X - Y + Z - 1.D0
            C =  X - Y + Z - 1.D0  
            D =  X + Y + Z - 1.D0
            G = 0.5D0 * B * B
            W(16) = (G + X*(A+B))*E
            W(17) = (G + Y*(B+C))*E
            W(18) = - R - H
            G = 0.5D0 * C * C
            W(19) = (-G + X*(C+D))*E
            W(20) = (G + Y*(B+C))*E
            W(21) = -R + I
            G = 0.5D0 * D * D
            W(22) = (-G + X*(C+D))*E
            W(23) = (-G + Y*(A+D))*E
            W(24) = -R + H
            G = 0.5D0 * A * A
            W(25) = (G + X*(A+B))*E
            W(26) = (-G + Y*(A+D))*E
            W(27) = -R - I
            E = 2.D0 * Z * E
            F = 2.D0 * F
            W(28) = (X+R)*E 
            W(29) = -Y*E
            W(30) = 1.D0 - 2.D0*(Z-X) + F 
            W(31) = -X*E
            W(32) = (Y+R)*E
            W(33) = 1.D0 - 2.D0*(Z-Y) - F
            W(34) = (X-R)*E
            W(35) = -Y*E
            W(36) = 1.D0 - 2.D0*(Z+X) + F
            W(37) = -X*E
            W(38) = (Y-R)*E
            W(39) = 1.D0 - 2.D0*(Z+Y) - F
          ENDIF
          NNO = 13
        ELSE
          GOTO 10
        ENDIF

      ELSE
        GOTO 10       
      ENDIF

      GOTO 20

 10   CONTINUE

      CALL UTMESS('F','FORME1','MAILLE '//TYPEMA//' INDISPONIBLE')
      
 20   CONTINUE

      END
