      SUBROUTINE FORME0(M,TYPEMA,W,NNO)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 02/04/2002   AUTEUR RATEAU G.RATEAU 
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
C                     CALCUL DES FONCTIONS DE FORME
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8       M(*)    : POINT SUR MAILLE DE REFERENCE (X,[Y],[Z])
C CHARACTER*8  TYPEMA  : TYPE DE LA MAILLE
C       
C VARIABLES DE SORTIE
C REAL*8       W(NNO)  : FONCTIONS DE FORME EN M (W1,W2,...)
C INTEGER      NNO     : NOMBRE DE NOEUDS 
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- PARAMETRE       
      REAL*8       ERPYRA
      PARAMETER    (ERPYRA = 1.D-6)

C --- VARIABLES
      CHARACTER*8  TYPEMA
      REAL*8       M(*),W(*),X,Y,Z,A,B,C,D,E,F,G,H,I,R
      INTEGER      NNO 

      IF (TYPEMA(1:3).EQ.'SEG') THEN
        X = M(1)
        IF (TYPEMA(4:4).EQ.'2') THEN
          W(1) = 0.5D0*(1.D0 - X)
          W(2) = W(1) + X
          NNO = 2
        ELSEIF (TYPEMA(4:4).EQ.'3') THEN
          W(1) = 0.5D0*X*(X - 1.D0)
          W(2) = W(1) + X 
          W(3) = 1.D0 - X*X
          NNO = 3
        ELSE
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:3).EQ.'TRI') THEN
        X = M(1)           
        Y = M(2)
        IF (TYPEMA(4:5).EQ.'A3') THEN
          W(1) = 1.D0 - X - Y
          W(2) = X
          W(3) = Y
          NNO = 3
        ELSEIF (TYPEMA(4:5).EQ.'A6') THEN
          A = 1.D0 - X - Y
          W(1) = A*(2.D0*A - 1.D0)
          W(2) = X*(2.D0*X - 1.D0)
          W(3) = Y*(2.D0*Y - 1.D0)
          W(4) = 4.D0*A*X
          W(5) = 4.D0*X*Y
          W(6) = 4.D0*Y*A
          NNO = 6
        ELSEIF (TYPEMA(4:5).EQ.'A7') THEN
          A = 1.D0 - X - Y
          B = 3.D0*A*X*Y
          W(1) = A*(2.D0*A-1.D0) + B
          W(2) = X*(2.D0*X-1.D0) + B
          W(3) = Y*(2.D0*Y-1.D0) + B
          W(4) = 4.D0*(A*X - B)
          W(5) = 4.D0*(X*Y - B)
          W(6) = 4.D0*(Y*A - B)
          W(7) = 9.D0*B
          NNO = 7
        ELSEIF (TYPEMA(4:5).EQ.'W6') THEN
          R = 2.8284271247461901D0
          A = 1.D0 - X - Y
          B = 0.5D0*A*(A - 1.D0)
          W(1) = Y*Y - B 
          W(2) = 2.D0*X*Y + A
          W(3) = X*X - B
          W(4) = 2.D0*X*(X - 1.D0)
          W(5) = 2.D0*Y*(Y - 1.D0)
          W(6) = R*B 
          NNO = 6
        ELSEIF (TYPEMA(4:5).EQ.'H3') THEN
          A = 2.D0*X - 1.D0/3.D0
          B = 2.D0*Y - 1.D0/3.D0
          W(1) = 1.D0 - A - B
          W(2) = A
          W(3) = B
          NNO = 3
        ELSE
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:3).EQ.'QUA') THEN
        X = M(1)
        Y = M(2)    
        IF (TYPEMA(4:5).EQ.'D4') THEN
          A = 0.5D0 * (1.D0 - X)
          B = 0.5D0 * (1.D0 - Y)
          C = A + X
          D = B + Y     
          W(1) = A*B
          W(2) = C*B
          W(3) = C*D
          W(4) = A*D
          NNO = 4
        ELSEIF (TYPEMA(4:5).EQ.'D6') THEN
          A = 0.5D0 * X * (X - 1.D0)
          B = 0.5D0 * (1.D0 - Y)
          C = A + X
          D = B + Y
          E = 1.D0 - X*X
          W(1) = A*B
          W(2) = C*B
          W(3) = C*D
          W(4) = A*D
          W(5) = E*B
          W(6) = E*D
          NNO = 6
        ELSEIF (TYPEMA(4:5).EQ.'D8') THEN
          A = 0.5D0 * (1.D0 - X)
          B = 0.5D0 * (1.D0 - Y)
          C = A + X
          D = B + Y 
          E = 1.D0 - X*X
          F = 1.D0 - Y*Y
          W(1) = A*B*(-1.D0 - X - Y)
          W(2) = C*B*(-1.D0 + X - Y)
          W(3) = C*D*(-1.D0 + X + Y)
          W(4) = A*D*(-1.D0 - X + Y)
          W(5) = E*B
          W(6) = F*C
          W(7) = E*D
          W(8) = F*A 
          NNO = 8
        ELSEIF (TYPEMA(4:5).EQ.'D9') THEN
          A = 0.5D0*X*(X-1.D0)
          B = 0.5D0*Y*(Y-1.D0)
          C = A + X
          D = B + Y
          E = 1.D0 - X*X
          F = 1.D0 - Y*Y
          W(1) = A*B
          W(2) = C*B
          W(3) = C*D
          W(4) = A*D
          W(5) = E*B
          W(6) = F*C
          W(7) = E*D
          W(8) = F*A
          W(9) = E*F
          NNO = 9
        ELSEIF (TYPEMA(4:5).EQ.'H4') THEN
          R = 0.86602540378443865D0
          X = X * R
          Y = Y * R
          A = 0.5D0 - X
          B = 0.5D0 - Y
          C = 0.5D0 + X
          D = 0.5D0 + Y
          W(1) = A*B
          W(2) = C*B
          W(3) = C*D
          W(4) = A*D
          NNO = 4
        ELSE
          GOTO 10            
        ENDIF
 
      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        IF (TYPEMA(6:6).EQ.'4') THEN
          W(1) = Y
          W(2) = Z
          W(3) = 1.D0 - X - Y - Z
          W(4) = X
          NNO = 4
        ELSEIF (TYPEMA(6:7).EQ.'10') THEN
          A = 1.D0 - X - Y - Z
          W(1) = Y*(2.D0*Y - 1.D0)
          W(2) = Z*(2.D0*Z - 1.D0)
          W(3) = A*(2.D0*A - 1.D0)
          W(4) = X*(2.D0*X - 1.D0)
          W(5) = 4.D0*Y*Z
          W(6) = 4.D0*Z*A
          W(7) = 4.D0*Y*A
          W(8) = 4.D0*X*Y
          W(9) = 4.D0*X*Z
          W(10) = 4.D0*X*A
          NNO = 10
        ELSE
          GOTO 10
        ENDIF
 
      ELSEIF (TYPEMA(1:4).EQ.'PENT') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)
        IF (TYPEMA(5:6).EQ.'A6') THEN
          A = 1.D0 - Y - Z
          B = 0.5D0 * (1.D0 - X)
          C = B + X
          W(1) = Y*B
          W(2) = Z*B
          W(3) = A*B
          W(4) = Y*C
          W(5) = Z*C
          W(6) = A*C
          NNO = 6
        ELSEIF (TYPEMA(5:7).EQ.'A12') THEN
          A = 1.D0 - Y - Z
          B = 0.5D0*(1.D0 - X)
          E = B + X
          C = Y*B
          D = Z*B
          B = A*B
          F = Y*E
          G = Z*E
          E = A*E
          W(1) = C*(2.D0*Y - 1.D0)
          W(2) = D*(2.D0*Z - 1.D0)
          W(3) = B*(2.D0*A - 1.D0)
          W(4) = F*(2.D0*Y - 1.D0)
          W(5) = G*(2.D0*Z - 1.D0)
          W(6) = E*(2.D0*A - 1.D0)
          W(7) = 4.D0*Y*D
          W(8) = 4.D0*Z*B
          W(9) = 4.D0*A*C
          W(10) = 4.D0*Y*G
          W(11) = 4.D0*Z*E
          W(12) = 4.D0*A*F
          NNO = 12
        ELSEIF (TYPEMA(5:7).EQ.'A14') THEN
          A = 1.D0 - Y - Z
          I = 3.D0*A*Y*Z
          B = 0.5D0*(1.D0 - X)
          F = B + X
          C = Y*B
          D = Z*B
          E = I*B
          B = A*B
          G = Y*F
          H = Z*F
          I = I*F
          F = A*F
          W(1) = C*(2.D0*Y - 1.D0) + E
          W(2) = D*(2.D0*Z - 1.D0) + E
          W(3) = B*(2.D0*A - 1.D0) + E
          W(4) = G*(2.D0*Y - 1.D0) + I
          W(5) = H*(2.D0*Z - 1.D0) + I
          W(6) = F*(2.D0*A - 1.D0) + I
          W(7) = 4.D0*(Y*D - E)
          W(8) = 4.D0*(Z*B - E)
          W(9) = 4.D0*(A*C - E)
          W(10) = 4.D0*(Y*H - I)
          W(11) = 4.D0*(Z*F - I)
          W(12) = 4.D0*(A*G - I)
          W(13) = 9.D0*E
          W(14) = 9.D0*I
          NNO = 14
        ELSEIF (TYPEMA(5:7).EQ.'A15') THEN
          A = 1.D0 - Y - Z
          B = 0.5D0*(1.D0 - X)
          E = B + X
          C = Y*B
          D = Z*B
          B = A*B
          F = Y*E
          G = Z*E
          E = A*E
          H = 1.D0 - X*X
          W(1) = C*(2.D0*Y - 2.D0 - X)
          W(2) = D*(2.D0*Z - 2.D0 - X)
          W(3) = B*(2.D0*A - 2.D0 - X)
          W(4) = F*(2.D0*Y - 2.D0 + X)
          W(5) = G*(2.D0*Z - 2.D0 + X)
          W(6) = E*(2.D0*A - 2.D0 + X)
          W(7) = 4.D0*Y*D
          W(8) = 4.D0*Z*B
          W(9) = 4.D0*A*C
          W(10) = Y*H
          W(11) = Z*H
          W(12) = A*H
          W(13) = 4.D0*Y*G
          W(14) = 4.D0*Z*E
          W(15) = 4.D0*A*F
          NNO = 15
        ELSEIF (TYPEMA(5:6).EQ.'H6') THEN
          R = 0.86602540378443865D0
          Z = Z * R
          A = 2.D0*X - 1.D0/3.D0
          B = 2.D0*Y - 1.D0/3.D0
          C = 0.5D0 - Z
          D = 0.5D0 + Z
          E = 1.D0 - A - B           
          W(1) = E*C
          W(2) = A*C
          W(3) = B*C
          W(4) = E*D
          W(5) = A*D
          W(6) = B*D
          NNO = 6
        ELSE 
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:3).EQ.'HEX') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)     
        IF (TYPEMA(4:5).EQ.'A8') THEN         
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*(1.D0 - Y)
          C = 0.5D0*(1.D0 - Z)
          D = A + X
          E = B + Y
          F = C + Z
          W(1) = A*B*C
          W(2) = D*B*C
          W(3) = D*E*C
          W(4) = A*E*C
          W(5) = A*B*F
          W(6) = D*B*F
          W(7) = D*E*F
          W(8) = A*E*F
          NNO = 8
        ELSEIF (TYPEMA(4:6).EQ.'A16') THEN
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*(1.D0 - Y)
          C = 0.5D0*(1.D0 - Z)
          D = A + X
          E = B + Y
          F = C + Z
          G = 1.D0 - Y*Y
          H = 1.D0 - Z*Z
          W(1) = A*B*C*(-1.D0 - Y - Z)
          W(2) = D*B*C*(-1.D0 - Y - Z)
          W(3) = D*E*C*(-1.D0 + Y - Z)
          W(4) = A*E*C*(-1.D0 + Y - Z)
          W(5) = A*B*F*(-1.D0 - Y + Z)
          W(6) = D*B*F*(-1.D0 - Y + Z)
          W(7) = D*E*F*(-1.D0 + Y + Z)
          W(8) = A*E*F*(-1.D0 + Y + Z)
          W(9) = D*G*C
          W(10) = A*G*C
          W(11) = A*B*H
          W(12) = D*B*H
          W(13) = D*E*H
          W(14) = A*E*H
          W(15) = D*G*F
          W(16) = A*G*F
          NNO = 16
        ELSEIF (TYPEMA(4:6).EQ.'A18') THEN
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*Y*(Y - 1.D0)
          C = 0.5D0*Z*(Z - 1.D0)
          D = A + X
          E = B + Y
          F = C + Z
          G = 1.D0 - Y*Y
          H = 1.D0 - Z*Z
          W(1) = A*B*C
          W(2) = D*B*C
          W(3) = D*E*C
          W(4) = A*E*C
          W(5) = A*B*F
          W(6) = D*B*F
          W(7) = D*E*F
          W(8) = A*E*F
          W(9) = D*G*C
          W(10) = A*G*C
          W(11) = A*B*H
          W(12) = D*B*H
          W(13) = D*E*H
          W(14) = A*E*H
          W(15) = D*G*F
          W(16) = A*G*F
          W(17) = D*G*H
          W(18) = A*G*H
          NNO = 18
        ELSEIF (TYPEMA(4:6).EQ.'A20') THEN
          A = 0.5D0*(1.D0 - X)
          B = 0.5D0*(1.D0 - Y)
          C = 0.5D0*(1.D0 - Z)
          D = A + X
          E = B + Y
          F = C + Z
          G = 1.D0 - X*X
          H = 1.D0 - Y*Y
          I = 1.D0 - Z*Z
          W(1) = A*B*C*(-2.D0-X-Y-Z)
          W(2) = D*B*C*(-2.D0+X-Y-Z)
          W(3) = D*E*C*(-2.D0+X+Y-Z)
          W(4) = A*E*C*(-2.D0-X+Y-Z)
          W(5) = A*B*F*(-2.D0-X-Y+Z)
          W(6) = D*B*F*(-2.D0+X-Y+Z)
          W(7) = D*E*F*(-2.D0+X+Y+Z)
          W(8) = A*E*F*(-2.D0-X+Y+Z)
          W(9) = G*B*C
          W(10) = D*H*C
          W(11) = G*E*C
          W(12) = A*H*C
          W(13) = A*B*I
          W(14) = D*B*I
          W(15) = D*E*I
          W(16) = A*E*I
          W(17) = G*B*F
          W(18) = D*H*F
          W(19) = G*E*F
          W(20) = A*H*F
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
          W(1) = A*B*C
          W(2) = D*B*C
          W(3) = D*E*C
          W(4) = A*E*C
          W(5) = A*B*F
          W(6) = D*B*F
          W(7) = D*E*F
          W(8) = A*E*F
          W(9) = G*B*C
          W(10) = D*H*C
          W(11) = G*E*C
          W(12) = A*H*C
          W(13) = A*B*I
          W(14) = D*B*I
          W(15) = D*E*I
          W(16) = A*E*I
          W(17) = G*B*F
          W(18) = D*H*F
          W(19) = G*E*F
          W(20) = A*H*F                              
          W(21) = G*H*C
          W(22) = G*B*I
          W(23) = D*H*I
          W(24) = G*E*I
          W(25) = A*H*I
          W(26) = G*H*F
          W(27) = G*H*I
          NNO = 27
        ELSEIF (TYPEMA(4:6).EQ.'H8') THEN
          R = 0.86602540378443865D0
          X = X * R
          Y = Y * R
          Z = Z * R
          A = 0.5D0 - X
          B = 0.5D0 - Y
          C = 0.5D0 - Z
          D = 0.5D0 + X
          E = 0.5D0 + Y
          F = 0.5D0 + Z
          W(1) = A*B*C
          W(2) = D*B*C
          W(3) = D*E*C
          W(4) = A*E*C
          W(5) = A*B*F
          W(6) = D*B*F
          W(7) = D*E*F
          W(8) = A*E*F
          NNO = 8
        ELSE          
          GOTO 10
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'PYRAM') THEN
        X = M(1)
        Y = M(2)
        Z = M(3)                 
        IF (TYPEMA(6:6).EQ.'5') THEN 
          IF (ABS(Z - 1.D0) .LT. ERPYRA) THEN
            W(1) = 0.D0
            W(2) = 0.D0
            W(3) = 0.D0
            W(4) = 0.D0
            W(5) = 1.D0
          ELSE
            A = 0.5D0 * (-X + Y + Z - 1.D0)
            B = A - Y
            C = B + X
            D = C + Y
            E = 1.D0/(1.D0 - Z)
            W(1) = A*B*E
            W(2) = B*C*E
            W(3) = C*D*E
            W(4) = D*A*E
            W(5) = Z
          ENDIF
          NNO = 5
        ELSEIF (TYPEMA(6:7).EQ.'13') THEN
          IF (ABS(Z - 1.D0) .LT. ERPYRA) THEN
            W(1) = 0.D0
            W(2) = 0.D0
            W(3) = 0.D0
            W(4) = 0.D0
            W(5) = 1.D0
            W(6) = 0.D0
            W(7) = 0.D0
            W(8) = 0.D0
            W(9) = 0.D0
            W(10) = 0.D0
            W(11) = 0.D0
            W(12) = 0.D0
            W(13) = 0.D0
          ELSE
            A = 0.5D0 * (-X + Y + Z - 1.D0)
            B = A - Y
            C = B + X
            D = C + Y
            E = 1.D0/(1.D0 - Z) 
            W(1) = A*B*(2.D0*X-1.D0)*E
            W(2) = B*C*(2.D0*Y-1.D0)*E
            W(3) = -C*D*(2.D0*X+1.D0)*E
            W(4) = -D*A*(2.D0*Y+1.D0)*E
            W(5) = Z*(2.D0*Z-1.D0)
            E = E * 4.D0
            W(6) = -A*B*C*E
            W(7) = -B*C*D*E
            W(8) = -C*D*A*E
            W(9) = -D*A*B*E
            W(10) = Z*A*B*E
            W(11) = Z*B*C*E
            W(12) = Z*C*D*E
            W(13) = Z*D*A*E
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

      CALL UTMESS('F','FORME0','MAILLE '//TYPEMA//' INDISPONIBLE')
      
 20   CONTINUE
 
      END
