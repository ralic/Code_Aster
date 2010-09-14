      SUBROUTINE ELRFDF(ELREFZ,X,DIMD,DFF,NNO,NDIM)
      IMPLICIT NONE
      INTEGER DIMD,NNO,NDIM
      REAL*8 X(*),DFF(3,*)
      CHARACTER*(*) ELREFZ
C ----------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ELEMENTS  DATE 14/09/2010   AUTEUR REZETTE C.REZETTE 
C RESPONSABLE VABHHTS J.PELLET
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
C THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
C IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
C THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
C (AT YOUR OPTION) ANY LATER VERSION.

C THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
C WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
C MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
C GENERAL PUBLIC LICENSE FOR MORE DETAILS.

C YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
C ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
C    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.


C ======================================================================
C TOLE CRP_20

C BUT:   CALCUL DES FONCTIONS DE FORMES ET DE LEURS DERIVEES
C        AU POINT DE COORDONNEES XI,YI,ZI

C ----------------------------------------------------------------------
C   IN   ELREFZ : NOM DE L'ELREFE (K8)
C        X      : POINT DE CALCUL DES F FORMES ET DERIVEES
C        DIMD   : DIMENSION DE DFF
C   OUT  DFF    : FONCTIONS DE FORMES EN XI,YI,ZI
C        NNO    : NOMBRE DE NOEUDS
C        NDIM : DIMENSION TOPOLOGIQUE DE L'ELREFE
C   -------------------------------------------------------------------
      CHARACTER*8 ELREFE
      INTEGER I,J
      REAL*8 ZERO,UNDEMI,UN,DEUX,TROIS,QUATRE,UNS4,UNS8
      REAL*8 X0,Y0,Z0,AL,X1,X2,X3,X4,D1,D2,D3,D4
      REAL*8 PFACE1,PFACE2,PFACE3,PFACE4,Z01,Z02,Z04
      REAL*8 PMILI1,PMILI2,PMILI3,PMILI4,HUIT
      REAL*8 U,AL31,AL32,AL33,DAL31,DAL32,DAL33
      REAL*8 R,R1,R2,A,B,C,D,E,F,G,H,O,P,Q,S,T

C -----  FONCTIONS FORMULES
      AL31(U)  = 0.5D0*U*(U-1.0D0)
      AL32(U)  = -(U+1.0D0)*(U-1.0D0)
      AL33(U)  = 0.5D0*U*(U+1.0D0)
      DAL31(U) = 0.5D0*(2.0D0*U-1.0D0)
      DAL32(U) = -2.0D0*U
      DAL33(U) = 0.5D0*(2.0D0*U+1.0D0)
C DEB ------------------------------------------------------------------
      ELREFE = ELREFZ

      ZERO   = 0.0D0
      UNDEMI = 0.5D0
      UN     = 1.0D0
      DEUX   = 2.0D0
      TROIS  = 3.0D0
      QUATRE = 4.0D0
      HUIT   = 8.0D0
      UNS4   = UN/QUATRE
      UNS8   = UN/HUIT

C     ------------------------------------------------------------------
      IF (ELREFE.EQ.'HE8') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 8
        NDIM = 3

        DFF(1,1) = - (UN-Y0)* (UN-Z0)*UNS8
        DFF(2,1) = - (UN-X0)* (UN-Z0)*UNS8
        DFF(3,1) = - (UN-X0)* (UN-Y0)*UNS8
        DFF(1,2) = (UN-Y0)* (UN-Z0)*UNS8
        DFF(2,2) = - (UN+X0)* (UN-Z0)*UNS8
        DFF(3,2) = - (UN+X0)* (UN-Y0)*UNS8
        DFF(1,3) = (UN+Y0)* (UN-Z0)*UNS8
        DFF(2,3) = (UN+X0)* (UN-Z0)*UNS8
        DFF(3,3) = - (UN+X0)* (UN+Y0)*UNS8
        DFF(1,4) = - (UN+Y0)* (UN-Z0)*UNS8
        DFF(2,4) = (UN-X0)* (UN-Z0)*UNS8
        DFF(3,4) = - (UN-X0)* (UN+Y0)*UNS8
        DFF(1,5) = - (UN-Y0)* (UN+Z0)*UNS8
        DFF(2,5) = - (UN-X0)* (UN+Z0)*UNS8
        DFF(3,5) = (UN-X0)* (UN-Y0)*UNS8
        DFF(1,6) = (UN-Y0)* (UN+Z0)*UNS8
        DFF(2,6) = - (UN+X0)* (UN+Z0)*UNS8
        DFF(3,6) = (UN+X0)* (UN-Y0)*UNS8
        DFF(1,7) = (UN+Y0)* (UN+Z0)*UNS8
        DFF(2,7) = (UN+X0)* (UN+Z0)*UNS8
        DFF(3,7) = (UN+X0)* (UN+Y0)*UNS8
        DFF(1,8) = - (UN+Y0)* (UN+Z0)*UNS8
        DFF(2,8) = (UN-X0)* (UN+Z0)*UNS8
        DFF(3,8) = (UN-X0)* (UN+Y0)*UNS8

C     ------------------------------------------------------------------
      ELSEIF (ELREFE.EQ.'HH8') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 8
        NDIM = 3
        R1 = 0.2165063509461097D0
        A = R1 - 0.375D0*X0
        B = R1 - 0.375D0*Y0
        C = R1 - 0.375D0*Z0
        D = R1 + 0.375D0*X0
        E = R1 + 0.375D0*Y0
        F = R1 + 0.375D0*Z0
        R2 = 1.7320508075688773D0
        X0 = X0 * R2
        Y0 = Y0 * R2
        Z0 = Z0 * R2

        DFF(1,1) =  B*(Z0 - UN)
        DFF(2,1) =  A*(Z0 - UN)
        DFF(3,1) =  A*(Y0 - UN)
        DFF(1,2) = -B*(Z0 - UN)
        DFF(2,2) =  D*(Z0 - UN)
        DFF(3,2) =  D*(Y0 - UN)
        DFF(1,3) =  E*(UN - Z0)
        DFF(2,3) = -D*(Z0 - UN)
        DFF(3,3) = -D*(UN + Y0)
        DFF(1,4) = -E*(UN - Z0)
        DFF(2,4) = -A*(Z0 - UN)
        DFF(3,4) = -A*(UN + Y0)
        DFF(1,5) = -B*(UN + Z0)
        DFF(2,5) = -A*(UN + Z0)
        DFF(3,5) = -A*(Y0 - UN)
        DFF(1,6) =  B*(UN + Z0)
        DFF(2,6) = -D*(UN + Z0)
        DFF(3,6) = -D*(Y0 - UN)
        DFF(1,7) =  E*(UN + Z0)
        DFF(2,7) =  D*(UN + Z0)
        DFF(3,7) =  D*(UN + Y0)
        DFF(1,8) = -E*(UN + Z0)
        DFF(2,8) =  A*(UN + Z0)
        DFF(3,8) =  A*(UN + Y0)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'H16') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 16
        NDIM = 3

        A = UNDEMI*(UN - X0)
        B = UNDEMI*(UN - Y0)
        C = UNDEMI*(UN - Z0)
        D = A + X0
        E = B + Y0
        F = C + Z0
        G = UNDEMI*(UN - Y0*Y0)
        H = UNDEMI*(UN - Z0*Z0)
        O = UNDEMI*Y0
        P = UNDEMI*Z0

        DFF(1,1) =  B*C*( E + P )
        DFF(2,1) =  A*C*( Y0 + P )
        DFF(3,1) =  A*B*( Z0 + O )
        DFF(1,2) = -B*C*( E + P )
        DFF(2,2) =  D*C*( Y0 + P )
        DFF(3,2) =  D*B*( Z0 + O )
        DFF(1,3) =  E*C*(-B - P )
        DFF(2,3) =  D*C*( Y0 - P )
        DFF(3,3) =  D*E*( Z0 - O )
        DFF(1,4) = -E*C*( -B - P )
        DFF(2,4) =  A*C*( Y0 - P )
        DFF(3,4) =  A*E*( Z0 - O )
        DFF(1,5) =  B*F*(  E - P )
        DFF(2,5) =  A*F*( Y0 - P )
        DFF(3,5) =  A*B*( Z0 - O )
        DFF(1,6) = -B*F*(  E - P )
        DFF(2,6) =  D*F*( Y0 - P )
        DFF(3,6) =  D*B*( Z0 - O )
        DFF(1,7) =  E*F*( -B + P )
        DFF(2,7) =  D*F*( Y0 + P )
        DFF(3,7) =  D*E*( Z0 + O )
        DFF(1,8) = -E*F*(-B + P )
        DFF(2,8) =  A*F*( Y0 + P )
        DFF(3,8) =  A*E*( Z0 + O )

        Y0 = -DEUX*Y0
        Z0 = -DEUX*Z0

        DFF(1,9)  =  G*C
        DFF(2,9)  =  D*Y0*C
        DFF(3,9)  = -D*G
        DFF(1,10) = -G*C
        DFF(2,10) =  A*Y0*C
        DFF(3,10) = -A*G
        DFF(1,11) = -B*H
        DFF(2,11) = -A*H
        DFF(3,11) =  A*B*Z0
        DFF(1,12) =  B*H
        DFF(2,12) = -D*H
        DFF(3,12) =  D*B*Z0
        DFF(1,13) =  E*H
        DFF(2,13) =  D*H
        DFF(3,13) =  D*E*Z0
        DFF(1,14) = -E*H
        DFF(2,14) =  A*H
        DFF(3,14) =  A*E*Z0
        DFF(1,15) =  G*F
        DFF(2,15) =  D*Y0*F
        DFF(3,15) =  D*G
        DFF(1,16) = -G*F
        DFF(2,16) =  A*Y0*F
        DFF(3,16) =  A*G

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'H18') THEN
        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 18
        NDIM = 3

        A = UNDEMI*(UN - X0)
        B = UNDEMI*Y0*(Y0 - UN)
        C = UNDEMI*Z0*(Z0 - UN)
        D = A + X0
        E = B + Y0
        F = C + Z0
        G = UNDEMI*(UN - Y0*Y0)
        H = UNDEMI*(UN - Z0*Z0)

        DFF(1,1) = -UNDEMI*B*C
        DFF(2,1) = A*C*(Y0-UNDEMI)
        DFF(3,1) = A*B*(Z0-UNDEMI)
        DFF(1,2) = UNDEMI*B*C
        DFF(2,2) = D*C*(Y0-UNDEMI)
        DFF(3,2) = D*B*(Z0-UNDEMI)
        DFF(1,3) = UNDEMI*E*C
        DFF(2,3) = D*C*(Y0+UNDEMI)
        DFF(3,3) = D*E*(Z0-UNDEMI)
        DFF(1,4) = -UNDEMI*E*C
        DFF(2,4) = A*C*(Y0+UNDEMI)
        DFF(3,4) = A*E*(Z0-UNDEMI)
        DFF(1,5) = -UNDEMI*B*F
        DFF(2,5) = A*F*(Y0-UNDEMI)
        DFF(3,5) = A*B*(Z0+UNDEMI)
        DFF(1,6) = UNDEMI*B*F
        DFF(2,6) = D*F*(Y0-UNDEMI)
        DFF(3,6) = D*B*(Z0+UNDEMI)
        DFF(1,7) = UNDEMI*E*F
        DFF(2,7) = D*F*(Y0+UNDEMI)
        DFF(3,7) = D*E*(Z0+UNDEMI)
        DFF(1,8) = -UNDEMI*E*F
        DFF(2,8) = A*F*(Y0+UNDEMI)
        DFF(3,8) = A*E*(Z0+UNDEMI)

        Y0 = DEUX*Y0
        Z0 = DEUX*Z0

        DFF(1,9) = G*C
        DFF(2,9) = -D*Y0*C
        DFF(3,9) = D*G*(Z0-UN)
        DFF(1,10) = -G*C
        DFF(2,10) = -A*Y0*C
        DFF(3,10) = A*G*(Z0-UN)
        DFF(1,11) = -B*H
        DFF(2,11) = A*H*(Y0-UN)
        DFF(3,11) = -A*B*Z0
        DFF(1,12) = B*H
        DFF(2,12) = D*H*(Y0-UN)
        DFF(3,12) = -D*B*Z0
        DFF(1,13) = E*H
        DFF(2,13) = D*H*(Y0+UN)
        DFF(3,13) = -D*E*Z0
        DFF(1,14) = -E*H
        DFF(2,14) = A*H*(Y0+UN)
        DFF(3,14) = -A*E*Z0
        DFF(1,15) = G*F
        DFF(2,15) = -D*Y0*F
        DFF(3,15) = D*G*(Z0+UN)
        DFF(1,16) = -G*F
        DFF(2,16) = -A*Y0*F
        DFF(3,16) = A*G*(Z0+UN)

        Y0 = DEUX*Y0
        Z0 = DEUX*Z0

        DFF(1,17) = DEUX*G*H
        DFF(2,17) = -D*Y0*H
        DFF(3,17) = -D*G*Z0
        DFF(1,18) = -DEUX*G*H
        DFF(2,18) = -A*Y0*H
        DFF(3,18) = -A*G*Z0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'H20') THEN
        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 20
        NDIM = 3

        DFF(1,1) = - (UN-Y0)* (UN-Z0)* (-DEUX*X0-Y0-Z0-UN)*UNS8
        DFF(2,1) = - (UN-X0)* (UN-Z0)* (-X0-DEUX*Y0-Z0-UN)*UNS8
        DFF(3,1) = - (UN-X0)* (UN-Y0)* (-X0-Y0-DEUX*Z0-UN)*UNS8
        DFF(1,2) = (UN-Y0)* (UN-Z0)* (DEUX*X0-Y0-Z0-UN)*UNS8
        DFF(2,2) = - (UN+X0)* (UN-Z0)* (X0-DEUX*Y0-Z0-UN)*UNS8
        DFF(3,2) = - (UN+X0)* (UN-Y0)* (X0-Y0-DEUX*Z0-UN)*UNS8
        DFF(1,3) = (UN+Y0)* (UN-Z0)* (DEUX*X0+Y0-Z0-UN)*UNS8
        DFF(2,3) = (UN+X0)* (UN-Z0)* (X0+DEUX*Y0-Z0-UN)*UNS8
        DFF(3,3) = - (UN+X0)* (UN+Y0)* (X0+Y0-DEUX*Z0-UN)*UNS8
        DFF(1,4) = - (UN+Y0)* (UN-Z0)* (-DEUX*X0+Y0-Z0-UN)*UNS8
        DFF(2,4) = (UN-X0)* (UN-Z0)* (-X0+DEUX*Y0-Z0-UN)*UNS8
        DFF(3,4) = - (UN-X0)* (UN+Y0)* (-X0+Y0-DEUX*Z0-UN)*UNS8
        DFF(1,5) = - (UN-Y0)* (UN+Z0)* (-DEUX*X0-Y0+Z0-UN)*UNS8
        DFF(2,5) = - (UN-X0)* (UN+Z0)* (-X0-DEUX*Y0+Z0-UN)*UNS8
        DFF(3,5) = (UN-X0)* (UN-Y0)* (-X0-Y0+DEUX*Z0-UN)*UNS8
        DFF(1,6) = (UN-Y0)* (UN+Z0)* (DEUX*X0-Y0+Z0-UN)*UNS8
        DFF(2,6) = - (UN+X0)* (UN+Z0)* (X0-DEUX*Y0+Z0-UN)*UNS8
        DFF(3,6) = (UN+X0)* (UN-Y0)* (X0-Y0+DEUX*Z0-UN)*UNS8
        DFF(1,7) = (UN+Y0)* (UN+Z0)* (DEUX*X0+Y0+Z0-UN)*UNS8
        DFF(2,7) = (UN+X0)* (UN+Z0)* (X0+DEUX*Y0+Z0-UN)*UNS8
        DFF(3,7) = (UN+X0)* (UN+Y0)* (X0+Y0+DEUX*Z0-UN)*UNS8
        DFF(1,8) = - (UN+Y0)* (UN+Z0)* (-DEUX*X0+Y0+Z0-UN)*UNS8
        DFF(2,8) = (UN-X0)* (UN+Z0)* (-X0+DEUX*Y0+Z0-UN)*UNS8
        DFF(3,8) = (UN-X0)* (UN+Y0)* (-X0+Y0+DEUX*Z0-UN)*UNS8
        DFF(1,9) = -DEUX*X0* (UN-Y0)* (UN-Z0)*UNS4
        DFF(2,9) = - (UN-X0*X0)* (UN-Z0)*UNS4
        DFF(3,9) = - (UN-X0*X0)* (UN-Y0)*UNS4
        DFF(1,10) = (UN-Y0*Y0)* (UN-Z0)*UNS4
        DFF(2,10) = -DEUX*Y0* (UN+X0)* (UN-Z0)*UNS4
        DFF(3,10) = - (UN+X0)* (UN-Y0*Y0)*UNS4
        DFF(1,11) = -DEUX*X0* (UN+Y0)* (UN-Z0)*UNS4
        DFF(2,11) = (UN-X0*X0)* (UN-Z0)*UNS4
        DFF(3,11) = - (UN-X0*X0)* (UN+Y0)*UNS4
        DFF(1,12) = - (UN-Y0*Y0)* (UN-Z0)*UNS4
        DFF(2,12) = -DEUX*Y0* (UN-X0)* (UN-Z0)*UNS4
        DFF(3,12) = - (UN-X0)* (UN-Y0*Y0)*UNS4
        DFF(1,13) = - (UN-Z0*Z0)* (UN-Y0)*UNS4
        DFF(2,13) = - (UN-X0)* (UN-Z0*Z0)*UNS4
        DFF(3,13) = -DEUX*Z0* (UN-Y0)* (UN-X0)*UNS4
        DFF(1,14) = (UN-Z0*Z0)* (UN-Y0)*UNS4
        DFF(2,14) = - (UN+X0)* (UN-Z0*Z0)*UNS4
        DFF(3,14) = -DEUX*Z0* (UN-Y0)* (UN+X0)*UNS4
        DFF(1,15) = (UN-Z0*Z0)* (UN+Y0)*UNS4
        DFF(2,15) = (UN+X0)* (UN-Z0*Z0)*UNS4
        DFF(3,15) = -DEUX*Z0* (UN+Y0)* (UN+X0)*UNS4
        DFF(1,16) = - (UN-Z0*Z0)* (UN+Y0)*UNS4
        DFF(2,16) = (UN-X0)* (UN-Z0*Z0)*UNS4
        DFF(3,16) = -DEUX*Z0* (UN+Y0)* (UN-X0)*UNS4
        DFF(1,17) = -DEUX*X0* (UN-Y0)* (UN+Z0)*UNS4
        DFF(2,17) = - (UN-X0*X0)* (UN+Z0)*UNS4
        DFF(3,17) = (UN-X0*X0)* (UN-Y0)*UNS4
        DFF(1,18) = (UN-Y0*Y0)* (UN+Z0)*UNS4
        DFF(2,18) = -DEUX*Y0* (UN+X0)* (UN+Z0)*UNS4
        DFF(3,18) = (UN+X0)* (UN-Y0*Y0)*UNS4
        DFF(1,19) = -DEUX*X0* (UN+Y0)* (UN+Z0)*UNS4
        DFF(2,19) = (UN-X0*X0)* (UN+Z0)*UNS4
        DFF(3,19) = (UN-X0*X0)* (UN+Y0)*UNS4
        DFF(1,20) = - (UN-Y0*Y0)* (UN+Z0)*UNS4
        DFF(2,20) = -DEUX*Y0* (UN-X0)* (UN+Z0)*UNS4
        DFF(3,20) = (UN-X0)* (UN-Y0*Y0)*UNS4

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'H27') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 27
        NDIM = 3

        DFF(1,1) = DAL31(X0)*AL31(Y0)*AL31(Z0)
        DFF(2,1) = AL31(X0)*DAL31(Y0)*AL31(Z0)
        DFF(3,1) = AL31(X0)*AL31(Y0)*DAL31(Z0)
        DFF(1,2) = DAL33(X0)*AL31(Y0)*AL31(Z0)
        DFF(2,2) = AL33(X0)*DAL31(Y0)*AL31(Z0)
        DFF(3,2) = AL33(X0)*AL31(Y0)*DAL31(Z0)
        DFF(1,3) = DAL33(X0)*AL33(Y0)*AL31(Z0)
        DFF(2,3) = AL33(X0)*DAL33(Y0)*AL31(Z0)
        DFF(3,3) = AL33(X0)*AL33(Y0)*DAL31(Z0)
        DFF(1,4) = DAL31(X0)*AL33(Y0)*AL31(Z0)
        DFF(2,4) = AL31(X0)*DAL33(Y0)*AL31(Z0)
        DFF(3,4) = AL31(X0)*AL33(Y0)*DAL31(Z0)
        DFF(1,5) = DAL31(X0)*AL31(Y0)*AL33(Z0)
        DFF(2,5) = AL31(X0)*DAL31(Y0)*AL33(Z0)
        DFF(3,5) = AL31(X0)*AL31(Y0)*DAL33(Z0)
        DFF(1,6) = DAL33(X0)*AL31(Y0)*AL33(Z0)
        DFF(2,6) = AL33(X0)*DAL31(Y0)*AL33(Z0)
        DFF(3,6) = AL33(X0)*AL31(Y0)*DAL33(Z0)
        DFF(1,7) = DAL33(X0)*AL33(Y0)*AL33(Z0)
        DFF(2,7) = AL33(X0)*DAL33(Y0)*AL33(Z0)
        DFF(3,7) = AL33(X0)*AL33(Y0)*DAL33(Z0)
        DFF(1,8) = DAL31(X0)*AL33(Y0)*AL33(Z0)
        DFF(2,8) = AL31(X0)*DAL33(Y0)*AL33(Z0)
        DFF(3,8) = AL31(X0)*AL33(Y0)*DAL33(Z0)
        DFF(1,9) = DAL32(X0)*AL31(Y0)*AL31(Z0)
        DFF(2,9) = AL32(X0)*DAL31(Y0)*AL31(Z0)
        DFF(3,9) = AL32(X0)*AL31(Y0)*DAL31(Z0)
        DFF(1,10) = DAL33(X0)*AL32(Y0)*AL31(Z0)
        DFF(2,10) = AL33(X0)*DAL32(Y0)*AL31(Z0)
        DFF(3,10) = AL33(X0)*AL32(Y0)*DAL31(Z0)
        DFF(1,11) = DAL32(X0)*AL33(Y0)*AL31(Z0)
        DFF(2,11) = AL32(X0)*DAL33(Y0)*AL31(Z0)
        DFF(3,11) = AL32(X0)*AL33(Y0)*DAL31(Z0)
        DFF(1,12) = DAL31(X0)*AL32(Y0)*AL31(Z0)
        DFF(2,12) = AL31(X0)*DAL32(Y0)*AL31(Z0)
        DFF(3,12) = AL31(X0)*AL32(Y0)*DAL31(Z0)
        DFF(1,13) = DAL31(X0)*AL31(Y0)*AL32(Z0)
        DFF(2,13) = AL31(X0)*DAL31(Y0)*AL32(Z0)
        DFF(3,13) = AL31(X0)*AL31(Y0)*DAL32(Z0)
        DFF(1,14) = DAL33(X0)*AL31(Y0)*AL32(Z0)
        DFF(2,14) = AL33(X0)*DAL31(Y0)*AL32(Z0)
        DFF(3,14) = AL33(X0)*AL31(Y0)*DAL32(Z0)
        DFF(1,15) = DAL33(X0)*AL33(Y0)*AL32(Z0)
        DFF(2,15) = AL33(X0)*DAL33(Y0)*AL32(Z0)
        DFF(3,15) = AL33(X0)*AL33(Y0)*DAL32(Z0)
        DFF(1,16) = DAL31(X0)*AL33(Y0)*AL32(Z0)
        DFF(2,16) = AL31(X0)*DAL33(Y0)*AL32(Z0)
        DFF(3,16) = AL31(X0)*AL33(Y0)*DAL32(Z0)
        DFF(1,17) = DAL32(X0)*AL31(Y0)*AL33(Z0)
        DFF(2,17) = AL32(X0)*DAL31(Y0)*AL33(Z0)
        DFF(3,17) = AL32(X0)*AL31(Y0)*DAL33(Z0)
        DFF(1,18) = DAL33(X0)*AL32(Y0)*AL33(Z0)
        DFF(2,18) = AL33(X0)*DAL32(Y0)*AL33(Z0)
        DFF(3,18) = AL33(X0)*AL32(Y0)*DAL33(Z0)
        DFF(1,19) = DAL32(X0)*AL33(Y0)*AL33(Z0)
        DFF(2,19) = AL32(X0)*DAL33(Y0)*AL33(Z0)
        DFF(3,19) = AL32(X0)*AL33(Y0)*DAL33(Z0)
        DFF(1,20) = DAL31(X0)*AL32(Y0)*AL33(Z0)
        DFF(2,20) = AL31(X0)*DAL32(Y0)*AL33(Z0)
        DFF(3,20) = AL31(X0)*AL32(Y0)*DAL33(Z0)
        DFF(1,21) = DAL32(X0)*AL32(Y0)*AL31(Z0)
        DFF(2,21) = AL32(X0)*DAL32(Y0)*AL31(Z0)
        DFF(3,21) = AL32(X0)*AL32(Y0)*DAL31(Z0)
        DFF(1,22) = DAL32(X0)*AL31(Y0)*AL32(Z0)
        DFF(2,22) = AL32(X0)*DAL31(Y0)*AL32(Z0)
        DFF(3,22) = AL32(X0)*AL31(Y0)*DAL32(Z0)
        DFF(1,23) = DAL33(X0)*AL32(Y0)*AL32(Z0)
        DFF(2,23) = AL33(X0)*DAL32(Y0)*AL32(Z0)
        DFF(3,23) = AL33(X0)*AL32(Y0)*DAL32(Z0)
        DFF(1,24) = DAL32(X0)*AL33(Y0)*AL32(Z0)
        DFF(2,24) = AL32(X0)*DAL33(Y0)*AL32(Z0)
        DFF(3,24) = AL32(X0)*AL33(Y0)*DAL32(Z0)
        DFF(1,25) = DAL31(X0)*AL32(Y0)*AL32(Z0)
        DFF(2,25) = AL31(X0)*DAL32(Y0)*AL32(Z0)
        DFF(3,25) = AL31(X0)*AL32(Y0)*DAL32(Z0)
        DFF(1,26) = DAL32(X0)*AL32(Y0)*AL33(Z0)
        DFF(2,26) = AL32(X0)*DAL32(Y0)*AL33(Z0)
        DFF(3,26) = AL32(X0)*AL32(Y0)*DAL33(Z0)
        DFF(1,27) = DAL32(X0)*AL32(Y0)*AL32(Z0)
        DFF(2,27) = AL32(X0)*DAL32(Y0)*AL32(Z0)
        DFF(3,27) = AL32(X0)*AL32(Y0)*DAL32(Z0)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PE6') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 6
        NDIM = 3
        AL = (UN-Y0-Z0)

        DFF(1,1) = -Y0*UNDEMI
        DFF(2,1) = (UN-X0)*UNDEMI
        DFF(3,1) = ZERO
        DFF(1,2) = -Z0*UNDEMI
        DFF(2,2) = ZERO
        DFF(3,2) = (UN-X0)*UNDEMI
        DFF(1,3) = -AL*UNDEMI
        DFF(2,3) = - (UN-X0)*UNDEMI
        DFF(3,3) = - (UN-X0)*UNDEMI
        DFF(1,4) = Y0*UNDEMI
        DFF(2,4) = (UN+X0)*UNDEMI
        DFF(3,4) = ZERO
        DFF(1,5) = Z0*UNDEMI
        DFF(2,5) = ZERO
        DFF(3,5) = (UN+X0)*UNDEMI
        DFF(1,6) = AL*UNDEMI
        DFF(2,6) = - (UN+X0)*UNDEMI
        DFF(3,6) = - (UN+X0)*UNDEMI

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PH6') THEN

        R  = 1.7320508075688773D0
        X0 = X(1) * R
        Y0 = X(2) * R
        Z0 = X(3) * R
        R1  = 0.2886751345948129D0
        R2  = 0.86602540378443865D0
        NNO = 6
        NDIM = 3

        DFF(1,1) = -(UN - Z0)
        DFF(2,1) = -(UN - Z0)
        DFF(3,1) = -(R2 - (X0 - R1) - (Y0 - R1))
        DFF(1,2) = UN - Z0
        DFF(2,2) = ZERO
        DFF(3,2) = -(X0 - R1)
        DFF(1,3) = ZERO
        DFF(2,3) = UN - Z0
        DFF(3,3) = -(Y0 - R1)
        DFF(1,4) = -(UN + Z0)
        DFF(2,4) = -(UN + Z0)
        DFF(3,4) = R2 - (X0 - R1) - (Y0 - R1)
        DFF(1,5) = UN + Z0
        DFF(2,5) = ZERO
        DFF(3,5) = X0 - R1
        DFF(1,6) = ZERO
        DFF(2,6) = UN + Z0
        DFF(3,6) = Y0 - R1

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'P12') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 12
        NDIM = 3

        A = UN - Y0 - Z0
        B = UNDEMI*(UN - X0)
        C = B + X0
        D = QUATRE*A*B
        E = QUATRE*Y0*B
        F = QUATRE*Z0*B
        G = QUATRE*A*C
        H = QUATRE*Y0*C
        O = QUATRE*Z0*C

        DFF(1,1) = Y0*(UNDEMI - Y0)
        DFF(2,1) = E - B
        DFF(3,1) = ZERO
        DFF(1,2) = Z0*(UNDEMI - Z0)
        DFF(2,2) = ZERO
        DFF(3,2) = F - B
        DFF(1,3) = A*(UNDEMI - A)
        DFF(2,3) = B - D
        DFF(3,3) = B - D
        DFF(1,4) = -Y0*(UNDEMI - Y0)
        DFF(2,4) = H - C
        DFF(3,4) = ZERO
        DFF(1,5) = -Z0*(UNDEMI - Z0)
        DFF(2,5) = ZERO
        DFF(3,5) = O - C
        DFF(1,6) = -A*(UNDEMI - A)
        DFF(2,6) = C - G
        DFF(3,6) = C - G
        DFF(1,7) = -DEUX*Y0*Z0
        DFF(2,7) = F
        DFF(3,7) = E
        DFF(1,8) = -DEUX*Z0*A
        DFF(2,8) = -F
        DFF(3,8) = D - F
        DFF(1,9) = -DEUX*A*Y0
        DFF(2,9) = D - E
        DFF(3,9) = -E
        DFF(1,10) = DEUX*Y0*Z0
        DFF(2,10) = O
        DFF(3,10) = H
        DFF(1,11) = DEUX*Z0*A
        DFF(2,11) = -O
        DFF(3,11) = G - O
        DFF(1,12) = DEUX*A*Y0
        DFF(2,12) = G - H
        DFF(3,12) = -H

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'P14') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 14
        NDIM = 3

        A = UN - Y0 - Z0
        B = UNDEMI*(UN - X0)
        C = B + X0
        D = QUATRE*A*B
        E = QUATRE*Y0*B
        F = QUATRE*Z0*B
        G = QUATRE*A*C
        H = QUATRE*Y0*C
        O = QUATRE*Z0*C
        P = 1.5D0*A*Y0*Z0
        Q = TROIS*Z0*(A - Y0)
        O = TROIS*Y0*(A - Z0)
        S = Q*B
        T = O*B
        Q = Q*C
        O = O*C

        DFF(1,1) = Y0*(UNDEMI - Y0) - P
        DFF(2,1) = S + E - B
        DFF(3,1) = T
        DFF(1,2) = Z0*(UNDEMI - Z0) - P
        DFF(2,2) = S
        DFF(3,2) = T + F - B
        DFF(1,3) = A*(UNDEMI - A) - P
        DFF(2,3) = S + B - D
        DFF(3,3) = T + B - D
        DFF(1,4) = -(Y0*(UNDEMI - Y0) - P)
        DFF(2,4) =  Q + H - C
        DFF(3,4) =  O
        DFF(1,5) = -(Z0*(UNDEMI - Z0) - P)
        DFF(2,5) =  Q
        DFF(3,5) = O + O - C
        DFF(1,6) = -(A*(UNDEMI - A) - P)
        DFF(2,6) = Q + C - G
        DFF(3,6) = O + C - G

        P = QUATRE * P
        S =-QUATRE * S
        T =-QUATRE * T
        Q =-QUATRE * Q
        O =-QUATRE * O

        DFF(1,7) = -DEUX*Y0*Z0 + P
        DFF(2,7) = S + F
        DFF(3,7) = T + E
        DFF(1,8) = -DEUX*Z0*A + P
        DFF(2,8) = S - F
        DFF(3,8) = T + D - F
        DFF(1,9) = -DEUX*A*Y0 + P
        DFF(2,9) = S + D - E
        DFF(3,9) = T - E
        DFF(1,10) = -(-DEUX*Y0*Z0 + P)
        DFF(2,10) = Q + O
        DFF(3,10) = O + H
        DFF(1,11) = -(-DEUX*Z0*A + P)
        DFF(2,11) = Q - O
        DFF(3,11) = O + G - O
        DFF(1,12) = -(-DEUX*A*Y0 + P)
        DFF(2,12) = Q + G - H
        DFF(3,12) = O - H
        DFF(1,13) = -2.25D0*P
        DFF(2,13) = -2.25D0*S
        DFF(3,13) = -2.25D0*T
        DFF(1,14) =  2.25D0*P
        DFF(2,14) = -2.25D0*Q
        DFF(3,14) = -2.25D0*O

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'P15') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 15
        NDIM = 3
        AL = UN - Y0 - Z0

        DFF(1,1) = (-Y0* (DEUX*Y0-DEUX-X0)-Y0* (UN-X0))/DEUX
        DFF(2,1) = (UN-X0)* (QUATRE*Y0-DEUX-X0)/DEUX
        DFF(3,1) = ZERO
        DFF(1,2) = -Z0* (DEUX*Z0-UN-DEUX*X0)/DEUX
        DFF(2,2) = ZERO
        DFF(3,2) = (UN-X0)* (QUATRE*Z0-DEUX-X0)/DEUX
        DFF(1,3) = AL* (DEUX*X0+DEUX*Y0+DEUX*Z0-UN)/DEUX
        DFF(2,3) = (X0-UN)* (-X0-QUATRE*Y0-QUATRE*Z0+DEUX)/DEUX
        DFF(3,3) = (X0-UN)* (-X0-QUATRE*Y0-QUATRE*Z0+DEUX)/DEUX
        DFF(1,4) = Y0* (DEUX*Y0-UN+DEUX*X0)/DEUX
        DFF(2,4) = (UN+X0)* (QUATRE*Y0-DEUX+X0)/DEUX
        DFF(3,4) = ZERO
        DFF(1,5) = Z0* (DEUX*Z0-UN+DEUX*X0)/DEUX
        DFF(2,5) = ZERO
        DFF(3,5) = (UN+X0)* (QUATRE*Z0-DEUX+X0)/DEUX
        DFF(1,6) = AL* (DEUX*X0-DEUX*Y0-DEUX*Z0+UN)/DEUX
        DFF(2,6) = (X0+UN)* (-X0+QUATRE*Y0+QUATRE*Z0-DEUX)/DEUX
        DFF(3,6) = (X0+UN)* (-X0+QUATRE*Y0+QUATRE*Z0-DEUX)/DEUX
        DFF(1,7) = -DEUX*Y0*Z0
        DFF(2,7) = DEUX*Z0* (UN-X0)
        DFF(3,7) = DEUX*Y0* (UN-X0)
        DFF(1,8) = -DEUX*AL*Z0
        DFF(2,8) = -DEUX*Z0* (UN-X0)
        DFF(3,8) = (DEUX*AL-DEUX*Z0)* (UN-X0)
        DFF(1,9) = -DEUX*Y0*AL
        DFF(2,9) = (DEUX*AL-DEUX*Y0)* (UN-X0)
        DFF(3,9) = -DEUX*Y0* (UN-X0)
        DFF(1,10) = -DEUX*Y0*X0
        DFF(2,10) = (UN-X0*X0)
        DFF(3,10) = ZERO
        DFF(1,11) = -DEUX*Z0*X0
        DFF(2,11) = ZERO
        DFF(3,11) = (UN-X0*X0)
        DFF(1,12) = -DEUX*AL*X0
        DFF(2,12) = - (UN-X0*X0)
        DFF(3,12) = - (UN-X0*X0)
        DFF(1,13) = DEUX*Y0*Z0
        DFF(2,13) = DEUX*Z0* (UN+X0)
        DFF(3,13) = DEUX*Y0* (UN+X0)
        DFF(1,14) = DEUX*AL*Z0
        DFF(2,14) = -DEUX*Z0* (UN+X0)
        DFF(3,14) = (DEUX*AL-DEUX*Z0)* (UN+X0)
        DFF(1,15) = DEUX*Y0*AL
        DFF(2,15) = (DEUX*AL-DEUX*Y0)* (UN+X0)
        DFF(3,15) = -DEUX*Y0* (UN+X0)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'P18') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 18
        NDIM = 3

        DFF(1,1) = Y0*(DEUX*X0-UN)*(DEUX*Y0-UN)/DEUX
        DFF(2,1) = X0*(X0-UN)*(QUATRE*Y0-UN)/DEUX
        DFF(3,1) = ZERO
        DFF(1,2) = Z0*(DEUX*X0-UN)*(DEUX*Z0-UN)/DEUX
        DFF(2,2) = ZERO
        DFF(3,2) = X0*(X0-UN)*(QUATRE*Z0-UN)/DEUX
        DFF(1,3) = (DEUX*X0-UN)*(Z0+Y0-UN)*(DEUX*Z0+DEUX*Y0-UN)/DEUX
        DFF(2,3) = X0*(X0-UN)*(QUATRE*Z0+QUATRE*Y0-TROIS)/DEUX
        DFF(3,3) = X0*(X0-UN)*(QUATRE*Z0+QUATRE*Y0-TROIS)/DEUX
        DFF(1,4) = Y0*(DEUX*X0+UN)*(DEUX*Y0-UN)/DEUX
        DFF(2,4) = X0*(X0+UN)*(QUATRE*Y0-UN)/DEUX
        DFF(3,4) = ZERO
        DFF(1,5) = Z0*(DEUX*X0+UN)*(DEUX*Z0-UN)/DEUX
        DFF(2,5) = ZERO
        DFF(3,5) = X0*(X0+UN)*(QUATRE*Z0-UN)/DEUX
        DFF(1,6) = (DEUX*X0+UN)*(Z0+Y0-UN)*(DEUX*Z0+DEUX*Y0-UN)/DEUX
        DFF(2,6) = X0*(X0+UN)*(QUATRE*Z0+QUATRE*Y0-TROIS)/DEUX
        DFF(3,6) = X0*(X0+UN)*(QUATRE*Z0+QUATRE*Y0-TROIS)/DEUX
        DFF(1,7) = DEUX*Y0*Z0*(DEUX*X0-UN)
        DFF(2,7) = DEUX*X0*Z0*(X0-UN)
        DFF(3,7) = DEUX*X0*Y0*(X0-UN)
        DFF(1,8) = -DEUX*Z0*(DEUX*X0-UN)*(Z0+Y0-UN)
        DFF(2,8) = -DEUX*X0*Z0*(X0-UN)
        DFF(3,8) = -DEUX*X0*(X0-UN)*(DEUX*Z0+Y0-UN)
        DFF(1,9) = -DEUX*Y0*(DEUX*X0-UN)*(Z0+Y0-UN)
        DFF(2,9) = -DEUX*X0*(X0-UN)*(DEUX*Y0+Z0-UN)
        DFF(3,9) = -DEUX*X0*Y0*(X0-UN)
        DFF(1,10) = -DEUX*X0*Y0*(DEUX*Y0-UN)
        DFF(2,10) = -(X0-UN)*(X0+UN)*(QUATRE*Y0-UN)
        DFF(3,10) = ZERO
        DFF(1,11) = -DEUX*X0*Z0*(DEUX*Z0-UN)
        DFF(2,11) = ZERO
        DFF(3,11) = -(X0-UN)*(X0+UN)*(QUATRE*Z0-UN)
        DFF(1,12) = -DEUX*X0*(Z0+Y0-UN)*(DEUX*Z0+DEUX*Y0-UN)
        DFF(2,12) = -(X0-UN)*(X0+UN)*(QUATRE*Z0+QUATRE*Y0-TROIS)
        DFF(3,12) = -(X0-UN)*(X0+UN)*(QUATRE*Z0+QUATRE*Y0-TROIS)
        DFF(1,13) = DEUX*Y0*Z0*(DEUX*X0+UN)
        DFF(2,13) = DEUX*X0*Z0*(X0+UN)
        DFF(3,13) = DEUX*X0*Y0*(X0+UN)
        DFF(1,14) = -DEUX*Z0*(DEUX*X0+UN)*(Z0+Y0-UN)
        DFF(2,14) = -DEUX*X0*Z0*(X0+UN)
        DFF(3,14) = -DEUX*X0*(X0+UN)*(DEUX*Z0+Y0-UN)
        DFF(1,15) = -DEUX*Y0*(DEUX*X0+UN)*(Z0+Y0-UN)
        DFF(2,15) = -DEUX*X0*(X0+UN)*(DEUX*Y0+Z0-UN)
        DFF(3,15) = -DEUX*X0*Y0*(X0+UN)
        DFF(1,16) = -HUIT*X0*Y0*Z0
        DFF(2,16) = -QUATRE*Z0*(X0-UN)*(X0+UN)
        DFF(3,16) = -QUATRE*Y0*(X0-UN)*(X0+UN)
        DFF(1,17) = HUIT*X0*Z0*(Z0+Y0-UN)
        DFF(2,17) = QUATRE*Z0*(X0-UN)*(X0+UN)
        DFF(3,17) = QUATRE*(X0-UN)*(X0+UN)*(DEUX*Z0+Y0-UN)
        DFF(1,18) = HUIT*X0*Y0*(Z0+Y0-UN)
        DFF(2,18) = QUATRE*(X0-UN)*(X0+UN)*(DEUX*Y0+Z0-UN)
        DFF(3,18) = QUATRE*Y0*(X0-UN)*(X0+UN)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TE4') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 4
        NDIM = 3

        DFF(1,1) = ZERO
        DFF(2,1) = UN
        DFF(3,1) = ZERO
        DFF(1,2) = ZERO
        DFF(2,2) = ZERO
        DFF(3,2) =  UN
        DFF(1,3) = -UN
        DFF(2,3) = -UN
        DFF(3,3) = -UN
        DFF(1,4) =  UN
        DFF(2,4) = ZERO
        DFF(3,4) = ZERO

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'T10') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 10
        NDIM = 3
        AL = UN - X0 - Y0 - Z0

        DFF(1,1) = ZERO
        DFF(2,1) = QUATRE*Y0 - UN
        DFF(3,1) = ZERO
        DFF(1,2) = ZERO
        DFF(2,2) = ZERO
        DFF(3,2) = QUATRE*Z0 - UN
        DFF(1,3) = UN - QUATRE*AL
        DFF(2,3) = UN - QUATRE*AL
        DFF(3,3) = UN - QUATRE*AL
        DFF(1,4) = QUATRE*X0 - UN
        DFF(2,4) = ZERO
        DFF(3,4) = ZERO
        DFF(1,5) = ZERO
        DFF(2,5) = QUATRE*Z0
        DFF(3,5) = QUATRE*Y0
        DFF(1,6) = -QUATRE*Z0
        DFF(2,6) = -QUATRE*Z0
        DFF(3,6) = QUATRE* (AL-Z0)
        DFF(1,7) = -QUATRE*Y0
        DFF(2,7) = QUATRE* (AL-Y0)
        DFF(3,7) = -QUATRE*Y0
        DFF(1,8) = QUATRE*Y0
        DFF(2,8) = QUATRE*X0
        DFF(3,8) = ZERO
        DFF(1,9) = QUATRE*Z0
        DFF(2,9) = ZERO
        DFF(3,9) = QUATRE*X0
        DFF(1,10) = QUATRE* (AL-X0)
        DFF(2,10) = -QUATRE*X0
        DFF(3,10) = -QUATRE*X0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PY5') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 5
        NDIM = 3
        Z01 = UN - Z0
        Z04 = (UN-Z0)*QUATRE

        PFACE1 = X0 + Y0 + Z0 - UN
        PFACE2 = -X0 + Y0 + Z0 - UN
        PFACE3 = -X0 - Y0 + Z0 - UN
        PFACE4 = X0 - Y0 + Z0 - UN

        IF (ABS(Z0-UN).LT.1.0D-6) THEN
          DO 20 I = 1,5
            DO 10 J = 1,2
              DFF(J,I) = ZERO
   10       CONTINUE
   20     CONTINUE

          DFF(1,1) = UNDEMI
          DFF(1,3) = -UNDEMI

          DFF(2,2) = UNDEMI
          DFF(2,4) = -UNDEMI

          DFF(3,1) = -1.D0/4.D0
          DFF(3,2) = -1.D0/4.D0
          DFF(3,3) = -1.D0/4.D0
          DFF(3,4) = -1.D0/4.D0
          DFF(3,5) = UN

        ELSE

          DFF(1,1) = (-PFACE2-PFACE3)/Z04
          DFF(1,2) = (PFACE3-PFACE4)/Z04
          DFF(1,3) = (PFACE1+PFACE4)/Z04
          DFF(1,4) = (PFACE2-PFACE1)/Z04
          DFF(1,5) = ZERO

          DFF(2,1) = (PFACE3-PFACE2)/Z04
          DFF(2,2) = (-PFACE3-PFACE4)/Z04
          DFF(2,3) = (PFACE4-PFACE1)/Z04
          DFF(2,4) = (PFACE1+PFACE2)/Z04
          DFF(2,5) = ZERO

          DFF(3,1) = (PFACE2+PFACE3+PFACE2*PFACE3/Z01)/Z04
          DFF(3,2) = (PFACE3+PFACE4+PFACE3*PFACE4/Z01)/Z04
          DFF(3,3) = (PFACE4+PFACE1+PFACE4*PFACE1/Z01)/Z04
          DFF(3,4) = (PFACE1+PFACE2+PFACE1*PFACE2/Z01)/Z04
          DFF(3,5) = UN
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'P13') THEN

        X0 = X(1)
        Y0 = X(2)
        Z0 = X(3)
        NNO = 13
        NDIM = 3
        Z01 = UN - Z0
        Z02 = (UN-Z0)*DEUX

        PFACE1 = X0 + Y0 + Z0 - UN
        PFACE2 = -X0 + Y0 + Z0 - UN
        PFACE3 = -X0 - Y0 + Z0 - UN
        PFACE4 = X0 - Y0 + Z0 - UN

        PMILI1 = X0 - UNDEMI
        PMILI2 = Y0 - UNDEMI
        PMILI3 = -X0 - UNDEMI
        PMILI4 = -Y0 - UNDEMI

        IF (ABS(Z0-UN).LT.1.0D-6) THEN
          DO 40 I = 1,13
            DO 30 J = 1,2
              DFF(J,I) = ZERO
   30       CONTINUE
   40     CONTINUE

          DFF(1,1) = -UNDEMI
          DFF(1,3) = UNDEMI
          DFF(1,9) = DEUX
          DFF(1,11) = -DEUX

          DFF(2,2) = -UNDEMI
          DFF(2,4) = UNDEMI
          DFF(2,10) = DEUX
          DFF(2,12) = -DEUX

          DFF(3,1) = UN/QUATRE
          DFF(3,2) = UN/QUATRE
          DFF(3,3) = UN/QUATRE
          DFF(3,4) = UN/QUATRE
          DFF(3,6) = ZERO
          DFF(3,7) = ZERO
          DFF(3,8) = ZERO
          DFF(3,9) = ZERO

          DO 50 I = 10,13
            DFF(3,I) = -UN
   50     CONTINUE

          DFF(3,5) = TROIS

        ELSE

          DFF(1,1) = (PFACE2*PFACE3- (PFACE2+PFACE3)*PMILI1)/Z02
          DFF(1,2) = (PFACE3-PFACE4)*PMILI2/Z02
          DFF(1,3) = ((PFACE1+PFACE4)*PMILI3-PFACE4*PFACE1)/Z02
          DFF(1,4) = (PFACE2-PFACE1)*PMILI4/Z02
          DFF(1,5) = ZERO
          DFF(1,6) = (PFACE3*PFACE4+PFACE2*PFACE4-PFACE2*PFACE3)/Z02
          DFF(1,7) = (PFACE4*PFACE1-PFACE3*PFACE1-PFACE3*PFACE4)/Z02
          DFF(1,8) = (PFACE4*PFACE1-PFACE1*PFACE2-PFACE4*PFACE2)/Z02
          DFF(1,9) = (PFACE1*PFACE3+PFACE1*PFACE2-PFACE3*PFACE2)/Z02
          DFF(1,10) = (-PFACE3-PFACE2)*Z0/Z01
          DFF(1,11) = (PFACE3-PFACE4)*Z0/Z01
          DFF(1,12) = (PFACE1+PFACE4)*Z0/Z01
          DFF(1,13) = (PFACE2-PFACE1)*Z0/Z01

          DFF(2,1) = (PFACE3-PFACE2)*PMILI1/Z02
          DFF(2,2) = (PFACE3*PFACE4- (PFACE3+PFACE4)*PMILI2)/Z02
          DFF(2,3) = (PFACE4-PFACE1)*PMILI3/Z02
          DFF(2,4) = ((PFACE2+PFACE1)*PMILI4-PFACE1*PFACE2)/Z02
          DFF(2,5) = ZERO
          DFF(2,6) = (PFACE2*PFACE4+PFACE2*PFACE3-PFACE3*PFACE4)/Z02
          DFF(2,7) = (PFACE4*PFACE1+PFACE3*PFACE1-PFACE3*PFACE4)/Z02
          DFF(2,8) = (PFACE1*PFACE2-PFACE4*PFACE2-PFACE4*PFACE1)/Z02
          DFF(2,9) = (PFACE1*PFACE2-PFACE2*PFACE3-PFACE1*PFACE3)/Z02
          DFF(2,10) = (PFACE3-PFACE2)*Z0/Z01
          DFF(2,11) = (-PFACE4-PFACE3)*Z0/Z01
          DFF(2,12) = (PFACE4-PFACE1)*Z0/Z01
          DFF(2,13) = (PFACE2+PFACE1)*Z0/Z01

          DFF(3,1) = (PFACE2+PFACE3+PFACE2*PFACE3/Z01)*PMILI1/Z02
          DFF(3,2) = (PFACE3+PFACE4+PFACE3*PFACE4/Z01)*PMILI2/Z02
          DFF(3,3) = (PFACE1+PFACE4+PFACE1*PFACE4/Z01)*PMILI3/Z02
          DFF(3,4) = (PFACE2+PFACE1+PFACE1*PFACE2/Z01)*PMILI4/Z02
          DFF(3,5) = QUATRE*Z0 - UN
          DFF(3,6) = - (PFACE3*PFACE4+PFACE2*PFACE4+PFACE2*PFACE3+
     &               PFACE2*PFACE3*PFACE4/Z01)/Z02
          DFF(3,7) = - (PFACE4*PFACE1+PFACE3*PFACE1+PFACE3*PFACE4+
     &               PFACE3*PFACE4*PFACE1/Z01)/Z02
          DFF(3,8) = - (PFACE1*PFACE2+PFACE4*PFACE2+PFACE4*PFACE1+
     &               PFACE4*PFACE1*PFACE2/Z01)/Z02
          DFF(3,9) = - (PFACE2*PFACE3+PFACE1*PFACE3+PFACE1*PFACE2+
     &               PFACE1*PFACE2*PFACE3/Z01)/Z02
          DFF(3,10) = PFACE2*PFACE3/Z01/Z01 + (PFACE3+PFACE2)*Z0/Z01
          DFF(3,11) = PFACE3*PFACE4/Z01/Z01 + (PFACE4+PFACE3)*Z0/Z01
          DFF(3,12) = PFACE4*PFACE1/Z01/Z01 + (PFACE1+PFACE4)*Z0/Z01
          DFF(3,13) = PFACE1*PFACE2/Z01/Z01 + (PFACE1+PFACE2)*Z0/Z01
        END IF

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TR3') THEN

        NNO = 3
        NDIM = 2

        DFF(1,1) = -UN
        DFF(2,1) = -UN
        DFF(1,2) = +UN
        DFF(2,2) = ZERO
        DFF(1,3) = ZERO
        DFF(2,3) = +UN

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TH3') THEN

        NNO = 3
        NDIM = 2

        DFF(1,1) = -DEUX
        DFF(2,1) = -DEUX
        DFF(1,2) =  DEUX
        DFF(2,2) =  ZERO
        DFF(1,3) =  ZERO
        DFF(2,3) =  DEUX

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TR6') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 6
        NDIM = 2
        AL = UN - X0 - Y0

        DFF(1,1) = UN - QUATRE*AL
        DFF(1,2) = -UN + QUATRE*X0
        DFF(1,3) = ZERO
        DFF(1,4) = QUATRE* (AL-X0)
        DFF(1,5) = QUATRE*Y0
        DFF(1,6) = -QUATRE*Y0
        DFF(2,1) = UN - QUATRE*AL
        DFF(2,2) = ZERO
        DFF(2,3) = -UN + QUATRE*Y0
        DFF(2,4) = -QUATRE*X0
        DFF(2,5) = QUATRE*X0
        DFF(2,6) = QUATRE* (AL-Y0)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TW6') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 6
        NDIM = 2

        R = 2.8284271247461901D0
        AL = UNDEMI - X0 - Y0
        X0 = DEUX * X0
        Y0 = DEUX * Y0

        DFF(1,1) = AL
        DFF(2,1) = AL + Y0
        DFF(1,2) = Y0 - UN
        DFF(2,2) = X0 - UN
        DFF(1,3) = AL + X0
        DFF(2,3) = AL
        DFF(1,4) = DEUX*(X0 - UN)
        DFF(2,4) = ZERO
        DFF(1,5) = ZERO
        DFF(2,5) = DEUX*(Y0 - UN)
        DFF(1,6) = -R*AL
        DFF(2,6) = -R*AL

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'TR7') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 7
        NDIM = 2

        DFF(1,1) = -TROIS + 4.0D0*X0 + 7.0D0*Y0 - 6.0D0*X0*Y0
     &                    - 3.0D0*Y0*Y0
        DFF(1,2) = -UN + 4.0D0*X0 + 3.0D0*Y0 - 6.0D0*X0*Y0
     &                    - 3.0D0*Y0*Y0
        DFF(1,3) = 3.0D0*Y0*( UN - 2.0D0*X0 - Y0 )
        DFF(1,4) = 4.0D0*( UN - 2.0D0*X0 - 4.0D0*Y0 + 6.0D0*X0*Y0
     &                        + 3.0D0*Y0*Y0 )
        DFF(1,5) = 4.0D0*Y0*( -2.0D0 + 6.0D0*X0 + 3.0D0*Y0 )
        DFF(1,6) = 4.0D0*Y0*( -4.0D0 + 6.0D0*X0 + 3.0D0*Y0 )
        DFF(1,7) = 27.D0*Y0*( UN - 2.0D0*X0 - Y0 )

        DFF(2,1) = -TROIS + 4.0D0*Y0 + 7.0D0*X0 - 6.0D0*X0*Y0
     &                    - 3.0D0*X0*X0
        DFF(2,2) = 3.0D0*X0*( UN - 2.0D0*Y0 - X0 )
        DFF(2,3) = -UN + 4.0D0*Y0 + 3.0D0*X0 - 6.0D0*X0*Y0
     &                    - 3.0D0*X0*X0
        DFF(2,4) = 4.0D0*X0*( -4.0D0 + 6.0D0*Y0 + 3.0D0*X0 )
        DFF(2,5) = 4.0D0*X0*( -2.0D0 + 6.0D0*Y0 + 3.0D0*X0 )
        DFF(2,6) = 4.0D0*( UN - 2.0D0*Y0 - 4.0D0*X0 + 6.0D0*X0*Y0
     &                        + 3.0D0*X0*X0 )
        DFF(2,7) = 27.D0*X0*( UN - 2.0D0*Y0 - X0 )

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU4') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 4
        NDIM = 2

        DFF(1,1) = -(UN-Y0)*UNS4
        DFF(2,1) = -(UN-X0)*UNS4
        DFF(1,2) =  (UN-Y0)*UNS4
        DFF(2,2) = -(UN+X0)*UNS4
        DFF(1,3) =  (UN+Y0)*UNS4
        DFF(2,3) =  (UN+X0)*UNS4
        DFF(1,4) = -(UN+Y0)*UNS4
        DFF(2,4) =  (UN-X0)*UNS4

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QH4') THEN

        R  = 0.4330127018922193D0
        X0 = X(1) * 0.75D0
        Y0 = X(2) * 0.75D0
        NNO = 4
        NDIM = 2

        DFF(1,1) = -(R - Y0)
        DFF(2,1) = -(R - X0)
        DFF(1,2) =   R - Y0
        DFF(2,2) = -(R + X0)
        DFF(1,3) =   R + Y0
        DFF(2,3) =   R + X0
        DFF(1,4) = -(R + Y0)
        DFF(2,4) =   R - X0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU6') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 6
        NDIM = 2

        DFF(1,1) = UNDEMI*(UN-Y0)*(X0-UNDEMI)
        DFF(2,1) =  -UNS4*X0*(X0-UN)
        DFF(1,2) = UNDEMI*(UN-Y0)*(X0+UNDEMI)
        DFF(2,2) = -(UNS4*X0*(X0-UN) + UNDEMI*X0)
        DFF(1,3) = (UNDEMI*(UN-Y0) + Y0)*(X0+UNDEMI)
        DFF(2,3) =   UNS4*X0*(X0-UN) + UNDEMI*X0
        DFF(1,4) = (UNDEMI*(UN-Y0) + Y0)*(X0-UNDEMI)
        DFF(2,4) = UNS4*X0*(X0-UN)
        DFF(1,5) = X0*(Y0 - UN)
        DFF(2,5) = -UNDEMI*(UN-X0*X0)
        DFF(1,6) = -X0*(UN + Y0)
        DFF(2,6) = UNDEMI*(UN-X0*X0)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU8') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 8
        NDIM = 2

        DFF(1,1) = -UNS4* (UN-Y0)* (-DEUX*X0-Y0)
        DFF(2,1) = -UNS4* (UN-X0)* (-DEUX*Y0-X0)
        DFF(1,2) =  UNS4* (UN-Y0)* ( DEUX*X0-Y0)
        DFF(2,2) = -UNS4* (UN+X0)* (-DEUX*Y0+X0)
        DFF(1,3) = UNS4* (UN+Y0)* (DEUX*X0+Y0)
        DFF(2,3) = UNS4* (UN+X0)* (DEUX*Y0+X0)
        DFF(1,4) = -UNS4* (UN+Y0)* (-DEUX*X0+Y0)
        DFF(2,4) =  UNS4* (UN-X0)* ( DEUX*Y0-X0)
        DFF(1,5) = -DEUX*X0* (UN-Y0)*UNDEMI
        DFF(2,5) = - (UN-X0*X0)*UNDEMI
        DFF(1,6) = (UN-Y0*Y0)*UNDEMI
        DFF(2,6) = -DEUX*Y0* (UN+X0)*UNDEMI
        DFF(1,7) = -DEUX*X0* (UN+Y0)*UNDEMI
        DFF(2,7) = (UN-X0*X0)*UNDEMI
        DFF(1,8) = - (UN-Y0*Y0)*UNDEMI
        DFF(2,8) = -DEUX*Y0* (UN-X0)*UNDEMI

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'QU9') THEN

        X0 = X(1)
        Y0 = X(2)
        NNO = 9
        NDIM = 2

        DFF(1,1) = DAL31(X0)*AL31(Y0)
        DFF(2,1) = AL31(X0)*DAL31(Y0)
        DFF(1,2) = DAL33(X0)*AL31(Y0)
        DFF(2,2) = AL33(X0)*DAL31(Y0)
        DFF(1,3) = DAL33(X0)*AL33(Y0)
        DFF(2,3) = AL33(X0)*DAL33(Y0)
        DFF(1,4) = DAL31(X0)*AL33(Y0)
        DFF(2,4) = AL31(X0)*DAL33(Y0)
        DFF(1,5) = DAL32(X0)*AL31(Y0)
        DFF(2,5) = AL32(X0)*DAL31(Y0)
        DFF(1,6) = DAL33(X0)*AL32(Y0)
        DFF(2,6) = AL33(X0)*DAL32(Y0)
        DFF(1,7) = DAL32(X0)*AL33(Y0)
        DFF(2,7) = AL32(X0)*DAL33(Y0)
        DFF(1,8) = DAL31(X0)*AL32(Y0)
        DFF(2,8) = AL31(X0)*DAL32(Y0)
        DFF(1,9) = DAL32(X0)*AL32(Y0)
        DFF(2,9) = AL32(X0)*DAL32(Y0)

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'PO1') THEN
        NNO = 1
        NDIM = 0

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SE2') THEN
        NNO = 2
        NDIM = 1

        DFF(1,1) = -UNDEMI
        DFF(1,2) =  UNDEMI

C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SE3') THEN
        X0 = X(1)
        NNO = 3
        NDIM = 1

        DFF(1,1) = X0 - UNDEMI
        DFF(1,2) = X0 + UNDEMI
        DFF(1,3) = -DEUX*X0
C     ------------------------------------------------------------------
      ELSE IF (ELREFE.EQ.'SE4') THEN
        X0 = X(1)
        NNO = 4
        NDIM = 1

        X1 = -UN
        X2 =  UN
        X3 = -UN/TROIS
        X4 =  UN/TROIS
        D1 = (X1-X2)* (X1-X3)* (X1-X4)
        DFF(1,1) = ((X0-X2)* (X0-X3)+ (X0-X2)* (X0-X4)+
     &             (X0-X3)* (X0-X4))/D1
        D2 = (X2-X1)* (X2-X3)* (X2-X4)
        DFF(1,2) = ((X0-X1)* (X0-X3)+ (X0-X1)* (X0-X4)+
     &             (X0-X3)* (X0-X4))/D2
        D3 = (X3-X1)* (X3-X2)* (X3-X4)
        DFF(1,3) = ((X0-X1)* (X0-X2)+ (X0-X1)* (X0-X4)+
     &             (X0-X2)* (X0-X4))/D3
        D4 = (X4-X1)* (X4-X2)* (X4-X3)
        DFF(1,4) = ((X0-X1)* (X0-X2)+ (X0-X1)* (X0-X3)+
     &             (X0-X2)* (X0-X3))/D4

C     ------------------------------------------------------------------
      ELSE
        CALL ASSERT(.FALSE.)
      END IF


C     ------------------------------------------------------------------

      CALL ASSERT(DIMD.GE.(NNO*NDIM))

      END
