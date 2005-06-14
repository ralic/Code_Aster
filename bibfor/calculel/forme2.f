       SUBROUTINE FORME2(M,TYPEMA,W,NNO,NCMP)

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
C A_UTIL
C ----------------------------------------------------------------------
C          CALCUL DES DERIVEES SECONDES DES FONCTIONS DE FORME
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C REAL*8      M(*)        : POINT SUR MAILLE DE REFERENCE (X,[Y],[Z])
C CHARACTER*8 TYPEMA      : TYPE DE LA MAILLE
C       
C VARIABLES DE SORTIE
C REAL*8      W(NCMP,NNO) : DERIVEES 2IEMES DES FONCTIONS DE FORME EN M
C                          ( D2W1/DX2,[D2W1/DY2],[D2W1/DZ2],[D2W1/DXDY],
C                            [D2W1/DXDZ],[D2W1/DYDZ],D2W2/DX2,...) 
C INTEGER     NNO         : NOMBRE DE NOEUDS
C INTEGER     NCMP        : NOMBRE DE COMPOSANTES
C ---------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8 TYPEMA
      REAL*8      M(*),W(*),X,Y,A,B,C,D,E,F,G
      INTEGER     NNO,NCMP 

      IF (TYPEMA(1:3).EQ.'TRI') THEN
        NCMP = 3
        IF (TYPEMA(4:5).EQ.'A6') THEN
          W(1) = 4.D0  
          W(2) = 4.D0 
          W(3) = 4.D0
          W(4) = 4.D0 
          W(5) = 0.D0    
          W(6) = 0.D0
          W(7) = 0.D0 
          W(8) = 4.D0
          W(9) = 0.D0
          W(10) = -8.D0 
          W(11) = 0.D0  
          W(12) = -4.D0
          W(13) = 0.D0  
          W(14) = 0.D0  
          W(15) = 4.D0
          W(16) = 0.D0   
          W(17) = -8.D0   
          W(18) = -4.D0
          NNO = 6 
        ELSEIF (TYPEMA(4:5).EQ.'A7') THEN
          X = 6.D0*M(1)
          Y = 6.D0*M(2)
          A = 6.D0 - X - Y
          B = 4.D0*A
          C = 4.D0*X
          D = 4.D0*Y
          W(1) = 4.D0 - Y   
          W(2) = 4.D0 - X 
          W(3) = 1.D0 + A
          W(4) = 4.D0 - Y  
          W(5) = -X 
          W(6) = -3.D0 + A
          W(7) = -Y 
          W(8) = 4.D0 - X 
          W(9) = -3.D0 + A
          W(10) = -8.D0 + D
          W(11) = C 
          W(12) = 8.D0 - B
          W(13) = D
          W(14) = C  
          W(15) = 16.D0 - B
          W(16) = D 
          W(17) = -8.D0 + C 
          W(18) = 8.D0 - B
          W(19) = -9.D0*Y 
          W(20) = -9.D0*X  
          W(21) = -27.D0 + 9.D0*A
          NNO = 7
        ELSEIF (TYPEMA(4:5).EQ.'W6') THEN
          W(1) = -1.D0
          W(2) = 1.D0
          W(3) = -1.D0
          W(4) = 0.D0
          W(5) = 0.D0
          W(6) = 2.D0
          W(7) = 1.D0
          W(8) = -1.D0
          W(9) = -1.D0
          W(10) = 4.D0
          W(11) = 0.D0
          W(12) = 0.D0
          W(13) = 0.D0
          W(14) = 4.D0
          W(15) = 0.D0
          W(16) = 2.8284271247461901D0
          W(17) = W(16)
          W(18) = W(16)
          NNO = 6
        ELSE
          GOTO 10
        ENDIF
  
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        X = M(1)
        Y = M(2)
        A = 0.5D0 * (1.D0 - X)
        B = 0.5D0 * (1.D0 - Y)
        C = A + X
        D = B + Y
        IF (TYPEMA(5:5).EQ.'6') THEN
          NCMP = 2
          W(1) = B
          W(2) = A - 0.25D0
          W(3) = B
          W(4) = 0.25D0 - C
          W(5) = D
          W(6) = C - 0.25D0
          W(7) = D
          W(8) = 0.25D0 - A
          W(9) = Y - 1.D0
          W(10) = X
          W(11) = -1.D0 - Y
          W(12) = -X 
          NNO = 6
        ELSEIF (TYPEMA(5:5).EQ.'8') THEN              
          NCMP = 3   
          W(1) = B  
          W(2) = A 
          W(3) = A - D + 0.25D0
          W(4) = B 
          W(5) = C 
          W(6) = D - C - 0.25D0
          W(7) = D 
          W(8) = C 
          W(9) = C - B + 0.25D0
          W(10) = D 
          W(11) = A  
          W(12) = B - A - 0.25D0
          W(13) = Y - 1.D0  
          W(14) = 0.D0  
          W(15) = X
          W(16) = 0.D0  
          W(17) = -1.D0 - X 
          W(18) = -Y
          W(19) = -1.D0 - Y 
          W(20) = 0.D0  
          W(21) = -X
          W(22) = 0.D0 
          W(23) = -1.D0 + X 
          W(24) = Y
          NNO = 8
        ELSEIF (TYPEMA(5:5).EQ.'9') THEN
          NCMP = 3
          E = X*Y
          F = 1.D0 - X*X
          G = 1.D0 - Y*Y 
          W(1) = -Y*B
          W(2) = -X*A
          W(3) = E + A - D + 0.25D0
          W(4) = W(1)
          W(5) = X*C 
          W(6) = E + D - C - 0.25D0 
          W(7) = Y*D 
          W(8) = W(5)
          W(9) = E + C - B + 0.25D0   
          W(10) = W(7)
          W(11) = W(2)
          W(12) = E + B - A - 0.25D0
          E = E * 2.D0
          W(13) = -2.D0*W(1)
          W(14) = F
          W(15) = X - E
          W(16) = G
          W(17) = -2.D0*W(5)
          W(18) = -Y - E
          W(19) = -2.D0*W(7)
          W(20) = F
          W(21) = -X - E 
          W(22) = G
          W(23) = -2.D0*W(2)
          W(24) = Y - E
          W(25) = -2.D0*G
          W(26) = -2.D0*F
          W(27) = 2.D0*E
          NNO = 9
        ELSE
          GOTO 10 
        ENDIF

      ELSE
        GOTO 10
      ENDIF
   
      GOTO 20

 10   CONTINUE

      CALL UTMESS('F','FORME2','MAILLE '//TYPEMA//' INDISPONIBLE')
      
 20   CONTINUE

      END
