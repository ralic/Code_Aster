      SUBROUTINE ORIEM2(TMA,NO)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/11/2004   AUTEUR DURAND C.DURAND 
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
C ----------------------------------------------------------------------
C   ORIENTATION DES MAILLES SELON LE SENS DE L'ELEMENT DE REFERENCE 
C ----------------------------------------------------------------------
C VARIABLE D'ENTREE
C CHARACTER*8   TMA     : TYPE DE LA MAILLE A PRIORI A REORIENTER
C
C VARIABLE D'ENTREE / SORTIE  
C REAL*8        NO      : COORDONNEES DE LA MAILLE (CF. CONOEU)
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTION
      REAL*8      R8DOT

C --- VARIABLES
      CHARACTER*8 TMA
      INTEGER     I,J,I1,I2,P,N,DIM
      REAL*8      NO(*),U(3),V(3),W(3),XN(3),R
      LOGICAL     L

C --- PARAMETRES
      INTEGER INDEX(2,6),INO(2,20)
      DATA INDEX / 1,2, 3,4, 6,7, 9,11, 14,16, 21,0 /
      DATA INO / 2,3,4,6, 2,4,5,8,6,7, 2,3,5,7,9,10, 2,3,5,6,
     & 7,9,11,12,13,15, 2,4,6,8,9,12,10,11,14,16,17,20,18,19 /

C --- SELECTION SUIVANT TYPE DE MAILLE

      IF (TMA(1:4).EQ.'TRIA') THEN

        U(1) = NO(3) - NO(1)
        U(2) = NO(4) - NO(2)
        V(1) = NO(5) - NO(1)
        V(2) = NO(6) - NO(2)

        IF (U(1)*V(2).GT.U(2)*V(1)) GOTO 20
        L = TMA(5:5) .EQ. '3'  
        DIM = 2
        I = 1

      ELSEIF (TMA(1:4).EQ.'QUAD') THEN

        U(1) = NO(5) - NO(1)
        U(2) = NO(6) - NO(2)
        V(1) = NO(7) - NO(3)
        V(2) = NO(8) - NO(4)

        IF (U(1)*V(2).GT.U(2)*V(1)) GOTO 20
        L = TMA(5:5).EQ.'4'
        DIM = 2
        I = 2

      ELSEIF (TMA(1:5).EQ.'TETRA') THEN

        U(1) = NO(4) - NO(1)
        U(2) = NO(5) - NO(2)
        U(3) = NO(6) - NO(3)
        V(1) = NO(7) - NO(1)
        V(2) = NO(8) - NO(2)
        V(3) = NO(9) - NO(3)
        W(1) = NO(10) - NO(1)
        W(2) = NO(11) - NO(2)
        W(3) = NO(12) - NO(3)

        CALL PROVEC(V,W,XN)
        IF (R8DOT(3,U,1,XN,1).GT.0.D0) GOTO 20
        L = TMA(6:6).EQ.'4'
        DIM = 3
        I = 3

      ELSEIF (TMA(1:5).EQ.'PENTA') THEN

        U(1) = NO(10) + NO(13) + NO(16) - NO(1) - NO(4) - NO(7)
        U(2) = NO(11) + NO(14) + NO(17) - NO(2) - NO(5) - NO(8)
        U(3) = NO(12) + NO(15) + NO(18) - NO(3) - NO(6) - NO(9)
        V(1) = NO(13) + NO(4) - NO(10) - NO(1)
        V(2) = NO(14) + NO(5) - NO(11) - NO(2)
        V(3) = NO(15) + NO(6) - NO(12) - NO(3)
        W(1) = NO(16) + NO(7) - NO(10) - NO(1)
        W(2) = NO(17) + NO(8) - NO(11) - NO(2)
        W(3) = NO(18) + NO(9) - NO(12) - NO(3)

        CALL PROVEC(V,W,XN)
        IF (R8DOT(3,U,1,XN,1).GT.0.D0) GOTO 20
        L = TMA(6:6).EQ.'6'
        DIM = 3
        I = 4

      ELSEIF (TMA(1:4).EQ.'HEXA') THEN

        U(1) = NO(19) + NO(7) - NO(13) - NO(1)
        U(2) = NO(20) + NO(8) - NO(14) - NO(2)
        U(3) = NO(21) + NO(9) - NO(15) - NO(3)
        V(1) = NO(22) + NO(10) - NO(16) - NO(4)
        V(2) = NO(23) + NO(11) - NO(17) - NO(5)
        V(3) = NO(24) + NO(12) - NO(18) - NO(6)
        W(1) = NO(16) + NO(13) - NO(10) - NO(7)
        W(2) = NO(17) + NO(14) - NO(11) - NO(8)
        W(3) = NO(18) + NO(15) - NO(12) - NO(9)

      
        CALL PROVEC(V,W,XN)
        IF (R8DOT(3,U,1,XN,1).GT.0.D0) GOTO 20
        L = TMA(5:5).EQ.'8'
        DIM = 3
        I = 5
        
      ELSE

        CALL UTMESS('F','ORIEM2','TYPE DE MAILLE '//TMA//' NON PREVU')

      ENDIF

C --- PERMUTATION

      P = INDEX(1,I)

      IF (L) THEN
        N = INDEX(2,I) - P
      ELSE
        N = INDEX(1,I+1) - P
      ENDIF

      DO 10 I = 1, N

        I1 = DIM*(INO(1,P)-1)
        I2 = DIM*(INO(2,P)-1)
        P = P + 1 

        DO 10 J = 1, DIM

          I1 = I1 + 1
          I2 = I2 + 1

          R = NO(I1)
          NO(I1) = NO(I2)
          NO(I2) = R

 10   CONTINUE

 20   CONTINUE

      END
