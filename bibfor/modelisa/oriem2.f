      SUBROUTINE ORIEM2(TYPEMA,CNOEUD)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 27/04/2009   AUTEUR MEUNIER S.MEUNIER 
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      CHARACTER*8 TYPEMA
      REAL*8      CNOEUD(*)
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C ORIENTATION DES MAILLES SELON LE SENS DE L'ELEMENT DE REFERENCE
C ON REMET LES COORDONNEES DES NOEUDS (CNOEUD) DANS LE BON SENS (2D)
C
C ----------------------------------------------------------------------
C
C IN  TYPEMA : TYPE DE LA MAILLE ('TRIA', 'QUAD', 'TETRA', ETC.)
C I/O CNOEUD : COORDONNEES DES NOEUDS DE LA MAILLE
C
C ----------------------------------------------------------------------
C
      REAL*8      DDOT
      INTEGER     IELEM,I,J,I1,I2,P,N,DIME
      REAL*8      U(3),V(3),W(3),XN(3),R
      LOGICAL     LINEAR
C
      INTEGER     ZELEM,ZINO
      PARAMETER   (ZELEM=6,ZINO=20)
      INTEGER     INDICE(2,ZELEM)
      INTEGER     INO  (2,ZINO)
C
      DATA INDICE /1 ,2 ,
     &            3 ,4 ,
     &            6 ,7 ,
     &            9 ,11,
     &            14,16,
     &            21,0 /
      DATA INO / 2,3,   4,6,   2,4,   5,8,   6,7,
     &           2,3,   5,7,   9,10,  2,3,   5,6,
     &           7,9,   11,12, 13,15, 2,4,   6,8,
     &           9,12,  10,11, 14,16, 17,20, 18,19 /
C
C ----------------------------------------------------------------------
C
      IF (TYPEMA(1:4).EQ.'TRIA') THEN
        U(1) = CNOEUD(3) - CNOEUD(1)
        U(2) = CNOEUD(4) - CNOEUD(2)
        V(1) = CNOEUD(5) - CNOEUD(1)
        V(2) = CNOEUD(6) - CNOEUD(2)
        IF (U(1)*V(2).GT.U(2)*V(1)) THEN
          GOTO 999
        ELSE
          LINEAR = TYPEMA(5:5) .EQ. '3'
          DIME   = 2
          IELEM  = 1
        ENDIF
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        U(1) = CNOEUD(5) - CNOEUD(1)
        U(2) = CNOEUD(6) - CNOEUD(2)
        V(1) = CNOEUD(7) - CNOEUD(3)
        V(2) = CNOEUD(8) - CNOEUD(4)
        IF (U(1)*V(2).GT.U(2)*V(1)) THEN
          GOTO 999
        ELSE
          LINEAR = TYPEMA(5:5).EQ.'4'
          DIME   = 2
          IELEM  = 2
        ENDIF
      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        U(1) = CNOEUD(4) - CNOEUD(1)
        U(2) = CNOEUD(5) - CNOEUD(2)
        U(3) = CNOEUD(6) - CNOEUD(3)
        V(1) = CNOEUD(7) - CNOEUD(1)
        V(2) = CNOEUD(8) - CNOEUD(2)
        V(3) = CNOEUD(9) - CNOEUD(3)
        W(1) = CNOEUD(10) - CNOEUD(1)
        W(2) = CNOEUD(11) - CNOEUD(2)
        W(3) = CNOEUD(12) - CNOEUD(3)
        CALL PROVEC(V,W,XN)
        IF (DDOT(3,U,1,XN,1).GT.0.D0) THEN
          GOTO 999
        ELSE
          LINEAR = TYPEMA(6:6).EQ.'4'
          DIME   = 3
          IELEM  = 3
        ENDIF
      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
        U(1) = CNOEUD(10) + CNOEUD(13) + CNOEUD(16)
     &           - CNOEUD(1) - CNOEUD(4) - CNOEUD(7)
        U(2) = CNOEUD(11) + CNOEUD(14) + CNOEUD(17)
     &           - CNOEUD(2) - CNOEUD(5) - CNOEUD(8)
        U(3) = CNOEUD(12) + CNOEUD(15) + CNOEUD(18)
     &           - CNOEUD(3) - CNOEUD(6) - CNOEUD(9)
        V(1) = CNOEUD(13) + CNOEUD(4) - CNOEUD(10) - CNOEUD(1)
        V(2) = CNOEUD(14) + CNOEUD(5) - CNOEUD(11) - CNOEUD(2)
        V(3) = CNOEUD(15) + CNOEUD(6) - CNOEUD(12) - CNOEUD(3)
        W(1) = CNOEUD(16) + CNOEUD(7) - CNOEUD(10) - CNOEUD(1)
        W(2) = CNOEUD(17) + CNOEUD(8) - CNOEUD(11) - CNOEUD(2)
        W(3) = CNOEUD(18) + CNOEUD(9) - CNOEUD(12) - CNOEUD(3)
        CALL PROVEC(V,W,XN)
        IF (DDOT(3,U,1,XN,1).GT.0.D0) THEN
          GOTO 999
        ELSE
          LINEAR = TYPEMA(6:6).EQ.'6'
          DIME   = 3
          IELEM  = 4
        ENDIF
      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
        U(1) = CNOEUD(19) + CNOEUD(7) - CNOEUD(13) - CNOEUD(1)
        U(2) = CNOEUD(20) + CNOEUD(8) - CNOEUD(14) - CNOEUD(2)
        U(3) = CNOEUD(21) + CNOEUD(9) - CNOEUD(15) - CNOEUD(3)
        V(1) = CNOEUD(22) + CNOEUD(10) - CNOEUD(16) - CNOEUD(4)
        V(2) = CNOEUD(23) + CNOEUD(11) - CNOEUD(17) - CNOEUD(5)
        V(3) = CNOEUD(24) + CNOEUD(12) - CNOEUD(18) - CNOEUD(6)
        W(1) = CNOEUD(16) + CNOEUD(13) - CNOEUD(10) - CNOEUD(7)
        W(2) = CNOEUD(17) + CNOEUD(14) - CNOEUD(11) - CNOEUD(8)
        W(3) = CNOEUD(18) + CNOEUD(15) - CNOEUD(12) - CNOEUD(9)
        CALL PROVEC(V,W,XN)
        IF (DDOT(3,U,1,XN,1).GT.0.D0) THEN
          GOTO 999
        ELSE
          LINEAR = TYPEMA(5:5).EQ.'8'
          DIME   = 3
          IELEM  = 5
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- VERIFICATIONS
C
      IF (IELEM.GT.ZELEM) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- PERMUTATION
C
      P = INDICE(1,IELEM)

      IF (LINEAR) THEN
        N = INDICE(2,IELEM) - P
      ELSE
        N = INDICE(1,IELEM+1) - P
      ENDIF

      DO 10 , I = 1, N
        I1 = DIME*(INO(1,P)-1)
        I2 = DIME*(INO(2,P)-1)
        P = P + 1
        DO 20 , J = 1, DIME
          I1 = I1 + 1
          I2 = I2 + 1
          R  = CNOEUD(I1)
          CNOEUD(I1) = CNOEUD(I2)
          CNOEUD(I2) = R
 20     CONTINUE
 10   CONTINUE
C
 999  CONTINUE

      END
