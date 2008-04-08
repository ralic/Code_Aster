      SUBROUTINE ORIEM3(NUMA  ,TYPEMA,CNOEUD,CNX   ,CNXC  ,
     &                  CNXMA)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
      INTEGER     NUMA
      CHARACTER*8 TYPEMA
      REAL*8      CNOEUD(3,*)
      INTEGER     CNX(*),CNXC(*),CNXMA(*)
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C ORIENTATION DES MAILLES SELON LE SENS DE L'ELEMENT DE REFERENCE
C ON REMET LES COORDONNEES DES NOEUDS (CNOEUD) DANS LE BON SENS (3D)
C
C ----------------------------------------------------------------------
C
C
C IN  NUMA   : NUMERO ABSOLU DE LA MAILLE
C IN  TYPEMA : TYPE DE LA MAILLE
C IN  CNOEUD : COORDONNEES DE LA MAILLE
C IN  CNX    : CONNECTIVITE DES MAILLES
C IN  CNXC   : LONGUEUR CUMULEE DE CNX
C OUT CNXMA  : CONNECTIVITE DE NUMA DANS LE BON SENS
C
C ----------------------------------------------------------------------
C
      REAL*8      DDOT
      INTEGER     I,NNO,P,Q,A,B,C,D,E,F,G,H
      REAL*8      U(3),V(3),W(3),N(3)
      LOGICAL     PERMUT
C
      INTEGER INDEX(5),INO(68)
      DATA INDEX / 1,8,17,27,42 /
      DATA INO / 0,2,1,5,4,3,6, 0,3,2,1,7,6,5,4,8, 0,2,1,3,6,5,4,7,9,8,
     & 0,2,1,3,5,4,8,7,6,9,11,10,14,13,12,  0,3,2,1,4,7,6,5,11,10,9,8,
     & 12,15,14,13,19,18,17,16,20,21,22,23,24,25,26 /
C
C ----------------------------------------------------------------------
C
      P   = CNXC(NUMA)
      NNO = CNXC(NUMA+1) - P

C --- SELECTION SUIVANT TYPE DE MAILLE

      IF ((TYPEMA(1:4).EQ.'TRIA').AND.
     &    ((NNO.EQ.3).OR.(NNO.EQ.6).OR.(NNO.EQ.7))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)

        U(1) = CNOEUD(1,B)-CNOEUD(1,A)
        U(2) = CNOEUD(2,B)-CNOEUD(2,A)
        V(1) = CNOEUD(1,C)-CNOEUD(1,A)
        V(2) = CNOEUD(2,C)-CNOEUD(2,A)

        PERMUT = (U(1)*V(2).LE.U(2)*V(1))
        Q = INDEX(1)

      ELSEIF ((TYPEMA(1:4).EQ.'QUAD').AND.
     &        ((NNO.EQ.4).OR.(NNO.EQ.8).OR.(NNO.EQ.9))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)

        U(1) = CNOEUD(1,C)-CNOEUD(1,A)
        U(2) = CNOEUD(2,C)-CNOEUD(2,A)
        V(1) = CNOEUD(1,D)-CNOEUD(1,B)
        V(2) = CNOEUD(2,D)-CNOEUD(2,B)

        PERMUT = (U(1)*V(2).LE.U(2)*V(1))
        Q = INDEX(2)

      ELSEIF ((TYPEMA(1:5).EQ.'TETRA').AND.
     &        ((NNO.EQ.4).OR.(NNO.EQ.10))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)

        U(1) = CNOEUD(1,B)-CNOEUD(1,A)
        U(2) = CNOEUD(2,B)-CNOEUD(2,A)
        U(3) = CNOEUD(3,B)-CNOEUD(3,A)
        V(1) = CNOEUD(1,C)-CNOEUD(1,A)
        V(2) = CNOEUD(2,C)-CNOEUD(2,A)
        V(3) = CNOEUD(3,C)-CNOEUD(3,A)
        W(1) = CNOEUD(1,D)-CNOEUD(1,A)
        W(2) = CNOEUD(2,D)-CNOEUD(2,A)
        W(3) = CNOEUD(3,D)-CNOEUD(3,A)

        CALL PROVEC(V,W,N)
        PERMUT = (DDOT(3,U,1,N,1).LE.0.D0)
        Q = INDEX(3)

      ELSEIF ((TYPEMA(1:5).EQ.'PENTA').AND.
     &        ((NNO.EQ.6).OR.(NNO.EQ.15))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)
        E = CNX(P+4)
        F = CNX(P+5)

        U(1) = CNOEUD(1,D)+CNOEUD(1,E)+CNOEUD(1,F)-CNOEUD(1,A)
     &                      -CNOEUD(1,B)-CNOEUD(1,C)
        U(2) = CNOEUD(2,D)+CNOEUD(2,E)+CNOEUD(2,F)-CNOEUD(2,A)
     &                      -CNOEUD(2,B)-CNOEUD(2,C)
        U(3) = CNOEUD(3,D)+CNOEUD(3,E)+CNOEUD(3,F)-CNOEUD(3,A)
     &                      -CNOEUD(3,B)-CNOEUD(3,C)
        V(1) = CNOEUD(1,E)+CNOEUD(1,B)-CNOEUD(1,D)-CNOEUD(1,A)
        V(2) = CNOEUD(2,E)+CNOEUD(2,B)-CNOEUD(2,D)-CNOEUD(2,A)
        V(3) = CNOEUD(3,E)+CNOEUD(3,B)-CNOEUD(3,D)-CNOEUD(3,A)
        W(1) = CNOEUD(1,F)+CNOEUD(1,C)-CNOEUD(1,D)-CNOEUD(1,A)
        W(2) = CNOEUD(2,F)+CNOEUD(2,C)-CNOEUD(2,D)-CNOEUD(2,A)
        W(3) = CNOEUD(3,F)+CNOEUD(3,C)-CNOEUD(3,D)-CNOEUD(3,A)

        CALL PROVEC(V,W,N)
        PERMUT = (DDOT(3,U,1,N,1).LE.0.D0)
        Q = INDEX(4)

      ELSEIF ((TYPEMA(1:4).EQ.'HEXA').AND.
     &        ((NNO.EQ.8).OR.(NNO.EQ.20).OR.(NNO.EQ.27))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)
        E = CNX(P+4)
        F = CNX(P+5)
        G = CNX(P+6)
        H = CNX(P+7)

        U(1) = CNOEUD(1,G)+CNOEUD(1,C)-CNOEUD(1,E)-CNOEUD(1,A)
        U(2) = CNOEUD(2,G)+CNOEUD(2,C)-CNOEUD(2,E)-CNOEUD(2,A)
        U(3) = CNOEUD(3,G)+CNOEUD(3,C)-CNOEUD(3,E)-CNOEUD(3,A)
        V(1) = CNOEUD(1,H)+CNOEUD(1,D)-CNOEUD(1,F)-CNOEUD(1,B)
        V(2) = CNOEUD(2,H)+CNOEUD(2,D)-CNOEUD(2,F)-CNOEUD(2,B)
        V(3) = CNOEUD(3,H)+CNOEUD(3,D)-CNOEUD(3,F)-CNOEUD(3,B)
        W(1) = CNOEUD(1,F)+CNOEUD(1,E)-CNOEUD(1,D)-CNOEUD(1,C)
        W(2) = CNOEUD(2,F)+CNOEUD(2,E)-CNOEUD(2,D)-CNOEUD(2,C)
        W(3) = CNOEUD(3,F)+CNOEUD(3,E)-CNOEUD(3,D)-CNOEUD(3,C)

        CALL PROVEC(V,W,N)
        PERMUT = (DDOT(3,U,1,N,1).LE.0.D0)
        Q = INDEX(5)

      ELSE
        CALL U2MESS('F','MODELISA6_9')
      ENDIF
C
C --- VERIFICATIONS
C
      IF (NNO.GT.27) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
C
C --- PERMUTATION
C
      IF (PERMUT) THEN
        DO 10 I = 1, NNO
          CNXMA(I) = CNX(P+INO(Q))
          Q = Q + 1
 10     CONTINUE
      ELSE
        DO 20 I = 1, NNO
          CNXMA(I) = CNX(P)
          P = P + 1
 20     CONTINUE
      ENDIF
      END
