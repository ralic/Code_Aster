      SUBROUTINE ORIEM3(MA,TMA,CNO,CNX,CNXC,CNXMA)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C INTEGER       MA        : NUMERO DE LA MAILLE
C CHARACTER*8   TMA       : TYPE DE LA MAILLE
C REAL*8        CNO(3,*)  : COORDONNEES DES NOEUDS DU MAILLAGE
C INTEGER       CNX(*)    : CONNECTIVITE DES MAILLES
C INTEGER       CNXC(*)   : LONGUEUR COMMULEE DE CNX
C
C VARIABLE DE SORTIE
C INTEGER       CNXMA(27) : CONNECTIVITE DE MA DANS LE BON SENS
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- FONCTION
      REAL*8      DDOT

C --- VARIABLES
      CHARACTER*8 TMA
      INTEGER     CNX(*),CNXC(*),CNXMA(*)
      INTEGER     MA,I,NNO,P,Q,A,B,C,D,E,F,G,H
      REAL*8      CNO(3,*),U(3),V(3),W(3),N(3),R
      LOGICAL     L

C --- PARAMETRES
      INTEGER INDEX(5),INO(68)
      DATA INDEX / 1,8,17,27,42 /
      DATA INO / 0,2,1,5,4,3,6, 0,3,2,1,7,6,5,4,8, 0,2,1,3,6,5,4,7,9,8,
     & 0,2,1,3,5,4,8,7,6,9,11,10,14,13,12,  0,3,2,1,4,7,6,5,11,10,9,8,
     & 12,15,14,13,19,18,17,16,20,21,22,23,24,25,26 /

      P = CNXC(MA)
      NNO = CNXC(MA+1) - P

C --- SELECTION SUIVANT TYPE DE MAILLE

      IF ((TMA(1:4).EQ.'TRIA').AND.
     &    ((NNO.EQ.3).OR.(NNO.EQ.6).OR.(NNO.EQ.7))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)

        U(1) = CNO(1,B)-CNO(1,A)
        U(2) = CNO(2,B)-CNO(2,A)
        V(1) = CNO(1,C)-CNO(1,A)
        V(2) = CNO(2,C)-CNO(2,A)

        L = (U(1)*V(2).LE.U(2)*V(1))
        Q = INDEX(1)

      ELSEIF ((TMA(1:4).EQ.'QUAD').AND.
     &        ((NNO.EQ.4).OR.(NNO.EQ.8).OR.(NNO.EQ.9))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)

        U(1) = CNO(1,C)-CNO(1,A)
        U(2) = CNO(2,C)-CNO(2,A)
        V(1) = CNO(1,D)-CNO(1,B)
        V(2) = CNO(2,D)-CNO(2,B)

        L = (U(1)*V(2).LE.U(2)*V(1))
        Q = INDEX(2)

      ELSEIF ((TMA(1:5).EQ.'TETRA').AND.
     &        ((NNO.EQ.4).OR.(NNO.EQ.10))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)

        U(1) = CNO(1,B)-CNO(1,A)
        U(2) = CNO(2,B)-CNO(2,A)
        U(3) = CNO(3,B)-CNO(3,A)
        V(1) = CNO(1,C)-CNO(1,A)
        V(2) = CNO(2,C)-CNO(2,A)
        V(3) = CNO(3,C)-CNO(3,A)
        W(1) = CNO(1,D)-CNO(1,A)
        W(2) = CNO(2,D)-CNO(2,A)
        W(3) = CNO(3,D)-CNO(3,A)

        CALL PROVEC(V,W,N)
        L = (DDOT(3,U,1,N,1).LE.0.D0)
        Q = INDEX(3)

      ELSEIF ((TMA(1:5).EQ.'PENTA').AND.
     &        ((NNO.EQ.6).OR.(NNO.EQ.15))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)
        E = CNX(P+4)
        F = CNX(P+5)

        U(1) = CNO(1,D)+CNO(1,E)+CNO(1,F)-CNO(1,A)-CNO(1,B)-CNO(1,C)
        U(2) = CNO(2,D)+CNO(2,E)+CNO(2,F)-CNO(2,A)-CNO(2,B)-CNO(2,C)
        U(3) = CNO(3,D)+CNO(3,E)+CNO(3,F)-CNO(3,A)-CNO(3,B)-CNO(3,C)
        V(1) = CNO(1,E)+CNO(1,B)-CNO(1,D)-CNO(1,A)
        V(2) = CNO(2,E)+CNO(2,B)-CNO(2,D)-CNO(2,A)
        V(3) = CNO(3,E)+CNO(3,B)-CNO(3,D)-CNO(3,A)
        W(1) = CNO(1,F)+CNO(1,C)-CNO(1,D)-CNO(1,A)
        W(2) = CNO(2,F)+CNO(2,C)-CNO(2,D)-CNO(2,A)
        W(3) = CNO(3,F)+CNO(3,C)-CNO(3,D)-CNO(3,A)

        CALL PROVEC(V,W,N)
        L = (DDOT(3,U,1,N,1).LE.0.D0)
        Q = INDEX(4)

      ELSEIF ((TMA(1:4).EQ.'HEXA').AND.
     &        ((NNO.EQ.8).OR.(NNO.EQ.20).OR.(NNO.EQ.27))) THEN

        A = CNX(P)
        B = CNX(P+1)
        C = CNX(P+2)
        D = CNX(P+3)
        E = CNX(P+4)
        F = CNX(P+5)
        G = CNX(P+6)
        H = CNX(P+7)

        U(1) = CNO(1,G)+CNO(1,C)-CNO(1,E)-CNO(1,A)
        U(2) = CNO(2,G)+CNO(2,C)-CNO(2,E)-CNO(2,A)
        U(3) = CNO(3,G)+CNO(3,C)-CNO(3,E)-CNO(3,A)
        V(1) = CNO(1,H)+CNO(1,D)-CNO(1,F)-CNO(1,B)
        V(2) = CNO(2,H)+CNO(2,D)-CNO(2,F)-CNO(2,B)
        V(3) = CNO(3,H)+CNO(3,D)-CNO(3,F)-CNO(3,B)
        W(1) = CNO(1,F)+CNO(1,E)-CNO(1,D)-CNO(1,C)
        W(2) = CNO(2,F)+CNO(2,E)-CNO(2,D)-CNO(2,C)
        W(3) = CNO(3,F)+CNO(3,E)-CNO(3,D)-CNO(3,C)

        CALL PROVEC(V,W,N)
        L = (DDOT(3,U,1,N,1).LE.0.D0)
        Q = INDEX(5)

      ELSE

        CALL U2MESK('F','MODELISA5_93',1,TMA)

      ENDIF

C --- PERMUTATION

      IF (L) THEN

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
