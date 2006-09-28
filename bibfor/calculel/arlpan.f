      SUBROUTINE ARLPAN(TMA,ARE,NARE,NPAN)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
C ======================================================================
C COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C
C ======================================================================
C
C ----------------------------------------------------------------------
C              ARETES LINEAIRES DES FACES DES MAILLES 3D
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8  TMA  : TYPE DE MAILLE
C
C VARIABLE DE SORTIE
C INTEGER ARE(*)    : ARETES LINEAIRES ASSOCIEES AUX FACES
C                    ( NB ARETES FACE.1, FACE.1.ARETE.1, FACE.1.ARETE.2,
C                      ..., NB ARETES FACE.2, FACE.2.ARETE.1, ... )
C                      FACE.* INDEX SUIVANT PANNO
C                      ARETE.* INDEX SUIVANT NOARET
C                      SI ARETE.* > 0 : MEME SENS QUE DANS NOAREL
C                      SI ARETE.* < 0 : SENS OPPOSE
C INTEGER NPAN      : NOMBRE DE FACES
C INTEGER NARE      : NOMBRE D'ARETES

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8  TMA
      INTEGER      ARE(*),NPAN,NARE

      IF (TMA(1:5).EQ.'TETRA') THEN
        NARE = 6
        NPAN = 4
        ARE(1) = 3
          ARE(2) = -3
          ARE(3) = -2
          ARE(4) = -1
        ARE(5) = 3
          ARE(6) = 1
          ARE(7) = -5
          ARE(8) = -4
        ARE(9) = 3
          ARE(10) = 4
          ARE(11) = 6
          ARE(12) = 3
        ARE(13) = 3
          ARE(14) = 2
          ARE(15) = -6
          ARE(16) = 5
      ELSEIF (TMA(1:5).EQ.'PENTA') THEN
        NARE = 9
        NPAN = 5
        IF ((TMA(6:7).EQ.'12').OR.(TMA(6:7).EQ.'14')) THEN
          ARE(1) = 3
            ARE(2) = -3
            ARE(3) = -2
            ARE(4) = -1
          ARE(5) = 4
            ARE(6) = 1
            ARE(7) = 5
            ARE(8) = -7
            ARE(9) = -4
          ARE(10) = 4
            ARE(11) = 2
            ARE(12) = 6
            ARE(13) = -8
            ARE(14) = -5
          ARE(15) = 4
            ARE(16) = 3
            ARE(17) = 4
            ARE(18) = -9
            ARE(19) = -6
          ARE(20) = 3
            ARE(21) = 7
            ARE(22) = 8
            ARE(23) = 9
        ELSE
          ARE(1) = 3
            ARE(2) = -3
            ARE(3) = -2
            ARE(4) = -1
          ARE(5) = 4
            ARE(6) = 1
            ARE(7) = 5
            ARE(8) = -7
            ARE(9) = -4
          ARE(10) = 4
            ARE(11) = 2
            ARE(12) = 6
            ARE(13) = -8
            ARE(14) = -5
          ARE(15) = 4
            ARE(16) = 4
            ARE(17) = -9
            ARE(18) = -6
            ARE(19) = 3
          ARE(20) = 3
            ARE(21) = 7
            ARE(22) = 8
            ARE(23) = 9
        ENDIF
      ELSEIF (TMA(1:4).EQ.'HEXA') THEN
        NARE = 12
        NPAN = 6
        IF ((TMA(5:6).EQ.'16').OR.(TMA(5:6).EQ.'18')) THEN
          ARE(1) = 4
            ARE(2) = -4
            ARE(3) = -3
            ARE(4) = -2
            ARE(5) = -1
          ARE(6) = 4
            ARE(7) = 6
            ARE(8) = -9
            ARE(9) = -5
            ARE(10) = 1
          ARE(11) = 4
            ARE(12) = 2
            ARE(13) = 7
            ARE(14) = -10
            ARE(15) = -6
          ARE(16) = 4
            ARE(17) = 8
            ARE(18) = -11
            ARE(19) = -7
            ARE(20) = 3
          ARE(21) = 4
            ARE(22) = 4
            ARE(23) = 5
            ARE(24) = -12
            ARE(25) = -8
          ARE(26) = 4
            ARE(27) = 10
            ARE(28) = 11
            ARE(29) = 12
            ARE(30) = 9
        ELSE
          ARE(1) = 4
            ARE(2) = -4
            ARE(3) = -3
            ARE(4) = -2
            ARE(5) = -1
          ARE(6) = 4
            ARE(7) = 1
            ARE(8) = 6
            ARE(9) = -9
            ARE(10) = -5
          ARE(11) = 4
            ARE(12) = 2
            ARE(13) = 7
            ARE(14) = -10
            ARE(15) = -6
          ARE(16) = 4
            ARE(17) = 3
            ARE(18) = 8
            ARE(19) = -11
            ARE(20) = -7
          ARE(21) = 4
            ARE(22) = 4
            ARE(23) = 5
            ARE(24) = -12
            ARE(25) = -8
          ARE(26) = 4
            ARE(27) = 9
            ARE(28) = 10
            ARE(29) = 11
            ARE(30) = 12
        ENDIF
      ELSE
        CALL U2MESS('F','CALCULEL_16')
      ENDIF

      END
