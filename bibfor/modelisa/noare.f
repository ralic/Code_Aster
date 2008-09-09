      SUBROUTINE NOARE(TYPEMA,NOEARE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 01/09/2008   AUTEUR MEUNIER S.MEUNIER 
C ======================================================================
C COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
      INTEGER     NOEARE(*)
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C INDICES DES NOEUDS DEFINISSANT LES ARETES D'UNE MAILLE
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEMA : TYPE DE MAILLE
C OUT NOEARE : NOEUDS DEFINISSANT LES ARETES DE LA MAILLE
C                           ( NOMBRE DE NOEUDS ARETES 1, N1, N2, ...
C                             NOMBRE DE NOEUDS ARETES 2, ... )
C
C INDICE DES NOEUDS: ORDRE DU NOEUD DANS LA MAILLE
C                     (EX: POUR HEXA8 NOEUDS 1 A 8)
C
C ROUTINE SOEUR : NBARE
C
C ----------------------------------------------------------------------
C
      IF (TYPEMA(1:5).EQ.'TRIA3') THEN
        NOEARE(1) = 2
          NOEARE(2) = 1
          NOEARE(3) = 2
        NOEARE(4) = 2
          NOEARE(5) = 2
          NOEARE(6) = 3
        NOEARE(7) = 2
          NOEARE(8) = 3
          NOEARE(9) = 1
      ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN
        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 4
        NOEARE(5) = 3
          NOEARE(6) = 2
          NOEARE(7) = 3
          NOEARE(8) = 5
        NOEARE(9) = 3
          NOEARE(10) = 3
          NOEARE(11) = 1
          NOEARE(12) = 6
      ELSEIF (TYPEMA(1:5).EQ.'QUAD4') THEN
        NOEARE(1) = 2
          NOEARE(2) = 1
          NOEARE(3) = 2
        NOEARE(4) = 2
          NOEARE(5) = 2
          NOEARE(6) = 3
        NOEARE(7) = 2
          NOEARE(8) = 3
          NOEARE(9) = 4
        NOEARE(10) = 2
          NOEARE(11) = 4
          NOEARE(12) = 1
      ELSEIF (TYPEMA(1:5).EQ.'QUAD6') THEN
        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 5
        NOEARE(5) = 2
          NOEARE(6) = 2
          NOEARE(7) = 3
        NOEARE(8) = 3
          NOEARE(9) = 3
          NOEARE(10) = 4
          NOEARE(11) = 6
        NOEARE(12) = 2
          NOEARE(13) = 4
          NOEARE(14) = 1
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 5
        NOEARE(5) = 3
          NOEARE(6) = 2
          NOEARE(7) = 3
          NOEARE(8) = 6
        NOEARE(9) = 3
          NOEARE(10) = 3
          NOEARE(11) = 4
          NOEARE(12) = 7
        NOEARE(13) = 3
          NOEARE(14) = 4
          NOEARE(15) = 1
          NOEARE(16) = 8
      ELSEIF (TYPEMA(1:6).EQ.'TETRA4') THEN
        NOEARE(1) = 2
          NOEARE(2) = 1
          NOEARE(3) = 2
        NOEARE(4) = 2
          NOEARE(5) = 2
          NOEARE(6) = 3

        NOEARE(7) = 2
          NOEARE(8) = 3
          NOEARE(9) = 1

        NOEARE(10) = 2
          NOEARE(11) = 1
          NOEARE(12) = 4

        NOEARE(13) = 2
          NOEARE(14) = 4
          NOEARE(15) = 2

        NOEARE(16) = 2
          NOEARE(17) = 4
          NOEARE(18) = 3

      ELSEIF (TYPEMA(1:7).EQ.'TETRA10') THEN

        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 5

        NOEARE(5) = 3
          NOEARE(6) = 2
          NOEARE(7) = 3
          NOEARE(8) = 6

        NOEARE(9) = 3
          NOEARE(10) = 3
          NOEARE(11) = 1
          NOEARE(12) = 7

        NOEARE(13) = 3
          NOEARE(14) = 1
          NOEARE(15) = 4
          NOEARE(16) = 8

        NOEARE(17) = 3
          NOEARE(18) = 4
          NOEARE(19) = 2
          NOEARE(20) = 9

        NOEARE(21) = 3
          NOEARE(22) = 4
          NOEARE(23) = 3
          NOEARE(24) = 10

      ELSEIF (TYPEMA(1:6).EQ.'PENTA6') THEN

        NOEARE(1) = 2
          NOEARE(2) = 1
          NOEARE(3) = 2

        NOEARE(4) = 2
          NOEARE(5) = 2
          NOEARE(6) = 3

        NOEARE(7) = 2
          NOEARE(8) = 3
          NOEARE(9) = 1

        NOEARE(10) = 2
          NOEARE(11) = 1
          NOEARE(12) = 4

        NOEARE(13) = 2
          NOEARE(14) = 2
          NOEARE(15) = 5

        NOEARE(16) = 2
          NOEARE(17) = 3
          NOEARE(18) = 6

        NOEARE(19) = 2
          NOEARE(20) = 4
          NOEARE(21) = 5

        NOEARE(22) = 2
          NOEARE(23) = 5
          NOEARE(24) = 6

        NOEARE(25) = 2
          NOEARE(26) = 6
          NOEARE(27) = 4

      ELSEIF ((TYPEMA(1:7).EQ.'PENTA12').OR.
     &        (TYPEMA(1:7).EQ.'PENTA14')) THEN

        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 7

        NOEARE(5) = 3
          NOEARE(6) = 2
          NOEARE(7) = 3
          NOEARE(8) = 8

        NOEARE(9) = 3
          NOEARE(10) = 3
          NOEARE(11) = 1
          NOEARE(12) = 9

        NOEARE(13) = 2
          NOEARE(14) = 1
          NOEARE(15) = 4

        NOEARE(16) = 2
          NOEARE(17) = 2
          NOEARE(18) = 5

        NOEARE(19) = 2
          NOEARE(20) = 3
          NOEARE(21) = 6

        NOEARE(22) = 3
          NOEARE(23) = 4
          NOEARE(24) = 5
          NOEARE(25) = 10

        NOEARE(26) = 3
          NOEARE(27) = 5
          NOEARE(28) = 6
          NOEARE(29) = 11

        NOEARE(30) = 3
          NOEARE(31) = 6
          NOEARE(32) = 4
          NOEARE(33) = 12

      ELSEIF (TYPEMA(1:7).EQ.'PENTA15') THEN

        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 7

        NOEARE(5) = 3
          NOEARE(6) = 2
          NOEARE(7) = 3
          NOEARE(8) = 8

        NOEARE(9) = 3
          NOEARE(10) = 3
          NOEARE(11) = 1
          NOEARE(12) = 9

        NOEARE(13) = 3
          NOEARE(14) = 1
          NOEARE(15) = 4
          NOEARE(16) = 10

        NOEARE(17) = 3
          NOEARE(18) = 2
          NOEARE(19) = 5
          NOEARE(20) = 11

        NOEARE(21) = 3
          NOEARE(22) = 3
          NOEARE(23) = 6
          NOEARE(24) = 12

        NOEARE(25) = 3
          NOEARE(26) = 4
          NOEARE(27) = 5
          NOEARE(28) = 13

        NOEARE(29) = 3
          NOEARE(30) = 5
          NOEARE(31) = 6
          NOEARE(32) = 14

        NOEARE(33) = 3
          NOEARE(34) = 6
          NOEARE(35) = 4
          NOEARE(36) = 15

      ELSEIF (TYPEMA(1:5).EQ.'HEXA8') THEN

        NOEARE(1) = 2
          NOEARE(2) = 1
          NOEARE(3) = 2

        NOEARE(4) = 2
          NOEARE(5) = 2
          NOEARE(6) = 3

        NOEARE(7) = 2
          NOEARE(8) = 3
          NOEARE(9) = 4

        NOEARE(10) = 2
          NOEARE(11) = 4
          NOEARE(12) = 1

        NOEARE(13) = 2
          NOEARE(14) = 1
          NOEARE(15) = 5

        NOEARE(16) = 2
          NOEARE(17) = 2
          NOEARE(18) = 6

        NOEARE(19) = 2
          NOEARE(20) = 3
          NOEARE(21) = 7

        NOEARE(22) = 2
          NOEARE(23) = 4
          NOEARE(24) = 8

        NOEARE(25) = 2
          NOEARE(26) = 5
          NOEARE(27) = 6

        NOEARE(28) = 2
          NOEARE(29) = 6
          NOEARE(30) = 7

        NOEARE(31) = 2
          NOEARE(32) = 7
          NOEARE(33) = 8

        NOEARE(34) = 2
          NOEARE(35) = 8
          NOEARE(36) = 5

      ELSEIF((TYPEMA(1:6).EQ.'HEXA16').OR.(TYPEMA(1:6).EQ.'HEXA18'))THEN

        NOEARE(1) = 2
          NOEARE(2) = 1
          NOEARE(3) = 2

        NOEARE(4) = 3
          NOEARE(5) = 2
          NOEARE(6) = 3
          NOEARE(7) = 9

        NOEARE(8) = 2
          NOEARE(9) = 3
          NOEARE(10) = 4

        NOEARE(11) = 3
          NOEARE(12) = 4
          NOEARE(13) = 1
          NOEARE(14) = 10

        NOEARE(15) = 3
          NOEARE(16) = 1
          NOEARE(17) = 5
          NOEARE(18) = 11

        NOEARE(19) = 3
          NOEARE(20) = 2
          NOEARE(21) = 6
          NOEARE(22) = 12

        NOEARE(23) = 3
          NOEARE(24) = 3
          NOEARE(25) = 7
          NOEARE(26) = 13

        NOEARE(27) = 3
          NOEARE(28) = 4
          NOEARE(29) = 8
          NOEARE(30) = 14

        NOEARE(31) = 2
          NOEARE(32) = 5
          NOEARE(33) = 6

        NOEARE(34) = 3
          NOEARE(35) = 6
          NOEARE(36) = 7
          NOEARE(37) = 15

        NOEARE(38) = 2
          NOEARE(39) = 7
          NOEARE(40) = 8

        NOEARE(41) = 3
          NOEARE(42) = 8
          NOEARE(43) = 5
          NOEARE(44) = 16

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN

        NOEARE(1) = 3
          NOEARE(2) = 1
          NOEARE(3) = 2
          NOEARE(4) = 9

        NOEARE(5) = 3
          NOEARE(6) = 2
          NOEARE(7) = 3
          NOEARE(8) = 10

        NOEARE(9) = 3
          NOEARE(10) = 3
          NOEARE(11) = 4
          NOEARE(12) = 11

        NOEARE(13) = 3
          NOEARE(14) = 4
          NOEARE(15) = 1
          NOEARE(16) = 12

        NOEARE(17) = 3
          NOEARE(18) = 1
          NOEARE(19) = 5
          NOEARE(20) = 13

        NOEARE(21) = 3
          NOEARE(22) = 2
          NOEARE(23) = 6
          NOEARE(24) = 14

        NOEARE(25) = 3
          NOEARE(26) = 3
          NOEARE(27) = 7
          NOEARE(28) = 15

        NOEARE(29) = 3
          NOEARE(30) = 4
          NOEARE(31) = 8
          NOEARE(32) = 16

        NOEARE(33) = 3
          NOEARE(34) = 5
          NOEARE(35) = 6
          NOEARE(36) = 17

        NOEARE(37) = 3
          NOEARE(38) = 6
          NOEARE(39) = 7
          NOEARE(40) = 18

        NOEARE(41) = 3
          NOEARE(42) = 7
          NOEARE(43) = 8
          NOEARE(44) = 19

        NOEARE(45) = 3
          NOEARE(46) = 8
          NOEARE(47) = 5
          NOEARE(48) = 20

      ELSE
        CALL U2MESK('F','ARLEQUIN_22',1,TYPEMA)
      ENDIF

      END
