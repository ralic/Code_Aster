      SUBROUTINE NOEUD(TYPEMA,NO,NNO,DIME)

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C COORDONNEES DES NOEUDS DES MAILLES DE REFERENCE
C ----------------------------------------------------------------------
C VARIABLES D'ENTREE
C CHARACTER*8  TYPEMA       : TYPE DE LA MAILLE
C
C VARIABLES DE SORTIE
C REAL*8       NO(DIME,NNO) : COORDONNEES NOEUDS DES MAILLES DE
C                             REFERENCE (X1,[Y1],[Z1],X2,...)
C INTEGER      NNO          : NOMBRE DE NOEUDS
C INTEGER      DIME         : DIMENSION DE LA MAILLE
C ----------------------------------------------------------------------

      IMPLICIT NONE

C --- VARIABLES
      CHARACTER*8  TYPEMA
      REAL*8       NO(*)
      INTEGER      NNO,DIME

      IF (TYPEMA(1:3).EQ.'SEG') THEN
          DIME   =  1
          NO(1)  = -1.D0
          NO(2)  =  1.D0
          NNO    =  2
        IF (TYPEMA(4:4).EQ.'3') THEN
          NO(3)  =  0.D0
          NNO    =  3
        ENDIF

      ELSEIF ((TYPEMA(1:4).EQ.'TRIA')) THEN
          DIME   =  2
          NO(1)  =  0.D0
          NO(2)  =  0.D0
          NO(3)  =  1.D0
          NO(4)  =  0.D0
          NO(5)  =  0.D0
          NO(6)  =  1.D0
          NNO    =  3
        IF (TYPEMA(5:5).NE.'3') THEN
          NO(7)  =  0.5D0
          NO(8)  =  0.D0
          NO(9)  =  0.5D0
          NO(10) =  0.5D0
          NO(11) =  0.D0
          NO(12) =  0.5D0
          NNO    =  6
        IF (TYPEMA(5:5).EQ.'7') THEN
          NO(13) =  1.D0/3.D0
          NO(14) =  NO(13)
          NNO    =  7
        ENDIF
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
          DIME   =  2
          NO(1)  = -1.D0
          NO(2)  = -1.D0
          NO(3)  =  1.D0
          NO(4)  = -1.D0
          NO(5)  =  1.D0
          NO(6)  =  1.D0
          NO(7)  = -1.D0
          NO(8)  =  1.D0
          NNO    =  4
        IF (TYPEMA(5:5).EQ.'6') THEN
          NO(9)  =  0.D0
          NO(10) = -1.D0
          NO(11) =  0.D0
          NO(12) =  1.D0
          NNO    =  6
        ELSEIF (TYPEMA(5:5).NE.'4') THEN
          NO(9)  =  0.D0
          NO(10) = -1.D0
          NO(11) =  1.D0
          NO(12) =  0.D0
          NO(13) =  0.D0
          NO(14) =  1.D0
          NO(15) = -1.D0
          NO(16) =  0.D0
          NNO    =  8
        IF (TYPEMA(5:5).EQ.'9') THEN
          NO(17) =  0.D0
          NO(18) =  0.D0
          NNO    =  9
        ENDIF
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
          DIME   =  3
          NO(1)  =  0.D0
          NO(2)  =  1.D0
          NO(3)  =  0.D0
          NO(4)  =  0.D0
          NO(5)  =  0.D0
          NO(6)  =  1.D0
          NO(7)  =  0.D0
          NO(8)  =  0.D0
          NO(9)  =  0.D0
          NO(10) =  1.D0
          NO(11) =  0.D0
          NO(12) =  0.D0
          NNO    =  4
        IF (TYPEMA(6:7).EQ.'10') THEN
          NO(13) =  0.D0
          NO(14) =  0.5D0
          NO(15) =  0.5D0
          NO(16) =  0.D0
          NO(17) =  0.D0
          NO(18) =  0.5D0
          NO(19) =  0.D0
          NO(20) =  0.5D0
          NO(21) =  0.D0
          NO(22) =  0.5D0
          NO(23) =  0.5D0
          NO(24) =  0.D0
          NO(25) =  0.5D0
          NO(26) =  0.D0
          NO(27) =  0.5D0
          NO(28) =  0.5D0
          NO(29) =  0.D0
          NO(30) =  0.D0
          NNO    =  10
        ENDIF

       ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
          DIME   =  3
          NO(1)  = -1.D0
          NO(2)  =  1.D0
          NO(3)  =  0.D0
          NO(4)  = -1.D0
          NO(5)  =  0.D0
          NO(6)  =  1.D0
          NO(7)  = -1.D0
          NO(8)  =  0.D0
          NO(9)  =  0.D0
          NO(10) =  1.D0
          NO(11) =  1.D0
          NO(12) =  0.D0
          NO(13) =  1.D0
          NO(14) =  0.D0
          NO(15) =  1.D0
          NO(16) =  1.D0
          NO(17) =  0.D0
          NO(18) =  0.D0
          NNO    =  6
        IF (TYPEMA(6:6).EQ.'1') THEN
        IF (TYPEMA(7:7).NE.'5') THEN
          NO(19) = -1.D0
          NO(20) =  0.5D0
          NO(21) =  0.5D0
          NO(22) = -1.D0
          NO(23) =  0.D0
          NO(24) =  0.5D0
          NO(25) = -1.D0
          NO(26) =  0.5D0
          NO(27) =  0.D0
          NO(28) =  1.D0
          NO(29) =  0.5D0
          NO(30) =  0.5D0
          NO(31) =  1.D0
          NO(32) =  0.D0
          NO(33) =  0.5D0
          NO(34) =  1.D0
          NO(35) =  0.5D0
          NO(36) =  0.D0
          NNO    =  12
        IF (TYPEMA(7:7).EQ.'4') THEN
          NO(37) = -1.D0
          NO(38) =  1.D0/3.D0
          NO(39) =  NO(38)
          NO(40) =  1.D0
          NO(41) =  NO(38)
          NO(42) =  NO(38)
          NNO    =  14
        ENDIF
        ELSE
          NO(19) = -1.D0
          NO(20) =  0.5D0
          NO(21) =  0.5D0
          NO(22) = -1.D0
          NO(23) =  0.D0
          NO(24) =  0.5D0
          NO(25) = -1.D0
          NO(26) =  0.5D0
          NO(27) =  0.D0
          NO(28) =  0.D0
          NO(29) =  1.D0
          NO(30) =  0.D0
          NO(31) =  0.D0
          NO(32) =  0.D0
          NO(33) =  1.D0
          NO(34) =  0.D0
          NO(35) =  0.D0
          NO(36) =  0.D0
          NO(37) =  1.D0
          NO(38) =  0.5D0
          NO(39) =  0.5D0
          NO(40) =  1.D0
          NO(41) =  0.D0
          NO(42) =  0.5D0
          NO(43) =  1.D0
          NO(44) =  0.5D0
          NO(45) =  0.D0
          NNO    =  15
        ENDIF
        ENDIF

      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
          DIME   =  3
          NO(1)  = -1.D0
          NO(2)  = -1.D0
          NO(3)  = -1.D0
          NO(4)  =  1.D0
          NO(5)  = -1.D0
          NO(6)  = -1.D0
          NO(7)  =  1.D0
          NO(8)  =  1.D0
          NO(9)  = -1.D0
          NO(10) = -1.D0
          NO(11) =  1.D0
          NO(12) = -1.D0
          NO(13) = -1.D0
          NO(14) = -1.D0
          NO(15) =  1.D0
          NO(16) =  1.D0
          NO(17) = -1.D0
          NO(18) =  1.D0
          NO(19) =  1.D0
          NO(20) =  1.D0
          NO(21) =  1.D0
          NO(22) = -1.D0
          NO(23) =  1.D0
          NO(24) =  1.D0
          NNO    =  8
        IF (TYPEMA(5:5).EQ.'1') THEN
          NO(25) =  1.D0
          NO(26) =  0.D0
          NO(27) = -1.D0
          NO(28) = -1.D0
          NO(29) =  0.D0
          NO(30) = -1.D0
          NO(31) = -1.D0
          NO(32) = -1.D0
          NO(33) =  0.D0
          NO(34) =  1.D0
          NO(35) = -1.D0
          NO(36) =  0.D0
          NO(37) =  1.D0
          NO(38) =  1.D0
          NO(39) =  0.D0
          NO(40) = -1.D0
          NO(41) =  1.D0
          NO(42) =  0.D0
          NO(43) =  1.D0
          NO(44) =  0.D0
          NO(45) =  1.D0
          NO(46) = -1.D0
          NO(47) =  0.D0
          NO(48) =  1.D0
          NNO    =  16
        IF (TYPEMA(6:6).EQ.'8') THEN
          NO(49) =  1.D0
          NO(50) =  0.D0
          NO(51) =  0.D0
          NO(52) = -1.D0
          NO(53) =  0.D0
          NO(54) =  0.D0
          NNO    =  18
        ENDIF
        ELSEIF (TYPEMA(5:5).EQ.'2') THEN
          NO(25) =  0.D0
          NO(26) = -1.D0
          NO(27) = -1.D0
          NO(28) =  1.D0
          NO(29) =  0.D0
          NO(30) = -1.D0
          NO(31) =  0.D0
          NO(32) =  1.D0
          NO(33) = -1.D0
          NO(34) = -1.D0
          NO(35) =  0.D0
          NO(36) = -1.D0
          NO(37) = -1.D0
          NO(38) = -1.D0
          NO(39) =  0.D0
          NO(40) =  1.D0
          NO(41) = -1.D0
          NO(42) =  0.D0
          NO(43) =  1.D0
          NO(44) =  1.D0
          NO(45) =  0.D0
          NO(46) = -1.D0
          NO(47) =  1.D0
          NO(48) =  0.D0
          NO(49) =  0.D0
          NO(50) = -1.D0
          NO(51) =  1.D0
          NO(52) =  1.D0
          NO(53) =  0.D0
          NO(54) =  1.D0
          NO(55) =  0.D0
          NO(56) =  1.D0
          NO(57) =  1.D0
          NO(58) = -1.D0
          NO(59) =  0.D0
          NO(60) =  1.D0
          NNO    =  20
        IF (TYPEMA(6:6).EQ.'7') THEN
          NO(61) =  0.D0
          NO(62) =  0.D0
          NO(63) = -1.D0
          NO(64) =  0.D0
          NO(65) = -1.D0
          NO(66) =  0.D0
          NO(67) =  1.D0
          NO(68) =  0.D0
          NO(69) =  0.D0
          NO(70) =  0.D0
          NO(71) =  1.D0
          NO(72) =  0.D0
          NO(73) = -1.D0
          NO(74) =  0.D0
          NO(75) =  0.D0
          NO(76) =  0.D0
          NO(77) =  0.D0
          NO(78) =  1.D0
          NO(79) =  0.D0
          NO(80) =  0.D0
          NO(81) =  0.D0
          NNO    =  27
        ENDIF
        ENDIF

      ELSEIF (TYPEMA(1:5).EQ.'PYRAM') THEN
          DIME   =  3
          NO(1)  =  1.D0
          NO(2)  =  0.D0
          NO(3)  =  0.D0
          NO(4)  =  0.D0
          NO(5)  =  1.D0
          NO(6)  =  0.D0
          NO(7)  = -1.D0
          NO(8)  =  0.D0
          NO(9)  =  0.D0
          NO(10) =  0.D0
          NO(11) = -1.D0
          NO(12) =  0.D0
          NO(13) =  0.D0
          NO(14) =  0.D0
          NO(15) =  1.D0
          NNO    =  5
        IF (TYPEMA(6:7).EQ.'13') THEN
          NO(16) =  0.5D0
          NO(17) =  0.5D0
          NO(18) =  0.D0
          NO(19) = -0.5D0
          NO(20) =  0.5D0
          NO(21) =  0.D0
          NO(22) = -0.5D0
          NO(23) = -0.5D0
          NO(24) =  0.D0
          NO(25) =  0.5D0
          NO(26) = -0.5D0
          NO(27) =  0.D0
          NO(28) =  0.5D0
          NO(29) =  0.D0
          NO(30) =  0.5D0
          NO(31) =  0.D0
          NO(32) =  0.5D0
          NO(33) =  0.5D0
          NO(34) = -0.5D0
          NO(35) =  0.D0
          NO(36) =  0.5D0
          NO(37) =  0.D0
          NO(38) = -0.5D0
          NO(39) =  0.5D0
          NNO    =  13
        ENDIF

      ELSE
        CALL U2MESK('F','CALCULEL2_31',1,TYPEMA)
      ENDIF

      END
