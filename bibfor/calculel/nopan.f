      SUBROUTINE NOPAN(TYPEMA,NOEPAN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 12/02/2008   AUTEUR ABBAS M.ABBAS 
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
C TOLE CRP_20
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  TYPEMA
      INTEGER      NOEPAN(*)
C      
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C INDICES DES NOEUDS DEFINISSANT LES PANS D'UNE MAILLE 
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEMA : TYPE DE MAILLE
C OUT NOEPAN : NOEUDS DEFINISSANT LES PANS DE LA MAILLE
C                   ( NOMBRE NOEUDS PAN    1, N1, N2, ...
C                     NOMBRE NOEUDS PAN    2, N1, N2, ...)
C                     EN 3D, NB NOEUDS < 0 : TRIANGLE
C                            NB NOEUDS > 0 : QUADRANGLE
C
C INDICE: ORDRE DU NOEUD DANS LA MAILLE 
C    EX: POUR HEXA8 NOEUDS 1 A 8
C 
C ROUTINE SOEUR : NBPAN
C
C ----------------------------------------------------------------------
C
      IF (TYPEMA(1:3).EQ.'SEG') THEN
        NOEPAN(1) = 1
          NOEPAN(2) = 1
        NOEPAN(3) = 1
          NOEPAN(4) = 2
      ELSEIF (TYPEMA(1:4).EQ.'TRIA') THEN
        IF (TYPEMA(5:5).EQ.'3') THEN
          NOEPAN(1) = 2
            NOEPAN(2) = 1
            NOEPAN(3) = 2
          NOEPAN(4) = 2
            NOEPAN(5) = 2
            NOEPAN(6) = 3
          NOEPAN(7) = 2
            NOEPAN(8) = 3
            NOEPAN(9) = 1
        ELSEIF (TYPEMA(5:5).EQ.'6') THEN
          NOEPAN(1) = 3
            NOEPAN(2)  = 1
            NOEPAN(3)  = 2
            NOEPAN(4)  = 4
          NOEPAN(5) = 3
            NOEPAN(6)  = 2
            NOEPAN(7)  = 3
            NOEPAN(8)  = 5
          NOEPAN(9) = 3
            NOEPAN(10) = 3
            NOEPAN(11) = 1
            NOEPAN(12) = 6
        ELSE
          WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
          CALL ASSERT(.FALSE.) 
        ENDIF
      ELSEIF (TYPEMA(1:4).EQ.'QUAD') THEN
        IF (TYPEMA(5:5).EQ.'4') THEN
          NOEPAN(1) = 2
            NOEPAN(2)  = 1
            NOEPAN(3)  = 2
          NOEPAN(4) = 2
            NOEPAN(5)  = 2
            NOEPAN(6)  = 3
          NOEPAN(7) = 2
            NOEPAN(8)  = 3
            NOEPAN(9)  = 4
          NOEPAN(10) = 2
            NOEPAN(11) = 4
            NOEPAN(12) = 1
        ELSEIF (TYPEMA(5:5).EQ.'6') THEN
          NOEPAN(1) = 3
            NOEPAN(2)  = 1
            NOEPAN(3)  = 2
            NOEPAN(4)  = 5
          NOEPAN(5) = 2
            NOEPAN(6)  = 2
            NOEPAN(7)  = 3
          NOEPAN(8) = 3
            NOEPAN(9)  = 3
            NOEPAN(10) = 4
            NOEPAN(11) = 6
          NOEPAN(12) = 2
            NOEPAN(13) = 4
            NOEPAN(14) = 1
        ELSEIF ((TYPEMA(5:5).EQ.'8').OR.(TYPEMA(5:5).EQ.'9')) THEN
          NOEPAN(1) = 3
            NOEPAN(2)  = 1
            NOEPAN(3)  = 2
            NOEPAN(4)  = 5
          NOEPAN(5) = 3
            NOEPAN(6)  = 2
            NOEPAN(7)  = 3
            NOEPAN(8)  = 6
          NOEPAN(9) = 3
            NOEPAN(10) = 3
            NOEPAN(11) = 4
            NOEPAN(12) = 7
          NOEPAN(13) = 3
            NOEPAN(14) = 4
            NOEPAN(15) = 1
            NOEPAN(16) = 8
        ELSE
          WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
          CALL ASSERT(.FALSE.)          
        ENDIF
      ELSEIF (TYPEMA(1:5).EQ.'TETRA') THEN
        IF (TYPEMA(6:6).EQ.'4') THEN
          NOEPAN(1) = -3
            NOEPAN(2)  = 1
            NOEPAN(3)  = 3
            NOEPAN(4)  = 2
          NOEPAN(5) = -3
            NOEPAN(6)  = 1
            NOEPAN(7)  = 2
            NOEPAN(8)  = 4
          NOEPAN(9) = -3
            NOEPAN(10) = 1
            NOEPAN(11) = 4
            NOEPAN(12) = 3
          NOEPAN(13) = -3
            NOEPAN(14) = 2
            NOEPAN(15) = 3
            NOEPAN(16) = 4
        ELSEIF (TYPEMA(6:7).EQ.'10') THEN
          NOEPAN(1) = -6
            NOEPAN(2)  = 1
            NOEPAN(3)  = 3
            NOEPAN(4)  = 2
            NOEPAN(5)  = 7
            NOEPAN(6)  = 6
            NOEPAN(7)  = 5
          NOEPAN(8) = -6
            NOEPAN(9)  = 1
            NOEPAN(10) = 2
            NOEPAN(11) = 4
            NOEPAN(12) = 5
            NOEPAN(13) = 9
            NOEPAN(14) = 8
          NOEPAN(15) = -6
            NOEPAN(16) = 1
            NOEPAN(17) = 4
            NOEPAN(18) = 3
            NOEPAN(19) = 8
            NOEPAN(20) = 10
            NOEPAN(21) = 7
          NOEPAN(22) = -6
            NOEPAN(23) = 2
            NOEPAN(24) = 3
            NOEPAN(25) = 4
            NOEPAN(26) = 6
            NOEPAN(27) = 10
            NOEPAN(28) = 9
        ELSE
          WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
          CALL ASSERT(.FALSE.) 
        ENDIF
      ELSEIF (TYPEMA(1:5).EQ.'PENTA') THEN
        IF (TYPEMA(6:6).EQ.'6') THEN
          NOEPAN(1) = -3
            NOEPAN(2)  = 1
            NOEPAN(3)  = 3
            NOEPAN(4)  = 2
          NOEPAN(5) = 4
            NOEPAN(6)  = 1
            NOEPAN(7)  = 2
            NOEPAN(8)  = 5
            NOEPAN(9)  = 4
          NOEPAN(10) = 4
            NOEPAN(11) = 2
            NOEPAN(12) = 3
            NOEPAN(13) = 6
            NOEPAN(14) = 5
          NOEPAN(15) = 4
            NOEPAN(16) = 1
            NOEPAN(17) = 4
            NOEPAN(18) = 6
            NOEPAN(19) = 3
          NOEPAN(20) = -3
            NOEPAN(21) = 4
            NOEPAN(22) = 5
            NOEPAN(23) = 6
        ELSEIF (TYPEMA(6:7).EQ.'12') THEN
          NOEPAN(1) = -6
            NOEPAN(2)  = 1
            NOEPAN(3)  = 3
            NOEPAN(4)  = 2
            NOEPAN(5)  = 9
            NOEPAN(6)  = 8
            NOEPAN(7)  = 7
          NOEPAN(8) = 6
          NOEPAN(9) = 1
          NOEPAN(10) = 2
          NOEPAN(11) = 5
          NOEPAN(12) = 4
          NOEPAN(13) = 7
          NOEPAN(14) = 10
          NOEPAN(15) = 6
          NOEPAN(16) = 2
          NOEPAN(17) = 3
          NOEPAN(18) = 6
          NOEPAN(19) = 5
          NOEPAN(20) = 8
          NOEPAN(21) = 11
          NOEPAN(22) = 6
          NOEPAN(23) = 3
          NOEPAN(24) = 1
          NOEPAN(25) = 4
          NOEPAN(26) = 6
          NOEPAN(27) = 9
          NOEPAN(28) = 12
          NOEPAN(29) = -6
          NOEPAN(30) = 4
          NOEPAN(31) = 5
          NOEPAN(32) = 6
          NOEPAN(33) = 10
          NOEPAN(34) = 11
          NOEPAN(35) = 12
        ELSEIF (TYPEMA(6:7).EQ.'14') THEN
          NOEPAN(1) = -7
          NOEPAN(2) = 1
          NOEPAN(3) = 3
          NOEPAN(4) = 2
          NOEPAN(5) = 9
          NOEPAN(6) = 8
          NOEPAN(7) = 7
          NOEPAN(8) = 13
          NOEPAN(9) = 6
          NOEPAN(10) = 1
          NOEPAN(11) = 2
          NOEPAN(12) = 5
          NOEPAN(13) = 4
          NOEPAN(14) = 7
          NOEPAN(15) = 10
          NOEPAN(16) = 6
          NOEPAN(17) = 2
          NOEPAN(18) = 3
          NOEPAN(19) = 6
          NOEPAN(20) = 5
          NOEPAN(21) = 8
          NOEPAN(22) = 11
          NOEPAN(23) = 6
          NOEPAN(24) = 3
          NOEPAN(25) = 1
          NOEPAN(26) = 4
          NOEPAN(27) = 6
          NOEPAN(28) = 9
          NOEPAN(29) = 12
          NOEPAN(30) = -7
          NOEPAN(31) = 4
          NOEPAN(32) = 5
          NOEPAN(33) = 6
          NOEPAN(34) = 10
          NOEPAN(35) = 11
          NOEPAN(36) = 12
          NOEPAN(37) = 14
        ELSEIF (TYPEMA(6:7).EQ.'15') THEN
          NOEPAN(1) = -6
          NOEPAN(2) = 1
          NOEPAN(3) = 3
          NOEPAN(4) = 2
          NOEPAN(5) = 9
          NOEPAN(6) = 8
          NOEPAN(7) = 7
          NOEPAN(8) = 8
          NOEPAN(9) = 1
          NOEPAN(10) = 2
          NOEPAN(11) = 5
          NOEPAN(12) = 4
          NOEPAN(13) = 7
          NOEPAN(14) = 11
          NOEPAN(15) = 13
          NOEPAN(16) = 10
          NOEPAN(17) = 8
          NOEPAN(18) = 2
          NOEPAN(19) = 3
          NOEPAN(20) = 6
          NOEPAN(21) = 5
          NOEPAN(22) = 8
          NOEPAN(23) = 12
          NOEPAN(24) = 14
          NOEPAN(25) = 11
          NOEPAN(26) = 8
          NOEPAN(27) = 1
          NOEPAN(28) = 4
          NOEPAN(29) = 6
          NOEPAN(30) = 3
          NOEPAN(31) = 10
          NOEPAN(32) = 15
          NOEPAN(33) = 12
          NOEPAN(34) = 9
          NOEPAN(35) = -6
          NOEPAN(36) = 4
          NOEPAN(37) = 5
          NOEPAN(38) = 6
          NOEPAN(39) = 13
          NOEPAN(40) = 14
          NOEPAN(41) = 15
        ELSE
          WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
          CALL ASSERT(.FALSE.)  
        ENDIF
      ELSEIF (TYPEMA(1:4).EQ.'HEXA') THEN
        IF (TYPEMA(5:5).EQ.'8') THEN
          NOEPAN(1) = 4
          NOEPAN(2) = 1
          NOEPAN(3) = 4
          NOEPAN(4) = 3
          NOEPAN(5) = 2
          NOEPAN(6) = 4
          NOEPAN(7) = 1
          NOEPAN(8) = 2
          NOEPAN(9) = 6
          NOEPAN(10) = 5
          NOEPAN(11) = 4
          NOEPAN(12) = 2
          NOEPAN(13) = 3
          NOEPAN(14) = 7
          NOEPAN(15) = 6
          NOEPAN(16) = 4
          NOEPAN(17) = 3
          NOEPAN(18) = 4
          NOEPAN(19) = 8
          NOEPAN(20) = 7
          NOEPAN(21) = 4
          NOEPAN(22) = 4
          NOEPAN(23) = 1
          NOEPAN(24) = 5
          NOEPAN(25) = 8
          NOEPAN(26) = 4
          NOEPAN(27) = 5
          NOEPAN(28) = 6
          NOEPAN(29) = 7
          NOEPAN(30) = 8
        ELSEIF (TYPEMA(5:6).EQ.'16') THEN
          NOEPAN(1) = 6
          NOEPAN(2) = 1
          NOEPAN(3) = 4
          NOEPAN(4) = 3
          NOEPAN(5) = 2
          NOEPAN(6) = 10
          NOEPAN(7) = 9
          NOEPAN(8) = 6
          NOEPAN(9) = 2
          NOEPAN(10) = 6
          NOEPAN(11) = 5
          NOEPAN(12) = 1
          NOEPAN(13) = 12
          NOEPAN(14) = 11
          NOEPAN(15) = 8
          NOEPAN(16) = 2
          NOEPAN(17) = 3
          NOEPAN(18) = 7
          NOEPAN(19) = 6
          NOEPAN(20) = 9
          NOEPAN(21) = 13
          NOEPAN(22) = 15
          NOEPAN(23) = 12
          NOEPAN(24) = 6
          NOEPAN(25) = 4
          NOEPAN(26) = 8
          NOEPAN(27) = 7
          NOEPAN(28) = 3
          NOEPAN(29) = 14
          NOEPAN(30) = 13
          NOEPAN(31) = 8
          NOEPAN(32) = 4
          NOEPAN(33) = 1
          NOEPAN(34) = 5
          NOEPAN(35) = 8
          NOEPAN(36) = 10
          NOEPAN(37) = 11
          NOEPAN(38) = 16
          NOEPAN(39) = 14
          NOEPAN(40) = 6
          NOEPAN(41) = 6
          NOEPAN(42) = 7
          NOEPAN(43) = 8
          NOEPAN(44) = 5
          NOEPAN(45) = 15
          NOEPAN(46) = 16
        ELSEIF (TYPEMA(5:6).EQ.'18') THEN
          NOEPAN(1) = 6
          NOEPAN(2) = 1
          NOEPAN(3) = 4
          NOEPAN(4) = 3
          NOEPAN(5) = 2
          NOEPAN(6) = 10
          NOEPAN(7) = 9
          NOEPAN(8) = 6
          NOEPAN(9) = 2
          NOEPAN(10) = 6
          NOEPAN(11) = 5
          NOEPAN(12) = 1
          NOEPAN(13) = 12
          NOEPAN(14) = 11
          NOEPAN(15) = 9
          NOEPAN(16) = 2
          NOEPAN(17) = 3
          NOEPAN(18) = 7
          NOEPAN(19) = 6
          NOEPAN(20) = 9
          NOEPAN(21) = 13
          NOEPAN(22) = 15
          NOEPAN(23) = 12
          NOEPAN(24) = 17
          NOEPAN(25) = 6
          NOEPAN(26) = 4
          NOEPAN(27) = 8
          NOEPAN(28) = 7
          NOEPAN(29) = 3
          NOEPAN(30) = 14
          NOEPAN(31) = 13
          NOEPAN(32) = 9
          NOEPAN(33) = 4
          NOEPAN(34) = 1
          NOEPAN(35) = 5
          NOEPAN(36) = 8
          NOEPAN(37) = 10
          NOEPAN(38) = 11
          NOEPAN(39) = 16
          NOEPAN(40) = 14
          NOEPAN(41) = 18
          NOEPAN(42) = 6
          NOEPAN(43) = 6
          NOEPAN(44) = 7
          NOEPAN(45) = 8
          NOEPAN(46) = 5
          NOEPAN(47) = 15
          NOEPAN(48) = 16
        ELSEIF (TYPEMA(5:6).EQ.'20') THEN
          NOEPAN(1) = 8
          NOEPAN(2) = 1
          NOEPAN(3) = 4
          NOEPAN(4) = 3
          NOEPAN(5) = 2
          NOEPAN(6) = 12
          NOEPAN(7) = 11
          NOEPAN(8) = 10
          NOEPAN(9) = 9
          NOEPAN(10) = 8
          NOEPAN(11) = 1
          NOEPAN(12) = 2
          NOEPAN(13) = 6
          NOEPAN(14) = 5
          NOEPAN(15) = 9
          NOEPAN(16) = 14
          NOEPAN(17) = 17
          NOEPAN(18) = 13
          NOEPAN(19) = 8
          NOEPAN(20) = 2
          NOEPAN(21) = 3
          NOEPAN(22) = 7
          NOEPAN(23) = 6
          NOEPAN(24) = 10
          NOEPAN(25) = 15
          NOEPAN(26) = 18
          NOEPAN(27) = 14
          NOEPAN(28) = 8
          NOEPAN(29) = 3
          NOEPAN(30) = 4
          NOEPAN(31) = 8
          NOEPAN(32) = 7
          NOEPAN(33) = 11
          NOEPAN(34) = 16
          NOEPAN(35) = 19
          NOEPAN(36) = 15
          NOEPAN(37) = 8
          NOEPAN(38) = 4
          NOEPAN(39) = 1
          NOEPAN(40) = 5
          NOEPAN(41) = 8
          NOEPAN(42) = 12
          NOEPAN(43) = 13
          NOEPAN(44) = 20
          NOEPAN(45) = 16
          NOEPAN(46) = 8
          NOEPAN(47) = 5
          NOEPAN(48) = 6
          NOEPAN(49) = 7
          NOEPAN(50) = 8
          NOEPAN(51) = 17
          NOEPAN(52) = 18
          NOEPAN(53) = 19
          NOEPAN(54) = 20
        ELSEIF (TYPEMA(5:6).EQ.'27') THEN
          NOEPAN(1) = 9
          NOEPAN(2) = 1
          NOEPAN(3) = 4
          NOEPAN(4) = 3
          NOEPAN(5) = 2
          NOEPAN(6) = 12
          NOEPAN(7) = 11
          NOEPAN(8) = 10
          NOEPAN(9) = 9
          NOEPAN(10) = 21
          NOEPAN(11) = 9
          NOEPAN(12) = 1
          NOEPAN(13) = 2
          NOEPAN(14) = 6
          NOEPAN(15) = 5
          NOEPAN(16) = 9
          NOEPAN(17) = 14
          NOEPAN(18) = 17
          NOEPAN(19) = 13
          NOEPAN(20) = 22
          NOEPAN(21) = 9
          NOEPAN(22) = 2
          NOEPAN(23) = 3
          NOEPAN(24) = 7
          NOEPAN(25) = 6
          NOEPAN(26) = 10
          NOEPAN(27) = 15
          NOEPAN(28) = 18
          NOEPAN(29) = 14
          NOEPAN(30) = 23
          NOEPAN(31) = 9
          NOEPAN(32) = 3
          NOEPAN(33) = 4
          NOEPAN(34) = 8
          NOEPAN(35) = 7
          NOEPAN(36) = 11
          NOEPAN(37) = 16
          NOEPAN(38) = 19
          NOEPAN(39) = 15
          NOEPAN(40) = 24
          NOEPAN(41) = 9
          NOEPAN(42) = 4
          NOEPAN(43) = 1
          NOEPAN(44) = 5
          NOEPAN(45) = 8
          NOEPAN(46) = 12
          NOEPAN(47) = 13
          NOEPAN(48) = 20
          NOEPAN(49) = 16
          NOEPAN(50) = 25
          NOEPAN(51) = 9
          NOEPAN(52) = 5
          NOEPAN(53) = 6
          NOEPAN(54) = 7
          NOEPAN(55) = 8
          NOEPAN(56) = 17
          NOEPAN(57) = 18
          NOEPAN(58) = 19
          NOEPAN(59) = 20
          NOEPAN(60) = 26
        ELSE
          WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
          CALL ASSERT(.FALSE.)  
        ENDIF
      ELSE
        WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
        CALL ASSERT(.FALSE.)
      ENDIF

      END
