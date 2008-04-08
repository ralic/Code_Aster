      SUBROUTINE COTANG(NBNO,DIME,TYPTAN,TANMAX,COETAN)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
C RESPONSABLE MEUNIER S.MEUNIER
C
      IMPLICIT NONE
      INTEGER DIME
      INTEGER NBNO
      INTEGER TANMAX
      REAL*8  COETAN(TANMAX)
      CHARACTER*8 TYPTAN
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C COEFFICIENTS POUR LE CALCUL DES TANGENTES
C
C ----------------------------------------------------------------------
C
C
C IN  NBNO   : NOMBRE DE NOEUDS MAILLE MOYENNE
C IN  DIME   : DIMENSION DE L'ESPACE
C I/O TYPTAN : "VARIABLE" -> TANGENTES VARIABLES SUR LA MAILLE
C              "CONSTANT" -> TANGENTES CONSTANTES SUR LA MAILLE
C OUT COETAN : COEF POUR LE CALCUL DES TANGENTES
C               DIMENSION DE COETAN: (NBNO,DIME-1,*)
C           ( C1.1.1,C1.1.2,...,[C1.2.1,C1.2.2,...],
C             [C2.1.1,C2.1.2,...,[C2.2.1,...]] )
C
C    FORME Ca.b.c
C          a: NUMERO DU NOEUD (VAUT 1 SI TANGENTES
C             CONSTANTES SUR LA MAILLE)
C          b: NUMERO DE LA TANGENTE (1 OU 2 TANGENTES SUIVANT 1D OU 2D)
C          c: COEFFICIENT DE LA TANGENTE
C
C             C1.1.* COEF * TANG 1 (NOEUD 1 OU CSTE)
C             C1.*.1 COEF 1 TANG * (NOEUD 1 OU CSTE)
C             C*.1.1 COEF 1 TANG 1 NOEUD *
C
C ----------------------------------------------------------------------
C
      INTEGER I
C
C ----------------------------------------------------------------------
C
      DO 10 I = 1, TANMAX
        COETAN(I) = 0.D0
 10   CONTINUE
C
      IF (DIME.EQ.2) THEN
        IF (NBNO.EQ.3) THEN
          COETAN(1) = -3.D0
          COETAN(2) = -1.D0
          COETAN(3) =  4.D0
          COETAN(4) =  1.D0
          COETAN(5) =  3.D0
          COETAN(6) = -4.D0
          COETAN(7) = -1.D0
          COETAN(8) =  1.D0
          COETAN(9) =  0.D0
          TYPTAN    = 'VARIABLE'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE IF (DIME.EQ.3) THEN
        IF (NBNO.EQ.3) THEN
          COETAN(1) = -1.D0
          COETAN(2) =  1.D0
          COETAN(3) =  0.D0
          COETAN(4) = -1.D0
          COETAN(5) =  0.D0
          COETAN(6) =  1.D0
          TYPTAN    = 'CONSTANT'
        ELSEIF (NBNO.EQ.6) THEN
          COETAN(1)  = -3.D0
          COETAN(2)  = -1.D0
          COETAN(4)  =  4.D0
          COETAN(7)  = -3.D0
          COETAN(9)  = -1.D0
          COETAN(12) =  4.D0
          COETAN(13) =  1.D0
          COETAN(14) =  3.D0
          COETAN(16) = -4.D0
          COETAN(19) =  1.D0
          COETAN(21) = -1.D0
          COETAN(22) = -4.D0
          COETAN(23) =  4.D0
          COETAN(25) =  1.D0
          COETAN(26) = -1.D0
          COETAN(29) =  4.D0
          COETAN(30) = -4.D0
          COETAN(31) =  1.D0
          COETAN(33) =  3.D0
          COETAN(36) = -4.D0
          COETAN(37) = -1.D0
          COETAN(38) =  1.D0
          COETAN(43) = -1.D0
          COETAN(45) = -1.D0
          COETAN(46) = -2.D0
          COETAN(47) =  2.D0
          COETAN(48) =  2.D0
          COETAN(49) =  1.D0
          COETAN(50) =  1.D0
          COETAN(52) = -2.D0
          COETAN(53) =  2.D0
          COETAN(54) = -2.D0
          COETAN(55) =  1.D0
          COETAN(57) =  1.D0
          COETAN(58) = -2.D0
          COETAN(59) =  2.D0
          COETAN(60) = -2.D0
          COETAN(61) = -1.D0
          COETAN(62) = -1.D0
          COETAN(64) =  2.D0
          COETAN(65) =  2.D0
          COETAN(66) = -2.D0
          COETAN(67) = -1.D0
          COETAN(69) =  1.D0
          TYPTAN     = 'VARIABLE'
        ELSEIF (NBNO.EQ.7) THEN
          COETAN(1)  = -3.D0
          COETAN(2)  = -1.D0
          COETAN(4)  =  4.D0
          COETAN(8)  = -3.D0
          COETAN(10) = -1.D0
          COETAN(13) =  4.D0
          COETAN(15) =  1.D0
          COETAN(16) =  3.D0
          COETAN(18) = -4.D0
          COETAN(22) =  1.D0
          COETAN(24) = -1.D0
          COETAN(25) = -4.D0
          COETAN(26) =  4.D0
          COETAN(29) =  1.D0
          COETAN(30) = -1.D0
          COETAN(33) =  4.D0
          COETAN(34) = -4.D0
          COETAN(36) =  1.D0
          COETAN(38) =  3.D0
          COETAN(41) = -4.D0
          COETAN(43) = -1.D0
          COETAN(44) =  1.D0
          COETAN(50) = -1.D0
          COETAN(51) =  3.D0
          COETAN(52) = -1.D0
          COETAN(53) =-20.D0
          COETAN(54) = -4.D0
          COETAN(55) = -4.D0
          COETAN(56) = 27.D0
          COETAN(57) =  1.D0
          COETAN(58) =  1.D0
          COETAN(59) = -3.D0
          COETAN(60) =  4.D0
          COETAN(61) = 20.D0
          COETAN(62) =  4.D0
          COETAN(63) =-27.D0
          COETAN(64) =  1.D0
          COETAN(65) = -3.D0
          COETAN(66) =  1.D0
          COETAN(67) =  4.D0
          COETAN(68) = 20.D0
          COETAN(69) =  4.D0
          COETAN(70) =-27.D0
          COETAN(71) = -1.D0
          COETAN(72) = -1.D0
          COETAN(73) =  3.D0
          COETAN(74) = -4.D0
          COETAN(75) = -4.D0
          COETAN(76) =-20.D0
          COETAN(77) = 27.D0
          COETAN(78) = -1.D0
          COETAN(80) =  1.D0
          COETAN(85) = -1.D0
          COETAN(86) =  1.D0
          COETAN(89) =  4.D0
          COETAN(90) = -4.D0
          COETAN(92) = -1.D0
          COETAN(94) =  1.D0
          COETAN(95) = -4.D0
          COETAN(96) =  4.D0
          TYPTAN     = 'VARIABLE'
        ELSEIF (NBNO.EQ.4) THEN
          IF (TYPTAN.EQ.'CONSTANT') THEN
            COETAN(1) = -1.D0
            COETAN(2) =  1.D0
            COETAN(3) =  1.D0
            COETAN(4) = -1.D0
            COETAN(5) = -1.D0
            COETAN(6) = -1.D0
            COETAN(7) =  1.D0
            COETAN(8) =  1.D0
          ELSEIF (TYPTAN.EQ.'VARIABLE') THEN
            COETAN(1)  = -1.D0
            COETAN(2)  =  1.D0
            COETAN(5)  = -1.D0
            COETAN(8)  =  1.D0
            COETAN(9)  = -1.D0
            COETAN(10) =  1.D0
            COETAN(14) = -1.D0
            COETAN(15) =  1.D0
            COETAN(19) =  1.D0
            COETAN(20) = -1.D0
            COETAN(22) = -1.D0
            COETAN(23) =  1.D0
            COETAN(27) =  1.D0
            COETAN(28) = -1.D0
            COETAN(29) = -1.D0
            COETAN(32) =  1.D0
          ELSE
            CALL ASSERT(.FALSE.)
          ENDIF
        ELSEIF (NBNO.EQ.8) THEN
          COETAN(1)   = -3.D0
          COETAN(2)   = -1.D0
          COETAN(5)   =  4.D0
          COETAN(9)   = -3.D0
          COETAN(12)  = -1.D0
          COETAN(16)  =  4.D0
          COETAN(17)  =  1.D0
          COETAN(18)  =  3.D0
          COETAN(21)  = -4.D0
          COETAN(26)  = -3.D0
          COETAN(27)  = -1.D0
          COETAN(30)  =  4.D0
          COETAN(35)  =  3.D0
          COETAN(36)  =  1.D0
          COETAN(39)  = -4.D0
          COETAN(42)  =  1.D0
          COETAN(43)  =  3.D0
          COETAN(46)  = -4.D0
          COETAN(51)  = -1.D0
          COETAN(52)  = -3.D0
          COETAN(55)  =  4.D0
          COETAN(57)  =  1.D0
          COETAN(60)  =  3.D0
          COETAN(64)  = -4.D0
          COETAN(65)  = -1.D0
          COETAN(66)  =  1.D0
          COETAN(73)  = -1.D0
          COETAN(74)  = -1.D0
          COETAN(75)  = -1.D0
          COETAN(76)  = -1.D0
          COETAN(77)  = -1.D0
          COETAN(78)  =  2.D0
          COETAN(79)  =  1.D0
          COETAN(80)  =  2.D0
          COETAN(81)  =  1.D0
          COETAN(82)  =  1.D0
          COETAN(83)  =  1.D0
          COETAN(84)  =  1.D0
          COETAN(85)  = -2.D0
          COETAN(86)  =  1.D0
          COETAN(87)  = -2.D0
          COETAN(88)  = -1.D0
          COETAN(90)  = -1.D0
          COETAN(91)  =  1.D0
          COETAN(99)  =  1.D0
          COETAN(100) = -1.D0
          COETAN(105) =  1.D0
          COETAN(106) =  1.D0
          COETAN(107) =  1.D0
          COETAN(108) =  1.D0
          COETAN(109) = -1.D0
          COETAN(110) = -2.D0
          COETAN(111) =  1.D0
          COETAN(112) = -2.D0
          COETAN(113) = -1.D0
          COETAN(114) = -1.D0
          COETAN(115) = -1.D0
          COETAN(116) = -1.D0
          COETAN(117) =  2.D0
          COETAN(118) =  1.D0
          COETAN(119) =  2.D0
          COETAN(120) = -1.D0
          COETAN(121) = -1.D0
          COETAN(124) =  1.D0
          TYPTAN      = 'VARIABLE'
        ELSEIF (NBNO.EQ.9) THEN
          COETAN(1)   = -3.D0
          COETAN(2)   = -1.D0
          COETAN(5)   =  4.D0
          COETAN(10)  = -3.D0
          COETAN(13)  = -1.D0
          COETAN(17)  =  4.D0
          COETAN(19)  =  1.D0
          COETAN(20)  =  3.D0
          COETAN(23)  = -4.D0
          COETAN(29)  = -3.D0
          COETAN(30)  = -1.D0
          COETAN(33)  =  4.D0
          COETAN(39)  =  3.D0
          COETAN(40)  =  1.D0
          COETAN(43)  = -4.D0
          COETAN(47)  =  1.D0
          COETAN(48)  =  3.D0
          COETAN(51)  = -4.D0
          COETAN(57)  = -1.D0
          COETAN(58)  = -3.D0
          COETAN(61)  =  4.D0
          COETAN(64)  =  1.D0
          COETAN(67)  =  3.D0
          COETAN(71)  = -4.D0
          COETAN(73)  = -1.D0
          COETAN(74)  =  1.D0
          COETAN(86)  = -3.D0
          COETAN(88)  = -1.D0
          COETAN(90)  =  4.D0
          COETAN(96)  =  3.D0
          COETAN(98)  =  1.D0
          COETAN(99)  = -4.D0
          COETAN(101) = -1.D0
          COETAN(102) =  1.D0
          COETAN(111) =  1.D0
          COETAN(112) = -1.D0
          COETAN(122) =  1.D0
          COETAN(124) =  3.D0
          COETAN(126) = -4.D0
          COETAN(132) = -1.D0
          COETAN(134) = -3.D0
          COETAN(135) =  4.D0
          COETAN(136) = -1.D0
          COETAN(139) =  1.D0
          COETAN(150) =  1.D0
          COETAN(152) = -1.D0
          COETAN(158) = -1.D0
          COETAN(160) =  1.D0
          TYPTAN      = 'VARIABLE'
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      END
