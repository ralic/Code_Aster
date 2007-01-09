      SUBROUTINE NOAREQ(TYPEMA,NOEUD)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 09/01/2007   AUTEUR ABBAS M.ABBAS 
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
C RESPONSABLE ABBAS M.ABBAS
C
      IMPLICIT NONE
      CHARACTER*8  TYPEMA
      INTEGER      NOEUD(*)  
C      
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C INDICES DES NOEUDS DEFINISSANT LES ARETES QUADRATIQUES D'UNE MAILLE
C
C ----------------------------------------------------------------------
C
C
C IN  TYPEMA : TYPE DE MAILLE
C OUT NOEUD  : NOEUDS DES ARETES QUADRATIQUES DE LA MAILLE
C               ( NO1.1, NO1.2, NO1.3, NO2.1, NO3.2, ... )
C                 NO*.1 NO*.2 NO*.3 FORMENT LE SEG3 DE L'ARETE *
C                 NO*.3       : NOEUD MILIEU
C                 NO*.1 NO*.2 : NOEUDS SOMMETS
C
C ROUTINE SOEUR : NBAREQ
C
C ----------------------------------------------------------------------
C
      IF (TYPEMA(1:5).EQ.'TRIA6') THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 4
        NOEUD(4) = 2
        NOEUD(5) = 3
        NOEUD(6) = 5
        NOEUD(7) = 3
        NOEUD(8) = 1
        NOEUD(9) = 6
      ELSEIF (TYPEMA(1:5).EQ.'QUAD6') THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 5
        NOEUD(4) = 3
        NOEUD(5) = 4
        NOEUD(6) = 6
      ELSEIF ((TYPEMA(1:5).EQ.'QUAD8').OR.
     &        (TYPEMA(1:5).EQ.'QUAD9')) THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 5
        NOEUD(4) = 2
        NOEUD(5) = 3
        NOEUD(6) = 6
        NOEUD(7) = 3
        NOEUD(8) = 4
        NOEUD(9) = 7
        NOEUD(10) = 4
        NOEUD(11) = 1
        NOEUD(12) = 8
      ELSEIF (TYPEMA(1:7).EQ.'TETRA10') THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 5
        NOEUD(4) = 2
        NOEUD(5) = 3
        NOEUD(6) = 6
        NOEUD(7) = 3
        NOEUD(8) = 1
        NOEUD(9) = 7
        NOEUD(10) = 1
        NOEUD(11) = 4
        NOEUD(12) = 8
        NOEUD(13) = 4
        NOEUD(14) = 2
        NOEUD(15) = 9
        NOEUD(16) = 4
        NOEUD(17) = 3
        NOEUD(18) = 10
      ELSEIF ((TYPEMA(1:7).EQ.'PENTA12').OR.
     &        (TYPEMA(1:7).EQ.'PENTA14')) THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 7
        NOEUD(4) = 2
        NOEUD(5) = 3
        NOEUD(6) = 8
        NOEUD(7) = 3
        NOEUD(8) = 1
        NOEUD(9) = 9
        NOEUD(10) = 4
        NOEUD(11) = 5
        NOEUD(12) = 10
        NOEUD(13) = 5
        NOEUD(14) = 6
        NOEUD(15) = 11
        NOEUD(16) = 6
        NOEUD(17) = 4
        NOEUD(18) = 12
      ELSEIF (TYPEMA(1:7).EQ.'PENTA15') THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 7
        NOEUD(4) = 2
        NOEUD(5) = 3
        NOEUD(6) = 8
        NOEUD(7) = 3
        NOEUD(8) = 1
        NOEUD(9) = 9
        NOEUD(10) = 1
        NOEUD(11) = 4
        NOEUD(12) = 10
        NOEUD(13) = 2
        NOEUD(14) = 5
        NOEUD(15) = 11
        NOEUD(16) = 3
        NOEUD(17) = 6
        NOEUD(18) = 12
        NOEUD(19) = 4
        NOEUD(20) = 5
        NOEUD(21) = 13
        NOEUD(22) = 5
        NOEUD(23) = 6
        NOEUD(24) = 14
        NOEUD(25) = 6
        NOEUD(26) = 4
        NOEUD(27) = 15
      ELSEIF((TYPEMA(1:6).EQ.'HEXA16').OR.
     &       (TYPEMA(1:6).EQ.'HEXA18')) THEN
        NOEUD(1) = 2
        NOEUD(2) = 3
        NOEUD(3) = 9
        NOEUD(4) = 4
        NOEUD(5) = 1
        NOEUD(6) = 10
        NOEUD(7) = 1
        NOEUD(8) = 5
        NOEUD(9) = 11
        NOEUD(10) = 2
        NOEUD(11) = 6
        NOEUD(12) = 12
        NOEUD(13) = 3
        NOEUD(14) = 7
        NOEUD(15) = 13
        NOEUD(16) = 4
        NOEUD(17) = 8
        NOEUD(18) = 14
        NOEUD(19) = 6
        NOEUD(20) = 7
        NOEUD(21) = 15
        NOEUD(22) = 8
        NOEUD(23) = 5
        NOEUD(24) = 16
      ELSEIF((TYPEMA(1:6).EQ.'HEXA20').OR.
     &       (TYPEMA(1:6).EQ.'HEXA27'))THEN
        NOEUD(1) = 1
        NOEUD(2) = 2
        NOEUD(3) = 9
        NOEUD(4) = 2
        NOEUD(5) = 3
        NOEUD(6) = 10
        NOEUD(7) = 3
        NOEUD(8) = 4
        NOEUD(9) = 11
        NOEUD(10) = 4
        NOEUD(11) = 1
        NOEUD(12) = 12
        NOEUD(13) = 1
        NOEUD(14) = 5
        NOEUD(15) = 13
        NOEUD(16) = 2
        NOEUD(17) = 6
        NOEUD(18) = 14
        NOEUD(19) = 3
        NOEUD(20) = 7
        NOEUD(21) = 15
        NOEUD(22) = 4
        NOEUD(23) = 8
        NOEUD(24) = 16
        NOEUD(25) = 5
        NOEUD(26) = 6
        NOEUD(27) = 17
        NOEUD(28) = 6
        NOEUD(29) = 7
        NOEUD(30) = 18
        NOEUD(31) = 7
        NOEUD(32) = 8
        NOEUD(33) = 19
        NOEUD(34) = 8
        NOEUD(35) = 5
        NOEUD(36) = 20
      ELSE
          WRITE(6,*) 'MAILLE INCONNUE: ',TYPEMA
          CALL ASSERT(.FALSE.) 
      ENDIF

      END
