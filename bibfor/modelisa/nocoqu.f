      SUBROUTINE NOCOQU(DIME  ,NBNO  ,NOECOQ)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF MODELISA  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
      INTEGER  DIME,NBNO
      INTEGER  NOECOQ(2,9)
C
C ----------------------------------------------------------------------
C
C CONSTRUCTION DE BOITES ENGLOBANTES POUR UN GROUPE DE MAILLES
C
C NOEUDS DEFINISSANT LE PASSAGE D'UNE MAILLE DE COQUE
C EN UNE MAILLE VOLUMIQUE
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NBNO   : NOMBRE DE NOEUDS DE LA MAILLE DE COQUE
C OUT NOECOQ : NOEUDS POUR LE PASSAGE D'UNE MAILLE DE COQUE
C              EN UNE MAILLE VOLUMIQUE
C              DIM: (2,NBNO)
C                   ( NO1.1, NO1.2,
C                     NO2.1, NO2.2,
C                     NO3.1, ...)
C
C        NO1.1 x--------------x NO2.1   --- CONTOUR DE LA
C              |              |             MAILLE VOLUMIQUE
C          NO1 x==============x NO2
C              |              |         === FIBRE MOYENNE DE
C        NO1.2 x--------------x NO2.2       LA COQUE
C
C
C COQUE EN 1D5 -> MAILLE SOLIDE 2D
C   SEG3  -> QUAD6
C COQUE EN 2D5 -> MAILLE SOLIDE 3D
C   TRIA3 -> PENTA6
C   TRIA6 -> PENTA12
C   TRIA7 -> PENTA14
C   QUAD4 -> HEXA8
C   QUAD8 -> HEXA16
C   QUAD9 -> HEXA18
C
C ----------------------------------------------------------------------
C
      IF ((NBNO.GT.9).OR.(NBNO.LT.0)) THEN
        CALL ASSERT(.FALSE.)
      ENDIF
C
      IF (DIME.EQ.2) THEN
        IF (NBNO.EQ.3) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 5
          NOECOQ(2,3) = 6
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF (DIME.EQ.3) THEN
        IF (NBNO.EQ.3) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 5
          NOECOQ(1,3) = 3
          NOECOQ(2,3) = 6
        ELSEIF (NBNO.EQ.6) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 5
          NOECOQ(1,3) = 3
          NOECOQ(2,3) = 6
          NOECOQ(1,4) = 7
          NOECOQ(2,4) = 10
          NOECOQ(1,5) = 8
          NOECOQ(2,5) = 11
          NOECOQ(1,6) = 9
          NOECOQ(2,6) = 12
        ELSEIF (NBNO.EQ.7) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 4
          NOECOQ(1,2) = 2
          NOECOQ(2,2) = 5
          NOECOQ(1,3) = 3
          NOECOQ(2,3) = 6
          NOECOQ(1,4) = 7
          NOECOQ(2,4) = 10
          NOECOQ(1,5) = 8
          NOECOQ(2,5) = 11
          NOECOQ(1,6) = 9
          NOECOQ(2,6) = 12
          NOECOQ(1,7) = 13
          NOECOQ(2,7) = 14
        ELSEIF (NBNO.EQ.4) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 2
          NOECOQ(1,2) = 4
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 8
          NOECOQ(2,3) = 7
          NOECOQ(1,4) = 5
          NOECOQ(2,4) = 6
        ELSEIF (NBNO.EQ.8) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 2
          NOECOQ(1,2) = 4
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 8
          NOECOQ(2,3) = 7
          NOECOQ(1,4) = 5
          NOECOQ(2,4) = 6
          NOECOQ(1,5) = 10
          NOECOQ(2,5) = 9
          NOECOQ(1,6) = 14
          NOECOQ(2,6) = 13
          NOECOQ(1,7) = 16
          NOECOQ(2,7) = 15
          NOECOQ(1,8) = 11
          NOECOQ(2,8) = 12
        ELSEIF (NBNO.EQ.9) THEN
          NOECOQ(1,1) = 1
          NOECOQ(2,1) = 2
          NOECOQ(1,2) = 4
          NOECOQ(2,2) = 3
          NOECOQ(1,3) = 8
          NOECOQ(2,3) = 7
          NOECOQ(1,4) = 5
          NOECOQ(2,4) = 6
          NOECOQ(1,5) = 10
          NOECOQ(2,5) = 9
          NOECOQ(1,6) = 14
          NOECOQ(2,6) = 13
          NOECOQ(1,7) = 16
          NOECOQ(2,7) = 15
          NOECOQ(1,8) = 11
          NOECOQ(2,8) = 12
          NOECOQ(1,9) = 18
          NOECOQ(2,9) = 17
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ENDIF
C
      END
