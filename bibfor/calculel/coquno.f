      SUBROUTINE COQUNO(DIME,NNO,COQNOE)
C
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF CALCULEL  DATE 08/04/2008   AUTEUR MEUNIER S.MEUNIER 
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
      INTEGER DIME
      INTEGER NNO
      INTEGER COQNOE(2,*)
C
C ----------------------------------------------------------------------
C
C ROUTINE ARLEQUIN
C
C CORRESPONDANCE NOEUDS MAILLE VOLUMIQUE -> NOEUDS MAILLE DE COQUE
C
C ----------------------------------------------------------------------
C
C
C IN  DIME   : DIMENSION DE L'ESPACE
C IN  NNO    : NOMBRE DE NOEUD DE LA MAILLE COQUE
C OUT COQNOE : (NOEUD ASSOCIE, POSITION, ...)
C                          0 : PEAU INFERIEURE
C                          1 : PEAU SUPERIEURE
C
C ----------------------------------------------------------------------
C
      IF (DIME.EQ.2) THEN
        IF (NNO.EQ.3) THEN
          COQNOE(1,1) = 1
          COQNOE(2,1) = 0
          COQNOE(1,2) = 2
          COQNOE(2,2) = 0
          COQNOE(1,3) = 2
          COQNOE(2,3) = 1
          COQNOE(1,4) = 1
          COQNOE(2,4) = 1
          COQNOE(1,5) = 3
          COQNOE(2,5) = 0
          COQNOE(1,6) = 3
          COQNOE(2,6) = 1
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSEIF (DIME.EQ.3) THEN
        IF ((NNO.EQ.3).OR.(NNO.EQ.6).OR.(NNO.EQ.7)) THEN
          COQNOE(1,1) = 1
          COQNOE(2,1) = 0
          COQNOE(1,2) = 2
          COQNOE(2,2) = 0
          COQNOE(1,3) = 3
          COQNOE(2,3) = 0
          COQNOE(1,4) = 1
          COQNOE(2,4) = 1
          COQNOE(1,5) = 2
          COQNOE(2,5) = 1
          COQNOE(1,6) = 3
          COQNOE(2,6) = 1
          COQNOE(1,7) = 4
          COQNOE(2,7) = 0
          COQNOE(1,8) = 5
          COQNOE(2,8) = 0
          COQNOE(1,9) = 6
          COQNOE(2,9) = 0
          COQNOE(1,10) = 4
          COQNOE(2,10) = 1
          COQNOE(1,11) = 5
          COQNOE(2,11) = 1
          COQNOE(1,12) = 6
          COQNOE(2,12) = 1
          COQNOE(1,13) = 7
          COQNOE(2,13) = 0
          COQNOE(1,14) = 7
          COQNOE(2,14) = 1
        ELSEIF ((NNO.EQ.4).OR.(NNO.EQ.8).OR.(NNO.EQ.9)) THEN
          COQNOE(1,1) = 1
          COQNOE(2,1) = 0
          COQNOE(1,2) = 1
          COQNOE(2,2) = 1
          COQNOE(1,3) = 2
          COQNOE(2,3) = 1
          COQNOE(1,4) = 2
          COQNOE(2,4) = 0
          COQNOE(1,5) = 4
          COQNOE(2,5) = 0
          COQNOE(1,6) = 4
          COQNOE(2,6) = 1
          COQNOE(1,7) = 3
          COQNOE(2,7) = 1
          COQNOE(1,8) = 3
          COQNOE(2,8) = 0
          COQNOE(1,9) = 5
          COQNOE(2,9) = 1
          COQNOE(1,10) = 5
          COQNOE(2,10) = 0
          COQNOE(1,11) = 8
          COQNOE(2,11) = 0
          COQNOE(1,12) = 8
          COQNOE(2,12) = 1
          COQNOE(1,13) = 6
          COQNOE(2,13) = 1
          COQNOE(1,14) = 6
          COQNOE(2,14) = 0
          COQNOE(1,15) = 7
          COQNOE(2,15) = 1
          COQNOE(1,16) = 7
          COQNOE(2,16) = 0
          COQNOE(1,17) = 9
          COQNOE(2,17) = 1
          COQNOE(1,18) = 9
          COQNOE(2,18) = 0
        ELSE
          CALL ASSERT(.FALSE.)
        ENDIF
      ELSE
        CALL ASSERT(.FALSE.)
      ENDIF
      END
