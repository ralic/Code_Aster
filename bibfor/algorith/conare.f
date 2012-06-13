      SUBROUTINE       CONARE(TYPMA,AR,NBAR)
      IMPLICIT NONE
      INCLUDE 'jeveux.h'
      INTEGER          AR(12,3),NBAR
      CHARACTER*8      TYPMA

C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 13/06/2012   AUTEUR COURTOIS M.COURTOIS 
C ======================================================================
C COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
C                       RENVOIE LA MATRICE DE CONNECTIVITÉ DES
C                       ARETES D'UNE MAILLE DE TYPE TYPMA
C
C    ENTREE :
C              TYPMA : TYPE DE LA MAILLE
C
C    SORTIE :
C              AR   : MATRICE DE CONNECTIVITÉ DES ARETES
C              NBAR : NOMBRE D'ARETES
C......................................................................
C
      INTEGER       I,J
C......................................................................

      CALL JEMARQ()

      DO 100 I=1,12
        DO 110 J=1,3
          AR(I,J)=0
 110    CONTINUE
 100  CONTINUE

      IF (TYPMA.EQ.'HEXA8' .OR. TYPMA.EQ.'HEXA20') THEN
        NBAR=12
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE HEXA8 OU HEXA20
        AR(1,1)=1
        AR(1,2)=2
        AR(2,1)=2
        AR(2,2)=3
        AR(3,1)=3
        AR(3,2)=4
        AR(4,1)=4
        AR(4,2)=1
        AR(5,1)=5
        AR(5,2)=6
        AR(6,1)=6
        AR(6,2)=7
        AR(7,1)=7
        AR(7,2)=8
        AR(8,1)=8
        AR(8,2)=5
        AR(9,1)=1
        AR(9,2)=5
        AR(10,1)=2
        AR(10,2)=6
        AR(11,1)=3
        AR(11,2)=7
        AR(12,1)=4
        AR(12,2)=8
      ELSEIF (TYPMA.EQ.'PENTA6'.OR.TYPMA.EQ.'PENTA15') THEN
        NBAR=9
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE PENTA6 OU PENTA15
        AR(1,1)=1
        AR(1,2)=2
        AR(2,1)=2
        AR(2,2)=3
        AR(3,1)=3
        AR(3,2)=1
        AR(4,1)=4
        AR(4,2)=5
        AR(5,1)=5
        AR(5,2)=6
        AR(6,1)=6
        AR(6,2)=4
        AR(7,1)=1
        AR(7,2)=4
        AR(8,1)=2
        AR(8,2)=5
        AR(9,1)=3
        AR(9,2)=6
      ELSEIF (TYPMA.EQ.'PYRAM5'.OR.TYPMA.EQ.'PYRAM13') THEN
        NBAR=8
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE PYRAM5 OU PYRAM13
        AR(1,1)=1
        AR(1,2)=2
        AR(2,1)=2
        AR(2,2)=3
        AR(3,1)=3
        AR(3,2)=4
        AR(4,1)=1
        AR(4,2)=4
        AR(5,1)=1
        AR(5,2)=5
        AR(6,1)=2
        AR(6,2)=5
        AR(7,1)=3
        AR(7,2)=5
        AR(8,1)=4
        AR(8,2)=5
      ELSEIF (TYPMA.EQ.'TETRA4'.OR.TYPMA.EQ.'TETRA10') THEN
        NBAR=6
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TETRA4 OU TETRA10
        AR(1,1)=1
        AR(1,2)=2
        AR(2,1)=1
        AR(2,2)=3
        AR(3,1)=1
        AR(3,2)=4
        AR(4,1)=2
        AR(4,2)=3
        AR(5,1)=2
        AR(5,2)=4
        AR(6,1)=3
        AR(6,2)=4
      ELSEIF (TYPMA.EQ.'QUAD4') THEN
        NBAR=4
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE QUAD4
        AR(1,1)=1
        AR(1,2)=2
        AR(2,1)=2
        AR(2,2)=3
        AR(3,1)=3
        AR(3,2)=4
        AR(4,1)=4
        AR(4,2)=1
      ELSEIF (TYPMA.EQ.'QUAD8' .OR. TYPMA.EQ.'QU8') THEN
        NBAR=4
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE QUAD8
        AR(1,1)=1
        AR(1,2)=2
        AR(1,3)=5
        AR(2,1)=2
        AR(2,2)=3
        AR(2,3)=6
        AR(3,1)=3
        AR(3,2)=4
        AR(3,3)=7
        AR(4,1)=4
        AR(4,2)=1
        AR(4,3)=8
      ELSEIF (TYPMA.EQ.'TRIA3') THEN
        NBAR=3
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TRIA3
        AR(1,1)=1
        AR(1,2)=2
        AR(2,1)=2
        AR(2,2)=3
        AR(3,1)=3
        AR(3,2)=1
      ELSEIF (TYPMA.EQ.'TRIA6' .OR. TYPMA.EQ.'TR6') THEN
        NBAR=3
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE TRIA6
        AR(1,1)=1
        AR(1,2)=2
        AR(1,3)=4
        AR(2,1)=2
        AR(2,2)=3
        AR(2,3)=5
        AR(3,1)=3
        AR(3,2)=1
        AR(3,3)=6
      ELSEIF (TYPMA.EQ.'SEG2') THEN
        NBAR=1
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE SEG2
        AR(1,1)=1
        AR(1,2)=2
      ELSEIF (TYPMA.EQ.'SEG3' .OR. TYPMA.EQ.'SE3') THEN
        NBAR=1
C       CONNECTIVITÉ DES ARETES POUR UNE MAILLE SEG3
        AR(1,1)=1
        AR(1,2)=2
        AR(1,3)=3
      ELSEIF (TYPMA.EQ.'POI1') THEN
        NBAR=0
      ELSE
        CALL U2MESK('F','ALGORITH2_23',1,TYPMA)
      ENDIF

      CALL JEDEMA()
      END
