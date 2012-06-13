      SUBROUTINE XDIVTE(NDIM,ELP,CNSET,NSE,NNOSE)
      IMPLICIT NONE

      INCLUDE 'jeveux.h'
      INTEGER       NDIM,CNSET(*),NSE,NNOSE
      CHARACTER*8   ELP
C     ------------------------------------------------------------------
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C                      CONNECTIVITÉ DES ÉLÉMENTS TETRAS À PARTIR
C                               D'UN ÉLÉMENT PARENT X-FEM
C                          (VOIR BOOK III 19/04/04)
C
C     ENTREE
C       NDIM    : DIMENSSION DU MODELE
C       ELP     : TYPE DE MAILLE
C
C     SORTIE
C       CNSET   : CONNECTIVITÉ DES NOEUDS DE LA MAILLE
C       NSE     : NOMBRE DE SOUS-TÉTRAS (SOUS TRIA)
C       NNOSE   : NOMBRE DE NOEUDS DU SOUS TETRA (SOUS TRIA)
C     ------------------------------------------------------------------
C
      INTEGER         INO,ISE,CONNEC(6,6)
C ----------------------------------------------------------------------

      CALL JEMARQ()

      IF (ELP.EQ.'HE8'.OR.ELP.EQ.'H20') THEN
        CONNEC(1,1)=6
        CONNEC(1,2)=3
        CONNEC(1,3)=2
        CONNEC(1,4)=4
        CONNEC(2,1)=4
        CONNEC(2,2)=5
        CONNEC(2,3)=1
        CONNEC(2,4)=2
        CONNEC(3,1)=2
        CONNEC(3,2)=5
        CONNEC(3,3)=6
        CONNEC(3,4)=4
        CONNEC(4,1)=5
        CONNEC(4,2)=4
        CONNEC(4,3)=8
        CONNEC(4,4)=6
        CONNEC(5,1)=3
        CONNEC(5,2)=6
        CONNEC(5,3)=7
        CONNEC(5,4)=8
        CONNEC(6,1)=3
        CONNEC(6,2)=8
        CONNEC(6,3)=4
        CONNEC(6,4)=6
        NSE=6
        NNOSE=4
      ELSEIF (ELP.EQ.'PE6'.OR.ELP.EQ.'P15') THEN
        CONNEC(1,1)=5
        CONNEC(1,2)=4
        CONNEC(1,3)=6
        CONNEC(1,4)=1
        CONNEC(2,1)=1
        CONNEC(2,2)=2
        CONNEC(2,3)=3
        CONNEC(2,4)=6
        CONNEC(3,1)=6
        CONNEC(3,2)=2
        CONNEC(3,3)=5
        CONNEC(3,4)=1
        NSE=3
        NNOSE=4
      ELSEIF (ELP.EQ.'PY5'.OR.ELP.EQ.'P13') THEN
C       SOUS-TETRAS
C       CONNEC = [1 2 3 5
C                 1 3 4 5]
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        CONNEC(1,4)=5
        CONNEC(2,1)=1
        CONNEC(2,2)=3
        CONNEC(2,3)=4
        CONNEC(2,4)=5
        NSE=2
        NNOSE=4
      ELSEIF (ELP.EQ.'TE4'.OR.ELP.EQ.'T10') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        CONNEC(1,4)=4
        NSE=1
        NNOSE=4
      ELSEIF (ELP.EQ.'QU4'.OR.ELP.EQ.'QU8'.AND.NDIM.EQ.3) THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=4
        CONNEC(2,1)=2
        CONNEC(2,2)=3
        CONNEC(2,3)=4
        NSE=2
        NNOSE=3
      ELSEIF (ELP.EQ.'QU8'.AND.NDIM.EQ.2) THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=4
        CONNEC(1,4)=5
        CONNEC(1,5)=9
        CONNEC(1,6)=8
        CONNEC(2,1)=2
        CONNEC(2,2)=3
        CONNEC(2,3)=4
        CONNEC(2,4)=6
        CONNEC(2,5)=7
        CONNEC(2,6)=9
        NSE=2
        NNOSE=6
      ELSEIF (ELP.EQ.'TR3'.OR.ELP.EQ.'TR6'.AND.NDIM.EQ.3) THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        NSE=1
        NNOSE=3
      ELSEIF (ELP.EQ.'TR6'.AND.NDIM.EQ.2) THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        CONNEC(1,4)=4
        CONNEC(1,5)=5
        CONNEC(1,6)=6
        NSE=1
        NNOSE=6
      ELSEIF (ELP.EQ.'SE2') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        NSE=1
        NNOSE=2
      ELSEIF (ELP.EQ.'SE3') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        NSE=1
        NNOSE=3
      ELSE
C       TYPE D'ELEMENT FINI PAS TRAITE
        CALL ASSERT(.FALSE.)
      ENDIF

      DO 10 ISE=1,NSE
        DO 20 INO=1,NNOSE
          CNSET(NNOSE*(ISE-1)+INO)=CONNEC(ISE,INO)
  20    CONTINUE
  10  CONTINUE

      CALL JEDEMA()
      END
