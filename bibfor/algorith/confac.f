      SUBROUTINE       CONFAC(TYPMA,FT,NBFT,F,NBF)
      IMPLICIT NONE
      INTEGER          FT(12,3),NBFT,F(6,4),NBF
      CHARACTER*8      TYPMA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 06/10/2009   AUTEUR GENIAUT S.GENIAUT 
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
C RESPONSABLE GENIAUT S.GENIAUT
C
C    RENVOIE LA MATRICE DE CONNECTIVITÉ :
C                   1) DES FACES TRIANGULARISEES D'UNE MAILLE
C                   2) DES FACES (USUELLES) D'UNE MAILLE
C
C    (REMARQUE : POUR UN TETRA, CES 2 MATRICES SONT IDENTIQUES)
C
C
C    ENTREE :
C              TYPMA : TYPE DE LA MAILLE
C
C    SORTIE :
C              FT   : MATRICE DE CONNECTIVITÉ DES FACES TRIANGULARISEES
C              NBFT : NOMBRE DE FACES TRIANGULARISEES
C              F    : MATRICE DE CONNECTIVITÉ DES FACES
C              NBF  : NOMBRE DE FACES
C     ------------------------------------------------------------------
C     ----- DEBUT COMMUNS NORMALISES  JEVEUX  --------------------------
      INTEGER          ZI
      COMMON  /IVARJE/ ZI(1)
      REAL*8           ZR
      COMMON  /RVARJE/ ZR(1)
      COMPLEX*16       ZC
      COMMON  /CVARJE/ ZC(1)
      LOGICAL          ZL
      COMMON  /LVARJE/ ZL(1)
      CHARACTER*8      ZK8
      CHARACTER*16             ZK16
      CHARACTER*24                      ZK24
      CHARACTER*32                               ZK32
      CHARACTER*80                                        ZK80
      COMMON  /KVARJE/ ZK8(1), ZK16(1), ZK24(1), ZK32(1), ZK80(1)
C     -----  FIN  COMMUNS NORMALISES  JEVEUX  --------------------------
C
      INTEGER       I,J
C ----------------------------------------------------------------------

      CALL JEMARQ()
      DO 100 I=1,12
        DO 110 J=1,3
          FT(I,J)=0
 110    CONTINUE
 100  CONTINUE

      IF (TYPMA(1:4).EQ.'HEXA') THEN
        NBFT=12
C       CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE HEXA8 OU HEXA20
        FT(1,1)=1
        FT(1,2)=2
        FT(1,3)=3
        FT(2,1)=3
        FT(2,2)=4
        FT(2,3)=1
        FT(3,1)=1
        FT(3,2)=2
        FT(3,3)=6
        FT(4,1)=6
        FT(4,2)=5
        FT(4,3)=1
        FT(5,1)=3
        FT(5,2)=4
        FT(5,3)=8
        FT(6,1)=8
        FT(6,2)=7
        FT(6,3)=3
        FT(7,1)=5
        FT(7,2)=6
        FT(7,3)=7
        FT(8,1)=7
        FT(8,2)=8
        FT(8,3)=5
        FT(9,1)=1
        FT(9,2)=4
        FT(9,3)=8
        FT(10,1)=8
        FT(10,2)=5
        FT(10,3)=1
        FT(11,1)=2
        FT(11,2)=3
        FT(11,3)=7
        FT(12,1)=7
        FT(12,2)=6
        FT(12,3)=2

        NBF=6
C       CONNECTIVITÉ DES FACES POUR UNE MAILLE HEXA8 OU HEXA20
        F(1,1)=1
        F(1,2)=2
        F(1,3)=3
        F(1,4)=4
        F(2,1)=1
        F(2,2)=2
        F(2,3)=6
        F(2,4)=5
        F(3,1)=3
        F(3,2)=4
        F(3,3)=8
        F(3,4)=7
        F(4,1)=5
        F(4,2)=6
        F(4,3)=7
        F(4,4)=8
        F(5,1)=1
        F(5,2)=4
        F(5,3)=8
        F(5,4)=5
        F(6,1)=2
        F(6,2)=3
        F(6,3)=7
        F(6,4)=6

      ELSEIF (TYPMA(1:5).EQ.'PENTA') THEN
        NBFT=8
C     CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE PENTA6 OU PENTA15
        FT(1,1)=1
        FT(1,2)=2
        FT(1,3)=3
        FT(2,1)=4
        FT(2,2)=5
        FT(2,3)=6
        FT(3,1)=1
        FT(3,2)=3
        FT(3,3)=6
        FT(4,1)=6
        FT(4,2)=4
        FT(4,3)=1
        FT(5,1)=2
        FT(5,2)=3
        FT(5,3)=6
        FT(6,1)=6
        FT(6,2)=5
        FT(6,3)=2
        FT(7,1)=1
        FT(7,2)=2
        FT(7,3)=5
        FT(8,1)=5
        FT(8,2)=4
        FT(8,3)=1

        NBF=5
C       CONNECTIVITÉ DES FACES POUR UNE MAILLE PENTA6 OU PENTA15
        F(1,1)=1
        F(1,2)=2
        F(1,3)=3
        F(1,4)=0
        F(2,1)=4
        F(2,2)=5
        F(2,3)=6
        F(2,4)=0
        F(3,1)=1
        F(3,2)=3
        F(3,3)=6
        F(3,4)=4
        F(4,1)=2
        F(4,2)=3
        F(4,3)=6
        F(4,4)=5
        F(5,1)=1
        F(5,2)=2
        F(5,3)=5
        F(5,4)=4

      ELSEIF (TYPMA(1:5).EQ.'PYRAM') THEN
        NBFT=6
C     CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE TETRA4 OU TETRA10
        FT(1,1)=1
        FT(1,2)=2
        FT(1,3)=5
        FT(2,1)=2
        FT(2,2)=3
        FT(2,3)=5
        FT(3,1)=3
        FT(3,2)=4
        FT(3,3)=5
        FT(4,1)=4
        FT(4,2)=1
        FT(4,3)=5
        FT(5,1)=1
        FT(5,2)=2
        FT(5,3)=3
        FT(6,1)=1
        FT(6,2)=3
        FT(6,3)=4

        NBF=5
C       CONNECTIVITÉ DES FACES POUR UNE MAILLE PYRAM5 OU PYRAM13
C       F : [ 1 2 5
C             2 3 5
C             3 4 5
C             4 1 5
C             1 2 3 4]
        F(1,1)=1
        F(1,2)=2
        F(1,3)=5
        F(1,4)=0
        F(2,1)=2
        F(2,2)=3
        F(2,3)=5
        F(2,4)=0
        F(3,1)=3
        F(3,2)=4
        F(3,3)=5
        F(3,4)=0
        F(4,1)=4
        F(4,2)=1
        F(4,3)=5
        F(4,4)=0
        F(5,1)=1
        F(5,2)=2
        F(5,3)=3
        F(5,4)=4

      ELSEIF (TYPMA.EQ.'TETRA4'.OR.TYPMA.EQ.'TETRA10') THEN
        NBFT=4
C     CONNECTIVITÉ DES FACES TRIANGLES POUR UNE MAILLE TETRA4 OU TETRA10
        FT(1,1)=1
        FT(1,2)=3
        FT(1,3)=2
        FT(2,1)=2
        FT(2,2)=3
        FT(2,3)=4
        FT(3,1)=1
        FT(3,2)=4
        FT(3,3)=3
        FT(4,1)=1
        FT(4,2)=2
        FT(4,3)=4

        NBF=4
C       CONNECTIVITÉ DES FACES POUR UNE MAILLE TETRA4 OU TETRA10
        F(1,1)=1
        F(1,2)=3
        F(1,3)=2
        F(1,4)=0
        F(2,1)=2
        F(2,2)=3
        F(2,3)=4
        F(2,4)=0
        F(3,1)=1
        F(3,2)=4
        F(3,3)=3
        F(3,4)=0
        F(4,1)=1
        F(4,2)=2
        F(4,3)=4
        F(4,4)=0
      ELSEIF (TYPMA.EQ.'QUAD4'.OR.TYPMA.EQ.'QUAD8') THEN
        NBFT=2
C     CONNECTIVITE DES TRIANGLES POUR UNE MAILLE QUAD4 OU QUAD8
        FT(1,1)=1
        FT(1,2)=2
        FT(1,3)=3
        FT(2,1)=3
        FT(2,2)=4
        FT(2,3)=1

        NBF=1
C       CONNECTIVITE DES FACES POUR UNE MAILLE QUAD4 OU QUAD8
        F(1,1)=1
        F(1,2)=2
        F(1,3)=3
        F(1,4)=4
      ELSEIF (TYPMA.EQ.'TRIA3'.OR.TYPMA.EQ.'TRIA6') THEN
        NBFT=1
C     CONNECTIVITE DES TRIANGLES POUR UNE MAILLE TRIA3 OU TRIA6
        FT(1,1)=1
        FT(1,2)=3
        FT(1,3)=2

        NBF=1
C       CONNECTIVITE DES FACES POUR UNE MAILLE TRIA3 OU TRIA6
        F(1,1)=1
        F(1,2)=3
        F(1,3)=2
        F(1,4)=0
      ELSE
        CALL U2MESK('F','ALGORITH2_24',1,TYPMA)
      ENDIF

      CALL JEDEMA()
      END
