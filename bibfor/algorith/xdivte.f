      SUBROUTINE XDIVTE(TYPMA,CONNEC,AREPAR,NIT)
      IMPLICIT NONE

      INTEGER       CONNEC(6,6),AREPAR(6,6),NIT
      CHARACTER*8   TYPMA
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/06/2010   AUTEUR CARON A.CARON 
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
C                      CONNECTIVITÉ DES ÉLÉMENTS TETRAS À PARTIR
C                               D'UN ÉLÉMENT PARENT X-FEM
C                          (VOIR BOOK III 19/04/04)
C
C     ENTREE
C       TYPMA  : TYPE DE MAILLE
C
C     SORTIE
C       CONNEC  : CONNECTIVITÉ DES NOEUDS DE LA MAILLE
C       AREPAR  : IT,AR -> ARETE DE L'ELEMENT PARENT QUI
C                 CORRESPOND A L'ARETE AR DU SSELT IT, OU ZERO
C                 SI AUCUNE ARETE DE L'ELEMENT PARENT NE CORRESPOND.
C                 LES ARETES SONT ORDONNEES PAR LA ROUTINE CONARE
C       NIT     : NOMBRE DE SOUS-TÉTRAS
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
      INTEGER         I,J
C ----------------------------------------------------------------------

      CALL JEMARQ()

      DO 100 I=1,6
        DO 110 J=1,6
          CONNEC(I,J)=0
          AREPAR(I,J)=0
 110    CONTINUE
 100  CONTINUE

      IF (TYPMA(1:4).EQ.'HEXA') THEN
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
        AREPAR(1,1)=0
        AREPAR(1,2)=10
        AREPAR(1,3)=0
        AREPAR(1,4)=2
        AREPAR(1,5)=3
        AREPAR(1,6)=0
        AREPAR(2,1)=0
        AREPAR(2,2)=4
        AREPAR(2,3)=0
        AREPAR(2,4)=9
        AREPAR(2,5)=0
        AREPAR(2,6)=1
        AREPAR(3,1)=0
        AREPAR(3,2)=10
        AREPAR(3,3)=0
        AREPAR(3,4)=5
        AREPAR(3,5)=0
        AREPAR(3,6)=0
        AREPAR(4,1)=0
        AREPAR(4,2)=8
        AREPAR(4,3)=5
        AREPAR(4,4)=12
        AREPAR(4,5)=0
        AREPAR(4,6)=0
        AREPAR(5,1)=0
        AREPAR(5,2)=11
        AREPAR(5,3)=0
        AREPAR(5,4)=6
        AREPAR(5,5)=0
        AREPAR(5,6)=7
        AREPAR(6,1)=0
        AREPAR(6,2)=3
        AREPAR(6,3)=0
        AREPAR(6,4)=12
        AREPAR(6,5)=0
        AREPAR(6,6)=0
        NIT=6
      ELSEIF (TYPMA(1:5).EQ.'PENTA') THEN
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
        AREPAR(1,1)=4
        AREPAR(1,2)=5
        AREPAR(1,3)=0
        AREPAR(1,4)=6
        AREPAR(1,5)=7
        AREPAR(1,6)=0
        AREPAR(2,1)=1
        AREPAR(2,2)=3
        AREPAR(2,3)=0
        AREPAR(2,4)=2
        AREPAR(2,5)=0
        AREPAR(2,6)=9
        AREPAR(3,1)=0
        AREPAR(3,2)=5
        AREPAR(3,3)=0
        AREPAR(3,4)=8
        AREPAR(3,5)=1
        AREPAR(3,6)=0
        NIT=3
      ELSEIF (TYPMA(1:5).EQ.'PYRAM') THEN
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
        AREPAR(1,1)=1
        AREPAR(1,2)=0
        AREPAR(1,3)=3
        AREPAR(1,4)=4
        AREPAR(1,5)=5
        AREPAR(1,6)=7
        AREPAR(2,1)=0
        AREPAR(2,2)=2
        AREPAR(2,3)=3
        AREPAR(2,4)=6
        AREPAR(2,5)=7
        AREPAR(2,6)=8
        NIT=2
      ELSEIF (TYPMA(1:5).EQ.'TETRA') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        CONNEC(1,4)=4
        AREPAR(1,1)=1
        AREPAR(1,2)=2
        AREPAR(1,3)=3
        AREPAR(1,4)=4
        AREPAR(1,5)=5
        AREPAR(1,6)=6
        NIT=1
      ELSEIF (TYPMA(1:5).EQ.'QUAD4') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=4
        CONNEC(2,1)=2
        CONNEC(2,2)=3
        CONNEC(2,3)=4
        AREPAR(1,1)=1
        AREPAR(1,2)=0
        AREPAR(1,3)=4
        AREPAR(2,1)=2
        AREPAR(2,2)=3
        AREPAR(2,3)=0
        NIT=2
      ELSEIF (TYPMA(1:5).EQ.'QUAD8') THEN
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
        AREPAR(1,1)=1
        AREPAR(1,2)=0
        AREPAR(1,3)=4
        AREPAR(2,1)=2
        AREPAR(2,2)=3
        AREPAR(2,3)=0
        NIT=2
      ELSEIF (TYPMA(1:5).EQ.'TRIA3') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        AREPAR(1,1)=1
        AREPAR(1,2)=2
        AREPAR(1,3)=3
        NIT=1
      ELSEIF (TYPMA(1:5).EQ.'TRIA6') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        CONNEC(1,4)=4
        CONNEC(1,5)=5
        CONNEC(1,6)=6
        AREPAR(1,1)=1
        AREPAR(1,2)=2
        AREPAR(1,3)=3
        NIT=1
      ELSEIF (TYPMA(1:4).EQ.'SEG2') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        AREPAR(1,1)=1
        AREPAR(1,2)=2
        NIT=1  
      ELSEIF (TYPMA(1:4).EQ.'SEG3') THEN
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        AREPAR(1,1)=1
        AREPAR(1,2)=2
        NIT=1
      ELSE
C       TYPE D'ELEMENT FINI PAS TRAITE
        CALL ASSERT(.FALSE.)
      ENDIF

      CALL JEDEMA()
      END
