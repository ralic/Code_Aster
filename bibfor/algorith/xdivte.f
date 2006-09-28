      SUBROUTINE XDIVTE(ELREFP,CONNEC,NIT)
      IMPLICIT NONE

      INTEGER       CONNEC(6,4),NIT
      CHARACTER*8   ELREFP
C     ------------------------------------------------------------------
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 29/09/2006   AUTEUR VABHHTS J.PELLET 
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
C       ELREFP  : TYPE D'ÉLÉMENT DE RÉFÉRENCE PARENT
C
C     SORTIE
C       CONNEC  : CONNECTIVITÉ DES NOEUDS DE L'ÉLÉMENT ENFANT
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
        DO 110 J=1,4
          CONNEC(I,J)=0
 110    CONTINUE
 100  CONTINUE

      IF (ELREFP.EQ.'HE8'.OR.ELREFP.EQ.'X20') THEN
C       CONNECTIVITÉ DES NOEUDS
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
        NIT=6
      ELSEIF (ELREFP.EQ.'PE6'.OR.ELREFP.EQ.'X15') THEN
C       CONNECTIVITÉ DES NOEUDS
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
        NIT=3
      ELSEIF (ELREFP.EQ.'TE4') THEN
C       CONNECTIVITÉ DES NOEUDS
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        CONNEC(1,4)=4
        NIT=1
      ELSEIF (ELREFP.EQ.'QU4'.OR.ELREFP.EQ.'X8') THEN
C       CONNECTIVITÉ DES NOEUDS
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=4
        CONNEC(2,1)=2
        CONNEC(2,2)=3
        CONNEC(2,3)=4
        NIT=2
      ELSEIF (ELREFP.EQ.'TR3'.OR.ELREFP.EQ.'X6') THEN
C       CONNECTIVITÉ DES NOEUDS
        CONNEC(1,1)=1
        CONNEC(1,2)=2
        CONNEC(1,3)=3
        NIT=1
      ELSE
        CALL U2MESS('F','ALGORITH5_15')
      ENDIF

      CALL JEDEMA()
      END
