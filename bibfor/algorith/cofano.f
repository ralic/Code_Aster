      SUBROUTINE       COFANO(TYPMA,FANO,NONO,NBFANO,NBNONO)
      IMPLICIT NONE
      INTEGER          FANO(8,4),NONO(8,4),NBFANO,NBNONO
      CHARACTER*8      TYPMA
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF ALGORITH  DATE 16/11/2009   AUTEUR DURAND C.DURAND 
C ======================================================================
C COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
C
C    SORTIE:
C              FANO : MATRICE DE CONNECTIVITÉ DES FACES  PAR NOEUD
C              FANO(INO,I) = INDICE DE LA I'EME FACE CONNECTEE AU NOEUD
C                            INO (I=1..NDIM)
C
C              NONO : MATRICE DE CONNECTIVITÉ DES NOEUDS PAR NOEUD
C              NONO(INO1,INO2) = INDICES LOCAUX DES NOEUDS CONNECTES
C                                AU NOEUD INO1 DANS UNE MAILLE TYPMA
C                                (INO2=1..NDIM)
C
C              L'INDICE D'UNE FACE EST SA POSITION DANS
C              LE TABLEAU DONNE PAR CONFAC
C
C              EN 2D, FANO EST LA CONNECTIVITÉ DES ARÊTES PAR NOEUD
C              L'INDICE D'UNE ARÊTE EST DONNÉE PAR SA POSITION
C              DANS LA TABLEAU AR() DONNÉE PAR CONARE
C
C    ENTREE :
C              TYPMA : TYPE DE LA MAILLE
C              NBFANO: NOMBRE DE FACES  PAR NOEUD
C              NBNONO: NOMBRE DE NOEUDS PAR NOEUD
C
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
      DO 100 I=1,8
        DO 110 J=1,4
          FANO(I,J)=0
          NONO(I,J)=0
 110    CONTINUE
 100  CONTINUE

      IF (TYPMA.EQ.'TETRA4'.OR.TYPMA.EQ.'TETRA10') THEN
C       CONNECTIVITÉ DES FACES PAR NOEUD POUR UNE MAILLE TETRA
        NBFANO=3
        FANO(1,1)=1
        FANO(1,2)=3
        FANO(1,3)=4
        FANO(2,1)=1
        FANO(2,2)=2
        FANO(2,3)=4
        FANO(3,1)=1
        FANO(3,2)=2
        FANO(3,3)=3
        FANO(4,1)=2
        FANO(4,2)=3
        FANO(4,3)=4

C CONNECTIVITÉ DES NOEUDS
        NBNONO=3
        NONO(1,1)=2
        NONO(1,2)=3
        NONO(1,3)=4
        NONO(2,1)=1
        NONO(2,2)=3
        NONO(2,3)=4
        NONO(3,1)=1
        NONO(3,2)=2
        NONO(3,3)=4
        NONO(4,1)=1
        NONO(4,2)=2
        NONO(4,3)=3

      ELSEIF (TYPMA.EQ.'HEXA8'.OR.TYPMA.EQ.'HEXA20') THEN
C       CONNECTIVITÉ DES FACES PAR NOEUD POUR UNE MAILLE HEXA8 OU HEXA20
        NBFANO=3
        FANO(1,1)=1
        FANO(1,2)=2
        FANO(1,3)=5
        FANO(2,1)=1
        FANO(2,2)=2
        FANO(2,3)=6
        FANO(3,1)=1
        FANO(3,2)=3
        FANO(3,3)=6
        FANO(4,1)=1
        FANO(4,2)=3
        FANO(4,3)=5
        FANO(5,1)=2
        FANO(5,2)=4
        FANO(5,3)=5
        FANO(6,1)=2
        FANO(6,2)=4
        FANO(6,3)=6
        FANO(7,1)=3
        FANO(7,2)=4
        FANO(7,3)=6
        FANO(8,1)=3
        FANO(8,2)=4
        FANO(8,3)=5

C CONNECTIVITÉ DES NOEUDS
        NBNONO=3
        NONO(1,1)=2
        NONO(1,2)=4
        NONO(1,3)=5
        NONO(2,1)=1
        NONO(2,2)=3
        NONO(2,3)=6
        NONO(3,1)=2
        NONO(3,2)=4
        NONO(3,3)=7
        NONO(4,1)=1
        NONO(4,2)=3
        NONO(4,3)=8
        NONO(5,1)=1
        NONO(5,2)=6
        NONO(5,3)=8
        NONO(6,1)=2
        NONO(6,2)=5
        NONO(6,3)=7
        NONO(7,1)=3
        NONO(7,2)=6
        NONO(7,3)=8
        NONO(8,1)=4
        NONO(8,2)=5
        NONO(8,3)=7

      ELSEIF (TYPMA.EQ.'PENTA6'.OR.TYPMA.EQ.'PENTA15') THEN
C     CONNECTIVITÉ DES FACES PAR NOEUD POUR UNE MAILLE PENTA6 OU PENTA15
        NBFANO=3
        FANO(1,1)=1
        FANO(1,2)=3
        FANO(1,3)=5
        FANO(2,1)=1
        FANO(2,2)=4
        FANO(2,3)=5
        FANO(3,1)=1
        FANO(3,2)=3
        FANO(3,3)=4
        FANO(4,1)=2
        FANO(4,2)=3
        FANO(4,3)=5
        FANO(5,1)=2
        FANO(5,2)=4
        FANO(5,3)=5
        FANO(6,1)=2
        FANO(6,2)=3
        FANO(6,3)=4

C CONNECTIVITÉ DES NOEUDS
        NBNONO=3
        NONO(1,1)=2
        NONO(1,2)=3
        NONO(1,3)=4
        NONO(2,1)=1
        NONO(2,2)=3
        NONO(2,3)=5
        NONO(3,1)=1
        NONO(3,2)=2
        NONO(3,3)=6
        NONO(4,1)=1
        NONO(4,2)=5
        NONO(4,3)=6
        NONO(5,1)=2
        NONO(5,2)=4
        NONO(5,3)=6
        NONO(6,1)=3
        NONO(6,2)=4
        NONO(6,3)=5

      ELSEIF (TYPMA(1:5).EQ.'PYRAM') THEN
C     CONNECTIVITÉ DES FACES PAR NOEUD POUR UNE MAILLE PYRAM
        NBFANO=4
        FANO(1,1)=4
        FANO(1,2)=1
        FANO(1,3)=5
        FANO(2,1)=1
        FANO(2,2)=2
        FANO(2,3)=5
        FANO(3,1)=2
        FANO(3,2)=3
        FANO(3,3)=5
        FANO(4,1)=3
        FANO(4,2)=4
        FANO(4,3)=5
        FANO(5,1)=1
        FANO(5,2)=2
        FANO(5,3)=3
        FANO(5,4)=4

C CONNECTIVITÉ DES NOEUDS
        NBNONO=4
        NONO(1,1)=4
        NONO(1,2)=2
        NONO(1,3)=5
        NONO(2,1)=1
        NONO(2,2)=3
        NONO(2,3)=5
        NONO(3,1)=2
        NONO(3,2)=4
        NONO(3,3)=5
        NONO(4,1)=3
        NONO(4,2)=1
        NONO(4,3)=5
        NONO(5,1)=1
        NONO(5,2)=2
        NONO(5,3)=3
        NONO(5,4)=4
      ELSEIF (TYPMA.EQ.'QUAD4'.OR.TYPMA.EQ.'QUAD8') THEN
C       CONNECTIVITE DES ARETES PAR NOEUD POUR UNE MAILLE QUAD4 OU QUAD8
C       INDEX DANS AR() CF. CONARE
        NBFANO=2
        FANO(1,1)=1
        FANO(1,2)=4
        FANO(2,1)=1
        FANO(2,2)=2
        FANO(3,1)=2
        FANO(3,2)=3
        FANO(4,1)=3
        FANO(4,2)=4

C CONNECTIVITÉ DES NOEUDS
        NBNONO=2
        NONO(1,1)=2
        NONO(1,2)=4
        NONO(2,1)=1
        NONO(2,2)=3
        NONO(3,1)=2
        NONO(3,2)=4
        NONO(4,1)=3
        NONO(4,2)=1

      ELSEIF (TYPMA.EQ.'TRIA3'.OR.TYPMA.EQ.'TRIA6') THEN
C       CONNECTIVITE DES ARETES PAR NOEUD POUR UNE MAILLE TRIA3 OU TRIA6
C       INDEX DANS AR() CF. CONARE
        NBFANO=2
        FANO(1,1)=1
        FANO(1,2)=3
        FANO(2,1)=1
        FANO(2,2)=2
        FANO(3,1)=2
        FANO(3,2)=3

C CONNECTIVITÉ DES NOEUDS
        NBNONO=2
        NONO(1,1)=2
        NONO(1,2)=3
        NONO(2,1)=1
        NONO(2,2)=3
        NONO(3,1)=2
        NONO(3,2)=1

      ELSE
        CALL U2MESK('F','ALGORITH2_23',1,TYPMA)
      ENDIF

      CALL JEDEMA()
      END
