      SUBROUTINE CM1518( MAIN, MAOUT, NBMA, LIMA, PREFIX, NDINIT)
C            CONFIGURATION MANAGEMENT OF EDF VERSION
C MODIF PREPOST  DATE 09/11/2012   AUTEUR DELMAS J.DELMAS 
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
        IMPLICIT NONE
      INCLUDE 'jeveux.h'

      CHARACTER*32 JEXNUM,JEXNOM
        INTEGER     NDINIT, NBMA, LIMA(NBMA)
        CHARACTER*8 MAIN, MAOUT,PREFIX

C ----------------------------------------------------------------------
C         TRANSFORMATION DES MAILLES PENTA15 EN PENTA18
C ----------------------------------------------------------------------
C IN        MAIN   K8  NOM DU MAILLAGE INITIAL
C IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
C IN        NBMA    I  NOMBRE DE MAILLES A TRAITER
C IN        LIMA    I  NUMERO ET TYPE DES MAILLES A TRAITER
C IN        PREFIX K8  PREFIXE DU NOM DES NOEUDS CREES (EX: N, NO, ...)
C IN        NDINIT  I  NUMERO INITIAL DES NOEUDS CREES
C ----------------------------------------------------------------------


      INTEGER      NBTYMA, ITYP, JNOEU, NFMAX
      PARAMETER    ( NBTYMA = 27, NFMAX = 24 )
      INTEGER JNOMIM, JNOMIP, JMILIE, JDIM, NBNO, IRET
      INTEGER NBNOMI, NBTOT, NO, JCOOR, NBNOMX, NBMATO
      INTEGER      DEFFAC(8,0:6,NBTYMA),JTYPMA,JREFE,
     &             REFTYP(NBTYMA), NBREF(NBTYMA), IMPMAI(NBTYMA)
      CHARACTER*8  NOMND, KBID, NOMAST(NBTYMA)
      CHARACTER*19 COORDO
      CHARACTER*24 NOMIMA, MILIEU, NOMIPE, NOMNOE, NOMNOI
      CHARACTER*24 TYPEMA, CONNEI, CONNEO
C
C     MAILLES TRAITEES PAR LA COMMANDE :
C
      DATA NOMAST / 'POI1    ', 'SEG2    ', 'SEG22   ', 'SEG3    ',
     &              'SEG33   ', 'SEG4    ', 'TRIA3   ', 'TRIA33  ',
     &              'TRIA6   ', 'TRIA66  ', 'TRIA7   ', 'QUAD4   ',
     &              'QUAD44  ', 'QUAD8   ', 'QUAD88  ', 'QUAD9   ',
     &              'QUAD99  ', 'TETRA4  ', 'TETRA10 ', 'PENTA6  ',
     &              'PENTA15 ', 'PENTA18 ', 'PYRAM5  ', 'PYRAM13 ',
     &              'HEXA8   ', 'HEXA20  ', 'HEXA27  '/

C     A PARTIR DU CATALOGUE TYPE_MAILLE__  :
C     REFERENCE     -->  NOUVEAU TYPE              NB DE NOEUDS
C
C                      REFTYP                         NBREF
C
C     1   POI1      -->  1                               1
C     2   SEG2      -->  2                               2
C     3   SEG22     -->  3                               4
C     4   SEG3      -->  4                               3
C     5   SEG33     -->  5                               6
C     6   SEG4      -->  6                               4
C     7   TRIA3     -->  7                               3
C     8   TRIA33    -->  8                               6
C     9   TRIA6     -->  9                               6
C     10  TRIA66    -->  10                             12
C     11  TRIA7     -->  11                              7
C     12  QUAD4     -->  12                              4
C     13  QUAD44    -->  13                              8
C     14  QUAD8     -->  16                              9
C     15  QUAD88    -->  15                             16
C     16  QUAD9     -->  16                              9
C     17  QUAD99    -->  17                             18
C     18  TETRA4    -->  18                              4
C     19  TETRA10   -->  19                             10
C     20  PENTA6    -->  20                              6
C     21  PENTA15   -->  22                             18
C     22  PENTA18   -->  22                             18
C     23  PYRAM5    -->  23                              5
C     24  PYRAM13   -->  24                             13
C     25  HEXA8     -->  25                              8
C     26  HEXA20    -->  26                             20
C     27  HEXA27    -->  27                             27
C
      DATA REFTYP /1,2,3,4,5,6,7,8,9,10,11,12,13,16,15,16,17,18,19,20,
     &             22,22,23,24,25,26,27/

C --- EXPLICATIONS DU DATA DEFFAC
C
C       POI1     SEG2     SEG22   SEG3   SEG33   SEG4
C       TRIA3    TRIA33   TRIA6   TRIA66   TRIA7
C       QUAD4,   QUAD44   QUAD8   QUAD88  QUAD9  QUAD99
C       TETRA4   TETRA10
C       PENTA6   PENTA15  PENTA18
C       PYRAM5   PYRAM13
C       HEXA8    HEXA20   HEXA27

      DATA DEFFAC /
     &  56*0,  56*0,  56*0,   56*0,  56*0,   56*0,
     &  56*0,  56*0,  56*0,   56*0,  56*0,
     &  56*0,  56*0,  1,7*0,1,2,3,4,5,6,7,8,40*0,   56*0,  56*0,   56*0,
     &  56*0,  56*0,
     &  56*0,  3,7*0,  1,4,5,2,10,13,11,7,
     &                 2,5,6,3,11,14,12,8,
     &                 3,6,4,1,12,15,10,9,24*0,                    56*0,
     &  56*0,  56*0,
     &  56*0,  56*0,   56*0   /

C ----------------------------------------------------------------------
      CALL JEMARQ()

C --- VERIFICATION QUE LE CATALOGUE DES TYPES DE MAILLE N'A PAS ETE
C     MODIFIE

      DO 10 ITYP = 1, NBTYMA
         IMPMAI(ITYP) = 0
         CALL JENUNO (JEXNUM('&CATA.TM.NOMTM',ITYP),KBID)
C VERIFICATION COHERENCE CATALOGUE FORTRAN
         CALL ASSERT( NOMAST(ITYP) .EQ. KBID )
         CALL JEVEUO (JEXNUM('&CATA.TM.NBNO',REFTYP(ITYP)),'L',JNOEU)
         NBREF(ITYP) = ZI(JNOEU)
 10   CONTINUE

C --- DIMENSIONS DU PROBLEME

      CALL JEVEUO(MAIN//'.DIME', 'L', JDIM)
      NBNO   = ZI(JDIM-1 + 1)
      NBMATO = ZI(JDIM-1 + 3)

C --- CREATION DES NOEUDS SUPPLEMENTAIRES SUR LES FACES

      NOMIMA = '&&CM1518.NOMIMA'
      MILIEU = '&&CM1518.MILIEU'
      NOMIPE = '&&CM1518.NOMIPE'
      CALL WKVECT(NOMIMA,'V V I',3*NBMA,     JNOMIM)
      CALL WKVECT(MILIEU,'V V I',4*NFMAX*NBNO,   JMILIE)
      CALL WKVECT(NOMIPE,'V V I',8*3*NBMA,   JNOMIP)
      CALL JEVEUO(MAIN//'.TYPMAIL','L',JTYPMA)

      CALL CM18NA(MAIN,NBMA,NBNO,LIMA,ZI(JTYPMA),
     &           ZI(JMILIE),ZI(JNOMIM),ZI(JNOMIP),NBNOMI,
     &           NBTYMA,DEFFAC)

C --- DUPLICATION A L'IDENTIQUE

      CALL JEDUPO(MAIN//'.GROUPENO','G',MAOUT//'.GROUPENO',.FALSE.)
      CALL JEDUPO(MAIN//'.NOMMAI'  ,'G',MAOUT//'.NOMMAI'  ,.FALSE.)
      CALL JEDUPO(MAIN//'.GROUPEMA','G',MAOUT//'.GROUPEMA',.FALSE.)

C --- DIMENSION DU MAILLAGE : NOMBRE TOTAL DE NOEUDS

      NBTOT = NBNO + NBNOMI
      CALL JEDUPO(MAIN//'.DIME','G',MAOUT//'.DIME',.FALSE.)
      CALL JEVEUO(MAOUT//'.DIME','E',JDIM)
      ZI(JDIM-1 + 1) = NBTOT

C --- REPERTOIRE DE NOM DES NOEUDS : COPIE DE LA PARTIE COMMUNE

      NOMNOI = MAIN  // '.NOMNOE'
      NOMNOE = MAOUT // '.NOMNOE'
      CALL JECREO(NOMNOE,'G N K8')
      CALL JEECRA(NOMNOE,'NOMMAX', NBTOT,' ')

      DO 5 NO = 1,NBNO
         CALL JENUNO(JEXNUM(NOMNOI,NO),NOMND)
         CALL JECROC(JEXNOM(NOMNOE,NOMND))
 5    CONTINUE

C --- CHAM_GEOM : RECOPIE DE LA PARTIE COMMUNE

      COORDO = MAOUT // '.COORDO'
      CALL COPISD('CHAMP_GD','G',MAIN//'.COORDO',COORDO)
      CALL JEVEUO(COORDO//'.REFE','E',JREFE)
      ZK24(JREFE) = MAOUT
      CALL JUVECA(COORDO//'.VALE', NBTOT*3)

C --- MISE A JOUR DES NOUVEAUX NOEUDS (NOM ET COORDONNEES)

      CALL JEVEUO(COORDO//'.VALE','E',JCOOR)
      CALL CM18ND(NBNO,NBNOMI,NBMA,LIMA,ZI(JTYPMA),
     &            PREFIX,NDINIT,ZI(JNOMIP),NOMNOE,ZR(JCOOR))

C --- MISE A JOUR DES MAILLES

      CALL DISMOI('F','NB_NO_MAX','&CATA','CATALOGUE',NBNOMX,KBID,IRET)
      TYPEMA = MAOUT // '.TYPMAIL'
      CONNEI = MAIN //'.CONNEX'
      CONNEO = MAOUT//'.CONNEX'
      CALL JEDUPO(MAIN//'.TYPMAIL' ,'G',TYPEMA,.FALSE.)
      CALL JEVEUO(TYPEMA,'E',JTYPMA)
      CALL JECREC(CONNEO,'G V I','NU','CONTIG','VARIABLE',NBMATO)
      CALL JEECRA(CONNEO,'LONT',NBNOMX*NBMATO,' ')
      CALL CM18MA(NBMATO,NBMA,NBNO,NBNOMI,LIMA,ZI(JTYPMA),
     &            CONNEI,CONNEO,ZI(JNOMIM), NBTYMA, NOMAST, REFTYP,
     &            NBREF, IMPMAI )

C     -- RETASSAGE  DE CONNEO (QUI A ETE ALLOUEE TROP GRANDE) :
      CALL JECCTA(CONNEO)

      CALL JEDETR(NOMIMA)
      CALL JEDETR(MILIEU)
      CALL JEDETR(NOMIPE)

      CALL JEDEMA()
      END
