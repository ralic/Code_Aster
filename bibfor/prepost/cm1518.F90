subroutine cm1518(main, maout, nbma, lima, prefix,&
                  ndinit)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cm18ma.h"
#include "asterfort/cm18na.h"
#include "asterfort/cm18nd.h"
#include "asterfort/copisd.h"
#include "asterfort/cpclma.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeccta.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/wkvect.h"
    integer :: ndinit, nbma, lima(nbma)
    character(len=8) :: main, maout, prefix
!
! ----------------------------------------------------------------------
!         TRANSFORMATION DES MAILLES PENTA15 EN PENTA18
! ----------------------------------------------------------------------
! IN        MAIN   K8  NOM DU MAILLAGE INITIAL
! IN/JXOUT  MAOUT  K8  NOM DU MAILLAGE TRANSFORME
! IN        NBMA    I  NOMBRE DE MAILLES A TRAITER
! IN        LIMA    I  NUMERO ET TYPE DES MAILLES A TRAITER
! IN        PREFIX K8  PREFIXE DU NOM DES NOEUDS CREES (EX: N, NO, ...)
! IN        NDINIT  I  NUMERO INITIAL DES NOEUDS CREES
! ----------------------------------------------------------------------
!
!
    integer :: nbtyma, ityp, jnoeu, nfmax
    parameter    ( nbtyma = 27, nfmax = 24 )
    integer :: jnomim, jnomip, jmilie, jdim, nbno, iret
    integer :: nbnomi, nbtot, no, jcoor, nbnomx, nbmato
    integer :: deffac(8, 0:6, nbtyma), jtypma, jrefe, reftyp(nbtyma)
    integer :: nbref(nbtyma), impmai(nbtyma)
    character(len=8) :: nomnd, kbid, nomast(nbtyma)
    character(len=19) :: coordo
    character(len=24) :: nomima, milieu, nomipe, nomnoe, nomnoi
    character(len=24) :: typema, connei, conneo
!
!     MAILLES TRAITEES PAR LA COMMANDE :
!
    data nomast / 'POI1    ', 'SEG2    ', 'SEG22   ', 'SEG3    ',&
     &              'SEG33   ', 'SEG4    ', 'TRIA3   ', 'TRIA33  ',&
     &              'TRIA6   ', 'TRIA66  ', 'TRIA7   ', 'QUAD4   ',&
     &              'QUAD44  ', 'QUAD8   ', 'QUAD88  ', 'QUAD9   ',&
     &              'QUAD99  ', 'TETRA4  ', 'TETRA10 ', 'PENTA6  ',&
     &              'PENTA15 ', 'PENTA18 ', 'PYRAM5  ', 'PYRAM13 ',&
     &              'HEXA8   ', 'HEXA20  ', 'HEXA27  '/
!
!     A PARTIR DU CATALOGUE TYPE_MAILLE__  :
!     REFERENCE     -->  NOUVEAU TYPE              NB DE NOEUDS
!
!                      REFTYP                         NBREF
!
!     1   POI1      -->  1                               1
!     2   SEG2      -->  2                               2
!     3   SEG22     -->  3                               4
!     4   SEG3      -->  4                               3
!     5   SEG33     -->  5                               6
!     6   SEG4      -->  6                               4
!     7   TRIA3     -->  7                               3
!     8   TRIA33    -->  8                               6
!     9   TRIA6     -->  9                               6
!     10  TRIA66    -->  10                             12
!     11  TRIA7     -->  11                              7
!     12  QUAD4     -->  12                              4
!     13  QUAD44    -->  13                              8
!     14  QUAD8     -->  16                              9
!     15  QUAD88    -->  15                             16
!     16  QUAD9     -->  16                              9
!     17  QUAD99    -->  17                             18
!     18  TETRA4    -->  18                              4
!     19  TETRA10   -->  19                             10
!     20  PENTA6    -->  20                              6
!     21  PENTA15   -->  22                             18
!     22  PENTA18   -->  22                             18
!     23  PYRAM5    -->  23                              5
!     24  PYRAM13   -->  24                             13
!     25  HEXA8     -->  25                              8
!     26  HEXA20    -->  26                             20
!     27  HEXA27    -->  27                             27
!
    data reftyp /1,2,3,4,5,6,7,8,9,10,11,12,13,16,15,16,17,18,19,20,&
     &             22,22,23,24,25,26,27/
!
! --- EXPLICATIONS DU DATA DEFFAC
!
!       POI1     SEG2     SEG22   SEG3   SEG33   SEG4
!       TRIA3    TRIA33   TRIA6   TRIA66   TRIA7
!       QUAD4,   QUAD44   QUAD8   QUAD88  QUAD9  QUAD99
!       TETRA4   TETRA10
!       PENTA6   PENTA15  PENTA18
!       PYRAM5   PYRAM13
!       HEXA8    HEXA20   HEXA27
!
    data deffac /&
     &  56*0,  56*0,  56*0,   56*0,  56*0,   56*0,&
     &  56*0,  56*0,  56*0,   56*0,  56*0,&
     &  56*0,  56*0,  1,7*0,1,2,3,4,5,6,7,8,40*0,   56*0,  56*0,   56*0,&
     &  56*0,  56*0,&
     &  56*0,  3,7*0,  1,4,5,2,10,13,11,7,&
     &                 2,5,6,3,11,14,12,8,&
     &                 3,6,4,1,12,15,10,9,24*0,                    56*0,&
     &  56*0,  56*0,&
     &  56*0,  56*0,   56*0   /
!
! ----------------------------------------------------------------------
    call jemarq()
!
! --- VERIFICATION QUE LE CATALOGUE DES TYPES DE MAILLE N'A PAS ETE
!     MODIFIE
!
    do 10 ityp = 1, nbtyma
        impmai(ityp) = 0
        call jenuno(jexnum('&CATA.TM.NOMTM', ityp), kbid)
! VERIFICATION COHERENCE CATALOGUE FORTRAN
        ASSERT(nomast(ityp) .eq. kbid)
        call jeveuo(jexnum('&CATA.TM.NBNO', reftyp(ityp)), 'L', jnoeu)
        nbref(ityp) = zi(jnoeu)
10  end do
!
! --- DIMENSIONS DU PROBLEME
!
    call jeveuo(main//'.DIME', 'L', jdim)
    nbno = zi(jdim-1 + 1)
    nbmato = zi(jdim-1 + 3)
!
! --- CREATION DES NOEUDS SUPPLEMENTAIRES SUR LES FACES
!
    nomima = '&&CM1518.NOMIMA'
    milieu = '&&CM1518.MILIEU'
    nomipe = '&&CM1518.NOMIPE'
    call wkvect(nomima, 'V V I', 3*nbma, jnomim)
    call wkvect(milieu, 'V V I', 4*nfmax*nbno, jmilie)
    call wkvect(nomipe, 'V V I', 8*3*nbma, jnomip)
    call jeveuo(main//'.TYPMAIL', 'L', jtypma)
!
    call cm18na(main, nbma, nbno, lima, zi(jtypma),&
                zi(jmilie), zi(jnomim), zi(jnomip), nbnomi, nbtyma,&
                deffac)
!
! --- DUPLICATION A L'IDENTIQUE
!
    call cpclma(main, maout, 'GROUPENO', 'G')
    call jedupo(main//'.NOMMAI', 'G', maout//'.NOMMAI', .false.)
    call cpclma(main, maout, 'GROUPEMA', 'G')
!
! --- DIMENSION DU MAILLAGE : NOMBRE TOTAL DE NOEUDS
!
    nbtot = nbno + nbnomi
    call jedupo(main//'.DIME', 'G', maout//'.DIME', .false.)
    call jeveuo(maout//'.DIME', 'E', jdim)
    zi(jdim-1 + 1) = nbtot
!
! --- REPERTOIRE DE NOM DES NOEUDS : COPIE DE LA PARTIE COMMUNE
!
    nomnoi = main // '.NOMNOE'
    nomnoe = maout // '.NOMNOE'
    call jecreo(nomnoe, 'G N K8')
    call jeecra(nomnoe, 'NOMMAX', nbtot, ' ')
!
    do 5 no = 1, nbno
        call jenuno(jexnum(nomnoi, no), nomnd)
        call jecroc(jexnom(nomnoe, nomnd))
 5  end do
!
! --- CHAM_GEOM : RECOPIE DE LA PARTIE COMMUNE
!
    coordo = maout // '.COORDO'
    call copisd('CHAMP_GD', 'G', main//'.COORDO', coordo)
    call jeveuo(coordo//'.REFE', 'E', jrefe)
    zk24(jrefe) = maout
    call juveca(coordo//'.VALE', nbtot*3)
!
! --- MISE A JOUR DES NOUVEAUX NOEUDS (NOM ET COORDONNEES)
!
    call jeveuo(coordo//'.VALE', 'E', jcoor)
    call cm18nd(nbno, nbnomi, prefix, ndinit, zi(jnomip),&
                nomnoe, zr(jcoor))
!
! --- MISE A JOUR DES MAILLES
!
    call dismoi('F', 'NB_NO_MAX', '&CATA', 'CATALOGUE', nbnomx,&
                kbid, iret)
    typema = maout // '.TYPMAIL'
    connei = main //'.CONNEX'
    conneo = maout//'.CONNEX'
    call jedupo(main//'.TYPMAIL', 'G', typema, .false.)
    call jeveuo(typema, 'E', jtypma)
    call jecrec(conneo, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmato)
    call jeecra(conneo, 'LONT', nbnomx*nbmato, ' ')
    call cm18ma(nbmato, nbma, nbno, lima, zi(jtypma),&
                connei, conneo, zi(jnomim), nbtyma, nomast,&
                reftyp, nbref, impmai)
!
!     -- RETASSAGE  DE CONNEO (QUI A ETE ALLOUEE TROP GRANDE) :
    call jeccta(conneo)
!
    call jedetr(nomima)
    call jedetr(milieu)
    call jedetr(nomipe)
!
    call jedema()
end subroutine
