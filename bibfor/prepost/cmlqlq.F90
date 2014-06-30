subroutine cmlqlq(main, maout, nbma, lima, prefix,&
                  ndinit)
!
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
#include "asterfort/assert.h"
#include "asterfort/cmlqdi.h"
#include "asterfort/cmlqma.h"
#include "asterfort/cmlqna.h"
#include "asterfort/cmlqnd.h"
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
!
    integer :: ndinit, nbma, lima(nbma)
    character(len=8) :: main, maout, prefix
! ----------------------------------------------------------------------
!         TRANSFORMATION DES MAILLES LINEAIRES -> QUADRATIQUES
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
    integer :: nbtyma, ityp, jnoeu
    parameter    ( nbtyma = 27 )
    integer :: jnomim, jnomip, jmilie, jdim, nbno, mxar
    integer :: nbnomi, nbtot, no, jcoor, nbnomx, nbmato, jtypma
    integer :: defare(2, 0:12, nbtyma), reftyp(nbtyma), nbref(nbtyma)
    integer :: impmai(nbtyma)
    character(len=8) :: nomnd, kbid, nomast(nbtyma)
    character(len=19) :: coordo
    character(len=24) :: nomima, milieu, nomipe, nomnoe, nomnoi
    character(len=24) :: typema, connei, conneo
    character(len=24), pointer :: refe(:) => null()
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
!     2   SEG2      -->  4    ( SEG2 EN SEG3 )           3
!     3   SEG22     -->  3                               4
!     4   SEG3      -->  4                               3
!     5   SEG33     -->  5                               6
!     6   SEG4      -->  6                               4
!     7   TRIA3     -->  9    ( TRIA3 EN TRIA6 )         6
!     8   TRIA33    -->  8                               6
!     9   TRIA6     -->  9                               6
!     10  TRIA66    -->  10                             12
!     11  TRIA7     -->  11                              7
!     12  QUAD4     -->  14   ( QUAD4 EN QUAD8 )         8
!     13  QUAD44    -->  13                              8
!     14  QUAD8     -->  14                              8
!     15  QUAD88    -->  15                             16
!     16  QUAD9     -->  16                              9
!     17  QUAD99    -->  17                             18
!     18  TETRA4    -->  19   ( TETRA4 EN TETRA10 )     10
!     19  TETRA10   -->  19                             10
!     20  PENTA6    -->  21   ( PENTA6 EN PENTA15 )     15
!     21  PENTA15   -->  21                             15
!     22  PENTA18   -->  22                             18
!     23  PYRAM5    -->  24   ( PYRAM5 EN PYRAM13 )     13
!     24  PYRAM13   -->  24                             13
!     25  HEXA8     -->  26   ( HEXA8 EN HEXA20 )       20
!     26  HEXA20    -->  26                             20
!     27  HEXA27    -->  27                             27
!
    data reftyp /1,4,3,4,5,6,9,8,9,10,11,14,13,14,15,16,17,19,19,21,&
     &             21,22,24,24,26,26,27/
!
! --- EXPLICATIONS DU DATA DEFARE
!
!       POI1   SEG2           SEG22   SEG3   SEG33   SEG4
!       TRIA3                  TRIA33   TRIA6   TRIA66   TRIA7
!       QUAD4,                    QUAD44  QUAD8  QUAD88  QUAD9  QUAD99
!       TETRA4                             TETRA10
!       PENTA6                                        PENTA15  PENTA18
!       PYRAM5                                    PYRAM13
!       HEXA8
!       HEXA20  HEXA27
!
    data defare /&
     &  26*0,  1,0,1,2,22*0,  26*0,   26*0,  26*0,   26*0,&
     &  3,0,1,2,2,3,3,1,18*0,  26*0,    26*0,   26*0,    26*0,&
     &  4,0,1,2,2,3,3,4,4,1,16*0, 26*0,   26*0,  26*0,   26*0,  26*0,&
     &  6,0,1,2,2,3,3,1,1,4,2,4,3,4,12*0,  26*0,&
     &  9,0,1,2,2,3,3,1,1,4,2,5,3,6,4,5,5,6,6,4,6*0,  26*0,  26*0,&
     &  8,0,1,2,2,3,3,4,4,1,1,5,2,5,3,5,4,5,8*0,  26*0,&
     &  12,0,1,2,2,3,3,4,4,1,1,5,2,6,3,7,4,8,5,6,6,7,7,8,8,5,&
     &  26*0,   26*0   /
!
! ----------------------------------------------------------------------
    call jemarq()
!
! --- VERIFICATION QUE LE CATALOGUE DES TYPES DE MAILLE N'A PAS ETE
!     MODIFIE
!
    do ityp = 1, nbtyma
        impmai(ityp) = 0
        call jenuno(jexnum('&CATA.TM.NOMTM', ityp), kbid)
! VERIFICATION COHERENCE CATALOGUE FORTRAN
        ASSERT(nomast(ityp) .eq. kbid)
        call jeveuo(jexnum('&CATA.TM.NBNO', reftyp(ityp)), 'L', jnoeu)
        nbref(ityp) = zi(jnoeu)
    end do
!
! --- DIMENSIONS DU PROBLEME
!
    call jeveuo(main//'.DIME', 'L', jdim)
    nbno = zi(jdim-1 + 1)
    nbmato = zi(jdim-1 + 3)
    mxar = cmlqdi(nbma,nbno,lima,main//'.CONNEX')
!
! --- CREATION DES NOEUDS MILIEUX
!
    nomima = '&&CMLQLQ.NOMIMA'
    milieu = '&&CMLQLQ.MILIEU'
    nomipe = '&&CMLQLQ.NOMIPE'
    call wkvect(nomima, 'V V I', 12*nbma, jnomim)
    call wkvect(milieu, 'V V I', 2*mxar*nbno, jmilie)
    call wkvect(nomipe, 'V V I', 2*12*nbma, jnomip)
    call jeveuo(main//'.TYPMAIL', 'L', jtypma)
!
    call cmlqna(nbma, nbno, lima, main//'.CONNEX', zi(jtypma),&
                mxar, zi(jmilie), zi(jnomim), zi(jnomip), nbnomi,&
                nbtyma, defare)
!
! --- DUPLICATION A L'IDENTIQUE
!
    call cpclma(main, maout, 'GROUPENO', 'G')
    call jedupo(main//'.NOMMAI', 'G', maout//'.NOMMAI', .false._1)
    call cpclma(main, maout, 'GROUPEMA', 'G')
!
! --- DIMENSION DU MAILLAGE : NOMBRE TOTAL DE NOEUDS
!
    nbtot = nbno + nbnomi
    call jedupo(main//'.DIME', 'G', maout//'.DIME', .false._1)
    call jeveuo(maout//'.DIME', 'E', jdim)
    zi(jdim-1 + 1) = nbtot
!
! --- REPERTOIRE DE NOM DES NOEUDS : COPIE DE LA PARTIE COMMUNE
!
    nomnoi = main // '.NOMNOE'
    nomnoe = maout // '.NOMNOE'
    call jecreo(nomnoe, 'G N K8')
    call jeecra(nomnoe, 'NOMMAX', nbtot)
!
    do no = 1, nbno
        call jenuno(jexnum(nomnoi, no), nomnd)
        call jecroc(jexnom(nomnoe, nomnd))
    end do
!
! --- CHAM_GEOM : RECOPIE DE LA PARTIE COMMUNE
!
    coordo = maout // '.COORDO'
    call copisd('CHAMP_GD', 'G', main//'.COORDO', coordo)
    call jeveuo(coordo//'.REFE', 'E', vk24=refe)
    refe(1) = maout
    call juveca(coordo//'.VALE', nbtot*3)
!
! --- MISE A JOUR DES NOUVEAUX NOEUDS (NOM ET COORDONNEES)
!
    call jeveuo(coordo//'.VALE', 'E', jcoor)
    call cmlqnd(nbno, nbnomi, prefix, ndinit, zi(jnomip),&
                nomnoe, zr(jcoor))
!
! --- MISE A JOUR DES MAILLES
!
    call dismoi('NB_NO_MAX', '&CATA', 'CATALOGUE', repi=nbnomx)
    typema = maout // '.TYPMAIL'
    connei = main //'.CONNEX'
    conneo = maout//'.CONNEX'
    call jedupo(main//'.TYPMAIL', 'G', typema, .false._1)
    call jeveuo(typema, 'E', jtypma)
    call jecrec(conneo, 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbmato)
    call jeecra(conneo, 'LONT', nbnomx*nbmato)
!
    call cmlqma(nbmato, nbma, nbno, lima, zi(jtypma),&
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
