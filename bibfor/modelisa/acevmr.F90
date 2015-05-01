subroutine acevmr(nbocc, noma, noemax, noemaf)
    implicit none
#include "jeveux.h"
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
    integer :: nbocc, noemax
    character(len=8) :: noma
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES DIMENSIONS POUR LES MASSES REPARTIES
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! IN  : NOMA   : NOM DU MAILLAGE
! OUT : NOEMAX : NOMBRE TOTAL DE NOEUDS MAX
! ----------------------------------------------------------------------
    character(len=24) :: magrma, manoma
    character(len=8) :: k8b
    integer :: iarg
!-----------------------------------------------------------------------
    integer :: i,   ii, ij, in, inoe
    integer :: ioc, ldgm, ldnm, nb, nbgr, nbgrmx, nbv
    integer :: nm, nn, noema2, noemaf
    character(len=24), pointer :: group_ma(:) => null()
    integer, pointer :: parno2(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    nbgrmx = 0
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    do 10 ioc = 1, nbocc
!        --- ON RECUPERE UNE LISTE DE GROUP_MA ---
        call getvem(noma, 'GROUP_MA', 'MASS_AJOU', 'GROUP_MA', ioc,&
                    iarg, 0, k8b, nbgr)
        nbgr = -nbgr
        nbgrmx = max(nbgrmx,nbgr)
10  end do
    AS_ALLOCATE(vk24=group_ma, size=nbgrmx)
    noemax = 0
    noemaf = 0
    do 11 ioc = 1, nbocc
        noema2 = 0
        call getvem(noma, 'GROUP_MA', 'MASS_AJOU', 'GROUP_MA', ioc,&
                    iarg, 0, k8b, nbgr)
        nbgr = -nbgr
        call getvem(noma, 'GROUP_MA', 'MASS_AJOU', 'GROUP_MA', ioc,&
                    iarg, nbgr, group_ma, nbv)
!
!        --- ON ECLATE LES GROUP_MA ---
        do 20 i = 1, nbgr
            call jelira(jexnom(magrma, group_ma(i)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, group_ma(i)), 'L', ldgm)
            do 22 in = 0, nb-1
                call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
                call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
                do 24 nn = 1, nm
                    inoe = zi(ldnm+nn-1)
                    noema2 = max(noema2,inoe)
24              continue
22          continue
20      continue
        noemaf = max(noemaf,noema2)
        AS_ALLOCATE(vi=parno2, size=noema2)
        do 41 i = 1, nbgr
            call jelira(jexnom(magrma, group_ma(i)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, group_ma(i)), 'L', ldgm)
            do 43 in = 0, nb-1
                call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
                call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
                do 45 nn = 1, nm
                    inoe = zi(ldnm+nn-1)
                    parno2(inoe) = parno2(inoe) + 1
45              continue
43          continue
41      continue
        ii = 0
        do 51 ij = 1, noema2
            if (parno2(ij) .eq. 0) goto 51
            ii = ii + 1
51      continue
        noema2 = ii
        noemax = noemax + noema2
        AS_DEALLOCATE(vi=parno2)
11  end do
    AS_DEALLOCATE(vk24=group_ma)
!
    call jedema()
end subroutine
