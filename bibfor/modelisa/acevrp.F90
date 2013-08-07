subroutine acevrp(nbocc, noma, noemax, noemaf)
    implicit none
#include "jeveux.h"
!
#include "asterfort/getvem.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: nbocc, noemax
    character(len=8) :: noma
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES DIMENSIONS POUR LES RAIDEURS REPARTIES
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! IN  : NOMA   : NOM DU MAILLAGE
! OUT : NOEMAX : NOMBRE TOTAL DE NOEUDS MAX
! ----------------------------------------------------------------------
    character(len=24) :: magrma, manoma
    character(len=8) :: k8b
    integer :: iarg
!-----------------------------------------------------------------------
    integer :: i, idgm, idno2, ii, ij, in, inoe
    integer :: ioc, ldgm, ldnm, nb, nbgr, nbgrmx, nbv
    integer :: nm, nn, noema2, noemaf
!-----------------------------------------------------------------------
    call jemarq()
    nbgrmx = 0
    magrma = noma//'.GROUPEMA'
    manoma = noma//'.CONNEX'
    do 10 ioc = 1, nbocc
!        --- ON RECUPERE UNE LISTE DE GROUP_MA ---
        call getvem(noma, 'GROUP_MA', 'RIGI_PARASOL', 'GROUP_MA', ioc,&
                    iarg, 0, k8b, nbgr)
        nbgr = -nbgr
        nbgrmx = max(nbgrmx,nbgr)
10  end do
    call wkvect('&&ACEVRP.GROUP_MA', 'V V K24', nbgrmx, idgm)
    noemax = 0
    noemaf = 0
    do 11 ioc = 1, nbocc
        noema2 = 0
        call getvem(noma, 'GROUP_MA', 'RIGI_PARASOL', 'GROUP_MA', ioc,&
                    iarg, 0, k8b, nbgr)
        nbgr = -nbgr
        call getvem(noma, 'GROUP_MA', 'RIGI_PARASOL', 'GROUP_MA', ioc,&
                    iarg, nbgr, zk24(idgm), nbv)
!
!        --- ON ECLATE LES GROUP_MA ---
        do 20 i = 1, nbgr
            call jelira(jexnom(magrma, zk24(idgm+i-1)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, zk24(idgm+i-1)), 'L', ldgm)
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
        call wkvect('&&ACEVRP.PARNO2', 'V V I', noema2, idno2)
        do 41 i = 1, nbgr
            call jelira(jexnom(magrma, zk24(idgm+i-1)), 'LONUTI', nb)
            call jeveuo(jexnom(magrma, zk24(idgm+i-1)), 'L', ldgm)
            do 43 in = 0, nb-1
                call jelira(jexnum(manoma, zi(ldgm+in)), 'LONMAX', nm)
                call jeveuo(jexnum(manoma, zi(ldgm+in)), 'L', ldnm)
                do 45 nn = 1, nm
                    inoe = zi(ldnm+nn-1)
                    zi(idno2+inoe-1) = zi(idno2+inoe-1) + 1
45              continue
43          continue
41      continue
        ii = 0
        do 51 ij = 1, noema2
            if (zi(idno2+ij-1) .eq. 0) goto 51
            ii = ii + 1
51      continue
        noema2 = ii
        noemax = noemax + noema2
        call jedetr('&&ACEVRP.PARNO2')
11  end do
    call jedetr('&&ACEVRP.GROUP_MA')
!
    call jedema()
end subroutine
