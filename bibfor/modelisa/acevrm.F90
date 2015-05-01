subroutine acevrm(nbocc, noma, noemax, noemaf)
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
    implicit none
#include "jeveux.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
!
    integer :: nbocc, noemax
    character(len=8) :: noma
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     VERIFICATION DES DIMENSIONS POUR LES RAIDEURS REPARTIES
! ----------------------------------------------------------------------
! IN  : NBOCC  : NOMBRE D'OCCURENCE
! IN  : NOMA   : NOM DU MAILLAGE
! OUT : NOEMAX : NOMBRE TOTAL DE NOEUDS MAX
! ----------------------------------------------------------------------
    character(len=24) :: magrma
    character(len=24) :: nogp, nogl
!-----------------------------------------------------------------------
    integer :: in, ioc, ldgm, ngl, ngp, nma, noemaf
!
!-----------------------------------------------------------------------
    call jemarq()
    magrma = noma//'.GROUPEMA'
    noemax = 0
    noemaf = 0
! --- BOUCLE SUR LES OCCURENCES DE DISCRET
    do 10 ioc = 1, nbocc
        call getvtx('RIGI_MISS_3D', 'GROUP_MA_POI1', iocc=ioc, scal=nogp, nbret=ngp)
        call getvtx('RIGI_MISS_3D', 'GROUP_MA_SEG2', iocc=ioc, scal=nogl, nbret=ngl)
!
        if (ngp .ne. 0) then
            call jelira(jexnom(magrma, nogp), 'LONUTI', nma)
            call jeveuo(jexnom(magrma, nogp), 'L', ldgm)
            do 11 in = 0, nma-1
                noemaf = max(noemaf,zi(ldgm+in))
11          continue
            noemax = noemax + nma
        endif
        if (ngl .ne. 0) then
            call jelira(jexnom(magrma, nogl), 'LONUTI', nma)
            call jeveuo(jexnom(magrma, nogl), 'L', ldgm)
            do 12 in = 0, nma-1
                noemaf = max(noemaf,zi(ldgm+in))
12          continue
            noemax = noemax + nma
        endif
10  end do
!
    call jedema()
end subroutine
