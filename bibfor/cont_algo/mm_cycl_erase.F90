subroutine mm_cycl_erase(sdcont_defi, sdcont_solv, cycl_type, point_curr)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
    integer, intent(in) :: cycl_type
    integer, intent(in) :: point_curr
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Erase cycling informations
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_solv      : name of contact solving datastructure
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  cycl_type        : type of cycling to erase
!                     0 - for erasing for all cycles
! In  point_curr       : contact point to erasing
!                     0 - when erasing all cycles
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cyclis
    integer, pointer :: p_sdcont_cyclis(:) => null()
    character(len=24) :: sdcont_cycnbr
    integer, pointer :: p_sdcont_cycnbr(:) => null()
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    integer :: nb_cont_poin, i_cont_poin
    integer :: cycl_index
    aster_logical :: lctcc
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    lctcc = cfdisl(sdcont_defi,'FORMUL_CONTINUE')
    if (.not.lctcc) then
        goto 99
    endif
!
! - Name of cycling objects
!
    sdcont_cyclis = sdcont_solv(1:14)//'.CYCLIS'
    sdcont_cycnbr = sdcont_solv(1:14)//'.CYCNBR'
    sdcont_cyceta = sdcont_solv(1:14)//'.CYCETA'
!
! - Access to cycling objects
!
    call jeveuo(sdcont_cyclis, 'E', vi = p_sdcont_cyclis)
    call jeveuo(sdcont_cycnbr, 'E', vi = p_sdcont_cycnbr)
    call jeveuo(sdcont_cyceta, 'E', vi = p_sdcont_cyceta)
!
! - Initializations
!
    nb_cont_poin = cfdisi(sdcont_defi,'NTPC' )
!
! - Erasing cycling information
!
    if (cycl_type .eq. 0) then
        ASSERT(point_curr.eq.0)
        do cycl_index = 1, 4
            do i_cont_poin = 1, nb_cont_poin
                p_sdcont_cyclis(4*(i_cont_poin-1)+cycl_index) = 0
                p_sdcont_cycnbr(4*(i_cont_poin-1)+cycl_index) = 0
                p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_index) = 0
            enddo
        end do
    else if (cycl_type.gt.0) then
        ASSERT(point_curr.le.nb_cont_poin)
        ASSERT(point_curr.ge.1)
        ASSERT(cycl_type.ge.1)
        ASSERT(cycl_type.le.4)
        cycl_index = cycl_type
        i_cont_poin = point_curr
        p_sdcont_cyclis(4*(i_cont_poin-1)+cycl_index) = 0
        p_sdcont_cycnbr(4*(i_cont_poin-1)+cycl_index) = 0
        p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_index) = 0
    else
        ASSERT(.false.)
    endif
!
 99 continue
!
    call jedema()
end subroutine
