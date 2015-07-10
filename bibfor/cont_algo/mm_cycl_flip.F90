subroutine mm_cycl_flip(sdcont_defi, sdcont_solv, cycl_flip)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisi.h"
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
    aster_logical, intent(out) :: cycl_flip
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Is flip-flop cycling ?
!
! --------------------------------------------------------------------------------------------------
!
! In  sdcont_solv      : name of contact solving datastructure
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! Out cycl_flip        : .true. if flip-flop cycling activated
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    integer :: cycl_index, cycl_stat
    integer :: nb_cont_poin, i_cont_poin
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    cycl_flip = .false.
    cycl_index = 4
!
! - Cycling objects
!
    sdcont_cyceta = sdcont_solv(1:14)//'.CYCETA'
    call jeveuo(sdcont_cyceta, 'L', vi = p_sdcont_cyceta)
!
! - Flip-flop dectected ?
!
    nb_cont_poin = cfdisi(sdcont_defi,'NTPC' )
    do i_cont_poin = 1, nb_cont_poin
        cycl_stat = p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_index)
        if (cycl_stat .gt. 0) cycl_flip = .true.
    end do
!
    call jedema()
end subroutine
