subroutine mm_cycl_flip(sd_cont_defi, sd_cont_solv, cycl_flip)
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
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
    aster_logical, intent(out) :: cycl_flip
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Is flip-flop cycling ?
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition
! Out cycl_flip    : .true. if flip-flop cycling activated
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_eta
    integer, pointer :: p_cycl_eta(:) => null()
    integer :: cycl_index, cycl_stat
    integer :: point_number, point_index
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
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
    call jeveuo(sd_cycl_eta, 'L', vi = p_cycl_eta)
!
! - Flip-flop dectected ?
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
    do point_index = 1, point_number
        cycl_stat = p_cycl_eta(4*(point_index-1)+cycl_index)
        if (cycl_stat .gt. 0) cycl_flip = .true.
    end do
!
    call jedema()
end subroutine
