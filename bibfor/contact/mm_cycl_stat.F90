subroutine mm_cycl_stat(sd_stat, sd_cont_defi, sd_cont_solv)
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
#include "asterfort/nmrvai.h"
#include "asterfort/mm_cycl_erase.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: sd_stat
    character(len=24), intent(in) :: sd_cont_defi
    character(len=24), intent(in) :: sd_cont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact (continue method) - Cycling
!
! Statistics
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_stat      : data structure for non-linear stats
! In  sd_cont_solv : data structure for contact solving
! In  sd_cont_defi : data structure from contact definition
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sd_cycl_eta
    integer, pointer :: p_cycl_eta(:) => null()
    integer :: cycl_index, cycl_stat
    integer :: point_index, point_number
    aster_logical :: cont_disc, cont_xfem
    integer :: cycl_nb(4)
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Formulation of contact
!
    cont_disc = cfdisl(sd_cont_defi,'FORMUL_DISCRETE')
    cont_xfem = cfdisl(sd_cont_defi,'FORMUL_XFEM')
    if (cont_disc .or. cont_xfem) goto 99
!
! - Initializations
!
    do cycl_index = 1, 4
        cycl_nb(cycl_index) = 0
    enddo
!
! - Acces to cycling objects
!
    sd_cycl_eta = sd_cont_solv(1:14)//'.CYCETA'
    call jeveuo(sd_cycl_eta, 'L', vi = p_cycl_eta)
!
! - Counting cycles
!
    point_number = cfdisi(sd_cont_defi,'NTPC' )
    do point_index = 1, point_number
        do cycl_index = 1, 4
            cycl_stat = p_cycl_eta(4*(point_index-1)+cycl_index)
            if (cycl_stat .ne. 0) then
                cycl_nb(cycl_index) = cycl_nb(cycl_index) + 1
            endif
            if (cycl_stat .lt. 0) then
                call mm_cycl_erase(sd_cont_defi, sd_cont_solv, cycl_index, point_index)
            endif
        end do
    end do
!
! - Saving for statistics
!
    call nmrvai(sd_stat, 'CTCC_CYCL_1', 'E', cycl_nb(1))
    call nmrvai(sd_stat, 'CTCC_CYCL_2', 'E', cycl_nb(2))
    call nmrvai(sd_stat, 'CTCC_CYCL_3', 'E', cycl_nb(3))
    call nmrvai(sd_stat, 'CTCC_CYCL_4', 'E', cycl_nb(4))
!
 99 continue
!
    call jedema()
end subroutine
