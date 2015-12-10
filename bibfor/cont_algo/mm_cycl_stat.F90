subroutine mm_cycl_stat(sdstat, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmrvai.h"
#include "asterfort/mm_cycl_erase.h"
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
    character(len=24), intent(in) :: sdstat
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve - Cycling
!
! Statistics
!
! --------------------------------------------------------------------------------------------------
!
! In  sdstat           : datastructure for statistics
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdcont_cyceta
    integer, pointer :: p_sdcont_cyceta(:) => null()
    integer :: cycl_index, cycl_stat
    integer :: i_cont_poin, nb_cont_poin
    aster_logical :: cont_disc, cont_xfem
    integer :: cycl_nb(4)
!
! --------------------------------------------------------------------------------------------------
!
    cycl_nb(1:4) = 0
!
! - Formulation of contact
!
    cont_disc = cfdisl(ds_contact%sdcont_defi,'FORMUL_DISCRETE')
    cont_xfem = cfdisl(ds_contact%sdcont_defi,'FORMUL_XFEM')
    if (cont_disc .or. cont_xfem) goto 99
!
! - Acces to cycling objects
!
    sdcont_cyceta = ds_contact%sdcont_solv(1:14)//'.CYCETA'
    call jeveuo(sdcont_cyceta, 'L', vi = p_sdcont_cyceta)
!
! - Counting cycles
!
    nb_cont_poin = cfdisi(ds_contact%sdcont_defi,'NTPC' )
    do i_cont_poin = 1, nb_cont_poin
        do cycl_index = 1, 4
            cycl_stat = p_sdcont_cyceta(4*(i_cont_poin-1)+cycl_index)
            if (cycl_stat .ne. 0) then
                cycl_nb(cycl_index) = cycl_nb(cycl_index) + 1
            endif
            if (cycl_stat .lt. 0) then
                call mm_cycl_erase(ds_contact, cycl_index, i_cont_poin)
            endif
        end do
    end do
!
! - Saving for statistics
!
    call nmrvai(sdstat, 'CTCC_CYCL_1', 'E', cycl_nb(1))
    call nmrvai(sdstat, 'CTCC_CYCL_2', 'E', cycl_nb(2))
    call nmrvai(sdstat, 'CTCC_CYCL_3', 'E', cycl_nb(3))
    call nmrvai(sdstat, 'CTCC_CYCL_4', 'E', cycl_nb(4))
!
 99 continue
!
end subroutine
