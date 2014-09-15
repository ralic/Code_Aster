subroutine nmcroi(sd_obsv, keyw_fact, nb_keyw_fact)
!
implicit none
!
#include "asterfort/impfoi.h"
#include "asterfort/nmcrpx.h"
#include "asterfort/getvtx.h"
#include "asterfort/wkvect.h"
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
    integer, intent(in) :: nb_keyw_fact
    character(len=19), intent(in) :: sd_obsv
    character(len=16), intent(in) :: keyw_fact
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - Observation
!
! Read parameters for list of time
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_obsv          : datastructure for observation parameters
! In  keyw_fact        : factor keyword to read observation parameters
! In  nb_keyw_fact     : number of factor keyword to read observation parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_keyw_fact
    character(len=1) :: base
    character(len=19) :: list_inst_obsv
    character(len=2) :: chaine
    character(len=16) :: keyw_step
    character(len=8) :: answer
    character(len=24) :: obsv_init
    character(len=8), pointer :: v_obsv_init(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    base      = 'V'
    keyw_step = 'PAS_OBSE'
    do i_keyw_fact = 1, nb_keyw_fact
        call impfoi(0, 2, i_keyw_fact, chaine)
        list_inst_obsv = sd_obsv(1:14)//chaine(1:2)//'.LI'
        call nmcrpx(keyw_fact, keyw_step, i_keyw_fact, list_inst_obsv, base)
    end do
!
! - Initial observation
!
    obsv_init = sd_obsv(1:14)//'     .INIT'
    call wkvect(obsv_init, 'V V K8', nb_keyw_fact, vk8 = v_obsv_init)
    do i_keyw_fact = 1, nb_keyw_fact
        call getvtx(keyw_fact, 'OBSE_ETAT_INIT', iocc=i_keyw_fact, scal = answer)
        v_obsv_init(i_keyw_fact) = answer
    end do
!
end subroutine
