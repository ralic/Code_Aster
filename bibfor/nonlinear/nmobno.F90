subroutine nmobno(sd_obsv, keyw_fact, nb_keyw_fact)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/impfoi.h"
#include "asterfort/wkvect.h"
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
    integer, intent(in) :: nb_keyw_fact
    character(len=19), intent(in) :: sd_obsv
    character(len=16), intent(in) :: keyw_fact
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - Observation
!
! Name of observations
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_obsv          : datastructure for observation parameters
! In  keyw_fact        : factor keyword to read observation parameters
! In  nb_keyw_fact     : number of factor keyword to read observation parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_keyw_fact, nb_title
    character(len=24) :: obsv_titl
    character(len=16), pointer :: v_obsv_titl(:) => null()
    character(len=16) :: title
    character(len=1) :: chaine
!
! --------------------------------------------------------------------------------------------------
!
!
! - Create vector for title
!
    obsv_titl = sd_obsv(1:14)//'     .TITR'
    call wkvect(obsv_titl, 'V V K16', nb_keyw_fact, vk16 = v_obsv_titl)
!
! - Set titles
!
    do i_keyw_fact = 1, nb_keyw_fact
        call impfoi(0, 1, i_keyw_fact, chaine)
        title = 'OBSERVATION_'//chaine
        call getvtx(keyw_fact, 'TITRE', iocc=i_keyw_fact, nbval=0, nbret=nb_title)
        nb_title = - nb_title
        ASSERT(nb_title.le.1)
        if (nb_title .ne. 0) then
            call getvtx(keyw_fact, 'TITRE', iocc=i_keyw_fact, nbval=nb_title, vect=title)
        endif
        v_obsv_titl(i_keyw_fact) = title
    end do
!
end subroutine
