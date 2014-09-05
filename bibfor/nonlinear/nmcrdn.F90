subroutine nmcrdn(sd_suiv, keyw_fact, nb_dof_monitor, nb_keyw_fact)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/impfoi.h"
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
    character(len=24), intent(in) :: sd_suiv
    character(len=16), intent(in) :: keyw_fact
    integer, intent(in) :: nb_dof_monitor
    integer, intent(in) :: nb_keyw_fact
!
! --------------------------------------------------------------------------------------------------
!
! Non-linear operators - DOF monitor
!
! Create datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  sd_suiv          : datastructure for dof monitor parameters
! In  keyw_fact        : factor keyword to read extraction parameters
! In  nb_keyw_fact     : number of factor keyword to read extraction parameters
! In  nb_dof_monitor   : number of factor dofs monitored
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_keyw_fact, i_dof_monitor, nb_line_title
    character(len=16) :: title(3)
    character(len=1) :: chaine
    character(len=24) :: dofm_titl
    character(len=16), pointer :: v_dofm_titl(:) => null()
!
! --------------------------------------------------------------------------------------------------
!

!
! - Create title vector
!
    dofm_titl = sd_suiv(1:14)//'     .TITR'
    call wkvect(dofm_titl, 'V V K16', 3*nb_dof_monitor, vk16 = v_dofm_titl)
!
! - Title from user
!
    do i_keyw_fact = 1, nb_keyw_fact
        call impfoi(0, 1, i_keyw_fact, chaine)
        title(1) = '    SUIVI '
        title(2) = '     DDL  '
        title(3) = '     '//chaine
        call getvtx(keyw_fact, 'TITRE', iocc=i_keyw_fact, nbval=0, nbret=nb_line_title)
        nb_line_title = - nb_line_title
        ASSERT(nb_line_title.le.3)
        if (nb_line_title .ne. 0) then
            call getvtx(keyw_fact, 'TITRE', iocc=i_keyw_fact, nbval=nb_line_title, vect=title)
        endif
        v_dofm_titl(3*(i_keyw_fact-1)+1) = title(1)
        v_dofm_titl(3*(i_keyw_fact-1)+2) = title(2)
        v_dofm_titl(3*(i_keyw_fact-1)+3) = title(3)
    end do
!
! - Automatic
!
    if (nb_dof_monitor .gt. nb_keyw_fact) then
        do i_dof_monitor = nb_keyw_fact+1, nb_dof_monitor
            call impfoi(0, 1, i_dof_monitor, chaine)
            title(1) = '    SUIVI '
            title(2) = '     DDL  '
            title(3) = '     '//chaine
            v_dofm_titl(3*(i_dof_monitor-1)+1) = title(1)
            v_dofm_titl(3*(i_dof_monitor-1)+2) = title(2)
            v_dofm_titl(3*(i_dof_monitor-1)+3) = title(3)
        end do
    endif
!
end subroutine
