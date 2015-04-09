!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! aslint: disable=W1504
!
#include "asterf_types.h"
!
interface
    subroutine nxpred(model , mate  , cara_elem, list_load, nume_dof,&
                      solver, lostat, time     , lonch    , matass  ,&
                      maprec, vtemp , vtempm   , vtempp   , vhydr   ,&
                      vhydrp, tmpchi, tmpchf   , compor   , cndirp  ,&
                      cnchci, vec2nd, vec2ni)
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: list_load
        character(len=24), intent(in) :: nume_dof
        character(len=19), intent(in) :: solver
        character(len=24), intent(in) :: time
        aster_logical :: lostat
        integer :: lonch
        character(len=24) :: matass
        character(len=19) :: maprec
        character(len=24) :: vtemp
        character(len=24) :: vtempm
        character(len=24) :: vtempp
        character(len=24) :: vhydr
        character(len=24) :: vhydrp
        character(len=24) :: tmpchi
        character(len=24) :: tmpchf
        character(len=24) :: compor
        character(len=24) :: cndirp
        character(len=24) :: cnchci
        character(len=24) :: vec2nd
        character(len=24) :: vec2ni
    end subroutine nxpred
end interface
