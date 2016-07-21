!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine nxacmv(model      , mate     , cara_elem, list_load, nume_dof   ,&
                      solver     , l_stat   , time     , tpsthe   , vtemp      ,&
                      vhydr      , varc_curr, dry_prev , dry_curr , cn2mbr_stat,&
                      cn2mbr_tran, matass   , maprec   , cndiri   , cncine     ,&
                      mediri     , compor)
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: list_load
        character(len=24), intent(in) :: nume_dof
        character(len=19), intent(in) :: solver
        character(len=24), intent(in) :: time
        character(len=19), intent(in) :: varc_curr
        aster_logical, intent(in) :: l_stat
        real(kind=8), intent(in) :: tpsthe(6)
        character(len=24), intent(in) :: vtemp
        character(len=24), intent(in) :: vhydr
        character(len=24), intent(in) :: dry_prev
        character(len=24), intent(in) :: dry_curr
        character(len=24), intent(in) :: cn2mbr_stat
        character(len=24), intent(in) :: cn2mbr_tran
        character(len=24), intent(in) :: matass
        character(len=19), intent(in) :: maprec
        character(len=24), intent(in) :: cndiri
        character(len=24), intent(out) :: cncine
        character(len=24), intent(in) :: mediri
        character(len=24), intent(in) :: compor
    end subroutine nxacmv
end interface
