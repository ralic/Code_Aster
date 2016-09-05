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
    subroutine nxnewt(model    , mate       , cara_elem  , list_load, nume_dof ,&
                      solver   , tpsthe     , time       , matass   , cn2mbr   ,&
                      maprec   , cnchci     , varc_curr  , temp_prev, temp_iter,&
                      vtempp   , vec2nd     , mediri     , conver   , hydr_prev,&
                      hydr_curr, dry_prev   , dry_curr   , compor   , cnvabt   ,&
                      cnresi   , ther_crit_i, ther_crit_r, reasma   , testr    ,&
                      testm    , ds_algorom)
        use ROM_Datastructure_type
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: list_load
        character(len=24), intent(in) :: nume_dof
        character(len=19), intent(in) :: solver
        real(kind=8) :: tpsthe(6)
        character(len=24), intent(in) :: time
        character(len=19), intent(in) :: varc_curr
        character(len=24) :: matass
        character(len=19) :: maprec
        character(len=24) :: cnchci
        character(len=24) :: cn2mbr
        character(len=24) :: temp_prev
        character(len=24) :: temp_iter
        character(len=24) :: vtempp
        character(len=24) :: vec2nd
        character(len=24) :: mediri
        aster_logical :: conver
        character(len=24) :: hydr_prev
        character(len=24) :: hydr_curr
        character(len=24) :: dry_prev
        character(len=24) :: dry_curr
        character(len=24) :: compor
        character(len=24) :: cnvabt
        character(len=24) :: cnresi
        integer :: ther_crit_i(*)
        real(kind=8) :: ther_crit_r(*)
        aster_logical :: reasma
        real(kind=8) :: testr
        real(kind=8) :: testm
        type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    end subroutine nxnewt
end interface
