!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
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
interface
    subroutine nxrech(model    , mate    , cara_elem, list_load  , nume_dof   ,&
                      tpsthe   , time    , lonch    , compor     , varc_curr  ,&
                      temp_iter, vtempp  , vtempr   , temp_prev  , hydr_prev  ,&
                      hydr_curr, dry_prev, dry_curr , vec2nd     , cnvabt     ,&
                      cnresi   , rho     , iterho   , ther_para_r, ther_para_i)
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=19), intent(in) :: list_load
        character(len=24), intent(in) :: nume_dof
        real(kind=8) :: tpsthe(6)
        character(len=24), intent(in) :: time
        character(len=19), intent(in) :: varc_curr
        integer :: lonch
        character(len=24) :: compor
        character(len=24) :: vtempp
        character(len=24) :: vtempr
        character(len=24) :: temp_prev
        character(len=24) :: temp_iter
        character(len=24) :: hydr_prev
        character(len=24) :: hydr_curr
        character(len=24) :: dry_prev
        character(len=24) :: dry_curr
        character(len=24) :: vec2nd
        character(len=24) :: cnvabt
        character(len=24) :: cnresi
        real(kind=8) :: rho
        integer :: iterho
        real(kind=8) :: ther_para_r(*)
        integer :: ther_para_i(*)
    end subroutine nxrech
end interface
