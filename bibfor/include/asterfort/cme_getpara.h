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
interface
    subroutine cme_getpara(option      ,&
                           model       , cara_elem, mate, compor_mult,&
                           v_list_load8, nb_load  ,&
                           rigi_meca   , mass_meca,&
                           time        , time_incr, nh       ,&
                           sigm        , strx     , disp)
        character(len=16), intent(out) :: option
        character(len=8), intent(out) :: model
        character(len=8), intent(out) :: cara_elem
        character(len=24), intent(out) :: mate
        character(len=24), intent(out) :: compor_mult
        character(len=8), intent(out), pointer :: v_list_load8(:)
        integer, intent(out) :: nb_load
        character(len=19), intent(out) :: rigi_meca
        character(len=19), intent(out) :: mass_meca
        real(kind=8), intent(out) :: time
        real(kind=8), intent(out) :: time_incr
        integer, intent(out) :: nh
        character(len=8), intent(out) :: sigm
        character(len=8), intent(out) :: strx
        character(len=8), intent(out) :: disp
    end subroutine cme_getpara
end interface
