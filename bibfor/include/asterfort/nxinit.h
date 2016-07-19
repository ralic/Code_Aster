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
    subroutine nxinit(model   , mate    , cara_elem, compor, list_load,&
                      para    , vhydr   , sdobse   , sddisc, sdcrit   ,&
                      ds_inout, nume_dof, l_stat   , l_evol, mesh     ,&
                      time    )
        use NonLin_Datastructure_type
        character(len=24), intent(in) :: model
        character(len=24), intent(in) :: mate
        character(len=24), intent(in) :: cara_elem
        character(len=24), intent(in) :: compor
        character(len=19), intent(in) :: list_load
        real(kind=8), intent(in) :: para(*)
        character(len=24), intent(in) :: vhydr
        character(len=19), intent(out) :: sdobse
        character(len=19), intent(in) :: sddisc
        character(len=19), intent(in) :: sdcrit
        type(NL_DS_InOut), intent(inout) :: ds_inout
        character(len=24), intent(out) :: nume_dof
        aster_logical, intent(out) :: l_stat
        aster_logical, intent(out) :: l_evol
        character(len=8), intent(out) :: mesh
        character(len=24), intent(out) :: time
    end subroutine nxinit
end interface 
