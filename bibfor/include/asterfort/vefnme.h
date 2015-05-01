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
interface
    subroutine vefnme(option, base  , model , mate      , carele  ,&
                      compor, partps, nh    , ligrelz   , varicomz,&
                      sigmaz, strxz , deplz , depl_incrz, vecelz)
        character(len=16), intent(in) :: option
        character(len=1), intent(in) :: base
        character(len=8), intent(in) :: model
        real(kind=8), intent(in) :: partps(*)
        character(len=24), intent(in) :: carele
        character(len=24), intent(in) :: mate
        character(len=*), intent(in) :: ligrelz
        integer, intent(in) :: nh
        character(len=19), intent(in) :: compor
        character(len=*), intent(in) :: sigmaz
        character(len=*), intent(in) :: varicomz
        character(len=*), intent(in) :: strxz
        character(len=*), intent(in) :: deplz
        character(len=*), intent(in) :: depl_incrz
        character(len=*), intent(inout) :: vecelz
    end subroutine vefnme
end interface
