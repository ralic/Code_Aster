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
    subroutine epsvmc(fami   , nno    , ndim  , nbsig, npg   ,&
                      j_poids, j_vf   , j_dfde, xyz  , disp  ,&
                      time   , repere, nharm, option,  epsi   )
        character(len=*), intent(in) :: fami
        integer, intent(in) :: nno
        integer, intent(in) :: ndim
        integer, intent(in) :: nbsig
        integer, intent(in) :: npg
        integer, intent(in) :: j_poids
        integer, intent(in) :: j_vf
        integer, intent(in) :: j_dfde
        real(kind=8), intent(in) :: xyz(1)
        real(kind=8), intent(in) :: disp(1)
        real(kind=8), intent(in) :: time
        real(kind=8), intent(in) :: repere(7)
        real(kind=8), intent(in) :: nharm
        character(len=16), intent(in) :: option
        real(kind=8), intent(out) :: epsi(1)
    end subroutine epsvmc
end interface
