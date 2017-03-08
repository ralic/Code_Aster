!
! COPYRIGHT (C) 1991 - 2017  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine fointe_varc(codmes   , fami        , kpg           , ksp           , poum,&
                           func_name, nb_para_user, para_name_user, para_vale_user,&
                           resu     , ier)
        character(len=*), intent(in) :: codmes
        character(len=*), intent(in) :: fami
        integer, intent(in) :: kpg
        integer, intent(in) :: ksp
        character(len=*), intent(in) :: poum
        character(len=*), intent(in) :: func_name
        integer, intent(in) :: nb_para_user
        character(len=*), intent(in) :: para_name_user(*)
        real(kind=8), intent(in) :: para_vale_user(*)
        real(kind=8), intent(out) :: resu
        integer, intent(out) :: ier
    end subroutine fointe_varc
end interface
