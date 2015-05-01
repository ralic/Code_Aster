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
    subroutine get_elas_para(fami     , j_mater, poum, ipg, ispg, &
                             elas_type,&
                             time     ,&
                             e   , nu  , g,&
                             e1  , e2  , e3,&
                             nu12, nu13, nu23,&
                             g1  , g2  , g3)
        character(len=*), intent(in) :: fami
        integer, intent(in) :: j_mater
        character(len=*), intent(in) :: poum
        integer, intent(in) :: ipg
        integer, intent(in) :: ispg
        integer, intent(out) :: elas_type
        real(kind=8), optional, intent(in) :: time
        real(kind=8), optional, intent(out) :: e
        real(kind=8), optional, intent(out) :: nu
        real(kind=8), optional, intent(out) :: e1
        real(kind=8), optional, intent(out) :: e2
        real(kind=8), optional, intent(out) :: e3
        real(kind=8), optional, intent(out) :: nu12
        real(kind=8), optional, intent(out) :: nu13
        real(kind=8), optional, intent(out) :: nu23
        real(kind=8), optional, intent(out) :: g1
        real(kind=8), optional, intent(out) :: g2
        real(kind=8), optional, intent(out) :: g3
        real(kind=8), optional, intent(out) :: g
    end subroutine get_elas_para
end interface
