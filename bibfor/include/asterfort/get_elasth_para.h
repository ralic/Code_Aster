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
    subroutine get_elasth_para(fami     , j_mater     , poum   , ipg    , ispg,&
                               elas_type, elas_keyword, materiz,&
                               alpha    , alpha_l     , alpha_t, alpha_n)
        character(len=*), intent(in) :: fami
        integer, intent(in) :: j_mater
        character(len=*), intent(in) :: poum
        integer, intent(in) :: ipg
        integer, intent(in) :: ispg
        integer, intent(in) :: elas_type
        character(len=16), intent(in) :: elas_keyword
        character(len=8), optional, intent(in) :: materiz
        real(kind=8), optional, intent(out) :: alpha(2)
        real(kind=8), optional, intent(out) :: alpha_l
        real(kind=8), optional, intent(out) :: alpha_t
        real(kind=8), optional, intent(out) :: alpha_n
    end subroutine get_elasth_para
end interface
