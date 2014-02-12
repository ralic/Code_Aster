!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    subroutine mmnewd(type_elem, nb_node  , nb_dim   , elem_coor, pt_coor,&
                      iter_maxi, tole_maxi, proj_dire, ksi1     , ksi2   ,&
                      tang_1   , tang_2   , error)
        character(len=8), intent(in) :: type_elem
        integer, intent(in) :: nb_node
        integer, intent(in) :: nb_dim
        real(kind=8), intent(in) :: elem_coor(27)
        real(kind=8), intent(in) :: pt_coor(3)
        integer, intent(in) :: iter_maxi
        real(kind=8), intent(in) :: tole_maxi
        real(kind=8), intent(in) :: proj_dire(3)
        real(kind=8), intent(out) :: ksi1
        real(kind=8), intent(out) :: ksi2
        real(kind=8), intent(out) :: tang_1(3)
        real(kind=8), intent(out) :: tang_2(3)
        integer, intent(out) :: error
    end subroutine mmnewd
end interface
