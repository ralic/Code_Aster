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
    subroutine lccoes(elem_dime   , nb_node_slav   , nb_lagr  ,&
                      norm_smooth , norm           , indi_lagc,&
                      poidpg      , shape_slav_func, jaco_upda,&
                      mmat        )
        integer, intent(in) :: elem_dime
        integer, intent(in) :: nb_node_slav
        integer, intent(in) :: nb_lagr
        integer, intent(in) :: norm_smooth
        real(kind=8), intent(in) :: norm(3)
        integer, intent(in) :: indi_lagc(10)
        real(kind=8), intent(in) :: poidpg
        real(kind=8), intent(in) :: shape_slav_func(9)
        real(kind=8), intent(in) :: jaco_upda
        real(kind=8), intent(inout) :: mmat(55,55)
     end subroutine lccoes
end interface
