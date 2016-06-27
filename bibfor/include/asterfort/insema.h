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
    subroutine insema(elem_nbnode , elem_dime, elem_coor , pair_tole,&
                      xp1         , yp1      , xp2       , yp2      ,&
                      nb_poin_inte, poin_inte, inte_neigh)
        integer, intent(in) :: elem_nbnode
        integer, intent(in) :: elem_dime
        real(kind=8), intent(in) :: elem_coor(2,elem_nbnode)
        real(kind=8) :: pair_tole
        real(kind=8), intent(in) :: xp1
        real(kind=8), intent(in) :: yp1
        real(kind=8), intent(in) :: xp2
        real(kind=8), intent(in) :: yp2
        integer, intent(inout) :: nb_poin_inte
        real(kind=8), intent(inout) :: poin_inte(elem_dime-1,16)
        integer, intent(inout) :: inte_neigh(4)
    end subroutine insema
end interface
