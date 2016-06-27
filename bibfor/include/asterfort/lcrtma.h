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
    subroutine lcrtma(elem_dime       , proj_tole,&
                      tria_coor       , &
                      elin_slav_nbnode, elin_slav_coor, elin_slav_code,&
                      elem_mast_nbnode, elem_mast_coor, elem_mast_code,&
                      tria_coot)
        integer, intent(in) :: elem_dime
        real(kind=8), intent(in) :: proj_tole
        real(kind=8), intent(in) :: tria_coor(elem_dime-1,3)
        integer, intent(in) :: elin_slav_nbnode
        real(kind=8), intent(in) :: elin_slav_coor(elem_dime,elin_slav_nbnode)
        character(len=8), intent(in) :: elin_slav_code
        integer, intent(in) :: elem_mast_nbnode
        real(kind=8), intent(in) :: elem_mast_coor(elem_dime,elem_mast_nbnode)
        character(len=8), intent(in) :: elem_mast_code
        real(kind=8), intent(out) :: tria_coot(2,3)
    end subroutine lcrtma
end interface
