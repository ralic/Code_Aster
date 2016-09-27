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
!
interface
    subroutine apprin(mesh          , newgeo        , pair_tole      ,nb_elem_mast  ,&
                      list_elem_mast, nb_elem_slav  , list_elem_slav ,elem_slav_flag,&
                      nb_mast_start , elem_mast_start,nb_slav_start  ,elem_slav_start)
        character(len=8), intent(in) :: mesh
        character(len=19), intent(in) :: newgeo
        real(kind=8), intent(in) :: pair_tole
        integer, intent(in) :: nb_elem_mast
        integer, intent(in) :: list_elem_mast(nb_elem_mast)
        integer, intent(in) :: nb_elem_slav
        integer, intent(in) :: list_elem_slav(nb_elem_slav)
        integer, pointer, intent(inout) :: elem_slav_flag(:)
        integer, intent(out) :: nb_mast_start
        integer, intent(out) :: elem_mast_start(nb_elem_slav)
        integer, intent(out) :: nb_slav_start
        integer, intent(out) :: elem_slav_start(nb_elem_slav)
    end subroutine apprin
end interface
