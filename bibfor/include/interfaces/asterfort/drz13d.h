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
    subroutine drz13d(noma, ligrmo, type_vale, nb_node, list_node, &
                  cmp_index_dx, cmp_index_dy, cmp_index_dz, cmp_index_drx, cmp_index_dry,&
                  cmp_index_drz, type_lagr, lisrel)
        character(len=8), intent(in)  :: noma
        character(len=19), intent(in) :: ligrmo
        character(len=4), intent(in) :: type_vale
        integer, intent(in) :: nb_node
        character(len=24), intent(in) :: list_node
        character(len=2), intent(in) :: type_lagr
        integer, intent(in) :: cmp_index_dx
        integer, intent(in) :: cmp_index_dy
        integer, intent(in) :: cmp_index_dz
        integer, intent(in) :: cmp_index_drx
        integer, intent(in) :: cmp_index_dry
        integer, intent(in) :: cmp_index_drz
        character(len=19), intent(in) :: lisrel
    end subroutine drz13d
end interface
