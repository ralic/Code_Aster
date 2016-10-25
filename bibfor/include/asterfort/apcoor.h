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
    subroutine apcoor(jv_geom  , elem_type, elem_nume, elem_coor,&
                      elem_nbnode, elem_code, elem_dime, v_mesh_connex, v_connex_lcum)
        integer, intent(in) :: jv_geom
        character(len=8), intent(in) :: elem_type
        integer, intent(in) :: elem_nume
        real(kind=8), intent(out) :: elem_coor(27)
        integer, intent(out) :: elem_nbnode
        character(len=8), intent(out) :: elem_code
        integer, intent(out) :: elem_dime
        integer, pointer, intent(in) :: v_mesh_connex(:)
        integer, pointer, intent(in) :: v_connex_lcum(:)  
    end subroutine apcoor
end interface
