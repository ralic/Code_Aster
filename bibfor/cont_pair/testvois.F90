subroutine testvois(jv_geom       , elem_slav_type,&
                    elem_mast_coor, elem_mast_code, elem_slav_nume,&
                    pair_tole     , inte_weight ,    v_mesh_connex,&
                    v_connex_lcum)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/apcoor.h"
#include "asterfort/prjint.h"
#include "asterfort/assert.h"
#include "asterfort/dctest.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    integer, intent(in) :: jv_geom
    character(len=8), intent(in) :: elem_slav_type
    real(kind=8),intent(in) :: elem_mast_coor(27)
    character(len=8),intent(in) :: elem_mast_code
    integer,intent(in) :: elem_slav_nume
    real(kind=8),intent(in) :: pair_tole
    real(kind=8),intent(out) :: inte_weight
    integer, pointer, intent(in) :: v_mesh_connex(:)
    integer, pointer, intent(in) :: v_connex_lcum(:) 
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Compute weight of intersection between slave/master element
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  jv_geom          : JEVEUX adress to updated geometry
! In  elem_slav_type   : type of slave element
! In  elem_mast_coor   : coordinates of master element
! In  elem_mast_code   : code of master element
! In  elem_slav_nume   : index of slave element
! In  pair_tole        : tolerance for pairing
! In  elem_dime        : dimension of elements
! Out inte_weight      : weight of intersection
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_elin_mast, i_node, i_elin_slav, i_dime
    integer :: elem_slav_nbnode, elem_dime
    real(kind=8) :: elem_slav_coor(27)
    character(len=8) :: elem_slav_code
    integer :: elin_slav_sub(8,9), elin_mast_sub(8,9)
    integer :: elin_slav_nbnode(8), elin_mast_nbnode(8)
    integer :: elin_slav_nbsub, elin_mast_nbsub 
    real(kind=8) :: elin_slav_coor(27), elin_mast_coor(27)
    character(len=8) :: elin_slav_code, elin_mast_code
    integer :: nb_poin_inte
    real(kind=8) :: ints_weight
    real(kind=8) :: poin_inte(32)
!
! --------------------------------------------------------------------------------------------------
!
    if (elem_slav_nume .ne. 0) then
!
! ----- Get informations about slave element
!
        call apcoor(jv_geom       , elem_slav_type  ,&
                    elem_slav_nume, elem_slav_coor, elem_slav_nbnode,&
                    elem_slav_code, elem_dime, v_mesh_connex, v_connex_lcum)
!
! ----- Cut slave element in linearized sub-elements
!
        call dctest(elem_slav_code, elin_slav_sub, elin_slav_nbnode, elin_slav_nbsub,&
                    elin_slav_code)
!
! ----- Cut master element in linearized sub-elements
!
        call dctest(elem_mast_code, elin_mast_sub, elin_mast_nbnode, elin_mast_nbsub,&
                    elin_mast_code)  
!
        inte_weight=0.d0
!
! ----- Loop on linearized slave sub-elements
!
        do i_elin_slav = 1, elin_slav_nbsub
            elin_slav_coor(:) = 0.d0
!
! --------- Get coordinates for current linearized slave sub-element
!         
            do i_node = 1, elin_slav_nbnode(i_elin_slav)
                do i_dime = 1, elem_dime
                    elin_slav_coor(3*(i_node-1)+i_dime) =&
                        elem_slav_coor(3*(elin_slav_sub(i_elin_slav,i_node)-1)+i_dime) 
                end do 
            end do
!
! --------- Loop on linearized master sub-elements
!           
            do i_elin_mast = 1, elin_mast_nbsub
                elin_mast_coor(:) = 0.d0
!
! ------------- Get coordinates for current linearized master sub-element
!
                do i_node = 1, elin_mast_nbnode(i_elin_mast)
                    do i_dime = 1, elem_dime
                        elin_mast_coor(3*(i_node-1)+i_dime) =&
                            elem_mast_coor(3*(elin_mast_sub(i_elin_mast,i_node)-1)+i_dime) 
                    end do 
                end do       
!
! ------------- Projection/intersection of elements in slave parametric space     
!
                call prjint(pair_tole     , elem_dime,&
                            elin_slav_coor, elin_slav_nbnode(i_elin_slav), elin_slav_code,&
                            elin_mast_coor, elin_mast_nbnode(i_elin_mast), elin_mast_code,&
                            poin_inte     , ints_weight                  , nb_poin_inte  )
!                       
                inte_weight = inte_weight+ints_weight
            end do      
        end do
    endif
end subroutine
