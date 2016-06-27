subroutine apprin(mesh          , newgeo        , pair_tole      ,nb_elem_mast  ,&
                  list_elem_mast, nb_elem_slav  , list_elem_slav ,elem_mast_flag,&
                  elem_slav_flag, nb_mast_start , elem_mast_start,nb_slav_start ,&
                  elem_slav_start)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/apcoor.h"
#include "asterfort/apdcma.h"
#include "asterfort/prjint.h"
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
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: newgeo
    real(kind=8), intent(in) :: pair_tole
    integer, intent(in) :: nb_elem_mast
    integer, intent(in) :: list_elem_mast(nb_elem_mast)
    integer, intent(in) :: nb_elem_slav
    integer, intent(in) :: list_elem_slav(nb_elem_slav)
    integer, pointer, intent(inout) :: elem_mast_flag(:)
    integer, pointer, intent(inout) :: elem_slav_flag(:)
    integer, intent(out) :: nb_mast_start
    integer, intent(out) :: elem_mast_start(nb_elem_slav)
    integer, intent(out) :: nb_slav_start
    integer, intent(out) :: elem_slav_start(nb_elem_slav)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Find initial elements for pairing by "robust' PANG method
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  pair_tole        : tolerance for pairing
! In  l_not_memory     : .true. is algorithm is the "non-memory" version for standard PANG
! In  nb_elem_mast     : number of master elements on current zone
! In  list_elem_mast   : name of datastructure for list of master elements on current zone
! In  nb_elem_slav     : number of slave elements on current zone
! In  list_elem_slav   : name of datastructure for list of slave elements on current zone
! IO  elem_mast_flag   : flag to mark master elements already tracked
! IO  elem_slav_flag   : flag to mark slave elements already tracked
! Out nb_mast_start    : number of master elements to start algorithm
! Out elem_mast_start  : list of master elements to start algorithm
! Out nb_slav_start    : number of slave elements to start algorithm
! Out elem_slav_start  : list of slave elements to start algorithm
!
! --------------------------------------------------------------------------------------------------
!
    integer :: elem_type_nume
    integer :: elem_slav_nbnode, elem_slav_dime, elem_slav_nume, elem_slav_indx
    character(len=8) :: elem_slav_type, elem_slav_code
    real(kind=8) :: elem_slav_coor(27)
    integer :: elin_slav_nbsub, elin_slav_sub(8,9), elin_slav_nbnode(8)
    integer :: elem_mast_nbnode, elem_mast_dime, elem_mast_nume, elem_mast_indx
    character(len=8) :: elem_mast_type, elem_mast_code, elem_slav_name, elem_mast_name
    real(kind=8) :: elem_mast_coor(27)
    integer :: elin_mast_nbsub, elin_mast_sub(8,4), elin_mast_nbnode(8)
    character(len=8) :: elin_mast_code, elin_slav_code
    real(kind=8) :: elin_mast_coor(27), elin_slav_coor(27)
    integer :: slav_indx_mini, mast_indx_mini
    integer :: jv_geom
    integer :: i_elem_slav, i_elem_mast, i_dime, i_elin_mast, i_elin_slav, i_node
    integer :: nb_poin_inte
    real(kind=8) :: poin_inte(32), inte_weight
    integer, pointer :: v_mesh_typmail(:) => null()
    aster_logical :: debug
!
! --------------------------------------------------------------------------------------------------
!
    debug          = .false.
    mast_indx_mini = minval(list_elem_mast)
    slav_indx_mini = minval(list_elem_slav)
    nb_mast_start  = 0
    nb_slav_start  = 0
    elem_slav_start(1:nb_elem_slav) = 0
    elem_mast_start(1:nb_elem_slav) = 0
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = v_mesh_typmail)
!
! - Access to updated geometry
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jv_geom)
    if (debug) then
        write(*,*)"Find start elements"
    end if
!
! - Loop on slave elements
!
    do i_elem_slav = 1, nb_elem_slav
!
! ----- Current slave element
!
        elem_slav_nume = list_elem_slav(i_elem_slav)
        elem_slav_indx = elem_slav_nume + 1 - slav_indx_mini
        elem_type_nume = v_mesh_typmail(elem_slav_nume)
        call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_slav_type)
        call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
        if (debug) then
            write(*,*) "Slave element", i_elem_slav, elem_slav_nume, elem_slav_name
        end if
!
! ----- Already tracked ?
!
        if (elem_slav_flag(elem_slav_indx) .eq. 0 ) then
            if (debug) then
                write(*,*) "Slave element not yet tracked"
            endif
!
! --------- Get informations about slave element
!
            call apcoor(mesh          , jv_geom       , elem_slav_type  ,&
                        elem_slav_nume, elem_slav_coor, elem_slav_nbnode,&
                        elem_slav_code, elem_slav_dime)
!
! --------- Cut slave element in linearized sub-elements (SEG2 or TRIA3)
!
            call apdcma(elem_slav_code, elin_slav_sub, elin_slav_nbnode, elin_slav_nbsub)
            if (debug) then
                write(*,*) "Cut slave: ", elin_slav_nbsub
            endif
!
! --------- Loop on master elements
!
            do i_elem_mast = 1, nb_elem_mast
!
! ------------- Current master element
!
                elem_mast_nume = list_elem_mast(i_elem_mast)
                elem_mast_indx = elem_mast_nume + 1 - mast_indx_mini
                elem_type_nume = v_mesh_typmail(elem_mast_nume)
                call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_mast_type)
                call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
                if (debug) then
                    write(*,*) "Master element", i_elem_mast, elem_mast_nume, elem_mast_name
                end if
!
! ------------- Already tracked ?
!
                if (elem_mast_flag(elem_mast_indx) .eq. 0 ) then
                    if (debug) then
                        write(*,*) "Master element not yet tracked"
                    endif
!
! ----------------- Get informations about master element
!
                    call apcoor(mesh          , jv_geom       , elem_mast_type  ,&
                                elem_mast_nume, elem_mast_coor, elem_mast_nbnode,&
                                elem_mast_code, elem_mast_dime)
!
! ----------------- Cut master element in linearized sub-elements (SEG2 or TRIA3)
!
                    call apdcma(elem_mast_code, elin_mast_sub, elin_mast_nbnode, elin_mast_nbsub)
                    if (debug) then
                        write(*,*) "Cut master: ", elin_mast_nbsub
                    endif
!
! ----------------- Loop on linearized master sub-elements
!
                    do i_elin_mast = 1, elin_mast_nbsub
!
! --------------------- Code for current linearized master sub-element
!
                        if (elin_mast_nbnode(i_elin_mast) .eq. 2) then
                            elin_mast_code = 'SE2' 
                        elseif (elin_mast_nbnode(i_elin_mast) .eq. 3) then
                            elin_mast_code = 'TR3'
                        else
                            ASSERT(.false.)
                        end if
!
! --------------------- Get coordinates for current linearized master sub-element
!
                        elin_mast_coor(:) = 0.d0
                        do i_node = 1, elin_mast_nbnode(i_elin_mast)
                            do i_dime = 1, elem_slav_dime
                                elin_mast_coor(3*(i_node-1)+i_dime) =&
                                    elem_mast_coor(3*(elin_mast_sub(i_elin_mast,i_node)-1)+i_dime) 
                            end do 
                        end do 
!
! --------------------- Loop on linearized slave sub-elements
!
                        do i_elin_slav = 1, elin_slav_nbsub
!
! ------------------------- Code for current linearized slave sub-element
!
                            if (elin_slav_nbnode(i_elin_slav) .eq. 2) then
                                elin_slav_code = 'SE2'
                            elseif (elin_slav_nbnode(i_elin_slav) .eq. 3) then
                                elin_slav_code = 'TR3'
                            else
                                ASSERT(.false.)
                            endif
!
! ------------------------- Get coordinates for current linearized slave sub-element
!
                            elin_slav_coor(:) = 0.d0
                            do i_node = 1, elin_slav_nbnode(i_elin_slav)
                                do i_dime = 1, elem_slav_dime
                                    elin_slav_coor(3*(i_node-1)+i_dime) =&
                                      elem_slav_coor(3*(elin_slav_sub(i_elin_slav,i_node)-1)+i_dime)
                                end do
                            end do
!
! ------------------------- Projection/intersection of elements in slave parametric space
!
                call prjint(pair_tole     , elem_mast_dime,&
                            elin_slav_coor, elin_slav_nbnode(i_elin_slav), elin_slav_code,&
                            elin_mast_coor, elin_mast_nbnode(i_elin_mast), elin_mast_code,&
                            poin_inte     , inte_weight                  , nb_poin_inte  )
!
! ------------------------- Set start elements
!
                            if (inte_weight .gt. 100*pair_tole) then
                                elem_mast_start(1)             = elem_mast_nume
                                nb_mast_start                  = 1
                                elem_slav_start(1)             = elem_slav_nume
                                nb_slav_start                  = 1
                                elem_slav_flag(elem_slav_indx) = 1
                                call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
                                call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
                                if (debug) then
                                    write(*,*)"Depart trouvé(M/S): ",elem_mast_name,elem_slav_name
                                endif
                                goto 100
                            end if
                        end do
                    end do
                else
                    if (debug) then
                        write(*,*) "Master element not yet tracked"
                    endif
                endif
            end do
        else
            if (debug) then
                write(*,*) "Slave element already tracked"
            endif
        endif     
    end do
100 continue
!
end subroutine
