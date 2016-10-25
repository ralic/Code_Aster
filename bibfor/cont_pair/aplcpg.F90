subroutine aplcpg(mesh        , newgeo        , sdappa      , i_zone        , pair_tole,&
                  nb_elem_mast, list_elem_mast, nb_elem_slav, list_elem_slav, &
                  nb_pair_zone, list_pair_zone, i_proc      , nb_proc, pair_method)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8nnem.h"
#include "asterfort/jecrec.h"
#include "asterfort/jexatr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/apcoor.h"
#include "asterfort/prjint.h"
#include "asterfort/gapint.h"
#include "asterfort/jecroc.h"
#include "asterfort/clpoma.h"
#include "asterfort/assert.h"
#include "asterfort/apdcma.h"
#include "asterfort/ap_infast.h"
#include "asterfort/apprin.h"
#include "asterfort/apdmae.h"
#include "asterfort/aprtpe.h"
#include "asterfort/cncinv.h"
#include "asterfort/wkvect.h"
#include "asterfort/codent.h"
#include "asterfort/testvois.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/apsave_pair.h"
#include "asterfort/apsave_patch.h"
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
! aslint: disable=W1306
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: newgeo
    character(len=19), intent(in) :: sdappa
    integer, intent(in) :: i_zone
    real(kind=8), intent(in) :: pair_tole
    integer, intent(in) :: nb_elem_slav
    integer, intent(in) :: nb_elem_mast
    integer, intent(in) :: list_elem_mast(nb_elem_mast)
    integer, intent(in) :: list_elem_slav(nb_elem_slav)
    integer, intent(inout) :: nb_pair_zone
    integer, pointer, intent(inout) :: list_pair_zone(:)
    integer, intent(in) :: i_proc
    integer, intent(in) :: nb_proc
    character(len=24), intent(in) :: pair_method
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Pairing by PANG method
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  sdappa           : name of pairing datastructure
! In  i_zone           : index of contact zone
! In  pair_tole        : tolerance for pairing
! In  nb_elem_mast     : number of master elements on current zone
! In  nb_elem_slav     : number of slave elements on current zone
! In  list_elem_mast   : name of datastructure for list of master elements on current zone
! In  list_elem_slav   : name of datastructure for list of slave elements on current zone
! IO  nb_pair_zone     : number of contact elements
! IO  list_pair_zone   : list of contact elements
!
! --------------------------------------------------------------------------------------------------
!
    integer :: list_pair(nb_elem_mast), nbpatch_t, iret
    integer :: elem_slav_nbnode, elem_slav_nume, elem_slav_dime, elem_slav_indx
    integer :: elem_mast_nbnode, elem_mast_nume, elem_mast_dime, elem_mast_indx
    character(len=8) :: elem_mast_code, elem_slav_code
    character(len=8) :: elem_slav_type, elem_mast_type
    real(kind=8) :: elem_mast_coor(27), elem_slav_coor(27)
    integer :: nb_pair, nb_poin_inte
    integer :: i_mast_neigh, i_elin_mast, i_elin_slav, i_slav_start, i_mast_start, i_find_mast
    integer :: i_node, i_dime, i_slav_neigh, i_neigh
    integer :: patch_indx
    real(kind=8) :: total_weight, inte_weight, gap_moy, elem_slav_weight
    real(kind=8) :: poin_inte(32)
    integer :: elin_mast_nbsub, elin_mast_sub(8,4), elin_mast_nbnode(8)
    integer :: elin_slav_nbsub, elin_slav_sub(8,9), elin_slav_nbnode(8)
    real(kind=8) :: elin_mast_coor(27), elin_slav_coor(27)
    character(len=8) :: elin_mast_code, elin_slav_code, elem_slav_name, elem_mast_name, elem_name
    integer :: nb_slav_start, nb_find_mast, nb_mast_start
    integer :: list_find_mast(nb_elem_mast)
    integer :: elem_start, elem_slav_start(nb_elem_slav), elem_mast_start(nb_elem_slav), elem_nume
    integer :: slav_indx_mini, mast_indx_mini, slav_indx_maxi, mast_indx_maxi
    integer :: elem_neigh_indx, mast_find_indx, elem_slav_neigh, elem_mast_neigh
    aster_logical :: l_recup, debug
    integer, pointer :: mast_find_flag(:) => null()
    integer, pointer :: elem_mast_flag(:) => null()
    integer, pointer :: elem_slav_flag(:) => null()
    character(len=8) :: knuzo
    character(len=24) :: sdappa_slne, sdappa_mane
    integer, pointer :: v_sdappa_slne(:) => null()
    integer, pointer :: v_sdappa_mane(:) => null()
    integer :: list_slav_master(4)
    integer :: nb_mast_neigh, nb_slav_neigh
    integer :: inte_neigh(4), inte_neigh_aux(4)
    integer :: jv_geom, elem_type_nume
    real(kind=8) :: list_slav_weight(4), weight_test, tole_weight
    character(len=24) :: sdappa_gapi, sdappa_coef, njv_weight_c, njv_weight_t,njv_nb_pair_zmpi
    real(kind=8), pointer :: v_sdappa_gapi(:) => null()
    real(kind=8), pointer :: v_sdappa_coef(:) => null()
    real(kind=8), pointer :: patch_weight_t(:) => null()
    real(kind=8), pointer :: patch_weight_c(:) => null()
    integer, pointer :: v_mesh_comapa(:) => null()
    integer, pointer :: v_mesh_typmail(:) => null()
    integer, pointer :: nb_pair_zmpi(:) => null()
    integer, pointer :: list_pair_zmpi(:) => null()
    integer, pointer :: v_mesh_connex(:)  => null()
    integer, pointer :: v_connex_lcum(:)  => null()
    character(len=16), pointer :: valk(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
! 
! - Initializations
!
    debug                          = .false.
    inte_neigh(1:4)                = 0
    list_slav_master(1:4)          = 0
    list_slav_weight(1:4)          = 0.d0
    call jelira(mesh//'.PATCH','NUTIOC', nbpatch_t)
    nbpatch_t= nbpatch_t-1
    njv_weight_c=sdappa(1:19)//'.PWC '
    call wkvect(njv_weight_c, "V V R", nbpatch_t, vr=patch_weight_c)
    njv_weight_t=sdappa(1:19)//'.PWT '
    call wkvect(njv_weight_t, "V V R", nbpatch_t, vr=patch_weight_t)
    njv_nb_pair_zmpi=sdappa(1:19)//'.NAPP'
    call wkvect(njv_nb_pair_zmpi, "V V I", nb_proc, vi=nb_pair_zmpi)
    call jeexin(sdappa(1:19)//'.MPID',iret)
    if (iret .eq. 0) then
        call wkvect(sdappa(1:19)//'.MPID','V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call wkvect(sdappa(1:19)//'.MPIE','V V K16',1,vk16=valk)
        valk(1)='MPI_INCOMPLET'
    else 
        call jeveuo(sdappa(1:19)//'.MPID', 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
        call jeveuo(sdappa(1:19)//'.MPIE', 'E',vk16=valk)
        valk(1)='MPI_INCOMPLET'
    endif   
    mast_indx_maxi = maxval(list_elem_mast)
    slav_indx_maxi = maxval(list_elem_slav)
    mast_indx_mini = minval(list_elem_mast)
    slav_indx_mini = minval(list_elem_slav)
!
! - Access to pairing datastructures
!
    sdappa_gapi = sdappa(1:19)//'.GAPI'
    sdappa_coef = sdappa(1:19)//'.COEF'
    call jeveuo(sdappa_gapi, 'E', vr = v_sdappa_gapi)
    call jeveuo(sdappa_coef, 'E', vr = v_sdappa_coef)
    AS_ALLOCATE(vi=list_pair_zmpi, size= 3*nb_elem_slav*nb_elem_mast)
!
! - Access to updated geometry
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', jv_geom)
!
! - Access to mesh
!
    call jeveuo(mesh//'.TYPMAIL', 'L', vi = v_mesh_typmail)
    call jeveuo(mesh//'.COMAPA','L', vi = v_mesh_comapa)
    call jeveuo(mesh//'.CONNEX', 'L', vi = v_mesh_connex)
    call jeveuo(jexatr(mesh//'.CONNEX', 'LONCUM'), 'L', vi = v_connex_lcum)
!
! - Objects for flags
!
    AS_ALLOCATE(vi=elem_slav_flag, size= slav_indx_maxi+1-slav_indx_mini)
    AS_ALLOCATE(vi=mast_find_flag, size= mast_indx_maxi+1-mast_indx_mini)
    AS_ALLOCATE(vi=elem_mast_flag, size= mast_indx_maxi+1-mast_indx_mini)
    list_find_mast(1:nb_elem_mast) = 0
!
! - Object for neighbours (inverse connectivity)
!
    ASSERT(i_zone .le. 9)
    call codent(i_zone, 'G', knuzo)
    sdappa_mane = sdappa(1:19)//'.MAN'//knuzo(1:1)
    sdappa_slne = sdappa(1:19)//'.ESN'//knuzo(1:1)
    call jeveuo(sdappa_mane, 'L', vi = v_sdappa_mane)
    call jeveuo(sdappa_slne, 'L', vi = v_sdappa_slne)
!
! - Find initial elements for pairing by PANG method
!
120 continue
    if (debug) then 
        write(*,*)'Recherche mailles de départ'
    end if
    if (pair_method .eq. "RAPIDE") then
        call ap_infast(mesh           , newgeo       , pair_tole      , nb_elem_mast  ,&
                       list_elem_mast , nb_elem_slav , list_elem_slav ,elem_slav_flag ,&
                       nb_mast_start, elem_mast_start, nb_slav_start  ,elem_slav_start,&
                       sdappa, i_zone)
    elseif (pair_method .eq. "ROBUSTE") then
        call apprin(mesh           , newgeo       , pair_tole      , nb_elem_mast  ,&
                    list_elem_mast , nb_elem_slav , list_elem_slav , elem_slav_flag ,&
                    nb_mast_start, elem_mast_start, nb_slav_start  , elem_slav_start)
    endif
    if (debug) then
        if (nb_slav_start .eq. 0) then
            write(*,*) ". No more slave start element "
        else
            call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_start(1)), elem_slav_name)
            write(*,*) ". Start slave element: ", elem_slav_name
        endif
        if (nb_mast_start .eq. 0) then
            write(*,*) ". No more master start element "
        else
            call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_start(1)), elem_mast_name)
            write(*,*) ". Start master element: ", elem_mast_name
        endif     
    endif
    if (nb_slav_start.eq.0) then
        goto 110
    end if
!
! - Pairing
!
    if (debug) then 
        write(*,*)'Boucle appariement PANG'
    end if
    do while(nb_slav_start .gt. 0)
!
! ----- Get slave element start
!
        elem_slav_nume = elem_slav_start(1)
        elem_slav_indx = elem_slav_nume +1 - slav_indx_mini
        elem_type_nume = v_mesh_typmail(elem_slav_nume)
        call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_slav_type)
        call jenuno(jexnum(mesh//'.NOMMAI', elem_slav_nume), elem_slav_name)
!
! ----- Shift list of slave element start
!
        do i_slav_start = 1, nb_slav_start - 1
            elem_slav_start(i_slav_start) = elem_slav_start(i_slav_start+1)
        end do
        nb_slav_start = nb_slav_start - 1
!
! ----- Get current patch
!
        patch_indx = v_mesh_comapa(elem_slav_nume)
        if (debug) then
            write(*,*) "Current patch: ", patch_indx
        endif
!
! ----- Get informations about slave element
!
        call apcoor(jv_geom       , elem_slav_type  ,&
                    elem_slav_nume, elem_slav_coor, elem_slav_nbnode,&
                    elem_slav_code, elem_slav_dime, v_mesh_connex   ,&
                    v_connex_lcum)
        if (debug) then 
            write(*,*) "Current slave element: ", elem_slav_nume, elem_slav_name,&
                       '(type : ', elem_slav_code, ')' 
        endif
!
! ----- Cut element in linearized sub-elements
!
        call apdmae(elem_slav_code, elin_slav_sub, elin_slav_nbnode, elin_slav_nbsub)
!
! ----- Compute weight of element
!
        call clpoma(elem_slav_dime  , elem_slav_code, elem_slav_coor, elem_slav_nbnode,&
                    elem_slav_weight)
        if (debug) then
            write(*,*) "Current slave element is cut: ", elin_slav_nbsub,&
                       "- Weight: ", elem_slav_weight
        endif
!
! ----- Total weight for patch
!                    
        patch_weight_t(patch_indx) = patch_weight_t(patch_indx) + elem_slav_weight
!
! ----- Number of neighbours
!
        if (elem_slav_dime .eq. 2) then
            nb_slav_neigh = 2
        elseif (elem_slav_code .eq. 'TR3' .or.&
                elem_slav_code .eq. 'TR6') then
            nb_slav_neigh = 3
        elseif (elem_slav_code .eq. 'QU4' .or.&
                elem_slav_code .eq. 'QU8' .or.&
                elem_slav_code .eq. 'QU9') then
            nb_slav_neigh = 4
        else
            ASSERT(.false.)
        endif
!
! ----- Access to neighbours
!
        if (debug) then
            do i_slav_neigh = 1, nb_slav_neigh
                elem_nume = v_sdappa_slne((elem_slav_indx-1)*4+i_slav_neigh)
                if (elem_nume .ne. 0) then
                    call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
                else
                    elem_name = 'None'
                endif
                write(*,*) "Current slave element neighbours: ", elem_name
            end do
        endif
        list_slav_master(1:nb_slav_neigh) = 0 
        list_slav_weight(1:4)             = 0.d0   
!
! ----- Get master element to start
!
        elem_start     = elem_mast_start(1)
        mast_find_indx = elem_start + 1 - mast_indx_mini
!
! ----- Shift list of master element start
!
        do i_mast_start = 1, nb_mast_start-1
            elem_mast_start(i_mast_start) = elem_mast_start(i_mast_start+1)
        end do
        nb_mast_start     = nb_mast_start-1
!
! ----- Management of list of master elements: first element to seek
!
        list_find_mast(1)              = elem_start
        nb_find_mast                   = 1
        mast_find_flag(mast_find_indx) = 1
!
! ----- Initialization list of contact pairs
!
        do i_elin_mast = 1, nb_elem_mast
            list_pair(i_elin_mast) = 0
        end do
        nb_pair = 0
        l_recup = .true.
!
! ----- Loop on master elements
!
        do while(nb_find_mast .gt. 0)
!
            total_weight = 0.d0
!
! --------- Get master element
!
            elem_mast_nume = list_find_mast(1)
            elem_mast_indx = elem_mast_nume+1-mast_indx_mini
            elem_type_nume = v_mesh_typmail(elem_mast_nume)
            call jenuno(jexnum('&CATA.TM.NOMTM', elem_type_nume), elem_mast_type)
            call jenuno(jexnum(mesh//'.NOMMAI', elem_mast_nume), elem_mast_name)
            if (debug) then 
                write(*,*) "Current master element: ", elem_mast_nume, elem_mast_name,&
                           '(type : ', elem_mast_type, ')' 
            endif
!
! --------- Access to neighbours
!        
            if (debug) then
                do i_mast_neigh = 1, 4
                    elem_nume = v_sdappa_mane((elem_mast_indx-1)*4+i_mast_neigh)
                    if (elem_nume .ne. 0) then
                        call jenuno(jexnum(mesh//'.NOMMAI', elem_nume), elem_name)
                    else
                        elem_name = 'None'
                    endif
                    write(*,*) "Current master element neighbours: ", elem_name
                end do
            endif
!
! --------- Shift list of master elements (on supprime de la liste)
!
            do i_find_mast = 1, nb_find_mast-1
                list_find_mast(i_find_mast) = list_find_mast(i_find_mast+1)
            end do
            nb_find_mast = nb_find_mast-1
!
! --------- Get informations about master element
!
            call apcoor(jv_geom       , elem_mast_type  ,&
                        elem_mast_nume, elem_mast_coor, elem_mast_nbnode,&
                        elem_mast_code, elem_mast_dime, v_mesh_connex   ,&
                        v_connex_lcum)
!
! --------- Cut master element in linearized sub-elements
!
            call apdcma(elem_mast_code, elin_mast_sub, elin_mast_nbnode, elin_mast_nbsub)
            if (debug) then
                write(*,*) "Current slave element is cut: ", elin_mast_nbsub
            endif
!
! --------- Loop on linearized slave sub-elements
!
            inte_neigh(1:4) = 0
            do i_elin_slav = 1, elin_slav_nbsub
!
                inte_neigh_aux(1:4) = 0
!
! ------------- Code for current linearized slave sub-element
!
                if  (elin_slav_nbnode(i_elin_slav) .eq. 2 .and.&
                     elem_slav_dime .eq. 2) then
                    elin_slav_code = 'SE2'
                elseif (elin_slav_nbnode(i_elin_slav) .eq. 3 .and.&
                        elem_slav_dime .eq. 2) then
                    elin_slav_code = 'SE3'        
                elseif (elin_slav_nbnode(i_elin_slav) .eq. 3 .and.&
                        elem_slav_dime .eq. 3) then
                    elin_slav_code = 'TR3'
                elseif (elin_slav_nbnode(i_elin_slav) .eq. 4 .and.&
                        elem_slav_dime .eq. 3) then
                    elin_slav_code = 'QU4'
                elseif (elin_slav_nbnode(i_elin_slav) .eq. 6 .and.&
                        elem_slav_dime .eq. 3) then
                    elin_slav_code = 'TR6'
                elseif (elin_slav_nbnode(i_elin_slav) .eq. 9 .and.&
                        elem_slav_dime .eq. 3) then
                    elin_slav_code = 'QU9'
                else 
                    ASSERT(.false.)
                end if
!
! ------------- Coordinates for current linearized slave sub-element
!
                elin_slav_coor(:) = 0.d0
                do i_node = 1, elin_slav_nbnode(i_elin_slav)
                    do i_dime = 1, elem_slav_dime
                        elin_slav_coor(3*(i_node-1)+i_dime) =&
                            elem_slav_coor(3*(elin_slav_sub(i_elin_slav,i_node)-1)+i_dime)
                    end do
                end do
!
! ------------- Loop on linearized master sub-elements
!
                do i_elin_mast = 1, elin_mast_nbsub
!
! ----------------- Code for current linearized master sub-element
!
                    if (elin_mast_nbnode(i_elin_mast) .eq. 2 .and. elem_slav_dime .eq. 2) then
                        elin_mast_code = 'SE2'
                    elseif (elin_mast_nbnode(i_elin_mast) .eq. 3 .and. elem_slav_dime .eq. 3) then
                        elin_mast_code = 'TR3'
                    elseif (elin_mast_nbnode(i_elin_mast) .eq. 4 .and. elem_slav_dime .eq. 3) then
                        elin_mast_code = 'QU4'
                    else
                        ASSERT(.false.)
                    endif
!
! ----------------- Get coordinates for current linearized master sub-element
!
                    elin_mast_coor(:) = 0.d0
                    do i_node = 1, elin_mast_nbnode(i_elin_mast)
                        do i_dime = 1,elem_slav_dime
                             elin_mast_coor(3*(i_node-1)+i_dime) = &
                                elem_mast_coor(3*(elin_mast_sub(i_elin_mast,i_node)-1)+i_dime)
                        end do
                    end do
!
! ----------------- Projection/intersection of elements in slave parametric space     
!
                    call prjint(pair_tole     , elem_slav_dime,&
                                elin_slav_coor, elin_slav_nbnode(i_elin_slav), elin_slav_code,&
                                elin_mast_coor, elin_mast_nbnode(i_elin_mast), elin_mast_code,&
                                poin_inte     , inte_weight                  , nb_poin_inte  ,&
                                inte_neigh_ = inte_neigh_aux)
                    if (debug) then
                        write(*,*) "Intersection - Master: ", elem_mast_name, i_elin_mast
                        write(*,*) "Intersection - Slave : ", elem_slav_name, i_elin_slav
                        write(*,*) "Intersection - Poids : ", inte_weight
                        write(*,*) "Intersection - Nb    : ", nb_poin_inte
                        write(*,*) "Intersection - Points: ", poin_inte
                    endif
!
! ----------------- Non-void intersection  
!            
                    if (inte_weight .gt. pair_tole) then
!
! --------------------- Set neighbours
!
                        if (elin_slav_code .ne. elem_slav_code .and.&
                            elem_slav_code .eq. 'QU4' ) then
                            if (i_elin_slav .eq. 1) then
                                inte_neigh(1) = inte_neigh_aux(1)
                                inte_neigh(2) = inte_neigh_aux(2)
                            elseif (i_elin_slav .eq. 2) then 
                                inte_neigh(3) = inte_neigh_aux(1)
                                inte_neigh(4) = inte_neigh_aux(2)
                            else 
                                ASSERT(.false.)
                            end if
                        elseif (elin_slav_code .ne. elem_slav_code .and.&
                                elem_slav_code .eq. 'QU8') then
                            if (i_elin_slav .eq. 1) then
                                inte_neigh(1) = inte_neigh_aux(2)
                                inte_neigh(4) = inte_neigh_aux(3)
                            elseif (i_elin_slav .eq. 2) then 
                                inte_neigh(1) = inte_neigh_aux(1)
                                inte_neigh(2) = inte_neigh_aux(2)
                            elseif (i_elin_slav .eq. 3) then 
                                inte_neigh(2) = inte_neigh_aux(1)
                                inte_neigh(3) = inte_neigh_aux(2)
                            elseif (i_elin_slav .eq. 4) then 
                                inte_neigh(3) = inte_neigh_aux(1)
                                inte_neigh(4) = inte_neigh_aux(2)
                            end if
                        else
                            do i_neigh = 1,nb_slav_neigh
                                if (inte_neigh_aux(i_neigh).ne.0) then
                                    inte_neigh(i_neigh) = inte_neigh_aux(i_neigh)
                                endif
                            end do     
                        end if
!
                        total_weight = total_weight+inte_weight
!
! --------------------- Projection from para. space of element into sub-element para. space
!
                        call aprtpe(elem_slav_dime, poin_inte  , nb_poin_inte,&
                                    elem_slav_code, i_elin_slav)
!
! --------------------- Compute mean square gap and weight of intersection
!
                        call gapint(pair_tole     , elem_slav_dime,&
                                    elem_slav_code, elem_slav_nbnode, elem_slav_coor,&
                                    elem_mast_code, elem_mast_nbnode, elem_mast_coor,&
                                    nb_poin_inte  , poin_inte                    , &
                                    gap_moy       , inte_weight                  )
!
! --------------------- Save values
!
                        v_sdappa_gapi(patch_indx)  = v_sdappa_gapi(patch_indx)-gap_moy
                        patch_weight_c(patch_indx) = patch_weight_c(patch_indx)+inte_weight
                    end if
                end do
            end do
!
! --------- Add element paired
!
            if (total_weight .gt. pair_tole) then
                nb_pair                        = nb_pair+1
                list_pair(nb_pair)             = elem_mast_nume
                elem_mast_flag(elem_mast_indx) = 1
            end if
!
! --------- Find neighbour of current master element
!
            if (total_weight .gt. pair_tole .or. l_recup) then
!
! ------------- Number of neighbours
!
                if (elem_mast_code .eq. 'SE2' .or. elem_mast_code .eq. 'SE3') then
                    nb_mast_neigh = 2
                    tole_weight   = 0.5
                elseif (elem_mast_code .eq. 'TR3' .or. elem_mast_code .eq. 'TR6') then
                    nb_mast_neigh = 3
                    tole_weight   = 0.05
                elseif (elem_mast_code .eq. 'QU4' .or. elem_mast_code .eq. 'QU8' .or.&
                        elem_mast_code .eq. 'QU9') then
                    nb_mast_neigh = 4
                    tole_weight   = 0.4
                else
                    ASSERT(.false.)
                endif
!
! ------------- Prepare next master element
!
                do i_mast_neigh = 1, nb_mast_neigh
                    elem_mast_neigh = v_sdappa_mane((elem_mast_indx-1)*4+i_mast_neigh)
                    elem_neigh_indx = elem_mast_neigh+1-mast_indx_mini
                    if (elem_mast_neigh .ne. 0 .and.&
                        mast_find_flag(elem_neigh_indx) .eq. 0 ) then
                        list_find_mast(nb_find_mast+1)  = elem_mast_neigh
                        nb_find_mast                    = nb_find_mast + 1
                        mast_find_flag(elem_neigh_indx) = 1
                    endif
                end do
!
! ------------- Prepare next slave element: higher weight
!  
                do i_slav_neigh = 1, nb_slav_neigh
                    elem_slav_neigh = v_sdappa_slne((elem_slav_indx-1)*4+i_slav_neigh) 
                    elem_neigh_indx = elem_slav_neigh+1-slav_indx_mini
                    if ( elem_slav_neigh .ne. 0 .and.&
                         inte_neigh(i_slav_neigh) .eq. 1 &
                        .and.elem_slav_flag(elem_neigh_indx) .ne. 1 &
                        .and. list_slav_weight(i_slav_neigh) .lt. tole_weight) then
                        weight_test=0.d0
                        call testvois(jv_geom       , elem_slav_type,&
                                      elem_mast_coor, elem_mast_code, elem_slav_nume,&
                                      pair_tole     , weight_test,    v_mesh_connex ,&
                                      v_connex_lcum)
                        if (weight_test .gt. list_slav_weight(i_slav_neigh).and.&
                            weight_test .gt. pair_tole) then
                            list_slav_master(i_slav_neigh) = elem_mast_nume
                            list_slav_weight(i_slav_neigh) = weight_test
                        end if
                    end if
                end do
                l_recup = .false.
            end if
        end do
!
! ----- Save pairing informations (contact pair)
!
        if (nb_pair .ne. 0) then
            call apsave_pair(i_zone      , elem_slav_nume,&
                             nb_pair     , list_pair     ,&
                             nb_pair_zmpi(i_proc+1), list_pair_zmpi)
        end if
!
! ----- Next elements
!
        if (debug) then 
            write(*,*)'Next elements - Nb: ',nb_slav_neigh
        endif
        do i_slav_neigh = 1, nb_slav_neigh
            elem_slav_neigh = v_sdappa_slne((elem_slav_indx-1)*4+i_slav_neigh)
            elem_neigh_indx = elem_slav_neigh+1-slav_indx_mini
            if (debug) then 
                write(*,*)'Next elements - Current: ',i_slav_neigh,elem_slav_neigh,&
                   list_slav_master(i_slav_neigh), elem_slav_flag(elem_neigh_indx)
            end if  
            if (elem_slav_neigh .ne. 0  .and.&
                list_slav_master(i_slav_neigh).ne. 0 .and.&
                elem_slav_flag(elem_neigh_indx) .ne. 1 ) then
                elem_slav_start(nb_slav_start+1) = elem_slav_neigh
                nb_slav_start                    = nb_slav_start+1
                elem_slav_flag(elem_neigh_indx)  = 1
                elem_mast_start(nb_mast_start+1) = list_slav_master(i_slav_neigh)
                nb_mast_start                    = nb_mast_start+1
            endif
        end do
        mast_find_flag(1:mast_indx_maxi+1-mast_indx_mini) = 0
    end do
    if (debug) then 
        write(*,*)'Fin boucle appariement PANG'
        write(*,*)'maille contact trouvee',nb_pair_zmpi
    end if
    goto 120
110 continue
    if (debug) then 
        write(*,*)'Fin appariement PANG RAPIDE'
    end if 
!
! - Save values for patch
!
    call apsave_patch(mesh          , sdappa        , i_zone, pair_tole,&
                      patch_weight_c, patch_weight_t, nb_proc, list_pair_zmpi,&
                      nb_pair_zmpi, list_pair_zone, nb_pair_zone, i_proc)
    !write(*,*)"Fin APSAVE_PATCH", i_proc
    !write(*,*)"NB_pair_zone: ",nb_pair_zone ,i_proc
    !write(*,*)"list_pair_zone: ",list_pair_zone(:) ,i_proc
!
    AS_DEALLOCATE(vi=mast_find_flag)
    AS_DEALLOCATE(vi=elem_slav_flag)
    AS_DEALLOCATE(vi=elem_mast_flag)
    AS_DEALLOCATE(vi=list_pair_zmpi)
    call jedetr(njv_weight_c)
    call jedetr(njv_weight_t)
    call jedetr(njv_nb_pair_zmpi)
    call jedema() 
end subroutine
