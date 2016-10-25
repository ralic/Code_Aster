subroutine prjint(pair_tole     , elem_dime       ,&
                  elin_slav_coor, elin_slav_nbnode, elin_slav_code,&
                  elin_mast_coor, elin_mast_nbnode, elin_mast_code,&
                  poin_inte     , inte_weight     , nb_poin_inte  ,&
                  inte_neigh_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/lcodrm.h"
#include "asterfort/insema.h"
#include "asterfort/ptinma.h"
#include "asterfort/mmnewt.h"
#include "asterfort/mmtang.h"
#include "asterfort/mmnorm.h"
#include "asterfort/mmdonf.h"
#include "asterfort/apdist.h"
#include "asterfort/apnorm.h"
#include "asterfort/apelem_getcenter.h"
#include "asterfort/apelem_getvertex.h"
#include "asterfort/apelem_inside.h"
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
    real(kind=8), intent(in) :: pair_tole
    integer, intent(in) :: elem_dime
    real(kind=8), intent(in) :: elin_slav_coor(3,9)
    integer, intent(in) :: elin_slav_nbnode
    character(len=8), intent(in) :: elin_slav_code
    real(kind=8), intent(in) :: elin_mast_coor(3,9)
    integer, intent(in) :: elin_mast_nbnode
    character(len=8), intent(in) :: elin_mast_code
    real(kind=8), intent(out) :: poin_inte(elem_dime-1,16)
    real(kind=8), intent(out) :: inte_weight
    integer, intent(out) :: nb_poin_inte
    integer, optional, intent(inout) :: inte_neigh_(4)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Projection/intersection of elements in slave parametric space
!
! --------------------------------------------------------------------------------------------------
!
! In  pair_tole        : tolerance for pairing
! In  elem_dime        : dimension of elements
! In  elin_slav_coor   : coordinates of sub-elements in slave element
! In  elin_slav_nbnode : number of nodes for each sub-element in slave element
! In  elin_slav_code   : code of all sub-elements in slave element
! In  elin_mast_coor   : coordinates of sub-elements in master element
! In  elin_mast_nbnode : number of nodes for each sub-element in master element
! In  elin_mast_code   : code of all sub-elements in master element
! Out poin_inte        : list (sorted) of intersection points
! Out inte_weight      : total weight of intersection
! Out nb_poin_inte     : number of intersection points
! IO  inte_neigh       : activation of neighbours of intersection
!
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: debug, l_reli
    real(kind=8) :: slav_para_coor(elem_dime-1,4)
    real(kind=8) :: mast_para_coor(elem_dime-1,4)
    real(kind=8) :: tevapr, dist_sign, sig
    real(kind=8) :: ksi1_cent, ksi2_cent
    real(kind=8) :: ksi1, ksi2, dist, vect_pm(3)
    real(kind=8) :: tau1(3), tau2(3), node_coor(3)
    real(kind=8) :: xp1, yp1, xp2, yp2, xpt, ypt
    real(kind=8) :: elin_mast_norm(3), elin_slav_norm(3)
    integer :: i_node, i_inte_poin, i_dime, niverr, test
    integer :: list_node_next(16), list_poin_next(16), list_node_prev(4)
    integer :: elem_auxi_nbnode, slav_para_nb, inte_neigh(4)
    character(len=8) :: elem_auxi_code, slav_para_code
!
! --------------------------------------------------------------------------------------------------
!
    nb_poin_inte = 0
    inte_neigh(1:4) = 0
    if (present(inte_neigh_)) then
        inte_neigh(:) = inte_neigh_(:)
    endif
    inte_weight     = 0.d0
    poin_inte(1:elem_dime-1,1:16) = 0.d0
    debug   = .false.
    l_reli  = .false.
    if (debug) then
        write(*,*) ". Projection/intersection"
    endif
!
! - Compute master element normal (at center)
!
    ASSERT(elin_mast_code .eq. 'SE2' .or. elin_mast_code .eq. 'TR3')
    call apelem_getcenter(elin_mast_code, ksi1_cent, ksi2_cent)
    call apnorm(elin_mast_nbnode, elin_mast_code, elem_dime     , elin_mast_coor,&
                ksi1_cent       , ksi2_cent     , elin_mast_norm)
    if (debug) then
        write(*,*) ".. Master/Norm: ", elin_mast_norm
    endif
!
! - Linearization of reference element for slave element
!
    if (elin_slav_code .eq. 'TR3' .or.&
        elin_slav_code .eq. 'TR6') then
        elem_auxi_code   = 'TR3'
        elem_auxi_nbnode = 3 
    elseif (elin_slav_code .eq. 'QU4' .or.&
            elin_slav_code .eq. 'QU8' .or. &
            elin_slav_code .eq. 'QU9') then
        elem_auxi_code   = 'QU4'
        elem_auxi_nbnode = 4  
    elseif (elin_slav_code .eq. 'SE2' .or.&
            elin_slav_code .eq. 'SE3') then
        elem_auxi_code   = 'SE2'
        elem_auxi_nbnode = 2
    else
        ASSERT(.false.) 
    end if
!
! - Compute slave element normal (at center)
!
    call apelem_getcenter(elem_auxi_code, ksi1_cent, ksi2_cent)
    call apnorm(elem_auxi_nbnode, elem_auxi_code, elem_dime     , elin_slav_coor,&
                ksi1_cent       , ksi2_cent     , elin_slav_norm)
    if (debug) then
        write(*,*) ".. Slave/Norm: ", elin_slav_norm
    endif 
!
! - Project master nodes in slave element parametric space
!
    if (debug) then
        write(*,*) ".. Project master nodes in slave element parametric space"
    endif 
    do i_node = 1, elin_mast_nbnode
!
! ----- Current coordinates of master node
!
        node_coor(1:3) = 0.d0
        do i_dime = 1, elem_dime
            node_coor(i_dime) = elin_mast_coor(i_dime, i_node)
        end do
!
! ----- Project current master node on slave linearized element
!
        call mmnewt(elin_slav_code, elin_slav_nbnode, elem_dime,&
                    elin_slav_coor, node_coor       , 75      ,&
                    pair_tole     , ksi1            , ksi2     ,&
                    tau1          , tau2            , niverr, l_reli)
        if (niverr .eq. 0) then
            mast_para_coor(1, i_node) = ksi1 
            if (elem_dime .eq. 3) then
                mast_para_coor(2, i_node) = ksi2
            end if
        else
           l_reli=.true.
           call mmnewt(elin_slav_code, elin_slav_nbnode, elem_dime,&
                    elin_slav_coor, node_coor       , 75      ,&
                    pair_tole     , ksi1            , ksi2     ,&
                    tau1          , tau2            , niverr, l_reli)
            if (niverr .eq. 0) then
                mast_para_coor(1, i_node) = ksi1 
                if (elem_dime .eq. 3) then
                    mast_para_coor(2, i_node) = ksi2
                end if
            else
                ASSERT(.false.)
            endif
        endif
!
! ----- Compute distance from point to its orthogonal projection
!
        dist = 0.d0
        call apdist(elin_slav_code, elin_slav_coor, elin_slav_nbnode, ksi1, ksi2,&
                    node_coor     , dist          , vect_pm)
!
! ----- Sign of colinear product VECT_PM . NORMAL(slave)
!
        sig = 0.d0
        if (elem_dime .eq. 3) then
            sig = vect_pm(1)*elin_slav_norm(1)+&
                  vect_pm(2)*elin_slav_norm(2)+&
                  vect_pm(3)*elin_slav_norm(3)
        elseif (elem_dime .eq. 2) then
            sig = vect_pm(1)*elin_slav_norm(1)+&
                  vect_pm(2)*elin_slav_norm(2)
        else
            ASSERT(.false.)
        end if       
        dist_sign = -sign(dist,sig)
!
! ----- Sign of colinear product VECT_PM . NORMAL(master)
!
        if (elem_dime .eq. 3) then
            tevapr = vect_pm(1)*elin_mast_norm(1)+&
                     vect_pm(2)*elin_mast_norm(2)+&
                     vect_pm(3)*elin_mast_norm(3)
        elseif (elem_dime .eq. 2) then
            tevapr = vect_pm(1)*elin_mast_norm(1)+&
                     vect_pm(2)*elin_mast_norm(2)
        else
            ASSERT(.false.)
        end if
        if (debug) then
            write(*,*) "... Node: ",i_node,' - Coord: ', node_coor
            write(*,*) " => Distance: ",dist,' - Distance signée: ', dist_sign
            write(*,*) " => VECT_PM . NORMAL: ", tevapr
        endif 
!
! ----- No change of sign => no intersection
!
        if (dist_sign .lt. 0.d0-pair_tole) then
            if (tevapr .gt. 0.d0-pair_tole) then
                if (debug) then
                    write(*,*) "... Pas d'intersection: ", dist_sign, tevapr
                endif
                goto 99
            end if
        elseif (dist_sign .gt. 0.d0+pair_tole) then
            if (tevapr .lt. 0.d0+pair_tole) then
                if (debug) then
                    write(*,*) "... Pas d'intersection: ", dist_sign, tevapr
                endif
                goto 99
            end if
        end if     
    end do
!
! - Parametric coordinates of slave nodes after linearization
!
    call apelem_getvertex(elem_dime     , elin_slav_code, .true._1,&
                          slav_para_coor, slav_para_nb  , slav_para_code)
!
! - Set index of previous nodes
!  
    do i_node = 2, slav_para_nb
        list_node_prev(i_node) = i_node-1
    end do
    list_node_prev(1) = slav_para_nb
!
! - Save projection of master nodes on slave element in list of intersection points
!
    call apelem_inside(pair_tole       , elem_dime, slav_para_code,&
                       elin_mast_nbnode, mast_para_coor,&
                       nb_poin_inte    , poin_inte)
!
! - Add slave nodes in list of intersection points
!
    do i_node = 1, slav_para_nb
!
! ----- Current coordinates of slave node
!
        xpt = slav_para_coor(1, i_node)
        ypt = 0.d0
        if (elem_dime .eq. 3) then    
            ypt = slav_para_coor(2, i_node)
        end if
!
! ----- Test if point is inside element
!
        call ptinma(elin_mast_nbnode, elem_dime, elin_mast_code, mast_para_coor, pair_tole,&
                    xpt             , ypt      , test)
        if (test .eq. 1) then    
            nb_poin_inte              = nb_poin_inte+1
            poin_inte(1,nb_poin_inte) = xpt
            if (elem_dime .eq. 3) then
                poin_inte(2,nb_poin_inte) = ypt
            end if
            if (elem_dime .eq. 3) then
                inte_neigh(i_node)                 = 1
                inte_neigh(list_node_prev(i_node)) = 1
            else if (elem_dime .eq. 2) then
                inte_neigh(i_node)                 = 1
            else
                ASSERT(.false.)
            endif
        else if (test .eq. -1) then
            nb_poin_inte                  = 0
            poin_inte(1:elem_dime-1,1:16) = 0.d0
            inte_neigh(1:4)               = 0
            goto 99   
        endif
    end do
!
! - Set index of next nodes
!  
    do i_node = 2, elin_mast_nbnode
        list_node_next(i_node-1) = i_node
    end do
    list_node_next(elin_mast_nbnode) = 1
!
! - Intersection of edges
!
    if (elem_dime .eq. 3) then  
        do i_node = 1, elin_mast_nbnode
!
! --------- Segment from edge of master element
!
            xp1 = mast_para_coor(1,i_node)
            yp1 = mast_para_coor(2,i_node)
            xp2 = mast_para_coor(1,list_node_next(i_node))
            yp2 = mast_para_coor(2,list_node_next(i_node))
!
! --------- Compute intersection between edge of master and slave element
!
            call insema(slav_para_nb, elem_dime, slav_para_coor, pair_tole,&
                        xp1         , yp1      , xp2           , yp2      ,&
                        nb_poin_inte, poin_inte, inte_neigh)
        end do
        ASSERT(nb_poin_inte.le.16)
    end if
!
! - Sort list of intersection points
!
    if ((nb_poin_inte .gt. 2 .and. elem_dime .eq. 3) .or.&
        (nb_poin_inte .ge. 2 .and. elem_dime .eq. 2)) then
        call lcodrm(elem_dime, pair_tole, nb_poin_inte, poin_inte)
    endif
!
! - Compute weight of intersection
!
    if ((nb_poin_inte .gt. 2 .and. elem_dime .eq. 3) .or.&
        (nb_poin_inte .ge. 2 .and. elem_dime .eq. 2)) then
        if ((elem_dime-1) .eq. 2) then
            do i_inte_poin = 2, nb_poin_inte
                list_poin_next(i_inte_poin-1) = i_inte_poin
            end do
            list_poin_next(nb_poin_inte)=1
            do i_inte_poin = 1,nb_poin_inte
                inte_weight = inte_weight + &
                        poin_inte(1,i_inte_poin)*&
                        poin_inte(2,list_poin_next(i_inte_poin))-&
                        poin_inte(1,list_poin_next(i_inte_poin))*&
                        poin_inte(2,i_inte_poin)
            end do
            inte_weight = 1.d0/2.d0*inte_weight
            inte_weight = sqrt(inte_weight**2)
        else
            inte_weight = sqrt((poin_inte(1,2)-poin_inte(1,1))**2)
        end if
    endif
99  continue
!
! - Copy
!
    if (present(inte_neigh_)) then
        inte_neigh_(1:4) = inte_neigh(1:4)
    endif
!
end subroutine
