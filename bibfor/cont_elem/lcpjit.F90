subroutine lcpjit(proj_tole       , elem_dime, &
                  elin_mast_nbnode, elin_mast_coor, elin_mast_code,&
                  elin_slav_nbnode, elin_slav_coor, elin_slav_code,&
                  poin_inte       , inte_weight   , nb_poin_inte)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/lcodrm.h"
#include "asterfort/insema.h"
#include "asterfort/ptinma.h"
#include "asterfort/mmnewt.h"
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
    real(kind=8), intent(in) :: proj_tole
    integer, intent(in) :: elem_dime
    integer, intent(in) :: elin_mast_nbnode
    real(kind=8), intent(in) :: elin_mast_coor(elem_dime,elin_mast_nbnode)
    character(len=8), intent(in) :: elin_mast_code
    integer, intent(in) :: elin_slav_nbnode
    real(kind=8), intent(in) :: elin_slav_coor(elem_dime,elin_slav_nbnode)
    character(len=8), intent(in) :: elin_slav_code
    real(kind=8), intent(out):: poin_inte(elem_dime-1,16)
    real(kind=8), intent(out) :: inte_weight
    integer, intent(out) :: nb_poin_inte
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Projection/intersection of elements in slave parametric space
!
! --------------------------------------------------------------------------------------------------
!
! In  proj_tole        : tolerance for projection
! In  elem_dime        : dimension of elements
! In  elin_mast_nbnode : number of nodes for each sub-element in master element
! In  elin_mast_coor   : coordinates of sub-elements in master element
! In  elin_mast_code   : code of all sub-elements in master element
! In  elin_slav_nbnode : number of nodes for each sub-element in slave element
! In  elin_slav_coor   : coordinates of sub-elements in slave element
! In  elin_slav_code   : code of all sub-elements in slave element
! Out poin_inte        : list (sorted) of intersection points
! Out inte_weight      : total weight of intersection
! Out nb_poin_inte     : number of intersection points
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: poin_coop(elem_dime-1,4), node_coop(elem_dime-1,4)
    real(kind=8) :: ksi1,ksi2, tau1(3), tau2(3), node_coor(3), xpt, ypt
    real(kind=8) :: xp1, yp1, xp2, yp2, elem_coor(3,9)
    integer :: i_inte_poin, i_node, i_dime
    integer :: niverr, test, list_node_next(16), nb_node_coop
    aster_logical :: l_reli
!
! --------------------------------------------------------------------------------------------------
!
    nb_poin_inte       = 0
    inte_weight        = 0.d0
    test               = 0
    niverr             = 0
    elem_coor(1:3,1:9) = 0.d0
    node_coop(1:elem_dime-1,1:4) = 0.d0
    poin_inte(:,:)     = 0.d0
    l_reli = .false.
    do i_node = 1,elin_slav_nbnode
        do i_dime = 1,elem_dime
            elem_coor(i_dime,i_node) = elin_slav_coor(i_dime,i_node)
        enddo
    enddo
!
! - Project master nodes in slave element parametric space
!
    do i_node = 1, elin_mast_nbnode
!
! ----- Get coordinates of current master node
!
        node_coor(1:3) = 0.d0
        do i_dime = 1, elem_dime
            node_coor(i_dime) = elin_mast_coor(i_dime,i_node)
        end do
!
! ----- Projection on slave element
!
        call mmnewt(elin_slav_code, elin_slav_nbnode, elem_dime,&
                    elem_coor     , node_coor       , 75,&
                    proj_tole     , ksi1            , ksi2,&
                    tau1          , tau2            ,&
                    niverr,l_reli)
        if (niverr .eq. 0) then
            poin_coop(1,i_node) = ksi1
            if (elem_dime .eq. 3) then
                poin_coop(2,i_node) = ksi2
            end if
        else
            l_reli=.true.
            call mmnewt(elin_slav_code, elin_slav_nbnode, elem_dime,&
                    elem_coor     , node_coor       , 75,&
                    proj_tole     , ksi1            , ksi2,&
                    tau1          , tau2            ,&
                    niverr, l_reli)
            if (niverr .eq. 0) then
                poin_coop(1,i_node) = ksi1
                if (elem_dime .eq. 3) then
                    poin_coop(2,i_node) = ksi2
                end if
            else
                write(*,*) "mmnewt failed"
                ASSERT(.false.)
            endif
        endif
    end do
!
! - Get parametric coordinates of slave nodes (depending on element type)
!
    if (elin_slav_code .eq. 'SE2' .or. elin_slav_code .eq. 'SE3') then
        node_coop(1,1) = -1.d0
        node_coop(1,2) = 1.d0
        nb_node_coop   = 2
    elseif (elin_slav_code .eq. 'TR3' .or. elin_slav_code .eq. 'TR6') then
        node_coop(1,1) = 0.d0
        node_coop(2,1) = 0.d0
        node_coop(1,2) = 1.d0
        node_coop(2,2) = 0.d0
        node_coop(1,3) = 0.d0
        node_coop(2,3) = 1.d0
        nb_node_coop   = 3
    elseif (elin_slav_code .eq. 'QU4' .or. elin_slav_code .eq. 'QU8' .or.&
            elin_slav_code .eq. 'QU9') then 
        node_coop(1,1) = -1.d0
        node_coop(2,1) = -1.d0
        node_coop(1,2) = 1.d0
        node_coop(2,2) = -1.d0
        node_coop(1,3) = 1.d0
        node_coop(2,3) = 1.d0
        node_coop(1,4) = -1.d0
        node_coop(2,4) = 1.d0
        nb_node_coop   = 4
    else
        ASSERT(.false.)
    end if
!
! - Master nodes belong to intersection
!
    if (nb_node_coop .eq. 2) then
        do i_node = 1, elin_mast_nbnode
            xpt = poin_coop(1,i_node)
            if (xpt .ge. (-1.d0-proj_tole) .and. xpt .le. (1.d0+proj_tole)) then       
                nb_poin_inte              = nb_poin_inte+1
                poin_inte(1,nb_poin_inte) = xpt
            endif
        end do 
    elseif (nb_node_coop .eq. 3) then
        do i_node = 1, elin_mast_nbnode
            xpt = poin_coop(1,i_node)
            ypt = poin_coop(2,i_node)
            if ((xpt.ge.-proj_tole) .and. (ypt.ge.-proj_tole) .and.&
                ((ypt+xpt).le.(1.d0+proj_tole))) then       
                nb_poin_inte              = nb_poin_inte+1
                poin_inte(1,nb_poin_inte) = xpt
                poin_inte(2,nb_poin_inte) = ypt
            endif
        end do
    elseif (nb_node_coop .eq. 4) then
        do i_node = 1, elin_mast_nbnode
            xpt = poin_coop(1,i_node)
            ypt = poin_coop(2,i_node)
            if ((xpt.ge. -1.d0-proj_tole) .and. (ypt.ge. -1.d0-proj_tole) .and. &
                (ypt.le.(1.d0+proj_tole)) .and. (xpt.le.(1.d0+proj_tole))) then       
                nb_poin_inte              = nb_poin_inte+1
                poin_inte(1,nb_poin_inte) = xpt
                poin_inte(2,nb_poin_inte) = ypt
            endif
        end do
    end if
!
! - Add slave nodes if they are inside master element
!
    do i_node = 1, nb_node_coop
        xpt = node_coop(1,i_node)
        if (elem_dime .eq. 3) then
            ypt = node_coop(2,i_node)
        elseif (elem_dime .eq. 2) then
            ypt = 0.d0
        endif
        call ptinma(elin_mast_nbnode, elem_dime, elin_mast_code, poin_coop, proj_tole,&
                    xpt             , ypt      , test)
        if (test .eq. 1) then        
            nb_poin_inte              = nb_poin_inte+1
            poin_inte(1,nb_poin_inte) = xpt
            if (elem_dime .eq. 3) then
                poin_inte(2,nb_poin_inte) = ypt
            end if
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
!
! ----- Set index of next nodes
!
        do i_node = 2, elin_mast_nbnode
            list_node_next(i_node-1) = i_node
        end do
        list_node_next(elin_mast_nbnode) = 1
!
! ----- Loop on master segments
!   
        do i_node = 1, elin_mast_nbnode
            xp1 = poin_coop(1,i_node)
            yp1 = poin_coop(2,i_node)
            xp2 = poin_coop(1,list_node_next(i_node))
            yp2 = poin_coop(2,list_node_next(i_node))
!
! --------- Compute intersection between segment and element
!
            call insema(nb_node_coop, elem_dime, node_coop, proj_tole,&
                        xp1         , yp1      , xp2      , yp2      ,&
                        nb_poin_inte, poin_inte)
        end do
        ASSERT(nb_poin_inte .le. 16)
    end if
!
! - Sort list of intersection points
!
    if ((nb_poin_inte .gt. 2 .and. elem_dime .eq. 3) .or.&
        (nb_poin_inte .ge. 2 .and. elem_dime .eq. 2)) then
        call lcodrm(elem_dime, proj_tole, nb_poin_inte, poin_inte)
    endif
!
! - Compute weight of intersection
!
    if ((nb_poin_inte .gt. 2 .and. elem_dime .eq. 3) .or.&
        (nb_poin_inte .ge. 2 .and. elem_dime .eq. 2) ) then   
        if (elem_dime .eq. 3) then
            do i_inte_poin = 2, nb_poin_inte
                list_node_next(i_inte_poin-1) = i_inte_poin
            end do
            list_node_next(nb_poin_inte)=1
            do i_inte_poin = 1,nb_poin_inte
                inte_weight = inte_weight+&
                        poin_inte(1,i_inte_poin)*&
                        poin_inte(2,list_node_next(i_inte_poin))-&
                        poin_inte(1,list_node_next(i_inte_poin))*&
                        poin_inte(2,i_inte_poin)
            end do
            inte_weight = 1.d0/2.d0*inte_weight
            inte_weight = sqrt(inte_weight**2)
        else
            inte_weight = sqrt((poin_inte(1,2)-poin_inte(1,1))**2)
        end if
    endif 
!
end subroutine
