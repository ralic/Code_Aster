subroutine drz03d_tria(dist_mini, nb_node, list_node, coor_node, numnoe_a, &
                       numnoe_b, numnoe_c, ab, ac, l_trian)
!
    implicit none
!
#include "asterfort/provec.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! =====================================================================
!
    real(kind=8), intent(in) :: dist_mini
    integer, intent(in) :: nb_node
    integer, intent(in) :: list_node(*)
    real(kind=8), intent(in) :: coor_node(*)
    integer, intent(in) :: numnoe_a
    integer, intent(out) :: numnoe_b
    integer, intent(out) :: numnoe_c
    real(kind=8), intent(out) :: ab(3)
    real(kind=8), intent(out) :: ac(3)
    logical(kind=1), intent(out) :: l_trian
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA / LIAISON_SOLIDE
!
! Apply transformation - 3D without any rotation DRX, DRY and DRZ dof - Choose triangle
!
! --------------------------------------------------------------------------------------------------
!
! In  dist_mini     : minimum distance to detect nodes in same place
! In  nb_node       : number of nodes applying transformation
! In  list_node     : list of nodes applying transformation
! In  coor_node     : geometric coordinates of nodes applying transformation
! In  numnoe_a      : number (in mesh) of node A
! Out numnoe_b      : number (in mesh) of node B
! Out numnoe_c      : number (in mesh) of node C
! Out ab            : AB segment
! Out ac            : AC segment
! Out l_trian       : .true. if (non-zero) triangle found
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i, i_no1, i_no2
    real(kind=8) :: lab, lac
    real(kind=8) :: norm_abc(3), sabc
!
! --------------------------------------------------------------------------------------------------
!
    l_trian = .false.
    numnoe_b = 0
    numnoe_c = 0
    do i = 1, 3
        ab(i) = 0.d0
        ac(i) = 0.d0
        norm_abc(i) = 0.d0
    enddo
    sabc = 0.d0
!
! - Find a triangle (three nodes) with non-zero surface
!
    do i_no1 = 2, nb_node
!
! ----- Find a non-zero edge
!
        numnoe_b = list_node(i_no1)
        ab(1) = coor_node(3*(numnoe_b-1)+1) - coor_node(3*(numnoe_a-1)+1)
        ab(2) = coor_node(3*(numnoe_b-1)+2) - coor_node(3*(numnoe_a-1)+2)
        ab(3) = coor_node(3*(numnoe_b-1)+3) - coor_node(3*(numnoe_a-1)+3)
        lab = sqrt(ab(1)*ab(1)+ab(2)*ab(2)+ab(3)*ab(3))
!
        if (lab .gt. dist_mini) then
!
! --------- Find the third vertex
!
            do i_no2 = i_no1 + 1, nb_node
                numnoe_c = list_node(i_no2)
                ac(1) = coor_node(3*(numnoe_c-1)+1) - coor_node(3*(numnoe_a-1)+1)
                ac(2) = coor_node(3*(numnoe_c-1)+2) - coor_node(3*(numnoe_a-1)+2)
                ac(3) = coor_node(3*(numnoe_c-1)+3) - coor_node(3*(numnoe_a-1)+3)
                lac = sqrt(ac(1)*ac(1)+ac(2)*ac(2)+ac(3)*ac(3))
                if (lac .gt. dist_mini) then
                    call provec(ab, ac, norm_abc)
                    sabc = sqrt(norm_abc(1)*norm_abc(1)+&
                                norm_abc(2)*norm_abc(2)+&
                                norm_abc(3)*norm_abc(3))
                    if (sabc/max(lab,lac) .le. dist_mini) then
!
! --------------------- Another edge
!
                        goto 60
                    else
!
! --------------------- Non-zero triangle found !
!
                        l_trian = .true.
                        goto 80
                    endif
                endif
            enddo
        endif
60      continue
    end do
80  continue
!
end subroutine
