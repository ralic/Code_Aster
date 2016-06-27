subroutine apelem_getvertex(elem_dime, elem_code, l_linear ,&
                            para_coor, nb_vertex, para_code)
!
implicit none
!
#include "asterfort/assert.h"
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
    integer, intent(in) :: elem_dime
    character(len=8), intent(in) :: elem_code
    aster_logical, intent(in) :: l_linear
    real(kind=8), intent(out) :: para_coor(elem_dime-1,4)
    integer, intent(out) :: nb_vertex
    character(len=8), intent(out) :: para_code
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get vertices of element in parametric space
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_dime        : dimension of current element
! In  elem_code        : code of current element
! In  l_linear         : .true. if get only linear vertices
! Out para_coor        : parametric coordinates for vertices of current element
! Out nb_vertex        : number of vertex (after linearization)
! Out para_code        : code of parametric element
!
! --------------------------------------------------------------------------------------------------
!
    if (l_linear) then
        if (elem_code .eq. 'SE2' .or.&
            elem_code .eq. 'SE3') then
            para_coor(1,1) = -1.d0
            para_coor(1,2) =  1.d0
            nb_vertex      = 2
            para_code      = 'SE2'
        elseif (elem_code .eq. 'TR3' .or.&
                elem_code .eq. 'TR6') then
            para_coor(1,1) = 0.d0
            para_coor(2,1) = 0.d0
            para_coor(1,2) = 1.d0
            para_coor(2,2) = 0.d0
            para_coor(1,3) = 0.d0
            para_coor(2,3) = 1.d0
            nb_vertex      = 3
            para_code      = 'TR3'
        else if (elem_code .eq. 'QU4' .or.&
                 elem_code .eq. 'QU8' .or.&
                 elem_code .eq. 'QU9') then
            para_coor(1,1) = -1.d0
            para_coor(2,1) = -1.d0
            para_coor(1,2) = +1.d0
            para_coor(2,2) = -1.d0
            para_coor(1,3) = +1.d0
            para_coor(2,3) = +1.d0
            para_coor(1,4) = -1.d0
            para_coor(2,4) = +1.d0
            nb_vertex      = 4
            para_code      = 'QU4'
        else
            ASSERT(.false.)
        end if
    else
        ASSERT(.false.)
    endif
!
end subroutine
