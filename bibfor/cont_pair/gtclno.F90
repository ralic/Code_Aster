subroutine gtclno(jv_geom, list_node, nb_node, testnode ,nume_node_cl)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "jeveux.h"
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
    integer, pointer, intent(in) :: list_node(:)
    integer, intent(in) :: nb_node
    real(kind=8), intent(in) :: testnode(3)
    integer, intent(out) :: nume_node_cl
    
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing segment to segment
!
! Get center of a given contact element 
!
! --------------------------------------------------------------------------------------------------
!
! In jv_geom          : JEVEUX adress to updated geometry
! In list_node        : list of master nodes on current zone
! In nb_node          : number of node in list_node
! In testnode         : node coordonate
! Out nume_node_cl    : closest node
!
! --------------------------------------------------------------------------------------------------
!
  integer      :: i_dime, i_node, node_nume
  real(kind=8) :: vect_pm(3), dist_min, dist
  
!
! --------------------------------------------------------------------------------------------------
!
!
! - Initialisation
!
    dist_min = 0.d0
    
    do i_node=1, nb_node
        node_nume = list_node(i_node)
!
! ----- Vector Point-Projection
!
        do i_dime = 1, 3
            vect_pm(i_dime) = zr(jv_geom+3*(node_nume-1)+i_dime-1) - testnode(i_dime)
        end do
!
! ----- Distance
!
        dist = sqrt(vect_pm(1)**2+vect_pm(2)**2+vect_pm(3)**2)
!
! ----- Check distance
!     
        if (dist.lt. dist_min .or. i_node .eq. 1)then
            dist_min     = dist
            nume_node_cl = node_nume
        end if       
    end do
!
! - Print check
!
    !write(*,*)"CLOSEST NODE: ",nume_node_cl
!
end subroutine       
