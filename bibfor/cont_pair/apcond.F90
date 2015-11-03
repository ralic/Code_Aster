subroutine apcond(newgeo, node_nume, node_coor)
!
implicit none
!
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19), intent(in) :: newgeo
    integer, intent(in) :: node_nume
    real(kind=8), intent(out) :: node_coor(3)
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Pairing
!
! Get coordinates of current node
!
! --------------------------------------------------------------------------------------------------
!
! In  newgeo           : name of field for geometry update from initial coordinates of nodes
! In  node_nume        : index of node in mesh datastructure
! Out node_coor        : coordinates of node 
!
! --------------------------------------------------------------------------------------------------
!
    real(kind=8), pointer :: v_newgeo_vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jeveuo(newgeo(1:19)//'.VALE', 'L', vr=v_newgeo_vale)
    node_coor(1) = v_newgeo_vale(3*(node_nume -1)+1)
    node_coor(2) = v_newgeo_vale(3*(node_nume -1)+2)
    node_coor(3) = v_newgeo_vale(3*(node_nume -1)+3)
!
end subroutine
