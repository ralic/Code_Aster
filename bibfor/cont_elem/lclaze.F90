subroutine lclaze(elem_dime, nb_lagr, nb_node_slav, indi_lagc,&
                  mmat)
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
    integer, intent(in) :: nb_node_slav
    integer, intent(in) :: nb_lagr
    integer, intent(in) :: indi_lagc(10)
    real(kind=8), intent(inout) :: mmat(55,55)
!
! --------------------------------------------------------------------------------------------------
!
! Contact (LAC) - Elementary computations
!
! Compute contact matrix (no contact)
!
! --------------------------------------------------------------------------------------------------
!
! In  elem_dime        : dimension of elements
! In  nb_node_slav     : number of nodes of for slave side from contact element
! In  nb_lagr          : total number of Lagrangian dof on contact element
! In  indi_lagc        : PREVIOUS node where Lagrangian dof is present (1) or not (0)
! IO  mmat             : matrix
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_node_slav, indlgc ,shift
    real(kind=8) :: r_nb_lagr
!
! --------------------------------------------------------------------------------------------------
!
    shift     = 0
    r_nb_lagr = real(nb_lagr,kind=8)
    do i_node_slav = 1, nb_node_slav
        shift=shift+indi_lagc(i_node_slav)     
        if (indi_lagc(i_node_slav+1) .eq. 1) then
            indlgc=(i_node_slav-1)*elem_dime+shift+elem_dime+1
            mmat(indlgc,indlgc) = 1.d0/(r_nb_lagr)
        end if
    end do
! 
end subroutine
