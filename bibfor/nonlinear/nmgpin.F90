subroutine nmgpin(ndim, nno, axi, vu)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
!
    aster_logical :: axi
    integer :: ndim, nno, vu(3, 27)
! ----------------------------------------------------------------------
!        INITIALISATION POUR LES ELEMENTS EN GRANDES DEFORMATIONS
! ----------------------------------------------------------------------
! IN  NDIM  DIMENSION DE L'ESPACE
! IN  NNO   NOMBRE DE NOEUDS
! IN  AXI   INDICATEUR DE MODELISATION AXISYMETRIQUE
! OUT VU    RENVOIE L'INDICE DU DDL CORRESPONDANT A (I,N)
! ----------------------------------------------------------------------
    integer :: n, i
! ----------------------------------------------------------------------
!
    do 10 n = 1, nno
        do 20 i = 1, ndim
            vu(i,n) = i + ndim*(n-1)
 20     continue
 10 end do
!
    if (axi) then
        do 30 n = 1, nno
            vu(3,n) = vu(1,n)
 30     continue
    endif
!
end subroutine
