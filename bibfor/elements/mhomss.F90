subroutine mhomss(young, nu, cmatlo, dsidep)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! Modified HOOKE Matrix for Solid-Shell elements
!
!
! Evaluation of modified HOOKE Matrix for Solid-Shell elements in case of
! linear or non linear material
!
!
! IN  young    young modulus
! IN  nu       Poisson's coefficient
! IN  dsideps  tangent operator d sigma / d epsilon
! OUT cmatlo   modified HOOKE matrix
!
#include "asterfort/r8inir.h"
!
    real(kind=8), intent(in) :: young
    real(kind=8), intent(in) :: nu
    real(kind=8), intent(out) :: cmatlo(6,6)
    real(kind=8), optional, intent(in) :: dsidep(6,6)
!
    real(kind=8) :: lambda, mu
!
! ......................................................................
!
    call r8inir(36, 0.d0, cmatlo, 1)
    mu    = 0.5d0*young/(1.d0+nu)
!
!   Common cmatlo componentss
    cmatlo(3,3) = young
    cmatlo(5,5) = mu
    cmatlo(6,6) = mu
!
!   Specific cmatlo components
    if (present(dsidep)) then
!
!      Non linear tangent modified Hook matrix
       cmatlo(1,1) = dsidep(1,1)
       cmatlo(2,1) = dsidep(2,1)
       cmatlo(4,1) = dsidep(4,1) * 0.5d0
       cmatlo(1,2) = dsidep(1,2)
       cmatlo(2,2) = dsidep(2,2)
       cmatlo(4,2) = dsidep(4,2) * 0.5d0
       cmatlo(1,4) = dsidep(1,4) * 0.5d0
       cmatlo(2,4) = dsidep(2,4) * 0.5d0
       cmatlo(4,4) = dsidep(4,4) * 0.5d0
!
    else
!
!      Linear modified Hook matrix
       lambda = young*nu/(1.d0-nu*nu)
!
       cmatlo(1,1) = lambda + 2.d0*mu
       cmatlo(2,2) = cmatlo(1,1)
       cmatlo(1,2) = lambda
       cmatlo(2,1) = lambda
       cmatlo(4,4) = mu
!
    endif
!
end subroutine
