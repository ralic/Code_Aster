subroutine mmmron(ndim, norm, tau1, tau2)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/provec.h"
    integer :: ndim
    real(kind=8) :: tau1(3)
    real(kind=8) :: tau2(3)
    real(kind=8) :: norm(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCULE LES TANGENTES EXTERIEURES A PARTIR DE LA NORMALE INTERIEURE
!
! ----------------------------------------------------------------------
!
!
! CETTE ROUTINE CALCULE LES VECTEURS TANGENTS EXTERIEURS A PARTIR
! DE LA NORMALE INTERIEURE A LA MAILLE
!
! CES VECTEURS TANGENTS DONNNENT:
!  - LA NORMALE EXTERIEURE PAR CALL PROVEC(TAU1,TAU2,NORM)
!  - LA NORMALE INTERIEURE PAR CALL PROVEC(TAU2,TAU1,NORM) - MMNORM
!
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  NORM   : NORMALE INTERIEURE
! OUT TAU1   : PREMIERE TANGENTE EXTERIEURE
! OUT TAU2   : SECONDE TANGENTE EXTERIEURE
!
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: cmpx, cmpy, cmpz
!
! ----------------------------------------------------------------------
!
    cmpx = norm(1)
    cmpy = norm(2)
    cmpz = norm(3)
    if (ndim .eq. 2) then
        tau1(1) = cmpy
        tau1(2) = -cmpx
        tau1(3) = 0.d0
        tau2(1) = 0.d0
        tau2(2) = 0.d0
        tau2(3) = 0.d0
    else if (ndim.eq. 3) then
        if (abs(cmpx) .gt. r8prem()) then
            tau1(1) = -cmpy/cmpx
            tau1(2) = 1.d0
            tau1(3) = 0.d0
        else if (abs(cmpy) .gt. r8prem()) then
            tau1(1) = 1.d0
            tau1(2) = -cmpx/cmpy
            tau1(3) = 0.d0
        else if (abs(cmpz) .gt. r8prem()) then
            tau1(1) = 0.d0
            tau1(2) = 1.d0
            tau1(3) = -cmpy/cmpz
        endif
        call provec(tau1, norm, tau2)
    else
        ASSERT(.false.)
    endif
!
end subroutine
