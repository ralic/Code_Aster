subroutine mmtann(ndim, tau1, tau2, iret)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/normev.h"
    integer :: ndim, iret
    real(kind=8) :: tau1(3), tau2(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT)
!
! NORMALISATION DES VECTEURS TANGENTS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE LA MAILLE (2 OU 3)
! OUT TAU1   : PREMIER VECTEUR TANGENT EN XI,YI
! OUT TAU2   : SECOND VECTEUR TANGENT EN XI,YI
! OUT IRET   : VAUT 1 SI TANGENTES NULLES, 0 SINON
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: nta1, nta2
!
! ----------------------------------------------------------------------
!
    iret = 0
!
    call normev(tau1, nta1)
!
    if (ndim .eq. 2) then
        call normev(tau1, nta1)
        nta2 = 1.d0
    else if (ndim.eq.3) then
        call normev(tau1, nta1)
        call normev(tau2, nta2)
    else
        ASSERT(.false.)
    endif
!
! --- VERIFICATION DES TANGENTES
!
    if (abs(nta1) .le. r8prem()) then
        iret = 1
    endif
    if (abs(nta2) .le. r8prem()) then
        iret = 2
    endif
!
end subroutine
