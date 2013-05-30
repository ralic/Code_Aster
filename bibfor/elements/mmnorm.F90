subroutine mmnorm(ndim, tau1, tau2, norm, noor)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2006  EDF R&D                  WWW.CODE-ASTER.ORG
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
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    integer :: ndim
    real(kind=8) :: tau1(3)
    real(kind=8) :: tau2(3)
    real(kind=8) :: norm(3)
    real(kind=8) :: noor
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCULE LA NORMALE VERS INTERIEUR A PARTIR DES TANGENTES
!
! ----------------------------------------------------------------------
!
!
! CETTE ROUTINE CALCULE LA NORMALE INTERIEURE A PARTIR DES
! TANGENTES EXTERIEURES
!
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  TAU1   : PREMIERE TANGENTE EXTERIEURE
! IN  TAU2   : SECONDE TANGENTE EXTERIEURE
! OUT NORM   : NORMALE INTERIEURE
! OUT NOOR   : NORME DE LA NORMALE
!
!
! ----------------------------------------------------------------------
!
!
!
! ----------------------------------------------------------------------
!
    noor = r8prem()
    if (ndim .eq. 2) then
        norm(1) = -tau1(2)
        norm(2) = tau1(1)
        norm(3) = 0.d0
    else if (ndim.eq.3) then
        call provec(tau2, tau1, norm)
    else
        call assert(.false.)
    endif
!
    call normev(norm, noor)
!
end subroutine
