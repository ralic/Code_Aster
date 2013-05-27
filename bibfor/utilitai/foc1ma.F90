subroutine foc1ma(nbvar, var, fon, nbmax, varmax,&
                  fonmax)
    implicit none
    integer :: nbvar, nbmax
    real(kind=8) :: var(*), fon(*), varmax(*), fonmax(*)
!     ----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ======================================================================
!     "MEDISIS"   CALCUL DES MAXIMA D'UNE FONCTION
!     ----------------------------------------------------------------
    real(kind=8) :: lemax, epsd, eps
!     ----------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    epsd = 1.d-6
    lemax = abs(fon(1))
    eps = epsd * lemax
    nbmax = 1
    varmax(1) = var(1)
    fonmax(1) = fon(1)
    do 100 i = 2, nbvar
        if (abs(fon(i)) .ge. lemax-eps) then
            if (abs(fon(i)) .gt. lemax+eps) then
                nbmax = 1
                lemax = abs(fon(i))
                eps = epsd * lemax
                varmax(nbmax) = var(i)
                fonmax(nbmax) = fon(i)
            else
                nbmax = nbmax + 1
                varmax(nbmax) = var(i)
                fonmax(nbmax) = fon(i)
            endif
        endif
100  end do
end subroutine
