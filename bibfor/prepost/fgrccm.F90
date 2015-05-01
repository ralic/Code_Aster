subroutine fgrccm(nbextr, ext, ncyc, sigmin, sigmax)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/utmess.h"
    real(kind=8) :: ext(*), sigmin(*), sigmax(*)
    integer :: nbextr, ncyc
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     -----------------------------------------------------------------
!     DETERMINATION DES CYCLES PAR LA METHODE DE COMPTAGE RCCM
!     ------------------------------------------------------------------
! IN  NBEXTR : I   : NOMBRE D'EXTREMA
! IN  EXT    : R   : VALEURS DES EXTREMA
! OUT NCYC   : I   : NOMBRE DE CYCLES
! OUT SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
! OUT SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
!     ------------------------------------------------------------------
!     -----------------------------------------------------------------
!
    real(kind=8) :: moyext, a
    aster_logical :: cyczer
!
! ------------------------------------------------------------
!
! --- CALCUL DE LA VALEUR MOYENNE DES CONTRAINTES ---
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    moyext = 0.d0
!
    cyczer = .true.
!
    do 21 i = 2, nbextr
        if ((ext(i) .gt. ext(1)) .or. (ext(i) .lt. ext(1))) then
            cyczer = .false.
        endif
 21 end do
!
    if (cyczer) then
        sigmax(1) = ext(1)
        sigmin(1) = ext(1)
        ncyc = 1
!
        call utmess('A', 'FATIGUE1_39')
!
        goto 999
    endif
!
    do 1 i = 1, nbextr
        moyext = moyext + ext(i)
  1 end do
    moyext = moyext/nbextr
!
! --- DETECTION DES CYCLES
!
    a = dble(nbextr/2)
    ncyc = int(a)
    do 2 i = 1, ncyc
        sigmax(i) = ext(nbextr-i+1)
        sigmin(i) = ext(i)
  2 end do
    if (nbextr .ne. (2*ncyc)) then
        ncyc = ncyc + 1
        if (ext(ncyc) .ge. moyext) then
            sigmax(ncyc) = ext(ncyc)
            sigmin(ncyc) = -ext(ncyc) + 2 * moyext
        else
            sigmax(ncyc) = -ext(ncyc) + 2 * moyext
            sigmin(ncyc) = ext(ncyc)
        endif
    endif
!
999 continue
!
end subroutine
