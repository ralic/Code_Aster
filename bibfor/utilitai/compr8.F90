function compr8(a, comp, b, eps, crit)
    implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
    aster_logical :: compr8
    real(kind=8) :: a, b, eps
    integer :: crit
    character(len=2) :: comp
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
! ----------------------------------------------------------------------
!
! FONCTION COMPARANT 2 REELS (REAL*8) ENTRE EUX
!     =.TRUE. SI    A.COMP.B A LA PRECISION PREC DONNEE
!
!     A = B EN ABSOLU   <=> |A-B| <= EPS
!     A = B EN RELATIF  <=> |A-B| <= EPS.MIN(|A|,|B|)
!
!     A <= B EN ABSOLU  <=> A <= B + EPS
!     A <= B EN RELATIF <=> A <= B + EPS.MIN(|A|,|B|)
!
!     A < B EN ABSOLU   <=> A < B - EPS
!     A < B EN RELATIF  <=> A < B - EPS.MIN(|A|,|B|)
!
!     A >= B EN ABSOLU  <=> A >= B - EPS
!     A >= B EN RELATIF <=> A >= B - EPS.MIN(|A|,|B|)
!
!     A > B EN ABSOLU   <=> A > B + EPS
!     A > B EN RELATIF  <=> A > B + EPS.MIN(|A|,|B|)
!
! ----------------------------------------------------------------------
!
! IN   A      : REEL A GAUCHE DU SIGNE
! IN   B      : REEL A DROITE DU SIGNE
! IN   EPS    : PRECISION
! IN   CRIT   : CRITERE (=0 si ABSOLU ou 1 si RELATIF)
! IN   COMP   : TYPE DE COMPARAISON ENTRE REELS : =, <, >, >=, <=
! OUT  COMPR8 : TRUE SI LA RELATION EST VERIFIEE
!
    real(kind=8) :: minab, min, tole
!
    compr8=.false.
    minab = min(abs(a),abs(b))
!
!     --------------------
!     TESTS PRELIMINAIRES
!     --------------------
!
!     TEST DE LA PRECISION (POSITIVE OU NULLE)
    ASSERT(eps.ge.0.d0)
!
    ASSERT(crit.eq.0.or.crit.eq.1)
!
!     --------------------
!     COMPARAISONS
!     --------------------
!
    if (crit .eq. 0) tole=eps
    if (crit .eq. 1) tole=eps*minab
!
    if (comp .eq. 'EQ') then
!
        if (abs(a-b) .le. tole) compr8=.true.
!
    else if (comp.eq.'LE') then
!
        if (a .le. b+tole) compr8=.true.
!
    else if (comp.eq.'LT') then
!
        if (a .lt. b-tole) compr8=.true.
!
    else if (comp.eq.'GE') then
!
        if (a .ge. b-tole) compr8=.true.
!
    else if (comp.eq.'GT') then
!
        if (a .gt. b+tole) compr8=.true.
!
    else
!
        ASSERT(.false.)
!
    endif
!
!
end function
