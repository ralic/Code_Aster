subroutine te0111(option, nomte)
!
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
!
    implicit none
    character(len=16) :: nomte, option
!
! --------------------------------------------------------------------------------------------------
!
!    ELEMENT MECABL2
!       OPTION : 'MASS_INER'
!
! --------------------------------------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
!
    integer ::          icodre(1)
    real(kind=8) ::     valres(1)
    character(len=16) :: nomres(1)
!
    integer :: imater, igeom, lsect, lcastr
    real(kind=8) :: r8bid, rho, xl, aire
! --------------------------------------------------------------------------------------------------
!
    call jevech('PMATERC', 'L', imater)
    r8bid = 0.0d0
    nomres(1) = 'RHO'
    call rcvalb('RIGI', 1, 1, '+', zi(imater), ' ', 'ELAS',  0, '  ', [r8bid],&
                1, nomres, valres, icodre, 1)
    rho = valres(1)
    if ( rho.le.r8prem() ) then
        call utmess('F', 'ELEMENTS5_45')
    endif
!
    call jevech('PCACABL', 'L', lsect)
    aire = zr(lsect)
!
!   Longueur de l'élément
    call lonele(3, igeom, xl)
!
    call jevech('PMASSINE', 'E', lcastr)
    zr(lcastr) = rho * aire * xl
    zr(lcastr+1) =( zr(igeom+4) + zr(igeom+1) ) / 2.d0
    zr(lcastr+2) =( zr(igeom+5) + zr(igeom+2) ) / 2.d0
    zr(lcastr+3) =( zr(igeom+6) + zr(igeom+3) ) / 2.d0
!
!   inertie de l'element ---
    zr(lcastr+4) = 0.d0
    zr(lcastr+5) = 0.d0
    zr(lcastr+6) = 0.d0
    zr(lcastr+7) = 0.d0
    zr(lcastr+8) = 0.d0
    zr(lcastr+9) = 0.d0
end subroutine
