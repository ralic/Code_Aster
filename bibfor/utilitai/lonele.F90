subroutine lonele(dime, igeom, xl)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: dime, igeom
    real(kind=8) :: xl
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
!                           CALCULE LA LONGEUR D'UN ELEMENT
!
!   Message <F> en cas le longueur <= 0
!
!   IN
!       dime    : dimension de l'espace
!
!   OUT
!       igeom   : adresse du zr
!       xl      : longueur de l'élément
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iadzi, iazk24
    real(kind=8) :: r8bid
    character(len=8) :: nomail
!
! --------------------------------------------------------------------------------------------------
!
    call jevech('PGEOMER', 'L', igeom)
    igeom = igeom - 1
!
    if (dime .eq. 3) then
        r8bid = ( zr(igeom+4) - zr(igeom+1) )**2
        r8bid = ( zr(igeom+5) - zr(igeom+2) )**2 + r8bid
        r8bid = ( zr(igeom+6) - zr(igeom+3) )**2 + r8bid
    else if (dime.eq.2) then
        r8bid = ( zr(igeom+3) - zr(igeom+1) )**2
        r8bid = ( zr(igeom+4) - zr(igeom+2) )**2 + r8bid
    else
!       no warning: ‘r8bid’ may be used uninitialized in this function
        r8bid = 0.0d0
        ASSERT( ASTER_FALSE )
    endif
    xl = sqrt( r8bid )
    if (xl .le. r8prem()) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call utmess('F', 'ELEMENTS2_43', sk=nomail)
    endif
end subroutine
