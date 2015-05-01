function lonele(dime, igeom)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!                           CALCULE LA LONGEUR D'UN ELEMENT
!
!   Message <F> en cas de longueur <= r8prem
!
!   IN
!       dime    : dimension de l'espace
!
!   OUT
!       igeom   : adresse-1 du zr sur la géométrie  x1,y1 : < zr(igeom+1), zr(igeom+2) >
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/jevech.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
!
    integer,optional,intent(in)  :: dime
    integer,optional,intent(out) :: igeom
    real(kind=8) :: lonele
!
! --------------------------------------------------------------------------------------------------
!
    integer             :: iadzi, iazk24, igeomloc, idimloc
    real(kind=8)        :: r8bid,xl
    character(len=8)    :: nomail
!
! --------------------------------------------------------------------------------------------------
!
    if ( present(dime) ) then
        idimloc = dime
    else
        idimloc = 3
    endif
!
    call jevech('PGEOMER', 'L', igeomloc)
    igeomloc = igeomloc - 1
!
    if (idimloc .eq. 3) then
        r8bid = ( zr(igeomloc+4) - zr(igeomloc+1) )**2
        r8bid = ( zr(igeomloc+5) - zr(igeomloc+2) )**2 + r8bid
        r8bid = ( zr(igeomloc+6) - zr(igeomloc+3) )**2 + r8bid
    else if (idimloc.eq.2) then
        r8bid = ( zr(igeomloc+3) - zr(igeomloc+1) )**2
        r8bid = ( zr(igeomloc+4) - zr(igeomloc+2) )**2 + r8bid
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
!
    lonele = xl
    if ( present(igeom) ) then
        igeom = igeomloc
    endif
end function
