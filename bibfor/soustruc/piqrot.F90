subroutine piqrot(x1, y1, tetax, nt, ret,&
                  rit, rep, tetaf, epsi)
    implicit   none
#include "asterc/r8pi.h"
    integer :: nt
    real(kind=8) :: x1, y1, tetax, ret, rit, rep, tetaf, epsi
!-----------------------------------------------------------------------
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
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     CALCULE L'ANGLE DE ROTATION DU PIQUAGE ROND
!
!-----------------------------------------------------------------------
!
    integer :: n
    real(kind=8) :: pi, rmp, rxt, ept, lmax, tetaf0
!     ------------------------------------------------------------------
!
    pi = r8pi()
    rmp = sqrt( x1**2 + y1**2 )
!
    tetax = tetaf
    n = nint( 2.0d0 * nt * tetaf / pi ) + 1
!
    rxt = ( ret + rit ) / 2.0d0
    ept = ret - rit
    lmax = pi * rxt
!
    if ((rep+ept) .le. rmp .and. rmp .le. (lmax+epsi)) then
        tetaf0 = ( n - 1 ) * pi / ( 2 * nt )
        tetax = (tetaf*(lmax-rmp)+tetaf0*(rmp-rep-ept))/(lmax-rep-ept)
    endif
!
end subroutine
