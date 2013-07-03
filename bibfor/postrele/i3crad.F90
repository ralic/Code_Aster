subroutine i3crad(k, f, a, nba, t,&
                  r1, r2)
    implicit none
!
#include "asterfort/u2mesi.h"
    integer :: k, f, nba, a
    real(kind=8) :: r1, r2, t
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COORDO REF SUR ARETE A DE FACE F AU POINT T
!     ------------------------------------------------------------------
! IN  K    : I : NUMERO GLOBAL DE LA MAILLE
! IN  F    : I : NUMERO LOCAL  DE LA FACE
! IN  A    : I : NUMERO LOCAL  DE L' ARETE
! IN  NBA  : I : NOMBRE D' A DE LA FACE
! IN  T    : I : ABSCISSE DU POINT SUR L' A 0<T<1
! OUT R1   : R : COORDO REF 1
! OUT R2   : R : COORDO REF 2
!     ------------------------------------------------------------------
!
    integer :: vali(2)
    real(kind=8) :: zero, un, deux
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    zero = 0.0d0
    un = 1.0d0
    deux = 2.0d0
    if (nba .eq. 3) then
        if (a .eq. 1) then
            r1 = t
            r2 = zero
        else if (a .eq. 2) then
            r1 = un - t
            r2 = t
        else
            r2 = t
            r1 = zero
        endif
    else if (nba .eq. 4) then
        if (a .eq. 1) then
            r1 = deux*t - 1
            r2 = -un
        else if (a .eq. 2) then
            r2 = deux*t - 1
            r1 = un
        else if (a .eq. 3) then
            r1 = -deux*t + 1
            r2 = un
        else
            r2 = -deux*t + 1
            r1 = -un
        endif
    else
        vali (1) = k
        vali (2) = f
        call u2mesi('F', 'INTEMAIL_22', 2, vali)
    endif
end subroutine
