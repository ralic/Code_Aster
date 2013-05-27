subroutine cjst(s, t)
    implicit none
!       ----------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!      CALCUL DE T = DET(S)*INV(S)
!       3D   : A = A11, A22, A33, RAC2 A12, RAC2 A13, RAC2 A23
!       D_PLAN OU AXIS A = A11, A22, A33, RAC2 A12
!       IN  S      :  MATRICE
!       OUT T      :  T (SOUS FORME VECTORIELLE AVEC RAC2)
!       ----------------------------------------------------------------
    integer :: n, nd
    real(kind=8) :: s(6), t(6), invrc2
    common /tdim/   n , nd
!
    invrc2 = 1.d0 / sqrt(2.d0)
    if (n .eq. 6) then
        t(1) = ( s(2)*s(3)-0.5d0*s(6)*s(6) )
        t(2) = ( s(1)*s(3)-0.5d0*s(5)*s(5) )
        t(3) = ( s(1)*s(2)-0.5d0*s(4)*s(4) )
        t(4) = ( invrc2*s(5)*s(6)-s(4)*s(3) )
        t(5) = ( invrc2*s(4)*s(6)-s(5)*s(2) )
        t(6) = ( invrc2*s(4)*s(5)-s(6)*s(1) )
    endif
!
    if (n .eq. 4) then
        t(1) = s(2)*s(3)
        t(2) = s(1)*s(3)
        t(3) = ( s(1)*s(2)-0.5d0*s(4)*s(4) )
        t(4) = -s(4)*s(3)
    endif
end subroutine
