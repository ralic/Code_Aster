subroutine lcmcli(nomfam, nbsys, is, pgl,&
                  sigf, sicl)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CONTRAINTE DE CLIVAGE MAXI POUR LE MONOCRISTAL
!     ----------------------------------------------------------------
#include "asterfort/lcmmsg.h"
#include "asterfort/tnsvec.h"
    integer :: i, nbsys, is, ir, j
    real(kind=8) :: sigf(6), pgl(3, 3), ms(6), ng(3), si(3, 3), sing(3), sicl, p
    real(kind=8) :: lg(3)
    real(kind=8) :: qm(3, 3)
    character(len=16) :: nomfam
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
!
    ir=0
!
    call lcmmsg(nomfam, nbsys, is, pgl, ms,&
                ng, lg, ir, qm)
!     SIGMA (3,3)
!     calcul du max de Ns.(SIGMA.Ns)
    if (gdef .eq. 1) then
        call tnsvec(6, 3, si, sigf, 1.d0)
    else
        call tnsvec(6, 3, si, sigf, 1.d0/sqrt(2.d0))
    endif
    sing(:) = 0.d0
    do i = 1, 3
        do j = 1, 3
            sing(i) = sing(i) + si(i,j) * ng(j)
        end do
    end do
    p = 0.d0
    do i = 1, 3
        p = p + sing(i)*ng(i)
    end do
    sicl = max(sicl, p)
!
end subroutine
