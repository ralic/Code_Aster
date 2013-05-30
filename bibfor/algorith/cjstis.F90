subroutine cjstis(mod, mater, sig, vin, dsde)
    implicit none
!       ================================================================
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
!     CALCUL DE LA MATRICE TANGENTE DU PROBLEME CONTINU DE LA LOI CJS
!     POUR LE MECANISME PLASTIQUE ISOTROPE
!     IN   MOD     :  MODELISATION
!          MATER   :  COEFFICIENTS MATERIAU
!          SIG     :  CONTRAINTES
!          VIN     :  VARIABLES INTERNES
!     OUT  DSDE    :  MATRICE TANGENTE
!       ----------------------------------------------------------------
!
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi
!
    real(kind=8) :: sig(6), dsde(6, 6), mater(14, 2), vin(*), i1
    real(kind=8) :: e, nu, al, la, mu, ke, kp, coef1, coef2
    real(kind=8) :: zero, d12, un, deux, trois, qinit, pa
    integer :: i, j
!
    character(len=8) :: mod
!
    common /tdim/   ndt, ndi
!
    data          zero  / 0.d0 /
    data          d12   / .5d0 /
    data          un    / 1.d0 /
    data          deux  / 2.d0 /
    data          trois / 3.d0 /
!
!       ----------------------------------------------------------------
!
!--->   CALCUL PREMIER INVARIANT DES CONTRAINTES
    qinit = mater(13,2)
    pa = mater(12,2)
    i1 = zero
    do 10 i = 1, ndi
        i1 = i1 + sig(i)
10  continue
!
    if ((i1+qinit) .eq. 0.d0) then
        i1 = -qinit+1.d-12 * pa
    endif
!
!--->   CALCUL DES COEF. UTILES
    coef1 = ((i1+qinit)/trois/mater(12,2))**mater(3,2)
    e = mater(1,1) * coef1
    nu = mater(2,1)
    ke = e/trois/( un-deux*nu )
    kp = mater(4,2)*(vin(1)/mater(12,2))**mater(3,2)
    coef2 = ke * ke / (ke+kp)
    al = e * (un-nu) / (un+nu) / (un-deux*nu) - coef2
    la = nu * e / (un+nu) / (un-deux*nu) - coef2
    mu = e * d12 / (un+nu)
!
!
! - EN FAIT ON NE TIENT PAS COMPTE DE COEF2, SINON NON CONVERGENCE
!
    al = e * (un-nu) / (un+nu) / (un-deux*nu)
    la = nu * e / (un+nu) / (un-deux*nu)
!
!
!
!--->   OPERATEUR DE RIGIDITE
!
! - 3D/DP/AX
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
        do 20 i = 1, ndi
            do 20 j = 1, ndi
                if (i .eq. j) dsde(i,j) = al
                if (i .ne. j) dsde(i,j) = la
20          continue
        do 30 i = ndi+1, ndt
            do 30 j = ndi+1, ndt
                if (i .eq. j) dsde(i,j) = deux* mu
30          continue
!
! - CP/1D
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call u2mess('F', 'ALGORITH2_15')
    endif
!
end subroutine
