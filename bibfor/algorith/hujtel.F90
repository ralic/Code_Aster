subroutine hujtel(mod, mater, sig, hook)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
!  ---------------------------------------------------------------
!  CALCUL DE LA MATRICE DE RIGIDITE ELASTIQUE DE LA LOI HUJEUX
!  IN   MOD   :  MODELISATION
!       MATER :  COEFFICIENTS MATERIAU
!       SIG   :  CONTRAINTES
!  OUT  HOOK  :  OPERATEUR RIGIDITE ELASTIQUE
!  ---------------------------------------------------------------
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, i, j
    real(kind=8) :: sig(6), hook(6, 6), mater(22, 2), i1, coef
    real(kind=8) :: e, nu, al, demu, la
    real(kind=8) :: un, d13, zero, deux
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: delta
    real(kind=8) :: piso
    character(len=8) :: mod
!
    common /tdim/     ndt, ndi
!
    data   d13   / .333333333334D0 /
    data   un    / 1.d0 /
    data   zero  / 0.d0 /
    data   deux  / 2.d0 /
!
    piso = 1.5d0*mater(21,2)
    piso = zero
!
!
! ----------------------------------------------------------------
! --->   CALCUL PREMIER INVARIANT DES CONTRAINTES
    i1 = zero
    do 10 i = 1, ndi
        i1 = i1 + d13*sig(i)
10  end do
!
!
! --->   CALCUL DES COEF. UTILES
    if (i1 .eq. zero) then
        coef = un
    else
        coef = ((i1 -piso)/mater(8,2)) **mater(1,2)
    endif
!
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
!
        if (mater(17,1) .eq. un) then
!
            e = mater(1,1)*coef
            nu = mater(2,1)
            al = e*(un-nu) /(un+nu) /(un-deux*nu)
            demu = e /(un+nu)
            la = e*nu/(un+nu)/(un-deux*nu)
!
            do 30 i = 1, ndi
                do 30 j = 1, ndi
                    if (i .eq. j) hook(i,j) = al
                    if (i .ne. j) hook(i,j) = la
30              continue
            do 35 i = ndi+1, ndt
                hook(i,i) = demu
35          continue
!
        else if (mater(17,1).eq.deux) then
!
            e1 = mater(1,1)*coef
            e2 = mater(2,1)*coef
            e3 = mater(3,1)*coef
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            g1 = mater(7,1)*coef
            g2 = mater(8,1)*coef
            g3 = mater(9,1)*coef
            nu21 = mater(13,1)
            nu31 = mater(14,1)
            nu32 = mater(15,1)
            delta= mater(16,1)
!
            hook(1,1) = (un - nu23*nu32)*e1/delta
            hook(1,2) = (nu21 + nu31*nu23)*e1/delta
            hook(1,3) = (nu31 + nu21*nu32)*e1/delta
            hook(2,2) = (un - nu13*nu31)*e2/delta
            hook(2,3) = (nu32 + nu31*nu12)*e2/delta
            hook(3,3) = (un - nu21*nu12)*e3/delta
            hook(2,1) = hook(1,2)
            hook(3,1) = hook(1,3)
            hook(3,2) = hook(2,3)
            hook(4,4) = g1
            hook(5,5) = g2
            hook(6,6) = g3
!
        else
            call u2mess('F', 'COMPOR1_36')
        endif
!
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
!
        call u2mess('F', 'COMPOR1_4')
!
    endif
!
end subroutine
