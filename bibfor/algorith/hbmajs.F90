subroutine hbmajs(dg, nbmat, materf, se, i1e,&
                  sigeqe, etap, sigp)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! =====================================================================
    implicit      none
    integer :: nbmat
    real(kind=8) :: dg, materf(nbmat, 2), se(6), i1e, sigp(6), sigeqe, etap
! ======================================================================
! --- LOI DE HOEK BROWN : MISE A JOUR DES CONTRAINTES A T+ -------------
! ======================================================================
! IN   DG      INCREMENT DE LA VARIABLE GAMMA --------------------------
! IN   NBMAT   NOMBRE DE DONNEES MATERIAU ------------------------------
! IN   MATERF  DONNEES MATERIAU ----------------------------------------
! IN   SE      DEVIATEUR ELASTIQUE -------------------------------------
! IN   ETAP    VALEUR DE ETA A GAMMA_PLUS ------------------------------
! OUT  SIGP    CONTRAINTES A T+ ----------------------------------------
! ======================================================================
    integer :: ii, ndi, ndt
    real(kind=8) :: k, mu, i1, dev(6), un, neuf, trois
! =================================================================
    parameter       ( un     =  1.0d0  )
    parameter       ( neuf   =  9.0d0  )
    parameter       ( trois  =  3.0d0  )
! ======================================================================
    common /tdim/   ndt, ndi
! ======================================================================
    k = materf(5,1)
    mu = materf(4,1)
    do 10 ii = 1, ndt
        dev(ii) = se(ii)*(un-trois*mu*dg/(sigeqe*(etap+un)))
10  end do
    i1 = i1e - neuf*k*etap*dg/(etap+un)
    do 20 ii = 1, ndt
        sigp(ii) = dev(ii)
20  end do
    do 30 ii = 1, ndi
        sigp(ii) = sigp(ii) + i1/trois
30  end do
! ======================================================================
end subroutine
