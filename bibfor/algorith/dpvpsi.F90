subroutine dpvpsi(nbmat, mater, se, seqe, i1e,&
                  fonecr, dp, sig)
! ====================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ====================================================================
! -----REACTUALISATION DES CONTRAINTES SI VISCOPLASTICITE ------------
! --- VISC_DRUC_PRAG --------------------------------------------------
! ====================================================================
    implicit      none
    integer :: nbmat
    real(kind=8) :: mater(nbmat, 2), dp, se(6), seqe, i1e, fonecr(3), sig(6)
! ====================================================================
! --- MISE A JOUR DES CONTRAINTES ------------------------------------
! ====================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: k, mu, troisk, trois, un
    real(kind=8) :: i1, dev(6)
    real(kind=8) :: beta
    parameter ( trois  =  3.0d0 )
    parameter ( un     =  1.0d0 )
! ====================================================================
    common /tdim/   ndt, ndi
! ====================================================================
! --- AFFECTATION DES VARIABLES --------------------------------------
! ====================================================================
    mu = mater(4,1)
    k = mater(5,1)
    troisk = trois*k
    beta = fonecr(3)
! ====================================================================
! --- MISE A JOUR DU DEVIATEUR ---------------------------------------
! ====================================================================
    do 10 ii = 1, ndt
        dev(ii) = se(ii)*(un-trois*mu*dp/seqe)
10  end do
! ====================================================================
! --- MISE A JOUR DU PREMIER INVARIANT -------------------------------
! ====================================================================
    i1 = i1e - trois*troisk*beta*dp
! ====================================================================
! --- MISE A JOUR DU VECTEUR DE CONTRAINTES --------------------------
! ====================================================================
    do 20 ii = 1, ndt
        sig(ii) = dev(ii)
20  end do
    do 30 ii = 1, ndi
        sig(ii) = sig(ii) + i1/trois
30  end do
! ====================================================================
end subroutine
