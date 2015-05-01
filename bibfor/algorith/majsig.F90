subroutine majsig(materf, se, seq, i1e, alpha,&
                  dp, plas, sig)
! =====================================================================
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
    real(kind=8) :: materf(5, 2), dp, se(*), seq, i1e, alpha, sig(6)
! =====================================================================
! --- MISE A JOUR DES CONTRAINTES -------------------------------------
! =====================================================================
    integer :: ii, ndt, ndi
    real(kind=8) :: young, nu, troisk, deuxmu, trois, deux, un
    real(kind=8) :: i1, dev(6), plas
    parameter ( trois  =  3.0d0 )
    parameter ( deux   =  2.0d0 )
    parameter ( un     =  1.0d0 )
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
! --- AFFECTATION DES VARIABLES ---------------------------------------
! =====================================================================
    young = materf(1,1)
    nu = materf(2,1)
    troisk = young / (un-deux*nu)
    deuxmu = young / (un+nu)
! =====================================================================
! --- MISE A JOUR DU DEVIATEUR ----------------------------------------
! =====================================================================
    if (plas .eq. 2.0d0) then
        do 10 ii = 1, ndt
            dev(ii) = 0.0d0
10      continue
    else
        do 20 ii = 1, ndt
            dev(ii) = se(ii)*(un-trois*deuxmu/deux*dp/seq)
20      continue
    endif
!
! =====================================================================
! --- MISE A JOUR DU PREMIER INVARIANT --------------------------------
! =====================================================================
    i1 = i1e - trois*troisk*alpha*dp
! =====================================================================
! --- MISE A JOUR DU VECTEUR DE CONTRAINTES ---------------------------
! =====================================================================
    do 30 ii = 1, ndt
        sig(ii) = dev(ii)
30  end do
    do 40 ii = 1, ndi
        sig(ii) = sig(ii) + i1/trois
40  end do
! =====================================================================
end subroutine
