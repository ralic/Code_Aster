subroutine dsde2d(ndim, f, dsde, d)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
!
! ----------------------------------------------------------------------
!     CALCUL DU TERME D=2FF(dS/dE)F^TF^T POUR LES FORMULATIONS INCO_LOG
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  F       : GRADIENT TRANSFORMATION EN T
! IN  DSDE    : 2dS/dC = dS/dE_GL AVEC RACINE DE 2 ET 2
! OUT D       : 2FF(dS/dE)F^TF^T
!
    implicit none
    integer :: ndim, indi(6), indj(6), mn, op, ij, kl, nmax
    real(kind=8) :: f(3, 3), d(6, 6), dsde(6, 6), f1, f2, f3, f4
    real(kind=8) :: rind(6)
    data    indi / 1, 2, 3, 1, 1, 2 /
    data    indj / 1, 2, 3, 2, 3, 3 /
    data    rind / 0.5d0, 0.5d0, 0.5d0, 0.70710678118655d0,&
     &               0.70710678118655d0, 0.70710678118655d0 /
!
    nmax=2*ndim
!
    do 100 mn = 1, nmax
        do 200 op = 1, nmax
            d(mn,op) = 0.d0
            do 300 ij = 1, nmax
                do 400 kl = 1, nmax
                    f1 = f(indi(mn),indi(ij))*f(indj(mn),indj(ij))
                    f2 = f(indi(op),indi(kl))*f(indj(op),indj(kl))
                    f3 = f(indi(mn),indj(ij))*f(indj(mn),indi(ij))
                    f4 = f(indi(op),indj(kl))*f(indj(op),indi(kl))
                    d(mn,op) = d(mn,op) +(f1+f3)*(f2+f4)*dsde(ij,kl)* rind(ij)*rind(kl)
400              continue
300          continue
200      continue
100  end do
end subroutine
