subroutine pk2sig(ndim, f, jac, pk2, sig,&
                  ind)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! ----------------------------------------------------------------------
!     SI IND=1
!     CALCUL DES CONTRAINTES DE CAUCHY, CONVERSION PK2 -> CAUCHY
!     LES CONTRAINTES PK2 EN ENTREE ONT DES RAC2
!     LES CONTRAINTES DE CAUCHY SIG EN SORTIE N'ONT PAS DE RAC2
!     SI IND=-1
!     CALCUL DES CONTRAINTES DE PIOLA-KIRCHHOFF-2 A PARTIR DE CAUCHY
!     LES CONTRAINTES DE CAUCHY SIG EN ENTREE N'ONT PAS DE RAC2
!     LES CONTRAINTES PK2 EN SORTIE N'ONT PAS DE RAC2
! ----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  F       : GRADIENT TRANSFORMATION EN T+
! IN  JAC     : DET DU GRADIENT TRANSFORMATION EN T-
! IN/OUT  PK2 : CONTRAINTES DE PIOLA-KIRCHHOFF 2EME ESPECE
! IN/OUT  SIG : CONTRAINTES DE CAUCHY
! IN  IND     : CHOIX
!
    implicit none
    include 'asterfort/matinv.h'
    integer :: ndim, indi(6), indj(6), ind, pq, kl
    real(kind=8) :: f(3, 3), jac, pk2(2*ndim), sig(2*ndim), ftf, rind1(6)
    real(kind=8) :: fmm(3, 3)
    real(kind=8) :: r8bid, rind(6)
    data    indi / 1 , 2 , 3 , 1, 1, 2 /
    data    indj / 1 , 2 , 3 , 2, 3, 3 /
    data    rind / 0.5d0,0.5d0,0.5d0,0.70710678118655D0,&
     &               0.70710678118655D0,0.70710678118655D0 /
    data    rind1 / 0.5d0 , 0.5d0 , 0.5d0 , 1.d0, 1.d0, 1.d0 /
!
!     SEPARATION DIM 2 ET DIM 3 POUR GAGNER DU TEMPS CPU
    if (ind .eq. 1) then
!
        if (ndim .eq. 2) then
!
            do 112 pq = 1, 4
                sig(pq) = 0.d0
                do 122 kl = 1, 4
                    ftf = (&
                          f(&
                          indi(pq), indi(kl))*f(indj(pq), indj(kl))+ f(indi(pq),&
                          indj(kl))*f(indj(pq), indi(kl))&
                          ) *rind(kl&
                          )
                    sig(pq) = sig(pq)+ ftf*pk2(kl)
122              continue
                sig(pq) = sig(pq)/jac
112          continue
!
        else if (ndim.eq.3) then
!
            do 113 pq = 1, 6
                sig(pq) = 0.d0
                do 123 kl = 1, 6
                    ftf = (&
                          f(&
                          indi(pq), indi(kl))*f(indj(pq), indj(kl))+ f(indi(pq),&
                          indj(kl))*f(indj(pq), indi(kl))&
                          ) *rind(kl&
                          )
                    sig(pq) = sig(pq)+ ftf*pk2(kl)
123              continue
                sig(pq) = sig(pq)/jac
113          continue
!
        endif
!
    else if (ind.eq.-1) then
!
        call matinv('S', 3, f, fmm, r8bid)
!
        if (ndim .eq. 2) then
!
            do 212 pq = 1, 4
                pk2(pq) = 0.d0
                do 222 kl = 1, 4
                    ftf=(fmm(indi(pq),indi(kl))*fmm(indj(pq),indj(kl))&
                    + fmm(indi(pq),indj(kl))*fmm(indj(pq),indi(kl)))&
                    *rind1(kl)
                    pk2(pq) = pk2(pq)+ ftf*sig(kl)
222              continue
                pk2(pq) = pk2(pq)*jac
212          continue
!
        else if (ndim.eq.3) then
!
            do 213 pq = 1, 6
                pk2(pq) = 0.d0
                do 223 kl = 1, 6
                    ftf=(fmm(indi(pq),indi(kl))*fmm(indj(pq),indj(kl))&
                    + fmm(indi(pq),indj(kl))*fmm(indj(pq),indi(kl)))&
                    *rind1(kl)
                    pk2(pq) = pk2(pq)+ ftf*sig(kl)
223              continue
                pk2(pq) = pk2(pq)*jac
213          continue
!
        endif
!
    endif
!
end subroutine
