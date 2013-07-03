subroutine lcervf(mode, ndimsi, eps, treps, epsdv,&
                  gameps, dgamde)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
#include "asterfort/r8inir.h"
#include "blas/ddot.h"
    integer :: mode, ndimsi
    real(kind=8) :: eps(ndimsi), treps, epsdv(ndimsi), gameps, dgamde(ndimsi)
! ----------------------------------------------------------------------
!  CALCUL DE GAMMA(EPS) POUR LA LOI ENDO_SCALAIRE AVEC GRAD_VARI
! ----------------------------------------------------------------------
!  IN  MODE    FONCTION RECHERCHEE (0 = VALEUR, 1 = VALEUR + DERIVEE)
!  IN  EPS     VALEUR DE L'ARGUMENT EPS(1:NDIMSI)
!  OUT TREPS   TRACE DES DEFORMATIONS
!  OUT EPSDV   DEVIATEUR DES DEFORMATIONS
!  OUT GAMEPS  FONCTION GAMMA(EPS)
!  OUT DGAMDE  DERIVEE DE GAMMA - SI MODE = 1
! ----------------------------------------------------------------------
    integer :: ij
    real(kind=8) :: kron(6), epseq2, ueps, heps, coefh, coefs
! ----------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcer/ pch,pct,pcs
! ----------------------------------------------------------------------
    data kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
! ----------------------------------------------------------------------
!
    treps = eps(1)+eps(2)+eps(3)
    do 15 ij = 1, ndimsi
        epsdv(ij) = eps(ij) - treps/3*kron(ij)
15  end do
!
    epseq2 = 1.5d0 * ddot(ndimsi,epsdv,1,epsdv,1)
    ueps = pch*treps**2+pcs*epseq2
    heps = pct*treps+sqrt(ueps)
    gameps = heps*heps
!
    if (mode .eq. 1) then
        call r8inir(ndimsi, 0.d0, dgamde, 1)
        if (ueps .ne. 0) then
            coefh = pct + pch*treps/sqrt(ueps)
            coefs = 1.5d0*pcs/sqrt(ueps)
            do 105 ij = 1, ndimsi
                dgamde(ij) = 2*heps*(coefh*kron(ij)+coefs*epsdv(ij))
105          continue
        endif
    endif
!
end subroutine
