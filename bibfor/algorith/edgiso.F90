subroutine edgiso(dp, pm, eqsitr, mu, gamma,&
                  m, n, seuil, dseuil)
    implicit none
!
!
    real(kind=8) :: dp, pm, eqsitr
    real(kind=8) :: mu, gamma(3), m(3), n(3)
    real(kind=8) :: seuil, dseuil
!
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ----------------------------------------------------------------------
!     MODELE VISCOPLASTIQUE SANS SEUIL DE EDGAR
!    CALCUL DE LA FONCTION SEUIL ET DE SA DERIVEE
!    SEUIL(DP)=EQSITR-3*MU*DP-GAMMA(K)*((PM+DP)**M(K))*(DP**N(K))
!  IN  DP      : INCREMENT DE DEFORMATION PLASTIQUE CUMULEE
!  IN  PM      : DEFORMATION PLASTIQUE CUMULEE A L INSTANT MOINS
!  IN  EQSITR : CONTRAINTE EQUIVALENTE ESSAI
!  IN  MU      : COEFFICIENT DE MATERIAU ELASTIQUE
!  IN  GAMMA   : COEFFICIENT VISQUEUX A MULTIPLIER PAR 2*MU
!  IN  M       : COEFFICIENT VISQUEUX
!  IN  N       : COEFFICIENT VISQUEUX
!
!  OUT SEUIL   : FONCTION SEUIL
!  OUT DSEUIL  : DERIVEE DE SEUIL
! ----------------------------------------------------------------------
!
    integer :: k
!
! 1 - FONCTION F
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    seuil = eqsitr-3.d0*mu*dp
    do 10 k = 1, 3
        seuil=seuil-2.d0*mu*gamma(k)*((pm+dp)**m(k))*(dp**n(k))
10  end do
!
! 2 - DERIVEE DE LA FONCTION F
!
    dseuil = -3.d0*mu
    do 20 k = 1, 3
        dseuil=dseuil -2.d0*mu*gamma(k)*m(k)*((pm+dp)**(m(k)-1.d0))*(&
        dp**n(k)) -2.d0*mu*gamma(k)*n(k)*((pm+dp)**m(k))*(dp**(n(k)-&
        1.d0))
20  end do
!
end subroutine
