subroutine fgcoke(nbcycl, sigmin, sigmax, n, m,&
                  sm, rke)
    implicit none
    include 'jeveux.h'
    real(kind=8) :: sigmin(*), sigmax(*)
    real(kind=8) :: n, m, sm, rke(*)
    integer :: nbcycl
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     -----------------------------------------------------------------
!     AMPLIFICATION DU CHARGEMENT (KE COEFFICIENT DE CORRECTION
!     ELASTO-PLASTIQUE
!     ------------------------------------------------------------------
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYCLES
! IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
! IN  N      : R   :
! IN  M      : R   :
! IN  SM     : R   :
! OUT RKE    : R   : VALEURS DU COEFFICIENT KE POUR CHAQUE CYCLE
!     ------------------------------------------------------------------
!
    real(kind=8) :: delta
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    do 10 i = 1, nbcycl
        delta = abs(sigmax(i)-sigmin(i))
        if (delta .le. 3.d0*sm) then
            rke(i) = 1.d0
        else if (delta.gt.3.d0*sm.and.delta.lt.3.d0*m*sm) then
            rke(i) = 1.d0 + ((1-n)/(n*(m-1)))*((delta/(3.d0*sm))-1.d0)
        else if (delta.ge.3*m*sm) then
            rke(i) = 1.d0/n
        endif
10  end do
!
end subroutine
