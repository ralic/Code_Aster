subroutine dpladg(yate, rho11, rho12, r, t,&
                  kh, congem, dimcon, adcp11, ndim,&
                  padp, dp11p1, dp11p2, dp21p1, dp21p2,&
                  dp11t, dp21t)
    implicit none
    integer :: yate, adcp11, ndim, dimcon
    real(kind=8) :: rho11, rho12, r, t, kh, congem(dimcon), padp
    real(kind=8) :: dp11p1, dp11p2, dp21p1, dp21p2
    real(kind=8) :: dp11t, dp21t
    real(kind=8) :: zero
    parameter (zero=0.d0)
! ----------------------------------------------------------------------
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
!      BUT :
!           CALCUL DES DERIVEES PARTIELLES DES PRESSIONS
!           UNIQUEMENT DANS LE CAS LIQU_AD_GAZ
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: l
!
! ----------------------------------------------------------------------
!
    dp11p1 = 1.d0/((rho12*r*t/rho11/kh)-1.d0)
    dp11p2 = (r*t/kh - 1.d0)/((rho12*r*t/rho11/kh)-1.d0)
    dp21p1 = zero
    dp21p2 = 1.d0
!
    if (yate .eq. 1.d0) then
        l = -congem(adcp11+ndim+1)
        dp11t = (-l*r*rho12/kh+padp/t)/((rho12*r*t/rho11/kh)-1.d0)
        dp21t = zero
    endif
!
end subroutine
