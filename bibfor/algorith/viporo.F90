subroutine viporo(nbvari, vintm, vintp, advico, vicphi,&
                  phi0, depsv, alpha0, dt, dp1,&
                  dp2, signe, sat, cs, biot,&
                  phi, phim, retcom)
    implicit      none
    integer :: nbvari, advico, vicphi, retcom
    real(kind=8) :: vintm(nbvari), vintp(nbvari), phi0
    real(kind=8) :: depsv, alpha0, dt, dp1, dp2, signe, sat, cs, biot, phi, phim
! ======================================================================
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ======================================================================
! --- CALCUL ET STOCKAGE DE LA VARIABLE INTERNE DE POROSITE ------------
! ======================================================================
    real(kind=8) :: varbio, epxmax
    parameter    (epxmax = 5.d0)
! ======================================================================
! ======================================================================
! --- CALCUL DES ARGUMENTS EN EXPONENTIELS -----------------------------
! --- ET VERIFICATION DE SA COHERENCE ----------------------------------
! ======================================================================
    varbio = - depsv + 3.d0*alpha0*dt - ( dp2 - sat*signe*dp1 )*cs
    if (varbio .gt. epxmax) then
        retcom = 2
        goto 30
    endif
    vintp(advico+vicphi) = biot - phi0 - ( biot-vintm(advico+vicphi)-phi0)*exp(varbio)
!
    phi = vintp(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
! ======================================================================
30  continue
! =====================================================================
! ======================================================================
end subroutine
