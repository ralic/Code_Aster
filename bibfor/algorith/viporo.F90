subroutine viporo(nbvari, vintm, vintp, advico, vicphi,&
                  phi0, deps, depsv, alphfi, dt,&
                  dp1, dp2, signe, sat, cs,&
                  tbiot, phi, phim, retcom, cbiot,&
                  unsks, alpha0, aniso, phenom)
    implicit      none
    integer :: nbvari, advico, vicphi, retcom, i, aniso
    real(kind=8) :: vintm(nbvari), vintp(nbvari), phi0
    real(kind=8) :: depsv, alphfi, dt, dp1, dp2, signe, sat, cs, tbiot(6)
    real(kind=8) :: phi, phim, rac2, deps(6), cbiot, unsks, alpha0
    character(len=16) :: phenom
! ======================================================================
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
    999 if (aniso.eq.0) then
    varbio = - depsv + 3.d0*alpha0*dt - (dp2-sat*signe*dp1)*unsks
    if (varbio .gt. epxmax) then
        retcom = 2
        goto 30
    endif
    vintp(advico+vicphi) = cbiot - phi0 - (cbiot-vintm(advico+ vicphi)-phi0)*exp(varbio)
!
    phi = vintp(advico+vicphi) + phi0
    phim = vintm(advico+vicphi) + phi0
else if ((aniso.eq.1).or.(aniso.eq.2)) then
    if (phenom .eq. 'ELAS') then
        aniso=0
        goto 999
        else if ((phenom.eq.'ELAS_ISTR').or. (phenom.eq.'ELAS_ORTH'))&
        then
        rac2 = sqrt(2.d0)
        varbio=0
        do 10 i = 1, 3
            varbio = varbio + tbiot(i)*deps(i)
10      continue
        do 20 i = 4, 6
            varbio = varbio + tbiot(i)*deps(i)/rac2
20      continue
        varbio = varbio-(&
                 vintm(advico+vicphi)-phi0)*depsv - 3.d0*alphfi*dt + cs*(dp2-sat*signe*dp1)
        if (varbio .gt. epxmax) then
            retcom = 2
            goto 30
        endif
        vintp(advico+vicphi) = varbio + vintm(advico+vicphi)
        phi = vintp(advico+vicphi) + phi0
        phim = vintm(advico+vicphi) + phi0
    endif
endif
! ======================================================================
30  continue
! =====================================================================
! ======================================================================
end subroutine
