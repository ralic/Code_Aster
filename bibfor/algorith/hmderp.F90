subroutine hmderp(yate, yavp, t, r, kh,&
                  pvp, pad, rho11, rho12, h11,&
                  h12, cliq, alpliq, dp11p1, dp11p2,&
                  dp11t, dp12p1, dp12p2, dp12t, dp21p1,&
                  dp21p2, dp21t, dp22p1, dp22p2, dp22t,&
                  dp1pp1, dp2pp1, dtpp1, dp1pp2, dp2pp2,&
                  dtpp2, dp1pt, dp2pt, dtpt)
! ======================================================================
! person_in_charge: sylvie.granet at edf.fr
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
! TOLE CRP_21
!
! *********************************************************************
! ROUTINE HMDERP
! CALCULE LES DERIVEES PARTIELLES DES PRESSIONS DU PREMIER
! ET SECOND ORDRE DANS LE CAS OU THMC = 'LIQU_AD_GAZ_VAPE'
! *********************************************************************
!
! OUT RETCOM : RETOUR LOI DE COMPORTEMENT
!  VARIABLES IN / OUT
!
    implicit none
    integer :: yate
    logical :: yavp
    real(kind=8) :: t, r, kh, pvp, pad, rho11
    real(kind=8) :: rho12, h11, h12, cliq, alpliq
!
! DERIVEES PARTIELLES DES PRESSIONS PAR RAP A P1, P2 ET T
    real(kind=8) :: dp11p1, dp11p2, dp11t, dp12p1, dp12p2, dp12t
    real(kind=8) :: dp21p1, dp21p2, dp21t, dp22p1, dp22p2, dp22t
!
! DERIVEES PARTIELLES SECONDES DE PVP (1) ET PAD (2)
! PAR RAP A P1, P2 ET T
    real(kind=8) :: dp1pp1(2), dp2pp1(2), dtpp1(2)
    real(kind=8) :: dp1pp2(2), dp2pp2(2), dtpp2(2)
    real(kind=8) :: dp1pt(2), dp2pt(2), dtpt(2)
!
!  VARIABLES LOCALES
!
!
    real(kind=8) :: a1, a2, a3, a4, l, zero
    parameter(zero=0.d0)
!
!
    if (yavp) then
!
! *********************************************************************
! CALCUL DES DERIVEES PARTIELLES DES PRESSIONS
!
        dp11p1 = rho11*kh/(rho12*r*t-rho11*kh)
        dp11p2 = rho11*kh*(r*t/kh - 1.d0)/(rho12*r*t-rho11*kh)
        dp12p1 = rho12/(rho12*r*t/kh-(rho11))
        dp12p2 = (r*t/kh-1.d0)*dp12p1
        dp21p1 = - dp12p1
        dp21p2 = 1.d0 - dp12p2
        dp22p1 = -1.d0- dp11p1
        dp22p2 = 1.d0- dp11p2
!
        if ((yate.eq.1)) then
            l = (h12-h11)
!
            dp11t = (-l*r*rho12/kh+pad/t)/ ((rho12*r*t/rho11/kh)-1)
            dp12t = (-l*rho11+pad)/t*dp12p1
            dp21t = - dp12t
            dp22t = - dp11t
        endif
!
! *********************************************************************
! CALCUL DES DERIVEES SECONDES DES  PRESSIONS
        a1 = r*t/kh - rho11/rho12
        a2 = rho11/rho12*(r*t/kh - 1)
        a3 = dp12t/pvp-1/t-cliq*dp22t-3*alpliq
        a4 = -dp12t/pvp+1/t+cliq*dp22t-3*alpliq
!
! *********************************************************************
!
!    DERIVEE DE LA DERIVEE PAR RAPPORT AU GAZ : DP1PP2,DP2PP2,
!
        dp1pp2(1) = a2/a1/a1*(cliq*dp11p1-1/pvp*dp12p1)
        dp2pp2(1) = a2/a1/a1*(cliq*dp11p2-1/pvp*dp12p2)
        dp1pp2(2) = r*t*a2/a1/a1/kh*(-cliq*dp11p1+1/pvp*dp12p1)
        dp2pp2(2) = r*t*a2/a1/a1/kh*(-cliq*dp11p2+1/pvp*dp12p2)
!
!
!    DERIVEE DE LA DERIVEE PAR RAPPORT A PC : DP1PP1,DP2PP1,
!
!
        dp1pp1(1) = -rho11/rho12/a1/a1*(dp12p1/pvp-cliq*dp11p1)
        dp2pp1(1) = -rho11/rho12/a1/a1*(dp12p2/pvp-cliq*dp11p2)
        dp1pp1(2) = r*t/kh*rho11/rho12/a1/a1*(dp12p1/pvp-cliq*dp11p1)
        dp2pp1(2) = r*t/kh*rho11/rho12/a1/a1*(dp12p2/pvp-cliq*dp11p2)
!
        if ((yate.eq.1)) then
            dtpp2(1) = r/a1/kh - (r*t/kh-1)/a1/a1*(r/kh-a4*rho11/ rho12)
            dtpp1(1) = -1.d0/a1/a1*(r/kh-rho11/rho12*a4)
            dtpp2(2) = r/a1/kh*rho11/rho12 + (rho11/rho12)*(rho11/ rho12)* a2*r/kh/a1/a1*(1.d0+a3&
                       &*t)
            dtpp1(2) = r/kh/a1/a1*rho11/rho12*(1.d0+a3*t)
!
!    DERIVEE DE LA DERIVEE PAR RAPPORT A T : DP1PT,DP2PT,DTPT
!
!
            dp1pt(1) = -1.d0/t/a1/a1*(&
                       a1*(&
                       1.d0-dp11p1*(1.d0+l*rho11* cliq)) +(pad-l*rho11)*rho11/rho12*(cliq*dp11p1-&
                       &dp12p1/pvp&
                       )&
                       )
            dp2pt(1) = -1.d0/t/a1/a1*(&
                       a1*(&
                       1.d0-dp11p2*(1.d0+l*rho11* cliq)) +(pad-l*rho11)*rho11/rho12*(cliq*dp11p2-&
                       &dp12p2/pvp&
                       )&
                       )
            dtpt(1) = +1.d0/t/a1*(&
                      dp22t-l*(rho11*dp11t*cliq-3.d0* alpliq*rho11))- 1.d0/t/t/a1/a1*(r*t/kh-rho1&
                      &1/rho12+t*(r/ kh-rho11/rho12*a4)) *(pad-l*rho11&
                      )
!
            dp1pt(2) = 1.d0/a1*rho11/rho12*(l*r/kh*rho12/pvp*dp12p1- dp22p1/t) -r*t/kh*rho11/rho1&
                       &2/a1/a1*(l*r/kh*rho12-pad/t)* a3
            dp2pt(2) = 1.d0/a1*rho11/rho12*(l*r/kh*rho12/pvp*dp12p2- dp22p2/t) -r*t/kh*rho11/rho1&
                       &2/a1/a1*(l*r/kh*rho12-pad/t)* a3
            dtpt(2) = rho11/rho12/a1*(&
                      l*r*rho12/kh*(dp12t/pvp-1.d0/t)+ pad/t/t -dp12t/t)-r*t/kh*rho11/rho12/a1/a1&
                      &* (l*r*rho12/kh- pad/t)*(a3+1.d0/t&
                      )
!
        endif
    else
! *********************************************************************
! CALCUL DES DERIVEES PARTIELLES DES PRESSIONS
!
        dp11p1 = rho11*kh/(rho12*r*t-rho11*kh)
        dp11p2 = rho11*kh*(r*t/kh - 1.d0)/(rho12*r*t-rho11*kh)
        dp12p1 = zero
        dp12p2 = zero
        dp21p1 = - dp12p1
        dp21p2 = 1.d0 - dp12p2
        dp22p1 = -1.d0- dp11p1
        dp22p2 = 1.d0- dp11p2
!
        if ((yate.eq.1)) then
            l = (h12-h11)
!
            dp11t = (-l*r*rho12/kh+pad/t)*rho11*kh/ (rho12*r*t-rho11* kh)
            dp12t = zero
            dp21t = - dp12t
            dp22t = - dp11t
        endif
!
! *********************************************************************
! CALCUL DES DERIVEES SECONDES DES  PRESSIONS
!
!
! *********************************************************************
!
!    DERIVEE DE LA DERIVEE PAR RAPPORT AU GAZ : DP1PP2,DP2PP2,
!
        dp1pp2(1) = zero
        dp2pp2(1) = zero
        dp1pp2(2) = zero
        dp2pp2(2) = zero
!
!
!    DERIVEE DE LA DERIVEE PAR RAPPORT A PC : DP1PP1,DP2PP1,
!
!
        dp1pp1(1) = zero
        dp2pp1(1) = zero
        dp1pp1(2) = zero
        dp2pp1(2) = zero
!
        if ((yate.eq.1)) then
            dtpp2(1) = zero
            dtpp1(1) = zero
            dtpp2(2) = zero
            dtpp1(2) = zero
!
!    DERIVEE DE LA DERIVEE PAR RAPPORT A T : DP1PT,DP2PT,DTPT
!
!
            dp1pt(1) = zero
            dp2pt(1) = zero
            dtpt(1) = zero
            dp1pt(2) = zero
            dp2pt(2) = zero
            dtpt(2) = zero
        endif
    endif
!
!
end subroutine
