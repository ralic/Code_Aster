subroutine lcmfbo(ep0, ep1, l0, l1, etamin,&
                  etamax, vide, etam, etap)
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "asterf_types.h"
#include "asterfort/lcrkbo.h"
#include "asterfort/lcvalp.h"
#include "asterfort/lcvpbo.h"
#include "asterfort/utmess.h"
    real(kind=8), intent(in) :: ep0(6), ep1(6), l0, l1, etamin, etamax
    aster_logical, intent(out) :: vide
    real(kind=8), intent(out) :: etam, etap
!
! --------------------------------------------------------------------------------------------------
!  BORNES POUR LE PILOTAGE RELATIF AU CRITERE M. FRANCOIS
!    chi**2 + l0 + l1*eta = 0
! --------------------------------------------------------------------------------------------------
! ep0(6)    déformation fixe
! ep1(6)    déformation pilotée
! l0        composante 0 du terme affine
! l1        composante 1 du terme affine
! etamin    borne min initiale
! etamax    borne max initiale
! vide      code retour: true=pas de solution, false=bornes
! etam      nouvelle borne min
! etap      nouvelle borne max
! --------------------------------------------------------------------------------------------------
!
    aster_logical :: vide1, vide2
    integer :: i, nsol, nsol1, nsol2, sgn(2), sgn1(2), sgn2(2), ptr
    real(kind=8) :: trep0, trep1, ts0(6), ts1(6), s0(3), s1(3), cb
    real(kind=8) :: s0s0, s0s1, s1s1, trs0, trs1, q0, q1, q2, sol(2), sol1(2), sol2(2)
    real(kind=8) :: am, ap, b
    real(kind=8), parameter :: zero=0.d0
    real(kind=8), parameter, dimension(6) :: kr=(/1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, gamma, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,gamma,rigmin,pc,pr,epsth
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: tau, sig0, beta
    common /lcmmf/ tau,sig0,beta
! --------------------------------------------------------------------------------------------------
!
! - INITIALISATION
    etam = etamin
    etap = etamax
!
!
! - VALEURS PROPRES DES CONTRAINTES NORMALISEES
!
    trep0 = ep0(1)+ep0(2)+ep0(3)
    trep1 = ep1(1)+ep1(2)+ep1(3)
!
    ts0 = (lambda*trep0*kr + deuxmu*ep0)/sig0
    ts1 = (lambda*trep1*kr + deuxmu*ep1)/sig0
    s0s0 = dot_product(ts0,ts0)
    s0s1 = dot_product(ts0,ts1)
    s1s1 = dot_product(ts1,ts1)
!
    trs0 = ts0(1)+ts0(2)+ts0(3)
    trs1 = ts1(1)+ts1(2)+ts1(3)
    call lcvalp(ts0, s0)
    call lcvalp(ts1, s1)
!
!
!
! - BORNES ISSUES DE LA PARTIE QUADRATIQUE DU CRITERE
!
    cb = 3*beta**2 - 1.d0/3.d0
    q2 = (s1s1 + cb*trs1*trs1)/tau**2
    q1 = 2*(s0s1 + cb*trs0*trs1)/tau**2 + l1
    q0 = (s0s0 + cb*trs0*trs0)/tau**2 + l0
!
    call lcvpbo(sqrt(q2), zero, q0, q1, etam,&
                etap, vide, nsol, sol, sgn)
!
    if (vide) goto 999
!
    if (nsol .eq. 1) then
        if (sgn(1) .eq. -1) then
            etam = sol(1)
        else
            etap = sol(1)
        endif
    else if (nsol.eq.2) then
        etam=sol(1)
        etap=sol(2)
    endif
!
!
! - BORNES ISSUES DU TERME DOMINANT DE L'EXPONENTIEL
!
!    ALTERNATIVE FIXEE (POSITIVE) DANS LE CHOIX DU MINORANT DE LA VP
    am = s1(3)/abs(log(tau))
    ap = s1(1)/abs(log(tau))
    b = s0(3)/abs(log(tau))
!
    if (etam .ge. 0) then
        call lcrkbo(ap, b, l0, l1, etam,&
                    etap, vide, nsol, sol, sgn)
!
!    ALTERNATIVE FIXEE (NEGATIVE) DANS LE CHOIX DU MINORANT DE LA VP
    else if (etap.le.0) then
        call lcrkbo(am, b, l0, l1, etam,&
                    etap, vide, nsol, sol, sgn)
!
!    ALTERNATIVE AVEC CHANGEMENT DE SIGNE
    else
        call lcrkbo(am, b, l0, l1, etam,&
                    zero, vide1, nsol1, sol1, sgn1)
        call lcrkbo(ap, b, l0, l1, zero,&
                    etap, vide2, nsol2, sol2, sgn2)
!
        vide = vide1 .and. vide2
        nsol = nsol1+nsol2
        if (nsol .gt. 2) call utmess('F', 'PILOTAGE_83')
!
        ptr = 0
        do i = 1, nsol1
            ptr = ptr+1
            sol(ptr)=sol1(i)
            sgn(ptr)=sgn1(i)
        end do
        do i = 1, nsol2
            ptr = ptr+1
            sol(ptr)=sol2(i)
            sgn(ptr)=sgn2(i)
        end do
    endif
!
    if (nsol .eq. 1) then
        if (sgn(1) .eq. -1) then
            etam = sol(1)
        else
            etap = sol(1)
        endif
    else if (nsol.eq.2) then
        etam=sol(1)
        etap=sol(2)
    endif
!
!
999 continue
end subroutine
