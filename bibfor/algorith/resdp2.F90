subroutine resdp2(materf, seq, i1e, pmoins, dp,&
                  plas)
! =====================================================================
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
! =====================================================================
    implicit none
#include "asterfort/schdp2.h"
#include "asterfort/utmess.h"
    real(kind=8) :: materf(5, 2), pmoins, dp, seq, i1e, plas
! =====================================================================
! --- RESOLUTION NUMERIQUE --------------------------------------------
! =====================================================================
    integer :: ndt, ndi
    real(kind=8) :: young, nu, troisk, deuxmu, alpha1, phi, c, pult, alpha
    real(kind=8) :: trois, deux, un, fcrit, valpro, gamapm, gamarp
    real(kind=8) :: neuf, douze, a1, b1, delta, quatre, valcoe, b2
    real(kind=8) :: fcrit0, pptest
    parameter ( douze  = 12.0d0 )
    parameter ( neuf   =  9.0d0 )
    parameter ( quatre =  4.0d0 )
    parameter ( trois  =  3.0d0 )
    parameter ( deux   =  2.0d0 )
    parameter ( un     =  1.0d0 )
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
! --- AFFECTATION DES VARIABLES ---------------------------------------
! =====================================================================
    young = materf(1,1)
    nu = materf(2,1)
    troisk = young / (un-deux*nu)
    deuxmu = young / (un+nu)
    alpha1 = materf(1,2)
    phi = materf(2,2)
    c = materf(3,2)
    pult = materf(4,2)
    gamarp = sqrt ( trois / deux ) * pult
    gamapm = sqrt ( trois / deux ) * pmoins
    alpha = deux*sin(phi)/(trois-sin(phi))
! =====================================================================
! --- CALCUL ELASTIQUE ------------------------------------------------
! =====================================================================
    fcrit = schdp2(seq, i1e, phi, alpha1, c, pult, pmoins)
! =====================================================================
! --- CALCUL PLASTIQUE ------------------------------------------------
! =====================================================================
    if (fcrit .gt. 0.0d0) then
        plas = 1.0d0
        if (pmoins .lt. pult) then
            a1 = - neuf*c*cos(phi)* (un-alpha1)*(un-alpha1)/gamarp/ gamarp/(trois-sin(phi))
            b1 = - (&
                 trois*deuxmu/deux + trois*troisk*alpha*alpha - sqrt(trois/deux)*douze*c*cos(phi)&
                 &/(trois-sin(phi))* (un-(un-alpha1)/gamarp*gamapm)*(un-alpha1)/gamarp&
                 )
            delta = b1*b1 - quatre*a1*fcrit
            if (a1 .eq. 0.0d0) then
                call utmess('F', 'ALGORITH10_43')
            endif
            dp = - (b1 + sqrt(delta))/deux/a1
            valcoe = sqrt(deux/trois)*(gamarp-gamapm)
            if (dp .gt. valcoe) then
                fcrit = schdp2(seq,i1e,phi,alpha1,c,pult,pult)
                b2 = - ( trois*deuxmu/deux + trois*troisk*alpha*alpha )
                if (b2 .eq. 0.0d0) then
                    call utmess('F', 'ALGORITH10_42')
                endif
                dp = - fcrit / b2
            endif
        else
            b2 = - ( trois*deuxmu/deux + trois*troisk*alpha*alpha )
            if (b2 .eq. 0.0d0) then
                call utmess('F', 'ALGORITH10_42')
            endif
            dp = - fcrit / b2
        endif
    else
        plas = 0.0d0
        dp = 0.0d0
    endif
! =====================================================================
! --- PROJECTION AU SOMMET --------------------------------------------
! =====================================================================
    pptest = pmoins + dp
    b2 = trois * troisk *alpha*alpha
    fcrit0 = schdp2(0.0d0, i1e, phi, alpha1, c, pult, pptest)
    valpro = fcrit0 / b2
!
    a1 = - neuf*c*cos(phi)* (un-alpha1)*(un-alpha1)/gamarp/gamarp/(trois-sin(phi))
!
    if ((plas.eq.1) .and. (dp.le.valpro)) then
        plas = 2.0d0
        fcrit = schdp2(0.0d0, i1e, phi, alpha1, c, pult, pmoins)
        if (pmoins .lt. pult) then
            b1 = - (&
                 trois*troisk*alpha*alpha - sqrt(trois/deux)* douze*c*cos(phi)/(trois-sin(phi))* &
                 &(un-(un-alpha1)/gamarp* gamapm)*(un-alpha1)/gamarp&
                 )
            delta = b1*b1 - quatre*a1*fcrit
            dp = - (b1 + sqrt(delta))/deux/a1
            valcoe = sqrt(deux/trois)*(gamarp-gamapm)
            if (dp .gt. valcoe) then
                fcrit = schdp2(0.0d0,i1e,phi,alpha1,c,pult,pult)
                dp = fcrit / b2
            endif
        else
            dp = fcrit / b2
        endif
    endif
! =====================================================================
end subroutine
