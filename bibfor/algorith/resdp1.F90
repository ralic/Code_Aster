subroutine resdp1(materf, seq, i1e, pmoins, dp,&
                  plas)
! =====================================================================
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
! =====================================================================
    implicit      none
    include 'asterfort/schdp1.h'
    include 'asterfort/u2mess.h'
    real(kind=8) :: materf(5, 2), pmoins, dp, seq, i1e, plas, pptest, fcrit0
! =====================================================================
! --- RESOLUTION NUMERIQUE --------------------------------------------
! =====================================================================
    integer :: ndt, ndi
    real(kind=8) :: young, nu, troisk, deuxmu, sy, h, pult
    real(kind=8) :: trois, deux, un, fcrit, valpro
    real(kind=8) :: a1, valcoe, b2, a
! =====================================================================
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
    sy = materf(1,2)
    h = materf(2,2)
    a = materf(3,2)
    pult = materf(4,2)
! =====================================================================
! --- CALCUL ELASTIQUE ------------------------------------------------
! =====================================================================
    fcrit = schdp1(seq, i1e, sy, h, a, pult, pmoins)
! =====================================================================
! --- CALCUL PLASTIQUE ------------------------------------------------
! =====================================================================
    if (fcrit .gt. 0.0d0) then
        plas = 1.0d0
        if (pmoins .lt. pult) then
            a1 = trois * deuxmu / deux + trois * troisk * a * a + h
            if (a1 .eq. 0.0d0) then
                call u2mess('F', 'ALGORITH10_41')
            endif
            dp = fcrit / a1
            valcoe = pult - pmoins
            if (dp .gt. valcoe) then
                fcrit = schdp1(seq, i1e, sy, h, a, pult, pult)
                b2 = trois * deuxmu / deux + trois * troisk * a * a
                if (b2 .eq. 0.0d0) then
                    call u2mess('F', 'ALGORITH10_42')
                endif
                dp = fcrit / b2
            endif
        else
            b2 = trois * deuxmu / deux + trois * troisk * a * a
            if (b2 .eq. 0.0d0) then
                call u2mess('F', 'ALGORITH10_42')
            endif
            dp = fcrit / b2
        endif
    else
        plas = 0.0d0
        dp = 0.0d0
    endif
!
! =====================================================================
! --- PROJECTION AU SOMMET --------------------------------------------
! =====================================================================
    pptest = pmoins + dp
    b2 = trois * troisk * a * a
    fcrit0 = schdp1(0.0d0, i1e, sy, h, a, pult, pptest)
    valpro = fcrit0 / b2
!
    if ((plas.eq.1) .and. (dp.le.valpro)) then
        plas = 2.0d0
        fcrit = schdp1(0.0d0, i1e, sy, h, a, pult, pmoins)
        if (pmoins .lt. pult) then
            a1 = trois * troisk * a * a + h
            if (a1 .eq. 0.0d0) then
                call u2mess('F', 'ALGORITH10_41')
            endif
            dp = fcrit / a1
            valcoe = pult - pmoins
            if (dp .gt. valcoe) then
                fcrit = schdp1(0.0d0, i1e, sy, h, a, pult, pult)
                dp = fcrit / b2
            endif
        else
            dp = fcrit / b2
        endif
    endif
! =====================================================================
end subroutine
