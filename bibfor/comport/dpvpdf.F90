function dpvpdf(x, n, const, fonc1, fonc2,&
                fonc3, fonc4)
    implicit      none
    real(kind=8) :: x, n, const, const1
    real(kind=8) :: fonc1, fonc2, fonc3, fonc4
    real(kind=8) :: zero, un, deux, trois
    real(kind=8) :: dpvpdf, fonc, foncp, dfdp
! =====================================================================
! --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE -------
! ---- VISC_DRUC_PRAG -------------------------------------------------
! --- EQUATION NON LINEAIRE EN DP -------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    parameter ( zero    =  0.0d0 )
    parameter ( un      =  1.0d0 )
    parameter ( deux    =  2.0d0 )
    parameter ( trois   =  3.0d0 )
! =====================================================================
    fonc = fonc1 - fonc2 *x - fonc3 * x**2 - fonc4 * x**3
    foncp = -fonc2 -deux*x*fonc3-trois*x**2*fonc4
    if (fonc .lt. zero) then
        fonc = zero
    else
        fonc = fonc
    endif
    const1 = n * const * fonc**(n-un)
    dfdp = const1 * foncp - un
!
    dpvpdf = dfdp
! =====================================================================
end function
