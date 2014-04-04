function dpvpeq(x, n, const, fonc1, fonc2,&
                fonc3, fonc4)
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
    implicit      none
    real(kind=8) :: x, n, const
    real(kind=8) :: fonc1, fonc2, fonc3, fonc4
    real(kind=8) :: dpvpeq, fonc, zero
! =====================================================================
! --- LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER VISCOPLASTIQUE -------
! ---- VISC_DRUC_PRAG -------------------------------------------------
! --- EQUATION NON LINEAIRE EN DP -------------------------------------
! =====================================================================
    parameter ( zero    =  0.0d0 )
! =====================================================================
    fonc = fonc1 - fonc2 *x - fonc3 * x**2 - fonc4 * x**3
!
    if (fonc .lt. zero) then
        fonc = zero
    else
        fonc = fonc
    endif
    dpvpeq = const * fonc**n - x
! =====================================================================
end function
