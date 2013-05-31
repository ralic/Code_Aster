subroutine zerofr(intini, algo, func, x1, x2,&
                  tol, itmax, solu, iret, iter)
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
! aslint: disable=
    implicit none
!
    include 'asterfort/assert.h'
    include 'asterfort/encadr.h'
    include 'asterfort/zerof2.h'
    include 'asterfort/zerofb.h'
    include 'asterfort/zerofc.h'
    include 'asterfort/zerofo.h'
    integer :: intini, itmax, iter, iret
    character(len=*) :: algo
    real(kind=8) :: solu, tol, x1, x2, func
    external      func
!
! ----------------------------------------------------------------------
!     BUT : TROUVER LE ZERO D'UNE FONCTION SCALAIRE REELLE
!
!
! IN  INTINI : RECHERCHE DE L'INTERVALLE INITIAL
!              TEL QUE F CHANGE DE SIGNE
!              = 0 : PAS DE RECHERCHE
!              = 1 : RECHERCHE PAR BRACKETING CROISSANT
!              = 2 : RECHERCHE PAR BRACKETING CROISSANT A DROITE
! IN  ALGO   : ALGORITHME DE RECHERCHE DU ZERO : 'AUTO', 'SECANTE',
!              'DEKKER', 'DEKKER2', 'BRENT'
!              SI ALGO VAUT 'AUTO', ON PREND 'BRENT'
! IN  FUNC   : FONCTION F
! IN  X1, X2 : INTERVELLE DE RECHERCHE
! IN  TOL    : PRECISION ABSOLUE : LA SOLUTION X EST TELLE QUE F(X)<TOL
! IN  ITMAX  : NOMBRE D'ITERATIONS MAXIMUM
! OUT SOLU   : ZERO DE F
! OUT IRET   : CODE RETOUR : IRET = 0 : OK
!            :               SINON    : PROBLEME
! OUT ITER   : NOMBRE D'ITERATIONS EFFECTUEES
! ----------------------------------------------------------------------
!
    character(len=8) :: algoz
    real(kind=8) :: a, b, fa, fb
!
    algoz = algo
    a = x1
    b = x2
!
!     ------------------------------------------------------------------
!     RECHERCHE DE L'INTERVALLE INITIAL [A,B]
!     ------------------------------------------------------------------
!
    call assert(intini.eq.0.or. intini.eq.1.or. intini.eq.2)
!
    if (intini .eq. 1) then
!
!       BRACKETING CROISSANT A GAUCHE ET A DROITE
        call encadr(func, a, b, fa, fb,&
                    itmax, 1.6d0, iret)
        if (iret .ne. 0) goto 9999
!
    else if (intini.eq.2) then
!
!       BRACKETING CROISSANT UNIQUEMNT A DROITE :
!       SOUVENT LE CAS POUR LES LOIS DE COMPORTEMENT
!       (SI F EST CROISSANTE ET F(A)<0, OU L'INVERSE),
!       CE QUI PERMET DE PRENDRE UN COEF MULT GRAND (10)
        call encadr(func, a, b, fa, fb,&
                    itmax, 10.d0, iret)
        if (iret .ne. 0) goto 9999
!
    endif
!
!     ------------------------------------------------------------------
!     RECHERCHE DU ZERO DE F ENTRE X1 ET X2
!     ------------------------------------------------------------------
!
    if (algoz .eq. 'AUTO') algoz = 'BRENT'
!
!
    if (algoz .eq. 'BRENT') then
!
        call zerofb(func, a, b, tol, itmax,&
                    solu, iret, iter)
!
    else if (algoz.eq.'SECANTE') then
!
        call zerofc(func, a, b, tol, itmax,&
                    solu, iret, iter)
!
    else if (algoz.eq.'DEKKER') then
!
        call zerofo(func, a, b, tol, itmax,&
                    solu, iret, iter)
!
    else if (algoz.eq.'DEKKER2') then
!
        call zerof2(func, a, b, tol, itmax,&
                    solu, iret, iter)
!
    else
!
        call assert(.false.)
!
    endif
!
!
9999  continue
end subroutine
