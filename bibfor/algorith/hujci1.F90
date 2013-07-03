subroutine hujci1(crit, mater, deps, sigd, i1f,&
                  tract, iret)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! -----------------------------------------------------------------
!       HUJEUX : CALCUL DE I1F  --> I1 A T+DT
!       RESOLUTION DE L'EQUATION SCALAIRE F(I1) = 0 DU COMPORTEMENT
!       ELASTIQUE NON LINEAIRE AVEC
!
!       F(I1) = I1F - I1D - [ KOE * TRACE(DEPS) ] * (----------)**N
!       OU   I1  = TRACE(SIGMA) /3
!       ET   KOE = K = E /3/(1 - 2*NU)
!
! -----------------------------------------------------------------
! IN  CRIT  : CRITERES DE CONVERGENCE
! IN  MATER : COEFFICIENTS MATERIAU A T+DT
! IN  DEPS  : INCREMENT DE DEFORMATION
! IN  SIGD  : CONTRAINTE A T
! OUT I1    : 1/3*TRACE DE SIG A T+DT
!     TRACT : VARIABLE LOGIQUE INDIQUANT LA TRACTION (I1F > 0.d0)
! OUT IRET  : CODE RETOUR DE LORS DE LA RESOLUTION DE L'EQUATION
!             SCALAIRE
!                 IRET=0 => PAS DE PROBLEME
!                 IRET=1 => ECHEC
! -----------------------------------------------------------------
#include "asterfort/infniv.h"
#include "asterfort/u2mess.h"
#include "asterfort/zeroco.h"
    integer :: ndt, ndi, iret, ifm, niv
    real(kind=8) :: mater(22, 2), crit(*), deps(6), sigd(6), i1d, i1f
    real(kind=8) :: trdeps, coef, prec, alpha, theta
    real(kind=8) :: x(4), y(4)
    real(kind=8) :: young, poisso, n, pa, piso
    real(kind=8) :: zero, un, deux, d13, c11, c12, c13, c22, c23, c33
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, nu21, nu31, nu32, delta
    logical :: tract, debug
    integer :: i, niter, icmpt
!
    common /tdim/   ndt, ndi
    common /meshuj/ debug
!
    data zero /0.d0/
    data un   /1.d0/
    data deux /2.d0/
    data d13  /0.33333333333334d0/
!
    call infniv(ifm, niv)
!
!
!       METHODE DE LA SECANTE
!       =====================
    young = mater(1,1)
    poisso = mater(2,1)
    pa = mater(8,2)
    n = mater(1,2)
    piso = 1.5d0*mater(21,2)
    piso = zero
    iret = 0
    theta = un
!
!
!---> DETERMINATION DU TERME COEF = K0 x DEPS_VOLUMIQUE
    if (mater(17,1) .eq. un) then
!
        trdeps = zero
        do 5 i = 1, ndi
            trdeps = trdeps + deps(i)
 5      continue
!
!        COEF = YOUNG*D13 /(UN-N)/(UN-DEUX*POISSO) * TRDEPS
        coef = young*d13 /(un-deux*poisso) * trdeps
!
    else if (mater(17,1).eq.deux) then
!
        e1 = mater(1,1)
        e2 = mater(2,1)
        e3 = mater(3,1)
        nu12 = mater(4,1)
        nu13 = mater(5,1)
        nu23 = mater(6,1)
        nu21 = mater(13,1)
        nu31 = mater(14,1)
        nu32 = mater(15,1)
        delta= mater(16,1)
!
        c11 = (un - nu23*nu32)*e1/delta
        c12 = (nu21 + nu31*nu23)*e1/delta
        c13 = (nu31 + nu21*nu32)*e1/delta
        c22 = (un - nu13*nu31)*e2/delta
        c23 = (nu32 + nu31*nu12)*e2/delta
        c33 = (un - nu21*nu12)*e3/delta
!
        coef = (c11+c12+c13)*deps(1) + (c12+c22+c23)*deps(2) + (c13+ c23+c33)*deps(3)
        coef = d13*coef /(un-n)
!
    endif
!
    i1d = zero
    do 10 i = 1, ndi
        i1d = i1d + d13*sigd(i)
10  continue
!
    i1d =i1d -piso
!
    if (i1d .ge. zero) then
        i1d = 1.d-6 * pa
        call u2mess('A', 'COMPOR1_18')
    endif
    if (trdeps .eq. zero) then
        i1f = i1d
        goto 9999
    endif
!
! ---> COEF < 0 => ON VERIFIE UN CRITERE APPROXIMATIF
!                  D'EXISTENCE DE LA SOLUTION AVEC P+ < P- < 0
!       IF (COEF .GE. ZERO) GOTO 35
!
!       EXIST = DEUX*I1D - PA * (PA /COEF /N)**(UN-N)
!
!       IF (EXIST .LE. ZERO) THEN
!         IF (DEBUG) CALL U2MESS ('A', 'COMPOR1_13')
!         X(4)  = ZERO
!         THETA = ZERO
!         GOTO 50
!       ENDIF
!
!   35  CONTINUE
!
    tract = .false.
    if (n .eq. zero) then
        i1f = i1d + coef
        if (i1f .ge. zero) tract = .true.
        goto 9999
    endif
!
!
!
! --- DETERMINATION DES BORNES DE RECHERCHE DE LA SOLUTION
!     ====================================================
    alpha = 4.d0
!
    if (coef .lt. zero) then
!
        x(1) = i1d
        y(1) = coef*(x(1)/pa)**n
        icmpt = 1
45      continue
        x(2) = alpha*x(1)
        y(2) = coef*(x(2)/pa)**n - x(2) + i1d
!
        if (y(2) .le. zero .and. icmpt .le. 20) then
            x(1) = x(2)
            y(1) = coef*(x(1)/pa)**n
            icmpt= icmpt+1
            goto 45
        else if (y(2) .le. zero) then
            if (debug) call u2mess('A', 'COMPOR1_17')
            x(4) = zero
            theta = zero
            goto 50
        endif
!
!
! ---> COEF > 0 => LA SOLUTION EXISTE NECESSAIREMENT ET P- < P+ < 0
    else if (coef .gt. zero) then
!
        x(2) = i1d
        y(2) = coef*(x(2)/pa)**n
        x(1) = zero
        y(1) = i1d
!
!
! ---> COEF = 0 => LA SOLUTION N'EXISTE PAS :: ERREUR FATALE!
    else
        call u2mess('F', 'COMPOR1_12')
    endif
!
!
!
! --- CALCUL DE X(4), SOLUTION DE L'EQUATION F = 0 :
!     ===========================================
!
    x(3) = x(1)
    y(3) = y(1)
    x(4) = x(2)
    y(4) = y(2)
    niter = int(crit(1))
    prec = crit(3)
    icmpt = 0
!
41  continue
    do 40 i = 1, niter
!
        if (abs(y(4)/pa) .lt. prec) goto 50
        call zeroco(x, y)
        y(4) = coef*(x(4)/pa)**n - x(4) + i1d
!
40  continue
!
    icmpt = icmpt+1
    if (icmpt .lt. 5) goto 41
!
    if (debug) then
        write (ifm,*) 'MODELE DE HUJEUX : ATTENTION DANS HUJCI1'
        write (ifm,*) 'NON CONVERGENCE A LA PRECISION DEMANDEE',prec
        write (ifm,*) 'AU BOUT DU NOMBRE D ITERATION DEMANDE',niter
        write (ifm,*) 'VALEUR DE F ACTUELLE', y(4),' ET P=',x(4)
        write (ifm,*) 'AUGMENTER ITER_INTE_MAXI'
    endif
!
    if ((y(4)/(coef*(i1d/pa)**n + i1d)) .gt. 1.d-2) then
        x(4) = zero
        theta = zero
    endif
!
50  continue
!
    i1f = (un-theta)*(coef*(i1d/pa)**n + i1d) + theta*x(4) +piso
    if (i1f .ge. piso) tract = .true.
!
9999  continue
end subroutine
