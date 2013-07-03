subroutine lcfrlo(ndim, typmod, imate, epsm, deps,&
                  vim, option, sig, vip, dsidep)
!
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
!
!
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
    character(len=8) :: typmod(*)
    character(len=16) :: option
    integer :: ndim, imate
    real(kind=8) :: epsm(6), deps(6), vim(2)
    real(kind=8) :: sig(6), vip(2), dsidep(6, 6)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ELASTIQUE FRAGILE (SANS REGULARISATION)
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  EPSM    : DEFORMATION EN T-
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT SIG     : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1   -> VALEUR DE L'ENDOMMAGEMENT
!                 2   -> ELASTIQUE (0) OU DISSIPATIF (1)
! OUT DSIDEP  : MATRICE TANGENTE
! ----------------------------------------------------------------------
!
!
!
!
    logical :: cplan, rigi, resi, secant, nonlin
    integer :: ndimsi, k, l, etat
!
    real(kind=8) :: eps(6), treps, coplan, sigel(6)
    real(kind=8) :: kron(6)
    real(kind=8) :: fd, d, dm, ener, coef
    real(kind=8) :: e, nu, lambda, deuxmu, gamma, sy, wy
!
    integer :: icodre(3)
    character(len=8) :: nomres(3)
    real(kind=8) :: valres(3)
!
!
    real(kind=8) :: dmax, fmin
    parameter  (dmax = 1.d0, fmin = 1.d-5)
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
! ----------------------------------------------------------------------
!
!
!
! ======================================================================
!                            INITIALISATION
! ======================================================================
!
!
! -- OPTION ET MODELISATION
!
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    secant = option(11:14).eq.'ELAS'
    cplan = (typmod(1).eq.'C_PLAN  ')
    ndimsi = 2*ndim
!
!
! -- LECTURE DES CARACTERISTIQUES ELASTIQUES
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    call rcvala(imate, ' ', 'ELAS', 0, ' ',&
                0.d0, 2, nomres, valres, icodre,&
                1)
!
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
!
!
! -- LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'SY'
    nomres(2) = 'D_SIGM_EPSI'
    call rcvala(imate, ' ', 'ECRO_LINE', 0, ' ',&
                0.d0, 2, nomres, valres, icodre,&
                1)
    sy = valres(1)
    gamma = - valres(2)/e
    wy = sy**2 / (2*e)
!
!
! -- DEFORMATIONS
!
    call dcopy(ndimsi, epsm, 1, eps, 1)
    if (resi) call daxpy(ndimsi, 1.d0, deps, 1, eps,&
                         1)
!
!
!
! ======================================================================
!                         CONTRAINTES ELASTIQUES
! ======================================================================
!
! -- SI CONTRAINTES PLANES
!
    if (cplan) then
        coplan = - nu/(1.d0-nu)
        eps(3) = coplan * (eps(1)+eps(2))
    endif
!
!
! -- CALCUL DES CONTRAINTES ELASTIQUES
!
    treps = eps(1)+eps(2)+eps(3)
    do 60 k = 1, ndimsi
        sigel(k) = lambda*treps*kron(k) + deuxmu*eps(k)
60  end do
    ener = 0.5d0 * ddot(ndimsi,eps,1,sigel,1)
!
!
!
! ======================================================================
!                 INTEGRATION DE LA LOI DE COMPORTEMENT
! ======================================================================
!
    if (resi) then
!
        dm = vim(1)
        etat = nint(vim(2))
!
!
! -- POINT DEJA SATURE
!
        if (etat .eq. 2) then
            d = dm
!
!
! -- CALCUL DE L'ETAT D'ENDOMMAGEMENT
        else
            if (ener .le. wy*((1+gamma)/(1+gamma-dm))**2) then
                d = dm
                etat = 0
            else
                etat = 1
                d = max(dm, (1+gamma)*(1-sqrt(wy/ener)))
                if (d .gt. dmax) then
                    d = dmax
                    etat = 2
                endif
            endif
        endif
!
! -- CALCUL DES CONTRAINTES
!
        do 30 k = 1, ndimsi
            sig(k) = (1-d) * sigel(k)
30      continue
!
!
! -- STOCKAGE DES VARIABLES INTERNES
!
        vip(1) = d
        vip(2) = etat
!
    else
        d = vim(1)
        etat=nint(vim(2))
    endif
!
!
!
! ======================================================================
!                            MATRICE TANGENTE
! ======================================================================
!
!
    if (rigi) then
!
!
        nonlin=.false.
        if (resi .and. (.not.secant)) nonlin = etat.eq.1
!
!
!
!
! -- CONTRIBUTION ELASTIQUE
!
        call r8inir(36, 0.d0, dsidep, 1)
        fd = 1-d
        fd = max(fmin, fd)
        do 100 k = 1, 3
            do 110 l = 1, 3
                dsidep(k,l) = fd*lambda
110          continue
100      continue
        do 120 k = 1, ndimsi
            dsidep(k,k) = dsidep(k,k) + fd*deuxmu
120      continue
!
!
! -- CONTRIBUTION DISSIPATIVE
!
        if (nonlin) then
            coef = (1+gamma-d)**3 / (wy*2*(1+gamma)**2)
            do 200 k = 1, ndimsi
                do 210 l = 1, ndimsi
                    dsidep(k,l) = dsidep(k,l) - coef * sigel(k) * sigel(l)
210              continue
200          continue
        endif
!
!
! -- CORRECTION CONTRAINTES PLANES
!
        if (cplan) then
            do 300 k = 1, ndimsi
                if (k .eq. 3) goto 300
                do 310 l = 1, ndimsi
                    if (l .eq. 3) goto 310
                    dsidep(k,l)=dsidep(k,l) - 1.d0/dsidep(3,3)*dsidep(&
                    k,3)*dsidep(3,l)
310              continue
300          continue
        endif
!
    endif
end subroutine
