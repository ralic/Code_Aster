subroutine lckimp(ndim, typmod, option, mat, epsm,&
                  deps, vim, nonloc, sig, vip,&
                  dsidep)
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
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
    character(len=8) :: typmod
    character(len=16) :: option
    integer :: ndim, mat
    real(kind=8) :: epsm(6), deps(6), vim(2), nonloc(3)
    real(kind=8) :: vip(2), sig(6), dsidep(6, 6, 4)
!
!     -----------------------------------------------------------------
!     ENDOMMAGEMENT FRAGILE ENDO_CARRE POUR GVNO
!     -----------------------------------------------------------------
!     IN  NDIM    DIMENSION DE L'ESPACE
!     IN  TYPMOD  TYPE DE MODELISATION
!     IN  OPTION  OPTION DE CALCUL
!     RIGI_MECA_TANG, RIGI_MECA_ELAS
!     RAPH_MECA
!     FULL_MECA, FULL_MECA_ELAS
!     IN  MAT     NATURE DU MATERIAU
!     IN  EPSM    CHAMP DE DEFORMATION EN T- ET PHIM=EPSM(7)
!     IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION ET DPHI=DEPS(7)
!     IN  VIM     VARIABLES INTERNES EN T-
!     IN  NONLOC  VARIABLES NON LOCALES
!     OUT VIP     DENSITE DE FISSURATION
!     OUT SIG     CONTRAINTE
!     OUT DSIDEP  MATRICES TANGENTES
!     -----------------------------------------------------------------
!
    logical :: cplan, resi
    integer :: ndimsi, ij, kl, i
    real(kind=8) :: val(3), nu, lambda, deuxmu
    real(kind=8) :: e
    real(kind=8) :: coplan, eps(6), phi, w, treps, epseps, sigel(6)
    real(kind=8) :: fd
    real(kind=8) :: kk, epsd(6)
    real(kind=8) :: sigm, w0
    integer :: k2(4)
    character(len=8) :: nom(3)
    real(kind=8) :: kron(6), rigmin
    parameter (rigmin = 1.d-5)
    data   kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!     -----------------------------------------------------------------
!
!
!     -----------------------------------------------------------------
!     INITIALISATIONS
!     -----------------------------------------------------------------
!
!
!     -- OPTIONS DE CALCUL
!
    cplan = typmod.eq.'C_PLAN  '
    resi=option(1:9).eq.'FULL_MECA'.or.option.eq.'RAPH_MECA'
!      RIGI=OPTION(1:9).EQ.'RIGI_MECA'.OR.OPTION(1:9).EQ.'FULL_MECA'
!      ELAS=OPTION.EQ.'RIGI_MECA_ELAS'.OR.OPTION.EQ.'FULL_MECA_ELAS'
    ndimsi = 2*ndim
!
!
!     -- LECTURE DES CARACTERISTIQUES MATERIAU
!
    nom(1) = 'E'
    nom(2) = 'NU'
    nom(3) = 'SY'
!
    call rcvala(mat, ' ', 'ELAS', 0, ' ',&
                0.d0, 2, nom(1), val(1), k2,&
                2)
!
    nu = val(2)
    e = val(1)
    lambda = val(1)*val(2) / (1-2*val(2)) / (1+val(2))
    deuxmu = val(1) / (1.d0+val(2))
    kk = lambda + deuxmu/(3.0d0)
!
    call rcvala(mat, ' ', 'ECRO_LINE', 0, ' ',&
                0.d0, 1, nom(3), val(3), k2,&
                2)
!
    sigm = val(3)
!
    w0 = sigm**2/(2*e)
!
!     -- DEFORMATIONS COURANTES
!
    call dcopy(ndimsi, epsm, 1, eps, 1)
    if (resi) call daxpy(ndimsi, 1.d0, deps, 1, eps,&
                         1)
!
!     DEFORMATION HORS PLAN POUR LES CONTRAINTES PLANES
    if (cplan) then
        coplan = - nu/(1.d0-nu)
        eps(3) = coplan * (eps(1)+eps(2))
    endif
!
    phi = nonloc(1)
!
!     -- ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE
!
    treps = eps(1)+eps(2)+eps(3)
!
!     -- DEVIATEUR DES DEFORMATIONS
!
    do 2 i = 1, ndimsi
        epsd(i) = eps(i) - treps*kron(i)/(3.0d0)
 2  end do
!
    epseps = ddot(ndimsi,eps,1,eps,1)
    w = 0.5d0 * (lambda*treps**2 + deuxmu*epseps)
    do 5 ij = 1, ndimsi
        sigel(ij) = lambda*treps*kron(ij) + deuxmu*eps(ij)
 5  end do
!
!     CORRECTION 1 DE LA DERIVEE PAR RAPPORT A D EN COMPRESSION
!
    if (treps .lt. 0.d0) then
        w = 0.5d0 * deuxmu*ddot(ndimsi,epsd,1,epsd,1)
    endif
!
!     -----------------------------------------------------------------
!     CALCUL DE L'ENDOMMAGEMENT
!     -----------------------------------------------------------------
!
    fd = (1.d0 - phi)**2 + rigmin
!
    if (.not.resi) goto 5000
!
    vip(1) = phi
!
    if (vip(1) .gt. vim(1)) then
        vip(2) = 1
    else
        vip(2) = 0
    endif
!
!     STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES
!
!
!     FORMULATION LOI DE COMPORTMENT AVEC CORRECTION EN COMPRESSION
!
    if (treps .lt. 0.d0) then
        do 20 ij = 1, ndimsi
            sig(ij) = kk*treps*kron(ij) + deuxmu*epsd(ij)*fd
20      continue
    else
        do 10 ij = 1, ndimsi
            sig(ij) = sigel(ij)*fd
10      continue
    endif
!
5000  continue
!
!     -----------------------------------------------------------------
!     CALCUL DES MATRICES TANGENTES
!     -----------------------------------------------------------------
!
    call r8inir(36*4, 0.d0, dsidep, 1)
!
    fd = (1.d0 - phi)**2 + rigmin
!
!     -- CONTRIBUTION ELASTIQUE
!
    do 80 ij = 1, 3
        do 90 kl = 1, 3
            if (treps .lt. 0.d0) then
                dsidep(ij,kl,1) = lambda+deuxmu/(3.0d0)*(1.d0-fd)
            else
                dsidep(ij,kl,1) = fd*lambda
            endif
90      continue
80  end do
    do 100 ij = 1, ndimsi
        dsidep(ij,ij,1) = dsidep(ij,ij,1) + fd*deuxmu
100  end do
!
!     -- CORRECTION POUR LES CONTRAINTES PLANES
!
    if (cplan) then
        do 130 ij = 1, ndimsi
            if (ij .eq. 3) goto 130
            do 140 kl = 1, ndimsi
                if (kl .eq. 3) goto 140
                dsidep(ij,kl,1)=dsidep(ij,kl,1) - 1.d0/dsidep(3,3,1)*&
                dsidep(ij,3,1)*dsidep(3,kl,1)
140          continue
130      continue
    endif
!
!     -- CORRECTION DISSIPATIVE
!
!     CORRECTION 2 DE LA DERIVEE PAR RAPPORT A D EN COMPRESSION
!
    if (treps .lt. 0.d0) then
        do 220 ij = 1, ndimsi
            sigel(ij) = deuxmu*epsd(ij)
220      continue
    endif
!
!     DERIVEES CROISEES
!
    do 200 ij = 1, ndimsi
        dsidep(ij,1,2) = -2.d0*(1.0d0-phi)*sigel(ij)
200  end do
!
!     DERIVEE SECONDE /ENDO
!
    dsidep(1,1,3) = 2.0d0*w
!
!    DERIVEE PREMIERE /ENDO
!
    dsidep(1,1,4) = 2.0d0*(w0-(1.0d0-phi)*w)
!
!
!
end subroutine
