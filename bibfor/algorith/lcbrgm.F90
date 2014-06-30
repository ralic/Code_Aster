subroutine lcbrgm(ndim, typmod, imate, epsm, deps,&
                  vim, option, sig, vip, dsidpt,&
                  proj, cdrett)
!
! ======================================================================
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
! ======================================================================
! LOI DE COMPORTEMENT ELASTIQUE ENDO HETEROGENE
! (AVEC REGULARIS. DES CONTRAINTES)
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
!                 2   -> ELASTIQUE (0) OU POINTE (1) RUPT AMORCAGE (2)
!                     -> RUPT PROPAGATION (3)
!                 3   -> CONTRAINTE RUPT AMORCAGE
!                 4   -> CONTRAINTE RUPT PROPAGATION
!                 5   -> NUMERO ELEMENT POINTE 1
!                 6   -> NUMERO ELEMENT POINTE 2 (SI RUPT AMORCAGE)
!                 7   -> IT DE NEWTON DE RUPTURE
!                 8   -> IT DE NEWTON COURANTE
!                 9   -> COORX POINTE DE FISSURE (APRES RUPT PROPA)
!                 10  -> COORY POINTE DE FISSURE (APRES RUPT PROPA)
! OUT DSIDPT  : MATRICE TANGENTE
! OUT PROJ    : NE SERT PLUS A RIEN
! ----------------------------------------------------------------------
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    character(len=8) :: typmod(*)
    character(len=16) :: option
    integer :: ndim, imate, cdrett
    real(kind=8) :: epsm(12), deps(12), vim(*)
    real(kind=8) :: sig(6), vip(*), dsidpt(6, 6, 2), proj(6, 6)
! ----------------------------------------------------------------------
!
!
!
!
    logical(kind=1) :: cplan, resi, rigi
    integer :: ndimsi, k, l, etat
!
    real(kind=8) :: eps(6), epsr(6), treps, sigel(6)
    real(kind=8) :: kron(6)
    real(kind=8) :: fd, d, dm, e, nu, lambda, deuxmu
!
    integer :: icodre(2)
    character(len=8) :: nomres(2)
    real(kind=8) :: valres(2)
!
    real(kind=8) :: dmax
    parameter  (dmax = 0.999999d0)
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
!
! ======================================================================
!                            INITIALISATION
! ======================================================================
!
! -- OPTION ET MODELISATION
!
!
    resi = option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
!
    cplan = (typmod(1).eq.'C_PLAN  ')
    ndimsi = 2*ndim
!
! -- COUPURE ISOTROPE DE LA REGULARISATION SI ENDOMMAGEMENT SATURE :
! A PREVOIR UN PROJECTEUR EVENTUEL
!
    if (nint(proj(1,1)) .eq. 1) then
        cdrett=nint(proj(1,1))
    endif
! -- LECTURE DES CARACTERISTIQUES ELASTIQUES
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    call rcvala(imate, ' ', 'ELAS', 0, ' ',&
                [0.d0], 2, nomres, valres, icodre, 1)
!
    e = valres(1)
    nu = valres(2)
    lambda = e * nu / (1.d0+nu) / (1.d0 - 2.d0*nu)
    deuxmu = e/(1.d0+nu)
!
! -- DEFORMATIONS
!
    call dcopy(ndimsi, epsm, 1, eps, 1)
    call dcopy(ndimsi, epsm(7), 1, epsr, 1)
    if (resi) then
        call daxpy(ndimsi, 1.d0, deps, 1, eps,&
                   1)
        call daxpy(ndimsi, 1.d0, deps(7), 1, epsr,&
                   1)
    endif
    do 50 k = 1, ndimsi
        dsidpt(k,k,1) = 0.d0
50  continue
!
!
!
! ======================================================================
!                         CONTRAINTES ELASTIQUES
! ======================================================================
!
! -- CALCUL DES CONTRAINTES ELASTIQUES
!
    treps = eps(1)+eps(2)+eps(3)
    do 60 k = 1, ndimsi
        sigel(k) = lambda*treps*kron(k) + deuxmu*eps(k)
60  end do
!
! ======================================================================
!                 INTEGRATION DE LA LOI DE COMPORTEMENT
! ======================================================================
!
    if (resi) then
        dm = vim(1)
        etat = nint(vip(2))
        vip(3)= vim(3)
        vip(4)= vim(4)
!
        if (etat .eq. 3) then
            d = dmax
            etat = 3
        else if (etat.eq.2) then
            d = dmax
            etat = 2
        else if (etat.eq.1) then
            d = dm
            etat = 1
        else if (etat.eq.0) then
            d = dm
            etat = 0
        endif
!
        do 30 k = 1, ndimsi
            sig(k) = (1-d) * sigel(k)
30      continue
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
! ======================================================================
!                            MATRICE TANGENTE
! ======================================================================
!
    if (rigi) then
        call r8inir(72, 0.d0, dsidpt, 1)
        fd = 1.d0-d
        do 100 k = 1, 3
            do 110 l = 1, 3
                dsidpt(k,l,1) = fd*lambda
110          continue
100      continue
        do 120 k = 1, ndimsi
            dsidpt(k,k,1) = dsidpt(k,k,1) + fd*deuxmu
120      continue
!
        if (cplan) then
            do 300 k = 1, ndimsi
                if (k .eq. 3) goto 300
                do 310 l = 1, ndimsi
                    if (l .eq. 3) goto 310
                    dsidpt(k,l,1)=dsidpt(k,l,1) - 1.d0/dsidpt(3,3,1)*&
                    dsidpt(k,3,1)*dsidpt(3,l,1)
310              continue
300          continue
        endif
!
!
    endif
!
end subroutine
