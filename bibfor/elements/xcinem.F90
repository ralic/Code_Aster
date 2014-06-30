subroutine xcinem(axi, nnop, nnos, idepl, grand, ndim, he,&
                  r, ur, fisno, nfiss, nfh, nfe, ddls, ddlm,&
                  fe, dgdgl, ff, dfdi, f, eps, grad)
!
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
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/matini.h"
#include "asterfort/vecini.h"

    logical(kind=1), intent(in) :: axi
    integer, intent(in) :: nnop
    integer, intent(in) :: nnos
    integer, intent(in) :: idepl
    logical(kind=1), intent(in) :: grand
    integer, intent(in) :: ndim
    integer, intent(in) :: nfiss
    real(kind=8), intent(in) :: he(nfiss)
    real(kind=8), intent(in) :: r
    real(kind=8), intent(in) :: ur
    integer, intent(in) :: fisno(nnop, nfiss)
    integer, intent(in) :: nfh
    integer, intent(in) :: nfe
    integer, intent(in) :: ddls
    integer, intent(in) :: ddlm
    real(kind=8), intent(in) :: fe(4)
    real(kind=8), intent(in) :: dgdgl(4, ndim)
    real(kind=8), intent(in) :: ff(nnop)
    real(kind=8), intent(in) :: dfdi(nnop, ndim)
    real(kind=8), intent(out) :: f(3, 3)
    real(kind=8), intent(out) :: eps(6)
    real(kind=8), intent(out) :: grad(ndim, ndim)
!
! ----------------------------------------------------------------------
!
! X-FEM : CALCUL DES ELEMENTS CINEMATIQUES F, EPS ET GRAD CONNAISSANT 
!         FF ET DFDI (VALEURS DES FF CLASSIQUES ET DE LEUR DERIVEES DANS 
!         LE REPERE DE REFERENCE PREALABLEMENT CALCULEES AVEC REEREF)
!
! ----------------------------------------------------------------------
!
!
! IN   AXI   : INDIQUER POUR MODEL AXIS
! IN  NNOP   : NOMBRE DE NOEUDS DE L'ELT DE RÉF PARENT
!   L'ORDRE DES DDLS DOIT ETRE 'DC' 'H1' 'E1' 'E2' 'E3' 'E4' 'LAGC'
! IN  DEPL   : DEPLACEMENT RÉEL À PARTIR DE LA CONF DE REF
! IN  GRAND  : INDICATEUR SI GRANDES TRANSFORMATIONS
!              SI GRAND = .FALSE.
!                --> MATRICE F: UNITE
!                --> DEFORMATION EPS PETITES
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  HE     : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  R      : RADIUS POUR CALCULER EPSILON_33 POUR AXI
! IN UR      : DEPLACEMNET RADIAL POUR CALCULER EPSILON_33 POUR AXI
! IN  NFH    : NOMBRE DE FONCTIONS HEAVYSIDE (PAR NOEUD)
! IN  NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  DDLT   : NOMBRE DE DDLS TOTAL PAR NOEUD
! IN  FE     : VALEURS AUX NOEUDS DES FONCTIONS D'ENRICHISSEMENT
! IN  DGDGL  : DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT
! IN  FF     : FONCTIONS DE FORMES EN XE
! IN  DFDI   : DÉRIVÉES DES FONCTIONS DE FORMES EN XE
! OUT F      : GRADIENT DE LA TRANSFORMATION
! OUT EPS    : DÉFORMATIONS
! OUT GRAD   : GRADIENT DES DÉPLACEMENTS
!
    real(kind=8) :: zero, un, rac2
    integer :: i, j, k, n, p, ig, cpt, nn
    real(kind=8) :: kron(3, 3), tmp, epstab(3, 3)
    logical(kind=1) :: ldec
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    zero = 0.d0
    un = 1.d0
    rac2 = sqrt(2.d0)
!
! --- MATRICE IDENTITE
!
    call matini(3, 3, zero, kron)
    do 10 p = 1, 3
        kron(p,p) = un
10  end do
!
! --- CALCUL DES GRADIENTS : GRAD(U) ET F
!
    do 21 j = 1, 3
        do 20 i = 1, 3
            f(i,j) = kron(i,j)
20      continue
21  end do
!
    do 31 j = 1, ndim
        do 30 i = 1, ndim
            grad(i,j) = zero
30      continue
31  end do
!
    ldec=.false.
    if (ddlm .eq. 0 .or. ddlm .eq. -1 .or. ddlm .eq. ddls) ldec=.true.
!
! --- L'ORDRE DES DDLS DOIT ETRE 'DC' 'H1' 'E1' 'E2' 'E3' 'E4' 'LAGC'
!
    do 402 n = 1, nnop
        if (ldec) then
! --- DDLM=-1 PERMET D'EVITER D'AVOIR A FOURNIR DDLM DANS CHAQUE CAS
            nn=ddls*(n-1)
        else
            call indent(n, ddls, ddlm, nnos, nn)
        endif
!
        cpt=0
!
! -- DDLS CLASSIQUES
        do 403 i = 1, ndim
            cpt = cpt+1
            do 404 j = 1, ndim
                grad(i,j) = grad(i,j) + dfdi(n,j) * zr(idepl-1+nn+cpt)
404          continue
403      continue
!
! -- DDLS HEAVISIDE
        do 405 ig = 1, nfh
            do 406 i = 1, ndim
                cpt = cpt+1
                do 407 j = 1, ndim
                    grad(i,j) = grad(i,j) + he(fisno(n,ig)) * dfdi(n, j) * zr(idepl-1+nn+cpt)
407              continue
406          continue
405      continue
!
! -- DDL ENRICHIS EN FOND DE FISSURE
        do 408 ig = 1, nfe
            do 409 i = 1, ndim
                cpt = cpt+1
                do 410 j = 1, ndim
                    grad(i,j) = grad(i,j) + zr(idepl-1+nn+cpt) * (dfdi(n,j) * fe(ig) + ff(n) * dg&
                                &dgl(ig,j))
410              continue
409          continue
408      continue
402  end do
!
    if (grand) then
        do 421 j = 1, ndim
            do 420 i = 1, ndim
                f(i,j) = f(i,j) + grad(i,j)
420          continue
421      end do
    endif
!
! --- CALCUL DES DÉFORMATIONS : EPS
!
    do 430 i = 1, ndim
        do 431 j = 1, i
            tmp = grad(i,j) + grad(j,i)
            if (grand) then
                do 432 k = 1, ndim
                    tmp = tmp + grad(k,i)*grad(k,j)
432              continue
            endif
            epstab(i,j) = 0.5d0*tmp
431      continue
430  end do
    call vecini(6, zero, eps)
    eps(1) = epstab(1,1)
    eps(2) = epstab(2,2)
    eps(4) = epstab(2,1)*rac2
    if (ndim .eq. 3) then
        eps(3) = epstab(3,3)
        eps(5) = epstab(3,1)*rac2
        eps(6) = epstab(3,2)*rac2
    else if (axi) then
        eps(3) = ur/r
    endif
!
    call jedema()
end subroutine
