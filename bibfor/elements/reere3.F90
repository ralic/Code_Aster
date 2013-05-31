subroutine reere3(elrefp, nnop, igeom, xg, depl,&
                  grand, ndim, he, fisno, nfiss,&
                  nfh, nfe, ddlt, fe, dgdgl,&
                  cinem, xe, ff, dfdi, f,&
                  eps, grad)
!
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
! aslint: disable=W1504
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/elrfdf.h'
    include 'asterfort/elrfvf.h'
    include 'asterfort/invjax.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/matini.h'
    include 'asterfort/reereg.h'
    include 'asterfort/vecini.h'
    character(len=3) :: cinem
    character(len=8) :: elrefp
    integer :: igeom, nnop, ndim, nfh, nfe, ddlt
    integer :: nfiss, fisno(nnop, nfiss)
    real(kind=8) :: xg(ndim), depl(ddlt, nnop), fe(4), dgdgl(4, ndim)
    real(kind=8) :: xe(ndim), ff(nnop), dfdi(nnop, ndim), f(3, 3)
    real(kind=8) :: eps(6), grad(ndim, ndim), he(nfiss)
    logical :: grand
!
!......................................................................
! TROUVER LES COORDONNEES DANS L'ELEMENT DE REFERENCE D'UN
! POINT DONNE DANS L'ELEMENT REEL PAR LA METHODE NEWTON
! ET CALCUL DES ELEMENTS CINEMATIQUES
! (MEME ROUTINE QUE REEREF A RESORBER LORS DU 3D QUADRATIQUE XFEM)
!......................................................................
!
! IN  ELREFP : TYPE DE L'ELEMENT DE REF PARENT
! IN  NNOP   : NOMBRE DE NOEUDS DE L'ELT DE RÉF PARENT
!   L'ORDRE DES DDLS DOIT ETRE 'DC' 'H1' 'E1' 'E2' 'E3' 'E4' 'LAGC'
! IN  IGEOM  : COORDONNEES DES NOEUDS
! IN  XG     : COORDONNES DU POINT DANS L'ELEMENT REEL
! IN  DEPL   : DEPLACEMENT RÉEL À PARTIR DE LA CONF DE REF
! IN  GRAND  : INDICATEUR SI GRANDES TRANSFORMATIONS
!              SI GRAND = .FALSE.
!                --> MATRICE F: UNITE
!                --> DEFORMATION EPS PETITES
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  HE     : VALEUR DE LA FONCTION HEAVISIDE SUR LE SOUS-ÉLT
! IN  DDLH   : NOMBRE DE DDL HEAVYSIDE (PAR NOEUD)
! IN  NFE    : NOMBRE DE FONCTIONS SINGULIÈRES D'ENRICHISSEMENT
! IN  DDLT   : NOMBRE DE DDLS TOTAL PAR NOEUD
! IN  FE     : VALEURS AUX NOEUDS DES FONCTIONS D'ENRICHISSEMENT
! IN  DGDGL  : DÉRIVÉES DES FONCTIONS D'ENRICHISSEMENT
! IN  CINEM  : CALCUL DES QUANTITÉS CINÉMATIQUES
!               'NON' : ON S'ARRETE APRES LE CALCUL DES FF
!               'DFF' : ON S'ARRETE APRES LE CALCUL DES DERIVEES DES FF
!               'OUI' : ON VA JUSQU'AU BOUT
! OUT XE     : COORDONNÉES DU POINT DANS L'ÉLÉMENT DE RÉF PARENT
! OUT FF     : FONCTIONS DE FORMES EN XE
! OUT DFDI   : DÉRIVÉES DES FONCTIONS DE FORMES EN XE
! OUT F      : GRADIENT DE LA TRANSFORMATION
! OUT EPS    : DÉFORMATIONS
! OUT GRAD   : GRADIENT DES DÉPLACEMENTS
!
!
!
!
    integer :: nbnomx
    parameter   (nbnomx = 27)
!
    real(kind=8) :: zero, un, rac2
    integer :: i, j, k, n, p, ig, cpt
    integer :: nno, nderiv, iret
    real(kind=8) :: invjac(3, 3)
    real(kind=8) :: dff(3, nbnomx)
    real(kind=8) :: kron(3, 3), tmp, epstab(3, 3)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call assert(cinem.eq.'NON'.or.cinem.eq.'OUI'.or.cinem.eq.'DFF')
    zero = 0.d0
    un = 1.d0
    rac2 = sqrt(2.d0)
!
! --- RECHERCHE DE XE PAR NEWTON-RAPHSON
!
    call reereg('S', elrefp, nnop, zr(igeom), xg,&
                ndim, xe, iret)
!
! --- VALEURS DES FONCTIONS DE FORME EN XE: FF
!
    call elrfvf(elrefp, xe, nbnomx, ff, nno)
!
! --- DERIVEES PREMIERES DES FONCTIONS DE FORME EN XE: DFF
!
    call elrfdf(elrefp, xe, ndim*nbnomx, dff, nno,&
                nderiv)
!
! --- CALCUL DE L'INVERSE DE LA JACOBIENNE EN XE: INVJAC
!
    call invjax('S', nno, ndim, nderiv, dff,&
                zr(igeom), invjac, iret)
!
    if (cinem .eq. 'NON') goto 9999
!
! --- DERIVEES DES FONCTIONS DE FORMES CLASSIQUES EN XE : DFDI
!
    call matini(nnop, ndim, zero, dfdi)
    do 300 n = 1, nno
        do 310 i = 1, ndim
            do 311 k = 1, ndim
                dfdi(n,i)= dfdi(n,i) + invjac(k,i)*dff(k,n)
311          continue
310      continue
300  end do
!
    if (cinem .eq. 'DFF') goto 9999
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
    do 400 i = 1, 3
        do 401 j = 1, 3
            f(i,j) = kron(i,j)
401      continue
400  end do
!
    do 399 i = 1, ndim
        do 398 j = 1, ndim
            grad(i,j) = zero
398      continue
399  end do
!
! --- L'ORDRE DES DDLS DOIT ETRE 'DC' 'H1' 'E1' 'E2' 'E3' 'E4' 'LAGC'
!
    do 402 n = 1, nno
        cpt=0
! -- DDLS CLASSIQUES
        do 403 i = 1, ndim
            cpt = cpt+1
            do 404 j = 1, ndim
                grad(i,j) = grad(i,j) + dfdi(n,j) * depl(cpt,n)
404          continue
403      continue
! -- DDLS HEAVISIDE
        do 405 ig = 1, nfh
            do 406 i = 1, ndim
                cpt = cpt+1
                do 407 j = 1, ndim
                    grad(i,j)=grad(i,j)+he(fisno(n,ig))*dfdi(n,j)*&
                    depl(cpt,n)
407              continue
406          continue
405      continue
! -- DDL ENRICHIS EN FOND DE FISSURE
        do 408 ig = 1, nfe
            do 409 i = 1, ndim
                cpt = cpt+1
                do 410 j = 1, ndim
                    grad(i,j) = grad(i,j) + depl(cpt,n) * (dfdi(n,j) * fe(ig) + ff(n) * dgdgl(ig,&
                                &j))
410              continue
409          continue
408      continue
402  end do
    if (grand) then
        do 420 i = 1, ndim
            do 421 j = 1, ndim
                f(i,j) = f(i,j) + grad(i,j)
421          continue
420      continue
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
    endif
!
9999  continue
!
    call jedema()
end subroutine
