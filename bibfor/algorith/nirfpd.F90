subroutine nirfpd(ndim, nno1, nno2, nno3, npg,&
                  iw, vff1, vff2, vff3, idff1,&
                  vu, vg, vp, typmod, geomi,&
                  sigref, epsref, vect)
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
! person_in_charge: sebastien.fayolle at edf.fr
!
! aslint: disable=W1306
    implicit none
    include 'asterfort/dfdmip.h'
    include 'asterfort/r8inir.h'
    include 'blas/ddot.h'
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1
    integer :: vu(3, 27), vg(27), vp(27)
    real(kind=8) :: geomi(ndim, nno1)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), vff3(nno3, npg)
    real(kind=8) :: sigref, epsref
    real(kind=8) :: vect(*)
    character(len=8) :: typmod(*)
!
!-----------------------------------------------------------------------
!          CALCUL DE REFE_FORC_NODA POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES GRANDES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0591
!-----------------------------------------------------------------------
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AU GONFLEMENT
! IN  NNO3    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES AU GONFLEMENT
! IN  VFF3    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  IDFF2   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VG      : TABLEAU DES INDICES DES DDL DE GONFLEMENT
! IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! IN  GEOMI   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  SIGREF  : CONTRAINTE DE REFERENCE
! IN  EPSREF  : DEFORMATION DE REFERENCE
! OUT VECT    : REFE_FORC_NODA
!-----------------------------------------------------------------------
!
    logical :: axi
    integer :: nddl, g
    integer :: kl, sa, ra, na, ia, ja, kk
    integer :: ndimsi
    real(kind=8) :: r, w, sigma(6)
    real(kind=8) :: rac2
    real(kind=8) :: f(3, 3)
    real(kind=8) :: def(6, nno1, ndim)
    real(kind=8) :: t1, dff1(nno1, 4)
!
    data         f    / 1.d0, 0.d0, 0.d0,&
     &                    0.d0, 1.d0, 0.d0,&
     &                    0.d0, 0.d0, 1.d0 /
!-----------------------------------------------------------------------
!
! - INITIALISATION
!
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3
    rac2 = sqrt(2.d0)
    ndimsi = 2*ndim
!
    call r8inir(nddl, 0.d0, vect, 1)
!
    do 1000 g = 1, npg
!
        call dfdmip(ndim, nno1, axi, geomi, g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
!
! - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
        if (ndim .eq. 2) then
            do 35 na = 1, nno1
                do 45 ia = 1, ndim
                    def(1,na,ia)= f(ia,1)*dff1(na,1)
                    def(2,na,ia)= f(ia,2)*dff1(na,2)
                    def(3,na,ia)= 0.d0
                    def(4,na,ia)=(f(ia,1)*dff1(na,2)+f(ia,2)*dff1(na,&
                    1))/rac2
45              continue
35          continue
        else
            do 36 na = 1, nno1
                do 46 ia = 1, ndim
                    def(1,na,ia)= f(ia,1)*dff1(na,1)
                    def(2,na,ia)= f(ia,2)*dff1(na,2)
                    def(3,na,ia)= f(ia,3)*dff1(na,3)
                    def(4,na,ia)=(f(ia,1)*dff1(na,2)+f(ia,2)*dff1(na,&
                    1))/rac2
                    def(5,na,ia)=(f(ia,1)*dff1(na,3)+f(ia,3)*dff1(na,&
                    1))/rac2
                    def(6,na,ia)=(f(ia,2)*dff1(na,3)+f(ia,3)*dff1(na,&
                    2))/rac2
46              continue
36          continue
        endif
!
! - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
        if (axi) then
            do 47 na = 1, nno1
                def(3,na,1) = f(3,3)*vff1(na,g)/r
47          continue
        endif
!
! - VECTEUR FINT:U
        do 10 kl = 1, ndimsi
            call r8inir(6, 0.d0, sigma, 1)
            sigma(kl) = sigref
            do 300 na = 1, nno1
                do 310 ia = 1, ndim
                    kk = vu(ia,na)
                    t1 = ddot(2*ndim, sigma,1, def(1,na,ia),1)
                    vect(kk) = vect(kk) + abs(w*t1)/ndimsi
310              continue
300          continue
10      continue
!
! - VECTEUR FINT:G
        do 350 ra = 1, nno2
            kk = vg(ra)
            t1 = vff2(ra,g)*sigref
            vect(kk) = vect(kk) + abs(w*t1)
350      continue
!
! - VECTEUR FINT:P
        do 370 sa = 1, nno3
            kk = vp(sa)
            t1 = vff3(sa,g)*epsref
            vect(kk) = vect(kk) + abs(w*t1)
370      continue
!
1000  end do
end subroutine
