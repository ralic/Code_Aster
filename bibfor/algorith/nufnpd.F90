subroutine nufnpd(ndim, nno1, nno2, npg, iw,&
                  vff1, vff2, idff1, vu, vp,&
                  typmod, mate, compor, geomi, sig,&
                  ddl, mini, vect)
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
! aslint: disable=W1306
    implicit none
!
    include 'asterfort/calkbb.h'
    include 'asterfort/calkbp.h'
    include 'asterfort/calkce.h'
    include 'asterfort/dfdmip.h'
    include 'asterfort/nmepsi.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tanbul.h'
    include 'blas/ddot.h'
    logical :: mini
    integer :: ndim, nno1, nno2, npg, iw, idff1
    integer :: mate
    integer :: vu(3, 27), vp(27)
    real(kind=8) :: geomi(ndim, nno1)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg)
    real(kind=8) :: sig(2*ndim+1, npg), ddl(*), vect(*)
    character(len=8) :: typmod(*)
    character(len=16) :: compor(4)
!-----------------------------------------------------------------------
!          CALCUL DES FORCES NODALES POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES PETITES DEFORMATIONS
!          3D/D_PLAN/AXIS
!          ROUTINE APPELEE PAR TE0591
!-----------------------------------------------------------------------
! IN  MINI    : STABILISATION BULLE - MINI ELEMENT
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO1    : NOMBRE DE NOEUDS DE L'ELEMENT LIES AUX DEPLACEMENTS
! IN  NNO2    : NOMBRE DE NOEUDS DE L'ELEMENT LIES A LA PRESSION
! IN  NPG     : NOMBRE DE POINTS DE GAUSS
! IN  IW      : POIDS DES POINTS DE GAUSS
! IN  VFF1    : VALEUR  DES FONCTIONS DE FORME LIES AUX DEPLACEMENTS
! IN  VFF2    : VALEUR  DES FONCTIONS DE FORME LIES A LA PRESSION
! IN  IDFF1   : DERIVEE DES FONCTIONS DE FORME ELEMENT DE REFERENCE
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VP      : TABLEAU DES INDICES DES DDL DE PRESSION
! IN  GEOMI   : COORDONEES DES NOEUDS
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  MATE    : MATERIAU CODE
! IN  COMPOR  : COMPORTEMENT
! IN  DDL     : DEGRES DE LIBERTE A L'INSTANT PRECEDENT
! IN  SIG     : CONTRAINTES A L'INSTANT PRECEDENT
! OUT VECT    : FORCES INTERNES
!-----------------------------------------------------------------------
!
    logical :: axi, grand
    integer :: nddl, g
    integer :: sa, na, ia, kk
    real(kind=8) :: deplm(3*27)
    real(kind=8) :: presm(27), presd(27), pm
    real(kind=8) :: dff1(nno1, ndim)
    real(kind=8) :: fm(3, 3)
    real(kind=8) :: r, w
    real(kind=8) :: rac2, def(2*ndim, nno1, ndim)
    real(kind=8) :: epsm(6), sigma(6)
    real(kind=8) :: divum
    real(kind=8) :: t1, t2
    real(kind=8) :: alpha, trepst
    real(kind=8) :: dsbdep(2*ndim, 2*ndim), kbb(ndim, ndim), kbp(ndim, nno2)
    real(kind=8) :: kce(nno2, nno2), rce(nno2)
    character(len=16) :: option
!
    parameter    (grand = .false.)
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2
    rac2 = sqrt(2.d0)
    option = 'FORC_NODA       '
!
    call r8inir(nddl, 0.d0, vect, 1)
!
! - EXTRACTION DES CHAMPS
    do 10 na = 1, nno1
        do 20 ia = 1, ndim
            deplm(ia+ndim*(na-1)) = ddl(vu(ia,na))
20      continue
10  end do
!
    do 40 sa = 1, nno2
        presm(sa) = ddl(vp(sa))
        presd(sa) = 0.d0
40  end do
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
    do 1000 g = 1, npg
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
        call r8inir(6, 0.d0, epsm, 1)
        call dfdmip(ndim, nno1, axi, geomi, g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1, g),&
                    r, dff1, deplm, fm, epsm)
!
        divum = epsm(1) + epsm(2) + epsm(3)
!
! - CALCUL DE LA MATRICE B EPS_ij=B_ijkl U_kl
! - DEF (XX,YY,ZZ,2/RAC(2)XY,2/RAC(2)XZ,2/RAC(2)YZ)
        if (ndim .eq. 2) then
            do 35 na = 1, nno1
                do 45 ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= 0.d0
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(&
                    na,1))/rac2
45              continue
35          continue
!
! - TERME DE CORRECTION (3,3) AXI QUI PORTE EN FAIT SUR LE DDL 1
            if (axi) then
                do 47 na = 1, nno1
                    def(3,na,1) = fm(3,3)*vff1(na,g)/r
47              continue
            endif
        else
            do 36 na = 1, nno1
                do 46 ia = 1, ndim
                    def(1,na,ia)= fm(ia,1)*dff1(na,1)
                    def(2,na,ia)= fm(ia,2)*dff1(na,2)
                    def(3,na,ia)= fm(ia,3)*dff1(na,3)
                    def(4,na,ia)=(fm(ia,1)*dff1(na,2)+fm(ia,2)*dff1(&
                    na,1))/rac2
                    def(5,na,ia)=(fm(ia,1)*dff1(na,3)+fm(ia,3)*dff1(&
                    na,1))/rac2
                    def(6,na,ia)=(fm(ia,2)*dff1(na,3)+fm(ia,3)*dff1(&
                    na,2))/rac2
46              continue
36          continue
        endif
!
! - CALCUL DE LA PRESSION
        pm = ddot(nno2,vff2(1,g),1,presm,1)
!
! - CALCUL DES CONTRAINTES MECANIQUES A L'EQUILIBRE
        do 50 ia = 1, 3
            sigma(ia) = sig(ia,g)
50      continue
        do 65 ia = 4, 2*ndim
            sigma(ia) = sig(ia,g)*rac2
65      continue
!
! - CALCUL DE LA MATRICE D'ELASTICITE BULLE
        call tanbul(option, ndim, g, mate, compor,&
                    .false., mini, alpha, dsbdep, trepst)
!
! - CALCUL DE LA MATRICE DE CONDENSATION STATIQUE
        if (mini) then
            call calkbb(nno1, ndim, w, def, dsbdep,&
                        kbb)
            call calkbp(nno2, ndim, w, dff1, kbp)
            call calkce(nno1, ndim, kbp, kbb, presm,&
                        presd, kce, rce)
        else
            call r8inir(nno2, 0.d0, rce, 1)
        endif
!
! - VECTEUR FINT:U
        do 300 na = 1, nno1
            do 310 ia = 1, ndim
                kk = vu(ia,na)
                t1 = ddot(2*ndim, sigma,1, def(1,na,ia),1)
                vect(kk) = vect(kk) + w*t1
310          continue
300      continue
!
! - VECTEUR FINT:P
        t2 = (divum-pm*alpha)
        do 370 sa = 1, nno2
            kk = vp(sa)
            t1 = vff2(sa,g)*t2
            vect(kk) = vect(kk) + w*t1 - rce(sa)
370      continue
1000  end do
!
end subroutine
