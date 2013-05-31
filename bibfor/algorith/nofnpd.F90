subroutine nofnpd(ndim, nno1, nno2, nno3, npg,&
                  iw, vff1, vff2, vff3, idff1,&
                  vu, vp, vpi, typmod, mate,&
                  compor, geomi, nomte, sig, ddl,&
                  vect)
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
! aslint: disable=W1306,W1504
    implicit none
!
    include 'asterfort/dfdmip.h'
    include 'asterfort/nmepsi.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tanbul.h'
    include 'asterfort/uthk.h'
    include 'blas/ddot.h'
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1
    integer :: mate
    integer :: vu(3, 27), vp(27), vpi(3, 27)
    real(kind=8) :: geomi(ndim, nno1)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), vff3(nno3, npg)
    real(kind=8) :: sig(2*ndim+1, npg), ddl(*), vect(*)
    character(len=8) :: typmod(*)
    character(len=16) :: compor(*), nomte
!-----------------------------------------------------------------------
!          CALCUL DES FORCES NODALES POUR LES ELEMENTS
!          INCOMPRESSIBLES POUR LES PETITES DEFORMATIONS
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
! IN  VU      : TABLEAU DES INDICES DES DDL DE DEPLACEMENTS
! IN  VG      : TABLEAU DES INDICES DES DDL DE GONFLEMENT
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
    integer :: ia, na, ra, sa, kk
    integer :: ibid
    real(kind=8) :: deplm(3*27), r
    real(kind=8) :: presm(27), pm, gpm(ndim), pim(ndim)
    real(kind=8) :: gpresm(3*27)
    real(kind=8) :: dff1(nno1, ndim)
    real(kind=8) :: fm(3, 3)
    real(kind=8) :: w
    real(kind=8) :: rac2
    real(kind=8) :: def(2*ndim, nno1, ndim)
    real(kind=8) :: epsm(6), sigma(6)
    real(kind=8) :: divum
    real(kind=8) :: t1, t2
    real(kind=8) :: alpha, trepst
    real(kind=8) :: dsbdep(2*ndim, 2*ndim)
    real(kind=8) :: stab, hk
    character(len=16) :: option
!
    parameter    (grand = .false.)
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3*ndim
    rac2 = sqrt(2.d0)
    option = 'FORC_NODA       '
!
    call uthk(nomte, geomi, hk, ndim, ibid,&
              ibid, ibid, ibid, 1, ibid)
    stab = 1.d-4*hk*hk
!
    call r8inir(nddl, 0.d0, vect, 1)
!
! - EXTRACTION DES CHAMPS
    do 10 na = 1, nno1
        do 11 ia = 1, ndim
            deplm(ia+ndim*(na-1)) = ddl(vu(ia,na))
11      continue
10  end do
!
    do 40 sa = 1, nno2
        presm(sa) = ddl(vp(sa))
40  end do
!
    do 31 ra = 1, nno3
        do 32 ia = 1, ndim
            gpresm(ia+ndim*(ra-1)) = ddl(vpi(ia,ra))
32      continue
31  end do
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
! - CALCUL DE LA PRESSION
        pm = ddot(nno2,vff2(1,g),1,presm,1)
!
! - CALCUL DU GRADIENT DE PRESSION ET DU GRADIENT DE PRESSION PROJETE
        do 20 ia = 1, ndim
            pim(ia) = ddot(nno3,vff3(1,g),1,gpresm(ia),ndim)
            gpm(ia) = ddot(nno2,dff1(1,ia),1,presm,1)
20      continue
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
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
! - CALCUL DES CONTRAINTES MECANIQUES A L'EQUILIBRE
        do 50 ia = 1, 3
            sigma(ia) = sig(ia,g)
50      continue
        do 65 ia = 4, 2*ndim
            sigma(ia) = sig(ia,g)*rac2
65      continue
!
! - CALCUL DE L'INVERSE DE KAPPA
        call tanbul(option, ndim, g, mate, compor,&
                    .false., .false., alpha, dsbdep, trepst)
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
            t1 = 0.d0
! - PRODUIT SCALAIRE DE GRAD FONC DE FORME DE P ET GRAD P OU FONC DE PI
            do 375 ia = 1, ndim
                t1 = t1 + dff1(sa,ia)*(gpm(ia)-pim(ia))
375          continue
            t1 = vff2(sa,g)*t2 - stab*t1
            vect(kk) = vect(kk) + w*t1
370      continue
!
! - VECTEUR FINT:PI
        do 380 ra = 1, nno3
            do 385 ia = 1, ndim
                kk = vpi(ia,ra)
                t1 = stab*vff3(ra,g)*(gpm(ia)-pim(ia))
                vect(kk) = vect(kk) + w*t1
385          continue
380      continue
1000  end do
!
end subroutine
