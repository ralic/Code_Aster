subroutine nifnlg(ndim, nno1, nno2, nno3, npg, iw, vff1, vff2, vff3, idff1,&
                  idff2, vu, vg, vp, typmod, mate, geomi, sig, ddl, vect)
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
#include "asterfort/dfdmip.h"
#include "asterfort/nirela.h"
#include "asterfort/nmepsi.h"
#include "asterfort/nmmalu.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvala.h"
#include "blas/dcopy.h"
#include "blas/ddot.h"
#include "blas/dscal.h"
    integer :: ndim, nno1, nno2, nno3, npg, iw, idff1
    integer :: idff2, mate
    integer :: vu(3, 27), vg(27), vp(27)
    real(kind=8) :: geomi(ndim, nno1)
    real(kind=8) :: vff1(nno1, npg), vff2(nno2, npg), vff3(nno3, npg)
    real(kind=8) :: sig(2*ndim, npg), ddl(*), vect(*)
    character(len=8) :: typmod(*)
!-----------------------------------------------------------------------
!          CALCUL DES FORCES NODALES POUR LES ELEMENTS
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
! IN  MATE    : MATERIAU CODE
! IN  DDL     : DEGRES DE LIBERTE A L'INSTANT PRECEDENT
! IN  SIG     : CONTRAINTES A L'INSTANT PRECEDENT
! OUT VECT    : FORCES INTERNES
!-----------------------------------------------------------------------
!
    logical :: axi, grand
    logical :: nonloc
    integer :: k2ret, vij(3, 3), lij(3, 3)
    integer :: nddl, ndu, g
    integer :: kl, sa, ra, na, ia, ja, kk
    real(kind=8) :: geomm(3*27), jm, wm, epsm(6)
    real(kind=8) :: deplm(3*27), gonfm(27), presm(27), gm, pm, r
    real(kind=8) :: dff1(nno1, 4), dff2(nno2, 3)
    real(kind=8) :: fm(3, 3)
    real(kind=8) :: w
    real(kind=8) :: tau(6), taudv(6), tauhy
    real(kind=8) :: gradgm(3), c
    real(kind=8) :: t1, t2
    real(kind=8) :: kr(6), id(3, 3)
    real(kind=8) :: am, ap, bm, boa, aa, bb, daa, dbb, dboa, d2boa
!
    parameter    (grand = .true.)
    data         vij  / 1, 4, 5,&
     &                  4, 2, 6,&
     &                  5, 6, 3 /
    data         kr   / 1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
    data         id   / 1.d0, 0.d0, 0.d0,&
     &                  0.d0, 1.d0, 0.d0,&
     &                  0.d0, 0.d0, 1.d0/
!-----------------------------------------------------------------------
!
! - INITIALISATION
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3
    ndu = ndim
    if (axi) ndu = 3
!
    call r8inir(nddl, 0.d0, vect, 1)
    call r8inir(6, 0.d0, tau, 1)
!
! - REACTUALISATION DE LA GEOMETRIE ET EXTRACTION DES CHAMPS
    do na = 1, nno1
        do ia = 1, ndim
            geomm(ia+ndim*(na-1)) = geomi(ia,na) + ddl(vu(ia,na))
            deplm(ia+ndim*(na-1)) = ddl(vu(ia,na))
        end do
    end do
!
    do ra = 1, nno2
        gonfm(ra) = ddl(vg(ra))
    end do
!
    do sa = 1, nno3
        presm(sa) = ddl(vp(sa))
    end do
!
! - CALCUL POUR CHAQUE POINT DE GAUSS
!
    do g = 1, npg
!
! - LONGUEUR CARACTERISTIQUE -> PARAMETRE C
        c=0.d0
        call rcvala(mate, ' ', 'NON_LOCAL', 0, ' ', 0.d0, 1, 'C_GONF', c, k2ret, 0)
        nonloc = k2ret.eq.0 .and. c.ne.0.d0
!
! - CALCUL DES ELEMENTS GEOMETRIQUES
        call dfdmip(ndim, nno1, axi, geomi, g, iw, vff1(1,g), idff1, r, w, dff1)
        call nmepsi(ndim, nno1, axi, grand, vff1(1,g), r, dff1, deplm, fm, epsm)
        call dfdmip(ndim, nno1, axi, geomm, g, iw, vff1(1,g), idff1, r, wm, dff1)
        call nmmalu(nno1, axi, r, vff1(1, g), dff1, lij)
!
        jm = fm(1,1)*(fm(2,2)*fm(3,3)-fm(2,3)*fm(3,2))&
           - fm(2,1)*(fm(1,2)*fm(3,3)-fm(1,3)*fm(3,2))&
           + fm(3,1)*(fm(1,2)*fm(2,3)-fm(1,3)*fm(2,2))
!
! - CALCUL DE LA PRESSION ET DU GONFLEMENT
        gm = ddot(nno2,vff2(1,g),1,gonfm,1)
        pm = ddot(nno3,vff3(1,g),1,presm,1)
!
! - CALCUL DU GRADIENT DU GONFLEMENT POUR LA REGULARISATION
        if (nonloc) then
            call dfdmip(ndim, nno2, axi, geomi, g, iw, vff2(1, g), idff2, r, w, dff2)
            do ia = 1, ndim
                gradgm(ia) = ddot(nno2,dff2(1,ia),1,gonfm,1)
            end do
        endif
!
! - CONTRAINTE DE KIRCHHOFF
        call dcopy(2*ndim, sig(1,g), 1, tau, 1)
        call dscal(2*ndim, jm, tau, 1)
        tauhy = (tau(1)+tau(2)+tau(3))/3.d0
        do kl = 1, 6
            taudv(kl) = tau(kl) - tauhy*kr(kl)
        end do
!
! - CALCUL DES FONCTIONS A,B,... QUI LIENT G ET J
        call nirela(2, jm, gm, gm, am, ap, bm, boa, aa, bb, daa, dbb, dboa, d2boa)
!
! - VECTEUR FINT:U
        do na = 1, nno1
            do ia = 1, ndu
                kk = vu(ia,na)
                t1 = 0.d0
                do ja = 1, ndu
                    t2 = taudv(vij(ia,ja)) + pm*bb*id(ia,ja)
                    t1 = t1 + t2*dff1(na,lij(ia,ja))
                end do
                vect(kk) = vect(kk) + w*t1
            end do
        end do
!
! - VECTEUR FINT:G
        t2 = tauhy*aa - pm*dboa
        do ra = 1, nno2
            kk = vg(ra)
            t1 = vff2(ra,g)*t2
            vect(kk) = vect(kk) + w*t1
        end do
!
        if (nonloc) then
            do ra = 1, nno2
                kk = vg(ra)
                t1 = c*ddot(ndim,gradgm,1,dff2(ra,1),nno2)
                vect(kk) = vect(kk) + w*t1
            end do
        endif
!
! - VECTEUR FINT:P
        t2 = bm - boa
        do sa = 1, nno3
            kk = vp(sa)
            t1 = vff3(sa,g)*t2
            vect(kk) = vect(kk) + w*t1
        end do
    end do
end subroutine
