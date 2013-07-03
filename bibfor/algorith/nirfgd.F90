subroutine nirfgd(ndim, nno1, nno2, nno3, npg,&
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
#include "asterfort/dfdmip.h"
#include "asterfort/nmmalu.h"
#include "asterfort/r8inir.h"
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
    integer :: nddl, ndu, g
    integer :: kl, sa, ra, na, ia, ja, kk
    integer :: ndimsi, vij(3, 3), lij(3, 3)
    real(kind=8) :: r, w, tau(6)
    real(kind=8) :: t1, dff1(nno1, 4)
!
    data         vij  / 1, 4, 5,&
     &                    4, 2, 6,&
     &                    5, 6, 3 /
!-----------------------------------------------------------------------
!
! - INITIALISATION
!
    axi = typmod(1).eq.'AXIS'
    nddl = nno1*ndim + nno2 + nno3
    ndu = ndim
    if (axi) ndu = 3
    ndimsi = 2*ndu
!
    call r8inir(nddl, 0.d0, vect, 1)
!
    do 1000 g = 1, npg
!
        call dfdmip(ndim, nno1, axi, geomi, g,&
                    iw, vff1(1, g), idff1, r, w,&
                    dff1)
        call nmmalu(nno1, axi, r, vff1(1, g), dff1,&
                    lij)
!
! - VECTEUR FINT:U
        do 10 kl = 1, ndimsi
            call r8inir(6, 0.d0, tau, 1)
            tau(kl) = sigref
            do 300 na = 1, nno1
                do 310 ia = 1, ndu
                    kk = vu(ia,na)
                    t1 = 0.d0
                    do 320 ja = 1, ndu
                        t1 = t1 + tau(vij(ia,ja))*dff1(na,lij(ia,ja))
320                  continue
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
