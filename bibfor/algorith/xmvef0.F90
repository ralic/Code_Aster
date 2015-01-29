subroutine xmvef0(ndim, jnne, nnc,&
                  hpg, ffc, jacobi, lpenac,&
                  dlagrf, tau1, tau2,&
                  jddle, nfhe, lmulti, heavno,&
                  vtmp)
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
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/xplma2.h"
    integer :: ndim, nnc, jnne(3), jddle(2)
    real(kind=8) :: hpg, ffc(9), jacobi
    real(kind=8) :: dlagrf(2)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: vtmp(336)
    integer :: nfhe, heavno(8)
    aster_logical :: lpenac, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DU SECOND MEMBRE POUR LE FROTTEMENT
! CAS SANS CONTACT (XFEM)
!
! ----------------------------------------------------------------------
! ROUTINE SPECIFIQUE A L'APPROCHE <<GRANDS GLISSEMENTS AVEC XFEM>>,
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  HPG    : POIDS DU POINT INTEGRATION DU POINT DE CONTACT
! IN  FFC    : FONCTIONS DE FORME DU POINT DE CONTACT
! IN  JACOBI : JACOBIEN DE LA MAILLE AU POINT DE CONTACT
! IN  LAMBD  : LAGRANGE DE CONTACT ET FROTTEMENT AU POINT D'INTÉGRATION
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : DEUXIEME VECTEUR TANGENT
! IN  DDLES : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! I/O VTMP   : VECTEUR SECOND MEMBRE ELEMENTAIRE DE CONTACT/FROTTEMENT
! ----------------------------------------------------------------------
!
    integer :: i, l, ii, pl, nne, nnes, ddles
    real(kind=8) :: tt(3), t
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    nne=jnne(1)
    nnes=jnne(2)
    ddles=jddle(1)
!
    do 100 i = 1, 2
        tt(i) = 0.d0
100 continue
!
! --- CALCUL DE T.T
!
    do 200 i = 1, ndim
        t = dlagrf(1)*tau1(i)+dlagrf(2)*tau2(i)
        tt(1) = t*tau1(i)+tt(1)
        if (ndim .eq. 3) tt(2) = t*tau2(i)+tt(2)
200 continue
!
! --------------------- CALCUL DE {L3_FROT}----------------------------
!
    do 500 i = 1, nnc
        call xplma2(ndim, nne, nnes, ddles, i,&
                    nfhe, pl)
        if (lmulti) pl = pl + (heavno(i)-1)*ndim
        do 600 l = 1, ndim-1
            ii = pl+l
            if (lpenac) then
                vtmp(ii)= jacobi*hpg*ffc(i)*tt(l)
            else
                vtmp(ii)= jacobi*hpg*ffc(i)*tt(l)
            endif
600     continue
500 continue
!
end subroutine
