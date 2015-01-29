subroutine xmmab0(ndim, nnc, jnne,&
                  hpg, ffc, jacobi, lpenac,&
                  tau1, tau2, jddle,&
                  nfhe, lmulti, heavno, mmat)
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
    implicit none
#include "asterf_types.h"
#include "asterfort/xplma2.h"
    integer :: ndim, nnc, jnne(3), jddle(2)
!
    real(kind=8) :: hpg, ffc(8), jacobi
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: mmat(336, 336)
    integer :: nfhe, heavno(8)
    aster_logical :: lpenac, lmulti
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE XFEMGG - CALCUL ELEM.)
!
! CALCUL DE F POUR LE CONTACT METHODE CONTINUE
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
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : DEUXIEME VECTEUR TANGENT
! IN  NDDLSE : NOMBRE DE DDLS D'UN NOEUD SOMMET ESCLAVE
! I/O MMAT   : MATRICE ELEMENTAIRE DE CONTACT/FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: i, j, k, l, ii, jj, pli, plj
    integer :: nne, nnes, ddles
    real(kind=8) :: tt(2, 2)
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
    nne=jnne(1)
    nnes=jnne(2)
    ddles=jddle(1)
!
    do 300 i = 1, 2
        do 290 j = 1, 2
            tt(i,j) = 0.d0
290     continue
300 continue
!
! --- MATRICE
!
    do 301 i = 1, ndim
        tt(1,1) = tau1(i)*tau1(i) + tt(1,1)
        tt(1,2) = tau1(i)*tau2(i) + tt(1,2)
        tt(2,1) = tau2(i)*tau1(i) + tt(2,1)
        tt(2,2) = tau2(i)*tau2(i) + tt(2,2)
301 continue
!
    do 284 i = 1, nnc
        do 283 j = 1, nnc
            call xplma2(ndim, nne, nnes, ddles, i,&
                        nfhe, pli)
            if (lmulti) pli = pli + (heavno(i)-1)*ndim
            call xplma2(ndim, nne, nnes, ddles, j,&
                        nfhe, plj)
            if (lmulti) plj = plj + (heavno(j)-1)*ndim
            do 282 l = 1, ndim-1
                do 281 k = 1, ndim-1
                    ii = pli+l
                    jj = plj+k
                    if (lpenac) then
                        mmat(ii,jj) = hpg*ffc(i)*ffc(j)* jacobi*tt(l, k)
                    else
                        mmat(ii,jj) = hpg*ffc(i)*ffc(j)* jacobi*tt(l, k)
                    endif
281             continue
282         continue
283     continue
284 continue
!
end subroutine
