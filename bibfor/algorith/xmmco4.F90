subroutine xmmco4(ndim, nno, pla, nd, tau1,&
                  tau2, ffc, ddls, jac, ffp,&
                  nnol, ddlm, nnos, mmat)
    implicit none
#include "jeveux.h"
#include "asterfort/indent.h"
#include "asterfort/matini.h"
#include "asterfort/transp.h"
    integer :: ndim, nno, ddls, pla(27)
    integer :: nnol, ddlm, nnos
    real(kind=8) :: mmat(216, 216)
    real(kind=8) :: ffp(27), jac, ffc(8)
    real(kind=8) :: nd(3), tau1(3), tau2(3)
! ----------------------------------------------------------------------
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
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DES MATRICES DE COHESION, LOI CZM_LIN_MIX
! --- PARTIE INDEPENDANTE DE LA LOI D'INTERFACE
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  NNO    : NOMBRE DE NOEUDS DE L'ELEMENT DE REF PARENT
! IN  PLA    : PLACE DES LAGRANGES DANS LA NUMEROTATION
! IN  ND     : DIRECTION NORMALE
! IN  TAU1   : DIRECTION TANGENTE 1
! IN  TAU2   : DIRECTION TANGENTE 2
! IN  DDLS   : NOMBRE DE DDLS DES NOEUDS SOMMET
! IN  JAC    : PRODUIT DU JACOBIEN ET DU POIDS
! IN  FFP    : FONCTIONS DE FORME DE L'ELEMENT PARENT
! IN  NNOL   : NOMBRE DE NOEUDS AVEC INCONNUES DE LAGRANGE
! IN  DDLM   : NOMBRE DE DDLS DES NOEUDS MILIEU
! IN  NNOS   : NOMBRE DE NOEUDS SOMMET 
! I/O MMAT   : MATRICE ELEMENTAITRE DE COHESION
!
!
!
!
    integer :: i, j, k, l, pli, plj, jn
    real(kind=8) :: dside2(3, 3), ptr(3, 3), temp(3, 3), au(3, 3)
    real(kind=8) :: p(3, 3)
!
! ----------------------------------------------------------------------
!
!     INITIALISATION
! on reecrit tout
! matrices A et AT
    call matini(3, 3, 0.d0, au)
    call matini(3, 3, 0.d0, dside2)
    call matini(3, 3, 0.d0, temp)
    call matini(3, 3, 0.d0, ptr)
    call matini(3, 3, 0.d0, p)
! idem, il va falloir introduire les matrices de passage
    do 17 i = 1, ndim
        p(1,i) = nd(i)
17  continue
    do 18 i = 1, ndim
        p(2,i) = tau1(i)
18  continue
    if (ndim .eq. 3) then
        do 19 i = 1, ndim
            p(3,i) = tau2(i)
19      continue
    endif
! on construit la transposee de la matrice de passage
    call transp(p, 3, ndim, ndim, ptr,&
                3)
!
    do 1 i = 1, nnol
        pli = pla(i)
        do 2 j = 1, nno
            call indent(j, ddls, ddlm, nnos, jn)
            do 3 l = 1, ndim
                do 8 k = 1, ndim
! on remplit A : matrice [u*] / mu
                    mmat(pli+2*ndim-1+k,jn+ndim+l) = mmat(pli+2*ndim-1+k,jn+ndim+l)+&
                    2.d0*ffc(i)*p(k,l)*ffp(j)*jac
! et sa transposee
                    mmat(jn+ndim+l,pli+2*ndim-1+k) = mmat(jn+ndim+l,pli+2*ndim-1+k)+&
                    2.d0*ffc(i)*p(k,l)*ffp(j)*jac
 8              continue
 3          continue
 2      continue
 1  end do
!
! on remplit B : matrice w* / mu
    do 4 i = 1, nnol
        pli=pla(i)
        do 5 j = 1, nnol
            plj=pla(j)
            do 6 l = 1, ndim
! on remplit B
                mmat(pli-1+ndim+l,plj+2*ndim-1+l) = mmat(pli-1+ndim+l,plj+2*ndim-1+ l)-&
                ffc(i)*ffc(j)*jac
! et sa transposee
                mmat(plj+2*ndim-1+l,pli+ndim-1+l) = mmat(plj+2*ndim-1+l,pli+ndim-1+ l)-&
                ffc(i)*ffc(j)*jac
!
 6          continue
 5      continue
 4  end do
! on a enlevé le remplissage de C
!
end subroutine
