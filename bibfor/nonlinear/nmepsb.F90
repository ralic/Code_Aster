subroutine nmepsb(ndim, nno, axi, vff, dfdi,&
                  deplg, epsb, geps)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2005  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
#include "asterfort/r8inir.h"
#include "asterfort/u2mess.h"
#include "blas/ddot.h"
    logical :: axi
    integer :: ndim, nno
    real(kind=8) :: vff(nno), dfdi(nno, ndim), deplg(*)
    real(kind=8) :: epsb(6), geps(6, 3)
!
! ----------------------------------------------------------------------
!       CALCUL DES DEFORMATIONS REGULARISEES ET LEURS GRADIENTS
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  NNO     : NOMBRE DE NOEUDS (FAMILLE E-BARRE)
! IN  AXI     : .TRUE. SI AXISYMETRIQUE
! IN  VFF     : VALEURS DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
! IN  DFDI    : DERIVEE DES FONCTIONS DE FORME (POINT DE GAUSS COURANT)
! IN  DEPLG   : DEPLACEMENT GENERALISE (U ET E-BARRE)
! OUT EPSB    : DEFORMATIONS REGULARISEES EPSB(6)
! OUT GEPS    : GRADIENT DES DEFORMATIONS REGULARISEES GEPS(6,3)
! ----------------------------------------------------------------------
!
    integer :: kl, i, ndimsi, ndl
! ----------------------------------------------------------------------
!
!
!
    ndimsi = 2*ndim
    ndl = ndim+ndimsi
!
    call r8inir(6, 0.d0, epsb, 1)
    call r8inir(18, 0.d0, geps, 1)
    do 10 kl = 1, ndimsi
        epsb(kl)=ddot(nno,deplg(kl+ndim),ndl,vff,1)
        do 20 i = 1, ndim
            geps(kl,i) = ddot(nno,deplg(kl+ndim),ndl,dfdi(1,i),1)
20      continue
10  end do
!
    if (axi) call u2mess('F', 'ALGORITH7_76')
end subroutine
