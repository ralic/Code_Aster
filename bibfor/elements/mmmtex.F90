subroutine mmmtex(ndexfr, ndim, nnl, nne, nnm,&
                  nbcps, matrff, matrfe, matrfm, matref,&
                  matrmf)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/isdeco.h"
#include "asterfort/mmmte2.h"
    integer :: ndim, nne, nnl, nnm, nbcps
    integer :: ndexfr
    real(kind=8) :: matrff(18, 18)
    real(kind=8) :: matref(27, 18), matrfe(18, 27)
    real(kind=8) :: matrmf(27, 18), matrfm(18, 27)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! CALCUL DES MATRICES - MODIFICATIONS EXCLUSION
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNE    : NOMBRE DE NOEUDS DE LA MAILLE ESCLAVE
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  NNL    : NOMBRE DE NOEUDS DE LAGRANGE
! IN  NBCPS  : NOMBRE DE COMPOSANTES/NOEUD DES LAGR_C+LAGR_F
! IN  ndexfr : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
! OUT MATRFF : MATRICE ELEMENTAIRE LAGR_F/LAGR_F
! OUT MATRFE : MATRICE ELEMENTAIRE LAGR_F/DEPL_E
! OUT MATRFM : MATRICE ELEMENTAIRE LAGR_F/DEPL_M
! OUT MATREF : MATRICE ELEMENTAIRE DEPL_E/LAGR_F
! OUT MATRMF : MATRICE ELEMENTAIRE DEPL_M/LAGR_F
!
! ----------------------------------------------------------------------
!
    integer :: ndexcl(10), nbcpf
!
! ----------------------------------------------------------------------
!
    nbcpf = nbcps - 1
!
! --- MODIFICATION DES TERMES SI EXCLUSION DIRECTION FROTT. SANS_NO_FR
!
    if (ndexfr .ne. 0) then
        call isdeco([ndexfr], ndexcl, 10)
        call mmmte2(ndim, nnl, nne, nnm, nbcpf,&
                    ndexcl, matrff, matrfe, matrfm, matref,&
                    matrmf)
    endif
!
end subroutine
