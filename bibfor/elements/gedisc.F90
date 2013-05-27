subroutine gedisc(ndim, nno, npg, vff, geom,&
                  pg)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    include 'blas/ddot.h'
    integer :: ndim, nno, npg
    real(kind=8) :: vff(nno, npg), geom(ndim, nno), pg(ndim+1, npg)
! ----------------------------------------------------------------------
!             CALCUL DES COORDONNEES DES POINTS DE GAUSS
! ----------------------------------------------------------------------
! IN  NDIM   DIMENSION DE L'ESPACE
! IN  NNO    NOMBRE DE NOEUDS
! IN  NPG    NOMBRE DE POINTS DE GAUSS
! IN  VFF    VALEUR DES FONCTIONS DE FORME
! IN  GEOM   COORDONNEES DES NOEUDS
! OUT PG     COORDONNEES DES POINTS DE GAUSS + POIDS
! ----------------------------------------------------------------------
    integer :: g, i
! ----------------------------------------------------------------------
    do 10 g = 1, npg
        do 20 i = 1, ndim
            pg(i,g) = ddot(nno,geom(i,1),ndim,vff(1,g),1)
20      continue
        pg(ndim+1,g) = 0.d0
10  end do
end subroutine
