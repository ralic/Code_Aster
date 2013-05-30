subroutine mmcalg(ndim, nnm, ddffm, geomam, tau1,&
                  tau2, norm, mprt1n, mprt2n, gene11,&
                  gene21)
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
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_21
!
    implicit     none
    include 'asterfort/matini.h'
    integer :: ndim, nnm
    real(kind=8) :: ddffm(3, 9)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3)
    real(kind=8) :: geomam(9, 3)
    real(kind=8) :: mprt1n(3, 3), mprt2n(3, 3)
    real(kind=8) :: gene11(3, 3), gene21(3, 3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DES MATRICES DE PROJECTION POUR NEWTON GENERALISE
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  NNM    : NOMBRE DE NOEUDS DE LA MAILLE MAITRE
! IN  DDFFM  : DERIVEES SECONDES DES FONCTIONS DE FORME MAITRES
! IN  GEOMAM : GEOMETRIE ACTUALISEE SUR NOEUDS MAITRES
! IN  TAU1   : PREMIERE TANGENTE EXTERIEURE
! IN  TAU2   : SECONDE TANGENTE EXTERIEURE
! IN  NORM   : NORMALE INTERIEURE
! OUT MPRT1N : MATRICE DE PROJECTION TANGENTE1/NORMALE
! OUT MPRT2N : MATRICE DE PROJECTION TANGENTE2/NORMALE
! OUT GENE11 : MATRICE
! OUT GENE21 : MATRICE
!
! ----------------------------------------------------------------------
!
    integer :: i, j, inom
!
! ----------------------------------------------------------------------
!
    call matini(3, 3, 0.d0, mprt1n)
    call matini(3, 3, 0.d0, mprt2n)
    call matini(3, 3, 0.d0, gene11)
    call matini(3, 3, 0.d0, gene21)
!
! --- MATRICE DE PROJECTION TANGENTE1/NORMALE
!
    do 126 i = 1, ndim
        do 116 j = 1, ndim
            mprt1n(i,j) = 1.d0*tau1(i)*norm(j)
116      continue
126  end do
!
! --- MATRICE DE PROJECTION TANGENTE2/NORMALE
!
    do 127 i = 1, ndim
        do 117 j = 1, ndim
            mprt2n(i,j) = 1.d0*tau2(i)*norm(j)
117      continue
127  end do
!
    do 24 i = 1, ndim
        do 25 j = 1, ndim
            do 27 inom = 1, nnm
                gene11(i,j) = gene11(i,j)+ ddffm(1,inom)*geomam(inom, i)*norm(j)
!
                gene21(i,j) = gene21(i,j)+ ddffm(3,inom)*geomam(inom, i)*norm(j)
27          continue
25      continue
24  end do
!
!
!
!
end subroutine
