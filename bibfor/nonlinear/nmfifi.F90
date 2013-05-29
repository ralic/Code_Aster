subroutine nmfifi(npg, typmod, geom, sigma, fint)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
!
! ======================================================================
!
    implicit none
    include 'asterfort/nmfisa.h'
    include 'asterfort/r8inir.h'
    integer :: npg
    real(kind=8) :: geom(2, 4), sigma(2, npg), fint(8)
    character(len=8) :: typmod(2)
!
!-----------------------------------------------------------------------
!
! BUT:
!      CALCUL DES FINT = ( B_T SIGMA) POUR L'OPTION FORC_NODA
!      SUBROUTINE APPELEE DANS LE TE0202
!
! IN  : GEOM,SIGMA,NPG,TYPMOD
! OUT : FINT
!-----------------------------------------------------------------------
!
    logical :: axi
    integer :: i, j, kpg
    real(kind=8) :: b(2, 8), poids
!-----------------------------------------------------------------------
!
!
!    INITIALISATION
    axi = typmod(1).eq.'AXIS'
    call r8inir(8, 0.d0, fint, 1)
!
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 11 kpg = 1, npg
!
!      CALCUL DU POIDS ET DE LA MATRICE B
        call nmfisa(axi, geom, kpg, poids, b)
!
!      CALCUL DES FINT = ( B_T SIGMA ) :
        do 20 i = 1, 8
            do 40 j = 1, 2
                fint(i) = fint(i) + poids*b(j,i)*sigma(j,kpg)
40          continue
20      continue
!
11  end do
end subroutine
