subroutine dilopt(dimdef, dimuel, poids, poids2, b,&
                  drde, matuu)
! ======================================================================
    implicit none
    include 'blas/dgemm.h'
    integer :: dimdef, dimuel
    real(kind=8) :: poids, poids2, drde(dimdef, dimdef), b(dimdef, dimuel)
    real(kind=8) :: matuu(dimuel*dimuel)
! ======================================================================
! TOLE CRS_1404
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
! --- BUT : ASSEMBLAGE DE L OPERATEUR TANGENT POUR LA PARTIE -----------
! ---       SECOND GRADIENT POUR L ELEMENT CALCULE ---------------------
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, kji
    real(kind=8) :: matr1(dimdef, dimuel), matri(dimuel, dimuel)
! ======================================================================
    do 10 i = 1, dimuel
        do 20 j = 1, dimdef
            matr1(j,i)=0.0d0
20      continue
        do 50 j = 1, dimuel
            matri(j,i)=0.0d0
50      continue
10  end do
! ======================================================================
    call dgemm('N', 'N', dimdef, dimuel, dimdef,&
               1.0d0, drde, dimdef, b, dimdef,&
               0.0d0, matr1, dimdef)
!
    call dgemm('T', 'N', dimuel, dimuel, dimdef,&
               poids, b, dimdef, matr1, dimdef,&
               0.0d0, matri, dimuel)
! ======================================================================
    kji=1
    do 30 i = 1, dimuel
        do 40 j = 1, dimuel
            matuu(kji) = matuu(kji)+matri(i,j)
            kji = kji+1
40      continue
30  end do
! ======================================================================
end subroutine
