subroutine promat(a, nlamax, dimal, dimac, b,&
                  nlbmax, dimbl, dimbc, res)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!    BUT : PRODUIT DE 2 MATRICES A*B DANS RES
!
!    IN  : NLAMAX    : NB DE LIGNES DE MATRICE A DIMENSIONNEES
!          DIMAL     : NB DE LIGNES DE MATRICE A UTILISEES
!          DIMAC     : NB DE COLONNES DE MATRICE A
!          A(DIMAL,DIMAC): MATRICE A
!          NLBMAX    : NB DE LIGNES DE MATRICE B DIMENSIONNEES
!          DIMBL     : NB DE LIGNES DE MATRICE B
!          DIMBC     : NB DE COLONNES DE MATRICE B
!          B(DIMBL,DIMBC): MATRICE B
!
!    OUT : RES(DIMAL,DIMBC): MATRICE PRODUIT DE A*B
! ------------------------------------------------------------------
    implicit none
    include 'asterfort/u2mess.h'
    integer :: dimac, dimal, dimbc, dimbl
    real(kind=8) :: a(nlamax, *), b(nlbmax, *), res(nlamax, *)
!-----------------------------------------------------------------------
    integer :: icol, ilig, k, nlamax, nlbmax
    real(kind=8) :: xaux
!-----------------------------------------------------------------------
    if (dimac .ne. dimbl) then
        call u2mess('F', 'ALGELINE3_30')
    endif
    do 10 ilig = 1, dimal
        do 10 icol = 1, dimbc
            xaux=0.d0
            do 5 k = 1, dimac
                xaux=xaux+a(ilig,k)*b(k,icol)
 5          continue
            res(ilig,icol)=xaux
10      continue
end subroutine
