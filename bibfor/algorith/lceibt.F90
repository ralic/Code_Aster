subroutine lceibt(ndimsi, eps, epsf, dep, invn,&
                  cn, dsidep)
! ======================================================================
! COPYRIGHT (C) 1991 - 2003  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/r8inir.h'
    real(kind=8) :: eps(6), epsf(6), dep(6, 12), dsidep(6, 6), invn(6, 6)
    real(kind=8) :: cn(6, 6)
    integer :: ndimsi
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ISOT_BETON - TERME COMPLEMENTAIRE DE
!                                           LA MATRICE TANGENTE POUR
!                                           LES LOIS COUPLES
! ----------------------------------------------------------------------
    integer :: i, j, k, l
    real(kind=8) :: sigel(6), sigme(6), temp1(6, 6)
!
    call r8inir(ndimsi, 0.d0, sigel, 1)
    call r8inir(ndimsi, 0.d0, sigme, 1)
    call r8inir(36, 0.d0, temp1, 1)
!
    do 30 i = 1, ndimsi
        temp1(i,i)=temp1(i,i)+1.d0
        do 30 j = 1, ndimsi
            do 30 k = 1, ndimsi
                do 30 l = 1, ndimsi
                    temp1(i,j)=temp1(i,j)-dep(i,k)*invn(k,l)*cn(l,j)
30              continue
!
    do 10 i = 1, ndimsi
        do 10 j = 1, ndimsi
            sigel(i) = sigel(i) + dep(i,j+6)*eps(j)
            sigme(i) = sigme(i) + dep(i,j+6)*(eps(j)-epsf(j))
10      continue
!
    do 20 i = 1, ndimsi
        do 20 j = 1, ndimsi
            do 20 k = 1, ndimsi
                dsidep(i,j)=dsidep(i,j)-temp1(i,k)*sigme(k)*sigel(j)
20          continue
!
end subroutine
