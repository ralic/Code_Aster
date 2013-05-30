subroutine nmgrib(nno, geom, dff, dir11, lexc,&
                  vecn, b, jac, p)
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
! ----------------------------------------------------------------------
! CALCUL DE LA MATRICE B ET JACOBIEN POUR LES GRILLES SECONDE GENERATION
! ----------------------------------------------------------------------
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/subaco.h'
    include 'asterfort/subacv.h'
    include 'asterfort/sumetr.h'
    include 'asterfort/u2mess.h'
    logical :: lexc
    integer :: nno
    real(kind=8) :: geom(3, nno), dff(2, nno), dir11(3)
    real(kind=8) :: b(6, nno), vecn(3), p(3, 6)
    integer :: i, j, n, alpha, beta, gamma
    real(kind=8) :: cova(3, 3), metr(2, 2), jac, cnva(3, 2), a(2, 2), r1(3)
    real(kind=8) :: projn
    real(kind=8) :: mtemp(3, nno), denomi
!
    call subaco(nno, dff, geom, cova)
    call sumetr(cova, metr, jac)
    call subacv(cova, metr, jac, cnva, a)
!
    call r8inir(3, 0.d0, r1, 1)
    call r8inir(6*nno, 0.d0, b, 1)
!
    projn = 0.d0
!
    do 5 j = 1, 3
        do 6 i = 1, 2
            r1(i) = r1(i)+cova(j,i)*dir11(j)
 6      continue
        projn = projn + cova(j,3) * dir11(j)
 5  end do
!
    denomi = (1.d0 - projn*projn)
    if (abs( denomi ) .le. r8prem()) then
        call u2mess('F', 'ELEMENTS_3')
    endif
!
    do 10 i = 1, 3
        do 10 n = 1, nno
            do 10 alpha = 1, 2
                do 10 beta = 1, 2
                    do 10 gamma = 1, 2
                        b(i,n) = b(i,n)+r1(alpha)*r1(gamma)*a(beta, gamma)* dff(beta,n)*cnva(i,al&
                                 &pha)/denomi
10                  continue
!
    if (lexc) then
        do 20 n = 1, nno
            do 20 i = 1, 3
                mtemp(i,n)=b(i,n)
20          continue
!
        call r8inir(18, 0.d0, p, 1)
        call r8inir(6*nno, 0.d0, b, 1)
!
        do 40 i = 1, 3
            p(i,i)=1.d0
40      continue
        p(1,5)=vecn(3)
        p(1,6)=-vecn(2)
        p(2,4)=-vecn(3)
        p(2,6)=vecn(1)
        p(3,4)=vecn(2)
        p(3,5)=-vecn(1)
!
        do 50 n = 1, nno
            do 50 i = 1, 6
                do 50 j = 1, 3
                    b(i,n)=b(i,n)+mtemp(j,n)*p(j,i)
50              continue
!
    endif
end subroutine
