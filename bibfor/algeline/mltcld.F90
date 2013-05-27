subroutine mltcld(n, front, adper, t1, ad,&
                  eps, ier)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    include 'asterfort/sspmvc.h'
    include 'blas/zgemv.h'
    integer :: n, adper(*), ad(*), ier
    real(kind=8) :: eps
    complex(kind=8) :: front(*), t1(*), alpha, beta
    integer :: i, k
    integer :: seuin, seuik
    parameter(seuin=1500,seuik=300)
    integer :: nn, kk, lda, incx, incy
    character(len=1) :: tra
!
    lda = n
    tra='N'
    alpha= dcmplx(-1.d0,0.d0)
    beta = dcmplx( 1.d0,0.d0)
    incx = 1
    incy = 1
    do 30 k = 1, n
        do 10 i = 1, k - 1
            ad(i) = adper(i) + k - i
            t1(i) = front(ad(i))*front(adper(i))
10      continue
        if (k .gt. 1) then
!
            nn= n-k+1
            kk= k-1
            if (nn .lt. seuin .or. kk .lt. seuik) then
                call sspmvc(n-k+1, k-1, front, ad, t1,&
                            front(adper(k)))
            else
                call zgemv(tra, nn, kk, alpha, front(k),&
                           lda, t1, incx, beta, front(adper(k)),&
                           incy)
            endif
        endif
!         DIVISION PAR LE TERME DIAGONAL
        if (abs(front(adper(k))) .le. eps) then
            ier = k
            goto 40
        endif
!RAY DIR$ IVDEP DIRECTIVE INHIBEE CAR DEPENDANCE AVANT
        do 20 i = 1, n - k
            front(adper(k)+i) = front(adper(k)+i)/front(adper(k))
20      continue
30  end do
40  continue
end subroutine
