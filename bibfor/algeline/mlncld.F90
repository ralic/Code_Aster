subroutine mlncld(n, frontl, frontu, adper, t1,&
                  t2, ad, eps, ier)
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
    complex(kind=8) :: frontl(*), t1(*), frontu(*), t2(*), alpha, beta
    integer :: i, k, incx, incy
    integer :: seuin, seuik
    parameter(seuin=1500,seuik=300)
    integer :: nn, kk, lda
    character(len=1) :: tra
!
!     MLTFLD TRAITE UNIQUEMENT LES BLOCS DIAGONAUX
    tra='N'
    alpha= dcmplx(-1.d0,0.d0)
    beta = dcmplx( 1.d0,0.d0)
    incx = 1
    incy = 1
    lda = n
    do 30 k = 1, n
        do 10 i = 1, k - 1
            ad(i) = adper(i) + k - i
            t1(i) = frontu(ad(i))
            t2(i) = frontl(ad(i))
10      continue
        if (k .gt. 1) then
!
            nn= n-k+1
            kk= k-1
            if (nn .lt. seuin .or. kk .lt. seuik) then
                call sspmvc(n-k+1, k-1, frontl, ad, t1,&
                            frontl(adper(k)))
                call sspmvc(n-k+1, k-1, frontu, ad, t2,&
                            frontu(adper(k)))
            else
                call zgemv(tra, nn, kk, alpha, frontl(k),&
                           lda, t1, incx, beta, frontl(adper(k)),&
                           incy)
                call zgemv(tra, nn, kk, alpha, frontu(k),&
                           lda, t2, incx, beta, frontu(adper(k)),&
                           incy)
            endif
        endif
!         DIVISION PAR LE TERME DIAGONAL DE FRONTL (PAS FRONTU)
        if (abs(frontl(adper(k))) .le. eps) then
            ier = k
            goto 40
        endif
!RAY DIR$ IVDEP DIRECTIVE INHIBEE CAR DEPENDANCE AVANT
        do 20 i = 1, n - k
            frontl(adper(k)+i) = frontl(adper(k)+i)/frontl(adper(k))
20      continue
30  end do
40  continue
end subroutine
