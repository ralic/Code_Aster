subroutine dfda2d(kpg, nno, poids, sdfrde, sdfrdk,&
                  sdedx, sdedy, sdkdx, sdkdy, sdfdx,&
                  sdfdy, geom, jac)
!
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
    include 'jeveux.h'
    include 'asterc/r8gaem.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    integer :: nno
    real(kind=8) :: poids, sdfrde(4, 4), sdfrdk(4, 4)
    real(kind=8) :: geom(2, 4), jac
!
    integer :: i, kpg, iadzi, iazk24
    real(kind=8) :: sdxde, sdxdk, sdyde, sdydk
    real(kind=8) :: sdedx(4), sdkdx(4), sdkdy(4), sdedy(4)
    real(kind=8) :: sdfdy(4, 4), sdfdx(4, 4)
    character(len=8) :: nomail
!
!
!
!
!
    call jemarq()
!
    sdxde = 0.d0
    sdxdk = 0.d0
    sdyde = 0.d0
    sdydk = 0.d0
!
    do 10 i = 1, nno
        sdxde = sdxde + geom(1,i)*sdfrde(kpg,i)
        sdxdk = sdxdk + geom(1,i)*sdfrdk(kpg,i)
        sdyde = sdyde + geom(2,i)*sdfrde(kpg,i)
        sdydk = sdydk + geom(2,i)*sdfrdk(kpg,i)
10  end do
!
    jac = sdxde*sdydk - sdxdk*sdyde
!
    if (abs(jac) .le. 1.d0/r8gaem()) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        call u2mesk('F', 'ALGORITH2_59', 1, nomail)
    endif
!
    do 20 i = 1, nno
        sdfdx(kpg,i) = (sdydk*sdfrde(kpg,i)-sdyde*sdfrdk(kpg,i))/jac
        sdfdy(kpg,i) = (sdxde*sdfrdk(kpg,i)-sdxdk*sdfrde(kpg,i))/jac
20  end do
!
    sdedx(kpg) = sdydk/jac
    sdkdy(kpg) = sdxde/jac
    sdedy(kpg) = -sdxdk/jac
    sdkdx(kpg) = -sdyde/jac
    jac = abs(jac)*poids
!
    call jedema()
end subroutine
