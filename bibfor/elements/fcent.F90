subroutine fcent(nomte, xi, nb1, vecl)
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
    implicit none
    include 'jeveux.h'
    include 'asterfort/dxroep.h'
    include 'asterfort/forcen.h'
    include 'asterfort/jevech.h'
    include 'asterfort/jevete.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/vectci.h'
    include 'asterfort/vexpan.h'
    character(len=16) :: nomte
!
!
    integer :: nb1
    real(kind=8) :: rho, epais
    real(kind=8) :: xi(3, *), vomega(3), vecl(51), vecl1(42)
    real(kind=8) :: xa(3)
!
!-----------------------------------------------------------------------
    integer :: i, intsn, irota, lzi, lzr, npgsn
    real(kind=8) :: rnormc
!-----------------------------------------------------------------------
    call jevech('PROTATR', 'L', irota)
    vomega(1)=zr(irota)*zr(irota+1)
    vomega(2)=zr(irota)*zr(irota+2)
    vomega(3)=zr(irota)*zr(irota+3)
!
    xa(1)=zr(irota+4)
    xa(2)=zr(irota+5)
    xa(3)=zr(irota+6)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    npgsn=zi(lzi-1+4)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call dxroep(rho, epais)
!
    call r8inir(42, 0.d0, vecl1, 1)
!
    do 40 intsn = 1, npgsn
        call vectci(intsn, nb1, xi, zr(lzr), rnormc)
        call forcen(rnormc, intsn, nb1, xi, zr(lzr),&
                    rho, epais, vomega, vecl1, xa)
40  end do
!
    call vexpan(nb1, vecl1, vecl)
    do 60 i = 1, 3
        vecl(6*nb1+i)=0.d0
60  end do
!
end subroutine
