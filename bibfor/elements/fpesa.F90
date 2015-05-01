subroutine fpesa(nomte, xi, nb1, vecl)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/dxroep.h"
#include "asterfort/forpes.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/r8inir.h"
#include "asterfort/vectci.h"
#include "asterfort/vexpan.h"
    character(len=16) :: nomte
!
!
    integer :: nb1, npgsn
    real(kind=8) :: rho, epais, pesan, rnormc
    real(kind=8) :: xi(3, *), vpesan(3), vecl(51), vecl1(42)
!     REAL*8 VECTC(3),VECPTX(3,3)
!
!-----------------------------------------------------------------------
    integer :: i, intsn, jpesa, lzi, lzr
!-----------------------------------------------------------------------
    call jevech('PPESANR', 'L', jpesa)
    pesan = zr(jpesa)
    do 5 i = 1, 3
        vpesan(i) = pesan*zr(jpesa+i)
 5  end do
!
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1 = zi(lzi-1+1)
    npgsn = zi(lzi-1+4)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call dxroep(rho, epais)
!
    call r8inir(42, 0.d0, vecl1, 1)
!
    do 40 intsn = 1, npgsn
        call vectci(intsn, nb1, xi, zr(lzr), rnormc)
        call forpes(intsn, nb1, zr(lzr), rho, epais,&
                    vpesan, rnormc, vecl1)
40  end do
!
    call vexpan(nb1, vecl1, vecl)
    do 60 i = 1, 3
        vecl(6*nb1+i) = 0.d0
60  end do
!
end subroutine
