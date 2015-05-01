subroutine mnluil(x, omega, alpha, eta, h,&
                  hf, nt, sort)
    implicit none
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================!
#include "jeveux.h"
#include "asterc/r8depi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedetr.h"
#include "asterfort/mnlfft.h"
#include "asterfort/wkvect.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
    integer :: h, hf, nt
    real(kind=8) :: x(2*h+1), omega, alpha, eta, sort(2*hf+1)
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: depi, xt
    integer :: it, iif
    integer :: k, j
!
    call jemarq()
!
    call wkvect('&&mnluil.t', 'V V R', nt, it)
    call wkvect('&&mnluil.f', 'V V R', nt, iif)
    depi=r8depi()
    zr(it)=0.d0
    do 10 k = 2, nt
        zr(it-1+k)=zr(it-1+k-1)+(depi/omega)/nt
10  continue
!
    do 20 k = 1, nt
        xt=x(1)
        do 21 j = 1, h
            xt=xt+x(1+j)*dcos(dble(j)*omega*zr(it-1+k))
            xt=xt+x(1+h+j)*dsin(dble(j)*omega*zr(it-1+k))
21      continue
        zr(iif-1+k)=((xt-1.d0)+sqrt((xt-1.d0)**2+4.d0*eta/alpha))/(2.d0/alpha)
20  continue
!
    call mnlfft(1, sort(1), zr(iif), hf, nt,1)
!
    call jedetr('&&mnluil.t')
    call jedetr('&&mnluil.f')
!
!
    call jedema()
end subroutine
