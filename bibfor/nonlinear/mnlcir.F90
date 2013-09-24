subroutine mnlcir(xdep, ydep, omega, alpha, eta,&
                  h, hf, nt, xsort)
    implicit none
!
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
! ----------------------------------------------------------------------
#include "jeveux.h"
! ----------------------------------------------------------------------
! --- DECLARATION DES ARGUMENTS DE LA ROUTINE
! ----------------------------------------------------------------------
#include "blas/dscal.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mnlfft.h"
#include "asterc/r8depi.h"
#include "asterfort/wkvect.h"

    integer :: h, hf, nt
    real(kind=8) :: omega, alpha, eta
    character(len=14) :: xdep, ydep, xsort
! ----------------------------------------------------------------------
! --- DECLARATION DES VARIABLES LOCALES
! ----------------------------------------------------------------------
    real(kind=8) :: depi, xt, yt, rk
    integer :: k, j, ix, iy, isor, it, ifx, ify, ifn, ir
!
    call jemarq()
!
    call wkvect('&&MNLCIR.T', 'V V R', nt, it)
    call wkvect('&&MNLCIR.FX', 'V V R', nt, ifx)
    call wkvect('&&MNLCIR.FY', 'V V R', nt, ify)
    call wkvect('&&MNLCIR.FN', 'V V R', nt, ifn)
    call wkvect('&&MNLCIR.R', 'V V R', nt, ir)
!
    call jeveuo(xdep, 'E', ix)
    call jeveuo(ydep, 'E', iy)
    call jeveuo(xsort, 'E', isor)
!
    depi=r8depi()
    zr(it)=0.d0
    do 10 k = 2, nt
        zr(it-1+k)=zr(it-1+k-1)+(depi/omega)/nt
10  continue
!
    do 20 k = 1, nt
        xt=zr(ix)
        yt=zr(iy)
        do 21 j = 1, h
            xt=xt+zr(ix+j)*dcos(dble(j)*omega*zr(it-1+k))
            yt=yt+zr(iy+j)*dcos(dble(j)*omega*zr(it-1+k))
            xt=xt+zr(ix+h+j)*dsin(dble(j)*omega*zr(it-1+k))
            yt=yt+zr(iy+h+j)*dsin(dble(j)*omega*zr(it-1+k))
21      continue
        rk=xt**2+yt**2
        zr(ir-1+k)=sqrt(rk)
        zr(ifn-1+k)=((zr(ir-1+k)-1.d0)+sqrt((zr(ir-1+k)-1.d0)**2+&
        4.d0*eta/alpha))/(2.d0/alpha)
        zr(ifx-1+k)=zr(ifn-1+k)*xt/zr(ir-1+k)
        zr(ify-1+k)=zr(ifn-1+k)*yt/zr(ir-1+k)
20  continue
!
    call dscal(4*(2*hf+1), 0.d0, zr(isor), 1)
    call mnlfft(1, zr(isor), zr(ifx), hf, nt,&
                1)
    call mnlfft(1, zr(isor+(2*hf+1)), zr(ify), hf, nt,&
                1)
    call mnlfft(1, zr(isor+2*(2*hf+1)), zr(ir), hf, nt,&
                1)
    call mnlfft(1, zr(isor+3*(2*hf+1)), zr(ifn), hf, nt,&
                1)
!
    call jedetr('&&MNLCIR.T')
    call jedetr('&&MNLCIR.FX')
    call jedetr('&&MNLCIR.FY')
    call jedetr('&&MNLCIR.FN')
    call jedetr('&&MNLCIR.R')

    call jedema()
!
end subroutine
