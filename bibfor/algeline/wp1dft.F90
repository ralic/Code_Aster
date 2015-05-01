subroutine wp1dft(lmat, imode, zeropo, z, detnor,&
                  det, idet, isturm)
    implicit none
#include "jeveux.h"
#include "asterfort/almulr.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    integer :: lmat, imode, idet
    complex(kind=8) :: zeropo(*), z, detnor
    real(kind=8) :: det
!     -----------------------------------------------------------------
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
!     -----------------------------------------------------------------
    real(kind=8) :: un, zero, dist
    character(len=24) :: nomdia
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iret, isturm, ldiag, neq
!-----------------------------------------------------------------------
    data  nomdia/'                   .DIGS'/
!
    call jemarq()
    un = 1.d0
    zero = 0.d0
!
!     --- PRELIMINAIRE ---
    nomdia(1:19) = zk24(zi(lmat+1))
    neq = zi(lmat+2 )
    call jeexin(nomdia, iret)
    if (iret .eq. 0) then
        call utmess('F', 'MODELISA2_9', sk=nomdia)
    endif
    call jeveuo(nomdia, 'L', ldiag)
    ldiag=ldiag+neq
!
!
!     --- CALCUL DE LA DEFLATION ---
    detnor = dcmplx(un,zero)
    do 1 i = 1, imode-1
        detnor = detnor / ( (z-zeropo(i))*(z-dconjg(zeropo(i))) )
 1  end do
!
!     --- CALCUL DU DETERMINANT DE LA MATRICE DEFLATEE ---
    det = un
    idet = 0
    isturm = 0
    do 33 i = ldiag, ldiag+neq-1
        dist = sqrt(dble(zc(i)*dconjg(zc(i))))
        detnor = detnor * zc(i) / dist
        if (dble(zc(i)) .lt. zero) isturm = isturm + 1
        call almulr('CUMUL', [dist], 1, det, idet)
33  end do
    call jedetr(nomdia)
    call jedema()
end subroutine
