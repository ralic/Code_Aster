subroutine rc36zz(noma, nomgd, nbcmp, nocmp, nbma,&
                  listma, chelem)
    implicit   none
#include "jeveux.h"
#include "asterfort/cescre.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: nbcmp, nbma, listma(*)
    character(len=8) :: noma, nomgd
    character(len=16) :: nocmp(*)
    character(len=24) :: chelem
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     MISE A ZERO D'UN CHAM_ELEM DE TYPE ELNO
!
!     ------------------------------------------------------------------
!
    integer :: jcesd, jcesv, jcesl, im, ima, nbpt, decal, iad, ipt, icmp, iret
! DEB ------------------------------------------------------------------
    call jemarq()
!
    call jeexin(chelem(1:19)//'.CESD', iret)
!
    if (iret .eq. 0) then
        call cescre('V', chelem, 'ELNO', noma, nomgd,&
                    nbcmp, nocmp, -1, -1, -nbcmp)
    endif
!
    call jeveuo(chelem(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(chelem(1:19)//'.CESV', 'E', jcesv)
    call jeveuo(chelem(1:19)//'.CESL', 'E', jcesl)
!
    do 20 im = 1, nbma
        ima = listma(im)
        nbpt = zi(jcesd-1+5+4*(ima-1)+1)
        decal = zi(jcesd-1+5+4*(ima-1)+4)
        do 22 ipt = 1, nbpt
            do 24 icmp = 1, nbcmp
                iad = decal + (ipt-1)*nbcmp + icmp
                zl(jcesl-1+iad) = .true.
                zr(jcesv-1+iad) = 0.d0
24          continue
22      continue
20  end do
!
    call jedema()
!
end subroutine
