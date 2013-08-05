subroutine assach(preel2, pimag2, base2, chout2)
    implicit none
#include "jeveux.h"
!
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nopar2.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vrrefe.h"
    character(len=*) :: chout2, preel2, pimag2, base2
    character(len=19) :: chout, preel, pimag
    character(len=1) :: base
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!
!
    integer :: i, ier, gdr, gdi, gdcpx, jncmpr, jncmpc, ibid
    integer :: nmax1, nmax2, nbvalr, ivalr, nbvali, ivali, jvale, iret
    integer :: jceldr, jceldi, jcelkr, nbvalc, jcelk
!
    character(len=8) :: nomgdr, nomgdi, nomcpx, k8b, kmpicr, kmpici
    character(len=24) :: ligrel, option, param
    character(len=24) :: valk(2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    base=base2
    preel=preel2
    pimag=pimag2
    chout=chout2
!
    call jeexin(preel//'.CELK', ier)
    ASSERT(ier.gt.0)
!
    call vrrefe(preel, pimag, ier)
    ASSERT(ier.eq.0)
!
!
!
!
    call jeveuo(preel//'.CELD', 'L', jceldr)
    gdr=zi(jceldr)
    call jenuno(jexnum('&CATA.GD.NOMGD', gdr), nomgdr)
    if ((nomgdr(7:7).ne.' ') .or. (nomgdr(5:6).ne.'_R')) then
        call u2mesk('F', 'CALCULEL_20', 1, nomgdr)
    endif
!
    call jeveuo(pimag//'.CELD', 'L', jceldi)
    gdi=zi(jceldi)
    call jenuno(jexnum('&CATA.GD.NOMGD', gdi), nomgdi)
!
    if ((nomgdi(7:7).ne.' ') .or. (nomgdi(5:6).ne.'_R')) then
        call u2mesk('F', 'CALCULEL_20', 1, nomgdi)
    endif
!
    if (nomgdr .ne. nomgdi) then
        call u2mess('F', 'CALCULEL_21')
    endif
!
    nomcpx=nomgdr(1:4)//'_C'
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomcpx), gdcpx)
!
    call jelira(jexnum('&CATA.GD.NOMCMP', gdr), 'LONMAX', nmax1, k8b)
    call jelira(jexnum('&CATA.GD.NOMCMP', gdcpx), 'LONMAX', nmax2, k8b)
!
    if (nmax1 .ne. nmax2) then
        valk(1)=nomgdr
        valk(2)=nomcpx
        call u2mesk('F', 'CALCULEL_22', 2, valk)
    endif
!
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gdr), 'L', jncmpr)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gdcpx), 'L', jncmpc)
!
    ier=0
    do 10 i = 1, nmax1
        if (zk8(jncmpr-1+i) .ne. zk8(jncmpc-1+i)) ier=1
10  end do
!
    if (ier .ne. 0) then
        valk(1)=nomgdr
        valk(2)=nomcpx
        call u2mesk('F', 'CALCULEL_23', 2, valk)
    endif
!
    call jeveuo(preel//'.CELK', 'L', jcelkr)
    ligrel=zk24(jcelkr)
    option=zk24(jcelkr+1)
!
    param=nopar2(option,nomcpx,'OUT')
    call exisd('CHAM_ELEM_S', preel, iret)
    if (iret .gt. 0) then
        call alchml(ligrel, option, param, base, chout,&
                    ier, preel)
    else
        call alchml(ligrel, option, param, base, chout,&
                    ier, ' ')
    endif
!
    call jelira(preel//'.CELV', 'LONMAX', nbvalr, k8b)
    call jeveuo(preel//'.CELV', 'L', ivalr)
    call jelira(pimag//'.CELV', 'LONMAX', nbvali, k8b)
    call jeveuo(pimag//'.CELV', 'L', ivali)
    ASSERT(nbvalr.eq.nbvali)
!
    call jeveuo(chout//'.CELV', 'E', jvale)
    call jelira(chout//'.CELV', 'LONMAX', nbvalc, k8b)
    ASSERT(nbvalr.eq.nbvalc)
!
    do 40 i = 1, nbvalr
        zc(jvale-1+i)=dcmplx(zr(ivalr-1+i),zr(ivali-1+i))
40  end do
!
    call dismoi('F', 'MPI_COMPLET', preel, 'CHAM_ELEM', ibid,&
                kmpicr, ibid)
    call dismoi('F', 'MPI_COMPLET', pimag, 'CHAM_ELEM', ibid,&
                kmpici, ibid)
    ASSERT(kmpicr.eq.kmpici)
!
    call jeveuo(chout//'.CELK', 'E', jcelk)
    if (kmpicr .eq. 'OUI') then
        zk24(jcelk-1+7)='MPI_COMPLET'
    else
        zk24(jcelk-1+7)='MPI_INCOMPLET'
    endif
!
    call jedema()
!
end subroutine
