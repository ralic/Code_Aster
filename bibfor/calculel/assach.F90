subroutine assach(preel2, pimag2, base2, chout2, parout)
    implicit none
#include "jeveux.h"
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
#include "asterfort/utmess.h"
#include "asterfort/vrrefe.h"
!
    character(len=*), intent(in) :: preel2, pimag2, base2
    character(len=*) :: chout2
    character(len=8), intent(in), optional :: parout
    character(len=19) :: chout, preel, pimag
    character(len=1) :: base
! ----------------------------------------------------------------------
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
! ======================================================================
!
! ----------------------------------------------------------------------
!
!
    integer :: i, ier, gdr, gdi, gdcpx, jncmpr, jncmpc
    integer :: nmax1, nmax2, nbvalr,  nbvali,   iret
    integer ::    nbvalc
!
    character(len=8) :: nomgdr, nomgdi, nomcpx, kmpicr, kmpici
    character(len=24) :: ligrel, option, param
    character(len=24) :: valk(2)
    integer, pointer :: celdi(:) => null()
    integer, pointer :: celdr(:) => null()
    character(len=24), pointer :: celk(:) => null()
    character(len=24), pointer :: celkr(:) => null()
    real(kind=8), pointer :: vali(:) => null()
    real(kind=8), pointer :: valr(:) => null()
    complex(kind=8), pointer :: vale(:) => null()
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
    call jeveuo(preel//'.CELD', 'L', vi=celdr)
    gdr=celdr(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', gdr), nomgdr)
    if ((nomgdr(7:7).ne.' ') .or. (nomgdr(5:6).ne.'_R')) then
        call utmess('F', 'CALCULEL_20', sk=nomgdr)
    endif
!
    call jeveuo(pimag//'.CELD', 'L', vi=celdi)
    gdi=celdi(1)
    call jenuno(jexnum('&CATA.GD.NOMGD', gdi), nomgdi)
!
    if ((nomgdi(7:7).ne.' ') .or. (nomgdi(5:6).ne.'_R')) then
        call utmess('F', 'CALCULEL_20', sk=nomgdi)
    endif
!
    if (nomgdr .ne. nomgdi) then
        call utmess('F', 'CALCULEL_21')
    endif
!
    nomcpx=nomgdr(1:4)//'_C'
!
    call jenonu(jexnom('&CATA.GD.NOMGD', nomcpx), gdcpx)
!
    call jelira(jexnum('&CATA.GD.NOMCMP', gdr), 'LONMAX', nmax1)
    call jelira(jexnum('&CATA.GD.NOMCMP', gdcpx), 'LONMAX', nmax2)
!
    if (nmax1 .ne. nmax2) then
        valk(1)=nomgdr
        valk(2)=nomcpx
        call utmess('F', 'CALCULEL_22', nk=2, valk=valk)
    endif
!
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gdr), 'L', jncmpr)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gdcpx), 'L', jncmpc)
!
    ier=0
    do i = 1, nmax1
        if (zk8(jncmpr-1+i) .ne. zk8(jncmpc-1+i)) ier=1
    end do
!
    if (ier .ne. 0) then
        valk(1)=nomgdr
        valk(2)=nomcpx
        call utmess('F', 'CALCULEL_23', nk=2, valk=valk)
    endif
!
    call jeveuo(preel//'.CELK', 'L', vk24=celkr)
    ligrel=celkr(1)
    option=celkr(2)
!
    if (present(parout)) then
        param=parout
    else
        param=nopar2(option,nomcpx,'OUT')
    endif

    call exisd('CHAM_ELEM_S', preel, iret)
    if (iret .gt. 0) then
        call alchml(ligrel, option, param, base, chout,&
                    ier, preel)
    else
        call alchml(ligrel, option, param, base, chout,&
                    ier, ' ')
    endif
!
    call jelira(preel//'.CELV', 'LONMAX', nbvalr)
    call jeveuo(preel//'.CELV', 'L', vr=valr)
    call jelira(pimag//'.CELV', 'LONMAX', nbvali)
    call jeveuo(pimag//'.CELV', 'L', vr=vali)
    ASSERT(nbvalr.eq.nbvali)
!
    call jeveuo(chout//'.CELV', 'E', vc=vale)
    call jelira(chout//'.CELV', 'LONMAX', nbvalc)
    ASSERT(nbvalr.eq.nbvalc)
!
    do i = 1, nbvalr
        vale(i)=dcmplx(valr(i),vali(i))
    end do
!
    call dismoi('MPI_COMPLET', preel, 'CHAM_ELEM', repk=kmpicr)
    call dismoi('MPI_COMPLET', pimag, 'CHAM_ELEM', repk=kmpici)
    ASSERT(kmpicr.eq.kmpici)
!
    call jeveuo(chout//'.CELK', 'E', vk24=celk)
    if (kmpicr .eq. 'OUI') then
        celk(7)='MPI_COMPLET'
    else
        celk(7)='MPI_INCOMPLET'
    endif
!
    call jedema()
!
end subroutine
