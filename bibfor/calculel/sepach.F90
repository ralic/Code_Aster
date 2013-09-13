subroutine sepach(carael, chinz, base, chreel, chimag)
!
    implicit none
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/cesvar.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/juveca.h"
#include "asterfort/nopar2.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=1) :: base
    character(len=8) :: carael
    character(len=19) :: chreel, chimag
    character(len=*) :: chinz
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
!
!-----------------------------------------------------------------------
!
!
    integer :: gd, gdre, jdesc, jdescr, jdesci, nbval, nbval2
    integer :: jvaler, jvalei, ivale, ier, iret1, iret2
    integer :: nmax1, nmax2, jncmpr, jncmpc, i, jceld, jcelk, icelv, jcelvr
    integer :: jcelvi, ibid, nbsp
    character(len=8) :: nomgd, nomre
    character(len=4) :: typch, kbid
    character(len=19) :: canbva, chin
    character(len=24) :: ligrel, option, param, valk(2)
!
    call jemarq()
!
    chin=chinz
!
    call jeexin(chin//'.DESC', ier)
    if (ier .ne. 0) then
        call jeexin(chin//'.LIMA', ier)
        if (ier .ne. 0) then
            typch='CART'
        else
            typch='CHNO'
        endif
    else
        call jeexin(chin//'.CELK', ier)
        if (ier .ne. 0) then
            typch='CHML'
        else
            call utmess('F', 'CALCULEL_17')
        endif
    endif
!
    ier=0
!
    if (typch .eq. 'CHNO' .or. typch .eq. 'CART') then
        call jeveuo(chin//'.DESC', 'L', jdesc)
        gd=zi(jdesc)
    else
        call jeveuo(chin//'.CELD', 'L', jceld)
        gd=zi(jceld)
    endif
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    if ((nomgd(7:7).ne.' ') .or. (nomgd(5:6).ne.'_C')) then
        call utmess('F', 'CALCULEL4_80', sk=nomgd)
    endif
    nomre=nomgd(1:4)//'_R'
    call jenonu(jexnom('&CATA.GD.NOMGD', nomre), gdre)
!
    call jelira(jexnum('&CATA.GD.NOMCMP', gd), 'LONMAX', nmax1)
    call jelira(jexnum('&CATA.GD.NOMCMP', gdre), 'LONMAX', nmax2)
!
    if (nmax1 .ne. nmax2) then
        valk(1) = nomgd
        valk(2) = nomre
        call utmess('F', 'CALCULEL4_81', nk=2, valk=valk)
    endif
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gdre), 'L', jncmpr)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', jncmpc)
!
    do 10 i = 1, nmax1
        if (zk8(jncmpr-1+i) .ne. zk8(jncmpc-1+i)) then
            ier=1
            goto 10
        endif
10  end do
!
    if (ier .ne. 0) then
        valk(1) = nomgd
        valk(2) = nomre
        call utmess('F', 'CALCULEL4_82', nk=2, valk=valk)
    endif
!
!     -- CHAM_NO :
!     -------------------
    if (typch .eq. 'CHNO') then
        call jedup1(chin//'.DESC', base, chreel//'.DESC')
        call jedup1(chin//'.REFE', base, chreel//'.REFE')
        call jeveuo(chreel//'.DESC', 'E', jdescr)
        zi(jdescr)=gdre
!
        call jedup1(chin//'.DESC', base, chimag//'.DESC')
        call jedup1(chin//'.REFE', base, chimag//'.REFE')
        call jeveuo(chimag//'.DESC', 'E', jdesci)
        zi(jdesci)=gdre
!
        call jelira(chin//'.VALE', 'LONMAX', nbval)
        call jeveuo(chin//'.VALE', 'L', ivale)
!
        call wkvect(chreel//'.VALE', base//' V R', nbval, jvaler)
        call wkvect(chimag//'.VALE', base//' V R', nbval, jvalei)
!
        do 20 i = 1, nbval
            zr(jvaler-1+i)=dble(zc(ivale-1+i))
            zr(jvalei-1+i)=dimag(zc(ivale-1+i))
20      continue
!
!
!
!     -- CHAM_ELEM :
!     -------------------
    else if (typch.eq.'CHML') then
        call jeveuo(chin//'.CELK', 'L', jcelk)
        ligrel=zk24(jcelk)
        option=zk24(jcelk+1)
!
        param=nopar2(option,nomre,'OUT')
!
!       -- SI LE CHIN A DES SOUS-POINTS, IL FAUT ALLOUER CHREEL
!          ET CHIMAG AVEC DES SOUS-POINTS :
        call dismoi('F', 'MXNBSP', chin, 'CHAM_ELEM', nbsp,&
                    kbid, ibid)
        if (nbsp .gt. 1) then
            canbva='&&SEPACH.CANBVA'
            call cesvar(carael, ' ', ligrel, canbva)
            call alchml(ligrel, option, param, base, chreel,&
                        iret1, canbva)
            call alchml(ligrel, option, param, base, chimag,&
                        iret2, canbva)
            call detrsd('CHAM_ELEM_S', canbva)
        else
            call alchml(ligrel, option, param, base, chreel,&
                        iret1, ' ')
            call alchml(ligrel, option, param, base, chimag,&
                        iret2, ' ')
        endif
!
        ASSERT((iret1.eq.0).or.(iret2.eq.0))
!
        call jelira(chin//'.CELV', 'LONMAX', nbval)
        call jeveuo(chin//'.CELV', 'L', icelv)
!
        call jelira(chreel//'.CELV', 'LONMAX', nbval2)
        if (nbval2 .ne. nbval) then
            ASSERT(nbval.gt.nbval2)
            call juveca(chreel//'.CELV', nbval)
            call juveca(chimag//'.CELV', nbval)
        endif
!
!
        call jeveuo(chreel//'.CELV', 'E', jcelvr)
        call jeveuo(chimag//'.CELV', 'E', jcelvi)
!
!
        do 50 i = 1, nbval
            zr(jcelvr-1+i)=dble(zc(icelv-1+i))
            zr(jcelvi-1+i)=dimag(zc(icelv-1+i))
50      continue
!
!     -- CART :
!     -------------------
    else if (typch.eq.'CART') then
        call jedup1(chin//'.DESC', base, chreel//'.DESC')
        call jedup1(chin//'.NOMA', base, chreel//'.NOMA')
        call jedup1(chin//'.NOLI', base, chreel//'.NOLI')
        call jedup1(chin//'.LIMA', base, chreel//'.LIMA')
        call jeveuo(chreel//'.DESC', 'E', jdescr)
        zi(jdescr)=gdre
!
        call jedup1(chin//'.DESC', base, chimag//'.DESC')
        call jedup1(chin//'.NOMA', base, chimag//'.NOMA')
        call jedup1(chin//'.NOLI', base, chimag//'.NOLI')
        call jedup1(chin//'.LIMA', base, chimag//'.LIMA')
        call jeveuo(chimag//'.DESC', 'E', jdesci)
        zi(jdesci)=gdre
!
        call jelira(chin//'.VALE', 'LONMAX', nbval)
        call jeveuo(chin//'.VALE', 'L', ivale)
!
        call wkvect(chreel//'.VALE', base//' V R', nbval, jvaler)
        call wkvect(chimag//'.VALE', base//' V R', nbval, jvalei)
!
        do 60 i = 1, nbval
            zr(jvaler-1+i)=dble(zc(ivale-1+i))
            zr(jvalei-1+i)=dimag(zc(ivale-1+i))
60      continue
!
    endif
!
    call jedema()
!
end subroutine
