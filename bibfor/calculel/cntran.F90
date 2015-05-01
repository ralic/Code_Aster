subroutine cntran(linoeu, nbno, chs1, chs2)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: linoeu(*), nbno
    character(len=*) :: chs1, chs2
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     COMMANDE:  CREA_RESU
!     TRAITEMENT DU MOT CLE FACTEUR "PERM_CHAMP"
!
! ----------------------------------------------------------------------
!
!
!
    integer :: ncmp1, ncmp2, ino1, ino2,    jcn1v, jcn1l
    integer :: icmp1,    jcn2v, jcn2l, icmp2
    character(len=3) :: tsca
    character(len=8) :: nomgd, nomgd2, nocmp
    character(len=19) :: cns1, cns2
    integer, pointer :: cn1d(:) => null()
    integer, pointer :: cn2d(:) => null()
    character(len=8), pointer :: cn1c(:) => null()
    character(len=8), pointer :: cn2c(:) => null()
    character(len=8), pointer :: cn1k(:) => null()
    character(len=8), pointer :: cn2k(:) => null()
! DEB ------------------------------------------------------------------
    call jemarq()
!
    cns1 = chs1
    cns2 = chs2
!
    call jeveuo(cns1//'.CNSK', 'L', vk8=cn1k)
    call jeveuo(cns1//'.CNSD', 'L', vi=cn1d)
    call jeveuo(cns1//'.CNSC', 'L', vk8=cn1c)
    call jeveuo(cns1//'.CNSV', 'L', jcn1v)
    call jeveuo(cns1//'.CNSL', 'E', jcn1l)
!
    call jeveuo(cns2//'.CNSK', 'L', vk8=cn2k)
    call jeveuo(cns2//'.CNSD', 'L', vi=cn2d)
    call jeveuo(cns2//'.CNSC', 'L', vk8=cn2c)
    call jeveuo(cns2//'.CNSV', 'E', jcn2v)
    call jeveuo(cns2//'.CNSL', 'E', jcn2l)
!
    nomgd = cn1k(2)
    ncmp1 = cn1d(2)
!
    nomgd2 = cn2k(2)
    ncmp2 = cn2d(2)
!
    ASSERT(nomgd2.eq.nomgd)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
    do ino2 = 1, nbno
!
        ino1 = linoeu(ino2)
        if (ino1 .eq. 0) goto 10
!
        do icmp2 = 1, ncmp2
!
            nocmp = cn2c(icmp2)
!
            icmp1 = indik8( cn1c, nocmp, 1, ncmp1 )
            if (icmp1 .eq. 0) goto 20
            if (.not. zl(jcn1l-1+(ino1-1)*ncmp1+icmp1)) goto 20
!
            zl(jcn2l-1+(ino2-1)*ncmp2+icmp2) = .true.
!
            if (tsca .eq. 'R') then
                zr(jcn2v-1+(ino2-1)*ncmp2+icmp2) = zr( jcn1v-1+(ino1-1 ) *ncmp1+icmp1 )
            else if (tsca.eq.'C') then
                zc(jcn2v-1+(ino2-1)*ncmp2+icmp2) = zc( jcn1v-1+(ino1-1 ) *ncmp1+icmp1 )
            else if (tsca.eq.'I') then
                zi(jcn2v-1+(ino2-1)*ncmp2+icmp2) = zi( jcn1v-1+(ino1-1 ) *ncmp1+icmp1 )
            else if (tsca.eq.'L') then
                zl(jcn2v-1+(ino2-1)*ncmp2+icmp2) = zl( jcn1v-1+(ino1-1 ) *ncmp1+icmp1 )
            else if (tsca.eq.'K8') then
                zk8(jcn2v-1+(ino2-1)*ncmp2+icmp2) = zk8( jcn1v-1+(ino1- 1 )*ncmp1+icmp1 )
            else
                ASSERT(.false.)
            endif
!
 20         continue
        end do
!
 10     continue
    end do
!
    call jedema()
end subroutine
