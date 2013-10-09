subroutine cetran(lima1, lima2, nbma, chs1, chs2)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: lima1(*), lima2(*), nbma
    character(len=*) :: chs1, chs2
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
    integer :: nbpt, nbpt2, nbsp, nbsp2, ncmp1, ncmp2, ipt, isp, iad1
    integer :: iad2, jce1k, jce1d, jce1c, jce1v, jce1l, icmp1, jce2k, jce2d
    integer :: jce2c, jce2v, jce2l, icmp2, ima, ima1, ima2
    character(len=3) :: tsca
    character(len=8) :: nomgd, nomgd2, nocmp
    character(len=19) :: ces1, ces2
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    ces1 = chs1
    ces2 = chs2
!
    call jeveuo(ces1//'.CESK', 'L', jce1k)
    call jeveuo(ces1//'.CESD', 'L', jce1d)
    call jeveuo(ces1//'.CESC', 'L', jce1c)
    call jeveuo(ces1//'.CESV', 'L', jce1v)
    call jeveuo(ces1//'.CESL', 'L', jce1l)
!
    call jeveuo(ces2//'.CESK', 'L', jce2k)
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
!
    nomgd = zk8(jce1k-1+2)
    ncmp1 = zi(jce1d-1+2)
!
    nomgd2 = zk8(jce2k-1+2)
    ncmp2 = zi(jce2d-1+2)
!
    ASSERT(nomgd2.eq.nomgd)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
    do ima = 1, nbma
!
        ima1 = lima1(ima)
        ima2 = lima2(ima)
!
        nbpt = zi(jce1d-1+5+4*(ima1-1)+1)
        nbsp = zi(jce1d-1+5+4*(ima1-1)+2)
!
        nbpt2 = zi(jce2d-1+5+4*(ima2-1)+1)
        nbsp2 = zi(jce2d-1+5+4*(ima2-1)+2)
        ASSERT(nbpt2.eq.nbpt)
        ASSERT(nbsp2.eq.nbsp)
!
        do icmp2 = 1, ncmp2
!
            nocmp = zk8(jce2c-1+icmp2)
!
            icmp1 = indik8( zk8(jce1c), nocmp, 1, ncmp1 )
            if (icmp1 .eq. 0) goto 20
!
            do ipt = 1, nbpt
!
                do isp = 1, nbsp
!
                    call cesexi('C', jce1d, jce1l, ima1, ipt,&
                                isp, icmp1, iad1)
                    if (iad1 .le. 0) goto 40
                    if (.not. zl(jce1l-1+iad1)) goto 40
                    call cesexi('C', jce2d, jce2l, ima2, ipt,&
                                isp, icmp2, iad2)
                    ASSERT(iad2.gt.0)
!
                    zl(jce2l-1+iad2) = .true.
!
                    if (tsca .eq. 'R') then
                        zr(jce2v-1+iad2) = zr(jce1v-1+iad1)
                    else if (tsca.eq.'C') then
                        zc(jce2v-1+iad2) = zc(jce1v-1+iad1)
                    else if (tsca.eq.'I') then
                        zi(jce2v-1+iad2) = zi(jce1v-1+iad1)
                    else if (tsca.eq.'L') then
                        zl(jce2v-1+iad2) = zl(jce1v-1+iad1)
                    else if (tsca.eq.'K8') then
                        zk8(jce2v-1+iad2) = zk8(jce1v-1+iad1)
                    else
                        ASSERT(.false.)
                    endif
!
 40                 continue
                end do
!
            end do
!
 20         continue
        end do
!
    end do
!
    call jedema()
end subroutine
