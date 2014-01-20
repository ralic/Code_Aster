subroutine cestas(cesz)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: cesz
! ---------------------------------------------------------------------
! BUT: "TASSER" UN CHAM_ELEM_S LORSQU'IL A ETE ALLOUE TROP GRAND
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CESZ   IN/JXIN  K19 : SD CHAM_ELEM_S A TASSER
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer ::  jcesd, jcesv, jcesl,  nbma
    integer :: jce2d, jce2v, jce2l
    integer ::    icmp
    integer :: ima, ipt, isp, nbpt, nbsp, iad, iad2
    integer :: ncmp, nbcmp, nbpt2, nbsp2, nbcmp2
    character(len=1) :: base
    character(len=8) :: ma, nomgd, typces
    character(len=3) :: tsca
    character(len=19) :: ces, ces2
    character(len=8), pointer :: cesc(:) => null()
    character(len=8), pointer :: cesk(:) => null()
    integer, pointer :: vnbcmp(:) => null()
    integer, pointer :: vnbpt(:) => null()
    integer, pointer :: vnbsp(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
!
    ces = cesz
!
    call jeveuo(ces//'.CESK', 'L', vk8=cesk)
    call jeveuo(ces//'.CESD', 'L', jcesd)
    call jeveuo(ces//'.CESC', 'L', vk8=cesc)
    call jeveuo(ces//'.CESL', 'L', jcesl)
    call jeveuo(ces//'.CESV', 'L', jcesv)
!
    call jelira(ces//'.CESK', 'CLAS', cval=base)
!
    ma = cesk(1)
    nomgd = cesk(2)
    typces = cesk(3)
!
    nbma = zi(jcesd-1+1)
    ncmp = zi(jcesd-1+2)
!
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!
!
!     1- CALCUL DES OBJETS :
!        '&&CESTAS.NBPT'
!        '&&CESTAS.NBSP'
!        '&&CESTAS.NBCMP'
!     -----------------------------------------------------------
    AS_ALLOCATE(vi=vnbpt, size=nbma)
    AS_ALLOCATE(vi=vnbsp, size=nbma)
    AS_ALLOCATE(vi=vnbcmp, size=nbma)
!
    do ima = 1, nbma
        nbpt = zi(jcesd-1+5+4* (ima-1)+1)
        nbsp = zi(jcesd-1+5+4* (ima-1)+2)
        nbcmp = zi(jcesd-1+5+4* (ima-1)+3)
        nbpt2 = 0
        nbsp2 = 0
        nbcmp2 = 0
!
        do ipt = 1, nbpt
            do isp = 1, nbsp
                do icmp = 1, nbcmp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, icmp, iad)
                    if (iad .gt. 0) then
                        nbpt2 = ipt
                        nbsp2 = isp
                        nbcmp2 = icmp
                    endif
                end do
            end do
        end do
        vnbpt(ima) = nbpt2
        vnbsp(ima) = nbsp2
        vnbcmp(ima) = nbcmp2
    end do
!
!
!
!     2- ALLOCATION DE CES2 :
!     --------------------------
    ces2 = '&&CESTAS.CES2'
    call cescre(base, ces2, typces, ma, nomgd,&
                ncmp, cesc, vnbpt, vnbsp,vnbcmp)
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
!
!
!
!
!     3- RECOPIE DES VALEURS DE CES DANS CES2 :
!     -----------------------------------------------------------
    do ima = 1, nbma
        nbpt = zi(jce2d-1+5+4* (ima-1)+1)
        nbsp = zi(jce2d-1+5+4* (ima-1)+2)
        nbcmp = zi(jce2d-1+5+4* (ima-1)+3)
!
        do ipt = 1, nbpt
            do isp = 1, nbsp
                do icmp = 1, nbcmp
                    call cesexi('C', jcesd, jcesl, ima, ipt,&
                                isp, icmp, iad)
                    call cesexi('C', jce2d, jce2l, ima, ipt,&
                                isp, icmp, iad2)
                    if (iad .gt. 0) then
                        ASSERT(iad2.lt.0)
                        iad2 = -iad2
                        zl(jce2l-1+iad2) = .true.
!
                        if (tsca .eq. 'R') then
                            zr(jce2v-1+iad2) = zr(jcesv-1+iad)
                        else if (tsca.eq.'I') then
                            zi(jce2v-1+iad2) = zi(jcesv-1+iad)
                        else if (tsca.eq.'C') then
                            zc(jce2v-1+iad2) = zc(jcesv-1+iad)
                        else if (tsca.eq.'L') then
                            zl(jce2v-1+iad2) = zl(jcesv-1+iad)
                        else if (tsca.eq.'K8') then
                            zk8(jce2v-1+iad2) = zk8(jcesv-1+iad)
                        else if (tsca.eq.'K16') then
                            zk16(jce2v-1+iad2) = zk16(jcesv-1+iad)
                        else if (tsca.eq.'K24') then
                            zk24(jce2v-1+iad2) = zk24(jcesv-1+iad)
                        else if (tsca.eq.'K32') then
                            zk32(jce2v-1+iad2) = zk32(jcesv-1+iad)
                        else if (tsca.eq.'K80') then
                            zk80(jce2v-1+iad2) = zk80(jcesv-1+iad)
                        else
                            ASSERT(.false.)
                        endif
                    endif
                end do
            end do
        end do
    end do
!
!
!
    call detrsd('CHAM_ELEM_S', ces)
    call copisd('CHAM_ELEM_S', base, ces2, ces)
!
!
!
!     7- MENAGE :
!     -----------
    call detrsd('CHAM_ELEM_S', ces2)
    AS_DEALLOCATE(vi=vnbpt)
    AS_DEALLOCATE(vi=vnbsp)
    AS_DEALLOCATE(vi=vnbcmp)
!
    call jedema()
end subroutine
