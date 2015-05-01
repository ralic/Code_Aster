subroutine rdtces(ma2, corrm, ces1, base, ces2,&
                  codret)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: codret
    character(len=8) :: ma2
    character(len=19) :: ces1, ces2
    character(len=*) :: corrm
    character(len=1) :: base
! ---------------------------------------------------------------------
! BUT: REDUIRE UN CHAM_ELEM_S SUR UN MAILLAGE REDUIT
! ---------------------------------------------------------------------
! ARGUMENTS:
! MA2    IN       K8  : MAILLAGE REDUIT
! CES1   IN/JXIN  K19 : CHAM_ELEM_S A REDUIRE
! CES2   IN/JXOUT K19 : CHAM_ELEM_S REDUIT
! BASE   IN       K1  : BASE DE CREATION POUR CES2Z : G/V/L
! CORRM  IN       K*  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
!                       IMA_RE -> IMA
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: jcorrm, nbma2, jnbpt, jnbsp, jnbcmp
    integer :: jce1d, jce1v, jce1l, ima1, ima2, nbpt, nbsp, iad1
    integer :: jce2d, jce2v, jce2l, jce2c
    integer :: ipt, isp, iad2
    integer :: ncmp, icmp
    character(len=8) :: nomgd, typces
    character(len=3) :: tsca
    aster_logical :: isvide
    character(len=8), pointer :: cesk(:) => null()
    character(len=8), pointer :: ce1c(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    ASSERT(ces2.ne.' ')
    ASSERT(ces1.ne.ces2)
    codret=1
!
!
!     1- RECUPERATION D'INFORMATIONS DANS CES1 :
!     ------------------------------------------
    call jeveuo(ces1//'.CESK', 'L', vk8=cesk)
    call jeveuo(ces1//'.CESD', 'L', jce1d)
    call jeveuo(ces1//'.CESC', 'L', vk8=ce1c)
    call jeveuo(ces1//'.CESV', 'L', jce1v)
    call jeveuo(ces1//'.CESL', 'L', jce1l)
!
    nomgd=cesk(2)
    typces=cesk(3)
    ncmp=zi(jce1d-1+2)
!
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
!
!
!     2- CREATION DE 3 OBJETS CONTENANT LES NOMBRES DE POINTS,
!         SOUS-POINTS ET CMPS POUR CHAQUE MAILLE :
!     -----------------------------------------------------------
    call dismoi('NB_MA_MAILLA', ma2, 'MAILLAGE', repi=nbma2)
    call jeveuo(corrm, 'L', jcorrm)
    call wkvect('&&CESRED.NBPT', 'V V I', nbma2, jnbpt)
    call wkvect('&&CESRED.NBSP', 'V V I', nbma2, jnbsp)
    call wkvect('&&CESRED.NBCMP', 'V V I', nbma2, jnbcmp)
    isvide=.true.
    do ima2 = 1, nbma2
        ima1=zi(jcorrm-1+ima2)
        zi(jnbpt-1+ima2)=zi(jce1d-1+5+4*(ima1-1)+1)
        zi(jnbsp-1+ima2)=zi(jce1d-1+5+4*(ima1-1)+2)
        zi(jnbcmp-1+ima2)=min(zi(jce1d-1+5+4*(ima1-1)+3),ncmp)
        if (zi(jnbpt-1+ima2) .ne. 0 .and. zi(jnbsp-1+ima2) .ne. 0 .and. zi( jnbcmp-1+ima2)&
            .ne. 0) then
            isvide=.false.
        endif
    end do
    if (isvide) goto 60
    codret=0
!
!
!     3- CREATION DE CES2 :
!     ---------------------------------------
    call cescre(base, ces2, typces, ma2, nomgd,&
                ncmp, ce1c, zi(jnbpt), zi(jnbsp), zi(jnbcmp))
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
!
!
!
!     4- REMPLISSAGE DES OBJETS .CESL ET .CESV :
!     ------------------------------------------
    do icmp = 1, ncmp
!
        do ima2 = 1, nbma2
            ima1=zi(jcorrm-1+ima2)
            nbpt=zi(jce2d-1+5+4*(ima2-1)+1)
            nbsp=zi(jce2d-1+5+4*(ima2-1)+2)
            do ipt = 1, nbpt
                do isp = 1, nbsp
                    call cesexi('C', jce1d, jce1l, ima1, ipt,&
                                isp, icmp, iad1)
                    call cesexi('C', jce2d, jce2l, ima2, ipt,&
                                isp, icmp, iad2)
                    ASSERT(iad2.le.0)
                    if ((iad1.le.0) .or. (iad2.eq.0)) goto 20
!
!               -- RECOPIE DE LA VALEUR:
                    zl(jce2l-1-iad2)=.true.
                    if (tsca .eq. 'R') then
                        zr(jce2v-1-iad2)=zr(jce1v-1+iad1)
                    else if (tsca.eq.'C') then
                        zc(jce2v-1-iad2)=zc(jce1v-1+iad1)
                    else if (tsca.eq.'I') then
                        zi(jce2v-1-iad2)=zi(jce1v-1+iad1)
                    else if (tsca.eq.'L') then
                        zl(jce2v-1-iad2)=zl(jce1v-1+iad1)
                    else if (tsca.eq.'K8') then
                        zk8(jce2v-1-iad2)=zk8(jce1v-1+iad1)
                    else if (tsca.eq.'K16') then
                        zk16(jce2v-1-iad2)=zk16(jce1v-1+iad1)
                    else if (tsca.eq.'K24') then
                        zk24(jce2v-1-iad2)=zk24(jce1v-1+iad1)
                    else if (tsca.eq.'K32') then
                        zk32(jce2v-1-iad2)=zk32(jce1v-1+iad1)
                    else if (tsca.eq.'K80') then
                        zk80(jce2v-1-iad2)=zk80(jce1v-1+iad1)
                    else
                        ASSERT(.false.)
                    endif
!
 20                 continue
                end do
            end do
!
        end do
    end do
!
!
!     5- MENAGE :
!     -----------
 60 continue
    call jedetr('&&CESRED.NBPT')
    call jedetr('&&CESRED.NBSP')
    call jedetr('&&CESRED.NBCMP')
!
!
!
    call jedema()
end subroutine
