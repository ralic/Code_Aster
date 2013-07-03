subroutine rdtcns(ma2, corrn, cns1, base, cns2)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=8) :: ma2
    character(len=19) :: cns1, cns2
    character(len=*) :: corrn
    character(len=1) :: base
! ---------------------------------------------------------------------
! BUT: REDUIRE UN CHAM_NO_S SUR UN MAILLAGE REDUIT
! ---------------------------------------------------------------------
! ARGUMENTS:
! MA2    IN       K8  : MAILLAGE REDUIT
! CNS1   IN/JXIN  K19 : CHAM_NO_S A REDUIRE
! CNS2   IN/JXOUT K19 : CHAM_NO_S REDUIT
! BASE   IN       K1  : BASE DE CREATION POUR CNS2Z : G/V/L
! CORRN  IN       K*  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
!                       INO_RE -> INO
!
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: nbno1, nbno2, jcorrn
    integer :: jcn1k, jcn1d, jcn1v, jcn1l, jcn1c
    integer :: jcn2d, jcn2v, jcn2l, jcn2c
    integer :: ibid
    integer :: ncmp, icmp
    integer :: ino2, ino1
    character(len=8) :: nomgd
    character(len=3) :: tsca
!     ------------------------------------------------------------------
    call jemarq()
!
    call assert(cns2.ne.' ')
    call assert(cns1.ne.cns2)
!
    call jeveuo(cns1//'.CNSK', 'L', jcn1k)
    call jeveuo(cns1//'.CNSD', 'L', jcn1d)
    call jeveuo(cns1//'.CNSC', 'L', jcn1c)
    call jeveuo(cns1//'.CNSV', 'L', jcn1v)
    call jeveuo(cns1//'.CNSL', 'L', jcn1l)
!
    nomgd=zk8(jcn1k-1+2)
    nbno1=zi(jcn1d-1+1)
    ncmp=zi(jcn1d-1+2)
    call assert(ncmp.gt.0)
!
    call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                tsca, ibid)
!
!
!
!     1- CREATION DE CNS2 :
!     ---------------------------------------
    call cnscre(ma2, nomgd, ncmp, zk8(jcn1c), base,&
                cns2)
    call jeveuo(cns2//'.CNSD', 'L', jcn2d)
    call jeveuo(cns2//'.CNSC', 'L', jcn2c)
    call jeveuo(cns2//'.CNSV', 'E', jcn2v)
    call jeveuo(cns2//'.CNSL', 'E', jcn2l)
    nbno2=zi(jcn2d-1+1)
    call assert(nbno2.gt.0)
    call assert(nbno2.le.nbno1)
!
!
!     3- REMPLISSAGE DES OBJETS .CNSL ET .CNSV :
!     ------------------------------------------
    call jeveuo(corrn, 'L', jcorrn)
!
    do 20,icmp=1,ncmp
!
    do 10,ino2=1,nbno2
    ino1=zi(jcorrn-1+ino2)
    if (zl(jcn1l-1+(ino1-1)*ncmp+icmp)) then
        zl(jcn2l-1+(ino2-1)*ncmp+icmp)=.true.
!
        if (tsca .eq. 'R') then
            zr(jcn2v-1+(ino2-1)*ncmp+icmp)=zr(jcn1v-1+(ino1-1)&
                    *ncmp+ icmp)
        else if (tsca.eq.'C') then
            zc(jcn2v-1+(ino2-1)*ncmp+icmp)=zc(jcn1v-1+(ino1-1)&
                    *ncmp+ icmp)
        else if (tsca.eq.'I') then
            zi(jcn2v-1+(ino2-1)*ncmp+icmp)=zi(jcn1v-1+(ino1-1)&
                    *ncmp+ icmp)
        else if (tsca.eq.'L') then
            zl(jcn2v-1+(ino2-1)*ncmp+icmp)=zl(jcn1v-1+(ino1-1)&
                    *ncmp+ icmp)
        else if (tsca.eq.'K8') then
            zk8(jcn2v-1+(ino2-1)*ncmp+icmp)=zk8(jcn1v-1+(ino1-&
                    1)*ncmp+ icmp)
        else
            call assert(.false.)
        endif
!
    endif
!
10  continue
    20 end do
!
!
    call jedema()
end subroutine
