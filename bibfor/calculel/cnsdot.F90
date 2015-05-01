subroutine cnsdot(cns1z, cns2z, pscal, ier)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: cns1z, cns2z
    real(kind=8) :: pscal
    integer :: ier
! ---------------------------------------------------------------------
! BUT: CALCULER LE "PRODUIT SCALAIRE" DE 2 CHAM_NO_S
! ---------------------------------------------------------------------
!     ARGUMENTS:
! CNS1Z  IN/JXIN  K19 : SD CHAM_NO_S
! CNS2Z  IN/JXIN  K19 : SD CHAM_NO_S
! PSCAL  OUT      R   : "PRODUIT SCALAIRE"
! IER    OUT      I   : CODE_RETOUR :
!                       0 -> OK
!                       1 -> LES 2 CHAM_NO_S N'ONT PAS LES MEMES CMPS
!
! REMARQUE :
! CETTE ROUTINE BOUCLE SUR TOUS LES DDLS DE CNS1 ET CNS2PSCAL=0
!  POUR IN IN CNS1 :
!      CMP1=CNS2
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer ::    jcnsl1
    integer ::    jcnsl2
    integer :: nbno, k, ino, ncmp, nbno1, nbno2, ncmp1, ncmp2
    character(len=8) :: ma1, nomgd1, ma2, nomgd2
    character(len=3) :: tsca1
    character(len=19) :: cns1, cns2
    real(kind=8) :: x1, x2
    character(len=8), pointer :: cnsk1(:) => null()
    character(len=8), pointer :: cnsk2(:) => null()
    character(len=8), pointer :: cnsc1(:) => null()
    character(len=8), pointer :: cnsc2(:) => null()
    real(kind=8), pointer :: cnsv1(:) => null()
    real(kind=8), pointer :: cnsv2(:) => null()
    integer, pointer :: cnsd1(:) => null()
    integer, pointer :: cnsd2(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
!
    cns1=cns1z
    cns2=cns2z
!
    call jeveuo(cns1//'.CNSK', 'L', vk8=cnsk1)
    call jeveuo(cns1//'.CNSD', 'L', vi=cnsd1)
    call jeveuo(cns1//'.CNSC', 'L', vk8=cnsc1)
    call jeveuo(cns1//'.CNSV', 'L', vr=cnsv1)
    call jeveuo(cns1//'.CNSL', 'L', jcnsl1)
!
    ma1=cnsk1(1)
    nomgd1=cnsk1(2)
    nbno1=cnsd1(1)
    ncmp1=cnsd1(2)
!
!
    call jeveuo(cns2//'.CNSK', 'L', vk8=cnsk2)
    call jeveuo(cns2//'.CNSD', 'L', vi=cnsd2)
    call jeveuo(cns2//'.CNSC', 'L', vk8=cnsc2)
    call jeveuo(cns2//'.CNSV', 'L', vr=cnsv2)
    call jeveuo(cns2//'.CNSL', 'L', jcnsl2)
!
    ma2=cnsk2(1)
    nomgd2=cnsk2(2)
    nbno2=cnsd2(1)
    ncmp2=cnsd2(2)
!
!     -- COHERENCE DES 2 CHAMPS :
    ASSERT(ma1.eq.ma2)
    ASSERT(nomgd1.eq.nomgd2)
    ASSERT(nbno1.eq.nbno2)
    ASSERT(ncmp1.eq.ncmp2)
    nbno=nbno1
    ncmp=ncmp1
!
!
!
    call dismoi('TYPE_SCA', nomgd1, 'GRANDEUR', repk=tsca1)
    ASSERT(tsca1.eq.'R')
!
    do k = 1, ncmp
        ASSERT(cnsc1(k).eq.cnsc2(k))
    end do
!
!
!     CALCUL DE LA SOMME DES PRODUITS DES CMPS :
!     -------------------------------------------
    pscal=0.d0
    ier=0
    do ino = 1, nbno
        do k = 1, ncmp
            if (zl(jcnsl1-1+(ino-1)*ncmp+k)) then
                if (.not.zl(jcnsl2-1+(ino-1)*ncmp+k)) then
                    ier=1
                    goto 40
!
                endif
                x1=cnsv1((ino-1)*ncmp+k)
                x2=cnsv2((ino-1)*ncmp+k)
                pscal=pscal+x1*x2
            else
                if (zl(jcnsl2-1+(ino-1)*ncmp+k)) then
                    ier=1
                    goto 40
!
                endif
            endif
        end do
    end do
!
 40 continue
    call jedema()
end subroutine
