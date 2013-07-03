subroutine cnsdot(cns1z, cns2z, pscal, ier)
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
    integer :: jcnsk1, jcnsd1, jcnsv1, jcnsl1, jcnsc1
    integer :: jcnsk2, jcnsd2, jcnsv2, jcnsl2, jcnsc2
    integer :: nbno, ibid, k, ino, ncmp, nbno1, nbno2, ncmp1, ncmp2
    character(len=8) :: ma1, nomgd1, ma2, nomgd2
    character(len=3) :: tsca1
    character(len=19) :: cns1, cns2
    real(kind=8) :: x1, x2
!     ------------------------------------------------------------------
    call jemarq()
!
!
    cns1=cns1z
    cns2=cns2z
!
    call jeveuo(cns1//'.CNSK', 'L', jcnsk1)
    call jeveuo(cns1//'.CNSD', 'L', jcnsd1)
    call jeveuo(cns1//'.CNSC', 'L', jcnsc1)
    call jeveuo(cns1//'.CNSV', 'L', jcnsv1)
    call jeveuo(cns1//'.CNSL', 'L', jcnsl1)
!
    ma1=zk8(jcnsk1-1+1)
    nomgd1=zk8(jcnsk1-1+2)
    nbno1=zi(jcnsd1-1+1)
    ncmp1=zi(jcnsd1-1+2)
!
!
    call jeveuo(cns2//'.CNSK', 'L', jcnsk2)
    call jeveuo(cns2//'.CNSD', 'L', jcnsd2)
    call jeveuo(cns2//'.CNSC', 'L', jcnsc2)
    call jeveuo(cns2//'.CNSV', 'L', jcnsv2)
    call jeveuo(cns2//'.CNSL', 'L', jcnsl2)
!
    ma2=zk8(jcnsk2-1+1)
    nomgd2=zk8(jcnsk2-1+2)
    nbno2=zi(jcnsd2-1+1)
    ncmp2=zi(jcnsd2-1+2)
!
!     -- COHERENCE DES 2 CHAMPS :
    call assert(ma1.eq.ma2)
    call assert(nomgd1.eq.nomgd2)
    call assert(nbno1.eq.nbno2)
    call assert(ncmp1.eq.ncmp2)
    nbno=nbno1
    ncmp=ncmp1
!
!
!
    call dismoi('F', 'TYPE_SCA', nomgd1, 'GRANDEUR', ibid,&
                tsca1, ibid)
    call assert(tsca1.eq.'R')
!
    do 10,k=1,ncmp
    call assert(zk8(jcnsc1-1+k).eq.zk8(jcnsc2-1+k))
    10 end do
!
!
!     CALCUL DE LA SOMME DES PRODUITS DES CMPS :
!     -------------------------------------------
    pscal=0.d0
    ier=0
    do 30,ino=1,nbno
    do 20,k=1,ncmp
    if (zl(jcnsl1-1+(ino-1)*ncmp+k)) then
        if (.not.zl(jcnsl2-1+(ino-1)*ncmp+k)) then
            ier=1
            goto 40
!
        endif
        x1=zr(jcnsv1-1+(ino-1)*ncmp+k)
        x2=zr(jcnsv2-1+(ino-1)*ncmp+k)
        pscal=pscal+x1*x2
    else
        if (zl(jcnsl2-1+(ino-1)*ncmp+k)) then
            ier=1
            goto 40
!
        endif
    endif
20  continue
    30 end do
!
40  continue
    call jedema()
end subroutine
