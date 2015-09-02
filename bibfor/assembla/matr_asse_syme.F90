subroutine matr_asse_syme(matas)
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelibe.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"

    character(len=*) :: matas
!-----------------------------------------------------------------------
! But : symetriser une matrice non-symetrique
!       A= 1/2(A+A^t)
!
! in/jxvar  k* matas : nom de la sd_matr_asse a symetriser
!
! Attention : La routine modifie l'objet matas.
!             On ne pourra plus revenir en arriere.
!-----------------------------------------------------------------------


    character(len=19) :: matas1
    character(len=1) :: bas1
    character(len=3) :: tysca
    integer :: n1,n2,k1,k2,jvalm1,jvalm2
    character(len=24), pointer :: refa(:) => null()
!-------------------------------------------------------------------
    call jemarq()
    matas1=matas
    call jelira(matas1//'.VALM', 'NUTIOC', n1)
    ASSERT(n1.eq.1 .or. n1.eq.2)

!   -- si la matrice est deja symetrique, il n'y a rien a faire :
    if (n1.eq.1) goto 999

    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
    ASSERT(refa(9).eq.'MR')
    call jelira(matas1//'.VALM', 'CLAS', cval=bas1)
    call jelira(matas1//'.VALM', 'TYPE', cval=tysca)

    call jedupo(matas1//'.VALM', 'V', '&&matr_asse_syme.VALM', .false._1)
    call jelira(jexnum(matas1//'.VALM',1),'LONMAX',n2)
    call jedetr(matas1//'.VALM')
    call jecrec(matas1//'.VALM', bas1//' V '//tysca, 'NU', 'DISPERSE',&
                'CONSTANT', 1)
    call jeecra(matas1//'.VALM', 'LONMAX', n2)
    call jecroc(jexnum(matas1//'.VALM', 1))
    call jeveuo(jexnum(matas1//'.VALM', 1),'E',jvalm1)
    do k1=1,2
        call jeveuo(jexnum('&&matr_asse_syme.VALM', k1),'L',jvalm2)
        if (tysca.eq.'R') then
            do k2=1,n2
                zr(jvalm1-1+k2)=zr(jvalm1-1+k2)+0.5d0*zr(jvalm2-1+k2)
            enddo
        elseif (tysca.eq.'C') then
            do k2=1,n2
                zc(jvalm1-1+k2)=zc(jvalm1-1+k2)+0.5d0*zc(jvalm2-1+k2)
            enddo
        else
            ASSERT(.false.)
        endif
        call jelibe(jexnum('&&matr_asse_syme.VALM', k1))
    enddo
    call jedetr('&&matr_asse_syme.VALM')
    refa(9)='MS'


999 continue
    call jedema()
end subroutine
