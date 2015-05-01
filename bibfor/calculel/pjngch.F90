subroutine pjngch(cham1z, cham2z, corres)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/chpnua.h"
#include "asterfort/chpver.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nuachp.h"
#include "asterfort/pronua.h"
#include "asterfort/wkvect.h"
!
    character(len=*) :: cham1z, cham2z, corres
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
!----------------------------------------------------------------------
! BUT : PROJETER UN CHAM_NO D'UN MAILLAGE SUR UN AUTRE POUR LES
!       METHODES NUAGE_DEG_0 ET NUAGE_DEG_1
!
! ARGUMENTS :
!  CHAM1 : IN/JXIN  CHAMP QUE L'ON VEUT PROJETER
!  CHAM2 : IN/JXOUT CHAMP RESULTAT DE LA PROJECTION
!  CORRES: IN/JXIN  SD_CORRESP_2_MAILLA
!----------------------------------------------------------------------
    integer :: ibid, nx,  jngi1, jngi2, ieq, nbocc, nbeq, ioc
    integer :: nb1, nb2, idec1, idec2, jlno1, jlno2
    character(len=1) :: type
    character(len=8) :: noma1, noma2, cnref, noma3, k8
    character(len=16) :: corr16, method
    character(len=19) :: nuage1, nuage2, cham1, cham2
    character(len=24) :: lno1, lno2
    real(kind=8), pointer :: vale(:) => null()
    character(len=24), pointer :: pjxx_k1(:) => null()
!
!
!----------------------------------------------------------------------
    call jemarq()
    corr16=corres
    cham1=cham1z
    cham2=cham2z
!
    call jeveuo(corr16//'.PJXX_K1', 'L', vk24=pjxx_k1)
    noma1=pjxx_k1(1)
    noma2=pjxx_k1(2)
    method=pjxx_k1(3)
    cnref=pjxx_k1(4)
    ASSERT(method(1:10).eq.'NUAGE_DEG_')
!
    call chpver('F', cham1, 'NOEU', '*', ibid)
    call chpver('F', cnref, 'NOEU', '*', ibid)
    call dismoi('NOM_MAILLA', cham1, 'CHAMP', repk=noma3)
    ASSERT(noma3.eq.noma1)
    call dismoi('NOM_MAILLA', cnref, 'CHAMP', repk=noma3)
    ASSERT(noma3.eq.noma2)
!
!
!     C'EST LE MAILLAGE MAILLA1 QUI IMPOSE LA DIMENSION D'ESPACE DES
!     DEUX NUAGES :
    nx=3
    call dismoi('Z_CST', noma1, 'MAILLAGE', repk=k8)
    if (k8 .eq. 'OUI') nx=2
!
    call jeveuo(corr16//'.PJNG_I1', 'L', jngi1)
    nbocc=zi(jngi1-1+1)
    if (nbocc .gt. 0) call jeveuo(corr16//'.PJNG_I2', 'L', jngi2)
!
!     -- CREATION DE CHAM2 VIERGE :
    call copisd('CHAMP_GD', 'G', cnref, cham2)
    call jelira(cham2//'.VALE', 'LONMAX', nbeq)
    call jelira(cham2//'.VALE', 'TYPE', cval=type)
    ASSERT(type.eq.'R')
    call jeveuo(cham2//'.VALE', 'E', vr=vale)
    if (type .eq. 'R') then
        do ieq = 0, nbeq-1
            vale(ieq+1)=0.d0
        end do
    else
        ASSERT(.false.)
    endif
!
    nuage1='&&NUAGE1'
    nuage2='&&NUAGE2'
!
    if (nbocc .gt. 0) then
        lno1='&&OP0166.LNO1'
        lno2='&&OP0166.LNO2'
        idec1=1+nbocc
        idec2=1+nbocc
        do ioc = 1, nbocc
            nb1=zi(jngi1-1+1+ioc)
            nb2=zi(jngi2-1+1+ioc)
            call wkvect(lno1, 'V V I', nb1, jlno1)
            call wkvect(lno2, 'V V I', nb2, jlno2)
            call jacopo(nb1, 'I', jngi1+idec1, jlno1)
            call jacopo(nb2, 'I', jngi2+idec2, jlno2)
!
            call chpnua(nx, cham1, lno1, nuage1)
            call chpnua(nx, cham2, lno2, nuage2)
            call pronua(method, nuage1, nuage2)
            call nuachp(nuage2, lno2, cham2)
            call detrsd('NUAGE', nuage1)
            call detrsd('NUAGE', nuage2)
            call jedetr(lno1)
            call jedetr(lno2)
            idec1=idec1+nb1
            idec2=idec2+nb2
        end do
!
    else
        call chpnua(nx, cham1, ' ', nuage1)
        call chpnua(nx, cham2, ' ', nuage2)
        call pronua(method, nuage1, nuage2)
        call nuachp(nuage2, ' ', cham2)
        call detrsd('NUAGE', nuage1)
        call detrsd('NUAGE', nuage2)
    endif
!
    call jedema()
end subroutine
