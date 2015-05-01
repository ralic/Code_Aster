subroutine peaire(resu, modele, nbocc)
    implicit none
#include "jeveux.h"
#include "asterfort/getvem.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/peair1.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: nbocc
    character(len=*) :: resu, modele
!     ------------------------------------------------------------------
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
!     OPERATEUR   POST_ELEM
!     TRAITEMENT DU MOT CLE-FACTEUR "AIRE_INTERNE"
!     ------------------------------------------------------------------
!
    integer :: nbparr, ibid, iret, iocc, ng,  ngb, jgb, igb, nbb
    integer :: ifm, niv, iadgma
    parameter    ( nbparr = 3 )
    real(kind=8) :: valpar(nbparr), aire, long
    character(len=3) :: typarr(nbparr)
    character(len=8) :: k8b, noma
    character(len=16) :: noparr(nbparr)
    character(len=24) :: grpma
    complex(kind=8) :: c16b
    integer :: iarg
    character(len=8), pointer :: lgrf(:) => null()
!     ------------------------------------------------------------------
    data noparr / 'GROUP_MA' , 'AIRE' , 'LONGUEUR' /
    data typarr / 'K24' , 'R' , 'R' /
!     ------------------------------------------------------------------
!
    call jemarq()
    ibid=0
    c16b=(0.d0,0.d0)
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ifm, niv)
!
    call jeveuo(modele(1:8)//'.MODELE    .LGRF', 'L', vk8=lgrf)
    noma = lgrf(1)
    grpma = noma//'.GROUPEMA'
!
!     --- CREATION DE LA TABLE ---
!
    call tbcrsd(resu, 'G')
    call tbajpa(resu, nbparr, noparr, typarr)
!
    do 10 iocc = 1, nbocc
        call getvem(noma, 'GROUP_MA', 'AIRE_INTERNE', 'GROUP_MA_BORD', iocc,&
                    iarg, 0, k8b, ngb)
        if (ngb .ne. 0) then
            ngb = -ngb
            call wkvect('&&PEAIRE.GROUP_NO', 'V V K24', ngb, jgb)
            call getvem(noma, 'GROUP_MA', 'AIRE_INTERNE', 'GROUP_MA_BORD', iocc,&
                        iarg, ngb, zk24(jgb), ng)
            do 40 igb = 1, ngb
                call jeexin(jexnom(grpma, zk24(jgb+igb-1)), iret)
                if (iret .eq. 0) then
                    call utmess('A', 'UTILITAI3_46', sk=zk24(jgb+igb-1))
                    goto 40
                endif
                call jelira(jexnom(grpma, zk24(jgb+igb-1)), 'LONMAX', nbb)
                if (nbb .eq. 0) then
                    call utmess('A', 'UTILITAI3_47', sk=zk24(jgb+igb-1))
                    goto 40
                endif
                call jeveuo(jexnom(grpma, zk24(jgb+igb-1)), 'L', iadgma)
!
!              BORD DU TROU : CALCUL DE L'AIRE
!
                call peair1(modele, nbb, zi(iadgma), aire, long)
                valpar(1) = aire
                valpar(2) = long
                call tbajli(resu, nbparr, noparr, [ibid], valpar,&
                            [c16b], zk24(jgb+igb-1), 0)
40          continue
            call jedetr('&&PEAIRE.GROUP_NO')
        endif
10  end do
!
    call jedema()
end subroutine
