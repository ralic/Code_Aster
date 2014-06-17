subroutine fondpl(modele, mate, numedd, neq, chondp,&
                  nchond, vecond, veonde, vaonde, temps,&
                  foonde)
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
    implicit none
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/reajre.h"
!
    integer :: i, ibid, iret, j,   jvaond
    integer :: nchond, neq, npain
    character(len=8) :: lpain(5), lpaout(1), chondp(nchond)
    character(len=24) :: modele, mate, numedd, vecond
    character(len=24) :: chinst
    character(len=24) :: veonde, vaonde, lchin(5), lchout(1)
    character(len=24) :: chgeom, ligrel
    real(kind=8) :: foonde(neq), temps
    character(len=8), pointer :: lgrf(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!
!-----------------------------------------------------------------------
    call jemarq()
!
    do 10 i = 1, neq
        foonde(i) = 0.d0
10  end do
!
    chinst = '&&CHINST'
    call mecact('V', chinst, 'MODELE', modele(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=temps)
    ligrel = modele(1:8)//'.MODELE'
    call jeveuo(ligrel(1:19)//'.LGRF', 'L', vk8=lgrf)
    chgeom = lgrf(1)//'.COORDO'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
!
    lpain(3) = 'PTEMPSR'
    lchin(3) = chinst
!
    lpain(4) = 'PONDPLA'
    lpain(5) = 'PONDPLR'
!
    npain = 5
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = vecond
!
    do 30 i = 1, nchond
        call exisd('CARTE', chondp(i)//'.CHME.ONDPL', iret)
        call exisd('CARTE', chondp(i)//'.CHME.ONDPR', ibid)
        if (iret .ne. 0 .and. ibid .ne. 0) then
            lchin(4) = chondp(i)//'.CHME.ONDPL.DESC'
            lchin(5) = chondp(i)//'.CHME.ONDPR.DESC'
!
            call calcul('S', 'ONDE_PLAN', ligrel, npain, lchin,&
                        lpain, 1, lchout, lpaout, 'V',&
                        'OUI')
!
            call corich('E', lchout(1), -1, ibid)
            call jedetr(veonde(1:19)//'.RELR')
            call reajre(veonde, lchout(1), 'V')
            call asasve(veonde, numedd, 'R', vaonde)
!
            call jeveuo(vaonde, 'L', jvaond)
            call jeveuo(zk24(jvaond) (1:19)//'.VALE', 'L', vr=vale)
!
            do 20 j = 1, neq
                foonde(j) = foonde(j) + vale(j)
20          continue
            call detrsd('CHAMP_GD', zk24(jvaond) (1:19))
!
        endif
30  end do
!
!
    call jedema()
end subroutine
