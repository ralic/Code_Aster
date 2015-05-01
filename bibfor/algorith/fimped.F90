subroutine fimped(modele, mate, numedd, neq, vitini,&
                  vitent, veccor, veanec, vaanec, temps,&
                  foimpe)
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
    implicit none
#include "jeveux.h"
#include "asterfort/asasve.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/reajre.h"
    integer :: i, ibid,   jvaanc, neq, npain
    character(len=8) :: lpain(5), lpaout(1)
    character(len=24) :: modele, mate, numedd, vitini, veccor
    character(len=24) :: vitent, chinst
    character(len=24) :: veanec, vaanec, lchin(5), lchout(1)
    character(len=24) :: chgeom, ligrel
    real(kind=8) :: foimpe(neq), temps
    character(len=8), pointer :: lgrf(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!
!-----------------------------------------------------------------------
    call jemarq()
!
    chinst = '&&CHINST'
    call mecact('V', chinst, 'MODELE', modele(1:8)//'.MODELE', 'INST_R',&
                ncmp=1, nomcmp='INST', sr=temps)
    call jedetr(veanec(1:19)//'.RELR')
!
    ligrel = modele(1:8)//'.MODELE'
    call jeveuo(ligrel(1:19)//'.LGRF', 'L', vk8=lgrf)
    chgeom = lgrf(1)//'.COORDO'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
!
    lpain(3) = 'PVITPLU'
    lchin(3) = vitini
    lpain(4) = 'PVITENT'
    lchin(4) = vitent
!
    lpain(5) = 'PTEMPSR'
    lchin(5) = chinst
!
    npain = 5
    lpaout(1) = 'PVECTUR'
    lchout(1) = veccor
!
    call calcul('S', 'IMPE_ABSO', ligrel, npain, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
!
    call corich('E', lchout(1), -1, ibid)
!
    call reajre(veanec, lchout(1), 'V')
    call asasve(veanec, numedd, 'R', vaanec)
!
    call jeveuo(vaanec, 'L', jvaanc)
    call jeveuo(zk24(jvaanc) (1:19)//'.VALE', 'L', vr=vale)
!
    do 10 i = 1, neq
        foimpe(i) = vale(i)
10  end do
    call detrsd('CHAMP_GD', zk24(jvaanc) (1:19))
!
!
    call jedema()
end subroutine
