subroutine vechbn(mdgene, nomno1, sst1, nomno2, sst2)
    implicit none
! ----------------------------------------------------------------------
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
!
!     BUT: S'ASSURER QUE LA DEFINITION DES NON-LINEARITES EST COMPATIBLE
!          AVEC LA DEFINITION DU MODELE GENERALISE
!
! ----------------------------------------------------------------------
!
!
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
!
!
    character(len=8) :: nomno1, nomno2, sst1, sst2, ss1, ss2, macro1, macro2
    character(len=8) :: liai1, liai2, noeud1, noeud2, k8bid
    character(len=24) :: mdgene, mdliai, mdnoms, bamo1, bamo2, intf1, intf2
    character(len=24) :: valk(4)
    character(len=24) :: lino1, lino2, maya1, maya2
!
!-----------------------------------------------------------------------
!
! --- LES SOUS-STRUCTURES SST1 ET SST2 SONT-ELLES LIAISONNEES
!
!-----------------------------------------------------------------------
    integer :: ibid, iliai, inoeu, jlino1, jlino2, jnoeu1, jnoeu2
    integer :: ir, ldefo1, ldefo2, lliai, llino1, llino2
    integer :: llnom1, llnom2, lmacr1, lmacr2, lrefe1, lrefe2, nbliai
    integer :: nbnoeu
!-----------------------------------------------------------------------
    call jemarq()
    mdliai = mdgene(1:14)//'.MODG.LIDF'
    mdnoms = mdgene(1:14)//'.MODG.SSME'
!
    k8bid = '        '
    call jelira(mdliai, 'NUTIOC', nbliai)
!
    do 10 iliai = 1, nbliai
        call jeveuo(jexnum(mdliai, iliai), 'L', lliai)
        ss1 = zk8(lliai)
        ss2 = zk8(lliai+2)
!
        if ((ss1.eq.sst1.and.ss2.eq.sst2) .or. ( ss1.eq.sst2.and.ss2.eq.sst1)) then
            liai1 = zk8(lliai+1)
            liai2 = zk8(lliai+3)
            call jenonu(jexnom(mdnoms(1:19)//'.SSNO', ss1), ibid)
            call jeveuo(jexnum(mdnoms, ibid), 'L', llnom1)
            call jenonu(jexnom(mdnoms(1:19)//'.SSNO', ss2), ibid)
            call jeveuo(jexnum(mdnoms, ibid), 'L', llnom2)
            macro1 = zk8(llnom1)
            macro2 = zk8(llnom2)
            call jeveuo(macro1//'.MAEL_MASS_REFE', 'L', lmacr1)
            call jeveuo(macro2//'.MAEL_MASS_REFE', 'L', lmacr2)
            bamo1 = zk24(lmacr1)
            bamo2 = zk24(lmacr2)
            call dismoi('F', 'REF_INTD_PREM', bamo1, 'RESU_DYNA', ibid,&
                        intf1, ir)
            call dismoi('F', 'REF_INTD_PREM', bamo2, 'RESU_DYNA', ibid,&
                        intf2, ir)
            lino1 = intf1(1:8)//'.IDC_LINO'
            lino2 = intf2(1:8)//'.IDC_LINO'
            call jenonu(jexnom(lino1(1:13)//'NOMS', liai1), ibid)
            call jeveuo(jexnum(lino1, ibid), 'L', llino1)
            call jenonu(jexnom(lino2(1:13)//'NOMS', liai2), ibid)
            call jeveuo(jexnum(lino2, ibid), 'L', llino2)
            k8bid = '        '
            call jelira(jexnum(lino2, ibid), 'LONMAX', nbnoeu)
            call jeveuo(intf1(1:8)//'.IDC_DEFO', 'L', ldefo1)
            call jeveuo(intf2(1:8)//'.IDC_DEFO', 'L', ldefo2)
            call jeveuo(intf1(1:8)//'.IDC_REFE', 'L', lrefe1)
            call jeveuo(intf2(1:8)//'.IDC_REFE', 'L', lrefe2)
            maya1 = zk24(lrefe1)(1:8)//'.NOMNOE'
            maya2 = zk24(lrefe2)(1:8)//'.NOMNOE'
!
! ------- LES NOEUDS NOMNO1 ET NOMNO2 SONT-ILS LIAISONNES
!
            do 20 inoeu = 1, nbnoeu
                jlino1 = zi(llino1-1+inoeu)
                jlino2 = zi(llino2-1+inoeu)
                jnoeu1 = zi(ldefo1+jlino1-1)
                jnoeu2 = zi(ldefo2+jlino2-1)
                call jenuno(jexnum(maya1, jnoeu1), noeud1)
                call jenuno(jexnum(maya2, jnoeu2), noeud2)
!
                if ((&
                    ss1 .eq. sst1 .and. ss2 .eq. sst2 .and. nomno1 .eq. noeud1 .and. nomno2&
                    .eq. noeud2&
                    )&
                    .or.&
                    (&
                    ss1 .eq. sst2 .and. ss2 .eq. sst1 .and. nomno1 .eq. noeud2 .and. nomno2&
                    .eq. noeud1&
                    )) then
!
                    valk (1) = nomno1
                    valk (2) = sst1
                    valk (3) = nomno2
                    valk (4) = sst2
                    call utmess('F', 'ALGORITH14_69', nk=4, valk=valk)
!
                endif
!
20          continue
!
        endif
!
10  continue
!
    call jedema()
end subroutine
