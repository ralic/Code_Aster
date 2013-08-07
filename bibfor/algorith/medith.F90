subroutine medith(modele, charge, infcha, mediri)
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
!
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/wkvect.h"
    character(len=24) :: modele, charge, infcha, mediri
!
! ----------------------------------------------------------------------
! CALCUL DES MATRICES ELEMENTAIRES DES ELEMENTS DE LAGRANGE
!
! C'EST UNE RECOPIE DE MEDIME OU L'ON MODIFIE CHME EN CHTH
!
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! OUT MEDIRI  : MATRICES ELEMENTAIRES
!
!
!
    character(len=8) :: nomcha, lpain(1), lpaout(1)
    character(len=19) :: matel
    character(len=16) :: option
    character(len=24) :: ligrch, lchin(1), lchout(1)
    integer :: iret, nchar, ilires, jmed, ichar, jchar, jinf
!
    call jemarq()
    call jeexin(charge, iret)
    if (iret .eq. 0) goto 20
    call jelira(charge, 'LONMAX', nchar)
    call jeveuo(charge, 'L', jchar)
!
    call jeexin(mediri, iret)
    if (iret .eq. 0) then
        matel = '&&METDIR           '
        mediri = matel//'.RELR'
        call memare('V', matel, modele(1:8), ' ', ' ',&
                    'RIGI_THER')
        call wkvect(mediri, 'V V K24', nchar, jmed)
    else
        call jeveuo(mediri, 'E', jmed)
    endif
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = mediri(1:8)//'.ME001'
!
    if (zk24(jchar) .ne. '        ') then
        ilires = 0
        call jeveuo(infcha, 'L', jinf)
        do 10 ichar = 1, nchar
            if (zi(jinf+ichar) .ne. 0) then
                nomcha = zk24(jchar+ichar-1) (1:8)
                ligrch = nomcha//'.CHTH.LIGRE'
!
                call jeexin(nomcha//'.CHTH.LIGRE.LIEL', iret)
                if (iret .le. 0) goto 10
                lchin(1) = nomcha//'.CHTH.CMULT'
                call exisd('CHAMP_GD', nomcha//'.CHTH.CMULT', iret)
                if (iret .le. 0) goto 10
!
                lpain(1) = 'PDDLMUR'
                call codent(ilires+1, 'D0', lchout(1) (12:14))
                option = 'THER_DDLM_R'
                call calcul('S', option, ligrch, 1, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                zk24(jmed+ilires) = lchout(1)
                ilires = ilires + 1
            endif
10      continue
        call jeecra(mediri, 'LONUTI', ilires)
    endif
20  continue
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
