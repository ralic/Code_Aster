subroutine memsth(modele, carele, mate, inst, memass)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=24) :: modele, carele, inst, memass, mate
! ----------------------------------------------------------------------
! CALCUL DES MATRICES ELEMENTAIRES DE MASSE THERMIQUE
!
! IN  MODELE  : NOM DU MODELE
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : MATERIAU CODE
! IN  INST    : CARTE CONTENANT LA VALEUR DU TEMPS
! OUT MEMASS  : MATRICES ELEMENTAIRES
!
!
!
    character(len=8) :: lpain(14), lpaout(1)
    character(len=16) :: option
    character(len=24) :: ligrmo, lchin(14), lchout(1)
    character(len=24) :: chgeom, chcara(18)
    character(len=19) :: chvarc, stano, pintto, cnseto, heavto, hea_no
    character(len=19) :: loncha, basloc, lsn, lst
    integer :: iret, ilires
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    chvarc = '&&NXACMV.CHVARC'
!
    call jemarq()
    call megeom(modele, chgeom)
    call mecara(carele, chcara)
!
    call jeexin(memass, iret)
    if (iret .eq. 0) then
        memass = '&&MEMASS           .RELR'
        call memare('V', memass(1:19), modele(1:8), mate, carele,&
                    'MASS_THER')
    endif
    ligrmo = modele(1:8)//'.MODELE'
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = memass(1:8)//'.ME001'
    ilires = 0
!
!     CADRE X-FEM
    call exixfe(modele, iret)
    if (iret .ne. 0) then
        stano = modele(1:8)//'.STNO'
        pintto = modele(1:8)//'.TOPOSE.PIN'
        cnseto = modele(1:8)//'.TOPOSE.CNS'
        heavto = modele(1:8)//'.TOPOSE.HEA'
        hea_no = modele(1:8)//'.TOPONO.HNO'
        loncha = modele(1:8)//'.TOPOSE.LON'
        basloc = modele(1:8)//'.BASLOC'
        lsn = modele(1:8)//'.LNNO'
        lst = modele(1:8)//'.LTNO'
    else
        stano = '&&MEMSTH.STNO.BID'
        pintto = '&&MEMSTH.PINTTO.BID'
        cnseto = '&&MEMSTH.CNSETO.BID'
        heavto = '&&MEMSTH.HEAVTO.BID'
        loncha = '&&MEMSTH.LONCHA.BID'
        basloc = '&&MEMSTH.BASLOC.BID'
        hea_no = '&&MEMSTH.HEA_NO.BID'
        lsn = '&&MEMSTH.LNNO.BID'
        lst = '&&MEMSTH.LTNO.BID'
    endif
!
    if (modele .ne. '        ') then
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PCACOQU'
        lchin(3) = chcara(7)
        lpain(4) = 'PTEMPSR'
        lchin(4) = inst
        lpain(5) = 'PVARCPR'
        lchin(5) = chvarc
        lpain(6) = 'PSTANO'
        lchin(6) = stano
        lpain(7) = 'PPINTTO'
        lchin(7) = pintto
        lpain(8) = 'PCNSETO'
        lchin(8) = cnseto
        lpain(9) = 'PHEAVTO'
        lchin(9) = heavto
        lpain(10) = 'PLONCHA'
        lchin(10) = loncha
        lpain(11) = 'PBASLOR'
        lchin(11) = basloc
        lpain(12) = 'PLSN'
        lchin(12) = lsn
        lpain(13) = 'PLST'
        lchin(13) = lst
        lpain(14) = 'PHEA_NO'
        lchin(14) = hea_no
        option = 'MASS_THER'
        ilires = 1
        call codent(ilires, 'D0', lchout(1) (12:14))
        call calcul('S', option, ligrmo, 14, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call jedetr(memass)
        call reajre(memass, lchout(1), 'V')
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
