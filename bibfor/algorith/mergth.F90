subroutine mergth(modele, charge, infcha, carele, mate,&
                  inst, merigi)
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
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/meharm.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=24) :: modele, charge, infcha, carele, inst, merigi, mate
! ----------------------------------------------------------------------
! CALCUL DES MATRICES ELEMENTAIRES DE RIGIDITE THERMIQUE
!  - TERMES DE VOLUME
!  - TERMES DE SURFACE DUS AUX CONDITIONS LIMITES
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : CHAMP DE MATERIAU
! IN  INST    : CARTE CONTENANT LA VALEUR DE L'INSTANT
! OUT MERIGI  : MATRICES ELEMENTAIRES
!
!
!
    character(len=8) :: nomcha, lpain(15), lpaout(1)
    character(len=16) :: option
    character(len=24) :: ligrel(2), lchin(15), lchout(1), ligcal
    character(len=24) :: chgeom, chcara(18), chharm
    character(len=19) :: chvarc, stano, pintto, cnseto, heavto, loncha, basloc
    character(len=19) :: lsn, lst, pinter, ainter, cface, longco, baseco
    integer :: iret, nchar, ilires, icha, jchar, jinf
    logical :: exicar, lxfem
! ----------------------------------------------------------------------
    integer :: nbchmx
!-----------------------------------------------------------------------
    integer :: iret3, k, nh
!-----------------------------------------------------------------------
    parameter (nbchmx=2)
    integer :: nligr(nbchmx)
    character(len=6) :: nompar(nbchmx), nomchp(nbchmx), nomopt(nbchmx)
    data nomchp/'.COEFH','.HECHP'/
    data nomopt/'_COEH_','_PARO_'/
    data nompar/'PCOEFH','PHECHP'/
    data nligr/1,2/
    chvarc = '&&NXACMV.CHVARC'
!
    call jemarq()
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar)
        call jeveuo(charge, 'L', jchar)
    else
        nchar = 0
    endif
!
    call megeom(modele, chgeom)
    call mecara(carele, exicar, chcara)
!
!  ON INITIALISE LE NUMERO D'HARMONIQUE A -1 POUR EMETTRE UN MESSAGE
!  FATAL S'IL Y A DES ELEMENTS DE FOURIER DANS LE MODELE
!
    nh = -1
    call meharm(modele, nh, chharm)
!
    call jeexin(merigi, iret)
    if (iret .eq. 0) then
        merigi = '&&MERIGI           .RELR'
        call memare('V', merigi(1:19), modele(1:8), mate, carele,&
                    'RIGI_THER')
    else
        call jedetr(merigi)
    endif
    ligrel(1) = modele(1:8)//'.MODELE'
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = merigi(1:8)//'.ME001'
    ilires = 0
!
!     CADRE X-FEM
    lxfem = .false.
    call exixfe(modele, iret)
    if (iret .ne. 0) lxfem = .true.
    if (lxfem) then
        stano = modele(1:8)//'.STNO'
        pintto = modele(1:8)//'.TOPOSE.PIN'
        cnseto = modele(1:8)//'.TOPOSE.CNS'
        heavto = modele(1:8)//'.TOPOSE.HEA'
        loncha = modele(1:8)//'.TOPOSE.LON'
        basloc = modele(1:8)//'.BASLOC'
        lsn = modele(1:8)//'.LNNO'
        lst = modele(1:8)//'.LTNO'
        pinter = modele(1:8)//'.TOPOFAC.OE'
        ainter = modele(1:8)//'.TOPOFAC.AI'
        cface = modele(1:8)//'.TOPOFAC.CF'
        longco = modele(1:8)//'.TOPOFAC.LO'
        baseco = modele(1:8)//'.TOPOFAC.BA'
    else
        stano = '&&MERGTH.STNO.BID'
        pintto = '&&MERGTH.PINTTO.BID'
        cnseto = '&&MERGTH.CNSETO.BID'
        heavto = '&&MERGTH.HEAVTO.BID'
        loncha = '&&MERGTH.LONCHA.BID'
        basloc = '&&MERGTH.BASLOC.BID'
        lsn = '&&MERGTH.LNNO.BID'
        lst = '&&MERGTH.LTNO.BID'
        pinter = '&&MERGTH.PINTER.BID'
        ainter = '&&MERGTH.AINTER.BID'
        cface = '&&MERGTH.CFACE.BID'
        longco = '&&MERGTH.LONGCO.BID'
        baseco = '&&MERGTH.BASECO.BID'
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
        lpain(5) = 'PHARMON'
        lchin(5) = chharm
        lpain(6) = 'PCAMASS'
        lchin(6) = chcara(12)
        lpain(7) = 'PVARCPR'
        lchin(7) = chvarc
        lpain(8) = 'PSTANO'
        lchin(8) = stano
        lpain(9) = 'PPINTTO'
        lchin(9) = pintto
        lpain(10) = 'PCNSETO'
        lchin(10) = cnseto
        lpain(11) = 'PHEAVTO'
        lchin(11) = heavto
        lpain(12) = 'PLONCHA'
        lchin(12) = loncha
        lpain(13) = 'PBASLOR'
        lchin(13) = basloc
        lpain(14) = 'PLSN'
        lchin(14) = lsn
        lpain(15) = 'PLST'
        lchin(15) = lst
        option = 'RIGI_THER'
        ilires = ilires + 1
        call codent(ilires, 'D0', lchout(1) (12:14))
        call calcul('S', option, ligrel(1), 15, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(merigi, lchout(1), 'V')
    endif
!
    if (nchar .gt. 0) then
        call jeveuo(infcha, 'L', jinf)
        do 20 icha = 1, nchar
            if (zi(jinf+nchar+icha) .ne. 0) then
                nomcha = zk24(jchar+icha-1) (1:8)
                ligrel(2) = nomcha//'.CHTH.LIGRE'
                lpain(1) = 'PGEOMER'
                lchin(1) = chgeom
                lpain(2) = 'PTEMPSR'
                lchin(2) = inst
!
                if (zi(jinf+nchar+icha) .eq. 1) then
                    option = 'RIGI_THER_    _R'
                    lpain(3) = '      R'
                else
                    option = 'RIGI_THER_    _F'
                    lpain(3) = '      F'
                endif
!
!           CADRE X-FEM
                lpain(4) = 'PPINTER'
                lchin(4) = pinter
                lpain(5) = 'PAINTER'
                lchin(5) = ainter
                lpain(6) = 'PCFACE'
                lchin(6) = cface
                lpain(7) = 'PLONGCO'
                lchin(7) = longco
                lpain(8) = 'PLST'
                lchin(8) = lst
                lpain(9) = 'PSTANO'
                lchin(9) = stano
                lpain(10) = 'PBASECO'
                lchin(10) = baseco
!
                do 10 k = 1, nbchmx
                    lchin(3) = nomcha//'.CHTH'//nomchp(k)//'.DESC'
                    call jeexin(lchin(3), iret3)
                    if (iret3 .gt. 0) then
                        if (lxfem) then
                            ligcal = ligrel(1)
                        else
                            ligcal = ligrel(nligr(k))
                        endif
                        option(10:15) = nomopt(k)
                        lpain(3) (1:6) = nompar(k)
                        ilires = ilires + 1
                        call codent(ilires, 'D0', lchout(1) (12:14))
                        call calcul('S', option, ligcal, 10, lchin,&
                                    lpain, 1, lchout, lpaout, 'V',&
                                    'OUI')
                        call reajre(merigi, lchout(1), 'V')
                    endif
10              continue
            endif
20      continue
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
