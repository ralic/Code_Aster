subroutine merxth(modele, charge, infcha, carele, mate,&
                  inst, chtni, merigi, compor, varc_curr,&
                  tmpchi, tmpchf)
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
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=24) :: modele, charge, infcha, carele, inst, chtni, merigi
    character(len=24) :: mate, compor, tmpchi, tmpchf
    character(len=19), intent(in) :: varc_curr
! ----------------------------------------------------------------------
! CALCUL DES MATRICES TANGENTES ELEMENTAIRES
! EN THERMIQUE NON LINEAIRE
!  - TERMES DE VOLUME
!  - TERMES DE SURFACE DUS AUX CONDITIONS LIMITES ET CHARGEMENTS
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGEMENTS
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : MATERIAU CODE
! IN  INST    : CARTE CONTENANT LA VALEUR DE L'INSTANT
! IN  CHTNI   : IEME ITEREE DU CHAMP DE TEMPERATURE
! IN  COMPOR  : COMPORTEMENT (POUR LE SECHAGE)
! IN  TMPCHI  : CHAMP DE TEMPERAT. A T    (POUR LE CALCUL DE D-SECHAGE)
! IN  TMPCHI  : CHAMP DE TEMPERAT. A T+DT (POUR LE CALCUL DE D-SECHAGE)
! OUT MERIGI  : MATRICES ELEMENTAIRES
!
!
!
    character(len=8) :: nomcha, lpain(10), lpaout(1)
    character(len=16) :: option
    character(len=24) :: ligrel(2), lchin(10), lchout(1)
    character(len=24) :: chgeom, chcara(18)
    integer :: iret, nchar, ilires, icha, jchar, jinf
! ----------------------------------------------------------------------
    integer :: nbchmx
    parameter (nbchmx=5)
    integer :: nligr(nbchmx), k
    character(len=6) :: nomopr(nbchmx), nomopf(nbchmx), nomchp(nbchmx)
    character(len=7) :: nompar(nbchmx), nompaf(nbchmx)
    data nomchp/'.COEFH','.FLUNL','.SOUNL','.RAYO','.HECHP'/
    data nomopr/'COEF_R','      ','      ','RAYO_R','PARO_R'/
    data nomopf/'COEF_F','FLUXNL','SOURNL','RAYO_F','PARO_F'/
    data nompar/'PCOEFHR','       ','       ','PRAYONR','PHECHPR'/
    data nompaf/'PCOEFHF','PFLUXNL','PSOURNL','PRAYONF','PHECHPF'/
    data nligr/1,1,1,1,2/
! DEB ------------------------------------------------------------------
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
    call mecara(carele, chcara)
!
    call jeexin(merigi, iret)
    if (iret .eq. 0) then
        merigi = '&&METRIG           .RELR'
        call memare('V', merigi, modele(1:8), mate, carele,&
                    'MTAN_THER')
    else
        call jedetr(merigi)
    endif
!
    ligrel(1) = modele(1:8)//'.MODELE'
!
    ilires = 0
!
    if (modele .ne. ' ') then
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PTEMPSR'
        lchin(3) = inst
        lpain(4) = 'PTEMPEI'
        lchin(4) = chtni
        lpain(5) = 'PCOMPOR'
        lchin(5) = compor
        lpain(6) = 'PTMPCHI'
        lchin(6) = tmpchi
        lpain(7) = 'PTMPCHF'
        lchin(7) = tmpchf
        lpain(8) = 'PVARCPR'
        lchin(8) = varc_curr
!
        lpaout(1) = 'PMATTTR'
        lchout(1) = merigi(1:8)//'.ME001'
        option = 'MTAN_RIGI_MASS'
        ilires = ilires + 1
        call codent(ilires, 'D0', lchout(1) (12:14))
        call calcul('S', option, ligrel(1), 8, lchin,&
                    lpain, 1, lchout, lpaout, 'V',&
                    'OUI')
        call reajre(merigi, lchout(1), 'V')
    endif
!
    if (nchar .gt. 0) then
        call jeveuo(infcha, 'L', jinf)
        do 20 icha = 1, nchar
            if (zi(jinf+nchar+icha) .gt. 0) then
                nomcha = zk24(jchar+icha-1) (1:8)
                ligrel(2) = nomcha//'.CHTH.LIGRE'
                lpain(1) = 'PGEOMER'
                lchin(1) = chgeom
                lpain(3) = 'PTEMPSR'
                lchin(3) = inst
                lpain(4) = 'PTEMPEI'
                lchin(4) = chtni
                lpain(5) = 'PVARCPR'
                lchin(5) = varc_curr
!
                lpaout(1) = 'PMATTTR'
                lchout(1) = merigi(1:8)//'.ME001'
!
                do 10 k = 1, nbchmx
                    lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH'// nomchp(k)// '.DESC'
                    call jeexin(lchin(2), iret)
                    if (iret .gt. 0) then
                        if (zi(jinf+nchar+icha) .eq. 1) then
                            option = 'MTAN_THER_'//nomopr(k)
                            lpain(2) = nompar(k)
                            else if (zi(jinf+nchar+icha).eq.2 .or.&
                        zi(jinf+nchar+icha).eq.3) then
                            option = 'MTAN_THER_'//nomopf(k)
                            lpain(2) = nompaf(k)
                        endif
                        ilires = ilires + 1
                        call codent(ilires, 'D0', lchout(1) (12:14))
                        call calcul('S', option, ligrel(nligr(k)), 5, lchin,&
                                    lpain, 1, lchout, lpaout, 'V',&
                                    'OUI')
                        call reajre(merigi, lchout(1), 'V')
                    endif
10              continue
            endif
!
20      continue
!
    endif
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
