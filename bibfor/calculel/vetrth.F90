subroutine vetrth(modele, charge, infcha, carele, mate,&
                  inst, chtn, chti, chlapm, chlapp,&
                  veres)
!
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
!
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/gcnco2.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
    character(len=24) :: modele, charge, infcha, carele, inst, chtn, chti
    character(len=24) :: chlapm, chlapp, veres, mate
! ----------------------------------------------------------------------
! CALCUL DES VECTEURS ELEMENTAIRES - SECOND MEMBRE DU PROBLEME TRANSPORT
!                                    EN THERMIQUE NON LINEAIRE -
!
! IN  MODELE  : NOM DU MODELE
! IN  CHARGE  : LISTE DES CHARGES
! IN  INFCHA  : INFORMATIONS SUR LES CHARGES
! IN  CARELE  : CHAMP DE CARA_ELEM
! IN  MATE    : CHAMP DE MATERIAU
! IN  INST    : CARTE CONTENANT LA VALEUR DU TEMPS ET AUTRES PARAMETRES
! IN  CHTN    : CHAMP DE TEMPERATURE A L'INSTANT PRECEDENT
! IN  CHTI    : I EME ITERE DU CHAMP DE TEMPERATURE
! IN  CHLAPM  : I EME ITERE DU CHAMP DES FONCTIONS ENTHALPIE
! OUT CHLAPP  : I+1 EME ITERE DU CHAMP DES FONCTIONS ENTHALPIE
! OUT VERES   : VECTEURS ELEMENTAIRES (SECOND MEMBRE)
!
!
!
    character(len=1) :: c1
    character(len=8) :: nomcha, lpain(7), lpaout(4), newnom
    character(len=16) :: option
    character(len=24) :: ligrmo, lchin(7), lchout(4)
    character(len=24) :: chvite, convch, chgeom, chcara(18)
    integer :: iret, jvites
!
! DEB ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, icha, ichar, iconv, jchar, jinf, nchar
!
!-----------------------------------------------------------------------
    call jemarq()
    newnom = '.0000000'
    ligrmo = modele(1:8)//'.MODELE'
    call jeexin(charge, iret)
    if (iret .ne. 0) then
        call jelira(charge, 'LONMAX', nchar)
        call jeveuo(charge, 'L', jchar)
        call jeveuo(infcha, 'L', jinf)
    else
        nchar = 0
    endif
!
    call megeom(modele, chgeom)
    call mecara(carele, chcara)
!
    lpaout(1) = 'PVECTTR'
    lpaout(2) = 'PLAGRP '
    lpaout(3) = 'PRESIDU'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PTEMPSR'
    lchin(3) = inst
    lpain(4) = 'PTEMPER'
    lchin(4) = chtn
    lpain(5) = 'PTEMPEI'
    lchin(5) = chti
    lpain(6) = 'PLAGRM '
    lchin(6) = chlapm
!
! --- TERME VOLUMIQUE PROVENANT DU COMPORTEMENT
!
    iconv = 0
    chvite = ' '
    option = 'CHAR_THER_TNL'
    lchout(2) = chlapp
    lpain(7) = 'PVITESR'
!
    do ichar = 1, nchar
        nomcha = zk24(jchar+ichar-1) (1:8)
        convch = nomcha//'.CHTH'//'.CONVE'//'.VALE'
        call jeexin(convch, iret)
        if (iret .gt. 0) then
            iconv = iconv + 1
            if (iconv .gt. 1) then
                call utmess('F', 'CALCULEL3_72')
            endif
            call jeveuo(convch, 'L', jvites)
            chvite = zk8(jvites)
        endif
    end do
    if (iconv .eq. 0) then
        call utmess('F', 'CALCULEL5_38')
    endif
    lchin(7) = chvite
!
    call gcnco2(newnom)
    lchout(1) = '&&VETRTH.'//newnom(2:8)
    call corich('E', lchout(1), -1, ibid)
    call gcnco2(newnom)
    lchout(3) = '&&VETRTH.'//newnom(2:8)
    call corich('E', lchout(3), -1, ibid)
    call calcul('S', option, ligrmo, 7, lchin,&
                lpain, 3, lchout, lpaout, 'V',&
                'OUI')
    call reajre(veres, lchout(3), 'V')
!
! --- TERME SURFACIQUE PROVENANT DES CONDITIONS AUX LIMITES
!
    lpaout(1) = 'PRESIDU'
    lpain(2) = 'PFLUXNL'
    if (nchar .gt. 0) then
        do icha = 1, nchar
            lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.FLUNL.DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                option = 'CHAR_THER_FLUTNL'
                call gcnco2(newnom)
                lchout(1) = '&&VETRTH.'//newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrmo, 5, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
            lchin(2) = zk24(jchar+icha-1) (1:8)//'.CHTH.T_EXT.DESC'
            call jeexin(lchin(2), iret)
            if (iret .ne. 0) then
                lpaout(1) = 'PRESIDU'
                c1 = 'R'
                if (zi(jinf+nchar+icha) .gt. 1) then
                    c1 = 'F'
                endif
                option = 'RESI_THER_COEH_'//c1
                lpain(4) = 'PCOEFH'//c1
                lchin(4) = zk24(jchar+icha-1) (1:8)//'.CHTH.COEFH'
                call gcnco2(newnom)
                lchout(1) = '&&VETRTH.'//newnom(2:8)
                call corich('E', lchout(1), -1, ibid)
                call calcul('S', option, ligrmo, 5, lchin,&
                            lpain, 1, lchout, lpaout, 'V',&
                            'OUI')
                call reajre(veres, lchout(1), 'V')
            endif
        end do
    endif
!
    call jedema()
end subroutine
