subroutine calmaa(moint, mate, dir, ligrmo, lchin,&
                  lpain, lpaout, num, maa)
!--------------------------------------------------------------------
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
!--------------------------------------------------------------------
    implicit none
!
! ROUTINE CALCULANT LES MATRICES AX ET AY SUR LE MODELE INTERFACE
! G. ROUSSEAU
! IN : MOINT : MODELE INTERFACE
! IN : MATE : MATERIAU CODE
! IN : DIR : DIRECTION X OU Y
! IN : LIGRMO : LIGREL DU MODELE
! IN: LCHIN,LPAIN,LPAOUT : PARAMETRES DU CALCUL ELEMENTAIRE
! IN : NUM : NUMEROTATION DES DDLS THERMIQUES D INTERFACE
!
! OUT : MAA : MATRICE AX OU AY
!---------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/assmam.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/crnslv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/memare.h"
#include "asterfort/numddl.h"
#include "asterfort/promor.h"
#include "asterfort/wkvect.h"
    character(len=*) :: moint, mate
    character(len=1) :: dir
    character(len=8) :: k8bid, lpain(1), lpaout(1)
    character(len=14) :: num
    character(len=16) :: option
    character(len=19) :: matel, maa
    character(len=24) :: lchout(1), lchin(1), ligrmo, maa2
!
!
!
! ----- CREATION DE LA MATR_ELEM AX OU AY DES N(I)*N(J)*NX OU NY----
!--------------SUR LE MODELE THERMIQUE------------------------------
!--------------------D'INTERFACE -----------------------------------
!
!-----------------------------------------------------------------------
    integer :: jlva
!-----------------------------------------------------------------------
    call jemarq()
    option = 'FLUX_FLUI_'//dir
    matel = '&&CA.MA'//dir
    maa2 = matel//'.RELR'
    call memare('V', matel, moint(1:8), mate, ' ',&
                'FLUX_FLUI_ '//dir)
    call wkvect(maa2, 'V V K24', 1, jlva)
    lchout(1) = matel(1:8)//'.ME000'
    call codent(1, 'D0', lchout(1) (12:14))
    call calcul('S', option, ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
    zk24(jlva) = lchout(1)
    call jeecra(maa2, 'LONUTI', 1, k8bid)
!
!
!-------------------- NUMEROTATION ----------------------------------
!
    num = '&&CALMAA.NUM'//dir
    call numddl(num, 'VV', 1, matel, 'RCMK')
    call promor(num, 'V')
    call crnslv(num, 'MULT_FRONT', 'METIS', 'V')
!
!---------------ASSEMBLAGE DES MATRICES AX OU AY DES N(I)N(J)NX OU NY
!
    maa = '&&CA.AA'//dir
!
    call assmam('V', maa, 1, matel, 1.d0,&
                num, 'ZERO', 1)
!
    call jedema()
end subroutine
