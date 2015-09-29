subroutine vecdid(model, list_load, disp_didi, vect_elem_)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dbgcal.h"
#include "asterfort/exisd.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=24), intent(in) :: model
    character(len=19), intent(in) :: disp_didi
    character(len=19), intent(in) :: list_load
    character(len=*), intent(in) :: vect_elem_
!
! --------------------------------------------------------------------------------------------------
!
! Mechanics - Load
!
! Elementary vector for Dirichlet BC (DIDI)
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  disp_didi        : displacement to compute DIDI loads
! In  list_load        : name of datastructure for list of loads
! In  vect_elem        : name of vect_elem for DIDI loads
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=3)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: iret
    integer :: nchar, nbres, icha
    real(kind=8) :: alpha
    character(len=8) :: nomcha
    character(len=19) :: vect_elem
    character(len=16) :: option
    character(len=1) :: base
    character(len=24) :: masque
    character(len=24) :: ligrch, chalph
    integer, pointer :: infc(:) => null()
    character(len=24), pointer :: lcha(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    vect_elem = vect_elem_
    base = 'V'
    call jeexin(list_load(1:19)// '.LCHA', iret)
    if (iret .eq. 0) goto 99
    option = 'MECA_BU_R'
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- CONSTRUCTION DU VECTEUR BDIDI.UREF
!
! REM : LE TERME BT.LAMBDA EST EGALEMENT CALCULE. IL EST NUL CAR A CE
!       STADE, LES LAMBDAS SONT NULS.
!
!
! --- LISTE DES CHARGES
!
    call jelira(list_load(1:19)//'.LCHA', 'LONMAX', nchar)
    call jeveuo(list_load(1:19)//'.LCHA', 'L', vk24=lcha)
    call jeveuo(list_load(1:19)//'.INFC', 'L', vi=infc)
!
! --- ALLOCATION DE LA CARTE DU CONDITIONNEMENT DES LAGRANGES
! REM : A CE STADE, ON FIXE LE COND A 1
!
    alpha=1.d0
    chalph = '&&VEBUME.CH_NEUT_R'
    call mecact('V', chalph, 'MODELE', model, 'NEUT_R  ',&
                ncmp=1, nomcmp='X1', sr=alpha)
!
! --- PREPARATION DES VECT_ELEM
!
    call jeexin(vect_elem(1:19)// '.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', vect_elem, model(1:8), ' ', ' ',&
                    'CHAR_MECA')
    endif
    call jedetr(vect_elem(1:19)//'.RELR')
    call reajre(vect_elem, ' ', 'V')
    masque = vect_elem(1:19)// '.VEXXX'
!
! --- BOUCLE SUR LES CHARGES DE TYPE DIRICHLET DIFFERENTIEL
!
    nbres = 0
    do icha = 1, nchar
!
! --- VERIF SI CHARGE DE TYPE DIRICHLET DIFFERENTIEL
!
        if (infc(icha+1) .le. 0 .or. infc(1+3*nchar+2+icha) .eq. 0) then
            cycle
        endif
        nomcha = lcha(icha)(1:8)
        call jeexin(nomcha(1:8)//'.CHME.LIGRE.LIEL', iret)
        if (iret .le. 0) cycle
        call exisd('CHAMP_GD', nomcha(1:8)//'.CHME.CMULT', iret)
        if (iret .le. 0) cycle
!
        call codent(nbres+1, 'D0', masque(12:14))
!
        ligrch = nomcha// '.CHME.LIGRE'
        lpain(1) = 'PDDLMUR'
        lchin(1) = nomcha// '.CHME.CMULT'
        lpain(2) = 'PDDLIMR'
        lchin(2) = disp_didi(1:19)
        lpain(3) = 'PALPHAR'
        lchin(3) = chalph(1:19)
        lpaout(1) = 'PVECTUR'
        lchout(1) = masque(1:19)
        call calcul('S', option, ligrch, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, base,&
                    'OUI')
        nbres = nbres + 1
        call reajre(vect_elem, lchout(1), 'V')
    end do
!
99  continue
!
    call jedema()
end subroutine
