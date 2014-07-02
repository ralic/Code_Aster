subroutine vebtla(base, modelz, mate, carele, deplaz,&
                  lischa, vecelz)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dbgcal.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=1) :: base
    character(len=*) :: modelz, deplaz, vecelz
    character(len=19) :: lischa
    character(len=24) :: mate, carele
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECTEURS ELEMENTAIRES BT.LAMBDA
!
! ----------------------------------------------------------------------
!
!
! IN  BASE   : BASE 'V' OU 'G' OU SONT CREES LES OBJETS EN SORTIE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES ELEMENTAIRES
! IN  MODELE : NOM DU MODELE
! IN  DEPLA  : CHAMP DE DEPLACEMENTS
! IN  LISCHA : SD L_CHARGES
! OUT VECELE : VECTEURS ELEMENTAIRES
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=2)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=6) :: masque
    character(len=8) :: nomcha
    character(len=16) :: option
    character(len=24) :: ligrch
    integer :: iret, nchar, ndir, icha
    character(len=8) :: modele
    character(len=19) :: depla, vecele
    integer :: ifmdbg, nivdbg
    aster_logical :: debug
    integer, pointer :: infc(:) => null()
    character(len=24), pointer :: lcha(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    modele = modelz
    depla = deplaz
    vecele = vecelz
    option = 'MECA_BTLA_R'
    masque = '.REXXX'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- ACCES AUX CHARGES
!
    call jeexin(lischa(1:19)//'.LCHA', iret)
    if (iret .eq. 0) goto 9999
    call jelira(lischa(1:19)//'.LCHA', 'LONMAX', nchar)
    call jeveuo(lischa(1:19)//'.LCHA', 'L', vk24=lcha)
    call jeveuo(lischa(1:19)//'.INFC', 'L', vi=infc)
!
! --- ALLOCATION DU VECT_ELEM RESULTAT :
!
    call jeexin(vecele//'.RELR', iret)
    if (iret .eq. 0) then
        call memare(base, vecele, modele, mate, carele,&
                    'CHAR_MECA')
    else
        call jedetr(vecele//'.RELR')
    endif
    call reajre(vecele, ' ', base)
!
! --- CALCUL DE L'OPTION BT.LAMBDA
!
    ndir = 0
    do 10 icha = 1, nchar
        if (infc(icha+1) .gt. 0) then
            nomcha = lcha(icha)(1:8)
            ligrch = nomcha // '.CHME.LIGRE'
            ndir = ndir + 1
            call codent(ndir, 'D0', masque(4:6))
            lpain(1) = 'PDDLMUR'
            lchin(1) = nomcha(1:8)//'.CHME.CMULT'
            lpain(2) = 'PLAGRAR'
            lchin(2) = depla
            lpaout(1) = 'PVECTUR'
            lchout(1) = vecele(1:8)//masque
!
            call calcul('S', option, ligrch, nbin, lchin,&
                        lpain, nbout, lchout, lpaout, base,&
                        'OUI')
!
            if (debug) then
                call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                            nbout, lpaout, lchout)
            endif
!
            call reajre(vecele, lchout(1), 'V')
        endif
 10 end do
!
9999 continue
    call jedema()
end subroutine
