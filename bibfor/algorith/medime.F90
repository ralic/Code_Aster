subroutine medime(base, cumul, modele, lischa, mediri)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dbgcal.h"
#include "asterfort/exisd.h"
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
    character(len=4) :: cumul
    character(len=24) :: modele
    character(len=19) :: mediri
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES MATRICES ELEMENTAIRES DES ELEMENTS DE LAGRANGE
!
! ----------------------------------------------------------------------
!
!
! IN  BASE    : BASE 'V' OU BASE OU SONT CREES LES OBJETS EN SORTIE
! IN  CUMUL   : /'ZERO' : ON INITIALISE LE MATR_ELEM
!               /'CUMU' : ON AJOUTE UN NOUVEAU MATR_ELEM AUX EXISTANTS
! IN  MODELE  : NOM DU MODELE
! IN  LISCHA  : SD L_CHARGES
! OUT MEDIRI  : MATRICES ELEMENTAIRES
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=1)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    character(len=8) :: nomcha
    character(len=16) :: option
    character(len=19) :: ligrch
    integer :: iret, jchar, jinf, icha, ilires
    integer :: nchar, nluti
    integer :: ifmdbg, nivdbg
    logical :: debug
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    option = 'MECA_DDLM_R'
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
! --- ACCES AUX CHARGEMENTS
!
    call jeexin(lischa(1:19)//'.LCHA', iret)
    if (iret .eq. 0) then
        nchar = 0
        goto 20
    else
        call jelira(lischa(1:19)//'.LCHA', 'LONMAX', nchar)
        call jeveuo(lischa(1:19)//'.LCHA', 'L', jchar)
        call jeveuo(lischa(1:19)//'.INFC', 'L', jinf)
    endif
!
! --- PREPARATION DES MATR_ELEM
!
    if (cumul .eq. 'ZERO') then
        call jedetr(mediri//'.RELR')
        call memare(base, mediri, modele(1:8), ' ', ' ',&
                    'RIGI_MECA')
        call reajre(mediri, ' ', base)
    endif
!
! --- CHAMPS DE SORTIE
!
    lpaout(1) = 'PMATUUR'
    lchout(1) = mediri(1:15)//'.'
!
    if (cumul .eq. 'ZERO') then
        nluti = 1
    else if (cumul.eq.'CUMU') then
        call jelira(mediri//'.RELR', 'LONUTI', nluti)
        nluti = nluti+1
        call codent(nluti+1, 'D0', lchout(1) (12:14))
    else
        ASSERT(.false.)
    endif
!
    ilires = nluti-1
    do 10 icha = 1, nchar
        if (zi(jinf+icha) .ne. 0) then
            nomcha = zk24(jchar+icha-1) (1:8)
            ligrch = nomcha(1:8)//'.CHME.LIGRE'
            call jeexin(ligrch(1:19)//'.LIEL', iret)
            if (iret .le. 0) goto 10
            call exisd('CHAMP_GD', nomcha(1:8)//'.CHME.CMULT', iret)
            if (iret .le. 0) goto 10
!
! ---     CHAMPS IN
!
            lpain(1) = 'PDDLMUR'
            lchin(1) = nomcha//'.CHME.CMULT'
!
! ---     CHAMPS OUT
!
            lpaout(1) = 'PMATUUR'
            call codent(ilires+1, 'D0', lchout(1) (17:19))
!
! ---     APPEL A CALCUL
!
            if (debug) then
                call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                            nbout, lpaout, lchout)
            endif
!
            call calcul('S', option, ligrch, nbin, lchin,&
                        lpain, nbout, lchout, lpaout, base,&
                        'OUI')
!
! ---     STOCKAGE DES RESU_ELEM
!
            call reajre(mediri, lchout(1), base)
            ilires = ilires + 1
        endif
10  end do
20  continue
!
    call jedema()
end subroutine
