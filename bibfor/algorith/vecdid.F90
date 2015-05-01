subroutine vecdid(modele, lischa, depdid, vecelz)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dbgcal.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/infdbg.h"
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
    character(len=*) :: vecelz
    character(len=24) :: modele
    character(len=19) :: depdid
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECTEURS ELEMENTAIRES DIRICHLET DIFFERENTIEL
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  LISCHA : SD L_CHARGES
! IN  DEPDID : DEPLACEMENTS
! OUT VECELE : VECTEURS ELEMENTAIRES DIRICHLET DIFFERENTIEL
!
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=3)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: numref, n1, nevo, iret
    integer :: nchar, nbres, icha
    real(kind=8) :: alpha
    character(len=8) :: nomcha
    character(len=19) :: vecele
    character(len=16) :: option
    character(len=1) :: base
    character(len=24) :: evol, masque
    character(len=24) :: ligrch, chalph
    aster_logical :: debug
    integer :: ifmdbg, nivdbg
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
    vecele = vecelz
    base = 'V'
    call jeexin(lischa(1:19)// '.LCHA', iret)
    if (iret .eq. 0) goto 9999
    option = 'MECA_BU_R'
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
! --- CONSTRUCTION DE LA CONFIGURATION DE REFERENCE
!
    call getvis('ETAT_INIT', 'NUME_DIDI', iocc=1, scal=numref, nbret=n1)
    call getvid('ETAT_INIT', 'EVOL_NOLI', iocc=1, scal=evol, nbret=nevo)
    if ((n1.gt.0) .and. (nevo.gt.0)) then
        call rsexch(' ', evol, 'DEPL', numref, depdid,&
                    iret)
        if (iret .ne. 0) then
            call utmess('F', 'MECANONLINE5_20', sk=evol)
        endif
    endif
!
! --- CONSTRUCTION DU VECTEUR BDIDI.UREF
!
! REM : LE TERME BT.LAMBDA EST EGALEMENT CALCULE. IL EST NUL CAR A CE
!       STADE, LES LAMBDAS SONT NULS.
!
!
! --- LISTE DES CHARGES
!
    call jelira(lischa(1:19)//'.LCHA', 'LONMAX', nchar)
    call jeveuo(lischa(1:19)//'.LCHA', 'L', vk24=lcha)
    call jeveuo(lischa(1:19)//'.INFC', 'L', vi=infc)
!
! --- ALLOCATION DE LA CARTE DU CONDITIONNEMENT DES LAGRANGES
! REM : A CE STADE, ON FIXE LE COND A 1
!
    alpha=1.d0
    chalph = '&&VEBUME.CH_NEUT_R'
    call mecact('V', chalph, 'MODELE', modele, 'NEUT_R  ',&
                ncmp=1, nomcmp='X1', sr=alpha)
!
! --- PREPARATION DES VECT_ELEM
!
    call jeexin(vecele(1:19)// '.RELR', iret)
    if (iret .eq. 0) then
        call memare('V', vecele, modele(1:8), ' ', ' ',&
                    'CHAR_MECA')
    endif
    call jedetr(vecele(1:19)//'.RELR')
    call reajre(vecele, ' ', 'V')
    masque = vecele(1:19)// '.VEXXX'
!
! --- BOUCLE SUR LES CHARGES DE TYPE DIRICHLET DIFFERENTIEL
!
    nbres = 0
    do 10 icha = 1, nchar
!
! --- VERIF SI CHARGE DE TYPE DIRICHLET DIFFERENTIEL
!
        if (infc(icha+1) .le. 0 .or. infc(1+3*nchar+2+icha) .eq. 0) then
            goto 10
        endif
        nomcha = lcha(icha)(1:8)
        call jeexin(nomcha(1:8)//'.CHME.LIGRE.LIEL', iret)
        if (iret .le. 0) goto 10
        call exisd('CHAMP_GD', nomcha(1:8)//'.CHME.CMULT', iret)
        if (iret .le. 0) goto 10
!
        call codent(nbres+1, 'D0', masque(12:14))
!
        ligrch = nomcha// '.CHME.LIGRE'
        lpain(1) = 'PDDLMUR'
        lchin(1) = nomcha// '.CHME.CMULT'
        lpain(2) = 'PDDLIMR'
        lchin(2) = depdid
        lpain(3) = 'PALPHAR'
        lchin(3) = chalph
        lpaout(1) = 'PVECTUR'
        lchout(1) = masque
        call calcul('S', option, ligrch, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, base,&
                    'OUI')
!
        if (debug) then
            call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                        nbout, lpaout, lchout)
        endif
!
        nbres = nbres + 1
        call reajre(vecele, lchout(1), 'V')
 10 end do
!
9999 continue
!
    call jedema()
end subroutine
