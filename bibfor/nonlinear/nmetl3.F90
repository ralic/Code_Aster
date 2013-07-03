subroutine nmetl3(modele, compor, evonol, result, numein,&
                  sdieto, leinit, icham)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetnc.h"
#include "asterfort/nmsigi.h"
#include "asterfort/rsexch.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vrcomp.h"
    character(len=24) :: modele, compor
    character(len=24) :: sdieto
    character(len=8) :: result
    logical :: evonol, leinit
    integer :: icham, numein
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! LECTURE D'UN CHAMP - VERIFICATIONS DIVERSES
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  EVONOL : .TRUE. SI CONCEPT EVOL DANS ETAT_INIT
! IN  LEINIT : .TRUE. SI LECTURE ETAT_INIT
! IN  RESULT : NOM SD EVOL_NOLI
! IN  SDIETO : SD GESTION IN ET OUT
! IN  NUMEIN : NUMERO ORDRE INSTANT INITIAL
! IN  ICHAM  : INDEX DU CHAMP DANS SDIETO
!
!
!
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: zioch
    integer :: iret, ibid
    character(len=24) :: chetin, nomchs, loccha, nomgd, statut
    character(len=24) :: valk(2)
    character(len=24) :: nomcha
    character(len=24) :: ligrmo, compom
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
!
    call dismoi('F', 'NOM_LIGREL', modele, 'MODELE', ibid,&
                ligrmo, iret)
!
! --- ACCES AUX SDS
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'E', jiolch)
    zioch = zi(jioinf+4-1)
!
! --- CHAMP A LIRE ?
!
    chetin = zk24(jiolch+zioch*(icham-1)+8-1)
    if (chetin .eq. 'NON') goto 999
!
! --- NOM DU CHAMP DANS SD RESULTAT
!
    nomchs = zk24(jiolch+zioch*(icham-1)+1-1)
!
! --- NOM DU CHAMP DANS L'OPERATEUR
!
    call nmetnc(sdieto, icham, nomcha)
!
! --- LOCALISATION DU CHAMP
!
    loccha = zk24(jiolch+zioch*(icham-1)+5-1)
!
! --- NOM DE LA GRANDEUR
!
    nomgd = zk24(jiolch+zioch*(icham-1)+7-1)
!
! --- STATUT DU CHAMP
!
    statut = zk24(jiolch+zioch*(icham-1)+4-1)
!
! --- LE CHAMP N'A JAMAIS ETE LU
!
    if (statut .eq. ' ') then
        call u2mesk('F', 'ETATINIT_30', 1, nomchs)
    else
        valk(1) = nomchs
        valk(2) = result(1:8)
        if (statut .eq. 'ZERO') then
            call u2mesk('I', 'ETATINIT_31', 1, nomchs)
        else if (statut.eq.'SDRESU') then
            call u2mesk('I', 'ETATINIT_32', 2, valk)
        else if (statut.eq.'CHAMP') then
            call u2mesk('I', 'ETATINIT_33', 1, nomchs)
        else
            call assert(.false.)
        endif
    endif
!
! --- VERIFICATION DE LA GRANDEUR ET DE LA LOCALISATION
!
    if (nomgd .ne. ' ') then
        call chpver('F', nomcha, loccha, nomgd, iret)
    endif
!
! --- TRAITEMENT DE LA PRE-CONTRAINTE
!
    if (nomchs .eq. 'SIEF_ELGA') then
        call nmsigi(ligrmo, compor, nomcha(1:19))
    endif
!
! --- VERIFIER LA COHERENCE DU CHAMP DE VARIABLES INTERNES
!
    if (nomchs .eq. 'VARI_ELGA') then
        if (leinit) then
            compom = ' '
            if (evonol) then
                call rsexch(' ', result, 'COMPORTEMENT', numein, compom,&
                            iret)
                if (iret .ne. 0) compom = ' '
            endif
            call vrcomp(compom, compor, nomcha, ligrmo)
        endif
    endif
!
999  continue
!
    call jedema()
end subroutine
