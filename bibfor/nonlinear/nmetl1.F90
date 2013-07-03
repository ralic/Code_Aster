subroutine nmetl1(result, numein, sdieto, icham)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmetnc.h"
#include "asterfort/rsexch.h"
#include "asterfort/u2mesk.h"
#include "asterfort/vtcopy.h"
    character(len=24) :: sdieto
    character(len=8) :: result
    integer :: icham, numein
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! LECTURE D'UN CHAMP - CAS DE LA SD RESULTAT DANS ETAT_INIT
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM SD RESULTAT
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
    character(len=24) :: champ
    integer :: ievol, iret
    character(len=24) :: chetin, nomchs, valk(2)
    character(len=24) :: nomcha, nomch0, loccha
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATION
!
    champ = '&&NMETL1.CHAMP'
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
! --- NOM DU CHAMP NUL
!
    nomch0 = zk24(jiolch+zioch*(icham-1)+2-1)
!
! --- LOCALISATION DU CHAMP
!
    loccha = zk24(jiolch+zioch*(icham-1)+5-1)
!
! --- NOM DU CHAMP DANS L'OPERATEUR
!
    call nmetnc(sdieto, icham, nomcha)
!
! --- RECUP DANS LA SD RESULTAT
!
    call rsexch(' ', result, nomchs, numein, champ,&
                ievol)
!
! --- TRAITEMENT DU CHAMP
!
    if (ievol .ne. 0) then
        if (nomch0 .ne. ' ') then
            call copisd('CHAMP', 'V', nomch0, nomcha)
            zk24(jiolch+zioch*(icham-1)+4-1) = 'ZERO'
        endif
    else
!
! ----- RECOPIE DU CHAMP EN LOCAL
!
        if (loccha .eq. 'NOEU') then
            call vtcopy(champ, nomcha, ' ', iret)
            if (iret .ne. 0) then
                valk(1) = champ
                valk(2) = nomcha
                call u2mesk('A', 'MECANONLINE_2', 2, valk)
            endif
            elseif ((loccha.eq.'ELGA').or. (loccha.eq.'ELNO').or. (&
        loccha.eq.'ELEM')) then
            call copisd('CHAMP_GD', 'V', champ, nomcha)
        else
            write(6,*) 'LOCCHA: ',loccha
            call assert(.false.)
        endif
!
! ----- STATUT DU CHAMP: LU DANS SD RESULTAT
!
        zk24(jiolch+zioch*(icham-1)+4-1) = 'SDRESU'
    endif
!
999  continue
!
    call jedema()
end subroutine
