subroutine nmeteo(result, sdimpr, sddisc, sdieto, force,&
                  numarc, instan, icham)
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
    implicit none
#include "jeveux.h"
#include "asterfort/diincl.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmarcc.h"
#include "asterfort/nmetnc.h"
#include "asterfort/obgetb.h"
#include "asterfort/utmess.h"
    character(len=24) :: sdieto, sdimpr
    character(len=19) :: sddisc
    character(len=8) :: result
    integer :: icham
    integer :: numarc
    real(kind=8) :: instan
    logical(kind=1) :: force
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! ECRITURE D'UN CHAMP DANS LA SD RESULAT
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM SD EVOL_NOLI
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDIMPR : SD AFFICHAGE
! IN  SDIETO : SD GESTION IN ET OUT
! IN  FORCE  : VRAI SI ON SOUHAITE FORCER L'ARCHIVAGE DE TOUS LES CHAMPS
! IN  INSTAN : INSTANT D'ARCHIVAGE
! IN  NUMARC : NUMERO D'ARCHIVAGE
! IN  ICHAM  : INDEX DU CHAMP DANS SDIETO
!
! ----------------------------------------------------------------------
!
    character(len=24) :: ioinfo, iolcha
    integer :: jioinf, jiolch
    integer :: zioch
    character(len=24) :: nomcha, nomchs, charch
    logical(kind=1) :: lprint
    integer :: iret
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD IN ET OUT
!
    ioinfo = sdieto(1:19)//'.INFO'
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(ioinfo, 'L', jioinf)
    call jeveuo(iolcha, 'E', jiolch)
    zioch = zi(jioinf+4-1)
!
! --- CHAMP A ARCHIVER ?
!
    charch = zk24(jiolch+zioch*(icham-1)+9-1)
    if (charch .eq. 'NON') goto 999
!
! --- AFFICHAGE POUR CE PAS ?
!
    lprint = .true.
    if (sdimpr .ne. ' ') call obgetb(sdimpr, 'PRINT', lprint)
!
! --- NOM DU CHAMP DANS SD RESULTAT
!
    nomchs = zk24(jiolch+zioch*(icham-1)+1-1)
!
! --- NOM DU CHAMP DANS L'OPERATEUR
!
    call nmetnc(sdieto, icham, nomcha)
    call exisd('CHAMP', nomcha, iret)
!
! --- ARCHIVAGE DU CHAMP
!
    if (diincl(sddisc,nomchs,force ) .and. (iret.eq.1)) then
        if (lprint) then
            call utmess('I', 'ARCHIVAGE_6', sk=nomchs, si=numarc, sr=instan)
        endif
        call nmarcc(result, numarc, nomchs, nomcha)
    endif
!
999  continue
!
    call jedema()
end subroutine
