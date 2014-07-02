subroutine nmetci(sdieto, zioch, icham, nomchs, nomgd,&
                  motcei, motcob, loccha, letin, larch)
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
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=24) :: sdieto
    integer :: zioch, icham
    character(len=24) :: nomchs, nomgd
    character(len=24) :: motcei, loccha, motcob
    aster_logical :: letin, larch
!
! ----------------------------------------------------------------------
!
! ROUTINE GESTION IN ET OUT
!
! ADDITION D'UN CHAMP DANS LA SDIETO
!
! ----------------------------------------------------------------------
!
!
! IN  SDIETO : SD GESTION IN ET OUT
! IN  ZIOCH  : NOMBRE DE PARAMETRES PAR CHAMP
! IN  ICHAM  : INDEX DU CHAMP DANS SDIETO
! IN  NOMCHS : NOM DU CHAMP DANS SD RESULTAT
! IN  NOMGD  : NOM DE LA GRANDEUR
! IN  MOTCEI : MOT-CLEF DANS ETAT_INIT, ' ' SI PAS DE MOT-CLEF
! IN  MOTCOB : MOT-CLEF DANS OBSERVATION, ' ' SI PAS DE MOT-CLEF
! IN  LOCCHA : LOCALISATION DU CHAMP
! IN  LETIN  : .TRUE. SI CHAMP LU DANS ETAT_INIT
! IN  LARCH  : .TRUE. SI CHAMP ECRIT DANS ARCHIVAGE
!
!
!
!
    character(len=24) :: iolcha
    integer :: jiolch
    character(len=24) :: chetin, charch
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD
!
    iolcha = sdieto(1:19)//'.LCHA'
    call jeveuo(iolcha, 'E', jiolch)
!
! --- INITIALISATIONS
!
    if (letin) then
        chetin = 'OUI'
    else
        chetin = 'NON'
    endif
    if (larch) then
        charch = 'OUI'
    else
        charch = 'NON'
    endif
!
! --- ECRITURE
!
    zk24(jiolch+zioch*(icham-1)+1 -1) = nomchs
    zk24(jiolch+zioch*(icham-1)+3 -1) = motcei
    zk24(jiolch+zioch*(icham-1)+5 -1) = loccha
    zk24(jiolch+zioch*(icham-1)+7 -1) = nomgd
    zk24(jiolch+zioch*(icham-1)+8 -1) = chetin
    zk24(jiolch+zioch*(icham-1)+9 -1) = charch
    zk24(jiolch+zioch*(icham-1)+10-1) = motcob
!
    call jedema()
end subroutine
