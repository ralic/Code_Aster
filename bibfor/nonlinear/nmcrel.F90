subroutine nmcrel(sderro, nomevt, vall)
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
!
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmeceb.h"
    character(len=24) :: sderro
    character(len=9) :: nomevt
    aster_logical :: vall
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (SD ERREUR)
!
! ENREGISTREMENT D'UN EVENEMENT INTRINSEQUE
!
! ----------------------------------------------------------------------
!
! POUR LES EVENEMENTS A CODE RETOUR, IL FAUT D'ABORD TRANSFORMER LE
! CODE RETOUR EN EVENEMENT - UTILISER NMCRET
!
!
! IN  SDERRO : SD GESTION DES ERREURS
! IN  NOMEVT : NOM DE L'EVENEMENT (VOIR LA LISTE DANS NMCRER)
! IN  VALL   : .TRUE. SI ON ACTIVE
!
! ----------------------------------------------------------------------
!
    integer :: ieven, zeven
    character(len=24) :: erreno, erreni, erraac
    integer :: jeenom, jeeniv, jeeact
    character(len=24) :: errinf
    integer :: jeinfo
    character(len=16) :: neven, teven
    character(len=4) :: nombcl
    integer :: ievact
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ievact = 0
!
! --- ACCES SD
!
    errinf = sderro(1:19)//'.INFO'
    call jeveuo(errinf, 'L', jeinfo)
    zeven = zi(jeinfo-1+1)
!
    erreno = sderro(1:19)//'.ENOM'
    erreni = sderro(1:19)//'.ENIV'
    erraac = sderro(1:19)//'.EACT'
    call jeveuo(erreno, 'L', jeenom)
    call jeveuo(erreni, 'L', jeeniv)
    call jeveuo(erraac, 'E', jeeact)
!
! --- RECHERCHE DE L'EVENEMENT
!
    do 15 ieven = 1, zeven
!
! ----- NOM DE L'EVENEMENT
!
        neven = zk16(jeenom-1+ieven)
!
! ----- TYPE DE L'EVENEMENT
!
        teven = zk16(jeeniv-1+ieven)
!
! ----- ACTIVATION DE L'EVENEMENT
!
        if (neven .eq. nomevt) then
            ievact = ieven
            goto 66
        endif
 15 end do
!
 66 continue
!
    ASSERT(ievact.ne.0)
!
! --- (DES-)ACTIVATION DE L'EVENEMENT
!
    if (vall) then
        zi(jeeact-1+ievact) = 1
    else
        zi(jeeact-1+ievact) = 0
    endif
!
! --- EVENEMENT DE TYPE ERREUR ACTIVE: ON CHANGE LE STATUT DE LA BOUCLE
!
    if (vall) then
        teven = zk16(jeeniv-1+ievact)
        if (teven(1:5) .eq. 'ERRI_') then
            nombcl = teven(6:9)
            call nmeceb(sderro, nombcl, 'ERRE')
!
! ------- UNE ERREUR DE NIVEAU CALC EST FATALE POUR TOUT LE MONDE
!
            if (nombcl .eq. 'CALC') then
                call nmeceb(sderro, 'NEWT', 'STOP')
                call nmeceb(sderro, 'FIXE', 'STOP')
                call nmeceb(sderro, 'INST', 'STOP')
            endif
        endif
    endif
!
!
    call jedema()
end subroutine
