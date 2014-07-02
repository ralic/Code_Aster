subroutine nmcrit(nomsd, nbinst, typsel, nume, inst,&
                  freq, tole, lselec)
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
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utacli.h"
    character(len=19) :: nomsd
    real(kind=8) :: inst
    character(len=4) :: typsel
    aster_logical :: lselec
    integer :: nume, freq, nbinst
    real(kind=8) :: tole
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
!
! L'INSTANT CHOISI DE VALEUR <INST> ET D'ORDRE <NUME> EST-IL
! SELECTIONNE DANS LA LISTE D'INSTANTS <LIST> OU PAR UN PAS
! DE FREQUENCE <FREQ> DONNE ?
!
! ----------------------------------------------------------------------
!
!
! IN  NOMSD  : NOM DE LA SD
! IN  TYPSEL : TYPE DE SELECTION
!              FREQ - PAR FREQUENCE SUR LES NUMEROS D'ORDRE DE LA LISTE
!              INST - PAR VALEUR D'INSTANT CHERCHE DANS LA LISTE
! IN  DEBUT  : DEBUT DE LA RECHERCHE DE L'INSTANT (SAUVEGARDE AILLEURS
!               POUR GAGNER DU TEMPS CAR LA LISTE EST CROISSANTE)
! IN  INST   : INSTANT CHERCHE SI TYPSEL='INST'
! IN  NUME   : ORDRE DE L'INSTANT SI TYPSEL='FREQ'
! IN  TOLE   : TOLERANCE POUR RECHERCHE DANS LISTE D'INSTANTS
!               >0 PRECISION RELATIVE (A MULTIPLIER PAR DELTA_MIN)
!               <0 PRECISION ABSOLUE
! IN  DTMIN  : DELTA MINIMUM SUR LA LISTE
! IN  FREQ   : FREQUENCE DE SELECTION
! OUT LSELEC : .TRUE. SI INSANT SELECTIONNE
!
!
!
!
    real(kind=8) :: reste
    integer :: nbindi
    character(len=24) :: sdlist
    integer :: jlist
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lselec = .false.
!
! --- SELECTION
!
    if (typsel .eq. 'FREQ') then
        reste = mod(nume,freq )
        if (reste .eq. 0.d0) then
            lselec = .true.
        else
            lselec = .false.
        endif
    else if (typsel.eq.'INST') then
        sdlist = nomsd(1:19)//'.LIST'
        call jeveuo(sdlist, 'L', jlist)
        call utacli(inst, zr(jlist), nbinst, tole, nbindi)
        if (nbindi .ge. 0) then
            lselec = .true.
        endif
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
end subroutine
