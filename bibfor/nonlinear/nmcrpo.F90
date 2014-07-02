subroutine nmcrpo(nomsd, nume, inst, lselec)
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
#include "asterfort/nmcrit.h"
    character(len=19) :: nomsd
    real(kind=8) :: inst
    integer :: nume
    aster_logical :: lselec
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (UTILITAIRE - SELEC. INST.)
!
! L'INSTANT CHOISI DE VALEUR <INST> ET D'ORDRE <NUME> EST-IL
! SELECTIONNE DANS LA LISTE D'INSTANTS OU PAR UN PAS
! DE FREQUENCE DONNE ?
!
! ----------------------------------------------------------------------
!
!
! IN  NOMSD  : NOM DE LA SD
! IN  INST   : INSTANT COURANT
! IN  NUME   : ORDRE DE L'INSTANT COURANT
! OUT LSELEC : .TRUE. SI INSANT SELECTIONNE
!
!
!
!
    real(kind=8) :: tole, tolr
    integer :: nbinst, freq
    character(len=24) :: sdinfl
    integer :: jinfl
    character(len=4) :: typsel
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lselec = .false.
    typsel = 'NONE'
!
! --- ACCES A LA SD
!
    sdinfl = nomsd(1:19)//'.INFL'
    call jeveuo(sdinfl, 'L', jinfl)
!
! --- INFORMATIONS
!
    freq = nint(zr(jinfl-1+1))
    tole = zr(jinfl-1+2)
    nbinst = nint(zr(jinfl-1+3))
    if (tole .gt. 0.d0) then
        tolr = abs(inst)*tole
    else
        tolr = abs(tole)
    endif
!
! --- TYPE DE SELECTION (INSTANT OU FREQUENCE)
!
    if (freq .eq. 0) then
        typsel = 'INST'
    else
        typsel = 'FREQ'
    endif
!
! --- RECHERCHE
!
    call nmcrit(nomsd, nbinst, typsel, nume, inst,&
                freq, tolr, lselec)
!
!
    call jedema()
!
end subroutine
