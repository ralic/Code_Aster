subroutine nmdini(motfac, iocc, provli, instin, linsei,&
                  tole, nbinst, linsti, numini)
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
#include "asterfort/assert.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utacli.h"
#include "asterfort/utmess.h"
    character(len=16) :: motfac
    character(len=19) :: provli
    real(kind=8) :: tole, instin
    aster_logical :: linsei, linsti
    integer :: numini, nbinst, iocc
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES - DISCRETISATION)
!
! DETERMINATION DU NUMERO D'ORDRE INITIAL
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLEF FACTEUR POUR INFO SUR INCREMENT
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! IN  PROVLI : NOM DE LA LISTE D'INSTANT PROVISOIRE
! IN  TOLE   : TOLERANCE POUR RECHERCHE DANS LISTE D'INSTANTS
! IN  LINSEI : .TRUE. SI L'INSTANT DE L'ETAT INITIAL EXISTE
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
! IN  NBINST : NOMBRE D'INSTANTS DANS LA LISTE
! OUT LINSTI : .TRUE. SI L'INSTANT INITIAL N'EXISTAIT PAS
!              DANS LA LISTE D'INSTANTS -> ON A PRIS LE PLUS PROCHE
! OUT NUMINI : NUMERO D'ORDRE INITIAL
!
!
!
!
    integer :: n1, n2, i
    real(kind=8) :: inst, ins, dt, dtmin
    integer :: jinst
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    numini = 0
    linsti = .false.
!
! --- ACCES LISTE D'INSTANTS PROVISOIRE
!
    call jeveuo(provli, 'L', jinst)
!
! --- LECTURE MOTS-CLEFS
!
    call getvis(motfac, 'NUME_INST_INIT', iocc=iocc, scal=numini, nbret=n1)
    call getvr8(motfac, 'INST_INIT', iocc=iocc, scal=inst, nbret=n2)
!
! --- PAS D'OCCURRENCE DES MOTS-CLES -> NUMERO INITIAL
!
    if ((n1+n2 .eq. 0) .and. (.not.linsei)) then
        numini = 0
!
! --- MOTS-CLES INST_INIT OU INSTANT DEFINI PAR ETAT_INIT
!
    else if (n1 .eq. 0) then
        if (n2 .eq. 0) then
!
! ------- INSTANT DEFINI PAR ETAT_INIT
!
            inst = instin
            call utacli(inst, zr(jinst), nbinst, tole, numini)
!
! ------- SI INST NON PRESENT DANS LA LISTE D'INSTANT
! ------- ON CHERCHE L INSTANT LE PLUS PROCHE AVANT L'INSTANT CHERCHE
!
            if (numini .lt. 0) then
                linsti = .true.
                dtmin = inst - zr(jinst)
                ins = zr(jinst)
                do 40 i = 1, nbinst-1
                    dt = inst - zr(jinst+i)
                    if (dt .le. 0.d0) then
                        goto 45
                    endif
                    if (dt .lt. dtmin) then
                        dtmin = dt
                        ins = zr(jinst+i)
                    endif
 40             continue
 45             continue
                inst = ins
            endif
        endif
!
        call utacli(inst, zr(jinst), nbinst, tole, numini)
        if (numini .lt. 0) then
            call utmess('F', 'DISCRETISATION_89', sr=inst)
        endif
    endif
!
! --- VERIFICATIONS
!
    ASSERT(numini.ge.0)
    ASSERT(numini.le.nbinst)
!
    call jedema()
!
end subroutine
