subroutine nmihht(modele, numedd, mate, compor, carele,&
                  lischa, carcri, comref, fonact, sdstat,&
                  sddyna, sdtime, sdnume, defico, resoco,&
                  resocu, valinc, sddisc, parcon, solalg,&
                  veasse)
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
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmchht.h"
#include "asterfort/nmvcex.h"
    integer :: fonact(*)
    character(len=19) :: sddyna, sdnume
    character(len=19) :: lischa
    character(len=24) :: modele, mate, carele, numedd
    character(len=24) :: compor, carcri, comref
    character(len=24) :: sdtime, sdstat
    character(len=19) :: sddisc
    real(kind=8) :: parcon(*)
    character(len=24) :: defico, resoco, resocu
    character(len=19) :: solalg(*), veasse(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! INITIALISATIONS SPECIFIQUES POUR SCHEMAS MULTIPAS EN POURSUITE
!
! ----------------------------------------------------------------------
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
!
!
!
!
    integer :: ifm, niv
    integer :: nocc, iret
    logical(kind=1) :: evonol
    character(len=19) :: commoi, insmoi
    character(len=24) :: k24bid
!
! ----------------------------------------------------------------------
!
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> INITIALISATION MULTI-PAS'
    endif
!
    call getvid('ETAT_INIT', 'EVOL_NOLI', iocc=1, scal=k24bid, nbret=nocc)
    evonol = nocc .gt. 0
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    if (evonol) then
        call nmvcex('INST', commoi, insmoi)
        call exisd('CHAMP_GD', insmoi, iret)
        if (iret .ne. 0) then
            call nmchht(modele, numedd, mate, compor, carele,&
                        lischa, carcri, comref, fonact, sdstat,&
                        sddyna, sdtime, defico, resoco, resocu,&
                        valinc, sddisc, parcon, solalg, veasse,&
                        sdnume)
        endif
    endif
!
    call jedema()
end subroutine
