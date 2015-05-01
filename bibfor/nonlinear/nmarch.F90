subroutine nmarch(result, numins, modele, mate, carele,&
                  fonact, carcri, sdimpr, sddisc, sdpost,&
                  sdcrit, sdtime, sderro, sddyna, sdpilo,&
                  sdener, sdieto, sdcriq, lisch2)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/diinst.h"
#include "asterfort/dinuar.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmarc0.h"
#include "asterfort/nmarce.h"
#include "asterfort/nmarpc.h"
#include "asterfort/nmfinp.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmtime.h"
#include "asterfort/obgetb.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsagsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/uttcpg.h"
    character(len=8) :: result
    integer :: fonact(*)
    integer :: numins
    character(len=24) :: sdieto, sdtime, sdimpr
    character(len=19) :: sddisc, sdcrit, sddyna, sdpost, sdpilo, sdener
    character(len=24) :: carcri
    character(len=24) :: sderro, sdcriq
    character(len=19) :: lisch2
    character(len=24) :: modele, mate, carele
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ARCHIVAGE
!
! ----------------------------------------------------------------------
!
!
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  SDIMPR : SD AFFICHAGE
! IN  SDIETO : SD GESTION IN ET OUT
! IN  SDIMPR : SD AFFICHAGE
! IN  NUMINS : NUMERO DE L'INSTANT
! IN  MODELE : NOM DU MODELEE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  CARCRI : CARTE DES CRITERES DE CONVERGENCE LOCAUX
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  SDCRIT : VALEUR DES CRITERES DE CONVERGENCE
! IN  SDCRIQ : SD CRITERE QUALITE
! IN  SDERRO : SD ERREUR
! IN  SDDYNA : SD DEDIEE A LA DYNAMIQUE
! IN  SDPILO : SD PILOTAGE
! IN  SDTIME : SD TIMER
! IN  SDENER : SD ENERGIE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
!
! ----------------------------------------------------------------------
!
    integer :: jinst
    integer :: iret
    integer :: numarc
    real(kind=8) :: instam, instan
    character(len=8) :: k8bid
    aster_logical :: force, lprint
    character(len=19) :: k19bid
    character(len=4) :: etcalc
    integer :: numrep
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- DEBUT MESURE TEMPS
!
    call nmtime(sdtime, 'RUN', 'ARC')
!
! --- CONVERGENCE DU CALCUL ?
!
    call nmleeb(sderro, 'CALC', etcalc)
!
! --- DERNIER PAS -> ON FORCE L'ARCHIVAGE
!
    force = .false.
    call nmfinp(sddisc, numins, force)
!
! --- ON FORCE L'ARCHIVAGE
!
    if (etcalc .eq. 'CONV') force = .true.
    if (etcalc .eq. 'STOP') force = .true.
!
! --- IMPRESSION EVENTUELLE DES MESURES DE TEMPS
!
    call uttcpg('IMPR', 'INCR')
!
! --- NUMERO D'ARCHIVAGE
!
    call dinuar(sddisc, numins, force, numarc, numrep)
!
! --- INSTANT COURANT
!
    instan = diinst(sddisc,numins)
!
! --- ARCHIVAGE DES PARAMETRES CALCULES DANS LA TABLE PARA_CALC
!
    call nmarpc(result, sdener, numrep, instan)
!
! --- AFFICHAGE POUR CE PAS ?
!
    call obgetb(sdimpr, 'PRINT', lprint)
!
! ----------------------------------------------------------------------
!
    if (numarc .ge. 0) then
!
! ----- INSTANT DEJA ARCHIVE ?
!
        if (numarc .ge. 2) then
            call rsadpa(result, 'L', 1, 'INST', numarc-1,&
                        0, sjv=jinst, styp=k8bid)
            instam = zr(jinst)
            if (instan .le. instam) goto 999
        endif
!
! ----- AFFICHAGE
!
        if (lprint) then
            call utmess('I', 'ARCHIVAGE_5')
        endif
!
! ----- EXTENSION DE RESULT SI TROP PETIT (DOUBLEMENT)
!
        call rsexch(' ', result, 'DEPL', numarc, k19bid,&
                    iret)
        if (iret .eq. 110) call rsagsd(result, 0)
!
! ----- ARCHIVAGE DES PARAMETRES
!
        call nmarc0(result, modele, mate, carele, fonact,&
                    sdcrit, sddyna, sdpost, carcri, sdcriq,&
                    sdpilo, lisch2, numarc, instan)
!
! ----- ARCHIVAGE DES CHAMPS
!
        call nmarce(sdieto, result, sdimpr, sddisc, instan,&
                    numarc, force)
    endif
!
999 continue
!
! --- FIN MESURE TEMPS
!
    call nmtime(sdtime, 'END', 'ARC')
!
    call jedema()
end subroutine
