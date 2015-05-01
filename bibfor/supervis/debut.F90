subroutine debut()
    implicit none
!     ------------------------------------------------------------------
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
!    DECODAGE DE LA COMMANDE DEBUT OU POURSUITE
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!        IBBASE  IBCATA
!     ------------------------------------------------------------------
!
#include "asterc/getres.h"
#include "asterc/prhead.h"
#include "asterfort/foint0.h"
#include "asterfort/fozero.h"
#include "asterfort/gcncon.h"
#include "asterfort/getvtx.h"
#include "asterfort/ibbase.h"
#include "asterfort/ibcata.h"
#include "asterfort/ibcode.h"
#include "asterfort/ibdbgs.h"
#include "asterfort/ibfhdf.h"
#include "asterfort/ibtcpu.h"
#include "asterfort/onerrf.h"
#include "asterfort/ulopen.h"
#include "asterfort/utmess.h"
    character(len=8) :: k8b, repons
    character(len=16) :: nomcmd, k16b, cmpdef, cmput, cmpout
    character(len=80) :: fichdf
    integer :: ier, lout, n, ncode
    integer :: ipass=0
!
    if (ipass .ne. 0) then
        call utmess('F', 'SUPERVIS_2')
    endif
    ipass = 1
!
    fichdf=' '
!
!   ERREUR / ERREUR_F : mot-cle CODE present ?
    call ibcode(ncode)
    if (ncode .ne. 0) then
        cmpdef = 'ABORT'
    else
        call prhead(3)
        cmpdef = 'EXCEPTION'
!       fermeture du .code (ouvert par ib0mai)
        call ulopen(-15, ' ', ' ', ' ', ' ')
    endif
    call getvtx('ERREUR', 'ERREUR_F', iocc=1, scal=cmput, nbret=n)
    if (n .eq. 1) then
        cmpdef = cmput
    endif
    call onerrf(cmpdef, cmpout, lout)
!
! --- LECTURE DU MOT CLE FACTEUR DEBUG OU DE GESTION MEMOIRE DEMANDE
    call ibdbgs()
!
! --- ALARME GENERIQUE
    call getvtx(' ', 'PAR_LOT', scal=repons, nbret=n)
    if (repons .eq. 'NON') then
        call utmess('A', 'SUPERVIS_1')
    endif
!
! --- LECTURE DU MOT CLEF TEMPS_CPU
    call ibtcpu(ier)
!
! --- LECTURE DU MOT CLE HDF ---
    call ibfhdf(fichdf)
!
! --- LECTURE DU MOT CLE FACTEUR BASE ET ---
! --- ALLOCATION DES BASES DE DONNEES ---
    call ibbase(ier, fichdf)
    if (ier .eq. 0) then
        call getres(k8b, k16b, nomcmd)
!        -- INITIALISATION DE LA FONCTION NULLE : '&FOZERO'
!           ET DU COMMON FOSAV
        call fozero('&FOZERO')
        call foint0()
    endif
!
! --- POUR EVITER QUE LA CREATION DE '&&_NUM_CONCEPT_UNIQUE'
!        NE SOIT REPROCHE A UNE COMMANDE CREANT UNE SD
!        (DEBUT/DEBUG/SDVERI='OUI')
    call gcncon('.', k8b)
!
! --- LECTURE DU MOT CLE FACTEUR  CATALOGUE ---
    if (fichdf .eq. '  ') call ibcata(ier)
!
end subroutine
