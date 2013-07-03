subroutine debut()
! aslint: disable=
    implicit none
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!    DECODAGE DE LA COMMANDE DEBUT OU POURSUITE
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!        IBBASE  IBCATA
!     ------------------------------------------------------------------
!
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterc/getvtx.h"
#include "asterc/prhead.h"
#include "asterfort/foint0.h"
#include "asterfort/fozero.h"
#include "asterfort/gcncon.h"
#include "asterfort/ibbase.h"
#include "asterfort/ibcata.h"
#include "asterfort/ibdbgs.h"
#include "asterfort/ibfhdf.h"
#include "asterfort/ibtcpu.h"
#include "asterfort/mpiexe.h"
#include "asterfort/onerrf.h"
#include "asterfort/u2mess.h"
#include "asterfort/ulopen.h"
    character(len=8) :: k8b, repons
    character(len=16) :: nomcmd, k16b, cmpdef, cmput, cmpout
    character(len=80) :: fichdf
    integer :: ier, lout, n, ncode, ibid
    integer :: iarg
!
    fichdf=' '
!
! --- ERREUR / ERREUR_F :
!     MOT-CLE CODE PRESENT ?
    call getfac('CODE', ncode)
    if (ncode .ne. 0) then
        cmpdef = 'ABORT'
    else
        call prhead(3)
        cmpdef = 'EXCEPTION'
!        FERMETURE DU .CODE (OUVERT PAR IB0MAI)
        call ulopen(-15, ' ', ' ', ' ', ' ')
    endif
    call getvtx('ERREUR', 'ERREUR_F', 1, iarg, 1,&
                cmput, n)
    if (n .eq. 1) then
        cmpdef = cmput
    endif
    call onerrf(cmpdef, cmpout, lout)
!
! --- LECTURE DU MOT CLE FACTEUR DEBUG OU DE GESTION MEMOIRE DEMANDE
    call ibdbgs()
!
! --- ALARME GENERIQUE
    call getvtx(' ', 'PAR_LOT', 1, iarg, 1,&
                repons, n)
    if (repons .eq. 'NON') then
        call u2mess('A', 'SUPERVIS_1')
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
! --- CREATION DE L'OBJET DE STOCKAGE DES COMMUNICATEURS MPI
!
    call mpiexe('SET_COMM_REFE', ibid, ibid, ibid, ibid)
!
end subroutine
