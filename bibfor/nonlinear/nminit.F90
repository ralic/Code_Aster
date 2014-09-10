subroutine nminit(result, model , numedd, numfix  , mate,&
                  compor, carele, parmet, lischa  , maprec,&
                  solveu, carcri, numins, sdstat  , sddisc,&
                  sdnume, defico, sdcrit, comref  , fonact,&
                  parcon, parcri, method, lisch2  , mesh,&
                  sdpilo, sddyna, sdimpr, sd_suiv , sd_obsv,&
                  sdtime, sderro, sdpost, sd_inout, sdener,&
                  sdconv, sdcriq, deficu, resocu  , resoco,&
                  valinc, solalg, measse, veelem  , meelem,&
                  veasse, codere)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/accel0.h"
#include "asterfort/assert.h"
#include "asterfort/cetule.h"
#include "asterfort/cfmxsd.h"
#include "asterfort/cucrsd.h"
#include "asterfort/diinit.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscpy.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchap.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcrcg.h"
#include "asterfort/nmcrch.h"
#include "asterfort/nmcrcv.h"
#include "asterfort/nmcrob.h"
#include "asterfort/nmcrdd.h"
#include "asterfort/nmcrst.h"
#include "asterfort/nmcrti.h"
#include "asterfort/nmdoco.h"
#include "asterfort/nmdoct.h"
#include "asterfort/nmdoet.h"
#include "asterfort/nmdopi.h"
#include "asterfort/nmetcr.h"
#include "asterfort/nmexso.h"
#include "asterfort/nmfonc.h"
#include "asterfort/nmihht.h"
#include "asterfort/nminim.h"
#include "asterfort/nminma.h"
#include "asterfort/nminmc.h"
#include "asterfort/nminvc.h"
#include "asterfort/nmlssv.h"
#include "asterfort/nmnoli.h"
#include "asterfort/nmnume.h"
#include "asterfort/nmobsv.h"
#include "asterfort/nmpro2.h"
#include "asterfort/nmrini.h"
#include "asterfort/nmvcle.h"
#include "asterfort/nmvcre.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    integer :: fonact(*)
    real(kind=8) :: parcon(*), parcri(*), parmet(*)
    character(len=16) :: method(*)
    integer :: numins
    character(len=8) :: result, mesh
    character(len=19) :: solveu, sdnume, sddisc, sdcrit, sdpilo, sdener
    character(len=19) :: sdpost
    character(len=19) :: lischa, lisch2, sddyna
    character(len=19) :: maprec
    character(len=24) :: model, compor, numedd, numfix
    character(len=24) :: defico, resoco
    character(len=24) :: carcri
    character(len=24) :: mate, carele, codere
    character(len=19) :: veelem(*), meelem(*)
    character(len=19) :: veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: sdimpr, sdtime, sderro, sdstat, sdconv
    character(len=24) :: deficu, resocu, sdcriq
    character(len=24) :: comref
    character(len=24), intent(out) :: sd_inout
    character(len=19), intent(out) :: sd_obsv
    character(len=24), intent(out) :: sd_suiv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE
!
! Init
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! IN  RESULT : NOM DE LA SD RESULTAT
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! OUT LISCH2 : NOM DE LA SD INFO CHARGE POUR STOCKAGE DANS LA SD
!              RESULTAT
! OUT FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! OUT NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! OUT NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! Out sd_inout         : datastructure for input/output parameters
! Out sd_obsv          : datastructure for observation parameters
! Out sd_suiv          : datastructure for dof monitoring parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, ibid, numreo
    real(kind=8) :: r8bid3(3)
    real(kind=8) :: instin
    character(len=19) :: commoi
    character(len=2) :: codret
    aster_logical :: lacc0, lpilo, lmpas, lsstf, lerrt, lreli, lviss
    aster_logical :: lcont, lunil
    character(len=19) :: ligrcf, ligrxf
    integer, pointer :: slvi(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    lacc0 = .false.
    lunil = .false.
    lcont = .false.
!
! --- CREATION DE LA STRUCTURE DE DONNEE GESTION DU TEMPS
!
    call nmcrti(sdtime)
!
! --- CREATION DE LA STRUCTURE DE DONNEE STATISTIQUES
!
    call nmcrst(sdstat)
!
! --- SAISIE ET VERIFICATION DE LA COHERENCE DU CHARGEMENT CONTACT
!
    call nmdoct(lischa, defico, deficu, lcont, lunil,&
                ligrcf, ligrxf)
!
! --- CREATION DE LA NUMEROTATION ET PROFIL DE LA MATRICE
!
    call nmnume(model, result, lischa, lcont, defico,&
                compor, solveu, numedd, sdnume)
!
! --- CREATION DE VARIABLES "CHAPEAU" POUR STOCKER LES NOMS
!
    call nmchap(valinc, solalg, meelem, veelem, veasse,&
                measse)
!
! --- FONCTIONNALITES ACTIVEES
!
    call nmfonc(parcri, parmet, method, solveu, model,&
                defico, lischa, lcont, lunil, sdnume,&
                sddyna, sdcriq, mate, compor, result,&
                fonact)
    lpilo = isfonc(fonact,'PILOTAGE' )
    lmpas = ndynlo(sddyna,'MULTI_PAS' )
    lsstf = isfonc(fonact,'SOUS_STRUC')
    lerrt = isfonc(fonact,'ERRE_TEMPS_THM')
    lreli = isfonc(fonact,'RECH_LINE' )
    lviss = ndynlo(sddyna,'VECT_ISS' )
!
! --- SI ON A BESOIN DE FACTORISER SIMULTANNEMENT DEUX MATRICES AVEC LE SOLVEUR MUMPS ON LUI
!     SIGNALE AFIN QU'IL OPTIMISE AU MIEUX LA MEMOIRE POUR CHACUNES D'ELLES.
!     CE N'EST VRAIMENT UTILE QUE SI SOLVEUR/GESTION_MEMOIRE='AUTO'.
    if (isfonc(fonact,'MUMPS')) then
        if (isfonc(fonact,'CRIT_STAB') .or. isfonc(fonact,'MODE_VIBR')) then
            call jeveuo(solveu//'.SLVI', 'E', vi=slvi)
            if (slvi(6) .lt. 0) then
! --- PB INITIALISATION DE LA SD_SOLVEUR
                ASSERT(.false.)
            else
                slvi(6)=2
            endif
        endif
    endif
!
! --- CREATION DE LA STRUCTURE DE DONNEE RESULTAT DU CONTACT
!
    if (lcont) then
        call cfmxsd(mesh, model, numedd, fonact, sddyna,&
                    defico, resoco, ligrcf, ligrxf)
    endif
!
! --- CREATION DE LA STRUCTURE DE LIAISON_UNILATERALE
!
    if (lunil) then
        call cucrsd(mesh, numedd, deficu, resocu)
    endif
!
! --- CREATION DES VECTEURS D'INCONNUS
!
    call nmcrch(numedd, fonact, sddyna, defico, valinc,&
                solalg, veasse)
!
! --- CONSTRUCTION DU CHAM_NO ASSOCIE AU PILOTAGE
!
    if (lpilo) then
        call nmdopi(model, numedd, method, lreli, sdpilo)
    endif
!
! --- DUPLICATION NUME_DDL POUR CREER UN DUME_DDL FIXE
!
    call nmpro2(fonact, numedd, numfix)
!
! --- CONSTRUCTION DU CHAM_ELEM_S ASSOCIE AU COMPORTEMENT
!
    call nmdoco(model, carele, compor)
!
! - Create input/output datastructure
!
    call nmetcr(model, compor, fonact  , sddyna, sdpost,&
                defico, resoco, sd_inout, carele)
!
! --- LECTURE ETAT_INIT
!
    call nmdoet(model, compor, fonact, numedd, sdpilo,&
                sddyna, sdcriq, sd_inout, solalg, lacc0,&
                instin)
!
! --- CREATION SD DISCRETISATION ET ARCHIVAGE
!
    call diinit(mesh  , model , result, mate  , carele,&
                fonact, sddyna, parcri, instin, solveu,&
                defico, sddisc)
!
! - Create observation datastructure
!
    call nmcrob(mesh   , model, result, sddisc, sd_inout,&
                sd_obsv)
!
! - Create dof monitoring datastructure
!
    call nmcrdd(mesh , model, sd_inout, sd_suiv)
!
! --- CREATION DU CHAMP DES VARIABLES DE COMMANDE DE REFERENCE
!
    call nmvcre(model, mate, carele, comref)
!
! --- PRE-CALCUL DES MATR_ELEM CONSTANTES AU COURS DU CALCUL
!
    call nminmc(fonact, lischa, sddyna, model, compor,&
                solveu, numedd, numfix, defico, resoco,&
                carcri, solalg, valinc, mate, carele,&
                sddisc, sdstat, sdtime, comref, meelem,&
                measse, veelem, codere)
!
! --- INSTANT INITIAL
!
    numins = 0
    instin = diinst(sddisc,numins)
!
! --- EXTRACTION VARIABLES DE COMMANDES AU TEMPS T-
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmvcle(model, mate, carele, lischa, instin,&
                commoi, codret)
!
! --- CALCUL ET ASSEMBLAGE DES VECT_ELEM CONSTANTS AU COURS DU CALCUL
!
    call nminvc(model, mate, carele, compor, carcri,&
                sdtime, sddisc, sddyna, valinc, solalg,&
                lischa, comref, resoco, resocu, numedd,&
                fonact, parcon, veelem, veasse, measse)
!
! --- CREATION DE LA SD POUR ARCHIVAGE DES INFORMATIONS DE CONVERGENCE
!
    call nmcrcv(sdcrit)
!
! --- INITIALISATION CALCUL PAR SOUS-STRUCTURATION
!
    if (lsstf) then
        call nmlssv('INIT', lischa, ibid)
    endif
!
! --- CREATION DE LA SD EXCIT_SOL
!
    if (lviss) call nmexso(mesh, result, sddyna, numedd)
!
! --- CALCUL DE L'ACCELERATION INITIALE
!
    if (lacc0) then
        call nmchar('ACCI', ' ', model, numedd, mate,&
                    carele, compor, lischa, carcri, numins,&
                    sdtime, sddisc, parcon, fonact, resoco,&
                    resocu, comref, valinc, solalg, veelem,&
                    measse, veasse, sddyna)
        call accel0(model, numedd, numfix, fonact, lischa,&
                    defico, resoco, maprec, solveu, valinc,&
                    sddyna, sdstat, sdtime, meelem, measse,&
                    veelem, veasse, solalg)
    endif
!
! --- CREATION DE LA SD CONVERGENCE
!
    call nmcrcg(fonact, sdconv)
!
! --- INITIALISATION DE LA SD AFFICHAGE
!
    call nminim(sd_suiv, sdimpr)
!
! --- PRE-CALCUL DES MATR_ASSE CONSTANTES AU COURS DU CALCUL
!
    call nminma(fonact, lischa, sddyna, solveu, numedd,&
                numfix, meelem, measse)
!
! --- CREATION DE LA SD EVOL_NOLI
!
    call nmnoli(result, sddisc, sderro, carcri, sdimpr,&
                sdcrit, fonact, sddyna, sdpost, model,&
                mate, carele, lisch2, sdpilo, sdtime,&
                sdener, sd_inout, sdcriq)
!
! - Make initial observation
!
    call nmobsv(mesh, sddisc, sd_obsv, numins, sd_inout)
!
!NS   ICI ON UTILISE LISCPY A LA PLACE DE COPISD POUR
!NS   RESPECTER L'ESPRIT DE COPISD QUI NE SERT QU'A
!NS   RECOPIER DE SD SANS FILTRER
!
    call liscpy(lischa, lisch2, 'G')
!
! --- CREATION DE LA TABLE DES GRANDEURS
!
    if (lerrt) then
        call cetule(model, r8bid3, iret)
    endif
!
! --- CALCUL DU SECOND MEMBRE INITIAL POUR MULTI-PAS
!
    if (lmpas) then
        call nmihht(model, numedd, mate, compor, carele,&
                    lischa, carcri, comref, fonact, sdstat,&
                    sddyna, sdtime, sdnume, defico, resoco,&
                    resocu, valinc, sddisc, parcon, solalg,&
                    veasse)
    endif
!
! --- INITIALISATIONS TIMERS ET STATISTIQUES
!
    call nmrini(sdtime, sdstat, 'T')
    call jedema()
!
end subroutine
