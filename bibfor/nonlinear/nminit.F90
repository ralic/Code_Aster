subroutine nminit(result  , model      , numedd , numfix     , mate       ,&
                  compor  , carele     , lischa , ds_algopara, maprec     ,&
                  solveu  , carcri     , numins , sdstat     , sddisc     ,&
                  sdnume  , sdcont_defi, sdcrit , varc_refe  , fonact     ,&
                  lisch2  , mesh       , sdpilo , sddyna     , ds_print   ,&
                  sd_suiv , sd_obsv    , sdtime , sderro     , sdpost     ,&
                  sd_inout, sdener     , ds_conv, sdcriq     , sdunil_defi,&
                  resocu  , resoco     , valinc , solalg     , measse     ,&
                  veelem  , meelem     , veasse , codere)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/accel0.h"
#include "asterfort/assert.h"
#include "asterfort/cetule.h"
#include "asterfort/cfmxsd.h"
#include "asterfort/cucrsd.h"
#include "asterfort/diinit.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/exfonc.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/liscpy.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchap.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmchex.h"
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
#include "asterfort/InitAlgoPara.h"
#include "asterfort/InitConv.h"
#include "asterfort/InitPrint.h"
#include "asterfort/nmrefe.h"
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
! aslint: disable=W1504
!
    integer :: fonact(*)
    integer :: numins
    character(len=8) :: result, mesh
    character(len=19) :: solveu, sdnume, sddisc, sdcrit, sdpilo, sdener
    character(len=19) :: sdpost
    character(len=19) :: lischa, lisch2, sddyna
    character(len=19) :: maprec
    character(len=24) :: model, compor, numedd, numfix
    character(len=24) :: resoco
    character(len=24) :: carcri
    character(len=24) :: mate, carele, codere
    character(len=19) :: veelem(*), meelem(*)
    character(len=19) :: veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: sdtime, sderro, sdstat
    character(len=24) :: resocu, sdcriq
    character(len=24) :: varc_refe
    character(len=24), intent(out) :: sdcont_defi
    character(len=24), intent(out) :: sdunil_defi
    character(len=24), intent(out) :: sd_inout
    character(len=19), intent(out) :: sd_obsv
    character(len=24), intent(out) :: sd_suiv
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_Conv), intent(inout) :: ds_conv
    type(NL_DS_AlgoPara), intent(inout) :: ds_algopara
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Initializations
!
! Initializations of datastructures
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
! Out sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! Out sdunil_defi      : name of unilateral condition datastructure (from DEFI_CONTACT)
! IO  ds_print         : datastructure for printing parameters
! IO  ds_conv          : datastructure for convergence management
! IO  ds_algopara      : datastructure for algorithm parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: iret, ibid
    real(kind=8) :: r8bid3(3)
    real(kind=8) :: instin
    character(len=19) :: varc_prev, disp_prev, strx_prev
    aster_logical :: lacc0, lpilo, lmpas, lsstf, lerrt, lviss, lrefe
    aster_logical :: lcont, lunil
    character(len=19) :: ligrcf, ligrxf
    character(len=24) :: sd_iden_rela
!
! --------------------------------------------------------------------------------------------------
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
    call nmdoct(mesh  , lischa, sdcont_defi, sdunil_defi , lcont, &
                lunil , ligrcf, ligrxf     , sd_iden_rela)
!
! --- CREATION DE LA NUMEROTATION ET PROFIL DE LA MATRICE
!
    call nmnume(model , result, lischa, lcont, sdcont_defi,&
                compor, solveu, numedd, sdnume,&
                sd_iden_rela)
!
! --- CREATION DE VARIABLES "CHAPEAU" POUR STOCKER LES NOMS
!
    call nmchap(valinc, solalg, meelem, veelem, veasse,&
                measse)
!
! - Prepare active functionnalities information
!
    call nmfonc(ds_conv, ds_algopara, solveu, model , sdcont_defi,&
                lischa , lcont      , lunil , sdnume, sddyna     ,&
                sdcriq , mate       , compor, result, carcri     ,&
                fonact)
!
! - Check compatibility of some functionnalities
!
    call exfonc(fonact, ds_algopara, solveu, sdcont_defi, sddyna,&
                mate)
    lpilo = isfonc(fonact,'PILOTAGE' )
    lmpas = ndynlo(sddyna,'MULTI_PAS' )
    lsstf = isfonc(fonact,'SOUS_STRUC')
    lerrt = isfonc(fonact,'ERRE_TEMPS_THM')
    lviss = ndynlo(sddyna,'VECT_ISS' )
    lrefe = isfonc(fonact,'RESI_REFE')
!
! --- CREATION DE LA STRUCTURE DE DONNEE RESULTAT DU CONTACT
!
    if (lcont) then
        call cfmxsd(mesh       , model , numedd, fonact, sddyna      ,&
                    sdcont_defi, resoco, ligrcf, ligrxf, sd_iden_rela)
    endif
!
! --- CREATION DE LA STRUCTURE DE LIAISON_UNILATERALE
!
    if (lunil) then
        call cucrsd(mesh, numedd, sdunil_defi, resocu)
    endif
!
! - Initializations for algorithm parameters
!
    call InitAlgoPara(fonact, ds_algopara)
!
! - Initializations for convergence management
!
    call InitConv(ds_conv, fonact, sdcont_defi)
!
! --- CREATION DES VECTEURS D'INCONNUS
!
    call nmcrch(numedd, fonact, sddyna, sdcont_defi, valinc,&
                solalg, veasse)
!
! --- CONSTRUCTION DU CHAM_NO ASSOCIE AU PILOTAGE
!
    if (lpilo) then
        call nmdopi(model, numedd, ds_algopara, sdpilo)
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
    call nmetcr(model , compor, fonact, sddyna, sdpost,&
                sdcont_defi, resoco, sd_inout, carele)
!
! --- LECTURE ETAT_INIT
!
    call nmdoet(model, compor, fonact, numedd, sdpilo,&
                sddyna, sdcriq, sd_inout, solalg, lacc0,&
                instin)
!
! - Create time discretization and storing datastructures
!
    call diinit(mesh  , model      , result , mate       , carele,&
                fonact, sddyna     , ds_conv, ds_algopara, instin,&
                solveu, sdcont_defi, sddisc)
!
! --- CREATION DU CHAMP DES VARIABLES DE COMMANDE DE REFERENCE
!
    call nmvcre(model, mate, carele, varc_refe)
!
! --- PRE-CALCUL DES MATR_ELEM CONSTANTES AU COURS DU CALCUL
!
    call nminmc(fonact, lischa, sddyna, model, compor,&
                numedd, numfix, sdcont_defi, resoco, ds_algopara,&
                carcri, solalg, valinc, mate, carele,&
                sddisc, sdstat, sdtime, varc_refe, meelem,&
                measse, veelem, codere)
!
! --- INSTANT INITIAL
!
    numins = 0
    instin = diinst(sddisc,numins)
!
! --- EXTRACTION VARIABLES DE COMMANDES AU TEMPS T-
!
    call nmchex(valinc, 'VALINC', 'COMMOI', varc_prev)
    call nmvcle(model , mate, carele, instin, varc_prev)
!
! --- CALCUL ET ASSEMBLAGE DES VECT_ELEM CONSTANTS AU COURS DU CALCUL
!
    call nminvc(model    , mate  , carele, compor, sdtime,&
                sddisc   , sddyna, valinc, solalg, lischa,&
                varc_refe, resoco, resocu, numedd, fonact,&
                veelem   , veasse, measse)
!
! - Compute reference vector for RESI_REFE_RELA
!
    if (lrefe) then
        call nmrefe(model  , compor, mate  , carele, numedd,&
                    ds_conv, valinc, veelem, veasse)
    endif              
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
        call nmchar('ACCI', ' '   , model , numedd, mate     ,&
                    carele, compor, lischa, numins, sdtime   ,&
                    sddisc, fonact, resoco, resocu, varc_refe,&
                    valinc, solalg, veelem, measse, veasse   ,&
                    sddyna)
        call accel0(model      , numedd, numfix, fonact     , lischa,&
                    sdcont_defi, resoco, maprec, solveu     , valinc,&
                    sddyna     , sdstat, sdtime, ds_algopara, meelem,&
                    measse     , veelem, veasse, solalg)
    endif
!
! - Extract variables
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', disp_prev)
    call nmchex(valinc, 'VALINC', 'STRMOI', strx_prev)
!
! - Create observation datastructure
!
    call nmcrob(mesh     , model    , result, sddisc   , sd_inout ,&
                carele   , mate     , compor, disp_prev, strx_prev,&
                varc_prev, varc_refe, instin, sd_obsv  )
!
! - Create dof monitoring datastructure
!
    call nmcrdd(mesh  , model    , sd_inout , carele   , mate     ,&
                compor, disp_prev, strx_prev, varc_prev, varc_refe,&
                instin, sd_suiv)
!
! - Initializations for printing
!
    call InitPrint(sd_suiv, ds_print)
!
! --- PRE-CALCUL DES MATR_ASSE CONSTANTES AU COURS DU CALCUL
!
    call nminma(fonact, lischa, sddyna, numedd, ds_algopara,&
                numfix, meelem, measse)
!
! --- CREATION DE LA SD EVOL_NOLI
!
    call nmnoli(result, sddisc  , sderro, carcri, ds_print,&
                sdcrit, fonact  , sddyna, sdpost, model   ,&
                mate  , carele  , lisch2, sdpilo, sdtime  ,&
                sdener, sd_inout, sdcriq)
!
! - Make initial observation
!
    call nmobsv(mesh    , model, sddisc, sd_obsv  , numins,&
                carele  , mate , compor, varc_refe, valinc,&
                sd_inout)
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
        call nmihht(model , numedd, mate     , compor     , carcri,&
                    carele, lischa, varc_refe, fonact     , sdstat,&
                    sddyna, sdtime, sdnume   , sdcont_defi, resoco,&
                    resocu, valinc, sddisc   , solalg     , veasse,&
                    result)
    endif
!
! --- INITIALISATIONS TIMERS ET STATISTIQUES
!
    call nmrini(sdtime, sdstat, 'T')
!
end subroutine
