subroutine nmpost(modele , mesh    , numedd, numfix     , carele  ,&
                  compor , numins  , mate  , comref     , ds_inout,&
                  ds_contact, ds_algopara, fonact  ,&
                  carcri , ds_print, sdstat, sddisc     , sdtime  ,&
                  sd_obsv, sderro  , sddyna, sdpost     , valinc  ,&
                  solalg , meelem  , measse, veelem     , veasse  ,&
                  sdener , sdcriq  , eta   , lischa)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfmxpo.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmener.h"
#include "asterfort/nmetca.h"
#include "asterfort/nmleeb.h"
#include "asterfort/nmobsv.h"
#include "asterfort/nmspec.h"
#include "asterfort/nmtime.h"
#include "asterfort/nmrest_ecro.h"
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
! aslint: disable=W1504
!
    integer :: numins
    character(len=8), intent(in) :: mesh
    real(kind=8) :: eta
    type(NL_DS_InOut), intent(in) :: ds_inout
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: meelem(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: sdener
    character(len=19) :: lischa
    character(len=19) :: sddisc, sddyna, sdpost
    character(len=19), intent(in) :: sd_obsv
    type(NL_DS_Print), intent(in) :: ds_print
    character(len=24) :: modele, numedd, numfix, compor
    character(len=19) :: veelem(*), measse(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: sdstat, sdtime, sderro, sdcriq
    character(len=24) :: mate, carele
    character(len=24) :: carcri, comref
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCULS DE POST-TRAITEMENT
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! In  ds_inout         : datastructure for input/output management
! In  ds_print         : datastructure for printing parameters
! In  ds_contact       : datastructure for contact management
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
! IN  SDCRIQ : SD CRITERE QUALITE
!
! ----------------------------------------------------------------------
!
    aster_logical :: lmvib, lflam, lerrt, lcont, lener, l_post_incr
    character(len=4) :: etfixe
!
! ----------------------------------------------------------------------
!
    lcont       = isfonc(fonact,'CONTACT')
    lerrt       = isfonc(fonact,'ERRE_TEMPS_THM')
    lmvib       = isfonc(fonact,'MODE_VIBR')
    lflam       = isfonc(fonact,'CRIT_STAB')
    lener       = isfonc(fonact,'ENERGIE')
    l_post_incr = isfonc(fonact,'POST_INCR')
!
! --- LE PAS FIXE A NECESSAIREMENT CONVERGE
!
    call nmleeb(sderro, 'FIXE', etfixe)
    if (etfixe .ne. 'CONV') then
        goto 99
    endif
!
! --- CALCUL EVENTUEL DE L'INDICATEUR D'ERREUR TEMPORELLE THM
!
    if (lerrt) then
        call nmetca(modele, mesh, mate, sddisc, sdcriq,&
                    numins, valinc)
    endif
!
! --- POST_TRAITEMENT DU CONTACT
!
    if (lcont) then
        call nmtime(sdtime, 'INI', 'POST_TRAITEMENT')
        call nmtime(sdtime, 'RUN', 'POST_TRAITEMENT')
        call cfmxpo(mesh  , modele, ds_contact%sdcont_defi, ds_contact%sdcont_solv, numins,&
                    sddisc, sdstat, solalg, valinc, veasse)
        call nmtime(sdtime, 'END', 'POST_TRAITEMENT')
    endif
!
! --- CALCUL DE POST-TRAITEMENT: STABILITE ET MODES VIBRATOIRES
!
    if (lmvib .or. lflam) then
        call nmtime(sdtime, 'INI', 'POST_TRAITEMENT')
        call nmtime(sdtime, 'RUN', 'POST_TRAITEMENT')
        call nmspec(modele  , numedd, numfix     , carele, compor,&
                    numins, mate       , comref, lischa,&
                    ds_contact, ds_algopara, fonact, carcri,&
                    ds_print, sdstat, sdtime     , sddisc, valinc,&
                    solalg  , meelem, measse     , veelem, sddyna,&
                    sdpost  , sderro)
        call nmtime(sdtime, 'END', 'POST_TRAITEMENT')
    endif
!
! --- CALCUL DES ENERGIES
!
    if (lener) then
        call nmener(valinc, veasse, measse, sddyna, eta        ,&
                    sdener, fonact, numedd, numfix, ds_algopara,&
                    meelem, numins, modele, mate  , carele     ,&
                    compor, sdtime, sddisc, solalg, lischa     ,&
                    comref, veelem, ds_inout)
    endif

!
! - Post-treatment for behavior laws.
!
    if (l_post_incr) then
        call nmrest_ecro(modele, mate, compor, valinc, carcri)
    endif
!
! - Make observation
!
    call nmobsv(mesh  , modele, sddisc, sd_obsv, numins,&
                carele, mate  , compor, comref , valinc)
!
99  continue
!
end subroutine
