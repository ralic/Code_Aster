subroutine nmspec(modele     , numedd, numfix, carele  , compor    ,&
                  numins     , mate  , comref, lischa  , ds_contact,&
                  ds_algopara, fonact, carcri, ds_print, sdstat    ,&
                  sdtime     , sddisc, valinc, solalg  , meelem    ,&
                  measse     , veelem, sddyna, sdpost  , sderro)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/affich.h"
#include "asterfort/assert.h"
#include "asterfort/diinst.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmcrpo.h"
#include "asterfort/nmflam.h"
#include "asterfort/nmimpx.h"
#include "asterfort/nmlesd.h"
#include "asterfort/utmess.h"
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
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: meelem(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24) :: sdstat, sdtime, sderro
    character(len=19) :: lischa, sddisc, sddyna, sdpost
    character(len=24) :: modele, numedd, numfix, carele, compor
    character(len=19) :: veelem(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: mate
    character(len=24) :: carcri, comref
    integer :: fonact(*)
    type(NL_DS_Print), intent(in) :: ds_print
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! ANALYSE DE FLAMBEMENT OU STABILITE ET/OU MODES VIBRATOIRES
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
! IN  LISCHA : LISTE DES CHARGES
! In  ds_contact       : datastructure for contact management
! In  ds_print         : datastructure for printing parameters
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISC_INST
! IN  PREMIE : SI PREMIER INSTANT DE CALCUL
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MATASS : MATRICE ASSEMBLEE GLOBALE
! IN  SDPOST : SD POUR POST-TRAITEMENTS (CRIT_STAB ET MODE_VIBR)
!
! ----------------------------------------------------------------------
!
    aster_logical :: lmvib, lflam
    aster_logical :: calcul
    integer :: ibid
    real(kind=8) :: r8bid, inst
    character(len=16) :: option
    character(len=19) :: nomlis
!
! ----------------------------------------------------------------------
!
    inst = diinst(sddisc,numins)
    calcul = .false.
    nomlis = ' '
    option = ' '
!
! --- FONCTIONNALITES ACTIVEES
!
    lmvib = isfonc(fonact,'MODE_VIBR')
    lflam = isfonc(fonact,'CRIT_STAB')
!
! --- DOIT-ON FAIRE LE CALCUL ?
!
    if (lflam) then
        nomlis = sdpost(1:14)//'.FLAM'
        call nmcrpo(nomlis, numins, inst, calcul)
    else if (lmvib) then
        nomlis = sdpost(1:14)//'.VIBR'
        call nmcrpo(nomlis, numins, inst, calcul)
    else
        goto 999
    endif
!
! --- CALCUL DE FLAMBEMENT EN STATIQUE ET DYNAMIQUE
!
    if (lflam) then
        if (calcul) then
            call nmlesd('POST_TRAITEMENT', sdpost, 'OPTION_CALCUL_FLAMB', ibid, r8bid, option)
!
! ------- IMPRESSION EN-TETE
!
            call nmimpx(ds_print)
            if (option .eq. 'FLAMBSTA') then
                call utmess('I', 'MECANONLINE6_2')
            else if (option.eq.'FLAMBDYN') then
                call utmess('I', 'MECANONLINE6_2')
            else
                ASSERT(.false.)
            endif
            call affich('MESSAGE', ' ')
!
! ------- CALCUL EFFECTIF
!
            call nmflam(option, modele, numedd, numfix     , carele,&
                        compor, numins, mate       , comref,&
                        lischa, ds_contact, ds_algopara, fonact,&
                        carcri, sdstat, sddisc, sdtime     , sddyna,&
                        sdpost, valinc, solalg, meelem     , measse,&
                        veelem, sderro)
        endif
    endif
!
! --- CALCUL DE MODES VIBRATOIRES EN DYNAMIQUE
!
    if (lmvib) then
        if (calcul) then
            call nmlesd('POST_TRAITEMENT', sdpost, 'OPTION_CALCUL_VIBR', ibid, r8bid, option)
!
! ------- IMPRESSION EN-TETE
!
            call nmimpx(ds_print)
            call utmess('I', 'MECANONLINE6_3')
            call affich('MESSAGE', ' ')
!
! ------- CALCUL EFFECTIF
!
            call nmflam(option, modele, numedd, numfix     , carele,&
                        compor, numins, mate       , comref,&
                        lischa, ds_contact, ds_algopara, fonact,&
                        carcri, sdstat, sddisc, sdtime     , sddyna,&
                        sdpost, valinc, solalg, meelem     , measse,&
                        veelem, sderro)
        endif
    endif
!
999 continue
!
end subroutine
