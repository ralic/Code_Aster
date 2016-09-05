subroutine nmprta(modele  , numedd, numfix  , mate       , carele,&
                  comref  , compor, lischa  , ds_algopara, solveu,&
                  fonact  , carcri, ds_print, ds_measure , ds_algorom,&
                  sddisc  , numins, valinc  , solalg     , matass,&
                  maprec  , ds_contact , sddyna,&
                  meelem  , measse, veelem  , veasse     , sdnume,&
                  ds_inout, ldccvg, faccvg  , rescvg     , codere)
!
use NonLin_Datastructure_type
use ROM_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmassp.h"
#include "asterfort/nmchar.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdep0.h"
#include "asterfort/nmfocc.h"
#include "asterfort/nmprma.h"
#include "asterfort/nmresd.h"
#include "asterfort/vtzero.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: numins, faccvg, rescvg, ldccvg
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    type(NL_DS_Measure), intent(inout) :: ds_measure
    type(ROM_DS_AlgoPara), intent(in) :: ds_algorom
    type(NL_DS_Print), intent(inout) :: ds_print
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: matass, maprec
    character(len=19) :: lischa, solveu, sddisc, sddyna, sdnume
    character(len=24) :: modele, mate, carele, comref, compor
    character(len=24) :: numedd, numfix
    character(len=24) :: carcri, codere
    character(len=19) :: solalg(*), valinc(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: meelem(*), measse(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION)
!
! PREDICTION PAR METHODE DE NEWTON-EULER
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! In  ds_algopara      : datastructure for algorithm parameters
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IO  ds_print         : datastructure for printing parameters
! In  ds_inout         : datastructure for input/output management
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_algorom       : datastructure for ROM parameters
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IO  ds_contact       : datastructure for contact management
! IN  SDDYNA : SD DYNAMIQUE
! IN  MATASS : NOM DE LA MATRICE DU PREMIER MEMBRE ASSEMBLEE
! IN  MAPREC : NOM DE LA MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  SDNUME : SD NUMEROTATION
! OUT FACCVG : CODE RETOUR FACTORISATION MATRICE GLOBALE
!                -1 : PAS DE FACTORISATION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : MATRICE SINGULIERE
!                 2 : ERREUR LORS DE LA FACTORISATION
!                 3 : ON NE SAIT PAS SI SINGULIERE
! OUT RESCVG : CODE RETOUR RESOLUTION SYSTEME LINEAIRE
!                -1 : PAS DE RESOLUTION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : NOMBRE MAXIMUM D'ITERATIONS ATTEINT
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT CODERE : CHAM_ELEM CODE RETOUR INTEGRATION LDC
!
!
!
!
    real(kind=8) :: instap
    character(len=19) :: cncine, cndonn, cnpilo
    aster_logical :: lstat, limpl, leltc
    integer :: ifm, niv
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PREDICTION TYPE EULER'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lstat = ndynlo(sddyna,'STATIQUE')
    limpl = ndynlo(sddyna,'IMPLICITE')
    leltc = isfonc(fonact,'ELT_CONTACT')
!
! --- INITIALISATIONS
!
    instap = diinst(sddisc,numins)
    ldccvg = -1
    faccvg = -1
    rescvg = -1
    cndonn = '&&CNCHAR.DONN'
    cnpilo = '&&CNCHAR.PILO'
    call vtzero(cndonn)
    call vtzero(cnpilo)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
!
! --- INCREMENT DE DEPLACEMENT NUL EN PREDICTION
!
    if (lstat .or. limpl) then
        call nmdep0('ON ', solalg)
    endif
!
! --- CALCUL DE LA MATRICE GLOBALE
!
    call nmprma(modele     , mate    , carele, compor, carcri,&
                ds_algopara, lischa  , numedd, numfix, solveu,&
                comref     , ds_print, ds_measure, sddisc,&
                sddyna     , numins  , fonact, ds_contact,&
                valinc     , solalg  , veelem, meelem, measse,&
                maprec     , matass  , codere, faccvg, ldccvg)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if ((faccvg.eq.1) .or. (faccvg.eq.2)) goto 999
    if (ldccvg .eq. 1) goto 999
!
! --- CALCUL DES CHARGEMENTS VARIABLES AU COURS DU PAS DE TEMPS
!
    call nmchar('VARI'  , 'PREDICTION', modele, numedd, mate,&
                carele  , compor      , lischa, numins, ds_measure,&
                sddisc  , fonact      , comref,&
                ds_inout, valinc      , solalg, veelem, measse,&
                veasse  , sddyna)
!
! --- CALCUL DU SECOND MEMBRE POUR CONTACT/XFEM
!
    if (leltc) then
        call nmfocc('PREDICTION', modele, mate, numedd, fonact,&
                    ds_contact, ds_measure, solalg,&
                    valinc, veelem, veasse)
    endif
!
! --- CALCUL DU SECOND MEMBRE
!
    call nmassp(modele, numedd, mate  , carele, comref,&
                compor, lischa, carcri, fonact, ds_measure,&
                ds_contact, sddyna, valinc, solalg, veelem,&
                veasse, ldccvg, codere, cnpilo,&
                cndonn, sdnume, matass)
!
! --- INCREMENT DE DEPLACEMENT NUL EN PREDICTION
!
    if (lstat .or. limpl) then
        call nmdep0('OFF', solalg)
    endif
!
! --- RESOLUTION K.DU = DF
!
    call nmresd(fonact, sddyna, ds_measure, solveu    , numedd,&
                instap, maprec, matass    , cndonn    , cnpilo,&
                cncine, solalg, rescvg    , ds_algorom)
!
999 continue
!
end subroutine
