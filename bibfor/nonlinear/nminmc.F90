subroutine nminmc(fonact, lischa, sddyna, modele, compor,&
                  numedd, numfix, defico, resoco, ds_algopara,&
                  carcri, solalg, valinc, mate, carele,&
                  sddisc, sdstat, sdtime, comref, meelem,&
                  measse, veelem, codere)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmcmat.h"
#include "asterfort/nmxmat.h"
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
    integer :: fonact(*)
    character(len=19) :: lischa, sddyna
    character(len=24) :: numedd, numfix, resoco, defico
    character(len=24) :: modele, compor
    character(len=24) :: carcri
    character(len=24) :: mate, carele
    character(len=19) :: meelem(*), measse(*)
    character(len=19) :: veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: sddisc
    character(len=24) :: sdtime, sdstat
    character(len=24) :: codere, comref
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! PRE-CALCUL DES MATRICES ELEMENTAIRES CONSTANTES AU COURS DU CALCUL
!
! ----------------------------------------------------------------------
!
!
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  SDDYNA : SD DYNAMIQUE
! IN  COMPOR : CARTE COMPORTEMENT
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  COMREF : VARIABLES DE COMMANDE DE REFERENCE
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  MATE   : NOM DU CHAMP DE MATERIAU
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR DEPLACEMENTS
! In  ds_algopara      : datastructure for algorithm parameters
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDTIME : SD TIMER
! OUT MEELEM : MATRICES ELEMENTAIRES
! OUT VEELEM : VECTEURS ELEMENTAIRES
! OUT MEASSE : MATRICES ASSEMBLEES
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    character(len=16) :: opmass, oprigi
    aster_logical :: lmacr, ldyna, lexpl
    aster_logical :: lamor, lktan, lelas, lvarc, lcfint, lamra
    integer :: ifm, niv
    integer :: numins, iterat, ldccvg
    integer :: nb_matr
    character(len=16) :: optrig, optamo
    character(len=6) :: list_matr_type(20)
    character(len=16) :: list_calc_opti(20), list_asse_opti(20)
    aster_logical :: list_l_asse(20), list_l_calc(20)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> PRECALCUL DES MATR_ELEM CONSTANTES'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lmacr = isfonc(fonact,'MACR_ELEM_STAT')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lexpl = ndynlo(sddyna,'EXPLICITE')
    lvarc = isfonc(fonact,'EXI_VARC' )
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lktan = ndynlo(sddyna,'RAYLEIGH_KTAN')
    lamra = ndynlo(sddyna,'AMOR_RAYLEIGH')
!
! - Initializations
!
    nb_matr              = 0
    list_matr_type(1:20) = ' '
    lelas                = .false.
    lcfint               = .false.
    ldccvg               = -1
    if (lamra .and. .not.lktan) then
        lelas = .true.
    endif
!
! --- INSTANT INITIAL
!
    numins = 1
!
! --- MATRICE DE RIGIDITE ASSOCIEE AUX LAGRANGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... MATR_ELEM DE RIGIDITE ASSOCIEE AUX LAGRANGE'
    endif
    call nmcmat('MEDIRI', ' ', ' ', .true._1,&
                .false._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                list_l_calc, list_l_asse)
!
! --- MATRICE DE MASSE
!
    if (ldyna) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MATR_ELEM DE MASSE'
        endif
        if (lexpl) then
            if (ndynlo(sddyna,'MASS_DIAG')) then
                opmass = 'MASS_MECA_EXPLI'
            else
                opmass = 'MASS_MECA'
            endif
        else
            opmass = 'MASS_MECA'
        endif
        call nmcmat('MEMASS', opmass, ' ', .true._1,&
                    .false._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse)
!
    endif
!
! --- MATRICES DES MACRO-ELEMENTS
! --- ON DOIT ASSEMBLER _AVANT_ ACCEL0
!
    if (lmacr) then
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... MATR_ELEM DES MACRO_ELEMENTS'
        endif
        oprigi = 'RIGI_MECA'
        call nmcmat('MESSTR', oprigi, ' ', .true._1,&
                    .true._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse)
    endif
!
! --- AJOUT DE LA MATRICE ELASTIQUE DANS LA LISTE
!
    if (lelas) then
        optrig = 'RIGI_MECA'
! ----- PARAMETRE INUTILE POUR OPTION RIGI_MECA
        iterat = 0
        call nmcmat('MERIGI', optrig, ' ', .true._1,&
                    .false._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse)
        if (lvarc) then
            call utmess('A', 'MECANONLINE3_2')
        endif
    endif
!
! --- AJOUT DE LA MATRICE AMORTISSEMENT DANS LA LISTE
!
    if (lamor .and. .not.lktan) then
        optamo = 'AMOR_MECA'
        call nmcmat('MEAMOR', optamo, ' ', .true._1,&
                    .false._1, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse)
!
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
    if (nb_matr .gt. 0) then
        call nmxmat(modele, mate, carele, compor, carcri,&
                    sddisc, sddyna, fonact, numins, iterat,&
                    valinc, solalg, lischa, comref, defico,&
                    resoco,  numedd, numfix, sdstat, ds_algopara,&
                    sdtime, nb_matr, list_matr_type, list_calc_opti, list_asse_opti,&
                    list_l_calc, list_l_asse, lcfint, meelem, measse,&
                    veelem, ldccvg, codere)
        if (ldccvg .gt. 0) then
            call utmess('F', 'MECANONLINE_1')
        endif
    endif
!
    call jedema()
end subroutine
