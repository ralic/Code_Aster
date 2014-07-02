subroutine nmprma(modelz, mate, carele, compor, carcri,&
                  parmet, method, lischa, numedd, numfix,&
                  solveu, comref, sdimpr, sdstat, sdtime,&
                  sddisc, sddyna, numins, fonact, defico,&
                  resoco, valinc, solalg, veelem, meelem,&
                  measse, maprec, matass, codere, faccvg,&
                  ldccvg)
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
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchcc.h"
#include "asterfort/nmchoi.h"
#include "asterfort/nmchra.h"
#include "asterfort/nmchrm.h"
#include "asterfort/nmcmat.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmmatr.h"
#include "asterfort/nmrenu.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/nmxmat.h"
#include "asterfort/preres.h"
    real(kind=8) :: parmet(*)
    character(len=16) :: method(*)
    integer :: fonact(*)
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=24) :: sdimpr, sdtime, sdstat
    character(len=24) :: compor, carcri, numedd, numfix
    character(len=19) :: sddisc, sddyna, lischa, solveu
    character(len=24) :: comref, codere
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veelem(*), meelem(*), measse(*)
    integer :: numins
    character(len=24) :: defico, resoco
    character(len=19) :: maprec, matass
    integer :: faccvg, ldccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DE LA MATRICE GLOBALE EN PREDICTION
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  DEFICO : SD DEFINITION CONTACT
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  SOLVEU : SOLVEUR
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! OUT LFINT  : .TRUE. SI FORCES INTERNES CALCULEES
! OUT MATASS : MATRICE DE RESOLUTION ASSEMBLEE
! OUT MAPREC : MATRICE DE RESOLUTION ASSEMBLEE - PRECONDITIONNEMENT
! OUT FACCVG : CODE RETOUR FACTORISATION MATRICE GLOBALE
!                -1 : PAS DE FACTORISATION
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : MATRICE SINGULIERE
!                 2 : ERREUR LORS DE LA FACTORISATION
!                 3 : ON NE SAIT PAS SI SINGULIERE
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    aster_logical :: reasma, renume
    aster_logical :: lcrigi, lcfint, lcamor, larigi
    aster_logical :: ldyna, lamor, lsuiv
    character(len=16) :: metcor, metpre, k16bla
    character(len=16) :: optrig, optamo
    integer :: ifm, niv, ibid
    integer :: iterat
    integer :: nbmatr
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    aster_logical :: lassme(20), lcalme(20)
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL MATRICE'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lsuiv = isfonc(fonact,'FORCE_SUIVEUSE')
!
! --- INITIALISATIONS
!
    call nmcmat('INIT', ' ', ' ', ' ', .false._1,&
                .false._1, nbmatr, ltypma, loptme, loptma,&
                lcalme, lassme)
    faccvg = -1
    ldccvg = -1
    iterat = 0
    k16bla = ' '
    lcamor = .false.
!
! --- CHOIX DE REASSEMBLAGE DE LA MATRICE GLOBALE
!
    call nmchrm('PREDICTION', parmet, method, fonact, sddisc,&
                sddyna, numins, iterat, defico, metpre,&
                metcor, reasma)
!
! --- CHOIX DE REASSEMBLAGE DE L'AMORTISSEMENT
!
    if (lamor) then
        call nmchra(sddyna, optamo, lcamor)
    endif
!
! --- RE-CREATION DU NUME_DDL OU PAS
!
    call nmrenu(modelz, fonact, numedd, lischa, solveu,&
                resoco, renume)
!
! --- OPTION DE CALCUL POUR MERIMO
!
    call nmchoi('PREDICTION', sddyna, numins, fonact, metpre,&
                metcor, reasma, lcamor, optrig, lcrigi,&
                larigi, lcfint)
!
    if (lcfint) ASSERT(.false.)
!
! --- CALCUL DES MATR_ELEM CONTACT/XFEM_CONTACT
!
    call nmchcc(fonact, nbmatr, ltypma, loptme, loptma,&
                lassme, lcalme)
!
! --- CALCUL DES MATR-ELEM DE RIGIDITE
!
    if (lcrigi) then
        call nmcmat('AJOU', 'MERIGI', optrig, ' ', .true._1,&
                    larigi, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR-ELEM D'AMORTISSEMENT DE RAYLEIGH
!
    if (lcamor) then
        call nmcmat('AJOU', 'MEAMOR', optamo, ' ', .true._1,&
                    .true._1, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DES MATR-ELEM DES CHARGEMENTS
!
    if (lsuiv .and. (metpre.ne.'EXTRAPOLE')) then
        call nmcmat('AJOU', 'MESUIV', ' ', ' ', .true._1,&
                    .false._1, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- RE-CREATION MATRICE MASSE SI NECESSAIRE (NOUVEUA NUME_DDL
!
    if (renume) then
        if (ldyna) then
            call nmcmat('AJOU', 'MEMASS', ' ', ' ', .false._1,&
                        .true._1, nbmatr, ltypma, loptme, loptma,&
                        lcalme, lassme)
        endif
        if (.not.reasma) then
            ASSERT(.false.)
        endif
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
    if (nbmatr .gt. 0) then
        call nmxmat(modelz, mate, carele, compor, carcri,&
                    sddisc, sddyna, fonact, numins, iterat,&
                    valinc, solalg, lischa, comref, defico,&
                    resoco, solveu, numedd, numfix, sdstat,&
                    sdtime, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme, lcfint, meelem, measse,&
                    veelem, ldccvg, codere)
    endif
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (ldccvg .eq. 1) goto 9999
!
! --- CALCUL DE LA MATRICE ASSEMBLEE GLOBALE
!
    if (reasma) then
        call nmmatr('PREDICTION', fonact, lischa, solveu, numedd,&
                    sddyna, numins, defico, resoco, meelem,&
                    measse, matass)
        call nmimck(sdimpr, 'MATR_ASSE', metpre, .true._1)
    else
        call nmimck(sdimpr, 'MATR_ASSE', k16bla, .false._1)
    endif
!
! --- FACTORISATION DE LA MATRICE ASSEMBLEE GLOBALE
!
    if (reasma) then
        call nmtime(sdtime, 'INI', 'FACTOR')
        call nmtime(sdtime, 'RUN', 'FACTOR')
        call preres(solveu, 'V', faccvg, maprec, matass,&
                    ibid, -9999)
        call nmtime(sdtime, 'END', 'FACTOR')
        call nmrinc(sdstat, 'FACTOR')
    endif
!
9999 continue
!
end subroutine
