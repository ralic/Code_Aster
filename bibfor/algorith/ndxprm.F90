subroutine ndxprm(modelz, mate, carele, compor, carcri,&
                  method, lischa, numedd, numfix, solveu,&
                  comref, sddisc, sddyna, sdstat, sdtime,&
                  numins, fonact, valinc, solalg, veelem,&
                  meelem, measse, maprec, matass, codere,&
                  faccvg, ldccvg)
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
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ndxmat.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchra.h"
#include "asterfort/nmcmat.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
#include "asterfort/nmxmat.h"
#include "asterfort/preres.h"
#include "asterfort/u2mess.h"
    character(len=16) :: method(*)
    integer :: fonact(*)
    character(len=*) :: modelz
    character(len=24) :: mate, carele
    character(len=24) :: sdtime, sdstat
    character(len=24) :: compor, carcri, numedd, numfix
    character(len=19) :: sddisc, sddyna, lischa, solveu
    character(len=24) :: comref, codere
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: veelem(*), meelem(*), measse(*)
    integer :: numins
    character(len=19) :: maprec, matass
    integer :: faccvg, ldccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL DE LA MATRICE GLOBALE EN PREDICTION - CAS EXPLICITE
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
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
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
! OUT CODERE : CHAM_ELEM CODE RETOUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    logical :: reasma
    logical :: lcrigi, lcfint, lcamor, larigi, lprem
    logical :: lamor, lsuiv, lshima, lprmo
    character(len=16) :: metpre
    character(len=16) :: optrig, optamo
    integer :: ifm, niv, ibid
    integer :: iterat
    integer :: nbmatr
    character(len=24) :: k24bla
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    logical :: lassme(20), lcalme(20)
!
! ----------------------------------------------------------------------
!
    call jemarq()
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
    lamor = ndynlo(sddyna,'MAT_AMORT')
    lprmo = ndynlo(sddyna,'PROJ_MODAL')
    lsuiv = isfonc(fonact,'FORCE_SUIVEUSE')
    lshima = ndynlo(sddyna,'COEF_MASS_SHIFT')
!
! --- PREMIER PAS DE TEMPS ?
!
    lprem = numins.le.1
!
! --- PARAMETRES
!
    metpre = method(5)
    if (metpre .ne. 'TANGENTE') then
        call u2mess('F', 'MECANONLINE5_1')
    endif
!
! --- INITIALISATIONS
!
    call nmcmat('INIT', ' ', ' ', ' ', .false.,&
                .false., nbmatr, ltypma, loptme, loptma,&
                lcalme, lassme)
    faccvg = -1
    ldccvg = -1
    iterat = 0
    lcamor = .false.
    k24bla = ' '
!
! --- CHOIX DE REASSEMBLAGE DE LA MATRICE GLOBALE
!
    reasma = .false.
    if (lprem) then
        if (lprmo) then
            reasma = .false.
        else
            reasma = .true.
        endif
    endif
!
! --- CHOIX DE REASSEMBLAGE DE L'AMORTISSEMENT
!
    if (lamor) then
        call nmchra(sddyna, optamo, lcamor)
    endif
!
! --- OPTION DE CALCUL POUR MERIMO
!
    optrig = 'RIGI_MECA_TANG'
    lcfint = .false.
    lcrigi = .false.
    larigi = .false.
!
! --- SI ON DOIT RECALCULER L'AMORTISSEMENT DE RAYLEIGH
!
    if (lcamor) then
        lcrigi = .true.
    endif
!
! --- DECALAGE COEF_MASS_SHIFT AU PREMIER PAS DE TEMPS -> ON A BESOIN
! --- DE LA MATRICE DE RIGIDITE
!
    if (lshima .and. lprem) then
        lcrigi = .true.
        larigi = .true.
    endif
!
! --- CALCUL DES MATR-ELEM DE RIGIDITE
!
    if (lcrigi) then
        call nmcmat('AJOU', 'MERIGI', optrig, ' ', .true.,&
                    larigi, nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR-ELEM D'AMORTISSEMENT DE RAYLEIGH
!
    if (lcamor) then
        call nmcmat('AJOU', 'MEAMOR', optamo, ' ', .true.,&
                    .true., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL DES MATR-ELEM DES CHARGEMENTS
!
    if (lsuiv) then
        call nmcmat('AJOU', 'MESUIV', ' ', ' ', .true.,&
                    .false., nbmatr, ltypma, loptme, loptma,&
                    lcalme, lassme)
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
    if (nbmatr .gt. 0) then
        call nmxmat(modelz, mate, carele, compor, carcri,&
                    sddisc, sddyna, fonact, numins, iterat,&
                    valinc, solalg, lischa, comref, k24bla,&
                    k24bla, solveu, numedd, numfix, sdstat,&
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
        call ndxmat(fonact, lischa, solveu, numedd, sddyna,&
                    numins, meelem, measse, matass)
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
9999  continue
!
    call jedema()
!
end subroutine
