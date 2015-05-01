subroutine nmprca(modele, numedd, numfix, mate, carele,&
                  comref, compor, lischa, method, solveu,&
                  fonact, parmet, carcri, sdimpr, sdstat,&
                  sddisc, sdtime, numins, valinc, solalg,&
                  matass, maprec, defico, resoco, sddyna,&
                  meelem, measse, veelem, veasse, depest,&
                  ldccvg, faccvg, rescvg, codere)
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
!
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmacin.h"
#include "asterfort/nmassd.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmprma.h"
#include "asterfort/nmreso.h"
#include "asterfort/vtzero.h"
    integer :: fonact(*)
    integer :: numins, ldccvg, faccvg, rescvg
    real(kind=8) :: parmet(*)
    character(len=16) :: method(*)
    character(len=19) :: maprec, matass
    character(len=24) :: sdimpr, sdtime, sdstat
    character(len=19) :: lischa, solveu, sddisc, sddyna
    character(len=24) :: modele, mate, carele, comref, compor
    character(len=24) :: numedd, numfix
    character(len=24) :: carcri
    character(len=24) :: defico, resoco
    character(len=24) :: codere
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: meelem(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=19) :: depest
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PREDICTION - DEPL. DONNE)
!
! PROJECTION DU CHAMP DONNE SUR L'ESPACE DES CONDITIONS AUX LIMITES
! CINEMATIQUEMENT ADMISSIBLES
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
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION
! IN  SOLVEU : SOLVEUR
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  SDIMPR : SD AFFICHAGE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDTIME : SD TIMER
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  MATASS : MATRICE ASSEMBLEE
! IN  MAPREC : MATRICE DE PRECONDITIONNEMENT (GCPC)
! IN  DEFICO : SD DEFINITION CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  DEPEST : DEPLACEMENT ESTIME
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
    integer :: neq, i
    character(len=19) :: depso1, depso2, cncine
    character(len=19) :: solu1, solu2, cndonn, cnpilo, cncind
    real(kind=8), pointer :: dep1(:) => null()
    real(kind=8), pointer :: dep2(:) => null()
    real(kind=8), pointer :: sol1(:) => null()
    real(kind=8), pointer :: sol2(:) => null()
    integer, pointer :: delg(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    solu1 = '&&CNPART.CHP2'
    solu2 = '&&CNPART.CHP3'
    cndonn = '&&CNCHAR.DONN'
    cnpilo = '&&CNCHAR.PILO'
    cncind = '&&CNCHAR.CINE'
    call vtzero(solu1)
    call vtzero(solu2)
    call vtzero(cndonn)
    call vtzero(cnpilo)
    call vtzero(cncind)
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    ldccvg = -1
    faccvg = -1
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPSO1', depso1)
    call nmchex(solalg, 'SOLALG', 'DEPSO2', depso2)
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
!
! --- CALCUL DE LA MATRICE GLOBALE
!
    call nmprma(modele, mate, carele, compor, carcri,&
                parmet, method, lischa, numedd, numfix,&
                solveu, comref, sdimpr, sdstat, sdtime,&
                sddisc, sddyna, numins, fonact, defico,&
                resoco, valinc, solalg, veelem, meelem,&
                measse, maprec, matass, codere, faccvg,&
                ldccvg)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if ((faccvg.eq.1) .or. (faccvg.eq.2)) goto 999
    if (ldccvg .eq. 1) goto 999
!
! --- CALCUL DU SECOND MEMBRE
! --- PRISE EN COMPTE DES CL DUALISEES
!
    call nmassd(modele, numedd, lischa, fonact, depest,&
                veasse, matass, cnpilo, cndonn)
!
! --- PRISE EN COMPTE DES CL ELIMINEES
!
    call copisd('CHAMP_GD', 'V', cncine, cncind)
    call nmacin(fonact, matass, depso1, cncind)
!
! --- RESOLUTION
!
    call nmreso(fonact, cndonn, cnpilo, cncind, solveu,&
                maprec, matass, solu1, solu2, rescvg)
!
! --- ERREUR SANS POSSIBILITE DE CONTINUER
!
    if (rescvg .eq. 1) goto 999
!
! --- CORRECTION DU DEPLACEMENT DONNE POUR LE RENDRE
! --- CINEMATIQUEMENT ADMISSIBLE
!
    call jeveuo(numedd(1:14)// '.NUME.DELG', 'L', vi=delg)
    call jeveuo(solu1 (1:19)//'.VALE', 'L', vr=sol1)
    call jeveuo(solu2 (1:19)//'.VALE', 'L', vr=sol2)
    call jeveuo(depso1(1:19)//'.VALE', 'E', vr=dep1)
    call jeveuo(depso2(1:19)//'.VALE', 'E', vr=dep2)
!
! --- LES LAGRANGES NE SONT PAS MODIFIES
!
    do i = 1, neq
        if (delg(i) .eq. 0) then
            dep1(i) = dep1(i)+sol1(i)
            dep2(i) = sol2(i)
        endif
    end do
!
999 continue
!
    call jedema()
end subroutine
