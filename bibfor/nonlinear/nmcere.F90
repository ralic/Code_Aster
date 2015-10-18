subroutine nmcere(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, fonact, sdstat,&
                  ds_contact, iterat, sdnume, valinc, solalg,&
                  veelem, veasse, sdtime, offset, rho,&
                  eta, residu, ldccvg, matass)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/majour.h"
#include "asterfort/nmadir.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmbudi.h"
#include "asterfort/nmcha0.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmchcp.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmchso.h"
#include "asterfort/nmdiri.h"
#include "asterfort/nmfext.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmpilr.h"
#include "asterfort/nmtime.h"
#include "asterfort/r8inir.h"
#include "blas/daxpy.h"
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
    integer :: iterat, ldccvg
    real(kind=8) :: eta, rho, offset, residu
    character(len=19) :: lischa, sdnume, matass
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=24) :: carcri
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24) :: sdtime, sdstat
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! CHOIX DU ETA DE PILOTAGE PAR CALCUL DU RESIDU
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! In  ds_contact       : datastructure for contact management
! IN  SDNUME : SD NUMEROTATION
! IN  SDSTAT : SD STATISTIQUES
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  OFFSET : DECALAGE DE ETA_PILOTAGE EN FONCTION DE RHO
! IN  RHO    : PARAMETRE DE RECHERCHE_LINEAIRE
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  SDNUME : SD NUMEROTATION
! IN  SDTIME : SD TIMER
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT RESIDU : RESIDU OPTIMAL SI L'ON A CHOISI LE RESIDU
! IN  MATASS : SD MATRICE ASSEMBLEE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer, parameter :: zvalin = 28
    integer, parameter :: zsolal = 17
    aster_logical :: lgrot, lendo
    integer :: neq, nmax
    character(len=19) :: vefint, vediri, vebudi
    character(len=19) :: cnfint, cndiri, cnfext, cnbudi
    character(len=24) :: codere
    character(len=19) :: valint(zvalin)
    character(len=19) :: solalt(zsolal)
    character(len=19) :: depdet, depdel, deppr1, deppr2
    character(len=19) :: depplt, ddep
    character(len=19) :: depplu
    character(len=19) :: sigplu, varplu, complu
    character(len=19) :: depl, vite, acce, k19bla
    real(kind=8), pointer :: ddepl(:) => null()
    real(kind=8), pointer :: depdl(:) => null()
    real(kind=8), pointer :: depdt(:) => null()
    real(kind=8), pointer :: deppl(:) => null()
    real(kind=8), pointer :: deppt(:) => null()
    real(kind=8), pointer :: du0(:) => null()
    real(kind=8), pointer :: du1(:) => null()
!
! ----------------------------------------------------------------------
!
    call infdbg('PILOTAGE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ...... CALCUL DU RESIDU'
    endif
!
! --- INITIALISATIONS
!
    k19bla = ' '
    lgrot = isfonc(fonact,'GD_ROTA')
    lendo = isfonc(fonact,'ENDO_NO')
    ddep = '&&CNCETA.CHP0'
    depdet = '&&CNCETA.CHP1'
    depplt = '&&CNCETA.CHP2'
    codere = '&&NMCETA.CODRE1'
    ldccvg = -1
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    call nmchai('VALINC', 'LONMAX', nmax)
    ASSERT(nmax.eq.zvalin)
    call nmchai('SOLALG', 'LONMAX', nmax)
    ASSERT(nmax.eq.zsolal)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
    call nmchex(solalg, 'SOLALG', 'DEPPR2', deppr2)
    call nmchex(veelem, 'VEELEM', 'CNDIRI', vediri)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veelem, 'VEELEM', 'CNBUDI', vebudi)
    call nmchex(veasse, 'VEASSE', 'CNBUDI', cnbudi)
!
! --- MISE A JOUR DEPLACEMENT
! --- DDEP = RHO*DEPPRE(1) + (ETA-OFFSET)*DEPPRE(2)
!
    call jeveuo(deppr1(1:19)//'.VALE', 'L', vr=du0)
    call jeveuo(deppr2(1:19)//'.VALE', 'L', vr=du1)
    call jeveuo(ddep(1:19) //'.VALE', 'E', vr=ddepl)
    call r8inir(neq, 0.d0, ddepl, 1)
    call daxpy(neq, rho, du0, 1, ddepl,&
               1)
    call daxpy(neq, eta-offset, du1, 1, ddepl,&
               1)
!
! --- MISE A JOUR DE L'INCREMENT DE DEPLACEMENT DEPUIS LE DEBUT
! --- DU PAS DE TEMPS DEPDET = DEPDEL+DDEP
!
    call jeveuo(depdel(1:19)//'.VALE', 'L', vr=depdl)
    call jeveuo(depdet(1:19)//'.VALE', 'E', vr=depdt)
    call majour(neq, lgrot, lendo, sdnume, depdl,&
                ddepl, 1.d0, depdt, 0)
!
! --- MISE A JOUR DU DEPLACEMENT DEPPLT = DEPPLU+DDEP
!
    call jeveuo(depplu(1:19)//'.VALE', 'L', vr=deppl)
    call jeveuo(depplt(1:19)//'.VALE', 'E', vr=deppt)
    call majour(neq, lgrot, lendo, sdnume, deppl,&
                ddepl, 1.d0, deppt, 1)
!
! --- RECONSTRUCTION DES VARIABLES CHAPEAUX
!
    call nmcha0('VALINC', 'ALLINI', ' ', valint)
    call nmchcp('VALINC', valinc, valint)
    call nmcha0('VALINC', 'DEPPLU', depplt, valint)
    call nmchso(solalg, 'SOLALG', 'DEPDEL', depdet, solalt)
    call nmchex(valint, 'VALINC', 'DEPPLU', depl)
    call nmchex(valint, 'VALINC', 'VITPLU', vite)
    call nmchex(valint, 'VALINC', 'ACCPLU', acce)
!
! --- REACTUALISATION DES FORCES INTERIEURES
!
!
    call nmfint(modele, mate, carele, comref, compor,&
                carcri, fonact, iterat, k19bla, sdstat,&
                sdtime, valint, solalt, ldccvg, codere,&
                vefint)
!
! --- ASSEMBLAGE DES FORCES INTERIEURES
!
    call nmaint(numedd, fonact, ds_contact, veasse, vefint,&
                cnfint, sdnume)
!
! --- MESURES
!
    call nmtime(sdtime, 'INI', 'SECO_MEMB')
    call nmtime(sdtime, 'RUN', 'SECO_MEMB')
!
! --- REACTUALISATION DES REACTIONS D'APPUI BT.LAMBDA
!
    call nmdiri(modele, mate, carele, lischa, k19bla,&
                depl, vite, acce, vediri)
    call nmadir(numedd, fonact, ds_contact, veasse, vediri,&
                cndiri)
!
! --- REACTUALISATION DES CONDITIONS DE DIRICHLET B.U
!
    call nmbudi(modele, numedd, lischa, depplt, vebudi,&
                cnbudi, matass)
!
! --- REACTUALISATION DES EFFORTS EXTERIEURS (AVEC ETA)
!
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmfext(eta, fonact, k19bla, veasse, cnfext)
!
    call nmtime(sdtime, 'END', 'SECO_MEMB')
!
! --- ON A FORCEMENT INTEGRE LA LDC !
!
    ASSERT(ldccvg.ge.0)
!
! --- CALCUL DU RESIDU
!
    if (ldccvg .eq. 0) then
        call nmpilr(fonact, numedd, matass, veasse, residu,&
                    eta)
    endif
!
end subroutine
