subroutine nmrepl(modele , numedd, mate       , carele, comref,&
                  compor , lischa, ds_algopara, carcri, fonact,&
                  iterat , sdstat, sdpilo     , sdnume, sddyna,&
                  ds_contact, deltat     , valinc, solalg,&
                  veelem , veasse, sdtime     , sddisc, etan  ,&
                  ds_conv, eta   , offset     , ldccvg, pilcvg,&
                  matass )
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/ismaem.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmceta.h"
#include "asterfort/nmcha0.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmchso.h"
#include "asterfort/nmfext.h"
#include "asterfort/nmpilo.h"
#include "asterfort/nmpilr.h"
#include "asterfort/nmrelp.h"
#include "asterfort/nmrep2.h"
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
    integer :: iterat
    real(kind=8) :: deltat, eta, etan, offset
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    character(len=19) :: lischa, sddyna, sdnume, sdpilo, sddisc, matass
    character(len=24) :: carcri
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24) :: sdstat, sdtime
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    type(NL_DS_Conv), intent(inout) :: ds_conv
    integer :: pilcvg, ldccvg
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! CHOIX DU ETA DE PILOTAGE AVEC RECHERCHE LINEAIRE
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
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  SDPILO : SD PILOTAGE
! IN  SDNUME : SD NUMEROTATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDSTAT : SD STATISTIQUES
! In  ds_algopara      : datastructure for algorithm parameters
! In  ds_contact       : datastructure for contact management
! IN  DELTAT : INCREMENT DE TEMPS
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  ETAN   : ETA_PILOTAGE AU DEBUT DE L'ITERATION
! IN  SDTIME : SD TIMER
! IN  SDDISC : SD DISCRETISATION
! IO  ds_conv          : datastructure for convergence management
! OUT ETA    : PARAMETRE DE PILOTAGE
! OUT RHO    : PARAMETRE DE RECHERCHE_LINEAIRE
! OUT OFFSET : DECALAGE DE ETA_PILOTAGE EN FONCTION DE RHO
! OUT PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
!                -1 : PAS DE CALCUL DU PILOTAGE
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : PAS DE SOLUTION
!                 2 : BORNE ATTEINTE -> FIN DU CALCUL
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! IN  MATASS : SD MATRICE ASSEMBLEE
!
! ----------------------------------------------------------------------
!
    integer :: zveass, zsolal, zvalin
    parameter    (zveass=32,zsolal=17,zvalin=28)
!
    aster_logical :: exopt, mieux, irecli
    integer :: itrlmx, iterho, act, opt
    integer :: pilopt
    integer :: nbeffe
    integer :: nr, pos, nbsto, n, nbatte, nmax
    real(kind=8) :: rhomin, rhomax, rhoexm, rhoexp, relirl, fcvg
    real(kind=8) :: rhoopt, f0, fopt, proeta(2)
    real(kind=8) :: r(1002), g(1002), memfg(1002)
    real(kind=8) :: fgmax, fgmin, amelio, residu, etaopt, rho
    character(len=19) :: veasst(zveass), solalt(zsolal), valint(zvalin, 2)
    character(len=19) :: cnfins(2), cndirs(2), k19bla
    character(len=19) :: cndiri, cnfint, cnfext
    character(len=19) :: depplu, sigplu, varplu, complu
    character(len=19) :: depdet
    character(len=19) :: sigplt, varplt, depplt
    character(len=24) :: typilo
    integer :: ifm, niv
    character(len=24), pointer :: pltk(:) => null()
!
! ----------------------------------------------------------------------
!
    call infdbg('PILOTAGE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<PILOTAGE> ... PILOTAGE AVEC RECH_LINE'
    endif
!
! --- INITIALISATIONS
!
    fopt = r8maem()
    pilopt = ismaem()
    nbsto = 0
    exopt = .false.
    irecli = .true.
    pilcvg = -1
    ldccvg = -1
    k19bla = ' '
    call nmchai('VEASSE', 'LONMAX', nmax)
    ASSERT(nmax.eq.zveass)
    call nmchai('SOLALG', 'LONMAX', nmax)
    ASSERT(nmax.eq.zsolal)
    call nmchai('VALINC', 'LONMAX', nmax)
    ASSERT(nmax.eq.zvalin)
!
! --- PARAMETRES RECHERCHE LINEAIRE
!
    itrlmx = ds_algopara%line_search%iter_maxi
    rhomin = ds_algopara%line_search%rho_mini
    rhomax = ds_algopara%line_search%rho_maxi
    rhoexm = -ds_algopara%line_search%rho_excl
    rhoexp = ds_algopara%line_search%rho_excl
    relirl = ds_algopara%line_search%resi_rela
    ASSERT(itrlmx.le.1000)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
!
! --- LECTURE DONNEES PILOTAGE
!
    call jeveuo(sdpilo(1:19)//'.PLTK', 'L', vk24=pltk)
    typilo = pltk(1)
!
! --- FONCTIONS DE PILOTAGE LINEAIRES : RECHERCHE LINEAIRE STANDARD
!
    if (typilo .eq. 'DDL_IMPO') then
        call nmrelp(modele , numedd, mate  , carele     , comref,&
                    compor , lischa, carcri, fonact     , iterat,&
                    sdstat , sdnume, sddyna, ds_algopara, ds_contact,&
                    valinc , solalg, veelem, veasse     , sdtime,&
                    ds_conv, ldccvg)
        goto 999
    endif
!
! --- PREPARATION DES ZONES TEMPORAIRES POUR ITERATION COURANTE
!
    cnfins(1) = cnfint
    cnfins(2) = '&&CNREPL.CHP1'
    cndirs(1) = cndiri
    cndirs(2) = '&&CNREPL.CHP2'
    depdet = '&&CNREPL.CHP3'
    depplt = '&&CNREPL.CHP4'
    sigplt = '&&NMREPL.SIGPLU'
    varplt = '&&NMREPL.VARPLU'
    call copisd('CHAMP_GD', 'V', varplu, varplt)
    call copisd('CHAMP_GD', 'V', sigplu, sigplt)
    call copisd('CHAMP_GD', 'V', depplu, depplt)
!
! --- CONSTRUCTION DES VARIABLES CHAPEAUX
!
    call nmcha0('VALINC', 'ALLINI', ' ', valint(1, 1))
    call nmchso(valinc, 'VALINC', '      ', k19bla, valint(1, 1))
    call nmchso(valint(1, 1), 'VALINC', 'DEPPLU', depplt, valint(1, 1))
    call nmcha0('VALINC', 'ALLINI', ' ', valint(1, 2))
    call nmchso(valinc, 'VALINC', '      ', k19bla, valint(1, 2))
    call nmchso(valint(1, 2), 'VALINC', 'DEPPLU', depplt, valint(1, 2))
    call nmchso(valint(1, 2), 'VALINC', 'SIGPLU', sigplt, valint(1, 2))
    call nmchso(valint(1, 2), 'VALINC', 'VARPLU', varplt, valint(1, 2))
    call nmchso(solalg, 'SOLALG', 'DEPDEL', depdet, solalt)
    call nmchso(veasse, 'VEASSE', 'CNDIRI', cndirs(1), veasst)
!
! --- CALCUL DE F(RHO=0)
!
    call nmpilr(fonact, numedd, matass, veasse, f0,&
                etan)
    fcvg = abs(relirl * f0)
!
! --- INITIALISATION ET DIRECTION DE DESCENTE
!
    nr = 2
    r(1) = 0.d0
    r(2) = 1.d0
    g(1) = f0
    pos = 2
    nbatte = 2
!
! --- BOUCLE DE RECHERCHE LINEAIRE
!
    rho = 1.d0
    act = 1
!
    do iterho = 0, itrlmx
!
! ----- RESOLUTION DE L'EQUATION DE PILOTAGE: NVELLE DIRECT. DE DESCENTE
!
        call nmpilo(sdpilo, deltat, rho, solalg, veasse,&
                    modele, mate, compor, ds_contact, valinc,&
                    nbatte, numedd, nbeffe, proeta, pilcvg,&
                    carele)
        if (pilcvg .eq. 1) goto 999
!
! ----- DECALAGE DU ETA_PILOTAGE
!
        offset = etan*(1-rho)
        do n = 1, nbeffe
            proeta(n) = proeta(n) + offset
        end do
!
! ----- CHOIX DU ETA_PILOTAGE
!
        call nmchso(veasse, 'VEASSE', 'CNDIRI', cndirs(act), veasst)
        call nmchso(veasse, 'VEASSE', 'CNFINT', cnfins(act), veasst)
        call nmceta(modele, numedd, mate, carele, comref,&
                    compor, lischa, carcri, fonact, sdstat,&
                    ds_contact, sdpilo, iterat, sdnume, valint(1, act),&
                    solalg, veelem, veasst, sdtime, sddisc,&
                    nbeffe, irecli, proeta, offset, rho,&
                    eta, ldccvg, pilcvg, residu, matass)
!
! ----- PB CVG: S'IL EXISTE DEJA UN RHO OPTIMAL, ON LE CONSERVE
! ----- ET ON SORT
!
        if (ldccvg .gt. 0) then
            if (exopt) goto 100
            goto 999
        endif
!
! ---    SI ON A PAS ENCORE CONVERGE LE PILO :
! ---      * ON PREND UN PILO CONVERGE QQ SOIT LE RESIDU
! ---    SINON :
! ---      * ON CHERCHE A BAISSER LE RESIDU AVEC UN PILO CONVERGE
!
        if (pilopt .gt. 0) then
            mieux = ((pilcvg.eq.0).or.(pilcvg.eq.2).or.( residu.lt.fopt))
        else
            mieux = (((pilcvg.eq.0).or.(pilcvg.eq.2)).and. ( residu.lt.fopt))
        endif
!
        if (mieux) then
            exopt = .true.
            rhoopt = rho
            etaopt = eta
            pilopt = pilcvg
            fopt = residu
            opt = act
            act = 3 - act
        endif
!
! ---   MEMOIRE DES RESIDUS ATTEINTS
!
        nbsto = nbsto + 1
        memfg(nbsto) = residu
!
! ---   ARRET SI SATISFACTION DU CRITERE
!
        if (residu .lt. fcvg) goto 100
!
! ---   ARRET SI IL N'Y A PLUS D'AMELIORATIONS SIGNIFICATIVES
!
        if (nbsto .ge. 3) then
            fgmax = max(memfg(nbsto),memfg(nbsto-1),memfg(nbsto-2))
            fgmin = min(memfg(nbsto),memfg(nbsto-1),memfg(nbsto-2))
            amelio = fgmin / fgmax
            if (amelio .gt. 0.95d0) goto 100
        endif
!
! ---   CALCUL DE RHO(N+1) PAR INTERPOLATION QUADRATIQUE AVEC BORNES
!
        g(pos) = residu
        call nmrep2(nr, r, g, fcvg, rhomin,&
                    rhomax, rhoexm, rhoexp, pos)
        rho = r(pos)
    end do
    iterho = itrlmx
!
! --- STOCKAGE DU RHO OPTIMAL ET DES CHAMPS CORRESPONDANTS
!
100 continue
!
! --- CALCUL DE ETA_PILOTAGE
!
    eta = etaopt
    rho = rhoopt
!
! --- REACTUALISATION DES EFFORTS EXTERIEURS (AVEC ETA)
!
    call nmchex(veasst, 'VEASSE', 'CNFEXT', cnfext)
    call nmfext(eta, fonact, sddyna, veasst, cnfext)
!
! --- RECUPERATION DES VARIABLES EN T+ (PAS DE RECALCUL)
!
    if (opt .ne. 1) then
        call copisd('CHAMP_GD', 'V', sigplt, sigplu)
        call copisd('CHAMP_GD', 'V', varplt, varplu)
        call copisd('CHAMP_GD', 'V', cnfins(opt), cnfint)
        call copisd('CHAMP_GD', 'V', cndirs(opt), cndiri)
    endif
!
! - Save results of line search
!
    ds_conv%line_sear_coef = rhoopt
    ds_conv%line_sear_iter = iterho
    pilcvg = pilopt
999 continue
!
! --- LE CALCUL DE PILOTAGE A FORCEMENT ETE REALISE
!
    ASSERT(pilcvg.ge.0)
!
end subroutine
