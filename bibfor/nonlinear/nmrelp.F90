subroutine nmrelp(modele , numedd, mate  , carele     , comref    ,&
                  compor , lischa, carcri, fonact     , iterat    ,&
                  ds_measure , sdnume, sddyna, ds_algopara, ds_contact,&
                  valinc , solalg, veelem, veasse     ,&
                  ds_conv, ldccvg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8maem.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmadir.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmcha0.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmchso.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmdiri.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmmaji.h"
#include "asterfort/nmrebo.h"
#include "asterfort/nmrech.h"
#include "asterfort/nmrecz.h"
#include "asterfort/nmtime.h"
#include "asterfort/vlaxpy.h"
#include "asterfort/vtcreb.h"
#include "asterfort/vtzero.h"
#include "asterfort/zbinit.h"
#include "blas/daxpy.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
! aslint: disable=W1504
!
    integer :: fonact(*)
    integer :: iterat, ldccvg
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=24) :: carcri
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=19) :: lischa, sddyna, sdnume
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    type(NL_DS_Conv), intent(inout) :: ds_conv
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! RECHERCHE LINEAIRE DANS LA DIRECTION DE DESCENTE
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
! IO  ds_measure       : datastructure for measure and statistics management
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  SDNUME : SD NUMEROTATION
! In  ds_contact       : datastructure for contact management
! In  ds_algopara      : datastructure for algorithm parameters
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  SDDYNA : SD DYNAMIQUE
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! IO  ds_conv          : datastructure for convergence management
!
! ----------------------------------------------------------------------
!
    integer :: zsolal, zvalin
    parameter    (zsolal=17,zvalin=28)
!
    integer :: itrlmx, iterho, neq, act, opt, ldcopt
    integer :: dimmem, nmax
    real(kind=8) :: rhomin, rhomax, rhoexm, rhoexp
    real(kind=8) :: rhom, rhoopt, rho
    real(kind=8) :: f0, fm, f, fopt, fcvg
    real(kind=8) :: parmul, relirl, sens
    real(kind=8) :: mem(2, 10)
    aster_logical :: stite, lnkry
    aster_logical :: lgrot, lendo
    character(len=19) :: cnfins(2), cndirs(2), k19bla
    character(len=19) :: depplu, sigplu, varplu, complu
    character(len=19) :: sigplt, varplt, depplt
    character(len=24) :: codere
    character(len=19) :: vefint, vediri
    character(len=19) :: cnfint, cndiri, cnfext
    character(len=19) :: depdet, ddepla, depdel
    character(len=19) :: solalt(zsolal), valint(zvalin, 2)
    aster_logical :: echec
    integer :: ifm, niv
    real(kind=8), pointer :: vale(:) => null()
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... RECHERCHE LINEAIRE'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    lgrot = isfonc(fonact,'GD_ROTA')
    lendo = isfonc(fonact,'ENDO_NO')
    lnkry = isfonc(fonact,'NEWTON_KRYLOV')
!
! --- INITIALISATIONS
!
    opt = 1
    parmul = 3.d0
    fopt = r8maem()
    k19bla = ' '
    ldccvg = -1
    call nmchai('VALINC', 'LONMAX', nmax)
    ASSERT(nmax.eq.zvalin)
    call nmchai('SOLALG', 'LONMAX', nmax)
    ASSERT(nmax.eq.zsolal)
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
    dimmem = 10
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmchex(veasse, 'VEASSE', 'CNFINT', cnfint)
    call nmchex(veasse, 'VEASSE', 'CNDIRI', cndiri)
    call nmchex(veasse, 'VEASSE', 'CNFEXT', cnfext)
    call nmchex(veelem, 'VEELEM', 'CNFINT', vefint)
    call nmchex(veelem, 'VEELEM', 'CNDIRI', vediri)
    call nmchex(solalg, 'SOLALG', 'DDEPLA', ddepla)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- ACCES VARIABLES
!
    call jeveuo(ddepla(1:19)//'.VALE', 'E', vr=vale)
!
! --- PREPARATION DES ZONES TEMPORAIRES POUR ITERATION COURANTE
!
    cnfins(1) = cnfint
    cnfins(2) = '&&NMRECH.RESI'
    cndirs(1) = cndiri
    cndirs(2) = '&&NMRECH.DIRI'
    depdet = '&&CNPART.CHP1'
    depplt = '&&CNPART.CHP2'
    sigplt = '&&NMRECH.SIGP'
    varplt = '&&NMRECH.VARP'
    call vtzero(depdet)
    call vtzero(depplt)
    call copisd('CHAMP_GD', 'V', varplu, varplt)
    call copisd('CHAMP_GD', 'V', sigplu, sigplt)
    call vtcreb('&&NMRECH.RESI', 'V', 'R', nume_ddlz = numedd, nb_equa_outz = neq)
    call vtcreb('&&NMRECH.DIRI', 'V', 'R', nume_ddlz = numedd, nb_equa_outz = neq)
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
!
! --- CALCUL DE F(RHO=0)
!
    call nmrecz(numedd, cndiri, cnfint, cnfext, ddepla,&
                f0)
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... FONCTIONNELLE INITIALE: ',f0
    endif
!
! --- VALEUR DE CONVERGENCE
!
    fcvg = abs(relirl * f0)
!
! --- INITIALISATION ET DIRECTION DE DESCENTE
!
    if (ds_algopara%line_search%method .eq. 'CORDE') then
        sens = 1.d0
        rhom = 0.d0
        fm = f0
        rhoopt = 1.d0
    else if (ds_algopara%line_search%method .eq.'MIXTE') then
        if (f0 .le. 0.d0) then
            sens = 1.d0
        else
            sens = -1.d0
        endif
        call zbinit(sens*f0, parmul, dimmem, mem)
        rhoopt = 1.d0
    else
        ASSERT(.false.)
    endif
!
! --- BOUCLE DE RECHERCHE LINEAIRE
!
    rho = sens
    act = 1
!
    do iterho = 0, itrlmx
!
! ----- CALCUL DE L'INCREMENT DE DEPLACEMENT TEMPORAIRE
!
        call nmmaji(numedd, lgrot, lendo, sdnume, rho,&
                    depdel, ddepla, depdet, 0)
        call nmmaji(numedd, lgrot, lendo, sdnume, rho,&
                    depplu, ddepla, depplt, 1)
        if (lnkry) then
            call vlaxpy(1.d0-rho, ddepla, depdet)
            call vlaxpy(1.d0-rho, ddepla, depplt)
        endif
!
! ----- AFFICHAGE
!
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ...... ITERATION <',iterho,'>'
            write (ifm,*) '<MECANONLINE> ...... RHO COURANT = ',rho
            write (ifm,*) '<MECANONLINE> ...... INCREMENT DEPL.'
            call nmdebg('VECT', depplt, 6)
            write (ifm,*) '<MECANONLINE> ...... INCREMENT DEPL. TOTAL'
            call nmdebg('VECT', depdet, 6)
        endif
!
! ----- REACTUALISATION DES FORCES INTERIEURES
!
        call nmfint(modele, mate, carele, comref, compor,&
                    carcri, fonact, iterat, sddyna, ds_measure,&
                    valint(1, act), solalt, ldccvg, codere,&
                    vefint)
!
! ----- ASSEMBLAGE DES FORCES INTERIEURES
!
        call nmaint(numedd, fonact, ds_contact, veasse, vefint,&
                    cnfins(act), sdnume)
!
! ----- REACTUALISATION DES REACTIONS D'APPUI BT.LAMBDA
!
        call nmtime(ds_measure, 'Init'  , '2nd_Member')
        call nmtime(ds_measure, 'Launch', '2nd_Member')
        call nmdiri(modele, mate, carele, lischa, sddyna,&
                    depplt, k19bla, k19bla, vediri)
        call nmadir(numedd, fonact, ds_contact, veasse, vediri,&
                    cndirs(act))
        call nmtime(ds_measure, 'Stop', '2nd_Member')
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ...... FORCES INTERNES'
            call nmdebg('VECT', cnfins(act), 6)
            write (ifm,*) '<MECANONLINE> ...... REACTIONS D''APPUI'
            call nmdebg('VECT', cndirs(act), 6)
        endif
!
! ----- ON A NECESSAIREMENT INTEGRE LA LOI DE COMPORTEMENT
!
        ASSERT(ldccvg.ne.-1)
!
! ----- ECHEC A L'INTEGRATION DE LA LOI DE COMPORTEMENT
!
        if (ldccvg .ne. 0) then
!
! ------- S'IL EXISTE DEJA UN RHO OPTIMAL, ON LE CONSERVE
!
            if (iterho .gt. 0) then
                goto 100
            else
                goto 999
            endif
        endif
!
! ----- CALCUL DE F(RHO)
!
        call nmrecz(numedd, cndirs(act), cnfins(act), cnfext, ddepla,&
                    f)
!
        if (niv .ge. 2) then
            write (ifm,*) '<MECANONLINE> ... FONCTIONNELLE COURANTE: ',f
        endif
!
! ----- CALCUL DU RHO OPTIMAL
!
        if (ds_algopara%line_search%method .eq. 'CORDE') then
            call nmrech(fm, f, fopt, fcvg, rhomin,&
                        rhomax, rhoexm, rhoexp, rhom, rho,&
                        rhoopt, ldcopt, ldccvg, opt, act,&
                        stite)
!
        else if (ds_algopara%line_search%method .eq. 'MIXTE') then
            call nmrebo(f, mem, sens, rho, rhoopt,&
                        ldcopt, ldccvg, fopt, fcvg, opt,&
                        act, rhomin, rhomax, rhoexm, rhoexp,&
                        stite, echec)
            if (echec) then
                goto 100
            endif
        else
            ASSERT(.false.)
        endif
        if (stite) then
            goto 100
        endif
    end do
    iterho = itrlmx
!
! --- STOCKAGE DU RHO OPTIMAL ET DES CHAMPS CORRESPONDANTS
!
100 continue
!
! --- AJUSTEMENT DE LA DIRECTION DE DESCENTE
!
    call daxpy(neq, rhoopt-1.d0, vale, 1, vale,&
               1)
!
! --- RECUPERATION DES VARIABLES EN T+ SI NECESSAIRE
!
    if (opt .ne. 1) then
        call copisd('CHAMP_GD', 'V', sigplt, sigplu)
        call copisd('CHAMP_GD', 'V', varplt, varplu)
        call copisd('CHAMP_GD', 'V', cnfins(opt), cnfint)
        call copisd('CHAMP_GD', 'V', cndirs(opt), cndiri)
    endif
!
! --- INFORMATIONS SUR LA RECHERCHE LINEAIRE
!
    ldccvg = ldcopt
!
999 continue
!
! - Save results of line search
!
    ds_conv%line_sear_coef = rhoopt
    ds_conv%line_sear_iter = iterho
!
    call detrsd('CHAMP', '&&NMRECH.RESI')
    call detrsd('CHAMP', '&&NMRECH.DIRI')
!
end subroutine
