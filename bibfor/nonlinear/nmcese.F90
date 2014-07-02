subroutine nmcese(modele, numedd, mate, carele, comref,&
                  compor, lischa, carcri, fonact, sdstat,&
                  defico, iterat, sdnume, sdpilo, valinc,&
                  solalg, veelem, veasse, sdtime, offset,&
                  typsel, sddisc, licite, rho, eta,&
                  etaf, criter, ldccvg, pilcvg, matass)
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
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/exixfe.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmceai.h"
#include "asterfort/nmceni.h"
#include "asterfort/nmcere.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmrcyc.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
    integer :: fonact(*)
    integer :: iterat
    real(kind=8) :: rho, offset, eta(2)
    character(len=19) :: lischa, sdnume, sdpilo, sddisc, matass
    character(len=24) :: modele, numedd, mate, carele, comref, compor
    character(len=24) :: carcri, defico
    character(len=19) :: veelem(*), veasse(*)
    character(len=19) :: solalg(*), valinc(*)
    character(len=24) :: typsel, sdtime, sdstat
    integer :: licite(2)
    integer :: ldccvg, pilcvg
    real(kind=8) :: etaf, criter
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! SELECTION DU PARAMETRE DE PILOTAGE ENTRE DEUX SOLUTIONS
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SDPILO : SD PILOTAGE
! IN  SDSTAT : SD STATISTIQUES
! IN  SDNUME : SD NUMEROTATION
! IN  CARCRI : PARAMETRES DES METHODES D'INTEGRATION LOCALES
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  DEFICO : SD DEFINITION CONTACT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  ITERAT : NUMERO D'ITERATION DE NEWTON
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  OFFSET : DECALAGE DE ETA_PILOTAGE EN FONCTION DE RHO
! IN  TYPSEL : TYPE DE SELECTION PILOTAGE
!                'ANGL_INCR_DEPL'
!                'NORM_INCR_DEPL'
!                'RESIDU'
! IN  SDDISC : SD DISCRETISATION
! IN  SDTIME : SD TIMER
! IN  LICITE : CODE RETOUR PILOTAGE DES DEUX PARAMETRES DE PILOTAGE
! IN  RHO    : PARAMETRE DE RECHERCHE_LINEAIRE
! IN  ETA    : LES DEUX PARAMETRES DE PILOTAGE
! OUT ETAF   : PARAMETRE DE PILOTAGE FINALEMENT CHOISI
! OUT CRITER : VALEUR DU CRITERE DE COMPARAISON
!                ANGL_INCR_DEPL
!                NORM_INCR_DEPL
!                RESIDU
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! I/O PILCVG : CODE DE CONVERGENCE POUR LE PILOTAGE
!                -1 : PAS DE CALCUL DU PILOTAGE
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : PAS DE SOLUTION
!                 2 : BORNE ATTEINTE -> FIN DU CALCUL
! IN  MATASS : SD MATRICE ASSEMBLEE
!
!
!
!
    integer :: ldccv(2), ierm, indic, sel
    real(kind=8) :: f(2), r8bid
    character(len=8) :: choix, txt
    character(len=19) :: depold, depdel, deppr1, deppr2
    character(len=24) :: typpil
    integer :: ifm, niv, ib, ibid
    aster_logical :: swloun, isxfe
    aster_logical :: switch, mixte
    real(kind=8) :: miincr, miresi, contra, precyc, fnid(2)
    real(kind=8), pointer :: plir(:) => null()
    character(len=24), pointer :: pltk(:) => null()
    parameter     (contra=0.1d0,precyc=5.d-2)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PILOTAGE', ifm, niv)
!
! --- LE CALCUL DE PILOTAGE A FORCEMENT ETE REALISE
!
    ASSERT(pilcvg.ge.0)
!
! --- INITIALISATIONS
!
    call jeveuo(sdpilo(1:19)//'.PLTK', 'L', vk24=pltk)
    typpil = pltk(1)
    f(1) = 0.d0
    f(2) = 0.d0
    ldccv(1) = -1
    ldccv(2) = -1
    ldccvg = -1
!
! --- STRATEGIE MIXTE BASEE SUR LE CONTRASTE DES CRITERES DE CHOIX
!
    mixte = typsel.eq.'MIXTE'
    switch = .false.
!
! --- VERIFICATION DE LA COMPATIBILITE
!
    if (mixte) then
        call utdidt('L', sddisc, 'ECHE', 0, 'CHOIX_SOLU_PILO',&
                    r8bid, ibid, choix)
        if (choix .eq. 'AUTRE') then
            call utmess('F', 'MECANONLINE_62')
        endif
    endif
!
! --- STRATEGIE BASEE SUR LES TECHNIQUES EVENT-DRIVEN 'AUTRE_PILOTAGE'
!
    swloun = .false.
!
! --- ETONNANT QUE X-FEM SE GLISSE A CE NIVEAU DU CODE (ET PLUS AVANT)
!
    call exixfe(modele, ierm)
    isxfe = (ierm.eq.1)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(solalg, 'SOLALG', 'DEPPR1', deppr1)
    call nmchex(solalg, 'SOLALG', 'DEPPR2', deppr2)
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- SELECTION SELON LA METHODE CHOISIE: ANGL_INCR_DEPL
!
    if (typsel .eq. 'ANGL_INCR_DEPL') then
!
        if (typpil .eq. 'LONG_ARC' .or. typpil .eq. 'SAUT_LONG_ARC') then
            call jeveuo(sdpilo(1:19)//'.PLIR', 'L', vr=plir)
            swloun = plir(1)*plir(6).lt.0.d0
        endif
!
        call nmceai(numedd, depdel, deppr1, deppr2, depold,&
                    sdpilo, rho, eta(1), isxfe, f(1),&
                    indic)
        call nmceai(numedd, depdel, deppr1, deppr2, depold,&
                    sdpilo, rho, eta(2), isxfe, f(2),&
                    indic)
        if (indic .eq. 0) then
            call nmceni(numedd, depdel, deppr1, deppr2, rho,&
                        sdpilo, eta(1), isxfe, f(1))
            call nmceni(numedd, depdel, deppr1, deppr2, rho,&
                        sdpilo, eta(2), isxfe, f(2))
        endif
        goto 5000
    endif
!
! --- SELECTION SELON LA METHODE CHOISIE: NORM_INCR_DEPL OU MIXTE
!
    if (typsel .eq. 'NORM_INCR_DEPL' .or. mixte) then
        call nmceni(numedd, depdel, deppr1, deppr2, rho,&
                    sdpilo, eta(1), isxfe, f(1))
        call nmceni(numedd, depdel, deppr1, deppr2, rho,&
                    sdpilo, eta(2), isxfe, f(2))
!
! ----- SI STRATEGIE MIXTE : EXAMEN DU CONTRASTE
!
        if (mixte) then
            miincr = min(f(1),f(2))/max(f(1),f(2))
            if (miincr .le. contra) goto 6000
!
! ------- ECHEC DU CONTRASTE: ON ENCHAINE PAR LA SELECTION RESIDU
!
            fnid(1) = f(1)
            fnid(2) = f(2)
        else
            goto 5000
        endif
    endif
!
! --- SELECTION SELON LA METHODE CHOISIE: RESIDU OU MIXTE
!
    if (typsel .eq. 'RESIDU' .or. mixte) then
        call nmcere(modele, numedd, mate, carele, comref,&
                    compor, lischa, carcri, fonact, sdstat,&
                    defico, iterat, sdnume, valinc, solalg,&
                    veelem, veasse, sdtime, offset, rho,&
                    eta(1), f(1), ldccv(1), matass)
        call nmcere(modele, numedd, mate, carele, comref,&
                    compor, lischa, carcri, fonact, sdstat,&
                    defico, iterat, sdnume, valinc, solalg,&
                    veelem, veasse, sdtime, offset, rho,&
                    eta(2), f(2), ldccv(2), matass)
!
! ----- SI STRATEGIE MIXTE : EXAMEN DU CONTRASTE
!
        if (mixte) then
            if (ldccv(1) .eq. 0 .and. ldccv(2) .eq. 0) then
                miresi = min(f(1),f(2))/max(f(1),f(2))
                if (miresi .le. contra) goto 6000
            endif
        else
            goto 5000
        endif
    endif
!
! --- STRATEGIE MIXTE: LES DEUX CONTRASTES SONT INSUFFISANTS
! --- ON REVIENT SUR NORM_INCR_DEPL ET ON TESTE LES CYCLES
!
    if (mixte) then
        f(1) = fnid(1)
        f(2) = fnid(2)
        ldccv(1) = 0
        ldccv(2) = 0
        switch = nmrcyc(sddisc,iterat,precyc)
        goto 6000
    endif
!
5000 continue
!
! --- PERMUTATION PAR EVENT DRIVEN (HORS STRATEGIE 'MIXTE')
!
    call utdidt('L', sddisc, 'ECHE', ib, 'CHOIX_SOLU_PILO',&
                r8bid, ibid, choix)
    ASSERT(choix.eq.'NATUREL'.or.choix.eq.'AUTRE')
    if (choix .eq. 'AUTRE' .or. swloun) then
        switch = .true.
        txt = 'NATUREL'
        if (choix .eq. 'AUTRE') then
            call utdidt('E', sddisc, 'ECHE', ib, 'CHOIX_SOLU_PILO',&
                        r8bid, ibid, txt)
        endif
    endif
!
6000 continue
!
! --- RETOUR DE LA SELECTION AVEC EVENTUELLEMENT INTERVERSION
!
    sel = 2
    if ((f(1).le.f(2) .and. .not.switch) .or. (f(1).gt.f(2) .and. switch)) sel=1
    etaf = eta(sel)
    pilcvg = licite(sel)
    ldccvg = ldccv(sel)
    criter = f(sel)
!
    call jedema()
end subroutine
