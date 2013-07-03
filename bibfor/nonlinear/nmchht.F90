subroutine nmchht(modele, numedd, mate, compor, carele,&
                  lischa, carcri, comref, fonact, sdstat,&
                  sddyna, sdtime, defico, resoco, resocu,&
                  valinc, sddisc, parcon, solalg, veasse,&
                  sdnume)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterc/getres.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecact.h"
#include "asterfort/ndynkk.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmaint.h"
#include "asterfort/nmassv.h"
#include "asterfort/nmcalv.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmfint.h"
#include "asterfort/nmvcaf.h"
#include "asterfort/nmvcex.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rsorac.h"
    integer :: fonact(*)
    character(len=19) :: sddyna, sdnume
    character(len=19) :: lischa
    character(len=24) :: modele, mate, carele, numedd
    character(len=24) :: compor, carcri, comref
    character(len=24) :: sdtime, sdstat
    character(len=19) :: sddisc
    real(kind=8) :: parcon(*)
    character(len=24) :: defico, resoco, resocu
    character(len=19) :: solalg(*), veasse(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL CHARGEMENT INITIAL POUR SCHEMAS MULTIPAS EN POURSUITE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
! IN  RESOCU : SD POUR LA RESOLUTION LIAISON_UNILATER
!
!
!
!
    logical :: londe, llapl, ldidi, lreuse
    character(len=8) :: result, k8bid
    character(len=16) :: k16bla, k16bid
    character(len=19) :: matass
    character(len=19) :: vefint, vedido
    character(len=19) :: vefedo, veondp, vedidi, velapl
    character(len=19) :: cnfedo, cndidi, cnfint
    character(len=19) :: cndido, cncine
    character(len=19) :: cnondp, cnlapl
    character(len=24) :: codere
    character(len=19) :: commoi, complu, insmoi, insplu
    complex(kind=8) :: c16bid
    real(kind=8) :: instap, instam, r8bid
    integer :: iterat, ldccvg, numder
    integer :: ifm, niv, ibid, jinst
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL CHARGEMENT'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    londe = ndynlo(sddyna,'ONDE_PLANE')
    llapl = isfonc(fonact,'LAPLACE')
    ldidi = isfonc(fonact,'DIDI')
    lreuse = isfonc(fonact,'REUSE')
    k16bla = ' '
!
! --- INSTANTS
!
    instam = 0.d0
    instam = ndynre(sddyna,'INST_PREC')
!
    if (lreuse) then
        call getres(result, k16bid, k16bid)
        call rsorac(result, 'DERNIER', ibid, r8bid, k8bid,&
                    c16bid, 0.d0, 'ABSOLU', numder, 1,&
                    ibid)
        call rsadpa(result, 'L', 1, 'INST_PREC', numder,&
                    0, jinst, k8bid)
        instam = zr(jinst)
    endif
    instap = diinst(sddisc,0)
    iterat = 0
    codere = '&&NMCHHT.CODERE'
!
! --- CREATION PSEUDO CARTE INSTANT PLUS
!
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmvcex('INST', commoi, insmoi)
    call nmvcaf('INST', insmoi, .true., complu)
    call nmvcex('INST', complu, insplu)
    k8bid = ' '
    call mecact('V', insmoi, 'MODELE', modele(1:8)//'.MODELE', 'INST_R',&
                1, 'INST', ibid, instam, c16bid,&
                k8bid)
    call mecact('V', insplu, 'MODELE', modele(1:8)//'.MODELE', 'INST_R',&
                1, 'INST', ibid, instap, c16bid,&
                k8bid)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call ndynkk(sddyna, 'OLDP_VEFEDO', vefedo)
    call ndynkk(sddyna, 'OLDP_VEDIDO', vedido)
    call ndynkk(sddyna, 'OLDP_VEDIDI', vedidi)
    call ndynkk(sddyna, 'OLDP_VEFINT', vefint)
    call ndynkk(sddyna, 'OLDP_VEONDP', veondp)
    call ndynkk(sddyna, 'OLDP_VELAPL', velapl)
    call ndynkk(sddyna, 'OLDP_CNFEDO', cnfedo)
    call ndynkk(sddyna, 'OLDP_CNDIDO', cndido)
    call ndynkk(sddyna, 'OLDP_CNDIDI', cndidi)
    call ndynkk(sddyna, 'OLDP_CNFINT', cnfint)
    call ndynkk(sddyna, 'OLDP_CNONDP', cnondp)
    call ndynkk(sddyna, 'OLDP_CNLAPL', cnlapl)
    call ndynkk(sddyna, 'OLDP_CNCINE', cncine)
    matass = ' '
!
! --- CALCUL DES FORCES INTERIEURES
!
    call nmfint(modele, mate, carele, comref, compor,&
                carcri, fonact, iterat, sddyna, sdstat,&
                sdtime, valinc, solalg, ldccvg, codere,&
                vefint)
!
! --- ASSEMBLAGE DES FORCES INTERIEURES
!
    call nmaint(numedd, fonact, defico, veasse, vefint,&
                cnfint, sdnume)
!
! --- DEPLACEMENTS IMPOSES DONNES
!
    call nmcalv('CNDIDO', modele, lischa, mate, carele,&
                compor, carcri, numedd, comref, sdtime,&
                parcon, instam, instap, valinc, solalg,&
                sddyna, k16bla, vedido)
    call nmassv('CNDIDO', modele, lischa, mate, carele,&
                compor, numedd, instam, instap, resoco,&
                resocu, sddyna, sdtime, valinc, comref,&
                matass, vedido, cndido)
    if (ldidi) then
        call nmcalv('CNDIDI', modele, lischa, mate, carele,&
                    compor, carcri, numedd, comref, sdtime,&
                    parcon, instam, instap, valinc, solalg,&
                    sddyna, k16bla, vedidi)
        call nmassv('CNDIDI', modele, lischa, mate, carele,&
                    compor, numedd, instam, instap, resoco,&
                    resocu, sddyna, sdtime, valinc, comref,&
                    matass, vedidi, cndidi)
    endif
!
! --- CHARGEMENTS FORCES DE LAPLACE
!
    if (llapl) then
        call nmcalv('CNLAPL', modele, lischa, mate, carele,&
                    compor, carcri, numedd, comref, sdtime,&
                    parcon, instam, instap, valinc, solalg,&
                    sddyna, k16bla, velapl)
        call nmassv('CNLAPL', modele, lischa, mate, carele,&
                    compor, numedd, instam, instap, resoco,&
                    resocu, sddyna, sdtime, valinc, comref,&
                    matass, velapl, cnlapl)
    endif
!
! --- CHARGEMENTS ONDE_PLANE
!
    if (londe) then
        call nmcalv('CNONDP', modele, lischa, mate, carele,&
                    compor, carcri, numedd, comref, sdtime,&
                    parcon, instam, instap, valinc, solalg,&
                    sddyna, k16bla, veondp)
        call nmassv('CNONDP', modele, lischa, mate, carele,&
                    compor, numedd, instam, instap, resoco,&
                    resocu, sddyna, sdtime, valinc, comref,&
                    matass, veondp, cnondp)
    endif
!
! --- CHARGEMENTS MECANIQUES FIXES DONNES
!
    call nmcalv('CNFEDO', modele, lischa, mate, carele,&
                compor, carcri, numedd, comref, sdtime,&
                parcon, instam, instap, valinc, solalg,&
                sddyna, k16bla, vefedo)
    call nmassv('CNFEDO', modele, lischa, mate, carele,&
                compor, numedd, instam, instap, resoco,&
                resocu, sddyna, sdtime, valinc, comref,&
                matass, vefedo, cnfedo)
!
! --- CONDITIONS CINEMATIQUES IMPOSEES  (AFFE_CHAR_CINE)
!
    call nmassv('CNCINE', modele, lischa, mate, carele,&
                compor, numedd, instam, instap, resoco,&
                resocu, sddyna, sdtime, valinc, comref,&
                matass, ' ', cncine)
!
    call jedema()
end subroutine
