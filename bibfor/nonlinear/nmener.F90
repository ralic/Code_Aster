subroutine nmener(valinc, veasse, measse, sddyna, eta,&
                  sdener, fonact, solveu, numedd, numfix,&
                  meelem, numins, modele, mate, carele,&
                  compor, carcri, sdtime, sddisc, solalg,&
                  lischa, comref, resoco, resocu, parcon,&
                  veelem)
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
! person_in_charge: ludovic.idoux at edf.fr
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/enerca.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchai.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmfini.h"
#include "asterfort/nmmass.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddyna, sdener, valinc(*), veasse(*), measse(*)
    character(len=19) :: solveu, meelem(*), sddisc, solalg(*), lischa, veelem(*)
    character(len=24) :: numedd, numfix, modele, mate, carele, compor, carcri
    character(len=24) :: sdtime, comref, resoco, resocu
    real(kind=8) :: eta, parcon(*)
    integer :: fonact(*), numins
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! CALCUL DES ENERGIES
!
! ----------------------------------------------------------------------
!
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  SDDYNA : SD DYNAMIQUE
! IN  ETA    : COEFFICIENT DU PILOTAGE
! IN  SDENER : SD ENERGIE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  SOLVEU : SOLVEUR
! IN  NUMEDD : NUME_DDL
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MEELEM : MATRICES ELEMENTAIRES
! IN  NUMINS : NUMERO D'INSTANT
! IN  MODELE : MODELE
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMPOR : COMPORTEMENT
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDTIME : SD TIMER
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  LISCHA : LISTE DES CHARGES
! IN  COMREF : VARI_COM DE REFERENCE
! IN  DEFICO : SD DEF. CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  PARCON : PARAMETRES DU CRITERE DE CONVERGENCE REFERENCE
! IN  VEELEM : VECTEURS ELEMENTAIRES
!
! ----------------------------------------------------------------------
!
    integer :: zveass
    parameter    (zveass=32)
    integer :: iret(zveass)
!
    character(len=19) :: depmoi, depplu, vitmoi, vitplu, masse, amort, rigid
    character(len=19) :: fexmoi, fexplu, fammoi, fnomoi
    character(len=19) :: famplu, flimoi, fliplu, fnoplu
    character(len=19) :: lisbid
    character(len=8) :: k8bid
    character(len=6) :: tychap_out
    integer :: ivitmo, ivitpl
    integer :: neq, i, j, long
    integer :: ifexte, ifamor, ifliai, ifcine, ifnoda
    aster_logical :: ldyna, lamor, lexpl, reassm
    real(kind=8), pointer :: epmo(:) => null()
    real(kind=8), pointer :: eppl(:) => null()
    real(kind=8), pointer :: fammo(:) => null()
    real(kind=8), pointer :: fampl(:) => null()
    real(kind=8), pointer :: fexmo(:) => null()
    real(kind=8), pointer :: fexpl(:) => null()
    real(kind=8), pointer :: flimo(:) => null()
    real(kind=8), pointer :: flipl(:) => null()
    real(kind=8), pointer :: fnomo(:) => null()
    real(kind=8), pointer :: fnopl(:) => null()
    real(kind=8), pointer :: veass(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call nmchai('VEASSE', 'CNFINT', 1, tychap_out)
    call nmchai('VEASSE', 'CNDIRI', 2, tychap_out)
    call nmchai('VEASSE', 'CNBUDI', 3, tychap_out)
    call nmchai('VEASSE', 'CNFNOD', 4, tychap_out)
    call nmchai('VEASSE', 'CNDIDO', 5, tychap_out)
    call nmchai('VEASSE', 'CNDIPI', 6, tychap_out)
    call nmchai('VEASSE', 'CNFEDO', 7, tychap_out)
    call nmchai('VEASSE', 'CNFEPI', 8, tychap_out)
    call nmchai('VEASSE', 'CNLAPL', 9, tychap_out)
    call nmchai('VEASSE', 'CNONDP', 10, tychap_out)
    call nmchai('VEASSE', 'CNFSDO', 11, tychap_out)
    call nmchai('VEASSE', 'CNIMPP', 12, tychap_out)
    call nmchai('VEASSE', '      ', 13, tychap_out)
    call nmchai('VEASSE', 'CNDIDI', 14, tychap_out)
    call nmchai('VEASSE', 'CNSSTF', 15, tychap_out)
    call nmchai('VEASSE', 'CNELTC', 16, tychap_out)
    call nmchai('VEASSE', 'CNELTF', 17, tychap_out)
    call nmchai('VEASSE', 'CNREFE', 18, tychap_out)
    call nmchai('VEASSE', 'CNVCF1', 19, tychap_out)
    call nmchai('VEASSE', 'CNVCF0', 20, tychap_out)
    call nmchai('VEASSE', 'CNCINE', 21, tychap_out)
    call nmchai('VEASSE', 'CNSSTR', 22, tychap_out)
    call nmchai('VEASSE', 'CNCTDF', 23, tychap_out)
    call nmchai('VEASSE', 'CNVCPR', 24, tychap_out)
    call nmchai('VEASSE', 'CNDYNA', 25, tychap_out)
    call nmchai('VEASSE', 'CNMODP', 26, tychap_out)
    call nmchai('VEASSE', 'CNMODC', 27, tychap_out)
    call nmchai('VEASSE', 'CNCTDC', 28, tychap_out)
    call nmchai('VEASSE', 'CNUNIL', 29, tychap_out)
    call nmchai('VEASSE', 'CNFEXT', 30, tychap_out)
    call nmchai('VEASSE', 'CNIMPC', 31, tychap_out)
    call nmchai('VEASSE', 'CNVISS', 32, tychap_out)
    call nmchai('VEASSE', 'LONMAX', long)
    ASSERT(long.eq.zveass)
!
    k8bid=' '
    reassm=.false.
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call jeveuo(depmoi//'.VALE', 'L', vr=epmo)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call jeveuo(depplu//'.VALE', 'L', vr=eppl)
    call jelira(depmoi//'.VALE', 'LONMAX', ival=neq)
    ldyna=ndynlo(sddyna,'DYNAMIQUE')
    lamor=ndynlo(sddyna,'MAT_AMORT')
    lexpl=ndynlo(sddyna,'EXPLICITE')
    if (ldyna) then
        call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
        call jeveuo(vitmoi//'.VALE', 'L', ivitmo)
        call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
        call jeveuo(vitplu//'.VALE', 'L', ivitpl)
    else
        ivitmo=1
        ivitpl=1
    endif
    call nmchex(valinc, 'VALINC', 'FEXMOI', fexmoi)
    call nmchex(valinc, 'VALINC', 'FEXPLU', fexplu)
    call nmchex(valinc, 'VALINC', 'FAMMOI', fammoi)
    call nmchex(valinc, 'VALINC', 'FNOMOI', fnomoi)
    call nmchex(valinc, 'VALINC', 'FAMPLU', famplu)
    call nmchex(valinc, 'VALINC', 'FLIMOI', flimoi)
    call nmchex(valinc, 'VALINC', 'FLIPLU', fliplu)
    call nmchex(valinc, 'VALINC', 'FNOPLU', fnoplu)
    call nmchex(measse, 'MEASSE', 'MERIGI', rigid)
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
    call nmchex(measse, 'MEASSE', 'MEAMOR', amort)
!
!
    do i = 1, zveass
        iret(i)=0
        call jeexin(veasse(i)//'.VALE', iret(i))
    end do
!
    call jeveuo(fexmoi//'.VALE', 'L', vr=fexmo)
    call jeveuo(fammoi//'.VALE', 'L', vr=fammo)
    call jeveuo(flimoi//'.VALE', 'L', vr=flimo)
    call jeveuo(fnomoi//'.VALE', 'L', vr=fnomo)
    call jeveuo(fexplu//'.VALE', 'E', vr=fexpl)
    call jeveuo(famplu//'.VALE', 'E', vr=fampl)
    call jeveuo(fliplu//'.VALE', 'E', vr=flipl)
    call jeveuo(fnoplu//'.VALE', 'E', vr=fnopl)
!
    do i = 1, neq
        fexpl(i)=0.d0
        fampl(i)=0.d0
        flipl(i)=0.d0
        fnopl(i)=0.d0
    end do
!
    call wkvect('FEXTE', 'V V R', 2*neq, ifexte)
    call wkvect('FAMOR', 'V V R', 2*neq, ifamor)
    call wkvect('FLIAI', 'V V R', 2*neq, ifliai)
    call wkvect('FNODA', 'V V R', 2*neq, ifnoda)
    call wkvect('FCINE', 'V V R', neq, ifcine)
!
! RECUPERATION DES DIFFERENTES CONTRIBUTIONS AUX VECTEURS DE FORCE
!
    do i = 1, zveass
        if (iret(i) .ne. 0) then
            call jeveuo(veasse(i)//'.VALE', 'L', vr=veass)
! --------------------------------------------------------------------
! 7  - CNFEDO : CHARGES MECANIQUES FIXES DONNEES
! 9  - CNLAPL : FORCES DE LAPLACE
! 11 - CNFSDO : FORCES SUIVEUSES
! 15 - CNSSTF : FORCES ISSUES DU CALCUL PAR SOUS-STRUCTURATION
! --------------------------------------------------------------------
            if ((i.eq.7 ) .or. (i.eq.9 ) .or. (i.eq.11) .or. (i.eq.15)) then
                do j = 1, neq
                    fexpl(j)=fexpl(j)+veass(j)
                end do
! --------------------------------------------------------------------
! 20 - CNVCF0 : FORCE DE REFERENCE LIEE AUX VAR. COMMANDES EN T+
! --------------------------------------------------------------------
            else if (i.eq.20) then
                do j = 1, neq
                    fexpl(j)=fexpl(j)+veass(j)
                end do
! ON AJOUTE LES CONTRAINTES ISSUES DES VARIABLES DE COMMANDE AUX
! FORCES INTERNES EGALEMENT
                do j = 1, neq
                    fnopl(j)=fnopl(j)+veass(j)
                end do
! --------------------------------------------------------------------
! 8  - CNFEPI : FORCES PILOTEES PARAMETRE ETA A PRENDRE EN COMPTE
! --------------------------------------------------------------------
            else if (i.eq.8) then
                do j = 1, neq
                    fexpl(j)=fexpl(j)+eta*veass(j)
                end do
! --------------------------------------------------------------------
! 2  - CNDIRI : BtLAMBDA                : IL FAUT PRENDRE L OPPOSE
! 10 - CNONDP : CHARGEMENT ONDES PLANES : IL FAUT PRENDRE L OPPOSE
! --------------------------------------------------------------------
            else if ((i.eq.2).or.(i.eq.10)) then
                do j = 1, neq
                    fexpl(j)=fexpl(j)-veass(j)
                end do
! --------------------------------------------------------------------
! 27 - CNMODC : FORCE D AMORTISSEMENT MODAL
! --------------------------------------------------------------------
            else if (i.eq.27) then
                do j = 1, neq
                    fampl(j)=fampl(j)+veass(j)
                end do
! --------------------------------------------------------------------
! 16 - CNELTC : FORCES ELEMENTS DE CONTACT (CONTINU + XFEM)
! 17 - CNELTF : FORCES ELEMENTS DE FROTTEMENT (CONTINU + XFEM)
! 31 - CNIMPC : FORCES IMPEDANCE
! 23 - CNCTDF : FORCES DE FROTTEMENT (CONTACT DISCRET)
! 28 - CNCTDC : FORCES DE CONTACT (CONTACT DISCRET)
! 29 - CNUNIL : FORCES DE CONTACT (LIAISON_UNILATERALE)
! --------------------------------------------------------------------
                else if ((i.eq.16).or.(i.eq.17).or.(i.eq.31).or. (i.eq.23)&
            .or.(i.eq.28).or.(i.eq.29)) then
                do j = 1, neq
                    flipl(j)=flipl(j)+veass(j)
                end do
                if ((i.eq.16) .or. (i.eq.17)) then
! ON ENLEVE LA CONTRIBUTION DU CONTACT (CONTINU + XFEM) DANS
! LES FORCES INTERNES (VOIR ROUTINE NMAINT)
                    do j = 1, neq
                        fnopl(j)=fnopl(j)-veass(j)
                    end do
                endif
! CNDIRI CONTIENT BTLAMBDA PLUS CONTRIBUTION CNCTDF DU CONTACT.
! ON SOUHAITE AJOUTER -BT.LAMBDA A FEXTE. ON AJOUTE DONC -CNDIRI,
! MAIS IL FAUT ALORS LUI RETRANCHER -CNCTDF.
                if (i .eq. 23) then
                    do j = 1, neq
                        fexpl(j)=fexpl(j)+veass(j)
                    end do
                endif
! --------------------------------------------------------------------
! 33 - CNVISS : CHARGEMENT VEC_ISS (FORCE_SOL)
! --------------------------------------------------------------------
            else if (i.eq.32) then
! CHARGEMENT FORCE_SOL CNVISS. SI ON COMPTE SA CONTRIBUTION EN TANT
! QUE FORCE DISSIPATIVE DE LIAISON, ON DOIT PRENDRE L OPPOSE.
                do j = 1, neq
                    flipl(j)=flipl(j)-veass(j)
                end do
! --------------------------------------------------------------------
!  1  - CNFINT : FORCES INTERNES
! --------------------------------------------------------------------
            else if (i.eq.1) then
! CONTIENT UNE CONTRIBUTION DU CONTACT QU ON ENLEVE PAR AILLEURS.
! CONTIENT LA CONTRIBUTION DES MACRO ELEMENTS.
                do j = 1, neq
                    fnopl(j)=fnopl(j)+veass(j)
                end do
! --------------------------------------------------------------------
! 21 - CNCINE : INCREMENTS DE DEPLACEMENT IMPOSES (AFFE_CHAR_CINE)
! --------------------------------------------------------------------
            else if (i.eq.21) then
! ON DOIT RECONSTRUIRE LA MATRICE DE MASSE CAR ELLE A ETE MODIFIEE
! POUR SUPPRIMER DES DEGRES DE LIBERTE EN RAISON DE AFFE_CHAR_CINE.
                reassm=.true.
                do j = 1, neq
                    zr(ifcine-1+j)=zr(ifcine-1+j)+veass(j)
                end do
            endif
        endif
    end do
!
    if (reassm) then
! --- REASSEMBLAGE DE LA MATRICE DE MASSE.
        lisbid=' '
        call nmmass(fonact, lisbid, sddyna, solveu, numedd,&
                    numfix, meelem, masse)
    endif
!
! --- INITIALISATION DE LA FORCE EXTERIEURE ET DES FORCES INTERNES
! --- AU PREMIER PAS DE TEMPS.
! --- ON LE FAIT ICI AFIN DE DISPOSER D UNE MATRICE D AMORTISSEMENT.
!
    if (numins .eq. 1) then
        call nmfini(sddyna, valinc, measse, modele, mate,&
                    carele, compor, carcri, sdtime, sddisc,&
                    numins, solalg, lischa, comref, resoco,&
                    resocu, numedd, parcon, veelem, veasse)
    endif
!
! --- PREPARATION DES CHAMPS DE FORCE
!
    do i = 1, neq
        zr(ifexte-1+i+neq)=fexpl(i)
        zr(ifexte-1+i)=fexmo(i)
        zr(ifliai-1+i+neq)=flipl(i)
        zr(ifliai-1+i)=flimo(i)
        zr(ifamor-1+i+neq)=fampl(i)
        zr(ifamor-1+i)=fammo(i)
        zr(ifnoda-1+i+neq)=fnopl(i)
        zr(ifnoda-1+i)=fnomo(i)
    end do
!
    call enerca(valinc, epmo, zr(ivitmo), eppl, zr(ivitpl),&
                masse, amort, rigid, zr(ifexte), zr(ifamor),&
                zr(ifliai), zr(ifnoda), zr(ifcine), lamor, ldyna,&
                lexpl, sdener, k8bid)
!
!     ON NE PEUT PAS UTILISER NMFPAS POUR METTRE LES CHAMPS PLUS
!     EN CHAMP MOINS, SINON CA POSE PROBLEME EN LECTURE D'ETAT INITIAL
!     SI POURSUITE.
!     ON FAIT DONC LE REPORT DE CHAMP ICI
!
    call copisd('CHAMP_GD', 'V', fexplu, fexmoi)
    call copisd('CHAMP_GD', 'V', famplu, fammoi)
    call copisd('CHAMP_GD', 'V', fliplu, flimoi)
    call copisd('CHAMP_GD', 'V', fnoplu, fnomoi)
!
    call jedetr('FEXTE')
    call jedetr('FAMOR')
    call jedetr('FLIAI')
    call jedetr('FNODA')
    call jedetr('FCINE')
!
    call jedema()
!
end subroutine
