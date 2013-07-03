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
    integer :: nveass, long
    parameter    (nveass=32)
    character(len=19) :: depmoi, depplu, vitmoi, vitplu, masse, amort, rigid
    character(len=19) :: fexmoi, fexplu, fammoi, fnomoi
    character(len=19) :: famplu, flimoi, fliplu, fnoplu
    character(len=19) :: lisbid
    character(len=24) :: k8b
    character(len=8) :: k8bid
    integer :: idepmo, ideppl, ivitmo, ivitpl
    integer :: neq, i, j, iveass
    integer :: ifexmo, ifammo, iflimo, ifnomo
    integer :: ifexpl, ifampl, iflipl, ifnopl
    integer :: ifexte, ifamor, ifliai, ifcine, iret(nveass), ifnoda
    logical :: ldyna, lamor, lexpl, reassm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    k8bid=' '
    reassm=.false.
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call jeveuo(depmoi//'.VALE', 'L', idepmo)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call jeveuo(depplu//'.VALE', 'L', ideppl)
    call jelira(depmoi//'.VALE', 'LONMAX', neq, k8b)
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
    call nmchai('VEASSE', 'LONMAX', long)
    call assert(long.eq.nveass)
    do 10 i = 1, nveass
        iret(i)=0
        call jeexin(veasse(i)//'.VALE', iret(i))
10  end do
!
    call jeveuo(fexmoi//'.VALE', 'L', ifexmo)
    call jeveuo(fammoi//'.VALE', 'L', ifammo)
    call jeveuo(flimoi//'.VALE', 'L', iflimo)
    call jeveuo(fnomoi//'.VALE', 'L', ifnomo)
    call jeveuo(fexplu//'.VALE', 'E', ifexpl)
    call jeveuo(famplu//'.VALE', 'E', ifampl)
    call jeveuo(fliplu//'.VALE', 'E', iflipl)
    call jeveuo(fnoplu//'.VALE', 'E', ifnopl)
!
    do 20 i = 1, neq
        zr(ifexpl-1+i)=0.d0
        zr(ifampl-1+i)=0.d0
        zr(iflipl-1+i)=0.d0
        zr(ifnopl-1+i)=0.d0
20  end do
!
    call wkvect('FEXTE', 'V V R', 2*neq, ifexte)
    call wkvect('FAMOR', 'V V R', 2*neq, ifamor)
    call wkvect('FLIAI', 'V V R', 2*neq, ifliai)
    call wkvect('FNODA', 'V V R', 2*neq, ifnoda)
    call wkvect('FCINE', 'V V R', neq, ifcine)
!
! RECUPERATION DES DIFFERENTES CONTRIBUTIONS AUX VECTEURS DE FORCE
!
    do 30 i = 1, nveass
        if (iret(i) .ne. 0) then
            call jeveuo(veasse(i)//'.VALE', 'L', iveass)
! --------------------------------------------------------------------
! 7  - CNFEDO : CHARGES MECANIQUES FIXES DONNEES
! 9  - CNLAPL : FORCES DE LAPLACE
! 11 - CNFSDO : FORCES SUIVEUSES
! 15 - CNSSTF : FORCES ISSUES DU CALCUL PAR SOUS-STRUCTURATION
! --------------------------------------------------------------------
            if ((i.eq.7 ) .or. (i.eq.9 ) .or. (i.eq.11) .or. (i.eq.15)) then
                do 40 j = 1, neq
                    zr(ifexpl-1+j)=zr(ifexpl-1+j)+zr(iveass-1+j)
40              continue
! --------------------------------------------------------------------
! 20 - CNVCF0 : FORCE DE REFERENCE LIEE AUX VAR. COMMANDES EN T+
! --------------------------------------------------------------------
            else if (i.eq.20) then
                do 50 j = 1, neq
                    zr(ifexpl-1+j)=zr(ifexpl-1+j)+zr(iveass-1+j)
50              continue
! ON AJOUTE LES CONTRAINTES ISSUES DES VARIABLES DE COMMANDE AUX
! FORCES INTERNES EGALEMENT
                do 51 j = 1, neq
                    zr(ifnopl-1+j)=zr(ifnopl-1+j)+zr(iveass-1+j)
51              continue
! --------------------------------------------------------------------
! 8  - CNFEPI : FORCES PILOTEES PARAMETRE ETA A PRENDRE EN COMPTE
! --------------------------------------------------------------------
            else if (i.eq.8) then
                do 60 j = 1, neq
                    zr(ifexpl-1+j)=zr(ifexpl-1+j)+eta*zr(iveass-1+j)
60              continue
! --------------------------------------------------------------------
! 2  - CNDIRI : BtLAMBDA                : IL FAUT PRENDRE L OPPOSE
! 10 - CNONDP : CHARGEMENT ONDES PLANES : IL FAUT PRENDRE L OPPOSE
! --------------------------------------------------------------------
            else if ((i.eq.2).or.(i.eq.10)) then
                do 70 j = 1, neq
                    zr(ifexpl-1+j)=zr(ifexpl-1+j)-zr(iveass-1+j)
70              continue
! --------------------------------------------------------------------
! 27 - CNMODC : FORCE D AMORTISSEMENT MODAL
! --------------------------------------------------------------------
            else if (i.eq.27) then
                do 80 j = 1, neq
                    zr(ifampl-1+j)=zr(ifampl-1+j)+zr(iveass-1+j)
80              continue
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
                do 90 j = 1, neq
                    zr(iflipl-1+j)=zr(iflipl-1+j)+zr(iveass-1+j)
90              continue
                if ((i.eq.16) .or. (i.eq.17)) then
! ON ENLEVE LA CONTRIBUTION DU CONTACT (CONTINU + XFEM) DANS
! LES FORCES INTERNES (VOIR ROUTINE NMAINT)
                    do 91 j = 1, neq
                        zr(ifnopl-1+j)=zr(ifnopl-1+j)-zr(iveass-1+j)
91                  continue
                endif
! CNDIRI CONTIENT BTLAMBDA PLUS CONTRIBUTION CNCTDF DU CONTACT.
! ON SOUHAITE AJOUTER -BT.LAMBDA A FEXTE. ON AJOUTE DONC -CNDIRI,
! MAIS IL FAUT ALORS LUI RETRANCHER -CNCTDF.
                if (i .eq. 23) then
                    do 92 j = 1, neq
                        zr(ifexpl-1+j)=zr(ifexpl-1+j)+zr(iveass-1+j)
92                  continue
                endif
! --------------------------------------------------------------------
! 33 - CNVISS : CHARGEMENT VEC_ISS (FORCE_SOL)
! --------------------------------------------------------------------
            else if (i.eq.33) then
! CHARGEMENT FORCE_SOL CNVISS. SI ON COMPTE SA CONTRIBUTION EN TANT
! QUE FORCE DISSIPATIVE DE LIAISON, ON DOIT PRENDRE L OPPOSE.
                do 100 j = 1, neq
                    zr(iflipl-1+j)=zr(iflipl-1+j)-zr(iveass-1+j)
100              continue
! --------------------------------------------------------------------
!  1  - CNFINT : FORCES INTERNES
! --------------------------------------------------------------------
            else if (i.eq.1) then
! CONTIENT UNE CONTRIBUTION DU CONTACT QU ON ENLEVE PAR AILLEURS.
! CONTIENT LA CONTRIBUTION DES MACRO ELEMENTS.
                do 110 j = 1, neq
                    zr(ifnopl-1+j)=zr(ifnopl-1+j)+zr(iveass-1+j)
110              continue
! --------------------------------------------------------------------
! 21 - CNCINE : INCREMENTS DE DEPLACEMENT IMPOSES (AFFE_CHAR_CINE)
! --------------------------------------------------------------------
            else if (i.eq.21) then
! ON DOIT RECONSTRUIRE LA MATRICE DE MASSE CAR ELLE A ETE MODIFIEE
! POUR SUPPRIMER DES DEGRES DE LIBERTE EN RAISON DE AFFE_CHAR_CINE.
                reassm=.true.
                do 120 j = 1, neq
                    zr(ifcine-1+j)=zr(ifcine-1+j)+zr(iveass-1+j)
120              continue
            endif
        endif
30  end do
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
    do 130 i = 1, neq
        zr(ifexte-1+i+neq)=zr(ifexpl-1+i)
        zr(ifexte-1+i)=zr(ifexmo-1+i)
        zr(ifliai-1+i+neq)=zr(iflipl-1+i)
        zr(ifliai-1+i)=zr(iflimo-1+i)
        zr(ifamor-1+i+neq)=zr(ifampl-1+i)
        zr(ifamor-1+i)=zr(ifammo-1+i)
        zr(ifnoda-1+i+neq)=zr(ifnopl-1+i)
        zr(ifnoda-1+i)=zr(ifnomo-1+i)
130  end do
!
    call enerca(valinc, zr(idepmo), zr(ivitmo), zr(ideppl), zr(ivitpl),&
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
