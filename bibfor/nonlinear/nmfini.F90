subroutine nmfini(sddyna, valinc, measse, modele, mate,&
                  carele, compor, carcri, sdtime, sddisc,&
                  numins, solalg, lischa, comref, resoco,&
                  resocu, numedd, parcon, veelem, veasse)
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
! person_in_charge: ludovic.idoux at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcvec.h"
#include "asterfort/nmxvec.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddyna, valinc(*), measse(*)
    character(len=24) :: modele, mate, carele, compor, carcri, sdtime, comref
    character(len=24) :: resoco, resocu, numedd
    character(len=19) :: sddisc, solalg(*), lischa, veelem(*), veasse(*)
    integer :: numins
    real(kind=8) :: parcon(8)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! CALCUL DES ENERGIES
! INITIALISATION DES VECTEURS DE FORCE POUR LE CALCUL DES ENERGIES
!
! ----------------------------------------------------------------------
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  MODELE : MODELE
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMPOR : COMPORTEMENT
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDTIME : SD TIMER
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  LISCHA : LISTE DES CHARGES
! IN  COMREF : VARI_COM DE REFERENCE
! IN  DEFICO : SD DEF. CONTACT
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  NUMEDD : NUME_DDL
! IN  PARCON : PARAMETRES DU CRITERE DE CONVERGENCE REFERENCE
!                     1 : SIGM_REFE
!                     2 : EPSI_REFE
!                     3 : FLUX_THER_REFE
!                     4 : FLUX_HYD1_REFE
!                     5 : FLUX_HYD2_REFE
!                     6 : VARI_REFE
!                     7 : EFFORT (FORC_REFE)
!                     8 : MOMENT (FORC_REFE)
! IN  VEELEM : VECTEURS ELEMENTAIRES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
!
!
!
    character(len=19) :: masse, amort, vitmoi, accmoi
    character(len=19) :: fexmoi, fammoi, flimoi
    integer :: imasse, iamort, ivitmo, iaccmo
    integer :: ifexmo, ifammo, iflimo
    integer :: neq, iaux, icv, ima
    logical :: lamor, ldyna
    character(len=8) :: k8bid
    integer :: nbvect, icnfno, ifnomo, iret
    character(len=16) :: loptve(20)
    character(len=6) :: ltypve(20)
    logical :: lassve(20), lcalve(20)
    character(len=19) :: cnfnod, fnomoi
!
    call jemarq()
!
    lamor = ndynlo(sddyna,'MAT_AMORT')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    call nmchex(valinc, 'VALINC', 'FEXMOI', fexmoi)
    call jeveuo(fexmoi//'.VALE', 'E', ifexmo)
    call dismoi('F', 'NB_EQUA', numedd, 'NUME_DDL', neq,&
                k8bid, iret)
!
! --- AJOUT DE LA FORCE DE LIAISON ET DE LA FORCE D AMORTISSEMENT
! --- MODAL
!
    call nmchex(valinc, 'VALINC', 'FAMMOI', fammoi)
    call jeveuo(fammoi//'.VALE', 'L', ifammo)
    call nmchex(valinc, 'VALINC', 'FLIMOI', flimoi)
    call jeveuo(flimoi//'.VALE', 'L', iflimo)
    do 10 iaux = 1, neq
        zr(ifexmo-1+iaux)=zr(ifammo-1+iaux)+zr(iflimo-1+iaux)
10  end do
!
! --- AJOUT DU TERME C.V
!
    if (lamor) then
        call nmchex(measse, 'MEASSE', 'MEAMOR', amort)
        call mtdscr(amort)
        call jeveuo(amort//'.&INT', 'L', iamort)
        call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
        call jeveuo(vitmoi//'.VALE', 'L', ivitmo)
        call wkvect('&&NMFINI.CV', 'V V R', neq, icv)
        call mrmult('ZERO', iamort, zr(ivitmo), zr(icv), 1,&
                    .true.)
        do 20 iaux = 1, neq
            zr(ifexmo-1+iaux) = zr(ifexmo-1+iaux) + zr(icv-1+iaux)
20      continue
        call jedetr('&&NMFINI.CV')
    endif
!
! --- AJOUT DU TERME M.A
!
    if (ldyna) then
        call nmchex(measse, 'MEASSE', 'MEMASS', masse)
        call mtdscr(masse)
        call jeveuo(masse//'.&INT', 'L', imasse)
        call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
        call jeveuo(accmoi//'.VALE', 'L', iaccmo)
        call wkvect('&&NMFINI.MA', 'V V R', neq, ima)
        call mrmult('ZERO', imasse, zr(iaccmo), zr(ima), 1,&
                    .true.)
        do 30 iaux = 1, neq
            zr(ifexmo-1+iaux) = zr(ifexmo-1+iaux) + zr(ima-1+iaux)
30      continue
        call jedetr('&&NMFINI.MA')
    endif
!
! --- AJOUT DU TERME CNFNOD
!
    nbvect=0
    call nmcvec('AJOU', 'CNFNOD', 'SIGMOI', .true., .true.,&
                nbvect, ltypve, loptve, lcalve, lassve)
    call nmxvec(modele, mate, carele, compor, carcri,&
                sdtime, sddisc, sddyna, numins, valinc,&
                solalg, lischa, comref, resoco, resocu,&
                numedd, parcon, veelem, veasse, measse,&
                nbvect, ltypve, lcalve, loptve, lassve)
    call nmchex(veasse, 'VEASSE', 'CNFNOD', cnfnod)
    call jeveuo(cnfnod//'.VALE', 'L', icnfno)
    do 40 iaux = 1, neq
        zr(ifexmo-1+iaux) = zr(ifexmo-1+iaux) + zr(icnfno-1+iaux)
40  end do
!
! --- INITIALISATION DES FORCES INTERNES
!
    call nmchex(valinc, 'VALINC', 'FNOMOI', fnomoi)
    call jeveuo(fnomoi//'.VALE', 'E', ifnomo)
    do 50 iaux = 1, neq
        zr(ifnomo-1+iaux) = zr(icnfno-1+iaux)
50  end do
!
    call jedema()
!
end subroutine
