subroutine nmfini(sddyna  , valinc, measse, modele, mate  ,&
                  carele  , compor, ds_measure, sddisc, numins,&
                  solalg  , lischa, comref,&
                  ds_inout, numedd, veelem, veasse)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/mtdscr.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmcvec.h"
#include "asterfort/nmxvec.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
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
!
    character(len=19) :: sddyna, valinc(*), measse(*)
    character(len=24) :: modele, mate, carele, compor, comref
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: numedd
    type(NL_DS_InOut), intent(in) :: ds_inout
    character(len=19) :: sddisc, solalg(*), lischa, veelem(*), veasse(*)
    integer :: numins
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
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! In  ds_inout         : datastructure for input/output management
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  LISCHA : LISTE DES CHARGES
! IN  COMREF : VARI_COM DE REFERENCE
! IN  NUMEDD : NUME_DDL
! IN  VEELEM : VECTEURS ELEMENTAIRES
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
! ----------------------------------------------------------------------
!
    character(len=19) :: masse, amort, vitmoi, accmoi
    character(len=19) :: fexmoi, fammoi, flimoi
    integer :: imasse, iamort
    integer :: neq, iaux
    aster_logical :: lamor, ldyna
    integer :: nbvect
    character(len=16) :: loptve(20)
    character(len=6) :: ltypve(20)
    aster_logical :: lassve(20), lcalve(20)
    character(len=19) :: cnfnod, fnomoi
    real(kind=8), pointer :: cv(:) => null()
    real(kind=8), pointer :: ma(:) => null()
    real(kind=8), pointer :: ccmo(:) => null()
    real(kind=8), pointer :: cnfno(:) => null()
    real(kind=8), pointer :: fammo(:) => null()
    real(kind=8), pointer :: fexmo(:) => null()
    real(kind=8), pointer :: flimo(:) => null()
    real(kind=8), pointer :: fnomo(:) => null()
    real(kind=8), pointer :: vitmo(:) => null()
!
! ----------------------------------------------------------------------
!
    lamor = ndynlo(sddyna,'MAT_AMORT')
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    call nmchex(valinc, 'VALINC', 'FEXMOI', fexmoi)
    call jeveuo(fexmoi//'.VALE', 'E', vr=fexmo)
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- AJOUT DE LA FORCE DE LIAISON ET DE LA FORCE D AMORTISSEMENT MODAL
!
    call nmchex(valinc, 'VALINC', 'FAMMOI', fammoi)
    call jeveuo(fammoi//'.VALE', 'L', vr=fammo)
    call nmchex(valinc, 'VALINC', 'FLIMOI', flimoi)
    call jeveuo(flimoi//'.VALE', 'L', vr=flimo)
    do iaux = 1, neq
        fexmo(iaux)=fammo(iaux)+flimo(iaux)
    end do
!
! --- AJOUT DU TERME C.V
!
    if (lamor) then
        call nmchex(measse, 'MEASSE', 'MEAMOR', amort)
        call mtdscr(amort)
        call jeveuo(amort//'.&INT', 'L', iamort)
        call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
        call jeveuo(vitmoi//'.VALE', 'L', vr=vitmo)
        AS_ALLOCATE(vr=cv, size=neq)
        call mrmult('ZERO', iamort, vitmo, cv, 1,&
                    .true._1)
        do iaux = 1, neq
            fexmo(iaux) = fexmo(iaux) + cv(iaux)
        end do
        AS_DEALLOCATE(vr=cv)
    endif
!
! --- AJOUT DU TERME M.A
!
    if (ldyna) then
        call nmchex(measse, 'MEASSE', 'MEMASS', masse)
        call mtdscr(masse)
        call jeveuo(masse//'.&INT', 'L', imasse)
        call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
        call jeveuo(accmoi//'.VALE', 'L', vr=ccmo)
        AS_ALLOCATE(vr=ma, size=neq)
        call mrmult('ZERO', imasse, ccmo, ma, 1,&
                    .true._1)
        do iaux = 1, neq
            fexmo(iaux) = fexmo(iaux) + ma(iaux)
        end do
        AS_DEALLOCATE(vr=ma)
    endif
!
! --- AJOUT DU TERME CNFNOD
!
    nbvect=0
    call nmcvec('AJOU', 'CNFNOD', 'SIGMOI', .true._1, .true._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
    call nmxvec(modele  , mate  , carele, compor, ds_measure,&
                sddisc  , sddyna, numins, valinc, solalg,&
                lischa  , comref, numedd,&
                ds_inout, veelem, veasse, measse, nbvect,&
                ltypve  , lcalve, loptve, lassve)
    call nmchex(veasse, 'VEASSE', 'CNFNOD', cnfnod)
    call jeveuo(cnfnod//'.VALE', 'L', vr=cnfno)
    do iaux = 1, neq
        fexmo(iaux) = fexmo(iaux) + cnfno(iaux)
    end do
!
! --- INITIALISATION DES FORCES INTERNES
!
    call nmchex(valinc, 'VALINC', 'FNOMOI', fnomoi)
    call jeveuo(fnomoi//'.VALE', 'E', vr=fnomo)
    do iaux = 1, neq
        fnomo(iaux) = cnfno(iaux)
    end do
!
end subroutine
