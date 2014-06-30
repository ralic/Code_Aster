subroutine ndiner(numedd, sddyna, valinc, measse, foiner)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrmult.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
#include "blas/dscal.h"
    character(len=24) :: numedd
    character(len=19) :: sddyna
    character(len=19) :: measse(*), valinc(*)
    character(len=19) :: foiner
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! CALCUL DE LA FORCE D'INERTIE DE REFERENCE
!
! ----------------------------------------------------------------------
!
!
! IN  NUMEDD : NUMEROTATION
! IN  SDDYNA : SD LIEE A LA DYNAMIQUE (CF NDLECT)
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! OUT FOINER : VECTEUR DES FORCES D'INERTIE POUR CONVERGENCE
!
!
!
!
    integer :: ifm, niv
    integer :: jmasse,   jvitm, jvect
    integer :: neq
    real(kind=8) :: coiner
    logical(kind=1) :: lnewma, lthetv, lthetd, lkrenk, ldepl, lvite
    character(len=19) :: vitmoi, vitplu, vector
    character(len=19) :: masse
    real(kind=8), pointer :: foine(:) => null()
    real(kind=8), pointer :: vitp(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> CALCUL DES FORCES D''INERTIE'
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
!
! --- FONCTIONNALITES ACTIVEES
!
    lnewma = ndynlo(sddyna,'FAMILLE_NEWMARK')
    lthetv = ndynlo(sddyna,'THETA_METHODE_VITE')
    lthetd = ndynlo(sddyna,'THETA_METHODE_DEPL')
    lkrenk = ndynlo(sddyna,'KRENK')
    if (lkrenk) then
        ldepl = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.1
        lvite = ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2
    endif
!
! --- INITIALISATIONS
!
    vector = '&&CNPART.CHP2'
    call vtzero(foiner)
    call vtzero(vector)
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- ACCES SD
!
    call jeveuo(masse(1:19)//'.&INT', 'L', jmasse)
    call jeveuo(foiner(1:19)//'.VALE', 'E', vr=foine)
    call jeveuo(vitplu(1:19)//'.VALE', 'L', vr=vitp)
    call jeveuo(vitmoi(1:19)//'.VALE', 'L', jvitm)
    call jeveuo(vector(1:19)//'.VALE', 'L', jvect)
!
    coiner = ndynre(sddyna,'COEF_FORC_INER')
!
! --- CALCUL DU TERME D'INERTIE
!
    if (lnewma) then
        call mrmult('ZERO', jmasse, vitp, foine, 1,&
                    .true._1)
        call dscal(neq, coiner, foine, 1)
        elseif (lthetv.or.(lkrenk.and.lvite).or.lthetd .or.(&
    lkrenk.and.ldepl)) then
        call vtaxpy(-1.d0, vitplu, vector)
        call vtaxpy(1.d0, vitmoi, vector)
        call jeveuo(vector(1:19)//'.VALE', 'L', jvect)
        call mrmult('ZERO', jmasse, zr(jvect), foine, 1,&
                    .true._1)
        call dscal(neq, coiner, foine, 1)
    else
        ASSERT(.false.)
    endif
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        call nmdebg('VECT', foiner, ifm)
    endif
!
    call jedema()
end subroutine
