subroutine ndfdyn(sddyna, measse, vitplu, accplu, cndyna)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmhyst.h"
#include "asterfort/nminer.h"
#include "asterfort/vtaxpy.h"
#include "asterfort/vtzero.h"
#include "asterfort/zerlag.h"
    character(len=19) :: sddyna
    character(len=19) :: measse(*)
    character(len=19) :: vitplu, accplu
    character(len=24) :: cndyna
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DES FORCES DE RAPPEL DYNAMIQUE
!
! ----------------------------------------------------------------------
!
!
!
!
!
!
!
    character(len=8) :: k8bid
    character(len=19) :: amort, masse, rigid
    character(len=19) :: vites, accel, vite2
    character(len=19) :: cniner, cnhyst
    character(len=24) :: nu
    integer :: jprov, jdeeq, jrefa, neq
    real(kind=8) :: coerma, coeram, coerri
    complex(kind=8) :: cbid
    logical :: lamor, limpl
    logical :: lnewma, ltheta, lthetd, lthetv, lkrenk
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- COEFFICIENTS DEVANTS MATRICES
!
    coerma = ndynre(sddyna,'COEF_FDYN_MASSE')
    coeram = ndynre(sddyna,'COEF_FDYN_AMORT')
    coerri = ndynre(sddyna,'COEF_FDYN_RIGID')
!
! --- FONCTIONNALITES ACTIVEES
!
    lamor = ndynlo(sddyna,'MAT_AMORT')
    limpl = ndynlo(sddyna,'IMPLICITE')
!
! --- TYPE DE SCHEMA: NEWMARK (ET SES DERIVEES) OU THETA
!
    lnewma = ndynlo(sddyna,'FAMILLE_NEWMARK')
    ltheta = ndynlo(sddyna,'THETA_METHODE')
    lkrenk = ndynlo(sddyna,'KRENK')
    if (.not.(lnewma.or.ltheta.or.lkrenk)) then
        ASSERT(.false.)
    endif
!
! --- TYPE DE THETA
!
    lthetd = ndynlo(sddyna,'THETA_METHODE_DEPL')
    lthetv = ndynlo(sddyna,'THETA_METHODE_VITE')
    if (lkrenk) then
        if (ndynin(sddyna,'FORMUL_DYNAMIQUE') .eq. 1) then
            lthetd = .true.
            lthetv = .false.
        else if (ndynin(sddyna,'FORMUL_DYNAMIQUE').eq.2) then
            lthetd = .false.
            lthetv = .true.
        else
            ASSERT(.false.)
        endif
    endif
!
! --- MATRICES ASSEMBLEES
!
    call nmchex(measse, 'MEASSE', 'MEAMOR', amort)
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
    call nmchex(measse, 'MEASSE', 'MERIGI', rigid)
!
! --- VECTEURS RESULTATS
!
    cniner = '&&CNPART.CHP1'
    cnhyst = '&&CNPART.CHP2'
    call vtzero(cniner)
    call vtzero(cnhyst)
    call vtzero(cndyna)
!
! --- VECTEURS SOLUTIONS
!
    vites = vitplu
    accel = accplu
    if (limpl) then
        if (lnewma) then
            call nminer(masse, accel, cniner)
            call vtaxpy(coerma, cniner, cndyna)
        else if (lthetd) then
            call nminer(masse, vites, cniner)
            call vtaxpy(coerma, cniner, cndyna)
        else if (lthetv) then
!  Mise a zero des termes Lagrange pour la resolution en vitesse
            vite2 = '&&NDFDYN.VITE'
            call copisd('CHAMP_GD', 'V', vites, vite2)
            call jeveuo(vite2(1:19)//'.VALE', 'E', jprov)
            call jeveuo(rigid//'.REFA', 'L', jrefa)
            nu = zk24(jrefa-1+2)
            call jeveuo(nu(1:14)//'.NUME.DEEQ', 'L', jdeeq)
            call jelira(vite2(1:19)//'.VALE', 'LONMAX', neq, k8bid)
            call zerlag('R', zr(jprov), cbid, neq, zi(jdeeq))
            call nminer(rigid, vite2, cniner)
            call jeveuo(cniner(1:19)//'.VALE', 'E', jprov)
            call zerlag('R', zr(jprov), cbid, neq, zi(jdeeq))
            call vtaxpy(coerri, cniner, cndyna)
            call jeveuo(cndyna(1:19)//'.VALE', 'E', jprov)
        else
            ASSERT(.false.)
        endif
    endif
!
    if (lamor) then
        call nmhyst(amort, vites, cnhyst)
        call vtaxpy(coeram, cnhyst, cndyna)
    endif
!
    call jedema()
end subroutine
