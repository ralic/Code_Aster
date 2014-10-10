subroutine elg_calc_rhs_red(matas1, nsecm, secm, solu2)
    implicit none
! aslint: disable=W0104
! person_in_charge: jacques.pellet at edf.fr
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
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: matas1
    integer :: nsecm
    real(kind=8) :: secm(*)
    character(len=24) :: solu2
!--------------------------------------------------------------
! BUT :
!   calculer le(s) second-membre(s) réduits correspondant à
!   un (ou plusieurs) second-membre(s) complets.
!
! IN  : MATAS1 : sd_matr_asse avec ses conditions dualisées
!                à eliminer
! IN  : NSECM  :  nombre de seconds membres
! IN  : SECM(*)  : vecteur de réels de dimension nsecm*neq1
!                  (valeurs de(s) second-membre(s) complets)
! IN/JXOUT : SOLU2  : objet jeveux qui contiendra le(s)
!                     seconds membre(s) réduit(s)
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
#include "asterfort/elg_calcx0.h"
#include "asterfort/elg_allocvr.h"
!
!================================================================
    character(len=1) :: kbid
    character(len=14) :: nu1, nu2
    character(len=19) :: matas2
    integer ::  ibid
    integer :: neq1, neq2, icob, icoc, ieq1, ieq2
    integer :: jsolu2
    character(len=24), pointer :: refa(:) => null()
    real(kind=8), pointer :: conl(:) => null()
    integer, pointer :: delg(:) => null()
    PetscInt :: ierr
    PetscInt :: n1, n2, n3
    PetscScalar :: xx(1), m1
    PetscOffset :: xidxb, xidxc, xidxb2
    Vec :: bx0, vecb2, vectmp
!
!
!----------------------------------------------------------------
    call jemarq()
!
    call jeveuo(matas1//'.REFA', 'L', vk24=refa)
    matas2=refa(19)(1:19)
    ASSERT(matas2.ne.' ')
!
    call dismoi('NOM_NUME_DDL', matas1, 'MATR_ASSE', repk=nu1)
    call dismoi('NOM_NUME_DDL', matas2, 'MATR_ASSE', repk=nu2)
    call dismoi('NB_EQUA', nu1, 'NUME_DDL', repi=neq1)
    call dismoi('NB_EQUA', nu2, 'NUME_DDL', repi=neq2)
!
! à faire ....
    ASSERT(nsecm.eq.1)
!
!     -- dimensions n1, n2, n3 :
    call MatGetSize(melim(ke)%tfinal, n1, n3, ierr)
    call MatGetSize(melim(ke)%ctrans, n1, n2, ierr)
    ASSERT(neq2.eq.n3)
    ASSERT(neq1.eq.n1+2*n2)
!
!
!     -- allocation de VecB, VecC, VecB2 :
!     ---------------------------------------------
    call elg_allocvr(melim(ke)%vecb, int(n1))
    call elg_allocvr(melim(ke)%vecc, int(n2))
    call elg_allocvr(vecb2, int(n3))
!
!
!     -- calcul de VecB et VecC (extraits de SECM) :
!     ------------------------------------------------
    call VecGetArray(melim(ke)%vecb, xx, xidxb, ierr)
    call VecGetArray(melim(ke)%vecc, xx, xidxc, ierr)
    call jeveuo(nu1//'.NUME.DELG', 'L', vi=delg)
    call jeveuo(matas1//'.CONL', 'L', vr=conl)
!
    icob=0
    icoc=0
    do ieq1 = 1, neq1
        if (delg(ieq1) .eq. 0) then
            icob=icob+1
            xx(xidxb+icob)=secm(ieq1)
        else if (delg(ieq1) .eq. -1) then
            icoc=icoc+1
            xx(xidxc+icoc)=secm(ieq1)*conl(ieq1)
        endif
    end do
    call VecRestoreArray(melim(ke)%vecb, xx, xidxb, ierr)
    call VecRestoreArray(melim(ke)%vecc, xx, xidxc, ierr)
!
!
!     -- calcul de Vx0 = A \ VecC
    call VecDuplicate(melim(ke)%vecb, melim(ke)%vx0, ierr)
!
    call elg_calcx0()
!
!     -- calcul de BX0 = B*Vx0 :
    call VecDuplicate(melim(ke)%vecb, bx0, ierr)
    call MatMult(melim(ke)%matb, melim(ke)%vx0, bx0, ierr)
!
!
!     -- calcul de VecTmp = b - B*Vx0 :
    m1=-1.d0
    call VecDuplicate(melim(ke)%vecb, vectmp, ierr)
    call VecCopy(melim(ke)%vecb, vectmp, ierr)
    call VecAXPY(vectmp, m1, bx0, ierr)
!
!     -- calcul de VecB2 = T'*(b - B*Vx0) :
    call MatMultTranspose(melim(ke)%tfinal, vectmp, vecb2, ierr)
!
!     -- recopie de VecB2 dans SOLU2 :
    call wkvect(solu2, 'V V R', neq2, jsolu2)
    call VecGetArray(vecb2, xx, xidxb2, ierr)
    do ieq2 = 1, neq2
        zr(jsolu2-1+ieq2)=xx(xidxb2+ieq2)
    end do
    call VecRestoreArray(vecb2, xx, xidxb2, ierr)
!
!
!
!     -- ménage :
    call VecDestroy(vectmp, ierr)
    call VecDestroy(bx0, ierr)
    call VecDestroy(vecb2, ierr)
!
    call jedema()
#else
    call utmess('F', 'ELIMLAGR_1')
#endif
!
end subroutine
