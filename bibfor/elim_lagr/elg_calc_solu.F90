subroutine elg_calc_solu(matas1, nsecm, rsolu2, rsolu1)
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
# include "jeveux.h"
# include "asterfort/assert.h"
# include "asterfort/dismoi.h"
# include "asterfort/jedema.h"
# include "asterfort/jemarq.h"
# include "asterfort/jeveuo.h"
# include "asterfort/nudlg2.h"
# include "asterfort/utmess.h"
!
    character(len=19) :: matas1
    integer :: nsecm
    real(kind=8) :: rsolu2(*), rsolu1(*)
!--------------------------------------------------------------
! BUT :
!   calculer les solutions complètes (RSOLU1) correspondant aux
!   solutions réduites (RSOLU2)
!
! IN  : MATAS1 : sd_matr_asse avec ses conditions dualisées
!                à eliminer
! IN  : NSECM  :  nombre de solutions
! IN  : RSOLU2(*)  : vecteur de réels de dimension nsecm*neq2
!                  (valeurs des solutions réduites)
! IN  : BASE  :  'V' / 'G'
! OUT : RSOLU1(*)  : vecteur de réels de dimension nsecm*neq1
!                  (valeurs des solutions complètes)
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
# include "asterfort/elg_allocvr.h"
# include "asterfort/elg_calcxl.h"
!
!================================================================
    character(len=1) :: kbid
    character(len=14) :: nu1, nu2
    character(len=19) :: matas2
    real(kind=8) :: val
    integer ::  ibid
    integer :: neq1, neq2, ico, ieq2
    integer :: k1, k2
    real(kind=8), pointer :: conl(:) => null()
    integer, pointer :: delg(:) => null()
    integer, pointer :: dlg2(:) => null()
    character(len=24), pointer :: refa(:) => null()
    PetscInt :: ierr
    PetscInt :: n1, n2, n3
    PetscScalar :: xx(1), p1
    PetscOffset :: xidx
    Vec :: x1, y, vlag, tmp1
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
    call jeveuo(nu1//'.NUME.DELG', 'L', vi=delg)
    call nudlg2(nu1)
    call jeveuo(nu1//'.NUME.DLG2', 'L', vi=dlg2)
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
!     allocation et remplissage de Y = RSOLU2
    call elg_allocvr(y, to_aster_int(n3))
    call VecGetArray(y, xx, xidx, ierr)
    do ieq2 = 1, neq2
        xx(xidx+ieq2)=rsolu2(ieq2)
    end do
    call VecRestoreArray(y, xx, xidx, ierr)
!
!
!     Calcul de TMP1 =  T*Y :
    call elg_allocvr(tmp1, to_aster_int(n1))
    call MatMult(melim(ke)%tfinal, y, tmp1, ierr)
!
!     Calcul de X1= x0 + T*Y :
    call elg_allocvr(x1, int(n1))
    call VecCopy(melim(ke)%vx0, x1, ierr)
    p1=1.
    call VecAXPY(x1, p1, tmp1, ierr)
!
!     calcul des coefficients de Lagrange :
    call elg_allocvr(vlag, to_aster_int(n2))
    call elg_calcxl(x1, vlag)
!
!
!     -- on recopie X1 dans RSOLU1 :
    call VecGetArray(x1, xx, xidx, ierr)
    ico=0
    do k1 = 1, neq1
        if (delg(k1) .eq. 0) then
            ico=ico+1
            rsolu1(k1)=xx(xidx+ico)
        endif
    enddo
    ASSERT(ico.eq.n1)
    call VecRestoreArray(x1, xx, xidx, ierr)
!
!
!     -- on recopie VLAG dans RSOLU1 :
!        remarque : il faut diviser VLAG par 2 (2 lagranges)
!                   Lagrange "1"  et "2" :
    call jeveuo(matas1//'.CONL', 'L', vr=conl)
    call VecGetArray(vlag, xx, xidx, ierr)
    ico=0
    do k1 = 1, neq1
        if (delg(k1) .eq. -1) then
            ico=ico+1
            val=xx(xidx+ico)*conl(k1)/2.d0
            rsolu1(k1)=val
! k2 lagrange "2" associé au lagrange "1" k1
            k2=dlg2(k1)
            ASSERT(k2.gt.0)
            rsolu1(k2)=val
        endif
    enddo
    ASSERT(ico.eq.n2)
    call VecRestoreArray(vlag, xx, xidx, ierr)
!
!
!
!
!     -- ménage :
    call VecDestroy(y, ierr)
    call VecDestroy(tmp1, ierr)
    call VecDestroy(x1, ierr)
    call VecDestroy(tmp1, ierr)
!
    call jedema()
#else
    call utmess('F', 'ELIMLAGR_1')
#endif
!
end subroutine
