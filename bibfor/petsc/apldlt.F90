subroutine apldlt(kptsc, action, prepost, rsolu, vcine, nbsol)
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
use petsc_data_module
    implicit none
!
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=W0104
! W0104 because of ifdef PETSc

#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedetr.h"
#include "asterfort/ldlt_matr.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"

    integer, intent(in) :: kptsc
    character(len=*), intent(in) :: action, prepost
    real(kind=8), intent(inout) :: rsolu(*)
    character(len=19), intent(inout) :: vcine
    integer, intent(in) :: nbsol

!----------------------------------------------------------------
!
!  Renumerotation de la matrice si PRE_COND=LDLT_INC
!  (pour etre plus efficace)
!  + renumerotation de rsolu
!  + renumerotation de vcine
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
!----------------------------------------------------------------
    integer :: k,isol,neq
    character(len=24) :: precon,kperm
    character(len=19) :: matas2,nosolv,vcin2, vcine_avant=' '
    character(len=24), dimension(:), pointer :: slvk => null()
    real(kind=8), dimension(:), pointer :: tempor => null()
    real(kind=8), dimension(:), pointer :: vciv1 => null()
    real(kind=8), dimension(:), pointer :: vciv2 => null()
    integer, dimension(:), pointer :: perm => null()
!
!----------------------------------------------------------------
    call jemarq()
    ASSERT(prepost.eq.'PRE' .or. prepost.eq.'POST')
    ASSERT(kptsc.ge.1 .and. kptsc.le.5)
    matas2='&&apldlt.matr'
    kperm='&&apldlt.perm'

    nomat_courant=nomats(kptsc)
    nonu_courant=nonus(kptsc)


!   1. Il n'y a peut-etre rien a faire => goto 999 :
!   ------------------------------------------------
    if (action.ne.'PRERES' .and. action.ne.'RESOUD') goto 999

    nosolv = nosols(kptsc)
    call jeveuo(nosolv//'.SLVK', 'L', vk24=slvk)
    precon = slvk(2)
    if (precon.ne.'LDLT_INC') goto 999


!   2. Calcul de la nouvelle matrice, du nouveau nume_ddl et de kprem :
!   -------------------------------------------------------------------
    if (prepost.eq.'PRE') then
        call ldlt_matr(nomats(kptsc), matas2, kperm, 'V')
    endif
    nomat_courant=matas2
    call dismoi('NOM_NUME_DDL', matas2, 'MATR_ASSE', repk=nonu_courant)


!   3. Renumerotation de rsolu et vcine :
!   -------------------------------------
    if (action.eq.'RESOUD') then
        call jeveuo(kperm, 'L', vi=perm)
        neq=size(perm)

!       3.1 Renumerotation de rsolu :
!       -----------------------------
        AS_ALLOCATE(vr=tempor,size=neq)
        do isol=1,nbsol
            do k=1,neq
                tempor(k)=rsolu((isol-1)*neq +k)
            enddo
            if (prepost.eq.'PRE') then
                do k=1,neq
                    rsolu((isol-1)*neq +perm(k))=tempor(k)
                enddo
            elseif (prepost.eq.'POST') then
                do k=1,neq
                    rsolu((isol-1)*neq +k)=tempor(perm(k))
                enddo
            endif
        enddo
        AS_DEALLOCATE(vr=tempor)

!       3.2 Renumerotation de vcine :
!       -----------------------------
        if (prepost.eq.'PRE') then
            vcin2='&&apldlt.vcine'
            call copisd('CHAMP', 'V', vcine, vcin2)
            call jeveuo(vcine//'.VALE', 'L', vr=vciv1)
            call jeveuo(vcin2//'.VALE', 'E', vr=vciv2)
            ASSERT(size(vciv1).eq.neq)
            ASSERT(size(vciv2).eq.neq)
            do k=1,neq
                vciv2(perm(k))=vciv1(k)
            enddo
            vcine_avant=vcine
            vcine=vcin2
        elseif (prepost.eq.'POST') then
            vcine=vcine_avant
            call detrsd('CHAMP', '&&apldlt.vcine')
        endif

    endif


!   4. Menage :
!   -----------
    if (prepost.eq.'POST') then
        call detrsd('MATR_ASSE', nomat_courant)
        call detrsd('NUME_DDL', nonu_courant)
        call jedetr(kperm)
    endif


999 continue
    call jedema()

#else
    integer :: idummy
    idummy = kptsc
#endif

end subroutine
