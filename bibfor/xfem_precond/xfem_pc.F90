subroutine xfem_pc(matass, base, filtrage, typ_pc)
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!-----------------------------------------------------------------------
! BUT : CALCUL DU PRE-CONDITIONNEUR XFEM
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/echmat.h"
#include "asterfort/infniv.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecroc.h"
#include "asterfort/jecrec.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mtdefs.h"
#include "asterfort/mtdscr.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xfem_count_ddl.h"
#include "asterfort/xfem_count_no.h"
#include "asterfort/xfem_calc_diag.h"
#include "asterfort/xfem_store_pc.h"
!-----------------------------------------------------------------------
    character(len=*) :: matass, typ_pc
    character(len=1) :: base
    aster_logical :: filtrage
!-----------------------------------------------------------------------
    character(len=1) :: bas1
    character(len=8) :: nomgd, noma
    character(len=14) :: nonu
    character(len=19) :: matas1, pc_1
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: refn(:) => null()
    real(kind=8), pointer :: tab_mloc(:) => null()
    integer, pointer :: ino_xfem(:) => null()
    integer, pointer :: neq_mloc(:) => null()
    integer, pointer :: iglob_ddl(:) => null()
    integer, pointer :: ieq_loc(:) => null()
    aster_logical, pointer :: is_xfem(:) => null()
    integer :: nuno, neq, ieq, jcmp, jdeeq
    integer :: jdime, nbnomax, nbnoxfem, deca, maxi_ddl, nvale, niv, ifm
    real(kind=8) :: kmin, kmax, coef, scal, seuil
    aster_logical :: lmd
    parameter    (pc_1='&&XFEM_PC_1')
!-----------------------------------------------------------------------
!
    call jemarq()
!
    matas1 = matass
    bas1=base
    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
!
    lmd=.false.
    if (refa(11) .eq. 'MATR_DISTR') lmd=.true.
    if (lmd) then 
       goto 999
    endif
!
    call infniv(ifm, niv)
    if(niv .ge. 2) call utmess('I', 'XFEMPRECOND_6')
!
    nonu=refa(2)(1:14)
!
    call jelira(matas1//'.VALM', 'NMAXOC', nvale)
    if (nvale.ne.1 .and. nvale.ne.2) then
        call utmess('A', 'XFEMPRECOND_1')
        goto 999
    endif
!
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', neq)
    call jeveuo(nonu//'.NUME.DEEQ', 'L', jdeeq)
    call jeveuo(nonu//'.NUME.REFN', 'L', vk24=refn)
    nomgd=refn(2)(1:8)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  - MARQUAGE DES NOEUDS XFEM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call dismoi('NOM_MAILLA', nonu, 'NUME_DDL', repk=noma)
    call jeveuo(noma//'.DIME', 'L', jdime)
    nbnomax=zi(jdime-1+1)
    AS_ALLOCATE(vl=is_xfem,size=nbnomax)
    AS_ALLOCATE(vi=ino_xfem,size=nbnomax)
!
    call xfem_count_no(neq, zi(jdeeq), zk8(jcmp), nbnomax, ino_xfem, is_xfem, nbnoxfem)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!  STOCKAGE DE 3 INFOS
!  IGLOB_DDL :: LES NUMEROS D EQUATIONS GLOBAUX ASSOCIES AUX DDLS D UN NOUED XFEM DONNE
!  NEQ_MLOC :: LE NOMBRE TOTAL DE DDL A CONSIDERER POUR UN NOUED XFEM DONNE
!  IEQ_LOC :: LA POSITION LOCALE D UN DDL DANS IGLOB_DDL (IF <> 0 => DDL MARQUE)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    AS_ALLOCATE(vi=neq_mloc,size=nbnoxfem)
    AS_ALLOCATE(vi=ieq_loc,size=neq)
!
    call xfem_count_ddl(neq, zi(jdeeq), zk8(jcmp), nbnomax, ino_xfem, is_xfem, &
                              nbnoxfem, ieq_loc, neq_mloc, maxi_ddl)
!
    AS_ALLOCATE(vi=iglob_ddl,size=nbnoxfem*maxi_ddl)
    do  ieq = 1, neq
       if (ieq_loc(ieq) .ne. 0) then
           nuno=zi(jdeeq-1+2*(ieq-1)+1)
           iglob_ddl(maxi_ddl*(ino_xfem(nuno)-1)+ieq_loc(ieq))=ieq
       endif
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CALCUL COEFFICIENT MISE A ECHELLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call echmat(matas1, lmd, kmin, kmax)
    coef=(kmin+kmax)/2.d0
    scal=dsqrt(coef)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! MISE A ECHELLE DES DDLS X-FEM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    AS_ALLOCATE(vr=tab_mloc,size=nbnoxfem*maxi_ddl)
    call xfem_calc_diag(matas1, nonu, neq, zi(jdeeq), nbnomax, &
                         ino_xfem, is_xfem, nbnoxfem, ieq_loc,&
                         scal, maxi_ddl, zk8(jcmp), tab_mloc)
    call xfem_store_pc(matas1, bas1, nonu, neq, zi(jdeeq),&
                       nbnoxfem, nbnomax, ino_xfem, ieq_loc, neq_mloc,&
                       maxi_ddl, iglob_ddl, maxi_ddl, tab_mloc, pc_1, 'DIAGO')
    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
    refa(17)='XFEM_PRECOND'
    ASSERT( refa(18)(1:19) .eq. ' ' )
    refa(18)(1:19)=pc_1
    call mtdscr(pc_1)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! DESALLOCATIONS  ...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    AS_DEALLOCATE(vl=is_xfem)
    AS_DEALLOCATE(vi=ino_xfem)
    AS_DEALLOCATE(vi=neq_mloc)
    AS_DEALLOCATE(vi=ieq_loc)
    AS_DEALLOCATE(vi=iglob_ddl)
    AS_DEALLOCATE(vr=tab_mloc)
!
999 continue
!
    call jedema()
!
end subroutine
