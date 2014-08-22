subroutine xfem_to_mat(matass, base)
!-----------------------------------------------------------------------
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
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/infniv.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
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
#include "asterfort/xfem_cmps.h"
#include "asterfort/xfem_count_ddl.h"
#include "asterfort/xfem_count_no.h"
#include "asterfort/xfem_calc_pc.h"
#include "asterfort/xfem_calc_mloc.h"
#include "asterfort/xfem_mult_l.h"
#include "asterfort/xfem_mult_r.h"
#include "asterfort/xfem_over_writ2.h"
!-----------------------------------------------------------------------
    character(len=*) :: matass
    character(len=1) :: base
!-----------------------------------------------------------------------
    character(len=1) :: bas1
    character(len=8) :: nomgd, noma
    character(len=14) :: nonu, nu_pc_0
    character(len=19) :: matas1, pc_0, pc_1
    character(len=24), pointer :: refa(:) => null()
    character(len=24), pointer :: refn(:) => null()
    real(kind=8), pointer :: tab_mloc(:) => null()
    real(kind=8), pointer :: vect_col(:) => null()
    real(kind=8), pointer :: vect_raw(:) => null()
    integer, pointer :: ino_xfem(:) => null()
    integer, pointer :: neq_mloc(:) => null()
    integer, pointer :: iglob_ddl(:) => null()
    integer, pointer :: nnz_mloc(:) => null()
    integer, pointer :: ieq_loc(:) => null()
    integer, pointer :: nblig_pc(:) => null()
    integer, pointer :: nz_raw(:) => null()
    integer, pointer :: smhc_pc(:) => null()
    integer, pointer :: smhc_adr(:) => null()
    integer, pointer :: vect_adr(:) => null()
    integer, pointer :: adr_raw(:) => null()
    integer, pointer :: count_raw(:) => null()
    aster_logical, pointer :: is_xfem(:) => null()
    aster_logical, pointer :: is_connec(:) => null()
    integer :: nuno, neq, ieq, jcmp, jnueq, jdeeq, jconl
    integer :: jdime, nbnomax, nbnoxfem, deca, maxi_ddl, nvale, niv, ifm
    aster_logical ::  lsym, lmd
    integer :: incr, iret, size_smhc, size_vect_col, size_vect_raw
    real(kind=8) :: scal
!-----------------------------------------------------------------------
!
    call jemarq()
!
    matas1 = matass
    bas1=base
    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
!
    ASSERT( refa(17) .eq. 'XFEM_PRECOND')
    ASSERT( refa(18) .ne. ' ')
    ASSERT( refa(16) .ne. ' ')
!
    pc_1=refa(18)(1:19)
    pc_0=refa(16)(1:19)
!
    lmd=.false.
    if (refa(11) .eq. 'MATR_DISTR') lmd=.true.
    if (lmd) then 
       goto 999
    endif
!    if (lmd) call jeveuo(nonu//'.NUML.NULG', 'L', jnlogl)
!
    call infniv(ifm, niv)
    if(niv .ge. 2) call utmess('I', 'XFEMPRECOND_7')
!
    nonu=refa(2)(1:14)
!
    call jelira(matas1//'.VALM', 'NMAXOC', nvale)
    if (nvale .eq. 1) then
        lsym=.true.
    else if (nvale.eq.2) then
        lsym=.false.
        call utmess('A', 'XFEMPRECOND_1')
        goto 999
    else
        ASSERT(.false.)
    endif
!
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', neq)
    call jeveuo(nonu//'.NUME.DEEQ', 'L', jdeeq)
    call jeveuo(nonu//'.NUME.REFN', 'L', vk24=refn)
    nomgd=refn(2)(1:8)
    call jeveuo(jexnom('&CATA.GD.NOMCMP', nomgd), 'L', jcmp)
! VERIFICATION DU NUEQ
    call jeveuo(nonu//'.NUME.NUEQ', 'L', jnueq)
    incr=0
    do ieq=1,neq
       if (zi(jnueq-1+ieq) .ne. ieq) incr=incr+1
    enddo
    if (incr .gt. 0) then 
         call utmess('F', 'XFEMPRECOND_2')
         goto 999
    endif
!  
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
    do ieq = 1, neq
       if (ieq_loc(ieq) .ne. 0) then
           nuno=zi(jdeeq-1+2*(ieq-1)+1)
           iglob_ddl(maxi_ddl*(ino_xfem(nuno)-1)+ieq_loc(ieq))=ieq
       endif
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   CONDENSATION DANS UN "ZR" DES MATRICES LOCALES DE PRE CONDITIONNMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    deca=maxi_ddl**2
    AS_ALLOCATE(vr=tab_mloc,size=nbnoxfem*deca)
    AS_ALLOCATE(vi=nnz_mloc,size=nbnoxfem)
!
    call dismoi('NOM_NUME_DDL', pc_0, 'MATR_ASSE', repk=nu_pc_0)
!
    call xfem_calc_mloc(pc_0, nu_pc_0, neq, zi(jdeeq), nbnomax, &
                         ino_xfem, is_xfem, nbnoxfem, ieq_loc, neq_mloc,&
                         nnz_mloc, deca, tab_mloc)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! ACTION DU PRE-CONDITIONNEUR A DROITE ET A GAUCHE SUR MATRICE ASTER
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!---------------------------------------
!   PHASE 1: PRODUIT A DROITE
!---------------------------------------
    AS_ALLOCATE(vi=nblig_pc,size=nbnoxfem)
!
!    - PREMIERE PASSE : CALCUL DE L ESPACE <DENSE> NECESSAIRE POUR STOCKER LES PRODUITS LOCAUX
!
    call xfem_mult_r('COMPTAGE', matas1, maxi_ddl, iglob_ddl, nbnoxfem,&
                     neq_mloc, nblig_pc, size_smhc, size_vect_col, deca)
!
    AS_ALLOCATE(vr=vect_col,size=size_vect_col)
    AS_ALLOCATE(vi=smhc_pc,size=size_smhc)
    AS_ALLOCATE(vi=smhc_adr,size=nbnoxfem)
    AS_ALLOCATE(vi=vect_adr,size=nbnoxfem)
!
!    - DEUXIEME PASSE : PRODUITS LOCAUX : K * Pc  
!
    call xfem_mult_r('REMPLISSAGE', matas1, maxi_ddl, iglob_ddl, nbnoxfem,&
                     neq_mloc, nblig_pc, size_smhc, size_vect_col, deca,&
                     tab_mloc, smhc_pc, smhc_adr, vect_adr, vect_col)
!
!---------------------------------------
!   PHASE 2: PRODUIT A GAUCHE
!---------------------------------------
    AS_ALLOCATE(vi=nz_raw,size=nbnoxfem)
    AS_ALLOCATE(vl=is_connec,size=neq)
    AS_ALLOCATE(vi=adr_raw,size=nbnoxfem)
!
!    - PREMIERE PASSE : CALCUL DE L ESPACE <DENSE> NECESSAIRE POUR STOCKER LES PRODUITS LOCAUX
!
    call xfem_mult_l('COMPTAGE', matas1, nbnomax, ino_xfem, nbnoxfem,&
                     neq, ieq_loc, neq_mloc, maxi_ddl, iglob_ddl,&
                     nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr,&
                     size_vect_col, deca, is_connec, nz_raw, adr_raw,&
                     size_vect_raw)
!
    AS_ALLOCATE(vr=vect_raw,size=size_vect_raw)
!
!    - DEUXIEME PASSE : PRODUITS LOCAUX : Pc^T * (K*Pc)
!
    call xfem_mult_l('REMPLISSAGE', matas1, nbnomax, ino_xfem, nbnoxfem,&
                     neq, ieq_loc, neq_mloc, maxi_ddl, iglob_ddl,&
                     nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr,&
                     size_vect_col, deca, is_connec, nz_raw, adr_raw,&
                     size_vect_raw, vect_raw, vect_col, tab_mloc)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   COMPRESSION ET REMPLACEMENT DE LA MATRICE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
    call jeveuo(pc_0//'.CONL', 'L', jconl)
    scal=zr(jconl)
    ASSERT( scal .gt. 0.d0)
!
    call xfem_calc_pc('PROD_SYM', nbnoxfem, neq_mloc, nnz_mloc, deca, tab_mloc,&
                        scal**2, iret)
!
    if ( iret .ne. 0) goto 999
!
    call xfem_over_writ2(matas1, bas1, nbnomax, ino_xfem, neq, &
                         ieq_loc, nbnoxfem, neq_mloc, maxi_ddl, iglob_ddl,&
                         nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr, &
                         size_vect_col, vect_col, adr_raw, size_vect_raw, &
                         vect_raw, is_connec, deca, tab_mloc)
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! STOCKAGE DU NOUVEL ETAT DE LA MATRICE ASTER
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
    refa(17)=' '
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NETTOYAGES ...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    AS_DEALLOCATE(vi=count_raw)
    AS_DEALLOCATE(vi=adr_raw)
    AS_DEALLOCATE(vi=nblig_pc)
    AS_DEALLOCATE(vi=nz_raw)
    AS_DEALLOCATE(vi=smhc_pc)
    AS_DEALLOCATE(vi=smhc_adr)
    AS_DEALLOCATE(vi=vect_adr)
    AS_DEALLOCATE(vl=is_connec)
    AS_DEALLOCATE(vr=vect_col)  
    AS_DEALLOCATE(vr=vect_raw)  
    AS_DEALLOCATE(vl=is_xfem)
    AS_DEALLOCATE(vi=ino_xfem)
    AS_DEALLOCATE(vi=neq_mloc)
    AS_DEALLOCATE(vi=iglob_ddl)
    AS_DEALLOCATE(vi=nnz_mloc)
    AS_DEALLOCATE(vi=ieq_loc)
    AS_DEALLOCATE(vr=tab_mloc)
!
999 continue
!
    call jedema()
!
end subroutine
