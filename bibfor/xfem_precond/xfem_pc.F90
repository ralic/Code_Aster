subroutine xfem_pc(matass, base, filtrage)
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
#include "asterfort/xfem_calc_pc.h"
#include "asterfort/xfem_calc_mloc.h"
#include "asterfort/xfem_store_pc.h"
#include "asterfort/xfem_mult_l.h"
#include "asterfort/xfem_mult_r.h"
#include "asterfort/xfem_over_write.h"
#include "asterfort/xfem_over_write_2.h"
!-----------------------------------------------------------------------
    character(len=*) :: matass
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
    real(kind=8), pointer :: vect_col(:) => null(), vect_col_2(:) => null()
    real(kind=8), pointer :: vect_raw(:) => null(), vect_raw_2(:) => null()
    integer, pointer :: ino_xfem(:) => null()
    integer, pointer :: neq_mloc(:) => null()
    integer, pointer :: iglob_ddl(:) => null()
    integer, pointer :: nnz_mloc(:) => null()
    integer, pointer :: ieq_loc(:) => null()
    integer, pointer :: nblig_pc(:) => null(), nblig_pc_2(:) => null()
    integer, pointer :: nz_raw(:) => null(), nz_raw_2(:) => null()
    integer, pointer :: smhc_pc(:) => null(), smhc_pc_2(:) => null()
    integer, pointer :: smhc_adr(:) => null(), smhc_adr_2(:) => null()
    integer, pointer :: vect_adr(:) => null(), vect_adr_2(:) => null()
    integer, pointer :: adr_raw(:) => null(), adr_raw_2(:) => null()
    aster_logical, pointer :: is_xfem(:) => null()
    aster_logical, pointer :: is_connec(:) => null(), is_connec_2(:) => null()
    aster_logical, pointer :: is_svd(:) => null()
    integer :: nuno, neq, ieq, jcmp, jnueq, jdeeq
    integer :: jdime, nbnomax, nbnoxfem, deca, maxi_ddl, nvale, niv, ifm, incr, iret_pc
    real(kind=8) :: kmin, kmax, coef, scal, seuil
    aster_logical :: lmd
    integer :: size_smhc, size_vect_col, size_vect_raw
    integer :: size_smhc_2, size_vect_col_2, size_vect_raw_2
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
!       call utmess('A', 'XFEM_PRECOND_2')
       goto 999
    endif
!    if (lmd) call jeveuo(nonu//'.NUML.NULG', 'L', jnlogl)
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
! VERIFICATION DU NUEQ
    call jeveuo(nonu//'.NUME.NUEQ', 'L', jnueq)
    incr=0
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
    do 21 ieq = 1, neq
       if (ieq_loc(ieq) .ne. 0) then
           nuno=zi(jdeeq-1+2*(ieq-1)+1)
           iglob_ddl(maxi_ddl*(ino_xfem(nuno)-1)+ieq_loc(ieq))=ieq
       endif
21  enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   CONDENSATION DANS UN "ZR" DES MATRICES LOCALES DE PRE CONDITIONNMENT
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    deca=maxi_ddl**2
    AS_ALLOCATE(vr=tab_mloc,size=nbnoxfem*deca)
    AS_ALLOCATE(vi=nnz_mloc,size=nbnoxfem)
!
    call xfem_calc_mloc(matas1, nonu, neq, zi(jdeeq), nbnomax, &
                         ino_xfem, is_xfem, nbnoxfem, ieq_loc, neq_mloc,&
                         nnz_mloc, deca, tab_mloc)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! CALCUL COEFFICIENT MISE A ECHELLE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call echmat(matas1, lmd, kmin, kmax)
    coef=(kmin+kmax)/2.d0
    scal=dsqrt(coef)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! FACTORISATION DE CHOLESKY DES MATRICES LOCALES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    if (nvale.eq.2) then
      call xfem_calc_pc('IS_SYME', nbnoxfem, neq_mloc, nnz_mloc, deca, tab_mloc,&
                        1.d0, iret_pc)
      if ( iret_pc .ne. 0) goto 999
    endif
!
    AS_ALLOCATE(vl=is_svd,size=nbnoxfem)
!
    call xfem_calc_pc('CHOLESKY', nbnoxfem, neq_mloc, nnz_mloc, deca, tab_mloc,&
                        1.d0, iret_pc, is_svd)
!
    if ( iret_pc .ne. 0) goto 999
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! INVERSE DES MATICES CHOLESKY DES MATRICES LOCALES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    call xfem_calc_pc('INV_AND_SCAL', nbnoxfem, neq_mloc, nnz_mloc, deca, tab_mloc,&
                        scal, iret_pc, is_svd)
!
    if ( iret_pc .ne. 0) goto 999
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! STOCKAGE DES MATRICES BLOC INVERSE DANS UNE MATR_ASSE (PRE CONDITIONNEUR)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    call xfem_store_pc(matas1, bas1, nonu, neq, zi(jdeeq),&
                       nbnoxfem, nbnomax, ino_xfem, ieq_loc, neq_mloc,&
                       maxi_ddl, iglob_ddl, deca, tab_mloc, pc_1)
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
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   CAS DES MATRICES NON SYMETRIQUES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( nvale.eq.2) then
      AS_ALLOCATE(vi=nblig_pc_2,size=nbnoxfem)
      call xfem_mult_r('COMPTAGE', matas1, maxi_ddl, iglob_ddl, nbnoxfem,&
                     neq_mloc, nblig_pc_2, size_smhc_2, size_vect_col_2, deca)
      AS_ALLOCATE(vr=vect_col_2,size=size_vect_col_2)
      AS_ALLOCATE(vi=smhc_pc_2,size=size_smhc_2)
      AS_ALLOCATE(vi=smhc_adr_2,size=nbnoxfem)
      AS_ALLOCATE(vi=vect_adr_2,size=nbnoxfem)
      call xfem_mult_r('REMPLISSAGE', matas1, maxi_ddl, iglob_ddl, nbnoxfem,&
                     neq_mloc, nblig_pc_2, size_smhc_2, size_vect_col_2, deca,&
                     tab_mloc, smhc_pc_2, smhc_adr_2, vect_adr_2, vect_col_2, 2)
!
      AS_ALLOCATE(vi=nz_raw_2,size=nbnoxfem)
      AS_ALLOCATE(vl=is_connec_2,size=neq)
      AS_ALLOCATE(vi=adr_raw_2,size=nbnoxfem)
      call xfem_mult_l('COMPTAGE', matas1, nbnomax, ino_xfem, nbnoxfem,&
                     neq, ieq_loc, neq_mloc, maxi_ddl, iglob_ddl,&
                     nblig_pc_2, size_smhc_2, smhc_pc_2, smhc_adr_2, vect_adr_2,&
                     size_vect_col_2, deca, is_connec_2, nz_raw_2, adr_raw_2,&
                     size_vect_raw_2)
      AS_ALLOCATE(vr=vect_raw_2,size=size_vect_raw_2)
      call xfem_mult_l('REMPLISSAGE', matas1, nbnomax, ino_xfem, nbnoxfem,&
                     neq, ieq_loc, neq_mloc, maxi_ddl, iglob_ddl,&
                     nblig_pc_2, size_smhc_2, smhc_pc_2, smhc_adr_2, vect_adr_2,&
                     size_vect_col_2, deca, is_connec_2, nz_raw_2, adr_raw_2,&
                     size_vect_raw_2, vect_raw_2, vect_col_2, tab_mloc, 2)
    endif
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   COMPRESSION ET REMPLACEMENT DE LA MATRICE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  CALCUL D UN SEUIL DE FILTRAGE DES TERMES DES MATRICES CALCULEES: SEUIL=10E-16*SCAL^2
    seuil=r8prem()*coef
!
    if (nvale .eq. 1) then 
      call xfem_over_write(matas1, bas1, nbnomax, ino_xfem, neq, &
                           ieq_loc, nbnoxfem, neq_mloc, maxi_ddl, iglob_ddl,&
                           nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr, &
                           size_vect_col, vect_col, adr_raw, size_vect_raw, &
                           vect_raw, is_connec, coef, filtrage, seuil)
    else
!      call xfem_matimp(matas1, 32, 'MATLAB')
      call xfem_over_write_2(matas1, bas1, nbnomax, ino_xfem, neq, &
                             ieq_loc, nbnoxfem, neq_mloc, maxi_ddl, iglob_ddl,&
                             nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr, &
                             size_vect_col, vect_col, adr_raw, size_vect_raw, &
                             vect_raw, is_connec, coef, filtrage, seuil, nvale,&
                             size_vect_col_2, vect_col_2,size_vect_raw_2, vect_raw_2)
!      call xfem_matimp(pc_1, 33, 'MATLAB')
!      call xfem_matimp(matas1, 34, 'MATLAB')
!      ASSERT(.false.)
    endif
 
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! STOCKAGE DU NOUVEL ETAT DE LA MATRICE ASTER
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
    refa(17)='XFEM_PRECOND'
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! STOCKAGE DU PRECONDITIONNEUR DANS LA MATR_ASSE / SUR LA BASE VOLATILE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    ASSERT( refa(18)(1:19) .eq. ' ' )
    refa(18)(1:19)=pc_1
    call mtdscr(pc_1)
!
! EN CAS DE SORTIE BRUTALE
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
999 continue
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
! NETTOYAGES ...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
    AS_DEALLOCATE(vl=is_xfem)
    AS_DEALLOCATE(vi=ino_xfem)
    AS_DEALLOCATE(vi=neq_mloc)
    AS_DEALLOCATE(vi=ieq_loc)
    AS_DEALLOCATE(vi=iglob_ddl)
    AS_DEALLOCATE(vr=tab_mloc)
    AS_DEALLOCATE(vi=nnz_mloc)
    AS_DEALLOCATE(vl=is_svd)
    AS_DEALLOCATE(vi=nblig_pc)
    AS_DEALLOCATE(vr=vect_col)
    AS_DEALLOCATE(vi=smhc_pc)
    AS_DEALLOCATE(vi=smhc_adr)
    AS_DEALLOCATE(vi=vect_adr)
    AS_DEALLOCATE(vi=nz_raw)
    AS_DEALLOCATE(vl=is_connec)
    AS_DEALLOCATE(vi=adr_raw)
    AS_DEALLOCATE(vr=vect_raw)
    if (nvale.eq.2) then
      AS_DEALLOCATE(vi=nblig_pc_2)
      AS_DEALLOCATE(vr=vect_col_2)
      AS_DEALLOCATE(vi=smhc_pc_2)
      AS_DEALLOCATE(vi=smhc_adr_2)
      AS_DEALLOCATE(vi=vect_adr_2)
      AS_DEALLOCATE(vi=nz_raw_2)
      AS_DEALLOCATE(vl=is_connec_2)
      AS_DEALLOCATE(vi=adr_raw_2)
      AS_DEALLOCATE(vr=vect_raw_2)
    endif
!
    call jedema()
!
end subroutine
