subroutine xfem_over_write(matas1, bas1, nbnomax, ino_xfem, neq, &
                           ieq_loc, nbnoxfem, neq_mloc, maxi_ddl, iglob_ddl,&
                           nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr, &
                           size_vect_col, vect_col, adr_raw, size_vect_raw, vect_raw, &
                           is_connec, coef, filtrage, seuil)
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
! BUT : REMPLACEMENT DE LA MATRICE AVEC/SANS COMPRESSION
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
#include "asterc/r8gaem.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/detrsd.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedetr.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/wkvect.h"
!-----------------------------------------------------------------------
    character(len=19) :: matas1   
    character(len=1) :: bas1
    integer :: neq, nbnoxfem, size_vect_col, size_vect_raw, nbnomax, maxi_ddl, size_smhc
    integer :: adr_raw(nbnoxfem), vect_adr(nbnoxfem)
    integer :: smhc_adr(nbnoxfem), ino_xfem(nbnomax), nblig_pc(nbnoxfem)
    integer :: neq_mloc(nbnoxfem), iglob_ddl(maxi_ddl*nbnoxfem)
    integer :: ieq_loc(neq), smhc_pc(size_smhc)
    aster_logical :: is_connec(neq)
    aster_logical :: filtrage
    real(kind=8) :: vect_col(size_vect_col), vect_raw(size_vect_raw), coef
    real(kind=8) :: seuil
!-----------------------------------------------------------------------
    character(len=19) :: matas2   
    character(len=14) :: nonu, nonu2
    character(len=1) :: kbid
    integer :: jcoll, iligl, nuno, nunoj, jdeeq, jsmhc, nblig, decaj, jadr, ipos
    integer :: nblig2, iadr, iadr2, ij, jvale, jsmde, jsmdi, jsmhc2, jval2
    integer :: incr, nbnox, nnz, nunos
    integer, pointer :: smdi(:) => null(), smdi2(:) => null()
    integer, pointer :: count_raw(:) => null(), nblig_mat(:) => null(), list_no(:) => null()
    aster_logical, pointer :: is_counted(:) => null()
!-----------------------------------------------------------------------
!
    call jemarq()
!
    if (.not. filtrage) seuil=-1.d0/r8gaem()
    if (filtrage) ASSERT( seuil .gt. 0.d0 )
!    if( present(filtrage) ) ASSERT( present(seuil) )
!    if (.not. present(filtrage)) ASSERT( .not. present(seuil) )
!
    call dismoi('NOM_NUME_DDL', matas1, 'MATR_ASSE', repk=nonu)
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(nonu//'.NUME.DEEQ', 'L', jdeeq)
!
    AS_ALLOCATE(vi=count_raw,size=nbnoxfem)
    AS_ALLOCATE(vi=list_no,size=nbnoxfem)
    AS_ALLOCATE(vl=is_counted,size=nbnoxfem)
    AS_ALLOCATE(vi=nblig_mat,size=neq)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    - PREMIERE PASSE : CALCUL DU NOMBRE D EMPLACEMENTS NECASSAIRES
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    nunoj=0
    do 80 jcoll=1,neq
       nunos=zi(jdeeq-1+2*(jcoll-1)+1)
       if ( nunos .gt. 0 ) nunoj=ino_xfem(nunos)
       if ( .not. is_connec(jcoll) .and. ieq_loc(jcoll) .eq. 0 ) then
          if (jcoll .gt. 1) then
             nblig=smdi(jcoll)-smdi(jcoll-1)
          else
             nblig=smdi(jcoll)
          endif
          nblig_mat(jcoll)=nblig
!
       elseif ( .not. is_connec(jcoll) .and. ieq_loc(jcoll) .ne. 0 ) then
          decaj=vect_adr(nunoj)          
          nblig=nblig_pc(nunoj)
! LECTURE DE VECT_COL : BOUCLE SUR LES LIGNES DIFFERENTES DE LA DIAGONALE
          do 82 ipos=1,nblig
             if (abs(vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll))) .gt. seuil) then
                nblig_mat(jcoll)=nblig_mat(jcoll)+1
             endif
82        enddo
! NE PAS OUBLIER LE COEFFICIENT DIAGONAL
          if (filtrage) then
             nblig_mat(jcoll)=nblig_mat(jcoll)+1
          else
             nblig_mat(jcoll)=nblig_mat(jcoll)+ieq_loc(jcoll) 
          endif    
!
       elseif ( is_connec(jcoll) .and. ieq_loc(jcoll) .eq. 0 ) then
          if (jcoll .gt. 1) then
             nblig=smdi(jcoll)-smdi(jcoll-1)
             decaj=smdi(jcoll-1)
          else
             nblig=smdi(jcoll)
             decaj=0
          endif
          nbnox=0
          do ipos=1,nblig
             iligl=zi4(jsmhc-1+decaj+ipos)
             if (ieq_loc(iligl) .eq. 0) then 
                nblig_mat(jcoll)=nblig_mat(jcoll)+1
             else
                nuno=ino_xfem(zi(jdeeq-1+2*(iligl-1)+1))      
                if ( .not. is_counted(nuno)) then
                   count_raw(nuno)=count_raw(nuno)+1
                   jadr=adr_raw(nuno)+(count_raw(nuno)-1)*neq_mloc(nuno)
                   nbnox=nbnox+1
                   list_no(nbnox)=nuno
                   is_counted(nuno)=.true.
                   do ij=1,neq_mloc(nuno)
                      if ( abs(vect_raw(jadr+ij)) .gt. seuil ) nblig_mat(jcoll)=nblig_mat(jcoll)+1
                   enddo
                endif
             endif
          enddo
!  REINITIALISATION DE L IDENTIFIFCATEUR DES NOEUDS
          do nuno=1,nbnox
             is_counted(list_no(nuno))=.false.
          enddo
!
       elseif ( is_connec(jcoll) .and. ieq_loc(jcoll) .ne. 0 ) then
          nblig=nblig_pc(nunoj)
          nbnox=0
          decaj=vect_adr(nunoj)
          do 84 ipos=1,nblig
             iligl=smhc_pc(smhc_adr(nunoj)+ipos)
             if (ieq_loc(iligl) .eq. 0) then 
                if (abs(vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll))) .gt. seuil) then 
                   nblig_mat(jcoll)=nblig_mat(jcoll)+1
                endif
             else
                nuno=ino_xfem(zi(jdeeq-1+2*(iligl-1)+1))      
                if ( .not. is_counted(nuno)) then
                   count_raw(nuno)=count_raw(nuno)+1
                   jadr=adr_raw(nuno)+(count_raw(nuno)-1)*neq_mloc(nuno)
                   nbnox=nbnox+1
                   list_no(nbnox)=nuno
                   is_counted(nuno)=.true.
                   do ij=1,neq_mloc(nuno)
                      if ( abs(vect_raw(jadr+ij)) .gt. seuil ) nblig_mat(jcoll)=nblig_mat(jcoll)+1
                   enddo
                endif
             endif
84        enddo
! NE PAS OUBLIER LE COEFFICIENT DIAGONAL
          if (filtrage) then
             nblig_mat(jcoll)=nblig_mat(jcoll)+1
          else
             nblig_mat(jcoll)=nblig_mat(jcoll)+ieq_loc(jcoll) 
          endif 
! REINITIALISATION DE L IDENTIFIFCATEUR DES NOEUDS
          do nuno=1,nbnox
             is_counted(list_no(nuno))=.false.
          enddo
       endif   
80  enddo 
!
    nnz=0
    do jcoll=1,neq
       nnz=nnz+nblig_mat(jcoll)
    enddo
!
!  REINITIALISATION DU COMPTEUR SUIVANT VECT_RAW
    do nuno=1,nbnoxfem
       count_raw(nuno)=0
    enddo
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    - DEUXIEME PASSE : ALLOCATION ET ECRITURE MATRASS
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!     -- allocation de MATAS1.VALM :
!     ------------------------------
!   DUPLICATIONS DES SD
    call gcncon('_', matas2)
    call copisd('MATR_ASSE', bas1, matas1, matas2)
    call gcncon('_', nonu2)
    call copisd('NUME_DDL', bas1, nonu, nonu2)
!
    call jeveuo(jexnum(matas2//'.VALM', 1), 'E', jval2)
!
!   ALLOCATIONS DE LA NOUVELLE DIMENSION (NNZ) 
!   - ALLOCATION DU .VALM
    call jedetr(matas1//'.VALM')
    call jecrec(matas1//'.VALM', bas1//' V R', 'NU', 'DISPERSE', 'CONSTANT', 1)
    call jecroc(jexnum(matas1//'.VALM', 1))
    call jeecra(matas1//'.VALM', 'LONMAX', nnz, kbid)
    call jeveuo(jexnum(matas1//'.VALM', 1), 'E', jvale)
!    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
!    nonu=refa(2)    
!   - ALLOCATION DU .SMOS.SMDI
    call jedetr(nonu//'.SMOS.SMDI')
    call wkvect(nonu//'.SMOS.SMDI', bas1//' V I', neq, jsmdi)
    jadr=0
    do iligl=1,neq
       jadr=jadr+nblig_mat(iligl)
       zi(jsmdi-1+iligl)=jadr
    enddo
!   - ALLOCATION DU .SMOS.SMHC
    call jedetr(nonu//'.SMOS.SMHC')
    call wkvect(nonu//'.SMOS.SMHC', bas1//' V S', nnz, jsmhc)
!   - ALLOCATION DU .SMOS.SMDE
    call jedetr(nonu//'.SMOS.SMDE')
    call wkvect(nonu//'.SMOS.SMDE', bas1//' V I', 3, jsmde)
    zi(jsmde-1+1)=neq
    zi(jsmde-1+2)=nnz
    zi(jsmde-1+3)=1
!   - PAR PRECAUION DESTR MLTF.ADNT POUR LA MULTI_FRONTALE
!    call jeexin(nonu//'.MLTF.ADNT', iret)
!    if (iret .gt. 0) call jedetr(nonu//'.MLTF.ADNT')
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nonu2//'.SMOS.SMDI', 'L', vi=smdi2)
    call jeveuo(nonu2//'.SMOS.SMHC', 'L', jsmhc2)
!
!     -- COPIE de MORCEAUX DE MATRICES => MATAS1.VALM :
!     -----------------------------------------------
!
    nunoj=0
    do 90 jcoll=1,neq
       nunos=zi(jdeeq-1+2*(jcoll-1)+1)
       if ( nunos .gt. 0 ) nunoj=ino_xfem(nunos)
       if ( .not. is_connec(jcoll) .and. ieq_loc(jcoll) .eq. 0 ) then
          if (jcoll .gt. 1) then
             iadr=smdi(jcoll-1)
             iadr2=smdi2(jcoll-1)
             nblig2=smdi2(jcoll)-smdi2(jcoll-1)
          else
             iadr=0
             iadr2=0
             nblig2=smdi2(jcoll)
          endif
          do ipos=1,nblig2
             zi4(jsmhc-1+iadr+ipos)=zi4(jsmhc2-1+iadr2+ipos)
             zr(jvale-1+iadr+ipos)=zr(jval2-1+iadr2+ipos)
          enddo
          incr=nblig2
!
       elseif ( .not. is_connec(jcoll) .and. ieq_loc(jcoll) .ne. 0 ) then
          decaj=vect_adr(nunoj)
          if (jcoll .gt. 1) then
             iadr=smdi(jcoll-1)
          else
             iadr=0
          endif
          nblig2=nblig_pc(nunoj)
! LES COEFFICIENTS HORS DIAGONAUX
          incr=0
          do 92 ipos=1,nblig2
             if (abs(vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll))) .gt. seuil) then
                incr=incr+1
                zi4(jsmhc-1+iadr+incr)=smhc_pc(smhc_adr(nunoj)+ipos)
                zr(jvale-1+iadr+incr)=vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll))
             endif
92        enddo
! LE COEFFICIENT DIAGONAL
          if (filtrage) then
             incr=incr+1
             zi4(jsmhc-1+iadr+incr)=jcoll
             zr(jvale-1+iadr+incr)=coef
          else
             do ij=1,(ieq_loc(jcoll)-1)
                incr=incr+1
                zi4(jsmhc-1+iadr+incr)=iglob_ddl(maxi_ddl*(nunoj-1)+ij)
                zr(jvale-1+iadr+incr)=0.d0
             enddo
             incr=incr+1
             zi4(jsmhc-1+iadr+incr)=jcoll
             zr(jvale-1+iadr+incr)=coef
          endif 
!
       elseif ( is_connec(jcoll) .and. ieq_loc(jcoll) .eq. 0 ) then
          if (jcoll .gt. 1) then
             iadr=smdi(jcoll-1)
             iadr2=smdi2(jcoll-1)
             nblig2=smdi2(jcoll)-smdi2(jcoll-1)
          else
             iadr=0
             iadr2=0
             nblig2=smdi2(jcoll)
          endif
          nbnox=0
          incr=0
          do ipos=1,nblig2
             iligl=zi4(jsmhc2-1+iadr2+ipos)
             if (ieq_loc(iligl) .eq. 0) then 
                incr=incr+1
                zi4(jsmhc-1+iadr+incr)=zi4(jsmhc2-1+iadr2+ipos)
                zr(jvale-1+iadr+incr)=zr(jval2-1+iadr2+ipos)
             else
                nuno=ino_xfem(zi(jdeeq-1+2*(iligl-1)+1))      
                if ( .not. is_counted(nuno)) then
                   count_raw(nuno)=count_raw(nuno)+1
                   jadr=adr_raw(nuno)+(count_raw(nuno)-1)*neq_mloc(nuno)
                   nbnox=nbnox+1
                   list_no(nbnox)=nuno
                   is_counted(nuno)=.true.
                   do ij=1,neq_mloc(nuno)
                      if ( abs(vect_raw(jadr+ij)) .gt. seuil ) then
                         incr=incr+1
                         zi4(jsmhc-1+iadr+incr)=iglob_ddl(maxi_ddl*(nuno-1)+ij)
                         zr(jvale-1+iadr+incr)=vect_raw(jadr+ij)
                      endif
                   enddo
                endif
             endif
          enddo
! REINITIALISATION DE L IDENTIFIFCATEUR DES NOEUDS
          do nuno=1,nbnox
             is_counted(list_no(nuno))=.false.
          enddo
!
       elseif ( is_connec(jcoll) .and. ieq_loc(jcoll) .ne. 0 ) then
          if (jcoll .gt. 1) then
             iadr=smdi(jcoll-1)
          else
             iadr=0
          endif
          decaj=vect_adr(nunoj)
          nblig2=nblig_pc(nunoj)
          nbnox=0
          incr=0
          do 94 ipos=1,nblig2
             iligl=smhc_pc(smhc_adr(nunoj)+ipos)
             if (ieq_loc(iligl) .eq. 0) then
                if (abs(vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll))) .gt. seuil) then
                   incr=incr+1
                   zi4(jsmhc-1+iadr+incr)=smhc_pc(smhc_adr(nunoj)+ipos)
                   zr(jvale-1+iadr+incr)=vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll)) 
                endif
             else
                nuno=ino_xfem(zi(jdeeq-1+2*(iligl-1)+1))      
                if ( .not. is_counted(nuno)) then
                   count_raw(nuno)=count_raw(nuno)+1
                   jadr=adr_raw(nuno)+(count_raw(nuno)-1)*neq_mloc(nuno)
                   nbnox=nbnox+1
                   list_no(nbnox)=nuno
                   is_counted(nuno)=.true.
                   do ij=1,neq_mloc(nuno)
                      if ( abs(vect_raw(jadr+ij)) .gt. seuil ) then
                         incr=incr+1
                         zi4(jsmhc-1+iadr+incr)=iglob_ddl(maxi_ddl*(nuno-1)+ij)
                         zr(jvale-1+iadr+incr)=vect_raw(jadr+ij)
                      endif                           
                   enddo
                endif
             endif
94        enddo
! NE PAS OUBLIER LE COEFFICIENT DIAGONAL
          if (filtrage) then
             incr=incr+1
             zi4(jsmhc-1+iadr+incr)=jcoll
             zr(jvale-1+iadr+incr)=coef
          else
             do ij=1,(ieq_loc(jcoll)-1)
                incr=incr+1
                zi4(jsmhc-1+iadr+incr)=iglob_ddl(maxi_ddl*(nunoj-1)+ij)
                zr(jvale-1+iadr+incr)=0.d0
             enddo
             incr=incr+1
             zi4(jsmhc-1+iadr+incr)=jcoll
             zr(jvale-1+iadr+incr)=coef
          endif         
! REINITIALISATION DE L IDENTIFIFCATEUR DES NOEUDS
          do nuno=1,nbnox
             is_counted(list_no(nuno))=.false.
          enddo
       endif   
! ULTIME VERIFICATION
       ASSERT( incr .eq. nblig_mat(jcoll))
90  enddo
!
    call detrsd('MATR_ASSE', matas2)
    call detrsd('NUME_DDL', nonu2)
!
    AS_DEALLOCATE(vi=list_no)
    AS_DEALLOCATE(vl=is_counted)
    AS_DEALLOCATE(vi=count_raw)
    AS_DEALLOCATE(vi=nblig_mat)
!
    call jedema()
!
end subroutine
