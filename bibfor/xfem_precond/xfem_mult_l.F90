subroutine xfem_mult_l(action, matass, nbnomax, ino_xfem, nbnoxfem,&
                       neq, ieq_loc, neq_mloc, maxi_ddl, iglob_ddl,&
                       nblig_pc, size_smhc, smhc_pc, smhc_adr, vect_adr,&
                       size_vect, deca, is_connec, nz_raw, adr_raw,&
                       cumul_kterm, vect_raw, vect_col, tab_mloc, xvalm)
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
! BUT : MULTIPLICATION A GAUCHE D UNE MATRICE ASTER < MATR_ASSE > 
!       PAR LE PRECONDITIONNEUR < TAB_MLOC >
!       ET STOCKAGE DANS UN ZR < VECT_RAW >
!      ***********************************************************
!   REMARQUES :
!     1- POUR MAITRISER L ESPACE MEMOIRE A ALLOUER (WORKING SPACE) 
!        LA MULTIPLICATION PROCEDE EN 2 PHASES:
!          * UNE ETAPE DE "COMPTAGE" QUI SIMULE TOUTES LES BOUCLES DE CALCUL
!            ET COMPTE LE NOMBRE D EMPLACEMENTS MEMOIRE NECESSAIRES
!          * UNE  ETAPE DE "REMPLISSAGE" QUI REMPLIT EFFECTIVEMENT LES EMPLACAMENTS
!            MEMOIRE APRES ALLOCATION DYNAMIQUE
!     2- LA LECTURE/ECRITURE DANS VECT_RAW DEPEND:
!          * D UN COMPTEUR DE DECALLAGE < COUNT_RAW >
!          * D UN VECTEUR D ADRESSE < ADR_RAW >
!       C EST UN STOCKAGE MORSE ANALOGUE A LA MATRICE ASTER ...
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!
!-----------------------------------------------------------------------
    implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jexnum.h"
#include "asterfort/jeveuo.h"
!-----------------------------------------------------------------------
    character(len=*) :: action
    character(len=19) :: matass
    integer :: nbnomax, nbnoxfem, maxi_ddl, neq, deca, size_smhc, cumul_kterm
    integer :: ino_xfem(nbnomax), nblig_pc(nbnoxfem), adr_raw(nbnoxfem)
    integer :: smhc_pc(size_smhc), ieq_loc(neq)
    integer :: vect_adr(nbnoxfem), smhc_adr(nbnoxfem), nz_raw(nbnoxfem)
    integer :: size_vect
    real(kind=8), optional :: vect_col(size_vect), tab_mloc(deca*nbnoxfem), vect_raw(cumul_kterm)
    integer :: neq_mloc(nbnoxfem), iglob_ddl(maxi_ddl*nbnoxfem)
    aster_logical :: is_connec(neq)
    integer, optional :: xvalm
!-----------------------------------------------------------------------
    character(len=14) :: nonu
    integer, pointer :: smdi(:) => null()
    integer, pointer :: list_no(:) => null()
    integer, pointer :: count_raw(:) => null()
    aster_logical, pointer :: is_counted(:) => null()
    integer :: jcoll, jsmhc, jdeeq, decaj, nblig, ipos, iligl, nbnox, nunoj, nuno, nunos, j
    integer :: adr_vect, ij, jadr, jvale
!-----------------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(( action .eq. 'COMPTAGE' ) .or. ( action .eq. 'REMPLISSAGE' ))
!
    call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=nonu)
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jeveuo(nonu//'.NUME.DEEQ', 'L', jdeeq)
!
    AS_ALLOCATE(vi=list_no,size=nbnoxfem)
    AS_ALLOCATE(vl=is_counted,size=nbnoxfem)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( action .eq. 'COMPTAGE' ) then    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    - PREMIERE PASSE : CALCUL DE L ESPACE <DENSE> NECESSAIRE POUR STOCKER LES PRODUITS LOCAUX
!
    nunoj=0
    do 50 jcoll=1,neq
       nunos=zi(jdeeq-1+2*(jcoll-1)+1)
       if ( nunos .gt. 0 ) nunoj=ino_xfem(nunos)
       nbnox=0
! COLONNE PAS XFEM
       if (ieq_loc(jcoll) .eq. 0) then 
          if (jcoll .gt. 1) then
             decaj=smdi(jcoll-1)
             nblig=smdi(jcoll)-smdi(jcoll-1)
          else
             decaj=0
             nblig=smdi(jcoll)
          endif
! COLONNE XFEM
       else
          decaj=vect_adr(nunoj)
          nblig=nblig_pc(nunoj)
       endif
       do 55 ipos=1,nblig
! LECTURE DU NUMERO DE LIGNE
          if (ieq_loc(jcoll) .eq. 0) then 
!             if ( abs(zr(jvale-1+decaj+ipos)) .lt. seuil) goto 55
             iligl=zi4(jsmhc-1+decaj+ipos)
          else
!             if ( abs(vect_col(decaj+ipos)) .lt. seuil) goto 55
             iligl=smhc_pc(smhc_adr(nunoj)+ipos)
          endif
! TEST BLOC XFEM
          if (ieq_loc(iligl) .gt. 0 ) then
             nuno=ino_xfem(zi(jdeeq-1+2*(iligl-1)+1))
             if ( jcoll .gt. iglob_ddl(maxi_ddl*(nuno-1)+neq_mloc(nuno))) then
                if ( .not. is_counted(nuno)) then
                   nz_raw(nuno)=nz_raw(nuno)+1
                   nbnox=nbnox+1
                   list_no(nbnox)=nuno
                   is_counted(nuno)=.true. 
                endif
             endif  
          endif
55     enddo
       if ( nbnox .ne. 0) then
          is_connec(jcoll)=.true.
          do j=1,nbnox
             is_counted(list_no(j))=.false.
          enddo
       endif
50  enddo
!
!       * CALCUL DE LA TAILLE MAXIMALE A ALLOUER
!
    cumul_kterm=0
    do j=1,nbnoxfem
       adr_raw(j)=cumul_kterm
       cumul_kterm=cumul_kterm+nz_raw(j)*neq_mloc(j)
    enddo
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    elseif ( action .eq. 'REMPLISSAGE' ) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    - DEUXIEME PASSE : PRODUITS LOCAUX : Pc^T * (K*Pc)
!
    ASSERT(cumul_kterm .gt. 0)
!
    if ( present(xvalm) )  then 
      call jeveuo(jexnum(matass//'.VALM', xvalm), 'L', jvale)
    else
      call jeveuo(jexnum(matass//'.VALM', 1), 'L', jvale)
    endif
!
    AS_ALLOCATE(vi=count_raw,size=nbnoxfem)
    adr_vect=0
    do 70 jcoll=1,neq
       if ( .not. is_connec(jcoll)) goto 70
       nunos=zi(jdeeq-1+2*(jcoll-1)+1)
       if ( nunos .gt. 0 ) nunoj=ino_xfem(nunos)
       nbnox=0
! COLONNE PAS XFEM
       if (ieq_loc(jcoll) .eq. 0) then 
          if (jcoll .gt. 1) then
             decaj=smdi(jcoll-1)
             nblig=smdi(jcoll)-smdi(jcoll-1)
          else
             decaj=0
             nblig=smdi(jcoll)
          endif
! COLONNE XFEM
       else
          decaj=vect_adr(nunoj)
          nblig=nblig_pc(nunoj)
       endif
       do 75 ipos=1,nblig
! LECTURE DU NUMERO DE LIGNE
          if (ieq_loc(jcoll) .eq. 0) then 
!             if ( zr(jvale-1+decaj+ipos) .lt. seuil) goto 75
             iligl=zi4(jsmhc-1+decaj+ipos)
          else
!             if ( vect_col(decaj+ipos) .lt. seuil) goto 75
             iligl=smhc_pc(smhc_adr(nunoj)+ipos)
          endif
! TEST BLOC XFEM
          if (ieq_loc(iligl) .gt. 0 ) then
             nuno=ino_xfem(zi(jdeeq-1+2*(iligl-1)+1))
             if ( jcoll .gt. iglob_ddl(maxi_ddl*(nuno-1)+neq_mloc(nuno))) then
                 if ( .not. is_counted(nuno)) then
                    count_raw(nuno)=count_raw(nuno)+1
                    nbnox=nbnox+1
                    list_no(nbnox)=nuno
                    is_counted(nuno)=.true.
                 endif                        
! LECTURE DU DECALAGE VECT_RAW
                 jadr=adr_raw(nuno)+(count_raw(nuno)-1)*neq_mloc(nuno)
! PRODUIT MLOC ^ T * COL
                 if(ieq_loc(jcoll) .eq. 0) then 
                    do ij=1,neq_mloc(nuno)
                      vect_raw(jadr+ij)=vect_raw(jadr+ij)+&
                             zr(jvale-1+decaj+ipos)*&
                                      tab_mloc(deca*(nuno-1)+ neq_mloc(nuno)*(ieq_loc(iligl)-1)+ij)
                    enddo
                 else
                    do ij=1,neq_mloc(nuno)
                      vect_raw(jadr+ij)=vect_raw(jadr+ij)+&
                             vect_col(decaj+neq_mloc(nunoj)*(ipos-1)+ieq_loc(jcoll))*&
                                      tab_mloc(deca*(nuno-1)+ neq_mloc(nuno)*(ieq_loc(iligl)-1)+ij)
                    enddo               
                 endif
             endif  
          endif
75     enddo
! REINITIALISATION DE L IDENTIFIFCATEUR DES NOEUDS
       do nuno=1,nbnox
          is_counted(list_no(nuno))=.false.
       enddo
70  enddo
!
    AS_DEALLOCATE(vi=count_raw)
    endif
!
    AS_DEALLOCATE(vi=list_no)
    AS_DEALLOCATE(vl=is_counted)
!
    call jedema()
!
end subroutine
