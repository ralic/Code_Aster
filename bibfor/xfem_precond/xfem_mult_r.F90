subroutine xfem_mult_r(action, matass, maxi_ddl, iglob_ddl, nbnoxfem,&
                       neq_mloc, nblig_pc, cumul_ilig, cumul_kterm, deca,&
                       tab_mloc, smhc_pc, smhc_adr, vect_adr, vect_col)
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
! BUT : ACTION DU PRE-CONDITIONNEUR A DROITE
!-----------------------------------------------------------------------
!
! ARGUMENTS :
!------------
!
!-----------------------------------------------------------------------
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jexnum.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/veiini.h"
!-----------------------------------------------------------------------
    character(len=*) :: action
    character(len=19) :: matass
    integer :: maxi_ddl, nbnoxfem, cumul_kterm, cumul_ilig, deca
    integer :: nblig_pc(nbnoxfem)
    integer :: neq_mloc(nbnoxfem), iglob_ddl(maxi_ddl*nbnoxfem)
    integer, optional :: smhc_pc(cumul_ilig)
    integer, optional :: smhc_adr(nbnoxfem), vect_adr(nbnoxfem)
    real(kind=8), optional :: vect_col(cumul_kterm), tab_mloc(deca*nbnoxfem)
!-----------------------------------------------------------------------
    character(len=14) :: nonu
    integer :: ddlmax
    parameter (ddlmax=21)
    integer :: count_col(ddlmax), list_min(ddlmax), nbmin, ladr(ddlmax), incr
    integer :: min_ilig
    integer :: list_cpt(ddlmax), nbcpt, max_cpt(ddlmax), j, cpt
    integer :: adr_smhc, adr_smdi, adr_vect, jadr, jvale, k, nm, ij,jsmhc 
    integer, pointer :: smdi(:) => null()
!-----------------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(( action .eq. 'COMPTAGE' ) .or. ( action .eq. 'REMPLISSAGE' ))
!
    call dismoi('NOM_NUME_DDL', matass, 'MATR_ASSE', repk=nonu)
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    if ( action .eq. 'COMPTAGE' ) then    
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    - PREMIERE PASSE : CALCUL DE L ESPACE <DENSE> NECESSAIRE POUR STOCKER LES PRODUITS LOCAUX
!
    call veiini(ddlmax,0,count_col)
    call veiini(ddlmax,0,ladr)
    call veiini(ddlmax,0,list_cpt)
    call veiini(ddlmax,0,max_cpt)
    call veiini(ddlmax,0,list_min)
    do 40 j=1,nbnoxfem
!      * INITILISATION DU COMPTEUR DE SMHC
       nbcpt=neq_mloc(j)
       do cpt=1,nbcpt
          count_col(cpt)=1
          if (iglob_ddl(maxi_ddl*(j-1)+cpt) .gt. 1) then
            ladr(cpt)=smdi(iglob_ddl(maxi_ddl*(j-1)+cpt)-1)
            max_cpt(cpt)=smdi(iglob_ddl(maxi_ddl*(j-1)+cpt))-smdi(iglob_ddl(maxi_ddl*(j-1)+cpt)-1)
          else
            ladr(cpt)=0
            max_cpt(cpt)=smdi(iglob_ddl(maxi_ddl*(j-1)+cpt))
          endif         
          list_min(cpt)=0
          list_cpt(cpt)=cpt         
       enddo
41     continue
       min_ilig=zi4(jsmhc-1+ladr(list_cpt(1))+count_col(list_cpt(1)))
       nbmin=0
       do cpt=2,nbcpt
          min_ilig=min(min_ilig,int(zi4(jsmhc-1+ladr(list_cpt(cpt))+count_col(list_cpt(cpt)))))
       enddo
       do cpt=1,nbcpt
          if (zi4(jsmhc-1+ladr(list_cpt(cpt))+count_col(list_cpt(cpt))) .eq. min_ilig) then
             nbmin=nbmin+1
             list_min(nbmin)=list_cpt(cpt)
          endif
       enddo
       if (min_ilig .ge.  iglob_ddl(maxi_ddl*(j-1)+1)) goto 40
       nblig_pc(j)=nblig_pc(j)+1
       ASSERT(nbmin .gt. 0)
       do k=1,nbmin
          cpt=list_min(k)
          if (count_col(cpt) .lt. max_cpt(cpt)) then
              count_col(cpt)=count_col(cpt)+1
          else
              nbcpt=nbcpt-1
              if ( nbcpt .eq. 0) goto 40
              list_cpt(1:nbcpt)=pack(list_cpt(1:(nbcpt+1)), list_cpt(1:(nbcpt+1)) .ne. cpt)
          endif
       enddo
       if (nbcpt .ne. 0) goto 41
       ASSERT(.false.)
40  enddo
!
!       * CALCUL DE LA TAILLE MAXIMALE A ALLOUER
!
    cumul_kterm=0
    cumul_ilig=0
    do j=1,nbnoxfem
       cumul_ilig=cumul_ilig+nblig_pc(j)
       cumul_kterm=cumul_kterm+nblig_pc(j)*neq_mloc(j)
    enddo
!
    ASSERT(cumul_ilig .gt. 0)
    ASSERT(cumul_kterm .gt. 0)
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    elseif ( action .eq. 'REMPLISSAGE' ) then
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!    - DEUXIEME PASSE : PRODUITS LOCAUX : K * Pc  
!
    ASSERT(cumul_ilig .gt. 0)
    ASSERT(cumul_kterm .gt. 0)
!
    call jeveuo(jexnum(matass//'.VALM', 1), 'L', jvale)
!
    call veiini(ddlmax,0,count_col)
    call veiini(ddlmax,0,ladr)
    call veiini(ddlmax,0,list_cpt)
    call veiini(ddlmax,0,max_cpt)
    call veiini(ddlmax,0,list_min)
    adr_smhc=0
    adr_smdi=0
    adr_vect=0
    do 50 j=1,nbnoxfem
!   INITILISATION DU COMPTEUR DE SMHC
       smhc_adr(j)=adr_smhc
       vect_adr(j)=adr_vect
       incr=0
       nbcpt=neq_mloc(j)
       do cpt=1,nbcpt
          count_col(cpt)=1
          if (iglob_ddl(maxi_ddl*(j-1)+cpt) .gt. 1) then
            ladr(cpt)=smdi(iglob_ddl(maxi_ddl*(j-1)+cpt)-1)
            max_cpt(cpt)=smdi(iglob_ddl(maxi_ddl*(j-1)+cpt))-smdi(iglob_ddl(maxi_ddl*(j-1)+cpt)-1)
          else
            ladr(cpt)=0
            max_cpt(cpt)=smdi(iglob_ddl(maxi_ddl*(j-1)+cpt))
          endif         
          list_min(cpt)=0
          list_cpt(cpt)=cpt         
       enddo
51     continue
       min_ilig=zi4(jsmhc-1+ladr(list_cpt(1))+count_col(list_cpt(1)))
       nbmin=0
       do cpt=2,nbcpt
          min_ilig=min(min_ilig,int(zi4(jsmhc-1+ladr(list_cpt(cpt))+count_col(list_cpt(cpt)))))
       enddo
       do cpt=1,nbcpt
          if (zi4(jsmhc-1+ladr(list_cpt(cpt))+count_col(list_cpt(cpt))) .eq. min_ilig) then
             nbmin=nbmin+1
             list_min(nbmin)=list_cpt(cpt)
          endif
       enddo
       if (min_ilig .ge.  iglob_ddl(maxi_ddl*(j-1)+1)) then 
          adr_smhc=adr_smhc+nblig_pc(j)
          adr_vect=adr_vect+nblig_pc(j)*neq_mloc(j)
          goto 50
       endif
       incr=incr+1
       nm=neq_mloc(j)
       smhc_pc(smhc_adr(j)+incr)=min_ilig
       ASSERT(nbmin .gt. 0)
       do k=1,nbmin
          cpt=list_min(k)
          jadr=adr_vect+nm*(incr-1)
          do ij=1,nm
               vect_col(jadr+ij)=vect_col(jadr+ij)+zr(jvale-1+ladr(cpt)+count_col(cpt))*&
                                        tab_mloc(deca*(j-1)+ nm*(cpt-1)+ij)
          enddo
          if (count_col(cpt) .lt. max_cpt(cpt)) then
              count_col(cpt)=count_col(cpt)+1
          else
              nbcpt=nbcpt-1
              if ( nbcpt .eq. 0) goto 50
              list_cpt(1:nbcpt)=pack(list_cpt(1:(nbcpt+1)), list_cpt(1:(nbcpt+1)) .ne. cpt)
          endif
       enddo
       if (nbcpt .ne. 0) goto 51
       ASSERT(.false.)
50  enddo 
!
    endif
!
!
    call jedema()
!
end subroutine
