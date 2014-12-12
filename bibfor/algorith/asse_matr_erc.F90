subroutine asse_matr_erc(nom_matr_erc,nom_nume_erc,dynam1,dynam2,matprod)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
! ----------------------------------------------------------------------
!
!  ROUTINE LIEE A L'OPERATEUR CALC_ERC_DYN
!
!  REMPLISSAGE DE LA MATRICE ERC EN MODALE POUR UNE FREQUENCE DONNEE
! ----------------------------------------------------------------------
! IN  : SOLVEU        : NOM DE L'OBJET JEVEUX CONTENANT LES INFORMATIONS
!                       DU SOLVEUR UTILISE POUR LA RESOLUTION DU PB
! IN  : NOM_MATR_ERC  : NOM DE L'OBJET JEVEUX DE LA MATRICE CREE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! IN  : NOM_NUME_ERC  : NOM DE L'OBJET JEVEUX DU NUME_DDL CREE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! IN  : DYNAM1        : NOM DE L'OBJET JEVEUX CONTENANT LA PREMIERE MATRICE
!                       DYNAMIQUE DU BLOC DIAGONAL DE LA MATRICE D'ERC
! IN  : DYNAM2        : NOM DE L'OBJET JEVEUX CONTENANT LA DEUXIEME MATRICE
!                       DYNAMIQUE DU BLOC HORS-DIAGONAL DE LA MATRICE D'ERC
! IN  : MATPROD       : LISTE DES NOMS DES OBJETS JEVEUX OU EST STOCKE LE 
!                       SOUS-BLOC CALCULE. LE STOCKAGE EST EN MORSE SELON LA 
!                       SD_NUME_DDL. (.SMDE,.SMHC,.SMDI,.VALM)
! ----------------------------------------------------------------------
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "blas/dcopy.h"
!
    character(len=14),intent(in) :: nom_nume_erc
    character(len=19),intent(in) :: dynam1,dynam2,nom_matr_erc
    character(len=24),intent(in) :: matprod(4)
    integer :: i_mprod_smde,i_mprod_smhc,i_mprod_smdi,i_mprod_valm,i_nume_smde,i_nume_smdi
    integer :: i_nume_smhc,mrefa,i_dynam1_valm,i_dynam2_valm,i_materc_valm,neq_tot,neq_bloc
    integer :: nonzero_tot,nzfirstblk,tt,ll,jj,kk,cumul_non_zero,non_zero_impe
    integer :: non_zero_matprod,hors_diag_impe,hd_matpro,nz_colncour
!
! -- RECUPERATION DES INFOS DES DIFFEERENTES MATRICES
!
   call jeveuo(matprod(1), 'L', i_mprod_smde)
   call jeveuo(matprod(2), 'L', i_mprod_smhc)
   call jeveuo(matprod(3), 'L', i_mprod_smdi)
   call jeveuo(matprod(4), 'L', i_mprod_valm)
!
   call jeveuo(nom_nume_erc//'.SMOS.SMDE', 'L',i_nume_smde)
   call jeveuo(nom_nume_erc//'.SMOS.SMDI', 'L',i_nume_smdi)
   call jeveuo(nom_nume_erc//'.SMOS.SMHC', 'L',i_nume_smhc)
!
   call jeveuo(jexnum(dynam1//'.VALM', 1), 'L', i_dynam1_valm)
   call jeveuo(jexnum(dynam2//'.VALM', 1), 'L', i_dynam2_valm)
!
   call jeveuo(jexnum(nom_matr_erc//'.VALM', 1), 'E', i_materc_valm)

! --- RECUPERATION D'INFOS DIVERSES
   neq_tot=zi(i_nume_smde-1+1)
   neq_bloc=neq_tot/2
   nonzero_tot=zi(i_nume_smde-1+2)
   nzfirstblk=zi(i_nume_smdi-1+neq_bloc)

! --- REMISE A JOUR DU .REFA AU CAS OU LA MATRICE AVAIT DEJA ETE FACTORISEE
    call jeveuo(nom_matr_erc//'.REFA', 'E', mrefa)
    zk24(mrefa-1+8)=''

! 
! --- RECOPIE DES VALEURS DU PREMIER BLOC
!     
   call dcopy(nzfirstblk,zr(i_dynam1_valm),1,zr(i_materc_valm),1)
!
!
! --- --- REMPLISSAGE DEUX DEUX AUTRES BLOCS
!
   cumul_non_zero=0
   non_zero_impe=0
   non_zero_matprod=0

   do jj=1,neq_bloc
     hors_diag_impe=zi(i_nume_smdi+jj-1)-non_zero_impe-1
     hd_matpro=zi(i_mprod_smdi+jj-1)-non_zero_matprod-1
! --- --- BLOC TRIANGULAIRE SUP IMPEDANCE 
     nz_colncour=0
     do tt=1,hors_diag_impe+1
      zr(i_materc_valm+nzfirstblk+cumul_non_zero+tt-1)= &
                                        zr(i_dynam2_valm+zi(i_nume_smdi+jj-1)-1-hors_diag_impe+tt-1)
      nz_colncour=nz_colncour+1
      non_zero_impe=non_zero_impe+1
     end do

! --- --- BLOC TRIANGULAIRE INF IMPEDANCE
      ! boucle sur les colonnes superieures
      do kk=jj+1,neq_bloc 
      if (jj.ne.neq_bloc) then
         ! boucle sur les elements non nuls de la colonne superieure en cours
         do ll=zi(i_nume_smdi+kk-2)+1,zi(i_nume_smdi+kk-1) 
            
            if (zi4(i_nume_smhc+ll-1).gt.jj) goto 111
            ! on test si on tombe sur le numero de file correspondant a la colonne reelle en cours
            if (zi4(i_nume_smhc+ll-1).eq.jj) then  
              nz_colncour=nz_colncour+1
              zr(i_materc_valm-1+nzfirstblk+cumul_non_zero+nz_colncour)= &
                                                                              zr(i_dynam2_valm+ll-1)

              goto 111
            end if
         end do
         ! boucle sur les elements non nuls de la colonne superieure en cours
111     continue
      end if       
      end do
      ! boucle sur les colonnes superieures

! --- --- BLOC MATRICE D'OBS

      do kk=1,hd_matpro+1
         nz_colncour=nz_colncour+1
         non_zero_matprod=non_zero_matprod+1
         zr(i_materc_valm-1+nzfirstblk+cumul_non_zero+nz_colncour)= &
                                                                 zr(i_mprod_valm-1+non_zero_matprod)
      end do
      cumul_non_zero=cumul_non_zero+nz_colncour

    end do  

end subroutine