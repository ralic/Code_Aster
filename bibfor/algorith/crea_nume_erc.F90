subroutine crea_nume_erc(baseno, numnu,matprod,nom_nume_erc,nom_matr_erc,nom_vect_erc,solveu,valei)
!
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
    implicit none
!
! ----------------------------------------------------------------------
!
!  ROUTINE LIEE A L'OPERATEUR CALC_ERC_DYN
!
!  CREATION D'UN NOVEAU NUME_DDL_GENE ASSOCIE A LA MATRICE D'ERC 
!  EN RESOLUTION MODALE (CONSTRUCTION D'UN PROBLEME DE TAILLE 2*N)
! ----------------------------------------------------------------------
! IN  : BASENO        : NOM COMMUN POUR LES OBJETS JEVEUX A CREER
! IN  : MATRIG        : NOM DE LA MATRICE DE RIGIDITE
! IN  : NUMNU         : NOM DU NUMEDDL DU MODELE M,C,K
! IN  : MATPROD       : LISTE DES NOMS DES OBJETS JEVEUX OU EST STOCKE LE SOUS-
!                       BLOC CALCULE. LE STOCKAGE EST EN MORSE SELON LA 
!                       SD_NUME_DDL. (.SMDE,.SMHC,.SMDI,.VALM)
! OUT : NOM_NUME_ERC  : NOM DE L'OBJET JEVEUX DU NUME_DDL CREE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! OUT : NOM_MATR_ERC  : NOM DE L'OBJET JEVEUX DE LA MATRICE CREE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! OUT : NOM_VECT_ERC  : NOM DE L'OBJET JEVEUX DU SEGOND MEMBRE ASSOCIE
!                       AU PB MATRICIEL D'ERC
! OUT : SOLVEU        : NOM DE L'OBJET JEVEUX CONTENANT LES INFORMATIONS
!                       DU SOLVEUR UTILISE POUR LA RESOLUTION DU PB
! ----------------------------------------------------------------------!
#include "jeveux.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jecreo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecroc.h"
#include "asterfort/jecrec.h"
#include "asterfort/jenonu.h"
#include "asterfort/wkvect.h"
#include "asterfort/cresol.h"
#include "asterfort/jexnum.h"
#include "asterfort/jexnom.h"
#include "asterfort/r8inir.h"
!
    character(len=8),intent(in) ::  numnu,baseno
    character(len=24),intent(in) ::matprod(4)
    character(len=14),intent(out) :: nom_nume_erc
    character(len=19),intent(out) :: nom_matr_erc,nom_vect_erc,solveu
    integer,intent(out) :: valei(8)
    character(len=8) ::  k8bid
    character(len=19) :: prgene
    integer :: ismde,ismdi,ismhc,improdsmde,improdsmhc,improdsmdi,improdvalm,neq,nozero,inewsmde
    integer :: inewsmdi,inewsmhc,cumul_non_zero,non_zero_impe,non_zero_matprod,hors_diag_impe
    integer :: hd_matprod,nz_colncour,ii,jj,kk,tt,lddesc,ldnequ,k,jrefn
    integer :: lddeeq,lddelg,ldnueq,ibid,ldprno,ldorig,mrefa,mdesc,vdesc,mvale,vvale,ll
!
! --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
!
    nom_nume_erc=baseno//'N.NUME'
! --- RECUPERATION DES INFOS NECESSAIRES A LA CREATION DU NUME_DDL_GENE
   call jeveuo(numnu//'      .SMOS.SMDE', 'L', ismde)
   call jeveuo(numnu//'      .SMOS.SMDI', 'L', ismdi)
   call jeveuo(numnu//'      .SMOS.SMHC', 'L', ismhc)
!  
   call jeveuo(matprod(1), 'L', improdsmde)
   call jeveuo(matprod(2), 'L', improdsmhc)
   call jeveuo(matprod(3), 'L', improdsmdi)
   call jeveuo(matprod(4), 'L', improdvalm)
!
   neq=zi(ismde)
   nozero=zi(ismde+1)+2*(zi(ismde+1)-neq)+neq+zi(improdsmde+1)

! --- REMPLISSAGE DU NUME_DDL_GENE ASSOCIE A LA MATRICE ERC
!
! --- SOLVEUR
    solveu=nom_nume_erc//'.SOLV'
    call cresol(solveu)
! --- CREATION DU PROF_GENE
    prgene=nom_nume_erc//'.NUME'
! --- DESC
    call wkvect(prgene//'.DESC', 'G V I', 1, lddesc)
    zi(lddesc)=2
! --- NEQU
    call wkvect(prgene//'.NEQU', 'G V I', 1, ldnequ)
    zi(ldnequ)=2*neq
! --- REFN
    call wkvect(prgene//'.REFN', 'G V K24', 4, jrefn)
    zk24(jrefn+1)='DEPL_R'
! --- DEEQ
    call wkvect(prgene//'.DEEQ', 'G V I', 4*neq, lddeeq)
    do k = 1, 2*neq
        zi(lddeeq-1+(k-1)*2+1)=k
        zi(lddeeq-1+(k-1)*2+2)=1
    end do
! --- DELG --> cet objet est apparamment inutile. On peut le supprimer?
    call wkvect(prgene//'.DELG', 'G V I', 2*neq, lddelg)
! --- LILI
    call jecreo(prgene//'.LILI', 'G N K8')
    call jeecra(prgene//'.LILI', 'NOMMAX', 2, k8bid)
    call jecroc(jexnom(prgene//'.LILI', '&SOUSSTR'))
    call jecroc(jexnom(prgene//'.LILI', 'LIAISONS'))
! --- NUEQ
    call wkvect(prgene//'.NUEQ', 'G V I', 2*neq, ldnueq)
    do k = 1, 2*neq
        zi(ldnueq-1+k)=k
    end do
! --- PRNO
    call jecrec(prgene//'.PRNO', 'G V I', 'NU', 'DISPERSE', 'VARIABLE',2)
    call jecrec(prgene//'.ORIG', 'G V I', 'NU', 'DISPERSE', 'VARIABLE',2)
    call jenonu(jexnom(prgene//'.LILI', '&SOUSSTR'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprno)
    zi(ldprno-1+1)=1
    zi(ldprno-1+2)=2*neq
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorig)
    zi(ldorig-1+1)=1
    zi(ldorig-1+2)=0
    call jenonu(jexnom(prgene//'.LILI', 'LIAISONS'), ibid)
    call jeecra(jexnum(prgene//'.PRNO', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.PRNO', ibid), 'E', ldprno)
    zi(ldprno-1+1)=0
    zi(ldprno-1+2)=0
    call jeecra(jexnum(prgene//'.ORIG', ibid), 'LONMAX', 2, ' ')
    call jeveuo(jexnum(prgene//'.ORIG', ibid), 'E', ldorig)
    zi(ldorig-1+1)=1
    zi(ldorig-1+2)=1
!
! --- REMPLISSAGE DU SMOS
!
! --- --- CALCUL DU NOMBRE DE VALEURS STOCKES DANS LA DEMIE MATRICE SUPERIEURE

   call wkvect(nom_nume_erc//'.SMOS.SMDE','G V I', 3, inewsmde)
   zi(inewsmde)= 2*neq
   zi(inewsmde+1)= nozero
   zi(inewsmde+2)= 1
   call wkvect(nom_nume_erc//'.SMOS.SMDI','G V I', 2*neq, inewsmdi)
   call wkvect(nom_nume_erc//'.SMOS.SMHC','G V S', nozero, inewsmhc)   
!
! ----- LE PREMIER BLOC DE LA MATRICE EST IDENTIQUE A CELUI DE LA MATRICE DE RIGIDITE (OU IMPEDANCE)
! ----- ON LES RECOPIE 
! ----- DEBUT SMDI
   do ii=1,neq
    zi(inewsmdi+ii-1)=zi(ismdi+ii-1)
   end do
!
! --- --- DEBUT SMHC
   do ii=1,zi(ismde+1)
    zi4(inewsmhc+ii-1)=zi4(ismhc+ii-1)
   end do
!
! --- --- REMPLISSAGE DEUX DEUX AUTRES BLOCS
!
   cumul_non_zero=0
   non_zero_impe=0
   non_zero_matprod=0
!   
   do    jj=1,neq
     hors_diag_impe=zi(ismdi+jj-1)-non_zero_impe-1
     hd_matprod=zi(improdsmdi+jj-1)-non_zero_matprod-1
! --- --- BLOC TRIANGULAIRE SUP IMPEDANCE 
     nz_colncour=0
     do tt=1,hors_diag_impe+1
      zi4(inewsmhc+zi(ismde+1)+cumul_non_zero+tt-1)=zi4(ismhc+zi(ismdi+jj-1)-1-hors_diag_impe+tt-1)
      nz_colncour=nz_colncour+1
      non_zero_impe=non_zero_impe+1
     end do

! --- --- BLOC TRIANGULAIRE INF IMPEDANCE

      ! boucle sur les colonnes superieures
      do kk=jj+1,neq 
      if (jj.ne.neq) then
         ! boucle sur les elements non nuls de la colonne superieure en cours
         do ll=zi(ismdi+kk-2)+1,zi(ismdi+kk-1) 
                                               
            if (zi4(ismhc+ll-1).gt.jj) goto 111
            ! on evalue si on tombe sur le numero de file correspondant a la colonne reelle en cours
            if (zi4(ismhc+ll-1).eq.jj) then 
              nz_colncour=nz_colncour+1
              zi4(inewsmhc-1+zi(ismde+1)+cumul_non_zero+nz_colncour)=int(kk,4)
              goto 111
            end if
         end do
         ! boucle sur les elements non nuls de la colonne superieure en cours
111     continue
      end if       
      end do
      ! boucle sur les colonnes superieures

! --- --- BLOC MATRICE D'OBS

      do kk=1,hd_matprod+1
         nz_colncour=nz_colncour+1
         non_zero_matprod=non_zero_matprod+1
         zi4(inewsmhc-1+zi(ismde+1)+cumul_non_zero+nz_colncour)=int(neq,4)+&
                                                                zi4(improdsmhc-1+non_zero_matprod)
      end do
      cumul_non_zero=cumul_non_zero+nz_colncour
! --- --- FINALISATION SMDI
      zi(inewsmdi+neq+jj-1)=zi(inewsmdi+neq+jj-2)+nz_colncour

    end do   
! 
! --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
!
! --- CREATION DU MATR_ASSE_GENE ASSOCIEE A LA MATRICE ERC
!
    nom_matr_erc=baseno//'.AA.ASS.ERC'
! --- REFA
    call wkvect(nom_matr_erc//'.REFA', 'G V K24', 20, mrefa)
    zk24(mrefa-1+1)=''
    zk24(mrefa-1+2)=nom_nume_erc
    zk24(mrefa-1+3)=''
    zk24(mrefa-1+4)='&&MELANGE'
    zk24(mrefa-1+5)=''
    zk24(mrefa-1+6)=''
    zk24(mrefa-1+7)=solveu
!
    zk24(mrefa-1+8)=''
    zk24(mrefa-1+9)='MS'
    zk24(mrefa-1+10)='GENE'
    zk24(mrefa-1+11)='MPI_COMPLET'
! --- DESC
    call wkvect(nom_matr_erc//'.DESC', 'G V I', 3, mdesc)
    zi(mdesc-1+1)=2
    zi(mdesc-1+2)=2*neq
    zi(mdesc-1+3)=2
! --- VALE (INITIALISE MAIS REMPLI DANS asse_matr_erc)
    call jecrec(nom_matr_erc//'.VALM', 'G V R', 'NU', 'DISPERSE', 'VARIABLE',1)
    call jeecra(jexnum(nom_matr_erc//'.VALM', 1), 'LONMAX', nozero, ' ')
    call jeveuo(jexnum(nom_matr_erc//'.VALM', 1), 'E', mvale)
! --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 
!
! --- CREATION DU VECT_ASSE_GENE ASSOCIEE A LA RESOLUTION DE L'ERC
    nom_vect_erc=baseno//'.BB.ASS.ERC'
! --- DESC
    call wkvect(nom_vect_erc//'.DESC', 'G V I', 3, vdesc)
    zi(vdesc-1+1)=1
    zi(vdesc-1+2)=2*neq
    zi(vdesc-1+3)=2
! --- VALE (INITIALISE MAIS REMPLI DANS asse_vect_erc)       

    call wkvect(nom_vect_erc//'.VALE', 'G V R', 2*neq, vvale)
    call r8inir(neq,0.d0,zr(vvale),1)
!
! --- INFORMATIONS RELATIVES A LA DIMENSION DU PROBLEME D'ERC
    valei(3)=non_zero_impe
    valei(6)=non_zero_matprod*2-neq
    valei(7)=2*neq
    valei(8)=nozero*2-2*neq 
!
end subroutine

