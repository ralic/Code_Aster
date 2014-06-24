subroutine elg_comptt(c, t, nworkt)
    implicit none
! person_in_charge: mathieu.corus at edf.fr
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
!----------------------------------------------------!
!----------------------------------------------------!
!--                                                --!
!-- Decompte contraite par contrainte              --!
!-- * Allocation de T                                --!
!-- * Remplissage avec des 0. pour garder le profil  --!
!--   au cours des MatAssembly                       --!     
!-- * Calcul de la taille du tableau de travail      --! 
!--   nécessaire pour le calcul de T (elg_rempltt)   --!
!--                                                --!
!----------------------------------------------------!
!----------------------------------------------------!
!
# include "jeveux.h"
# include "asterc/asmpi_comm.h"
# include "asterc/r8prem.h"
# include "asterfort/as_allocate.h"
# include "asterfort/as_deallocate.h" 
# include "asterfort/assert.h"
# include "asterfort/jeveuo.h"
# include "asterfort/wkvect.h"
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!
    Mat, intent(in)      :: c
    Mat, intent(out)     :: t
    integer, intent(out) :: nworkt 
!
!================================================================
    PetscInt :: ierr, numcon, icol
    PetscInt :: one = 1 
    integer :: i1, pass
    PetscInt :: nlag, nbeq, nbnzc
    integer :: nnzt, contr, j1, ctemp,imax, nbelim
    integer :: nzrow, valrow, k1, indnz, iscons, nblib
    integer :: nbcont, nbnz, indcon, indlib,  compnd
    real(kind=8) :: eps, norm, valt, normc,cmax
    mpi_int :: mpicomm
    logical :: info2
!-----------------------------------------------------------------
!
  info2 = .false. 
  if (info2) then 
    write(6,*),'%-- COMPTT.F90'
  endif
!-----------------------------------------!
!--                                     --!
!-- Decompte des elements non nuls de T --!
!--       et initialisation de T        --!
!--                                     --!
!-----------------------------------------!
!
    call asmpi_comm('GET_WORLD', mpicomm)
    call jeveuo('&&APELIM.NNZ_MAT_T      ', 'E', nnzt)
    call jeveuo('&&APELIM.CONSTRAINED_DDL', 'E', contr)
    call jeveuo('&&APELIM.IND_NZ_ROW     ', 'E', nzrow)
    call jeveuo('&&APELIM.VAL_NZ_ROW     ', 'E', valrow)
    call jeveuo('&&APELIM.IND_NZ_T       ', 'E', indnz)
    call jeveuo('&&APELIM.IND_LIBRES     ', 'E', indlib)
    call jeveuo('&&APELIM.LIGNE_C_TEMP   ', 'E', ctemp)
!

! 
!   Initialisation
!   ==============
!   nlag : nombre de ddls Lagrange 
!   nbeq : nombre de ddls "physiques" (i.e. non-Lagrange)
!   La matrice des contraintes C est de taille nlag x nbeq 
    call MatGetSize(c, nlag, nbeq, ierr)
    if (info2) then 
       write(6,*),'C est de taille nlag= ', nlag,' x neq= ', nbeq
    endif
!   Seuil pour le filtrage des valeurs significatives de C   
    eps=r8prem()
!   Valeur par défaut utilisée pour le remplissage de T     
    valt = 0.d0
! 
!  
!   On effectue deux passes 
!   ======================= 
!   1) compter les termes non nuls de T pour pré-allouer la matrice
!   2) remplir T avec des zéros pour que le profil soit gardé lors des appels
!     ultérieurs à MatAssembly 
    do pass = 1, 2 
     if ( info2 ) then 
       write(6,*) " =================== "
       write(6,*) " Passe ", pass
       write(6,*) " =================== "
     endif
!
!   Initialisation à 0 du tableau stockant si un ddl est impliqué 
!   dans une contrainte précédemment éliminée 
    zi4( contr-1+1: contr-1+nbeq ) = 0 
!   Initialisation à 1 du tableau stockant le profil de T 
!   (T est initialisé à l'identité)
    zi4( nnzt-1+1: nnzt-1+nbeq ) = 1 
!   Dimensionnement des tableaux intermediaires de elg_remplt.F90
    nworkt=1
!---------------------------------------!
!--                                   --!
!-- Decompte contraite par contrainte --!
!--                                   --!
!---------------------------------------!
!
  do i1 = 1, nlag
    if (info2) then 
       write(6,*), 'Contrainte -',i1
    endif 
!
! On traite la contrainte I1 (convention Fortran) / I1-1 (convention C)
!
! zi4(nzrow-1+1: nzrow-1+nbnzc) = indices C des colonnes des termes non-nuls 
! de la ligne  C(i1,:). Il s'agit des termes définis dans la structure creuse de la 
! matrice, ils peuvent être numériquement égaux à zéro. 
! zr(valrow-1+1: valrow-1+nbnzc) = valeurs des termes de la ligne. 
!
    call MatGetRow(c, to_petsc_int(i1-1), nbnzc, zi4(nzrow),  zr(valrow),&
                       ierr)
! Calcul de la norme de C(i1,:)
        norm=0.d0
        do j1 = 1, nbnzc
            norm=norm+zr(valrow+j1-1)**2
        end do
        norm=sqrt(norm)
!
! On sélectionne dans la ligne I1 les termes numériquement 
! non-nuls (selection avec R8PREM) 
! zi4(indnz-1+k) = indice C  
! On copie ces termes dans zr(ctemp-1+1:ctemp-1+nbnz)
! et on normalise ce vecteur.
        nbnz=0
        do j1 = 1, nbnzc
            if (abs(zr(valrow+j1-1))/norm .gt. eps) then
            !!!!!!!! ATTENTION Convention C pour un tableau fortran 
                zi4(indnz+nbnz)=j1-1
                nbnz=nbnz+1
                zr(ctemp+nbnz-1)=zr(valrow+j1-1)/norm
            endif
        end do     
!
! Comptage des termes pour T
!
        nblib=0
        nbcont=0
        cmax=0.d0
        imax=0
!
        if (nbnz .gt. 0) then
        ! Nombre de ddls qui ont été impliqués dans les lignes de la matrice des 
        ! contraintes précédemment traitées 
        nbelim = count(zi4(contr-1+1: contr-1+nbeq) ==1)
        
        ! Comptage des DDL impliques dans d'autres contraintes, précédemment éliminées
            do j1 = 1, nbnz
                ! Indice C global du ddl courant 
                numcon=zi4(nzrow+zi4(indnz+j1-1))
                ! Le ddl courant est-il impliqué dans une contrainte
                ! précédemment éliminée ?    
                iscons=zi4(contr + numcon)
                ! Si non, on stocke l'indice dans le tableau des indices "libres"           
                if (iscons .eq. 0) then
                    zi4(indlib+nblib)=numcon
                    nblib=nblib+1
                    ! On recherche le terme de module maximum 
                    if (abs(zr(ctemp+j1-1)) .gt. cmax) then
                        cmax=abs(zr(ctemp+j1-1))
                        imax=nblib-1
                    endif
                else
                ! Si oui, on compte 
                  nbcont=nbcont+1    
                endif
            end do
!
! Pistes pour ameliorer les perfs, si besoin :
! On surestime, en comptant toutes les colonnes de zeros :
!   * On pourrait virer la premiere, dont on sait qu'elle sera nulle
!     => Initialiser avec NBNZ-1
!   * On sait que elg_nllspc donne une matrice triangulaire + sous diagonale
!     => ne compter que ces termes pour Tii
!   * la resolution Tic=Ci\(Cc*Tcc) peut ne remplir qu'une ligne
!     Tic, celle associee a la valeur max de C(indlib)
!     => Ne pas compter les autres
!   La, on compte NBNZ pour toutes les lignes de indlib
!   et on re alloue la ligne pour Tic=Ci\(Cc*Tcc) si besoin
!
! Par ailleurs, on ne tient pas compte des contraintes qui seraient ensuite
! mal eliminees => on sur alloue ca aussi.
!
!
! On réserve la place nécessaire au stockage du bloc T_{LL} 
! Chaque ligne est de taille nblib : on surestime en prenant nbnz 
            do j1 = 1, nblib
                numcon=zi4(indlib+j1-1)
! Nombre de termes non-nuls dans la ligne (indice Fortran) numcon+1 de T 
!                zi4(nnzt+numcon) = nbnz
               zi4(nnzt+numcon) = nblib 
                ! nworkt = max (nlib x nlib)
                if (nblib**2 .gt. nworkt) then
                   nworkt=nblib**2
                endif
 
                
                if (pass == 2) then
                ! On remplit la ligne courante avec des 0 
                  do k1 = 1, nblib
                    icol=zi4(indlib+k1-1)  
                    call MatSetValues(t, one, [to_petsc_int(numcon)], one, [to_petsc_int(icol)],&
                                    [valt], INSERT_VALUES, ierr)
                  end do
                endif
               !
            end do
! Si des ddls sont deja contraints, on devra construire deux blocs dans T: T_{LL} et T_{LE}
! "E" correspond à l'ensemble des ddls non-nuls des contraintes précédemment éliminées
! (et pas seulement de ceux de C(i1,:))
! "L" correspond à l'ensemble des ddls non-nuls de C(i1,:) qui n'ont pas été 
!  impliqués dans une contrainte déjà éliminée
! On alloue maintenant la place nécessaire pour stocker T_{LE}. 
! Par construction, une seule ligne de T_{LE}
! est non-nulle. Il s'agit de la ligne imax, qui compte :
! nbelim = nbre de ddls participant à une contrainte  déjà éliminée 
! termes
            if (nbcont .gt. 0) then
              numcon=zi4(indlib+imax)
              zi4(nnzt+numcon) = zi4(nnzt+numcon) + nbelim 
              !-- il faudra virer ca quand le calcul de (C(i1,con) * T(con,con))
              !--  sera fait en creux, en extrayant les sous blocs!
              if ( ( zi4(nnzt+numcon)  )**2 .gt. nworkt) then
                nworkt=( zi4(nnzt+numcon) )**2
              endif
              if (pass == 2) then 
              ! On remplit la ligne imax avec des zéros
              ! On boucle sur tous les ddls et on sélectionne ceux qui sont
              ! impliqués dans une contrainte précedemment éliminée 
                do icol = 1, nbeq
                  ! Le ddl courant est-il impliqué dans une contrainte
                  ! précédemment éliminée ?     
                  iscons=zi4(contr-1+icol)
                  if ( iscons == 1 ) then  
                    call MatSetValues(t, one, [to_petsc_int(numcon)], one, [to_petsc_int(icol-1)],&
                                    [valt], INSERT_VALUES, ierr)
                  endif                   
                end do
              endif
              !
            endif
            !
            ! Les dls libres sont à présent éliminés: on les marque 
            do j1 = 1, nblib
              numcon=zi4(indlib+j1-1)
              zi4(contr + numcon) = 1
            enddo
           !
        endif
        !
        call MatRestoreRow(c, to_petsc_int(i1-1), nbnzc, zi4(nzrow), zr(valrow),&
                           ierr)
    end do
!
!
!   -- Allocation de T :
!   ---------------------
    if (info2) then 
     write(6,*),'-- profil de T'
      do i1=1,nbeq
        write(6,*),'nnzt(',i1,')=',zi4(nnzt+i1-1)
      end do
    endif  
    !
    if (pass == 1) then 
       call MatCreateSeqAIJ(mpicomm, nbeq, nbeq, PETSC_NULL_INTEGER, zi4(nnzt),&
                         t, ierr)
 
    !   Initialisation à l'identité 
    do i1 = 1, nbeq
        call MatSetValues(t, one, [to_petsc_int(i1-1)], one, [to_petsc_int(i1-1)],&
                          [valt], INSERT_VALUES, ierr)
    end do
    endif
!
!  Fin de la boucle pass 
!
  if (info2) then 
    write(6,*) "Fin de la passe "
  endif
  !  
  enddo
!
!--
!-- Assemblage de T avec le bon profil
!--
    call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
!
 if (info2) then 
    write(6,*) "Fin de elg_comptt "
  endif
!
!
#else
    integer :: c, t
    integer :: nworkt
    t = c + nworkt
    ASSERT(.false.)
#endif
!
end subroutine
