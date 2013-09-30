subroutine elg_comptt(c, t, nbeq, clag1)
    implicit none
! aslint: disable=W0104
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
!-- Allocation de T                                --!
!-- Remplissage avec des 0. pour garder le profil  --!
!--  au cours des MatAssembly                      --!
!--                                                --!
!----------------------------------------------------!
!----------------------------------------------------!
!
#   include "jeveux.h"
#   include "asterfort/jeveuo.h"
#   include "asterfort/assert.h"
#   include "asterc/r8prem.h"
#   include "asterc/asmpi_comm.h"
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
    Mat :: c, t
    integer :: nbeq, clag1
!
!================================================================
    PetscInt :: ierr,numcon,icol
    integer :: i1
    PetscInt :: n1, n2
    integer(kind=4) :: nbnzc
    integer :: nnzt, contr, j1, ctemp
    integer :: nzrow, valrow, k1, indnz, iscons, nblib
    integer :: nbcont, nbnz, indcon, indlib
    real(kind=8) :: eps, norm, valt, normc
    mpi_int :: mpicow
!-----------------------------------------------------------------
    call MatGetSize(c, n1, n2, ierr)
!
!-----------------------------------------!
!--                                     --!
!-- Decompte des elements non nuls de T --!
!--       et initialisation de T        --!
!--                                     --!
!-----------------------------------------!
!
    call asmpi_comm('GET_WORLD', mpicow)
    call jeveuo('&&APELIM.NNZ_MAT_T      ', 'E', nnzt)
    call jeveuo('&&APELIM.CONSTRAINED_DDL', 'E', contr)
    call jeveuo('&&APELIM.IND_NZ_ROW     ', 'E', nzrow)
    call jeveuo('&&APELIM.VAL_NZ_ROW     ', 'E', valrow)
    call jeveuo('&&APELIM.IND_NZ_T       ', 'E', indnz)
    call jeveuo('&&APELIM.IND_CONTRAINTS ', 'E', indcon)
    call jeveuo('&&APELIM.IND_LIBRES     ', 'E', indlib)
    call jeveuo('&&APELIM.LIGNE_C_TEMP   ', 'E', ctemp)
!
    eps=r8prem()
!
!---------------------------------------!
!--                                   --!
!-- Decompte contraite par contrainte --!
!--                                   --!
!---------------------------------------!
!
    do 10 i1 = 1, clag1
! Recherche des indices des DDL impliques  dans la contrainte I1
! Premier tri PETSc, base uniquement sur la structure de la matrice
        call MatGetRow(c, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
!
        norm=0.d0
        do 20 j1 = 1, nbnzc
            norm=norm+zr(valrow+j1-1)**2
20      continue
        norm=sqrt(norm)
!
! raffinement, base sur les valeurs - selection avec R8PREM
! et normalisation des lignes
        nbnz=0
        do 30 j1 = 1, nbnzc
            if (abs(zr(valrow+j1-1))/norm .gt. eps) then
                zi4(indnz+nbnz)=j1-1
                nbnz=nbnz+1
            endif
30      continue
!
! Comptage des termes pour T
        nblib=0
!
        if (nbnz .gt. 1) then
! Comptage des DDL impliques dans d'autres contraintes
            do 40 j1 = 1, nbnz
                numcon=zi4(nzrow+zi4(indnz+j1-1))
                iscons=zi4(contr + numcon)
!
                if (iscons .eq. 0) then
                    zi4(indlib+nblib)=numcon
                    nblib=nblib+1
                endif
40          continue
!
            do 50 j1 = 1, nblib
                numcon=zi4(indlib+j1-1)
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
                zi4(nnzt+numcon) = nbnz
! Chaque contrainte apparait alors comme utilisee
                zi4(contr + numcon) = 1
50          continue
!
        else
            numcon=zi4(nzrow+zi4(indnz))
            iscons=zi4(contr + numcon)
            if (iscons .eq. 0) then
! On alloue juste le terme sur la diagonale si non implique par ailleurs
! Pour un meilleur comptage, on peut desallouer la valeur
                zi4(nnzt+numcon)=1
                zi4(contr+numcon)=1
            endif
        endif
        call MatRestoreRow(c, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                           ierr)
10  end do


!   -- Allocation de T :
!   ---------------------
    call MatCreateSeqAIJ(mpicow, nbeq, nbeq, int(PETSC_NULL_INTEGER), zi4(nnzt), t, ierr)

!   -- Reinitialisation de CONTR et initialisation de la diagonale de T :
!   --------------------------------------------------------------------
    valt=0.d0
    do 60 i1 = 1, nbeq
        zi4(contr+i1-1)=0
        call MatSetValues(t, 1, [int(i1-1,4)], 1, [int(i1-1,4)],&
                          [valt], INSERT_VALUES, ierr)
60  end do
!
!-----------------------------------------!
!--                                     --!
!-- Remplissage de epsilon, pour garder --!
!-- le profil au cours des  MatAssembly --!
!--                                     --!
!-----------------------------------------!
!
    do 70 i1 = 1, clag1
!--
!-- Recuperation de la ligne de C a traiter et normalisation
!--
        call MatGetRow(c, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
        nbnz=0
        norm=0.d0
        do 80 j1 = 1, nbnzc
            norm=norm+zr(valrow+j1-1)**2
80      continue
        norm=sqrt(norm)
        normc=norm
        do 90 j1 = 1, nbnzc
            if (abs(zr(valrow+j1-1))/norm .gt. eps) then
                zi4(indnz+nbnz)=j1-1
                nbnz=nbnz+1
                zr(ctemp+nbnz-1)=zr(valrow+j1-1)/norm
            endif
90      continue
!--
!-- Recherche du remplissage
!--
        nbcont=0
        nblib=0
        if (nbnz .gt. 1) then
!-- Comptage des DDL impliques dans d'autres contraintes
            do 100 j1 = 1, nbnz
                numcon=zi4(nzrow+zi4(indnz+j1-1))
                iscons=zi4(contr + numcon)
                if (iscons .eq. 0) then
                    zi4(indlib+nblib)=numcon
                    nblib=nblib+1
                else
                    zi4(indcon+nbcont)=numcon
                    nbcont=nbcont+1
                endif
100          continue
!
            if (nbcont .eq. 0) then
!-- Insertion d'un bloc plein de taille NBNZ x NBNZ
                do 110 j1 = 1, nbnz
                    numcon=zi4(indlib+j1-1)
                    do 120 k1 = 1, nbnz
                        icol=zi4(nzrow + zi4(indnz+k1-1))
                        call MatSetValues(t, 1, [numcon], 1, [icol],&
                                          [valt], INSERT_VALUES, ierr)
120                  continue
                    zi4(contr + numcon) = 1
110              continue
!
!
            else if (nblib .gt. 0) then
!-- cont > 0 et lib > 0 : probleme moindre carre + elg_nllspc
!
!-- Insertion d'un bloc plein de taille NBLIB x NBNZ
!
! Pour l'instant, pas de comptage "intelligent"
! Si ca doit se faire, separer les deux parties
!
! 1 : Partie T(lib,con) = -C(i1,lib) \ (C(i1,con) * T(con,con))
! 2 : Partie QR lib-lib
! La 1 ne prenant qu'une ligne
!
                do 130 j1 = 1, nblib
                    numcon=zi4(indlib+j1-1)
!
                    do 140 k1 = 1, nbnz
                        icol=zi4(nzrow + zi4(indnz+k1-1))
                        call MatSetValues(t, 1, [numcon], 1, [icol],&
                                          [valt], INSERT_VALUES, ierr)
140                  continue
                    zi4(contr + numcon) = 1
130              continue
!
            else
                goto 1235
            endif
!
        else
!-- On alloue tout de meme la place des 0 sur la diagonale
            numcon=zi4(nzrow+zi4(indnz))
            iscons=zi4(contr + numcon)
            if (iscons .eq. 0) then
                call MatSetValues(t, 1, [numcon], 1, [numcon],&
                                  [valt], INSERT_VALUES, ierr)
                zi4(contr + numcon) = 1
            else
                goto 1235
            endif
!
        endif
!
!-- Si la contrainte est deja verifiee, on arrive direct la
1235      continue
!
        call MatRestoreRow(c, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                           ierr)
!
!-- Normalisation de la ligne de C avant reprojection eventuelle
        do 150 j1 = 1, nbnzc
            icol=zi4(nzrow+j1-1)
            call MatSetValues(c, 1, [int(i1-1,4)], 1, [int(icol,4)],&
                              [zr(valrow+j1-1)/normc], INSERT_VALUES, ierr)
150      continue
!
        call MatAssemblyBegin(c, MAT_FINAL_ASSEMBLY, ierr)
        call MatAssemblyEnd(c, MAT_FINAL_ASSEMBLY, ierr)
!
!
70  end do
!
!--
!-- Assemblage de T avec le bon profil
!--
    call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)



#else
    integer(kind=8) :: c, t
    integer :: nbeq, clag1
    ASSERT(.false.)
#endif
!
end subroutine
