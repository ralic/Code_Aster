subroutine elg_remplt(c, nonu, nworkt, t, nbnvco)
    implicit none
! aslint: disable=W1304
!
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
# include "jeveux.h"
# include "asterc/asmpi_comm.h"
# include "asterc/r8prem.h"
# include "asterfort/assert.h"
# include "asterfort/elg_nllspc.h"
# include "asterfort/infniv.h"
# include "asterfort/jedetr.h"
# include "asterfort/jeveuo.h"
# include "asterfort/wkvect.h"
# include "blas/dgemv.h"
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!
    Mat, intent(in)               :: c
    character(len=14), intent(in) :: nonu
    integer, intent(in)           :: nworkt
    Mat, intent(inout)            :: t
    integer, intent(inout)        :: nbnvco

!
!================================================================
!
    PetscInt :: ierr, nbeq, clag1
    Vec :: c_temp, v_temp
    integer(kind=4) :: nbnzc
    integer :: i1, ldelg, nnzt, contr, j1, nzrow, valrow, k1, indnz, iscons, numcon, nblib, nbcont
    integer :: nbnz, indcon, indlib, icol, lwork1, imax, ctemp, ltlib, lccon, lclib,nzmax
    integer :: nvcont, ifm, niv, nblibt,posind,compnd
    real(kind=8) :: eps, norm, cmax, normc
    logical :: info2
    mpi_int :: mpicomm
!----------------------------------------------------------------------
    eps=r8prem()
    call asmpi_comm('GET_WORLD', mpicomm)
    call infniv(ifm, niv)
    info2=niv.eq.2
!
!---------------------------------!
!--                             --!
!-- Remplissage de la matrice T --!
!--                             --!
!---------------------------------!
!
    call jeveuo('&&APELIM.NNZ_MAT_T      ', 'E', nnzt)
    call jeveuo('&&APELIM.CONSTRAINED_DDL', 'E', contr)
    call jeveuo('&&APELIM.IND_NZ_ROW     ', 'L', nzrow)
    call jeveuo('&&APELIM.VAL_NZ_ROW     ', 'E', valrow)
    call jeveuo('&&APELIM.IND_NZ_T       ', 'E', indnz)
    call jeveuo('&&APELIM.IND_CONTRAINTS ', 'E', indcon)
    call jeveuo('&&APELIM.IND_LIBRES     ', 'E', indlib)
    call jeveuo('&&APELIM.CONTR_NON_VERIF', 'E', nvcont)
    call jeveuo('&&APELIM.LIGNE_C_TEMP   ', 'E', ctemp)
!
!   nlag : nombre de ddls Lagrange 
!   nbeq : nombre de ddls "physiques" (i.e. non-Lagrange)
!   La matrice des contraintes C est de taille nlag x nbeq 
    call MatGetSize(c, clag1, nbeq, ierr)
    ASSERT(ierr == 0 ) 
    if (info2) then 
       write(6,*),'C est de taille nlag= ', clag1,' x neq= ', nbeq
    endif
!
!--
!-- Reinitialisation de CONTR et initialisation de diag. de T
!--
    call jeveuo(nonu//'.NUME.DELG', 'L', ldelg)
    nzmax=0
    do i1 = 1, nbeq
        zi4(contr+i1-1)=0
        if (zi(ldelg+i1-1) .eq. 0) then
            call MatSetValues(t, 1, [int(i1-1, 4)], 1, [int(i1-1, 4)],&
                              [1.d0], INSERT_VALUES, ierr)
        else
            call MatSetValues(t, 1, [int(i1-1, 4)], 1, [int(i1-1, 4)],&
                              [0.d0], INSERT_VALUES, ierr)
        endif
        if (zi4(nnzt+i1-1) .gt. nzmax) nzmax=zi4(nnzt+i1-1)
    end do
    !write(6,*),'NZMAX=',nzmax
    call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
!--
!-- Allocation des objets de travail
!--
    call wkvect('&&ELG_REMPLT.C_I1.CON', 'V V R', nzmax, lccon)
    call wkvect('&&ELG_REMPLT.C_I1.LIB', 'V V R', nzmax, lclib)
    call wkvect('&&ELG_REMPLT.VEC_WORK1', 'V V R', nzmax, lwork1)
! 
    call wkvect('&&ELG_REMPLT.T_LIB.CON', 'V V R', nworkt, ltlib)
    call wkvect('&&ELG_REMPLT.COMP_IND', 'V V S', nbeq, compnd)
    posind=0
    call VecCreateSeq(mpicomm, nbeq, c_temp, ierr)
    call VecCreateSeq(mpicomm, nbeq, v_temp, ierr)
!
!--------------------------------!
!--                            --!
!-- Boucle sur les contraintes --!
!--                            --!
!--------------------------------!
!
    !write(6,*),'%-- REMPLT.F90'
    !write(6,*),'    nworkt=',nworkt
    do i1 = 1, clag1
        if (info2) write(ifm,*),' '
        if (info2) write(ifm,*),' '
        if (info2) write(ifm,'(A14,I3)'),' CONTRAINTE : ',i1
        if (info2) write(ifm,*),' '
        call MatGetRow(c, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
        !write(6,*),' OK MatGetRow',ierr              
        call MatRestoreRow(c, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                           ierr)
        !write(6,*),' OK MatRestoreRow',ierr               
!--
!-- Normalisation de C
!-- Normalement, les lignes de C sont deja normees
        nbnz=0
        norm=0.d0
        do j1 = 1, nbnzc
            norm=norm+zr(valrow+j1-1)**2
        end do
        norm=sqrt(norm)
        normc=norm
        do j1 = 1, nbnzc
            if (abs(zr(valrow+j1-1))/norm .gt. eps) then
                zi4(indnz+nbnz)=j1-1
                zr(ctemp+nbnz)=zr(valrow+j1-1)/norm
                nbnz=nbnz+1
            endif
        end do
!--
!-- Recopie de la contrainte, en normalisant
        call VecSet(c_temp, 0.d0, ierr)
        !write(6,*),' OK VecSet',ierr
        call VecSet(v_temp, 0.d0, ierr)
        !write(6,*),' OK VecSet',ierr
        
        do j1 = 1, nbnz
            numcon=zi4(nzrow+zi4(indnz+j1-1))
            call VecSetValues(c_temp, 1, [int(numcon, 4)], zr(ctemp+j1-1), INSERT_VALUES,&
                              ierr)
        end do
        !write(6,*),' OK VecSetValues',ierr
        call VecAssemblyBegin(c_temp, ierr)
        !write(6,*),' OK VecAssemblyBegin',ierr
        call VecAssemblyEnd(c_temp, ierr)
        !write(6,*),' OK VecAssemblyEnd',ierr
!--
!-- Calcul du produit C_temp*T
        call MatMultTranspose(t, c_temp, v_temp, ierr)
        !write(6,*),' OK MatMultTranspose',ierr
        call VecNorm(v_temp, norm_2, norm, ierr)
        !write(6,*),' OK VecNorm',ierr
        if (info2) write(ifm,*),'   |C(I1,:).T|=',norm
        if (info2) write(ifm,*),' '
        if (norm .lt. 1e-12) then
            if (info2) write(ifm,*),' CONTRAINTE DEJA VERIFIEE'
            goto 123
        endif
!--
!-- Comptage des DDL impliques dans d'autres contraintes
!--
        nbcont=0
        nblib=0
        nblibt=0
        cmax=0.d0
        imax=0
        if (nbnz .gt. 1) then
            do j1 = 1, nbnz
                if (zi4(contr + numcon) .eq. 0) nblibt=nblibt+1
            end do
            do j1 = 1, nbnz
                numcon=zi4(nzrow+zi4(indnz+j1-1))
                iscons=zi4(contr + numcon)
                if (iscons .eq. 0) then
                    zi4(indlib+nblib)=numcon
                    zr(lclib+nblib)=zr(ctemp+j1-1)
                    nblib=nblib+1
                    if (abs(zr(ctemp+j1-1)) .gt. cmax) then
                        cmax=abs(zr(ctemp+j1-1))
                        imax=nblib-1
                    endif
                else
                    if (nblibt .gt. 0) then 
                      zi4(indcon+nbcont)=numcon
                      zr(lccon+nbcont)=zr(ctemp+j1-1)
                    endif
                    nbcont=nbcont+1
                endif
            end do
            !write(6,*),'  imax=',imax
            !write(6,*),'  cmax=',cmax
!
            if (nbcont .eq. 0) then
!--------------------------------------------------------------------------!
!--                                                                      --!
!-- Construction d'une base du noyau de toute la contrainte - elg_nllspc --!
!--                                                                      --!
!--------------------------------------------------------------------------!
                call elg_nllspc(nbnz, zr(lclib), zr(ltlib))
!            CALL JEVEUO('&&APELIM.T_LIB.CON','E',LTLIB)
                do j1 = 1, nbnz
                    numcon=zi4(nzrow + zi4(indnz+j1-1))
                    do k1 = 1, nbnz
                        icol=zi4(nzrow + zi4(indnz+k1-1))
                        call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                          [zr(ltlib+ nbnz*(k1-1)+j1-1)], INSERT_VALUES, ierr)
                        zr(ltlib+nbnz*(k1-1)+j1-1)=0.d0
                    end do
                    zi4(contr + numcon) = 1
                    zr(lclib+j1-1)=0.d0
                end do
!
                call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
                call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
!--
!-- On verifie que la contrainte est bien eliminee
!--
                call MatMultTranspose(t, c_temp, v_temp, ierr)
                call VecNorm(v_temp, norm_2, norm, ierr)
!
                if (norm .gt. 1.d-10) then
!--
!-- On ne devrait, normalement, jamais passer par la
!--    => voir note cas NBLIB < NBNZ
!-- mais ca coute pas cher a prevoir
!--
                    if (info2) write(ifm,*),'CAS NBCONT = 0'
                    if (info2) write(ifm,*),'CONTRAINTE MAL ELIMINEE - ', norm
                    zi4(nvcont+nbnvco)=i1-1
                    nbnvco=nbnvco+1
!--
!-- On reinitialise la sous matrice de T
!--
                    do j1 = 1, nbnz
                        numcon=zi4(nzrow + zi4(indnz+j1-1))
                        do k1 = 1, nbnz
                            icol=zi4(nzrow + zi4(indnz+k1-1))
                            if (j1 .eq. k1) then
                                call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                                  [1.d0], INSERT_VALUES, ierr)
                            else
                                call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                                  [0.d0], INSERT_VALUES, ierr)
                            endif
                        end do
                        zi4(contr + numcon) = 0
                    end do
!
                    call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
                    call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
!
                else
                    do j1 = 1, nblib
                        numcon=zi4(indlib+j1-1)
                        zi4(compnd+posind+j1-1)=numcon
                     end do
                     posind=posind+nbnz   
                    if (info2) write(ifm,*),'CAS NBCONT = 0'
                    if (info2) write(ifm,*),'CONTRAINTE BIEN ELIMINEE - ', norm
                endif
!
!
            else if (nblib .gt. 0) then
!---------------------------------------------------------------!
!--                                                           --!
!-- cont > 0 et lib > 0 : probleme moindre carre + elg_nllspc --!
!--                                                           --!
!---------------------------------------------------------------!
!--
!-- Partie T(lib,con) = -C(i1,lib) \ (C(i1,con) * T(con,con)) ;
!--
!--
!--   Pour les perf : faire ca en creux en extrayant les sous blocs
!--    des matrices! Pas la peine defaire la recopie en plein
!--

                nbcont=posind
! zr(ltcon) et zr(ltlib) ne sont jamais utilises ensemble => pas besoin de 2 tableaux
!              call MatGetValues(t, nbcont, zi4(compnd), nbcont, zi4(compnd),&
!                zr(ltcon), ierr)
                call MatGetValues(t, nbcont, zi4(compnd), nbcont, zi4(compnd),&
                                  zr(ltlib), ierr)
!--         Retourne la matrice stockee en ligne
!                call dgemv('N', nbcont, nbcont, 1.d0, zr(ltcon),&
!                           nbcont, zr(lccon), 1, 0.d0, zr(lwork1),&
!                           1)

!  Suite changement des indices, on re affecte la partir de C pour le produit 
!  et on renorme la ligne de C               
                call MatGetValues(c,1,[int(i1-1,4)], nbcont, zi4(compnd),&
                                  zr(lccon), ierr)
                do j1 = 1, nbcont
                  zr(lccon+j1-1)=zr(lccon+j1-1)/normc
                end do                  
                call dgemv('N', nbcont, nbcont, 1.d0, zr(ltlib),&
                           nbcont, zr(lccon), 1, 0.d0, zr(lwork1),&
                           1)
                numcon=zi4(indlib+imax)
!
!
!
                if (cmax .lt. 1.d-10) then
!--
!-- On ne peut eliminer correctement la contrainte -> On sort
!--
                    zi4(nvcont+nbnvco)=i1-1
                    nbnvco=nbnvco+1
                    goto 123
                endif
                !
                do j1 = 1, nbcont
                    icol=zi4(compnd+j1-1)
                    call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                      [-zr(lwork1+ j1-1)/zr(lclib+imax)], INSERT_VALUES, ierr)
                end do
!
!
!--
!-- Partie elg_nllspc lib-lib
!--
                call elg_nllspc(nblib, zr(lclib), zr(ltlib))
!            CALL JEVEUO('&&APELIM.T_LIB.CON','E',LTLIB)
                do j1 = 1, nblib
                    numcon=zi4(indlib+j1-1)
                    do k1 = 1, nblib
                        icol=zi4(indlib+k1-1)
                        call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                          [zr(ltlib+ nblib*(k1-1)+j1-1)], INSERT_VALUES, ierr)
                        zr(ltlib+nblib*(k1-1)+j1-1)=0.d0
                    end do
                    zi4(contr + numcon) = 1
                    zr(lclib+j1-1)=0.d0
                end do
!
                call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
                call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
!--
!-- On verifie que la contrainte est bien eliminee
!--
                call MatMultTranspose(t, c_temp, v_temp, ierr)
                call VecNorm(v_temp, norm_2, norm, ierr)
!
                if (norm .gt. 1.d-10) then
!-- Si mal eliminee -> On reinitialise et on sort
!
! Cas qui survient quand, dans les contraintes deja utilisees,
! il y a des colonnes hors INDCON courant deja remplies (cas d'une
! contrainte precedemment eliminee) - Ce cas n'est pas simple a prendre
! en compte puisqu'il necessite de connaitre T pour effectuer le
! comptage de termes non nuls et le remplissage
!
                    if (info2) write(ifm,*),'CAS NBLIB < NBNZ'
                    if (info2) write(ifm,*),'CONTRAINTE MAL ELIMINEE - ', norm
                    zi4(nvcont+nbnvco)=i1-1
                    nbnvco=nbnvco+1
!
                    numcon=zi4(indlib+imax)
                    do j1 = 1, nbcont
                        icol=zi4(indcon+j1-1)
                        call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                          [0.d0], INSERT_VALUES, ierr)
                    end do
!
!
                    do j1 = 1, nblib
                        numcon=zi4(indlib+j1-1)
                        do k1 = 1, nblib
                            icol=zi4(indlib+k1-1)
                            if (j1 .eq. k1) then
                                call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                                  [1.d0], INSERT_VALUES, ierr)
                            else
                                call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(icol, 4)],&
                                                  [0.d0], INSERT_VALUES, ierr)
                            endif
                        end do
!-- On laisse garde le "tag" sur les DDL, sinon, ca fout le bordel
!-- pour le comptage. C'est pas optimal, mais ca marche
!                ZI4(CONTR + NUMCON) = 0
!
                    end do
!
                    call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
                    call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
!
                else
                    do j1 = 1, nblib
                        numcon=zi4(indlib+j1-1)
                        zi4(compnd+posind+j1-1)=numcon
                     end do
                     posind=posind+nblib
                    
                    if (info2) write(ifm,*),'CAS NBLIB < NBNZ'
                    if (info2) write(ifm,*),'CONTRAINTE BIEN ELIMINEE - ', norm
                endif
!
            else
!--
!-- On peut pas eliminer, on stocke le numero pour projection ulterieure
!--
                zi4(nvcont+nbnvco)=i1-1
                nbnvco=nbnvco+1
                goto 123
            endif
!
        else
            numcon=zi4(nzrow+zi4(indnz))
            iscons=zi4(contr + numcon)
            !write(6,*),'numcon=',numcon
            !write(6,*),'iscons=',iscons
            
            if (iscons .eq. 0) then
                call MatSetValues(t, 1, [int(numcon, 4)], 1, [int(numcon, 4)],&
                                  [0.d0], INSERT_VALUES, ierr)
                zi4(contr + numcon) = 1
                call MatAssemblyBegin(t, MAT_FINAL_ASSEMBLY, ierr)
                call MatAssemblyEnd(t, MAT_FINAL_ASSEMBLY, ierr)
                zi4(compnd+posind)=numcon
                posind=posind+1 
!
            else
!--
!-- On peut pas eliminer, on stocke le numero pour projection ulterieure
!--
                zi4(nvcont+nbnvco)=i1-1
                nbnvco=nbnvco+1
                goto 123
            endif
!
        endif
!--
!-- Si la contrainte est deja verifiee, on arrive direct la
!--
123     continue
!
!        call MatRestoreRow(C,I1-1,int(nbnzc),ZI4(NZROW),ZR(VALROW),ierr)
!
    end do
!
!
!
    call jedetr('&&ELG_REMPLT.C_I1.CON')
    call jedetr('&&ELG_REMPLT.C_I1.LIB')
    call jedetr('&&ELG_REMPLT.VEC_WORK1')
    call jedetr('&&ELG_REMPLT.T_CON.CON')
    call jedetr('&&ELG_REMPLT.T_LIB.CON')
    call jedetr('&&ELG_REMPLT.COMP_IND')
!
#else
    integer :: c, t
    integer :: nworkt, nbnvco
    character(len=14) :: nonu
    nonu = ' '
    t = c + nworkt + nbnvco
    ASSERT(.false.)
#endif
end subroutine
