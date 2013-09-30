subroutine elg_matrqr(ct, r, nbphys, nblag)
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
!--    IN : Ct    : matrice dont on realise la decomposition QR
!--         NBPHYS  : nombre de lignes de Ct
!--         NBLAG : nombre de colonnes de Ct
!--     /!\  Ne traite que le cas NBPHYS > NBLAG  /!\
!--
!--    OUT : R : Matrice "R" de la decomposition QR de Ct
!
#   include "asterfort/utmess.h"
#   include "asterfort/assert.h"

#ifdef _HAVE_PETSC
#   include "elim_lagr.h"

#   include "jeveux.h"
#   include "blas/ddot.h"
#   include "asterc/asmpi_comm.h"
#   include "asterfort/jedetr.h"
#   include "asterfort/jeveuo.h"
#   include "asterfort/elg_setdif.h"
#   include "asterfort/elg_setint.h"
#   include "asterfort/wkvect.h"
!
    integer :: nbphys, nblag
!
!
    Mat :: ct, ctt, r
    PetscInt :: ierr,indc(1)
    PetscInt :: n1, n2
    Mat :: c, rt
!
    PetscInt :: nbnzc, nbnzc2
    integer :: i1, j1, k1, nnzt, nzrow, valrow, varow2, nzrow2, inda, indb, nba, nbb, nbc
    integer :: diagr, vtemp, vtemp2, nzrowc
!
    real(kind=8) :: norm, rbid, dj1, fak, trbid(1)
    mpi_int :: mpicow
!--------------------------------------------------------------------
!-- Source de l'algo :
!--          Algorithms for the QR-Decomposition
!--                    WALTER GANDER
!--
!--               RESEARCH REPORT NO. 80-021
!--                       APRIL 1980
!--         SEMINAR FUER ANGEWANDTE MATHEMATIK
!--        EIDGENOESSISCHE TECHNISCHE HOCHSCHULE
!--                    CH-8092 ZUERICH
!--------------------------------------------------------------------
!
    call asmpi_comm('GET_WORLD', mpicow)
    call MatGetSize(ct, n1, n2, ierr)
!
    if (nblag .gt. nbphys) then
        write(6,*),' PLUS DE LIAISONS QUE D''EQUATIONS...'
        ASSERT(.false.)
    endif
!
!-- ON COMMENCE PAR CONSTRUIRE R transpose, selon l'algo ci dessus
!
!--
!-- STEP 0 : rajouter la diagonale dans C, si elle n'est pas presente
!--
    call jeveuo('&&APELIM.NNZ_MAT_T', 'E', nnzt)
    call jeveuo('&&APELIM.VAL_NZ_ROW', 'E', valrow)
    call jeveuo('&&APELIM.IND_NZ_ROW', 'E', nzrow)
!
!
!
    do 5 i1 = 1, nbphys
        call MatGetRow(ct, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
        call MatRestoreRow(ct, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                           ierr)
        if (i1 .le. nblag) then
            nba=nbnzc
            call elg_setint(zi4(nzrow), nba, [int(i1-1,4)], 1, indc, nbc, 'COMPTE')
            if (nbc .eq. 0) then
!               -- On rajoute le terme diagonal
                zi4(nnzt+i1-1)=nbnzc+1
            else
                zi4(nnzt+i1-1)=nbnzc
            endif
        endif
 5  end do
!
!-- On alloue
    call MatCreateSeqAIJ(mpicow, nbphys, nblag, int(PETSC_NULL_INTEGER), zi4(nnzt),&
                         ctt, ierr)
!
!-- On recopie
    do 6 i1 = 1, nbphys
        call MatGetRow(ct, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
        call MatRestoreRow(ct, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                           ierr)
        call MatSetValues(ctt, 1, [int(i1-1,4)], int(nbnzc), zi4(nzrow),&
                          zr(valrow), INSERT_VALUES, ierr)
        if (i1 .le. nblag) then
            nba=nbnzc
            call elg_setint(zi4(nzrow), nba, [int(i1-1,4)], 1, indc, nbc, 'COMPTE')
            if (nbc .eq. 0) then
!               -- On rajoute le terme diagonal
                call MatSetValues(ctt, 1, [int(i1-1,4)], 1, [int(i1-1,4)],&
                                  [0.d0], INSERT_VALUES, ierr)
            endif
        endif
!
 6  end do
    call MatAssemblyBegin(ctt, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(ctt, MAT_FINAL_ASSEMBLY, ierr)
!
!--
!-- STEP 1 : CALCUL DES NOMBRES D'ELEMENTS NON NULS DE Rt
!--
    call MatTranspose(ctt, MAT_INITIAL_MATRIX, c, ierr)
!
    call MatDestroy(ctt, ierr)
!
!
!
    call wkvect('&&ELG_MATRQR.VAL_NZ_ROW2', 'V V R', nbphys, varow2)
    call wkvect('&&ELG_MATRQR.IND_NZ_ROW2', 'V V S', nbphys, nzrow2)
    call wkvect('&&ELG_MATRQR.IND_NZ_ROWC', 'V V S', nbphys, nzrowc)
    call wkvect('&&ELG_MATRQR.DIAG_R',      'V V R', nblag, diagr)
    call wkvect('&&ELG_MATRQR.VTEMP',       'V V R', nbphys, vtemp)
    call wkvect('&&ELG_MATRQR.VTEMP2',      'V V S', nbphys, vtemp2)
!
    do 10 j1 = 1, nblag
!---------------------!
!--                 --!
!-- PARTIE COMPTAGE --!
!--                 --!
!---------------------!
!
        do 20 i1 = 1, j1-1
            call MatGetRow(c, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                           ierr)
            zi4(nnzt+i1-1)=nbnzc
            call MatRestoreRow(c, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                               ierr)
20      continue
!
!-- ALLOCATION DE LA COLONNE J1 DE R :
!-- JUSTE AU DESSUS DE LA DIAGONALE, LE RESTE SERT PAS
        call MatGetRow(c, j1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
        call MatRestoreRow(c, j1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                           ierr)
!
!-- RECHERCHE DES TERMES AU DELA DE LA DIAGONALE
        inda=0
30      continue
        if (zi4(nzrow+inda) .lt. j1-1) then
            inda=inda+1
            goto 30
        endif
        nba=nbnzc-inda
        zi4(nnzt+j1-1)=inda+1
!
        if (nbnzc .gt. nbphys) ASSERT(.false.)
        if (inda .gt. nbphys) ASSERT(.false.)
        if (nba .gt. nbphys) ASSERT(.false.)
!
        do 40 i1 = j1+1, nblag
            call MatGetRow(c, i1-1, nbnzc2, zi4(nzrow2), zr(varow2),&
                           ierr)
            call MatRestoreRow(c, i1-1, int(nbnzc2), zi4(nzrow2), zr(varow2),&
                               ierr)
            if (nbnzc2 .gt. nbphys) ASSERT(.false.)
!
!-- RECHERCHE DES TERMES AU DELA DE LA DIAGONALE
            indb=0
50          continue
            if (zi4(nzrow2+indb) .lt. j1-1) then
                indb=indb+1
                goto 50
            endif
            nbb=nbnzc2-indb
            call elg_setint(zi4(nzrow+inda), nba, zi4(nzrow2+indb), nbb, indc, nbc, 'COMPTE')

            if (nbc .gt. 0) then
!
                call elg_setdif(zi4(nzrow+inda), nba, zi4(nzrow2+indb), nbb, indc,&
                            nbc, 'COMPTE')
                zi4(nnzt+i1-1)=nbnzc2+nbc
!
            else
                zi4(nnzt+i1-1)=nbnzc2
            endif
!
40      continue
!
!--
!-- ALLOCATION DE LA MATRICE
!--
        call MatCreateSeqAIJ(mpicow, nblag, nbphys, int(PETSC_NULL_INTEGER), zi4(nnzt),&
                             rt, ierr)
!
!------------------------!
!--                    --!
!-- PARTIE REMPLISSAGE --!
!--                    --!
!------------------------!
!
!-- On recopie les lignes deja traitees
        do 60 i1 = 1, j1-1
            call MatGetRow(c, i1-1, nbnzc, zi4(nzrow), zr(valrow),&
                           ierr)
            call MatSetValues(rt, 1, [int(i1-1,4)], int(nbnzc), zi4(nzrow),&
                              zr(valrow), INSERT_VALUES, ierr)
            call MatRestoreRow(c, i1-1, int(nbnzc), zi4(nzrow), zr(valrow),&
                               ierr)
60      continue
        call MatGetRow(c, j1-1, nbnzc, zi4(nzrow), zr(valrow),&
                       ierr)
        call MatRestoreRow(c, j1-1, int(nbnzc), zi4(nzrow), zr(valrow), ierr)
        zi4(nzrowc:nzrowc+nbnzc)=zi4(nzrow:nzrow+nbnzc)
!-- RECHERCHE DES TERMES AU DELA DE LA DIAGONALE
        inda=0
70      continue
        if (zi4(nzrow+inda) .lt. j1-1) then
            inda=inda+1
            goto 70
        endif
        nba=nbnzc-inda
        norm=sqrt(ddot(nba,zr(valrow+inda),1,zr(valrow+inda),1))
        rbid=0.d0
        call MatGetValues(c, 1, [int(j1-1,4)], 1, [int(j1-1,4)],&
                          trbid, ierr)
        rbid=trbid(1)
!
        if (rbid .gt. 0) then
            zr(diagr+j1-1)=-norm
        else
            zr(diagr+j1-1)=norm
        endif
        dj1=zr(diagr+j1-1)
!
        fak=sqrt(norm*(norm+abs(rbid)))
!
!-- ON RECOPIE LE DEBUT DE LA LIGNE J1
        do 74 i1 = 1, inda
            call MatSetValues(rt, 1, [int(j1-1,4)], 1, zi4(nzrow+i1-1),&
                              zr(valrow+i1- 1), INSERT_VALUES, ierr)
74      continue
!
!-- ON NORME LA LIGNE J1
        do 75 i1 = 1, nba
            zr(vtemp+i1-1)=zr(valrow+inda+i1-1)/fak
!-- TERME SUR LA DIAGONALE
            if (zi4(nzrow+inda+i1-1) .eq. j1-1) then
                zr(vtemp+i1-1)=(rbid-dj1)/fak
                call MatSetValues(rt, 1, [int(j1-1,4)], 1, [int(j1-1,4)],&
                                  [dj1], INSERT_VALUES, ierr)
            endif
75      continue
!
!--
!-- ON ORTHOGONALISE LES LIGNES RESTANTES
!--
        do 80 i1 = j1+1, nblag
            call MatGetRow(c, i1-1, nbnzc2, zi4(nzrow2), zr(varow2),&
                           ierr)
            call MatRestoreRow(c, i1-1, int(nbnzc2), zi4(nzrow2), zr(varow2),&
                               ierr)
!
!-- RECHERCHE DES TERMES AU DELA DE LA DIAGONALE
            indb=0
90          continue
            if (zi4(nzrow2+indb) .lt. j1-1) then
                indb=indb+1
                goto 90
            endif
!--
!-- C'EST PAS OPTIMAL, MAIS CA DOIT MARCHER...
!--
!
!-- RECOPIE DE LA LIGNE I1
            call MatSetValues(rt, 1, [int(i1-1,4)], int(nbnzc2), zi4(nzrow2),&
                              zr(varow2), INSERT_VALUES, ierr)
            zi4(nzrow:nzrow+nbnzc)=zi4(nzrowc:nzrowc+nbnzc)
!
            nbb=nbnzc2-indb
            call elg_setint(zi4(nzrow+inda), nba, zi4(nzrow2+indb), nbb, zi4(vtemp2), nbc, 'REMPLI')
!
!-- ET ON LA MODIFIE, SI PAS ORTHOGONALE A LA LIGNE J1
            if (nbc .gt. 0) then
                norm=0.d0
                do 110 k1 = 1, nbc
                    norm=norm+zr(vtemp + zi4(nzrow +inda+k1-1) -1 )*&
                    zr(varow2+indb+ zi4(nzrow2+indb+k1-1) -1 )
110              continue
!
!-- REMPLISSAGE DE L'INTERSECTION
                do 120 k1 = 1, nbc
                    rbid=zr(varow2+indb+ zi4(nzrow2+indb+k1-1) -1 )-&
                    norm*zr(vtemp + zi4(nzrow +inda+k1-1) -1 )
!
                    call MatSetValues(rt, 1, [int(i1-1,4)], 1, &
                         zi4(nzrowc+inda+zi4(nzrow+inda+k1-1)-1),[rbid], INSERT_VALUES, ierr)
120              continue
!
                call MatGetRow(c, i1-1, nbnzc2, zi4(nzrow2), zr(varow2),&
                               ierr)
                call MatRestoreRow(c, i1-1, int(nbnzc2), zi4(nzrow2), zr(varow2), ierr)
                zi4(nzrow:nzrow+nbnzc)=zi4(nzrowc:nzrowc+nbnzc)
!
!-- REMPLISSAGE DE LA DIFFERENCE  J1-I1
!
                call elg_setdif(zi4(nzrow+inda), nba, zi4(nzrow2+indb), nbb, zi4(vtemp2),&
                            nbc, 'REMPLI')
                do 130 k1 = 1, nbc
                    rbid=-norm*zr(vtemp + zi4(nzrow +inda+k1-1) -1)
                    call MatSetValues(rt, 1, [int(i1-1,4)], 1, &
                         zi4(nzrowc+inda + zi4(nzrow +inda+k1-1) -1), [rbid], INSERT_VALUES, ierr)
130              continue
!
            endif
!
80      continue
!
!------------------------!
!--                    --!
!-- ON PERMUTE C et Rt --!
!--                    --!
!------------------------!
!
        call MatAssemblyBegin(rt, MAT_FINAL_ASSEMBLY, ierr)
        call MatAssemblyEnd(rt, MAT_FINAL_ASSEMBLY, ierr)
        call MatDuplicate(rt, MAT_COPY_VALUES, c, ierr)
        call MatDestroy(rt, ierr)
!
10  end do
!
!
    call MatTranspose(c, MAT_INITIAL_MATRIX, r, ierr)
    call MatDestroy(c, ierr)
!
!     -- ménage :
!     ------------
    call jedetr('&&ELG_MATRQR.VAL_NZ_ROW2')
    call jedetr('&&ELG_MATRQR.IND_NZ_ROW2')
    call jedetr('&&ELG_MATRQR.IND_NZ_ROWC')
    call jedetr('&&ELG_MATRQR.DIAG_R')
    call jedetr('&&ELG_MATRQR.VTEMP')
    call jedetr('&&ELG_MATRQR.VTEMP2')
!
!
    102 format (a30,2(2x,i4))
    103 format (a30,3(2x,i4))
#else
    integer :: ct, r, nbphys, nblag
    call utmess('F', 'ELIMLAGR_1')
#endif

end subroutine
