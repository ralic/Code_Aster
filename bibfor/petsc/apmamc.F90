subroutine apmamc(kptsc)
!
! COPYRIGHT (C) 1991 - 2016  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
! person_in_charge: natacha.bereux at edf.fr

use petsc_data_module

    implicit none
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    integer :: kptsc
!----------------------------------------------------------------
!
!  REMPLISSAGE DE LA MATRICE PETSC (INSTANCE NUMERO KPTSC)
!
!  En entrée : la matrice ASTER complète
!  En sortie : les valeurs de la matrice PETSc sont remplies à
!              partir des valeurs de la matrice ASTER
!
!  Rq :
!  - la matrice PETSc n'a pas de stockage symétrique: que la matrice
!    ASTER soit symétrique ou non, la matrice PETSc est stockée en entier
!    (termes non-nuls).
!  - dans le mode "matrice complète" (MC) tous les processeurs connaissent
!    toute la matrice ASTER. Chaque processeur initialise sa partie de la
!    matrice PETSc (ie le bloc de lignes A(low2:high2-1))
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: nsmdi, nsmhc, nz, nvalm, nlong
    integer :: jdxi1, jdxi2, jdval1, jdval2, jvalm, jvalm2
    integer :: k, ilig1, ilig2, nzdeb, nzfin,bs
    integer :: iterm, jterm, nbterm, neq2
    integer :: nbloc, kbloc, k1, k2, k3, fictif
    integer(kind=4), pointer :: new_ieq(:) => null()
    integer(kind=4), pointer :: old_ieq(:) => null()
!
    character(len=19) :: nomat, nosolv
    character(len=16) :: idxi1, idxi2, trans1, trans2
    character(len=14) :: nonu
!
    aster_logical :: lmnsy
!
    real(kind=8) :: valm
    integer, pointer :: smdi(:) => null()
    integer(kind=4), pointer :: smhc(:) => null()
!
    parameter (idxi1 ='&&APMAMC.IDXI1__')
    parameter (idxi2 ='&&APMAMC.IDXI2__')
    parameter (trans1='&&APMAMC.TRANS1_')
    parameter (trans2='&&APMAMC.TRANS2_')
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low2, high2, neq, jcol1, jcol2, low1
    PetscErrorCode ::  ierr
    PetscInt :: one = 1, zero = 0
    Mat :: a
!----------------------------------------------------------------
    call jemarq()
!
!   -- LECTURE DU COMMUN
    nomat = nomat_courant
    nonu = nonu_courant
    nosolv = nosols(kptsc)
    a = ap(kptsc)
    bs=tblocs(kptsc)
    ASSERT(bs.ge.1)

    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', vi4=smhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
!   neq: nb total de ddls
    neq=nsmdi
!   nz: nombre de termes non-nuls (dans la partie triangulaire superieure
!                                             ou inferieure de la matrice)
    nz=smdi(neq)

    fictif=fictifs(kptsc)
    if (fictif.eq.1) then
        new_ieq => new_ieqs(kptsc)%pi4
        old_ieq => old_ieqs(kptsc)%pi4
        ASSERT(size(new_ieq).eq.neq)
        neq2=size(old_ieq)
        ASSERT(neq2.gt.neq)
    else
        neq2=neq
        allocate(new_ieq(neq))
        allocate(old_ieq(neq))
        do k=1,neq
            new_ieq(k)=k
            old_ieq(k)=k
        enddo
    endif
    ASSERT(mod(neq2,bs).eq.0)

!   la matrice est-elle symetrique ?
!   ---------------------------------
    call jelira(nomat//'.VALM', 'NMAXOC', nvalm)
    if (nvalm .eq. 1) then
        lmnsy=.false.
    else if (nvalm.eq.2) then
        lmnsy=.true.
    else
        ASSERT(.false.)
    endif


!   les valeurs de la partie triangulaire superieure de la matrice sont stockees
!   dans valm
!   -----------------------------------------------------------------------------
    call jeveuo(jexnum(nomat//'.VALM', 1), 'L', jvalm)
    call jelira(jexnum(nomat//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nz)
!   si la matrice n'est pas symetrique, on a aussi besoin des valeurs de
!   la partie triangulaire inferieure
    if (lmnsy) then
        call jeveuo(jexnum(nomat//'.VALM', 2), 'L', jvalm2)
        call jelira(jexnum(nomat//'.VALM', 2), 'LONMAX', nlong)
        ASSERT(nlong.eq.nz)
    endif


!   low2  Donne la premiere ligne stockee localement
!   high2 Donne la premiere ligne stockee par le processus de (rang+1)
!   *ATTENTION* ces indices commencent a zero (convention C de PETSc)
!   -----------------------------------------------------------------------------
    call MatGetOwnershipRange(a, low2, high2, ierr)
    ASSERT(ierr.eq.0)
!
    call wkvect(idxi1, 'V V S', neq2, jdxi1)
    call wkvect(idxi2, 'V V S', neq2, jdxi2)
    call wkvect(trans1, 'V V R', neq2, jdval1)
    call wkvect(trans2, 'V V R', neq2, jdval2)


!
!   Le bloc de lignes A(low2:high2-1) est compose de trois sous-blocs:
!            (                               )
!   low2     ( x x x \ o o o | v v v v v v v )
!            ( x x x x \ o o | v v v v v v v )
!   high2-1  ( x x x x x \ o | v v v v v v v )
!            (                               )
!   - bloc C (x) = lower(A(low2:high2-1))
!   - bloc D (o) = upper(A(low2:high2-1,low2:high2-1))
!   - bloc E (v) = A(low2:high2-1,high2:neq2)
!
!   upper(A) est stockee au format CSC.
!   Si A n'est pas symetrique, on stocke egalement
!   lower(A),  au format CSR
!--------------------------------------------------------------------------------


!   -- On commence par s'occuper des blocs C et D
!      Indices C : jcol2
!      Indices F : jcol1, ilig1, ilig2
!------------------------------------------------

    do jcol2 = low2, high2-1
!       -- Les termes non-nuls de A(1:jcol2,jcol2) sont stockes dans valm (nzdeb:nzfin)
!          Si A n'est pas symetrique, les termes non-nuls de A(jcol2,1:jcol2) sont stockes
!          dans valm2 (nzdeb:nzfin)
        iterm=0
        jterm=0
        jcol1=old_ieq(jcol2+1)

        if (jcol1.gt.0) then
            if (jcol1.eq.1) then
                nzdeb = 1
            else
                nzdeb = smdi(jcol1-1) + 1
            endif
            nzfin = smdi(jcol1)
            do k = nzdeb, nzfin
!               -- ilig1 : indice ligne (fortran) du terme courant dans la matrice Aster
                ilig1 = smhc(k)
                ilig2 = new_ieq(ilig1)
! ======
! Bloc C
! ======
!               -- Lecture de la ligne C(jcol2,:)
!               -- Compteur de termes dans la ligne jcol2 de C
                jterm=jterm+1
!               -- si A n'est pas symetrique, on lit valm2
                if (lmnsy) then
                    valm=zr(jvalm2-1+k)
                else
!                   -- si A est symetrique, on lit valm1
                    valm=zr(jvalm-1+k)
                endif
                zr(jdval2+jterm-1)=valm
!               -- on stocke l'indice C de la ligne, c'est
!                  l'indice de la colonne transposee
                zi4(jdxi2+jterm-1)=ilig2-1
! ======
! bloc D
! ======
!               -- il est lu en colonne depuis valm
                if (ilig2 .ge. (low2+1)) then
!                   -- Compteur de termes dans la colonne jcol2 de D
                    iterm=iterm+1
                    valm=zr(jvalm-1+k)
                    zr(jdval1+iterm-1)=valm
!                   -- on stocke l'indice C de la ligne
                    zi4(jdxi1+iterm-1)=ilig2-1
                endif
            end do

!           -- On enleve un terme dans la ligne C(jcol2,:): c'est le terme diagonal
!              que l'on a stocke deux fois (pour C et pour D)
            jterm=jterm-1


!           -- Valeurs de D => on envoie les valeurs de la colonne jcol2
            call MatSetValues(a, to_petsc_int(iterm), zi4(jdxi1), one, [to_petsc_int(jcol2)],&
                              zr(jdval1), INSERT_VALUES, ierr)
            ASSERT(ierr.eq.0)

!           -- Valeurs de C => on envoie les valeurs de la ligne jcol2
            call MatSetValues(a, one, [to_petsc_int(jcol2)], to_petsc_int(jterm), zi4(jdxi2),&
                              zr(jdval2), INSERT_VALUES, ierr)
            ASSERT(ierr.eq.0)
        else
!           -- pour un ddl fictif, on se contente d'ajouter un 1. sur la diagonale :
            call MatSetValues(a, one, [to_petsc_int(jcol2)], one, [to_petsc_int(jcol2)],&
                              [1.d0], INSERT_VALUES, ierr)
        endif
    end do


!  -- Ensuite on finit par le bloc hors diagonal E
!      Indices C : jcol2
!      Indices F : jcol1, ilig1, ilig2
!  --------------------------------------------------
!
!   -- On lit colonne par colonne upper(A( :,high2:))
    do jcol2 = high2, neq2-1
        iterm=0
        jcol1=old_ieq(jcol2+1)
!       -- les ddls fictifs n'ont pas de termes dans le bloc E :
        if (jcol1.eq.0) cycle

        ASSERT(jcol1.ge.2)
        nzdeb = smdi(jcol1-1) + 1
        nzfin = smdi(jcol1)
        do k = nzdeb, nzfin
            ilig1 = smhc(k)
            ilig2 = new_ieq(ilig1)
!           -- On ignore les lignes avant low2
            if (ilig2 .lt. (low2+1)) then
                continue
!           -- On lit et on stocke A(low2+1:high2,jcol2)= E(:,jcol2)
            else if (ilig2.le.high2) then
                iterm=iterm+1
                valm=zr(jvalm-1+k)
                zr(jdval1+iterm-1)=valm
                zi4(jdxi1+iterm-1)=ilig2-1
            else
!               -- On ignore les lignes après high2
                exit
            endif
        end do
!       -- Valeurs de E => on envoie les valeurs de la colonne jcol2
        call MatSetValues(a, to_petsc_int(iterm), zi4(jdxi1), one, [to_petsc_int(jcol2)],&
                          zr(jdval1), INSERT_VALUES, ierr)
    end do

    call jelibe(nonu//'.SMOS.SMDI')
    call jelibe(nonu//'.SMOS.SMHC')
    call jelibe(jexnum(nomat//'.VALM', 1))
    if (lmnsy) call jelibe(jexnum(nomat//'.VALM', 2))

!   -- menage :
    call jedetr(idxi1)
    call jedetr(idxi2)
    call jedetr(trans1)
    call jedetr(trans2)
    if (fictif.eq.0) then
        deallocate(new_ieq)
        deallocate(old_ieq)
    endif


    call jedema()

#endif
!
end subroutine
