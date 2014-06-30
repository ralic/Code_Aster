subroutine apmamc(kptsc)
!
! COPYRIGHT (C) 1991 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
    implicit none
! person_in_charge: thomas.de-soza at edf.fr
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
!    matrice PETSc (ie le bloc de lignes A(low:high-1))
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: nsmdi, nsmhc, n, nz, nvalm, nlong
    integer ::  jsmhc, jdxi1, jdxi2, jdval1, jdval2, jvalm, jvalm2
    integer :: k, ilig,  nzdeb, nzfin
    integer :: iterm, jterm
!
    character(len=19) :: nomat, nosolv
    character(len=16) :: idxi1, idxi2, trans1, trans2
    character(len=14) :: nonu
!
    logical(kind=1) :: lmnsy
!
    real(kind=8) :: valm
    integer, pointer :: smdi(:) => null()
!
    parameter (idxi1 ='&&APMAMC.IDXI1__')
    parameter (idxi2 ='&&APMAMC.IDXI2__')
    parameter (trans1='&&APMAMC.TRANS1_')
    parameter (trans2='&&APMAMC.TRANS2_')
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low, high, neq, dlow, ierr,jcol
    PetscInt :: one = 1, zero = 0 
    Mat :: a
!----------------------------------------------------------------
    call jemarq()
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
    a = ap(kptsc)
!
    call jeveuo(nonu//'.SMOS.SMDI', 'L', vi=smdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
!   n: nb total de ddls
    n=nsmdi
    neq=n
!   nz: nombre de termes non-nuls (dans la partie triangulaire supérieure 
!                                             ou inférieure de la matrice) 
    nz=smdi(n)
!   la matrice est-elle symétrique ?   
    call jelira(nomat//'.VALM', 'NMAXOC', nvalm)
    if (nvalm .eq. 1) then
        lmnsy=.false.
    else if (nvalm.eq.2) then
        lmnsy=.true.
    else
        ASSERT(.false.)
    endif
!   les valeurs de la partie triangulaire supérieure de la matrice sont stockées  
!   dans valm 
    call jeveuo(jexnum(nomat//'.VALM', 1), 'L', jvalm)
    call jelira(jexnum(nomat//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nz)
!   si la matrice n'est pas symétrique, on a aussi besoin des valeurs de 
!   la partie triangulaire inférieure
    if (lmnsy) then
        call jeveuo(jexnum(nomat//'.VALM', 2), 'L', jvalm2)
        call jelira(jexnum(nomat//'.VALM', 2), 'LONMAX', nlong)
        ASSERT(nlong.eq.nz)
    endif
!
!     low DONNE LA PREMIERE LIGNE STOCKEE LOCALEMENT
!     high DONNE LA PREMIERE LIGNE STOCKEE PAR LE PROCESSUS DE (RANG+1)
!     *ATTENTION* CES INDICES COMMENCENT A ZERO (CONVENTION C DE PETSc)
    call MatGetOwnershipRange(a, low, high, ierr)
    ASSERT(ierr.eq.0)
!
    call wkvect(idxi1, 'V V S', n, jdxi1)
    call wkvect(idxi2, 'V V S', n, jdxi2)
    call wkvect(trans1, 'V V R', n, jdval1)
    call wkvect(trans2, 'V V R', n, jdval2)
!
    iterm=0
    jterm=0
!
!     CAS OU ON POSSEDE LE PREMIER BLOC DE LIGNES:
!     on initialise directement la valeur du premier terme A(0,0) 
    if (low .eq. 0) then
        call MatSetValue(a, zero, zero, zr(jvalm), INSERT_VALUES, ierr)
        dlow=1
    else
        dlow=0
    endif
!
!   Le bloc de lignes A(low:high-1) est composé de trois sous-blocs:
!           (                               )
!   low     ( x x x \ o o o | v v v v v v v ) 
!           ( x x x x \ o o | v v v v v v v )
!   high-1  ( x x x x x \ o | v v v v v v v )
!           (                               )
!   - bloc C (x) = lower(A(low:high-1)) 
!   - bloc D (o) = upper(A(low:high-1,low:high-1))
!   - bloc E (v) = A(low:high-1,high:n)
!   
!   upper(A) est stockée au format CSC.
!   Si A n'est pas symétrique, on stocke également  
!   lower(A),  au format CSR 
!   
!   
! 
!  ON COMMENCE PAR S'OCCUPER DES BLOCS C ET D
!
    do jcol = low+dlow, high-1
    ! Les termes non-nuls de A(1:jcol,jcol) sont stockés dans valm (nzdeb:nzfin)
    ! Si A n'est pas symétrique, les termes non-nuls de A(jcol,1:jcol) sont stockés
    ! dans valm2 (nzdeb:nzfin) 
        nzdeb = smdi(jcol) + 1
        nzfin = smdi(jcol+1)
        do k = nzdeb, nzfin
        ! indice ligne (fortran) du terme courant dans la matrice Aster 
            ilig = zi4(jsmhc-1+k)
            ! ======
            ! Bloc C 
            ! ======
            ! Lecture de la ligne C(jcol,:) 
            ! Compteur de termes dans la ligne jcol de C 
            jterm=jterm+1  
            ! si A n'est pas symétrique, on lit valm2 
            if (lmnsy) then
                valm=zr(jvalm2-1+k)
            else
            ! si A est symétrique, on lit valm1 
                valm=zr(jvalm-1+k)
            endif
            zr(jdval2+jterm-1)=valm
            ! on stocke l'indice C de la ligne, c'est 
            ! l'indice de la colonne transposée 
            zi4(jdxi2+jterm-1)=ilig-1
            ! ======
            ! bloc D 
            ! ======
            ! il est lu en colonne depuis valm
            if (ilig .ge. (low+1)) then
               ! Compteur de termes dans la colonne jcol de D
               iterm=iterm+1
               valm=zr(jvalm-1+k)
               zr(jdval1+iterm-1)=valm
               ! on stocke l'indice C de la ligne
               zi4(jdxi1+iterm-1)=ilig-1
            endif
        end do
        ! On enlève un terme dans la ligne C(jcol,:): c'est le terme diagonal
        ! que l'on a stocké deux fois (pour C et pour D) 
        jterm=jterm-1
        ! Valeurs de D => on envoie les valeurs de la colonne jcol 
        call MatSetValues(a, to_petsc_int(iterm), zi4(jdxi1), one, [to_petsc_int(jcol)],&
                          zr(jdval1), INSERT_VALUES, ierr)
        ! Valeurs de C => on envoie les valeurs de la ligne jcol
        call MatSetValues(a, one, [to_petsc_int(jcol)], to_petsc_int(jterm), zi4(jdxi2),&
                          zr(jdval2), INSERT_VALUES, ierr)
        iterm=0
        jterm=0
    end do
!
!     ENSUITE ON FINIT PAR LE BLOC HORS DIAGONAL E
!
!   
! On lit colonne par colonne upper(A( :,high:))
    do jcol = high, neq-1
        nzdeb = smdi(jcol) + 1
        nzfin = smdi(jcol+1)
        do k = nzdeb, nzfin
            ilig = zi4(jsmhc-1+k)
            ! On ignore les lignes avant low 
            if (ilig .lt. (low+1)) then
                continue
            ! On lit et on stocke A(low+1:high,jcol)= E(:,jcol)
            else if (ilig.le.high) then
                iterm=iterm+1
                valm=zr(jvalm-1+k)
                zr(jdval1+iterm-1)=valm
                zi4(jdxi1+iterm-1)=ilig-1
            else
            ! On ignore les lignes après high
                exit
            endif
        end do
         ! Valeurs de E => on envoie les valeurs de la colonne jcol 
        call MatSetValues(a, to_petsc_int(iterm), zi4(jdxi1), one, [to_petsc_int(jcol)],&
                          zr(jdval1), INSERT_VALUES, ierr)
        iterm=0
    end do
!
    call jelibe(nonu//'.SMOS.SMDI')
    call jelibe(nonu//'.SMOS.SMHC')
    call jelibe(jexnum(nomat//'.VALM', 1))
    if (lmnsy) call jelibe(jexnum(nomat//'.VALM', 2))
!
!     ON N'OUBLIE PAS DE DETRUIRE LES TABLEAUX
!     APRES AVOIR ALLOUE CORRECTEMENT
    call jedetr(idxi1)
    call jedetr(idxi2)
    call jedetr(trans1)
    call jedetr(trans2)
!
    call jedema()
!
#endif
!
end subroutine
