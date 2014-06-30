subroutine apmamd(kptsc)
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
! person_in_charge: nicolas.sellenet at edf.fr
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterfort/asmpi_info.h"
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
!  DANS LE CAS MATR_DISTRIBUEE
!
!  En entrée : la matrice locale ASTER  
!  En sortie : les valeurs de la matrice PETSc sont remplies à 
!              partir des valeurs de la matrice ASTER
!
!  Rq : 
!  - la matrice PETSc n'a pas de stockage symétrique: que la matrice 
!    ASTER soit symétrique ou non, la matrice PETSc est stockée en entier    
!    (termes non-nuls).
!  - dans le mode "matrice distribuée" chaque processeur connait une partie de 
!    la matrice ASTER, la matrice ASTER "locale" Aloc. 
!    Il initialise sa partie de matrice PETSc (i.e. le bloc de lignes A(low:high-1)). 
!    La matrice locale ASTER et la matrice locale PETSc sont différentes. 
!    Lors du MatSetValues, le processeur local envoie aux autres processeurs
!    les valeurs dont ils ont besoin et il récupère les valeurs
!    lui permettant d'initialiser son bloc. C'est PETSc qui gère les 
!    communications entre processeurs (qui possède quoi etc ), et cette gestion 
!    est cachée. Ici chaque proc envoie *toutes* les valeurs qu'il possède, sans
!    savoir à qui.    
!
!----------------------------------------------------------------
!
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "asterf_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: nsmdi, nsmhc, nz, nvalm, nlong
    integer :: jsmdi, jsmhc, jdxi1, jdxi2, jdval1, jdval2, jvalm, jvalm2
    integer :: k, iligl, jcoll, nzdeb, nzfin
    integer :: iterm, jterm, jcolg, iligg, jnugll
    integer :: jnequ, nloc, nglo,  jnequl
!
    character(len=19) :: nomat, nosolv
    character(len=16) :: idxi1, idxi2, trans1, trans2
    character(len=14) :: nonu
!
    logical(kind=1) :: lmnsy
!
    real(kind=8) :: valm
!
    parameter (idxi1 ='&&APMAMD.IDXI1__')
    parameter (idxi2 ='&&APMAMD.IDXI2__')
    parameter (trans1='&&APMAMD.TRANS1_')
    parameter (trans2='&&APMAMD.TRANS2_')
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: neql, neqg, ierr
    PetscInt :: one = 1 
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
    call jeveuo(nonu//'.SMOS.SMDI', 'L', jsmdi)
    call jelira(nonu//'.SMOS.SMDI', 'LONMAX', nsmdi)
    call jeveuo(nonu//'.SMOS.SMHC', 'L', jsmhc)
    call jelira(nonu//'.SMOS.SMHC', 'LONMAX', nsmhc)
    call jeveuo(nonu//'.NUME.NEQU', 'L', jnequ)
    call jeveuo(nonu//'.NUML.NEQU', 'L', jnequl)
    call jeveuo(nonu//'.NUML.NLGP', 'L', jnugll)
    nloc = zi(jnequl)
    nglo = zi(jnequ)
    neql = nloc
    neqg = nglo
    nz=zi(jsmdi-1+nloc)
!
! La matrice Aster est-elle symétrique ? 
    call jelira(nomat//'.VALM', 'NMAXOC', nvalm)
    if (nvalm .eq. 1) then
        lmnsy=.false.
    else if (nvalm.eq.2) then
        lmnsy=.true.
    else
        ASSERT(.false.)
    endif
! Vérification de la cohérence entre le(s) tableau(x) stockant les 
! valeurs de la matrice nomat et sa structure creuse (telle que définie 
! dans nonu)
    call jeveuo(jexnum(nomat//'.VALM', 1), 'L', jvalm)
    call jelira(jexnum(nomat//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nz)
    if (lmnsy) then
        call jeveuo(jexnum(nomat//'.VALM', 2), 'L', jvalm2)
        call jelira(jexnum(nomat//'.VALM', 2), 'LONMAX', nlong)
        ASSERT(nlong.eq.nz)
    endif
!
    call wkvect(idxi1, 'V V S', nloc, jdxi1)
    call wkvect(idxi2, 'V V S', nloc, jdxi2)
    call wkvect(trans1, 'V V R', nloc, jdval1)
    call wkvect(trans2, 'V V R', nloc, jdval2)
!
    iterm=0
    jterm=0
!
!  Recopie de la matrice
!  C'est PETSc qui s'occupe de la recopie des termes vers
!  le bon processeur
! 
! Envoi de Aloc(1,1)
    call MatSetValue(a, to_petsc_int(zi(jnugll)-1), to_petsc_int(zi(jnugll)-1), &
                     zr(jvalm), ADD_VALUES, ierr)
    ASSERT(ierr==0)
! 
    do jcoll = 2, nloc
        nzdeb = zi(jsmdi+jcoll-2) + 1
        nzfin = zi(jsmdi+jcoll-1)
        ! Indice colonne global (F) de la colonne locale jcoll
        jcolg = zi(jnugll+jcoll-1)
        do k = nzdeb, nzfin
            iligl = zi4(jsmhc-1+k)
            iligg = zi(jnugll-1+iligl)
            ! Compteur de termes sur la colonne locale jcoll
            iterm=iterm+1
            valm=zr(jvalm-1+k)
            ! Stockage dans val1 de A(iligg,jcolg)  
            zr(jdval1+iterm-1)=valm
            ! et de son indice ligne global (C)
            zi4(jdxi1+iterm-1)=iligg-1
            ! On passe à la *ligne* jcoll
            if (iligg .ne. jcolg) then
              ! Attention, il ne faut pas stocker le terme diagonal A(jcolg, jcolg) 
              ! qui a déjà été rencontré dans la *colonne* jcoll
              ! Compteur de termes sur la ligne jcoll
              jterm=jterm+1  
              if ( .not. lmnsy ) then 
                ! si la matrice ASTER est symétrique
                ! la ligne jcoll est la transposée de la colonne jcoll  
                ! on reprend la valeur lue depuis valm
                valm=zr(jvalm-1+k)  
              else 
              ! si la matrice ASTER n'est pas symétrique
              ! on lit les termes de la ligne jcoll depuis valm2
                valm=zr(jvalm2-1+k) 
              endif
              ! on stocke dans val2 
              zr(jdval2+jterm-1)=valm
              ! avec l'indice colonne global (C) correspondant 
              zi4(jdxi2+jterm-1)=iligg-1
            endif
        end do
        ! Envoi de la colonne jcolg 
        call MatSetValues(a, to_petsc_int(iterm), zi4(jdxi1), one, [to_petsc_int(jcolg-1)],&
                          zr(jdval1), ADD_VALUES, ierr)
        ASSERT(ierr==0)
        ! Envoi de la ligne jcolg
        call MatSetValues(a, one, [to_petsc_int(jcolg-1)], to_petsc_int(jterm), zi4(jdxi2),&
                          zr(jdval2), ADD_VALUES, ierr)      
        ASSERT(ierr==0) 
        iterm=0
        jterm=0
    end do
!
    call jelibe(nonu//'.SMOS.SMDI')
    call jelibe(nonu//'.SMOS.SMHC')
    call jelibe(jexnum(nomat//'.VALM', 1))
    if (lmnsy) call jelibe(jexnum(nomat//'.VALM', 2))
!
    call jedetr(idxi1)
    call jedetr(idxi2)
    call jedetr(trans1)
    call jedetr(trans2)
!
    call jedema()
!
#else
    integer :: idummy
    idummy = kptsc
#endif
!
end subroutine
