subroutine apetsc(action, solvez, matasz, rsolu, vcinez,&
                  nbsol, istop, iret)
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
!
use petsc_data_module
!
    implicit none
!
#include "asterf_types.h"
#include "asterf.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/apmain.h"
#include "asterfort/apldlt.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/elg_apelim.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtmchc.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/dcopy.h"
!
    character(len=*) :: action, solvez, matasz, vcinez
    real(kind=8) :: rsolu(*)
    integer :: nbsol, istop, iret
!-----------------------------------------------------------------------
! BUT : ROUTINE D'INTERFACE ENTRE CODE_ASTER ET LA BIBLIOTHEQUE PETSC
!       DE RESOLUTION DE SYSTEMES LINEAIRES.
!
! IN  : ACTION
!     /'DETR_MAT': POUR DETRUIRE L'INSTANCE PETSC ASSOCIEE A UNE MATRICE
!     /'PRERES'  : POUR CONSTRUIRE LE PRECONDITIONNEUR
!                 (ATTENTION EN // LA CONSTRUCTION DE CERTAINS PC EST
!                  RETARDEE)
!     /'RESOUD'  : POUR RESOUDRE LE SYSTEME LINEAIRE
!     /'ELIM_LAGR[+/-]R'  : CALCULE LES MATRICES NECESSAIRES A
!                         LA FONCTIONNALITE ELIM_LAGR='OUI'
!             'ELIM_LAGR+R' : on calcule la matrice R
!             'ELIM_LAGR-R' : on ne calcule pas la matrice R
!     /'FIN'     : POUR FERMER DEFINITIVEMENT PETSC
!                  NECESSAIRE POUR DECLENCHER L'AFFICHAGE DU PROFILING
!
! IN  : SOLVEU   (K19) : NOM DE LA SD SOLVEUR
!                       (SI ACTION=PRERES/ELIM_LAGR[+/-]R)
! IN  : MATASS   (K19) : NOM DE LA MATR_ASSE
!                       (SI ACTION=PRERES/RESOUD)
! I/O : RSOLU      (R) : EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                        EN SORTIE : VECTEUR SOLUTION      (REEL)
!                       (SI ACTION=RESOUD)
! IN  : VCINE    (K19) : NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!                       (SI ACTION=RESOUD)
! IN  : NBSOL      (I) : NOMBRE DE SYSTEMES A RESOUDRE
! IN  : ISTOP      (I) : COMPORTEMENT EN CAS D'ERREUR
! OUT : IRET       (I) : CODE_RETOUR
!                      /  0 : OK
!                      /  1 : NOMBRE MAX D'ITERATIONS ATTEINT
!-----------------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "asterf_petsc.h"
!
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: iprem, k, nglo, kdeb, jnequ
    integer ::  kptsc
    integer :: np
    real(kind=8) :: r8
    PetscInt :: m, n
!
    character(len=19) :: solveu, matas, vcine
    character(len=14) :: nu
    character(len=4) :: etamat
    character(len=1) :: rouc
    real(kind=8), pointer :: travail(:) => null()
    character(len=24), pointer :: refa(:) => null()
!
!----------------------------------------------------------------
!
!     Variables PETSc
    PetscErrorCode :: ierr
    Mat :: mbid
    Vec :: vbid
    PetscScalar :: sbid
    PetscOffset :: offbid
    PetscReal :: rbid

!----------------------------------------------------------------
!   INITIALISATION DE PETSC A FAIRE AU PREMIER APPEL
    save iprem
    data iprem /0/
!----------------------------------------------------------------
    call jemarq()
!
    solveu = solvez
    matas = matasz
    vcine = vcinez
    iret = 0
!
!
!   0. FERMETURE DE PETSC DANS FIN
!   ------------------------------
    if (action .eq. 'FIN') then
!       petsc a-t-il ete initialise ?
        if (iprem .eq. 1) then
            call PetscFinalize(ierr)
!           on ne verifie pas le code retour car on peut
!           se retrouver dans fin suite a une erreur dans l'initialisation
            iprem = 0
        endif
        goto 999
    endif
!
!
    if (iprem .eq. 0) then
!     --------------------
!        -- quelques verifications sur la coherence Aster / Petsc :
        ASSERT(kind(rbid).eq.kind(r8))
        ASSERT(kind(sbid).eq.kind(r8))
        ASSERT(kind(mbid).eq.kind(np))
        ASSERT(kind(vbid).eq.kind(np))
        ASSERT(kind(offbid).eq.kind(np))
!
        call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
        if (ierr .ne. 0) call utmess('F', 'PETSC_1')
        call PetscInitializeFortran(ierr)
        ASSERT(ierr .eq. 0)
        do k = 1, nmxins
            ap(k) = 0
            kp(k) = 0
            nomats(k) = ' '
            nosols(k) = ' '
            nonus(k) = ' '
            tblocs(k) = -1
            if (fictifs(k).eq.1) then
                deallocate(new_ieqs(k)%pi4)
                deallocate(old_ieqs(k)%pi4)
            endif
            fictifs(k) = -1
        enddo
        xlocal = 0
        xglobal = 0
        xscatt = 0
        spsomu = ' '
        spmat = ' '
        spsolv = ' '
        iprem = 1
    endif
    ASSERT(matas.ne.' ')


!   1. On ne veut pas de matrice complexe :
!   ----------------------------------------
    call jelira(matas//'.VALM', 'TYPE', cval=rouc)
    if (rouc .ne. 'R') call utmess('F', 'PETSC_2')


!   2. On cherche si la matrice est dans le common :
!   -------------------------------------------------
!   On teste le nom de la matrice, celui du nume_ddl,
!   et la taille des matrices aster et petsc
    call dismoi('NOM_NUME_DDL', matas, 'MATR_ASSE', repk=nu)
    call jeveuo(nu//'.NUME.NEQU', 'L', jnequ)
    nglo = zi(jnequ)
!
    kptsc=1
    do k = 1, nmxins
        if ((nomats(k).eq.matas) .and. (nonus (k).eq.nu )) then
            ASSERT(ap(k).ne.0)
            call MatGetSize(ap(k), m, n, ierr)
            ASSERT(ierr.eq.0)
            ASSERT(m.eq.n)
            if (fictifs(k).eq.1) then
                ASSERT(n.ge.nglo)
                ASSERT(size(new_ieqs(k)%pi4).eq.nglo)
                kptsc = k
                goto 1
            else
                if (nglo .eq. n) then
                    kptsc = k
                    goto 1
                endif
            endif
        endif
    enddo
!
    if ( action == 'DETR_MAT' ) then
! La matrice n'est pas stockée dans le tableau ap()
! On ne fait rien   
       goto 999
    endif 
    ASSERT(action.ne.'RESOUD')

 

!   Y-a-t-il encore une place libre dans le common ? Calcul de kptsc :
!   ------------------------------------------------------------------
    do k = 1, nmxins
        if (nomats(k) .eq. ' ') then
            kptsc = k
            goto 1
        endif
    end do
    call utmess('F', 'PETSC_3')
 1  continue

   if (action .eq. 'DETR_MAT') then
! La matrice est stockée dans le tableau ap, à l'indice kptsc
! on la détruit 
      call apmain( action, kptsc, rsolu, vcine, istop, iret )  
      goto 999
    endif


!   3. Quelques verifications et petites actions :
!   ----------------------------------------------
!
    if (action .eq. 'PRERES') then
!        -- remplissage du commun
        ASSERT(nomats(kptsc).eq.' ')
        ASSERT(nosols(kptsc).eq.' ')
        ASSERT(nonus(kptsc).eq.' ')
        ASSERT(matas .ne.' ')
        ASSERT(solveu.ne.' ')
        ASSERT(nu .ne.' ')
        nomats(kptsc) = matas
        nonus(kptsc) = nu
        nosols(kptsc) = solveu
!
    else if (action.eq.'RESOUD') then
        ASSERT(nbsol.ge.1)
        ASSERT((istop.eq.0).or.(istop.eq.2))

    else if (action.eq.'ELIM_LAGR') then
!        -- remplissage du commun spetsc
        nomats(kptsc) = matas
        nonus(kptsc) = nu
        nosols(kptsc) = solveu
        tblocs(kptsc) = -1
        fictifs(kptsc) = -1
        new_ieqs(kptsc)%pi4 => null()
        old_ieqs(kptsc)%pi4 => null()

    endif


!   4. Si LDLT_INC, il faut renumeroter la matrice (RCMK) :
!   --------------------------------------------------------
    call apldlt(kptsc,action,'PRE',rsolu,vcine,nbsol)


!   5. Verifications + elimination des ddls (affe_char_cine)
!   --------------------------------------------------------
    call jeveuo(nomat_courant//'.REFA', 'E', vk24=refa)
    if (action .eq. 'PRERES' .or. nomat_courant.eq.'&&apldlt.matr') then
!
!       -- Verification que la matrice n'a pas deja ete factorisee
        etamat = refa(8)
        if (etamat .eq. 'DECT') then
            call utmess('A', 'PETSC_4')
            goto 999
        else
            refa(8) = 'DECT'
        endif
!
!        -- elimination des ddls (affe_char_cine)
        ASSERT(refa(3).ne.'ELIMF')
        if (refa(3) .eq. 'ELIML') call mtmchc(nomat_courant, 'ELIMF')
        ASSERT(refa(3).ne.'ELIML')

    else if (action.eq.'ELIM_LAGR') then
        call elg_apelim(kptsc)
        iret=0
        goto 999
    endif


!   5. APPEL DE PETSC :
!   -------------------
    if (action .eq. 'RESOUD') then
        AS_ALLOCATE(vr=travail, size=nglo)
        do k = 1, nbsol
            kdeb = (k-1)*nglo+1
            call dcopy(nglo, rsolu(kdeb), 1, travail, 1)
            call apmain(action, kptsc, travail, vcine, istop,&
                        iret)
            call dcopy(nglo, travail, 1, rsolu(kdeb), 1)
        end do
        AS_DEALLOCATE(vr=travail)
    else
        call apmain(action, kptsc, rsolu, vcine, istop, iret)
    endif


!   6. Si LDLT_INC, il faut revenir a la numerotation initiale :
!   -------------------------------------------------------------
    call apldlt(kptsc,action,'POST',rsolu,vcine,nbsol)


999 continue
    call jedema()
#else
    call utmess('F', 'FERMETUR_10')
#endif
end subroutine
