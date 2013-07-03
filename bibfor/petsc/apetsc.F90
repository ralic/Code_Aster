subroutine apetsc(action, solvez, matasz, rsolu, vcinez,&
                  nbsol, istop, iret)
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
! person_in_charge: thomas.desoza at edf.fr
#include "jeveux.h"
#include "asterfort/apmain.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtmchc.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
#include "blas/dcopy.h"
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
!     /'FIN'     : POUR FERMER DEFINITIVEMENT PETSC
!                  NECESSAIRE POUR DECLENCHER L'AFFICHAGE DU PROFILING
!
! IN  : SOLVEU   (K19) : NOM DE LA SD SOLVEUR
!                       (SI ACTION=PRERES/RESOUD)
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
#include "aster_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: iprem, k, ibid, ierd, nglo, kdeb, jnequ
    integer :: jrefa, jtrav, kptsc
    integer(kind=4) :: n4
!
    character(len=19) :: solveu, matas, vcine
    character(len=14) :: nu
    character(len=4) :: etamat, kbid
    character(len=1) :: rouc
!
!
!     Variables PETSc
    PetscInt :: m, n, ierr
!----------------------------------------------------------------
!     INITIALISATION DE PETSC A FAIRE AU PREMIER APPEL
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
!     0. FERMETURE DE PETSC DANS FIN
!     ------------------------------
    if (action .eq. 'FIN') then
!       PETSC A-T-IL ETE INITIALISE ?
        if (iprem .eq. 1) then
            call PetscFinalize(ierr)
!         ON NE VERIFIE PAS LE CODE RETOUR CAR ON PEUT
!         SE RETROUVER DANS FIN SUITE A UNE ERREUR DANS L'INITIALISATION
            iprem = 0
        endif
        goto 9999
    endif
!
    if (iprem .eq. 0) then
!     --------------------
        call PetscInitialize(PETSC_NULL_CHARACTER, ierr)
        if (ierr .ne. 0) then
            call u2mess('F', 'PETSC_1')
        endif
        do k = 1, nmxins
            ap(k) = 0
            kp(k) = 0
            nomats(k) = ' '
            nosols(k) = ' '
            nonus(k) = ' '
        enddo
        xlocal = 0
        xglobal = 0
        xscatt = 0
        spsomu = ' '
        spmat = ' '
        spsolv = ' '
        iprem = 1
    endif
!
    call assert(matas.ne.' ')
    call jelira(matas//'.VALM', 'TYPE', ibid, rouc)
!
!     1. INFORMATIONS SUR LA MATRICE :
!     --------------------------------
    if (rouc .ne. 'R') then
        call u2mess('F', 'PETSC_2')
    endif
!
!     2. ON CHERCHE UNE PLACE EN MEMOIRE :
!     ------------------------------------
!     ON TESTE LE NOM DE LA MATRICE, CELUI DU NUME_DDL,
!     LA TAILLE DES MATRICES ASTER ET PETSC
    call dismoi('F', 'NOM_NUME_DDL', matas, 'MATR_ASSE', ibid,&
                nu, ierd)
    call jeveuo(nu//'.NUME.NEQU', 'L', jnequ)
    nglo = zi(jnequ)
!
    kptsc=1
    do k = 1, nmxins
        if ((nomats(k).eq.matas) .and. (nonus (k).eq.nu )) then
            call assert(ap(k).ne.0)
            call MatGetSize(ap(k), m, n, ierr)
            call assert(ierr.eq.0)
            if (m .eq. n .and. nglo .eq. n) then
                kptsc = k
                goto 1
            endif
        endif
    enddo
!
    call assert(action.ne.'RESOUD')
!
    if (action .eq. 'DETR_MAT') then
        goto 9999
    endif
!
!     Y-A-T-IL ENCORE UNE PLACE LIBRE ?
    do k = 1, nmxins
        if (nomats(k) .eq. ' ') then
            kptsc = k
            goto 1
        endif
    end do
!
    call u2mess('F', 'PETSC_3')
!
 1  continue
!
!     3. QUELQUES VERIFICATIONS ET PETITES ACTIONS :
!     ----------------------------------------------
    if (action .eq. 'PRERES') then
!        -- REMPLISSAGE DU COMMUN
        call assert(nomats(kptsc).eq.' ')
        call assert(nosols(kptsc).eq.' ')
        call assert(nonus(kptsc).eq.' ')
        call assert(matas .ne.' ')
        call assert(solveu.ne.' ')
        call assert(nu .ne.' ')
        nomats(kptsc) = matas
        nosols(kptsc) = solveu
        nonus(kptsc) = nu
!
!        -- VERIFICATION QUE LA MATRICE N'A PAS ETE FACTORISEE
        call jeveuo(matas//'.REFA', 'E', jrefa)
        etamat = zk24(jrefa-1+8)
        if (etamat .eq. 'DECT') then
            call u2mess('A', 'PETSC_4')
            goto 9999
        else
            zk24(jrefa-1+8) = 'DECT'
        endif
!
!        -- ELIMINATION DES DDLS (AFFE_CHAR_CINE)
        call assert(zk24(jrefa-1+3).ne.'ELIMF')
        if (zk24(jrefa-1+3) .eq. 'ELIML') call mtmchc(matas, 'ELIMF')
        call assert(zk24(jrefa-1+3).ne.'ELIML')
!
    else if (action.eq.'RESOUD') then
        call assert(nbsol.ge.1)
        call assert((istop.eq.0).or.(istop.eq.2))
    else if (action.eq.'DETR_MAT') then
!        RIEN A VERIFIER
    else
        call assert(.false.)
    endif
!
!     4. APPEL DE PETSC :
!     -------------------
!
    if (action .eq. 'RESOUD') then
        call wkvect('&&APETSC.TRAVAIL', 'V V R', nglo, jtrav)
        n4 = nglo
        do k = 1, nbsol
            kdeb = (k-1)*nglo+1
            call dcopy(n4, rsolu(kdeb), 1, zr(jtrav), 1)
            call apmain(action, kptsc, zr(jtrav), vcine, istop,&
                        iret)
            call dcopy(n4, zr(jtrav), 1, rsolu(kdeb), 1)
        end do
        call jedetr('&&APETSC.TRAVAIL')
    else
        call apmain(action, kptsc, rsolu, vcine, istop,&
                    iret)
    endif
!
9999  continue
!
    call jedema()
!
#else
!
    call u2mess('F', 'FERMETUR_10')
!
#endif
!
end subroutine
