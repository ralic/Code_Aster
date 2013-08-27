subroutine apmain(action, kptsc, rsolu, vcine, istop,&
                  iret)
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
#include "aster_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterc/matfpe.h"
#include "asterfort/apalmc.h"
#include "asterfort/apalmd.h"
#include "asterfort/apksp.h"
#include "asterfort/apmamc.h"
#include "asterfort/apmamd.h"
#include "asterfort/appcpr.h"
#include "asterfort/appcrs.h"
#include "asterfort/apsolu.h"
#include "asterfort/apvsmb.h"
#include "asterfort/assert.h"
#include "asterfort/csmbgg.h"
#include "asterfort/detrsd.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mrconl.h"
#include "asterfort/mtdscr.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mesr.h"
#include "asterfort/u2mess.h"
    character(len=*) :: action
    integer :: kptsc
    real(kind=8) :: rsolu(*)
    character(len=19) :: vcine
    integer :: istop, iret
!--------------------------------------------------------------
!
! IN  : ACTION :
!     /'DETR_MAT': POUR DETRUIRE L'INSTANCE PETSC ASSOCIEE A UNE MATRICE
!     /'PRERES'  : POUR CONSTRUIRE LE PRECONDITIONNEUR
!                 (ATTENTION EN // LA CONSTRUCTION DE CERTAINS PC EST
!                  RETARDEE)
!     /'RESOUD'  : POUR RESOUDRE LE SYSTEME LINEAIRE
!
! IN  : KPTSC (I): INDICE DES INSTANCES PETSC DANS Ap,Kp
! I/O : RSOLU (R): EN ENTREE : VECTEUR SECOND MEMBRE (REEL)
!                  EN SORTIE : VECTEUR SOLUTION      (REEL)
!                 (SI ACTION=RESOUD)
! IN  : VCINE (K19): NOM DU CHAM_NO DE CHARGEMENT CINEMATIQUE
!                   (SI ACTION=RESOUD)
! IN  : ISTOP (I)  : COMPORTEMENT EN CAS D'ERREUR
! OUT : IRET  (I)  : CODE RETOUR
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "aster_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: ifm, niv, ierd, ibid, nmaxit, ptserr, jnequ
    integer :: lmat, idvalc, jslvi, jslvk, jslvr
    mpi_int :: mpicou
!
    character(len=24) :: precon
    character(len=19) :: nomat, nosolv
    character(len=14) :: nonu
    character(len=1) :: rouc
!
    real(kind=8) :: divtol, resipc
    complex(kind=8) :: cbid
!
    logical :: lmd
!
!
!----------------------------------------------------------------
!     Variables PETSc
!
    PetscInt :: ierr, neq, i
    PetscInt :: maxits
    PetscReal :: rtol, atol, dtol
    Vec :: r
    PetscScalar :: ires, fres
    VecScatter :: ctx
    KSPConvergedReason :: indic
    Mat :: a
    KSP :: ksp
!----------------------------------------------------------------
    call jemarq()
!---- COMMUNICATEUR MPI DE TRAVAIL
    call asmpi_comm('GET', mpicou)
!
!     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LES MKL
    call matfpe(-1)
!
    call infniv(ifm, niv)
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', jslvk)
    lmd = zk24(jslvk-1+10)(1:3).eq.'OUI'
!
    if (action .eq. 'PRERES') then
!     ----------------------------
!
!        1.1 CREATION ET PREALLOCATION DE LA MATRICE PETSc :
!        ---------------------------------------------------
!
        if (lmd) then
            call apalmd(kptsc)
        else
            call apalmc(kptsc)
        endif
!
!        1.2 COPIE DE LA MATRICE ASTER VERS LA MATRICE PETSc :
!        -----------------------------------------------------
!
        if (lmd) then
            call apmamd(kptsc)
        else
            call apmamc(kptsc)
        endif
!
!        1.3 ASSEMBLAGE DE LA MATRICE PETSc :
!        ------------------------------------
!
        call MatAssemblyBegin(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
        ASSERT(ierr.eq.0)
        call MatAssemblyEnd(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
        ASSERT(ierr.eq.0)
!
!        1.4 CREATION DU PRECONDITIONNEUR PETSc (EXTRAIT DU KSP) :
!        ---------------------------------------------------------
!
        call KSPCreate(mpicou, kp(kptsc), ierr)
        ASSERT(ierr.eq.0)
!
        call KSPSetOperators(kp(kptsc), ap(kptsc), ap(kptsc), DIFFERENT_NONZERO_PATTERN, ierr)
        ASSERT(ierr.eq.0)
!
        call appcpr(kptsc)
!
    else if (action.eq.'RESOUD') then
!     ---------------------------------
!
!        2.0 RECUPERATION DES POINTEURS DANS LE COMMUN :
!        -----------------------------------------------
!
        a = ap(kptsc)
        ksp = kp(kptsc)
!
!        2.1 PRETRAITEMENT DU SECOND MEMBRE :
!        ------------------------------------
!
!        -- MISE A L'ECHELLE DES LAGRANGES DANS LE SECOND MEMBRE
        call mtdscr(nomat)
        call jeveuo(nomat//'.&INT', 'L', lmat)
        call mrconl('MULT', lmat, 0, 'R', rsolu,&
                    1)
!
!        -- PRISE EN COMPTE DES CHARGES CINEMATIQUES :
        call jeexin(vcine//'.VALE', ierd)
        if (ierd .ne. 0) then
            call jeveuo(vcine//'.VALE', 'L', idvalc)
            call jelira(vcine//'.VALE', 'TYPE', ibid, rouc)
            ASSERT(rouc.eq.'R')
            call csmbgg(lmat, rsolu, zr(idvalc), cbid, cbid,&
                        'R')
        endif
!
!        2.2 CREATION DU VECTEUR PETSc :
!        -------------------------------
!
        call apvsmb(kptsc, lmd, rsolu)
!
!        2.3 PARAMETRES DU KSP :
!        -----------------------
!
        call apksp(kptsc)
!
!        2.3b CREATION DES PRECONDITIONNEURS RETARDES :
!        ----------------------------------------------
!
        call appcrs(kptsc, lmd)
!
!        2.4 RESOLUTION :
!        ----------------
!
        call VecDuplicate(b, x, ierr)
        ASSERT(ierr.eq.0)
        call KSPSolve(ksp, b, x, ierr)
!
!        2.5 DIAGNOSTIC :
!        ----------------
!
!        ARRET ANORMAL DU KSP
        if (ierr .gt. 0) then
            call u2mess('F', 'PETSC_13')
        endif
!
!        ANALYSE DE LA CONVERGENCE DU KSP
        call KSPGetConvergedReason(ksp, indic, ierr)
        ASSERT(ierr.eq.0)
!
!        ANALYSE DES CAUSES ET EMISSION EVENTUELLE D'UN MESSAGE
!        EN CAS DE DIVERGENCE
        if (indic .lt. 0) then
            call KSPGetTolerances(ksp, rtol, atol, dtol, maxits,&
                                  ierr)
            ASSERT(ierr.eq.0)
!           -- PRECONDITIONNEUR UTILISE
            call jeveuo(nosolv//'.SLVK', 'L', jslvk)
            precon = zk24(jslvk-1+2)
            if (indic .eq. KSP_DIVERGED_ITS) then
!              NOMBRE MAX D'ITERATIONS
                if ((istop.eq.0) .or. (precon.ne.'LDLT_SP')) then
                    nmaxit=maxits
                    call u2mesi('F', 'PETSC_5', 1, nmaxit)
                else
                    iret = 1
                    goto 999
                endif
            else if (indic.eq.KSP_DIVERGED_DTOL) then
!              DIVERGENCE
                divtol = dtol
                call u2mesr('F', 'PETSC_6', 1, divtol)
            else if (indic.eq.KSP_DIVERGED_BREAKDOWN) then
!              BREAKDOWN
                call u2mess('F', 'PETSC_7')
            else if (indic.eq.KSP_DIVERGED_BREAKDOWN_BICG) then
!              BREAKDOWN BiCG
                call u2mess('F', 'PETSC_8')
            else if (indic.eq.KSP_DIVERGED_NONSYMMETRIC) then
!              MATRICE NON SYMETRIQUE
                call u2mess('F', 'PETSC_9')
            else if (indic.eq.KSP_DIVERGED_INDEFINITE_PC) then
!              PRECONDITIONNEUR NON DEFINI
                call u2mess('F', 'PETSC_10')
            else if (indic.eq.KSP_DIVERGED_INDEFINITE_MAT) then
!              MATRICE NON DEFINIE
                call u2mess('F', 'PETSC_11')
            else
!              AUTRE ERREUR
                ptserr = indic
                call u2mesi('F', 'PETSC_12', 1, ptserr)
            endif
        endif
!
!        2.5b VERIFICATION DE LA SOLUTION :
!        ----------------------------------
!
!        -- DOIT-ON VERIFIER LE CRITERE EN NORME NON PRECONDITIONNEE ?
        call jeveuo(nosolv//'.SLVR', 'L', jslvr)
        resipc = zr(jslvr-1+4)
!
        if (resipc .ge. 0.d0) then
            call VecDuplicate(x, r, ierr)
            ASSERT(ierr.eq.0)
!           r = Ax
            call MatMult(a, x, r, ierr)
            ASSERT(ierr.eq.0)
!           r = b - Ax
            call VecAYPX(r, -1.d0, b, ierr)
            ASSERT(ierr.eq.0)
!           fres = ||r||_2
            call VecNorm(r, norm_2, fres, ierr)
            ASSERT(ierr.eq.0)
!           ires = ||b||_2
            call VecNorm(b, norm_2, ires, ierr)
            ASSERT(ierr.eq.0)
!
            call VecDestroy(r, ierr)
            ASSERT(ierr.eq.0)
!
            call KSPGetTolerances(ksp, rtol, atol, dtol, maxits,&
                                  ierr)
            ASSERT(ierr.eq.0)
!
            if (fres .gt. sqrt(rtol)*ires) then
                call u2mess('F', 'PETSC_16')
            endif
        endif
!
!        2.6 RECOPIE DE LA SOLUTION :
!        ----------------------------
!
        call apsolu(kptsc, lmd, rsolu)
!
!         2.7 NETTOYAGE PETSc (VECTEURS) :
!         --------------------------------
!
!        -- EN CAS D'ERREUR DANS LES ITERATIONS DE KRYLOV ON SAUTE ICI
999      continue
        call VecDestroy(b, ierr)
        ASSERT(ierr.eq.0)
        call VecDestroy(x, ierr)
        ASSERT(ierr.eq.0)
!
!        -- PRECONDITIONNEUR UTILISE
        call jeveuo(nosolv//'.SLVK', 'L', jslvk)
        precon = zk24(jslvk-1+2)
!
!        -- TRAITEMENT PARTICULIER DU PRECONDITIONNEUR LDLT_SP
        if (precon .eq. 'LDLT_SP') then
!           MENAGE
            spsomu = zk24(jslvk-1+3)
            call detrsd('SOLVEUR', spsomu)
            spsomu = ' '
!
            call VecDestroy(xlocal, ierr)
            ASSERT(ierr.eq.0)
            call VecDestroy(xglobal, ierr)
            ASSERT(ierr.eq.0)
            call VecScatterDestroy(xscatt, ierr)
            ASSERT(ierr.eq.0)
            xlocal = 0
            xglobal = 0
            xscatt = 0
!           ON STOCKE LE NOMBRE D'ITERATIONS DU KSP
            call KSPGetIterationNumber(ksp, maxits, ierr)
            ASSERT(ierr.eq.0)
            nmaxit = maxits
            call jeveuo(nosolv//'.SLVI', 'E', jslvi)
            zi(jslvi-1+5) = nmaxit
        endif
!
    else if (action.eq.'DETR_MAT') then
!     -----------------------------------
!
!        3.0 RECUPERATION DES POINTEURS :
!        --------------------------------
!
        a = ap(kptsc)
        ksp = kp(kptsc)
!
!        3.1 NETTOYAGE PETSc :
!        ---------------------
!
!        -- DESTRUCTION DES OBJETS PETSC GENERAUX
        call MatDestroy(a, ierr)
        ASSERT(ierr.eq.0)
        call KSPDestroy(ksp, ierr)
        ASSERT(ierr.eq.0)
!
!        -- SUPRESSION DE L'INSTANCE PETSC
        nomats(kptsc) = ' '
        nosols(kptsc) = ' '
        nonus(kptsc) = ' '
        ap(kptsc) = 0
        kp(kptsc) = 0
!
!        -- PRECONDITIONNEUR UTILISE
        call jeveuo(nosolv//'.SLVK', 'L', jslvk)
        precon = zk24(jslvk-1+2)
!
        if (precon .eq. 'LDLT_SP') then
!           MENAGE
            spmat = ' '
            spsolv = ' '
        endif
!
    else
        ASSERT(.false.)
    endif
!
!     -- ON REACTIVE LA LEVEE D'EXCEPTION
    call matfpe(1)
!
    call jedema()
!
#else
    character(len=1) :: kdummy
    integer :: idummy
    real(kind=8) :: rdummy
    kdummy = action(1:1)
    idummy = kptsc
    rdummy = rsolu(1)
    kdummy = vcine(1:1)
    idummy = istop
    idummy = iret
#endif
!
end subroutine
