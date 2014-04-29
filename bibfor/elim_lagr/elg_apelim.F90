subroutine elg_apelim(kptsc, lqr)
    implicit none
! person_in_charge: jacques.pellet at edf.fr
! aslint: disable=W0104
!
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
# include "asterc/getres.h"
# include "asterc/matfpe.h"
# include "asterc/r8prem.h"
# include "asterfort/apalmc.h"
# include "asterfort/apmamc.h"
# include "asterfort/as_allocate.h"
# include "asterfort/as_deallocate.h"
# include "asterfort/assert.h"
# include "asterfort/dismoi.h"
# include "asterfort/infniv.h"
# include "asterfort/jedema.h"
# include "asterfort/jedetr.h"
# include "asterfort/jemarq.h"
# include "asterfort/jeveuo.h"
# include "asterfort/utmess.h"
# include "asterfort/wkvect.h"
!
    integer :: kptsc
    logical :: lqr
!--------------------------------------------------------------
! BUT : calculer (dans PETSC) les matrices nécessaires à
!       ELIM_LAGR='OUI' :
!       Kproj,Ctrans,Tfinal,RCt,MatB
!       (voir la description de ces matrices dans elim_lagr.h)
!
! IN  : KPTSC (I): INDICE DE L'INSTANCE PETSC LE COMMON SPETSC
!       (pour la matrice complète).
! IN  : LQR (L) : .TRUE. on souhaite le calcul de la matrice R
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
#include "asterf_petsc.h"
#include "asterfort/elg_matrqr.h"
#include "asterfort/elg_comptt.h"
#include "asterfort/elg_remplt.h"
!
!================================================================
!
!     VARIABLES LOCALES
    integer :: ifm, niv
    character(len=19) :: nomat, nosolv, rigi1
    character(len=14) :: nonu
    character(len=16) :: concep, nomcmd
    logical :: lmd, info2
    PetscInt :: ierr
    Mat :: c, ct,c2, t, t2, mtemp
    IS :: isfull, islag1, isphys, isred
    IS :: isnvco, istout
    integer :: clag1, clag2, cphys, nbphys, nblag, i1, j1, nbelig
    PetscInt ::  nterm
    integer :: nbeq, ilag1, ilag2, iphys, nnzt, contr, pos, istat 
    integer :: nzrow, valrow, indnz, indcon, indlib, nworkt
    integer :: ctemp, redem, nvcont, nbnvco, ifull, nbred
    integer :: ilig, jcol, ico, kterm, ieq, k, kptscr, ktrou
    real(kind=8) :: temp, eps
    character(len=8) :: k8b
    integer, pointer :: delg(:) => null()
    real(kind=8), dimension(:), pointer :: norm_t => null()
    character(len=24), pointer :: slvk(:) => null()
    mpi_int :: mpicomm
!----------------------------------------------------------------
    call jemarq()
    call asmpi_comm('GET_WORLD', mpicomm)
    call infniv(ifm, niv)
    info2=niv.eq.2
!
!
!     -- ON DESACTIVE LA LEVEE D'EXCEPTION FPE DANS LES MKL
    call matfpe(-1)
!
!
!     -- LECTURE DU COMMUN
    nomat = nomats(kptsc)
    nosolv = nosols(kptsc)
    nonu = nonus(kptsc)
!
    call jeveuo(nosolv//'.SLVK', 'L', vk24=slvk)
!
!     -- ON NE VEUT PAS DE MATRICE DISTRIBUEE :
    lmd = slvk(10)(1:3).eq.'OUI'
    ASSERT(.not.lmd)
!
!
!     0.1 CREATION ET PREALLOCATION DE LA MATRICE PETSc :
!     ---------------------------------------------------
    call apalmc(kptsc)
!
!
!     0.2 COPIE DE LA MATRICE ASTER VERS LA MATRICE PETSc :
!     -----------------------------------------------------
    call apmamc(kptsc)
!
!
!     0.3 ASSEMBLAGE DE LA MATRICE PETSc :
!     ------------------------------------
    call MatAssemblyBegin(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
    ASSERT(ierr.eq.0)
    call MatAssemblyEnd(ap(kptsc), MAT_FINAL_ASSEMBLY, ierr)
    ASSERT(ierr.eq.0)
!
!
!     1. Extraction de la matrice Ctrans de Ap(KPTSC) :
!     --------------------------------------------------------
!
!     -- calcul de KPTSCR (matrice décrivant les relations linéaires):
!     ----------------------------------------------------------------
    kptscr=kptsc
!
    ktrou=0
    do k = 1, 5
        if (nomelim(k,1) .eq. nomat) then
            ktrou=k
            goto 1
        endif
    enddo
  1 continue
    ASSERT(ktrou.gt.0)
    rigi1=nomelim(ktrou,3)(1:19)
!
    if (rigi1 .ne. ' ') then
        ktrou=0
        do k = 1, 5
            if (nomats(k) .eq. rigi1) then
                ktrou=k
                goto 2
            endif
        enddo
  2     continue
        ASSERT(ktrou.gt.0)
        kptscr=ktrou
    endif
!
!
!--   Reperage des ddls : physique, lagrange_1 et lagrange_2 :
    call dismoi('NB_EQUA', nonu, 'NUME_DDL', repi=nbeq)
    call jeveuo(nonu//'.NUME.DELG', 'L', vi=delg)
    clag1=0
    clag2=0
    cphys=0
    do i1 = 1, nbeq
        if (delg(i1) .eq. 0) then
            cphys=cphys+1
        endif
        if (delg(i1) .eq. -1) then
            clag1=clag1+1
        endif
        if (delg(i1) .eq. -2) then
            clag2=clag2+1
        endif
    end do
    ASSERT(clag2.eq.clag1)
    nblag=clag1
    nbphys=cphys
    ASSERT(nbeq.eq.cphys+2*nblag)
!
    if (nblag .eq. 0) call utmess('F', 'ELIMLAGR_9')
!
    call wkvect('&&APELIM.ILAG1', 'V V S', nblag, ilag1)
    call wkvect('&&APELIM.ILAG2', 'V V S', nblag, ilag2)
    call wkvect('&&APELIM.IPHYS', 'V V S', nbphys, iphys)
    call wkvect('&&APELIM.IND_ALL_DDL', 'V V S', nbeq, ifull)
!
!-- Construction des vecteurs d'indice
    clag1=0
    clag2=0
    cphys=0
    do i1 = 1, nbeq
        if (delg(i1) .eq. 0) then
            zi4(iphys+cphys)=i1-1
            cphys=cphys+1
        endif
        if (delg(i1) .eq. -1) then
            zi4(ilag1+clag1)=i1-1
            clag1=clag1+1
        endif
        if (delg(i1) .eq. -2) then
            zi4(ilag2+clag2)=i1-1
            clag2=clag2+1
        endif
        zi4(ifull+i1-1)=i1-1
    end do
    ASSERT(cphys.eq.nbphys)
!
    call ISCreateGeneral(mpicomm, nbeq, zi4(ifull), PETSC_USE_POINTER, isfull,&
                         ierr)
    call ISCreateGeneral(mpicomm, nbphys, zi4(iphys), PETSC_USE_POINTER, isphys,&
                         ierr)
    call ISCreateGeneral(mpicomm, nblag, zi4(ilag1), PETSC_USE_POINTER, islag1,&
                         ierr)
!
!
!   -- On extrait la matrice C transposée
    call MatGetSubMatrix(ap(kptscr), isfull, islag1, MAT_INITIAL_MATRIX, melim(ke)%ctrans,&
                         ierr)
!
!
!   --On annule les lignes associées a ILAG1 et ILAG2
    call MatZeroRows(melim(ke)%ctrans, nblag, zi4(ilag1), 0.d0, 0,&
                     0, ierr)
    call MatZeroRows(melim(ke)%ctrans, nblag, zi4(ilag2), 0.d0, 0,&
                     0, ierr)
!   -- On transpose pour avoir C, de la bonne taille
    call MatTranspose(melim(ke)%ctrans, MAT_INITIAL_MATRIX, c, ierr)
!
!  -- Allocation et remplissage de Tfinal, qui servira pour la projection de la matrice :
!  ---------------------------------------------------------------------------------------
    call wkvect('&&APELIM.NNZ_MAT_T      ', 'V V S', nbeq, nnzt)
    do i1 = 1, nbeq
        zi4(nnzt+i1-1)=1
    end do
    call MatCreateSeqAIJ(mpicomm, nbeq, nbeq, int(PETSC_NULL_INTEGER), zi4(nnzt),&
                         melim(ke)%tfinal, ierr)
!
    do i1 = 1, nbeq
        if (delg(i1) .eq. 0) then
            call MatSetValues(melim(ke)%tfinal, 1, [int(i1-1, 4)], 1, [int(i1-1, 4)],&
                              [1.d0], INSERT_VALUES, ierr)
        else
            call MatSetValues(melim(ke)%tfinal, 1, [int(i1-1, 4)], 1, [int(i1-1, 4)],&
                              [0.d0], INSERT_VALUES, ierr)
        endif
    end do
    call MatAssemblyBegin(melim(ke)%tfinal, MAT_FINAL_ASSEMBLY, ierr)
    call MatAssemblyEnd(melim(ke)%tfinal, MAT_FINAL_ASSEMBLY, ierr)
!
!---------------------------------------------------!
!--                                               --!
!-- Boucle sur la verif de toutes les contraintes --!
!--                                               --!
!---------------------------------------------------!
!
!-- Vecteurs de travail pour la recherche des elements non nuls de T
    call wkvect('&&APELIM.CONSTRAINED_DDL', 'V V S', nbeq, contr)
    call wkvect('&&APELIM.IND_NZ_ROW', 'V V S', nbphys, nzrow)
    call wkvect('&&APELIM.VAL_NZ_ROW', 'V V R', nbphys, valrow)
    call wkvect('&&APELIM.IND_NZ_T', 'V V S', nbphys, indnz)
    call wkvect('&&APELIM.IND_CONTRAINTS', 'V V S', nbphys, indcon)
    call wkvect('&&APELIM.IND_LIBRES', 'V V S', nbphys, indlib)
    call wkvect('&&APELIM.CONTR_NON_VERIF', 'V V S', nblag, nvcont)
    call wkvect('&&APELIM.LIGNE_C_TEMP', 'V V R', nbeq, ctemp)
!
    nbnvco=0
    redem=0
123 continue
!
!----------------------------------------------------!
!--                                                --!
!-- Decompte contraite par contrainte              --!
!-- Allocation de T                                --!
!-- Remplissage avec des EPS pour garder le profil --!
!--  au cours des MatAssembly                      --!
!--                                                --!
!----------------------------------------------------!
!
    call elg_comptt(c, t, nworkt)
!
!--------------------------------!
!--                            --!
!-- Remplissage proprement dit --!
!--                            --!
!--------------------------------!
!
    call elg_remplt(c, nonu, nworkt, t, nbnvco)
!
!
!-----------------------------!
!--                         --!
!-- On traite le redemarrage --!
!--                         --!
!-----------------------------!
!
    if (nbnvco .gt. 0) then
        redem=redem+1
        if (info2) write(ifm,'(A14,I3)') 'Redemarrage #',redem
        if (info2) write(ifm,'(A3,I3,A26)') ' * ',nbnvco, ' CONTRAINTES NON VERIFIEES'
!
        if (info2) write(ifm,*) 'NEW NUMBER    ','OLD NUMBER    '
        do j1 = 1, nbnvco
            if (info2) write(ifm,*) j1,' - ',zi4(nvcont+j1-1)+1
        end do
! CT <- C*T
        call MatMatMult(c, t, MAT_INITIAL_MATRIX, PETSC_DEFAULT_DOUBLE_PRECISION, ct,&
                        ierr)
        call MatDestroy(c, ierr)
! On extrait de CT la sous-matrice des contraintes qui n'ont pas été éliminées
! On nomme C cette nouvelle matrice des contraintes 
        call ISCreateGeneral(mpicomm, nbnvco, zi4(nvcont), PETSC_USE_POINTER, isnvco,&
                             ierr)
        call MatGetSubMatrix(ct, isnvco, isfull, MAT_INITIAL_MATRIX, c,&
                             ierr)
        call MatDestroy(ct, ierr)
! T2 = TFinal 
        call MatDuplicate(melim(ke)%tfinal, MAT_COPY_VALUES, t2, ierr)
        call MatDestroy(melim(ke)%tfinal, ierr)
! TFinal = T2 * T 
        call MatMatMult(t2, t, MAT_INITIAL_MATRIX, PETSC_DEFAULT_DOUBLE_PRECISION,&
                        melim(ke)%tfinal, ierr)
! 
        call MatDestroy(t, ierr)
!
        do i1 = 1, nbnvco
            zi4(nvcont+i1-1)=0
        end do
        nbnvco=0
!
        do i1 = 1, nbeq
            zi4(contr+i1-1)=0
            zi4(nnzt+i1-1)=1
        end do
!
        goto 123
!
    endif
!
!
!   -- Derniere projection pour obtenir Tfinal
!   ---------------------------------------------
!
    call MatDuplicate(melim(ke)%tfinal, MAT_COPY_VALUES, t2, ierr)
    call MatDestroy(melim(ke)%tfinal, ierr)
    call MatMatMult(t2, t, MAT_INITIAL_MATRIX, PETSC_DEFAULT_DOUBLE_PRECISION, melim(ke)%tfinal,&
                    ierr)
    ASSERT(ierr.eq.0)
!
!
!   -- Verif de la qualite de la base
!   ----------------------------------
    call MatDestroy(c, ierr)
    ASSERT(ierr.eq.0)
    call MatGetColumnNorms(melim(ke)%ctrans, norm_2, zr(valrow), ierr)
    ASSERT(ierr.eq.0)
!
!-- Changement de version PETSc 3.2 -> 3.3 
!   Renamed MatMatMultTranspose() for C=A^T*B to MatTransposeMatMult()
#ifdef ASTER_PETSC_VERSION_32
        call MatMatMultTranspose(melim(ke)%tfinal, melim(ke)%ctrans, MAT_INITIAL_MATRIX, &
                                     PETSC_DEFAULT_DOUBLE_PRECISION, c2, ierr)
#else
    call MatTransposeMatMult(melim(ke)%tfinal,melim(ke)%ctrans,MAT_INITIAL_MATRIX, &
                           PETSC_DEFAULT_DOUBLE_PRECISION,C2,ierr)
#endif
!
    call MatGetColumnNorms(c2, norm_2, zr(ctemp), ierr)
    if (info2) write(ifm,*) ' '
    if (info2) write(ifm,*) ' '
    if (info2) write(ifm,*) '|C.T|/|C| apres elimination :'
    do i1 = 1, nblag
        if (info2) write(ifm, '(A11,I3,A3,E11.4)') 'CONTRAINTE ', i1, ' : ',&
                   zr(ctemp+i1-1)/zr(valrow+i1-1)
    end do
!
!
!   -- on "retasse" les matrices Ctrans, Tfinal :
!   ---------------------------------------------
!
!   -- Ctrans :
    call MatDuplicate(melim(ke)%ctrans, MAT_COPY_VALUES, mtemp, ierr)
    call MatDestroy(melim(ke)%ctrans, ierr)
    call ISCreateGeneral(mpicomm, nblag, zi4(ifull), PETSC_USE_POINTER, istout,&
                         ierr)
    call MatGetSubMatrix(mtemp, isphys, istout, MAT_INITIAL_MATRIX, melim(ke)%ctrans,&
                         ierr)
    call MatDestroy(mtemp, ierr)
    call ISDestroy(istout, ierr)
!
!   -- Tfinal :
!       calcul de isred : indices des colonnes non vides de Tfinal
!       calcul de nbred : longueur de isred
!       calcul de indred : tableau fortran stocké dans le common
    AS_ALLOCATE( vr=norm_t, size=nbeq )
    call MatGetColumnNorms(melim(ke)%tfinal, norm_2, norm_t(1), ierr)
    ASSERT( ierr == 0 ) 
    ! Nombre de colonnes non-nulles de Tfinal 
    nbred=count( norm_t > r8prem()) 
    ! Le tableau melim(ke)%indred est dans le common elim_lagr.h 
    ! il n'est pas forcément désalloué à la fin de la commande courante
    ! -> allocation avec allocate (et pas AS_ALLOCATE)
    if (nbred > 0 ) then 
      allocate( melim(ke)%indred(nbred), stat = istat ) 
      ASSERT( istat == 0 ) 
    endif   
    ! Indices FORTRAN des colonnes non-nulles de Tfinal
    pos = 0
    do ieq = 1, nbeq
      if ( norm_t(ieq) > r8prem()) then
        pos = pos+1  
        melim(ke)%indred(pos) = ieq
      endif
    enddo
!
!   -- On passe en convention C 
    melim(ke)%indred(:)=melim(ke)%indred(:)-1
    call ISCreateGeneral(mpicomm, nbred, melim(ke)%indred, PETSC_USE_POINTER, isred,&
                         ierr)
!
    call MatDuplicate(melim(ke)%tfinal, MAT_COPY_VALUES, mtemp, ierr)
    call MatDestroy(melim(ke)%tfinal, ierr)
    call MatGetSubMatrix(mtemp, isphys, isred, MAT_INITIAL_MATRIX, melim(ke)%tfinal,&
                         ierr)
!     -- on revient aux indices FORTRAN :
    melim(ke)%indred(:)=melim(ke)%indred(:)+1
    call MatDestroy(mtemp, ierr)
    call ISDestroy(istout, ierr)
!
!
!
!   -- Extraction de MatB depuis Ap(KPTSC) :
!   -----------------------------------------
    call MatGetSubMatrix(ap(kptsc), isphys, isphys, MAT_INITIAL_MATRIX, melim(ke)%matb,&
                         ierr)
!
!
!   -- Projection T'*(MatB*T) :
!   -----------------------------
    call MatPtAP(melim(ke)%matb, melim(ke)%tfinal, MAT_INITIAL_MATRIX, 1.d0, melim(ke)%kproj,&
                 ierr)
!
!  -- Vérification du nombre de contraintes éliminées :
!  ----------------------------------------------------
    clag1=count(norm_t < r8prem())
!
    nbelig=clag1-2*nblag
    if (info2) then
        write(ifm,*) 'On a ',nbeq,' ddls (y compris les double-Lagrange).'
        write(ifm,*) 'On a ',nbeq-2*nblag,' ddls "physiques".'
        write(ifm,*) 'On avait ',nblag,' contraintes'
        write(ifm,*) 'On a ete capable d''en eliminer ',nbelig
    endif
    ASSERT(nbelig.le.nblag)
!
    if (nbelig .eq. 0) call utmess('F', 'ELIMLAGR_10', sk=nomat)
!
    if (nblag .ne. nbelig) then
!       Si la condition ci-dessus n'est pas respectée,
!       c'est que A n'est pas de rang maximum. Il faut regarder de plus près
!       et se préoccuper de la cohérence du second membre (c).
!
        call getres(k8b, concep, nomcmd)
!       -- dans la commande ELIM_LAGR, c==0, on peut donc continuer
        if (nomcmd .ne. 'ELIM_LAGR') then
! développement à faire
            ASSERT(.false.)
        endif
    endif
!
!--
!-- On profite de l'extraction de Ctrans pour en faire une
!-- factorisation QR, sans assembler Q  => RCt
!--
!
    if (lqr) then
        call elg_matrqr(melim(ke)%ctrans, melim(ke)%rct, nbphys, nblag)
        melim(ke)%lqr=.true.
    else
        melim(ke)%rct=0
        melim(ke)%lqr=.false.
    endif
!
!
    call jedetr('&&APELIM.ILAG1')
    call jedetr('&&APELIM.ILAG2')
    call jedetr('&&APELIM.IPHYS')
    call jedetr('&&APELIM.IND_ALL_DDL')
    call jedetr('&&APELIM.NNZ_MAT_T')
    call jedetr('&&APELIM.CONSTRAINED_DDL')
    call jedetr('&&APELIM.IND_NZ_ROW')
    call jedetr('&&APELIM.VAL_NZ_ROW')
    call jedetr('&&APELIM.IND_NZ_T')
    call jedetr('&&APELIM.IND_CONTRAINTS')
    call jedetr('&&APELIM.IND_LIBRES')
    call jedetr('&&APELIM.CONTR_NON_VERIF')
    call jedetr('&&APELIM.LIGNE_C_TEMP')
    AS_DEALLOCATE(vr=norm_t)
!
!
    call matfpe(1)
    call jedema()
!
#else
    ASSERT(.false.)
#endif
!
end subroutine
