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
!----------------------------------------------------------------
!
#ifdef _HAVE_PETSC
!
#include "aster_petsc.h"
!----------------------------------------------------------------
!
!     VARIABLES LOCALES
    integer :: nsmdi, nsmhc, n, nz, nvalm, nlong
    integer :: jsmdi, jsmhc, jdxi1, jdxi2, jdval1, jdval2, jvalm, jvalm2
    integer :: k, ilig, jcol, nzdeb, nzfin
    integer :: iterm, jterm
!
    character(len=19) :: nomat, nosolv
    character(len=16) :: idxi1, idxi2, trans1, trans2
    character(len=14) :: nonu
!
    logical :: lmnsy
!
    real(kind=8) :: valm
!
    parameter (idxi1 ='&&APMAMC.IDXI1__')
    parameter (idxi2 ='&&APMAMC.IDXI2__')
    parameter (trans1='&&APMAMC.TRANS1_')
    parameter (trans2='&&APMAMC.TRANS2_')
!
!----------------------------------------------------------------
!     Variables PETSc
    PetscInt :: low, high, neq, dlow, ierr
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
    n=nsmdi
    neq=n
    nz=zi(jsmdi-1+n)
!
    call jelira(nomat//'.VALM', 'NMAXOC', nvalm)
    if (nvalm .eq. 1) then
        lmnsy=.false.
    else if (nvalm.eq.2) then
        lmnsy=.true.
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(jexnum(nomat//'.VALM', 1), 'L', jvalm)
    call jelira(jexnum(nomat//'.VALM', 1), 'LONMAX', nlong)
    ASSERT(nlong.eq.nz)
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
!     CAS OU ON POSSEDE LE PREMIER BLOC DE LIGNES
    if (low .eq. 0) then
        call MatSetValue(a, 0, 0, zr(jvalm), INSERT_VALUES, ierr)
        dlow=1
    else
        dlow=0
    endif
!
!     ON COMMENCE PAR S'OCCUPER DU BLOC DIAGONAL
    do jcol = low+dlow, high-1
        nzdeb = zi(jsmdi+jcol-1) + 1
        nzfin = zi(jsmdi+jcol)
        do k = nzdeb, nzfin
            ilig = zi4(jsmhc-1+k)
            jterm=jterm+1
            if (lmnsy) then
                valm=zr(jvalm2-1+k)
                zr(jdval2+jterm-1)=valm
                zi4(jdxi2+jterm-1)=ilig-1
            else
                valm=zr(jvalm-1+k)
                zr(jdval2+jterm-1)=valm
                zi4(jdxi2+jterm-1)=ilig-1
            endif
            if (ilig .ge. (low+1)) then
                iterm=iterm+1
                valm=zr(jvalm-1+k)
                zr(jdval1+iterm-1)=valm
                zi4(jdxi1+iterm-1)=ilig-1
            endif
        end do
        jterm=jterm-1
        call MatSetValues(a, iterm, zi4(jdxi1), 1, [int(jcol, 4)],&
                          zr(jdval1), INSERT_VALUES, ierr)
        call MatSetValues(a, 1, [int(jcol, 4)], jterm, zi4(jdxi2),&
                          zr(jdval2), INSERT_VALUES, ierr)
        iterm=0
        jterm=0
    end do
!
!     ENSUITE ON FINIT PAR LE BLOC HORS DIAGONAL
    do jcol = high, neq-1
        nzdeb = zi(jsmdi+jcol-1) + 1
        nzfin = zi(jsmdi+jcol)
        do k = nzdeb, nzfin
            ilig = zi4(jsmhc-1+k)
            if (ilig .lt. (low+1)) then
                continue
            else if (ilig.le.high) then
                iterm=iterm+1
                valm=zr(jvalm-1+k)
                zr(jdval1+iterm-1)=valm
                zi4(jdxi1+iterm-1)=ilig-1
            else
                exit
            endif
        end do
        call MatSetValues(a, iterm, zi4(jdxi1), 1, [int(jcol, 4)],&
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
