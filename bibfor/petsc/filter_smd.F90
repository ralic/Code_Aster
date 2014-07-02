subroutine filter_smd(nommat, vsmb)
    implicit none
! person_in_charge: natacha.bereux at edf.fr
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/asmpi_comm.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    character(len=*) :: nommat
    real(kind=8) :: vsmb(*)
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
!-----------------------------------------------------------------------
! BUT : ON MET A ZERO LES TERMES DU SECOND MEMBRE QUI N'APPARTIENNENT PAS
!       DE FACON EXCLUSIVE (AU SENS PETSC) AU PROCESSEUR COURANT. 
!-----------------------------------------------------------------------
!     VARIABLES LOCALES
!-----------------------------------------------------------------------
    integer :: ieql, ieqg, jpddl, neqg, neql
    integer :: iccid, rang
    character(len=14) :: nu
    character(len=19) :: mat
    mpi_int :: mrank, msize
    aster_logical :: is_ddl_cine, iam_sole_owner
    character(len=24), pointer :: refa(:) => null()
    integer, pointer :: ccid(:) => null()
    integer, pointer :: nulg(:) => null()
    integer, pointer :: nequl(:) => null()
    integer, pointer :: nequ(:) => null()
!-----------------------------------------------------------------------
!     DEBUT
    call jemarq()
!-----------------------------------------------------------------------
    mat = nommat
!
    call jeveuo(mat//'.REFA', 'L', vk24=refa)
    if (refa(11) .eq. 'MATR_DISTR') then
! Infos du processeur courant
        call asmpi_info(rank=mrank, size=msize)
        rang = to_aster_int(mrank)
! Infos du NUME_DDL
        nu = refa(2)(1:14)
        call jeveuo(nu//'.NUML.NULG', 'L', vi=nulg)
        call jeveuo(nu//'.NUML.PDDL', 'L', jpddl)
        call jeveuo(nu//'.NUML.NEQU', 'L', vi=nequl)
        call jeveuo(nu//'.NUME.NEQU', 'L', vi=nequ)
        neqg=nequ(1)
        neql=nequl(1)
        call jeexin(mat//'.CCID', iccid)
!
        if (iccid .ne. 0) then
            call jeveuo(mat//'.CCID', 'L', vi=ccid)
        endif
!
        do ieql = 1, neql
            ieqg=nulg(ieql)
! Le dl courant est-il fixé par une charge cinématique ? 
            if (iccid == 0) then
! Il n'y a pas de charge cinématique sur le modèle  
                is_ddl_cine=.false.
            else 
! Il existe au moins une charge cinématique. On vérifie si
! le numéro global de dl est concerné 
                is_ddl_cine=ccid(ieqg).eq.1
            endif
! Suis-je le proriétaire exclusif (PETSc) de ce ddl ? 
            iam_sole_owner= zi(jpddl-1+ieql) .eq. rang
            if ((.not.is_ddl_cine) .and. (.not.iam_sole_owner)) then
                vsmb(ieqg) = 0.d0
            endif
        enddo
    endif
!
    call jedema()
end subroutine
