subroutine elg_calc_matk_red(mat1z, solv1z, mat2z, bas1)
    implicit none
! aslint: disable=W0104
! person_in_charge: jacques.pellet at edf.fr
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/apetsc.h"
#include "asterfort/asmpi_info.h"
#include "asterfort/assert.h"
#include "asterfort/detrsd.h"
#include "asterfort/elg_calc_matm_red.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: mat1z, mat2z, solv1z
    character(len=1) :: bas1
!--------------------------------------------------------------
! but :
!   calculer la matrice reduite mat2z correspondant a mat1z
!   (solveur/elim_lagr='oui')
! in/jxin  : mat1z : sd_matr_asse avec ses conditions dualisees
!                    a eliminer
! in/jxin  : solv1z : sd_solveur
! in/jxout : mat2z : sd_matr_asse "reduite" (sans lagranges)
! in       : bas1 : 'G'/'V' (pour la création de mat2z)
! remarque : on cree egalement un nume_ddl (sous-terrain) pour
!            mat2z.
!---------------------------------------------------------------
!
#ifdef _HAVE_PETSC
#include "elim_lagr.h"
!
!================================================================
    character(len=19) :: matas1, matas2, solve1
    character(len=1) :: ktyp
    mpi_int :: rang, nbproc
    integer :: iret, ibid, iexi
    real(kind=8) :: rbid(1)
    character(len=24), pointer :: refa(:) => null()
!----------------------------------------------------------------
    call jemarq()
    matas1=mat1z
    matas2=mat2z
    solve1=solv1z
!
    call asmpi_info(rank=rang, size=nbproc)
    if (nbproc .ne. 1) call utmess('F', 'ELIMLAGR_2')
!
!     -- quelques garde fous :
    call jelira(matas1//'.VALM', 'TYPE', ibid, ktyp)
    if (ktyp .ne. 'R') call utmess('F', 'ELIMLAGR_3')
    call jeexin(matas1//'.CCID', iexi)
    if (iexi .ne. 0) call utmess('F', 'ELIMLAGR_4')
!
!
!   -- mise a jour de matas1.refa(19):
    call jeveuo(matas1//'.REFA', 'E', vk24=refa)
    if (refa(19) .ne. ' ') then
! ce n'est peut etre pas tres normal de reduire une matrice qui
! a deja ete reduite ...
        ASSERT(.false.)
        call detrsd('MATR_ASSE', refa(19))
    endif
    refa(19)=matas2
!
!
!     1. CALCUL DANS PETSC DES MATRICES NECESSAIRES :
!        Kproj, Tfinal, ...
!     --------------------------------------------------
    call apetsc('ELIM_LAGR', solve1, matas1, rbid, ' ',&
                    0, 0, iret)

    ASSERT(iret.eq.0)
!
!
!     2. CALCUL DANS L'ESPACE JEVEUX DE LA MATRICE Kproj
!        ET DE SON NUME_DDL => MATAS2 (et NU2)
!     --------------------------------------------------
    call elg_calc_matm_red(matas1, matas2, bas1)
!
!
    call jedema()
#else
    call utmess('F', 'ELIMLAGR_1')
#endif
!
end subroutine
