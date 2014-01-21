subroutine elg_resoud(matas1, chcine, nsecm, chsecm, chsolu,&
                      base, rsolu, csolu, criter, prepos,&
                      istop, iret)
    implicit none
# include "jeveux.h"
# include "asterfort/assert.h"
# include "asterfort/copisd.h"
# include "asterfort/dismoi.h"
# include "asterfort/elg_calc_rhs_red.h"
# include "asterfort/elg_calc_solu.h"
# include "asterfort/elg_gest_common.h"
# include "asterfort/jedema.h"
# include "asterfort/jedetr.h"
# include "asterfort/jelira.h"
# include "asterfort/jemarq.h"
# include "asterfort/jeveuo.h"
# include "asterfort/resou1.h"
# include "asterfort/uttcpu.h"
!-----------------------------------------------------------------------
! But : faire "resoud" si ELIM_LAGR='OUI'
!-----------------------------------------------------------------------
!
    character(len=19) :: matas1
    character(len=*) :: chcine
    integer :: nsecm
    character(len=*) :: chsecm, chsolu, base
    real(kind=8) :: rsolu(*)
    complex(kind=8) :: csolu(*)
    character(len=*) :: criter
    logical :: prepos
    integer :: istop, iret
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
    integer ::  jsolu1, jsolu2, jsecm, jrefa1, nsecmb
    character(len=19) :: matas2, secm19, solve2, solu19
    character(len=24) :: solu2
! ----------------------------------------------------------------------
!
    call jemarq()
!
!
    call jeveuo(matas1//'.REFA', 'L', jrefa1)
    matas2=zk24(jrefa1-1+19)(1:19)
    ASSERT(matas2.ne.' ')
    call dismoi('SOLVEUR', matas2, 'MATR_ASSE', repk=solve2)
!
!   -- mise a  jour du COMMON ELIMLG :
!   ---------------------------------------------
    call elg_gest_common('CHERCHE', matas1, matas2, ' ')
!
!
!   -- Second-membre(s) : passage complet -> reduit :
!   --------------------------------------------------
    solu2='&&ELG_RESOUD.SOLU2'
    if (nsecm .eq. 0) then
        secm19=chsecm
        call jeveuo(secm19//'.VALE', 'L', jsecm)
        call elg_calc_rhs_red(matas1, 1, zr(jsecm), solu2)
    else
        ASSERT(nsecm.gt.0)
        call elg_calc_rhs_red(matas1, nsecm, rsolu, solu2)
    endif
!
!
!   -- on appelle resou1 avec le(s) second-membre(s) reduit(s) :
!   ------------------------------------------------------------
    call jeveuo(solu2, 'E', jsolu2)
    nsecmb=max(nsecm,1)
    call resou1(matas2, ' ', solve2, chcine, nsecmb,&
                ' ', ' ', 'V', zr(jsolu2), csolu,&
                criter, prepos, istop, iret)
!
!
!   -- Solution(s) : passage reduit -> complet :
!   ---------------------------------------------
    if (nsecm .eq. 0) then
        solu19=chsolu
        call copisd('CHAMP', base, chsecm, solu19)
        call jeveuo(solu19//'.VALE', 'E', jsolu1)
        call elg_calc_solu(matas1, 1, zr(jsolu2), zr(jsolu1))
    else
        call elg_calc_solu(matas1, nsecm, zr(jsolu2), rsolu)
    endif
    call jedetr(solu2)
!
!
!
    call jedema()
end subroutine
