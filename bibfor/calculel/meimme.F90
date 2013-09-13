subroutine meimme(modele, nchar, lchar, mate, matel)
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecham.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
    integer :: nchar
    character(len=8) :: modele, lchar(*)
    character(len=19) :: matel
    character(len=*) :: mate
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCUL DES MATRICES ELEMENTAIRES D 'IMPEDANCE ACOUSTIQUE DANS LE
!     PHENOMENE MECANIQUE
!
! ----------------------------------------------------------------------
! IN  : MODELE : NOM DU MODELE
! IN  : NCHAR  : NOMBRE DE CHARGES
! IN  : LCHAR  : LISTE DES CHARGES
! IN  : MATE   : CARTE DE MATERIAU
! VAR : MATEL  : NOM  DU  MATELE (N RESUELEM) PRODUIT
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: nh
    character(len=8) :: k8b, cara, lpain(4), lpaout(1)
    character(len=16) :: option
    character(len=24) :: ligrmo, lchin(4), lchout(1)
    character(len=24) :: chgeom, chcara(18), chharm
    logical :: lfonc
!
!-----------------------------------------------------------------------
    integer :: ibid, icha, icode, ierd, ilires, iret
!-----------------------------------------------------------------------
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call utmess('F', 'CALCULEL2_82')
    endif
!
    cara = ' '
    nh = 0
    option = 'IMPE_MECA'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, icode)
!
    call jeexin(matel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(matel//'.RERR')
        call jedetr(matel//'.RELR')
    endif
    call memare('G', matel, modele, mate, ' ',&
                option)
!
    lpaout(1) = 'PMATUUR'
    lchout(1) = matel(1:8)//'.ME001'
    ilires = 0
    if (lchar(1) .ne. '        ') then
!
        ligrmo = modele//'.MODELE'
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
!
        lpain(3) = 'PMATERC'
        lchin(3) = mate
!
        do 10 icha = 1, nchar
            call dismoi('F', 'TYPE_CHARGE', lchar(icha), 'CHARGE', ibid,&
                        k8b, ierd)
            if (k8b(5:7) .eq. '_FO') then
                lfonc = .true.
            else
                lfonc = .false.
            endif
!           LIGRCH = LCHAR(ICHA)//'.CHME.LIGRE'
            call exisd('CHAMP_GD', lchar(icha)//'.CHME.IMPE ', iret)
            if (iret .ne. 0) then
                if (lfonc) then
                    option = 'IMPE_MECA_F'
                    lpain(2) = 'PIMPEDF'
                else
                    option = 'IMPE_MECA'
                    lpain(2) = 'PIMPEDR'
                endif
                lchin(2) = lchar(icha)//'.CHME.IMPE .DESC'
                ilires = ilires + 1
                call codent(ilires, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 3, lchin,&
                            lpain, 1, lchout, lpaout, 'G',&
                            'OUI')
                call reajre(matel, lchout(1), 'G')
            endif
10      continue
    endif
    call jedema()
end subroutine
