subroutine asmatr(nbmat, tlimat, licoef, nu, &
                  infcha, cumul, base, itysca, mataz)
! person_in_charge: jacques.pellet at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
#include "jeveux.h"
#include "asterfort/ascima.h"
#include "asterfort/assert.h"
#include "asterfort/assmam.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedbg2.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/masyns.h"
#include "asterfort/typmat.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"

    character(len=*) :: base, mataz, tlimat(*), licoef, nu
    integer :: nbmat, itysca
    character(len=*) :: infcha
    character(len=4) :: cumul
!-----------------------------------------------------------------------
! in  i   nbmat  : nombre de matr_elem de la liste tlimat
! in  k19 tlimat : liste des matr_elem
! in  k24 licoef : nom du vecteur contenant les coef. mult.
!                  des matr_elem
!                  si licoef=' ' on prend 1.d0 comme coef.
! in  k14 nu     : nom du nume_ddl
! in  k19 infcha : pour les charges cinematiques :
!                  / sd_infcha (k19)
!                  / nom d'un objet jeveux (k24) contenant
!                    les noms des charges cinematiques (k24)
! in  k4 cumul  : 'ZERO' ou 'CUMU'
!                 'ZERO':si un objet de nom matas et de type
!                        matr_asse existe on ecrase son contenu.
!                 'CUMU':si un objet de nom matas et de type
!                        matr_asse existe on cumule dans .valm
! in  k1  base   : base sur laquelle on cree l'objet mataz
! in  i   itysca  : type des matrices elementaires a assembler
!                          1 --> reelles
!                          2 --> complexes
! in/out k19 mataz : l'objet mataz de type matr_asse est cree et rempli
!-----------------------------------------------------------------------

    character(len=1) :: matsym
    character(len=24) :: licoe2
    integer :: k
    character(len=19) :: tlima2(150), matas, infc19
    integer :: ilicoe, i, iret, ibid, idbgav
    integer :: jrefa
!-------------------------------------------------------------------
    call jemarq()
    call jedbg2(idbgav, 0)

    matas = mataz
    licoe2 = licoef
    infc19 = infcha

    ASSERT(cumul.eq.'ZERO'.or.cumul.eq.'CUMU')
    if (cumul .eq. 'ZERO') call detrsd('MATR_ASSE', matas)

    ASSERT(nbmat.le.150)
    do k = 1, nbmat
        tlima2(k) = tlimat(k)
    end do


!   -- traitement de la liste des coef. multiplicateurs :
!   ---------------------------------------------------------------
    if (licoe2 .eq. ' ') then
        call wkvect('&&ASMATR.LICOEF', 'V V R', nbmat, ilicoe)
        do i = 1, nbmat
            zr(ilicoe+i-1) = 1.d0
        end do
    else
        call jeveuo(licoe2, 'L', ilicoe)
    endif


!   -- preparation de la liste de matr_elem pour qu'ils soient
!      du meme type (symetrique ou non) que la matr_asse :
!   ---------------------------------------------------------------
    matsym = typmat(nbmat,tlima2)


!   -- si matrice existe deja et qu'elle doit etre non-symetrique,
!      on la de-symetrise :
!   ---------------------------------------------------------------
    if (cumul .eq. 'CUMU') then
        call jeexin(matas//'.REFA', iret)
        ASSERT(iret.gt.0)
        call jeveuo(matas//'.REFA', 'L', jrefa)
        if (matsym .eq. 'N' .and. zk24(jrefa-1+9) .eq. 'MS') call masyns(matas)
    endif


!   -- assemblage proprement dit :
!   -------------------------------
    call assmam(base, matas, nbmat, tlima2, zr(ilicoe),&
                nu, cumul, itysca)


!   -- traitement des charges cinematiques :
!   ----------------------------------------
    call jeveuo(matas//'.REFA', 'L', jrefa)
    ASSERT(zk24(jrefa-1+3).ne.'ELIMF')
    call ascima(infc19, nu, matas, cumul)


!   -- menage :
!   -----------
    call jedetr('&&ASMATR.LICOEF')

    call jedbg2(ibid, idbgav)
    call jedema()
end subroutine
