subroutine afchno(chamn, base, gran_name, mesh, nb_node,&
                  nbcpno, desc, nb_equa, typval, rval,&
                  cval, kval)
    implicit none
#include "jeveux.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/cmpcha.h"
#include "asterfort/vtcreb.h"
#include "asterfort/crprno.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/pteequ.h"
!
    integer :: nbcpno(*), desc(*)
    real(kind=8) :: rval(*)
    complex(kind=8) :: cval(*)
    character(len=*) :: chamn, gran_name, base, typval, kval(*), mesh
!--------------------------------------------------------------------
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
!--------------------------------------------------------------------
!
!
!
    character(len=19) :: chamno, prof_chno
    integer :: ncmp, ncmpmx
!
!-----------------------------------------------------------------------
    integer :: i1, ic, idec, iec, ii, inec
    integer :: ino, jj, lnueq, nb_equa, lvale, nb_node
    integer :: nec, nn, idx_gd
    integer, pointer :: cata_to_field(:) => null()
    integer, pointer :: field_to_cata(:) => null()
    character(len=8), pointer :: cmp_name(:) => null()
    integer, pointer :: prno(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    chamno = chamn
!
    call jenonu(jexnom('&CATA.GD.NOMGD', gran_name), idx_gd)
    call jelira(jexnum('&CATA.GD.NOMCMP', idx_gd), 'LONMAX', ncmpmx)
    call dismoi('NB_EC', gran_name, 'GRANDEUR', repi=nec)
!
! - Create PROF_CHNO
!
    prof_chno = chamno(1:19)
    call crprno(prof_chno, base, mesh, gran_name, nb_equa)
!
! - Create NODE field
!
    call vtcreb(chamno      , base                  , typval,&
                meshz = mesh, prof_chnoz = prof_chno, idx_gdz = idx_gd, nb_equa_inz = nb_equa)
!
!     --- AFFECTATION DU .PRNO DE L'OBJET PROF_CHNO ---
!
    call jeveuo(chamno//'.PRNO', 'E', vi=prno)
    ii = 0
    idec = 1
    do ino = 1, nb_node
        prno((nec+2)*(ino-1)+1) = idec
        prno((nec+2)*(ino-1)+2) = nbcpno(ino)
        do inec = 1, nec
            ii = ii + 1
            prno((nec+2)*(ino-1)+2+inec) = desc(ii)
        end do
        idec = idec + nbcpno(ino)
    end do
!
!     --- AFFECTATION DU .VALE DE L'OBJET CHAMNO ---
!
    call jeveuo(chamno//'.VALE', 'E', lvale)
    call jeveuo(chamno//'.NUEQ', 'E', lnueq)
    do ino = 1, nb_node
        i1 = prno((nec+2)*(ino-1)+1) + lnueq - 1
        do ic = 1, ncmpmx
            iec = ( ic - 1 ) / 30 + 1
            jj = ic - 30 * ( iec - 1 )
            ii = 2**jj
            nn = iand( desc((ino-1)*nec+iec) , ii )
            if (nn .gt. 0) then
                if (typval(1:1) .eq. 'R') then
                    zr(lvale-1+zi(i1)) = rval((ino-1)*ncmpmx+ic)
                else if (typval(1:1).eq.'C') then
                    zc(lvale-1+zi(i1)) = cval((ino-1)*ncmpmx+ic)
                else if (typval(1:2).eq.'K8') then
                    zk8(lvale-1+zi(i1)) = kval((ino-1)*ncmpmx+ic)
                endif
                i1 = i1 + 1
            endif
        end do
    end do
!
! - Create object local components (field) => global components (catalog)
!
    call cmpcha(chamno, cmp_name, cata_to_field, field_to_cata, nb_cmpz = ncmp)
!
! - Compute .DEEQ object
!
    call pteequ(chamno       , base, nb_equa, idx_gd, ncmp,&
                field_to_cata)
    AS_DEALLOCATE(vi = cata_to_field)
    AS_DEALLOCATE(vi = field_to_cata)
    AS_DEALLOCATE(vk8 = cmp_name)
!
    call jedema()
end subroutine
