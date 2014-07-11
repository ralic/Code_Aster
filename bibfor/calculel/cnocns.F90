subroutine cnocns(cnoz, basez, cnsz)
!
implicit none
!
#include "jeveux.h"
#include "asterc/cheksd.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/cmpcha.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!
    character(len=*), intent(in) :: cnoz
    character(len=*), intent(in) :: cnsz
    character(len=*), intent(in) :: basez
!
! --------------------------------------------------------------------------------------------------
!
! BUT : TRANSFORMER UN CHAM_NO (CNOZ) EN CHAM_NO_S (CNSZ)
!
! --------------------------------------------------------------------------------------------------
!
!     ARGUMENTS:
! CNOZ    IN/JXIN  K19 : SD CHAM_NO A TRANSFORMER
! BASEZ   IN       K1  : BASE DE CREATION POUR CNSZ : G/V/L
! CNSZ    IN/JXOUT K19 : SD CHAM_NO_S A CREER
!
! --------------------------------------------------------------------------------------------------
!
    character(len=1) :: base
    character(len=3) :: tsca
    character(len=8) :: mesh, gran_name
    character(len=19) :: cno, cns, prof_chno
    integer :: nb_ec, idx_gd, nb_cmp_mx, nb_node,  jrefe, jvale, ierr
    integer :: iadg, jprno,  i_node, ncmp, nb_cmp, jcnsl, jcnsv
    integer :: ival, ico, ieq, i_cmp_cata, i_cmp_field, i_cmp
    logical :: sdveri
    integer, pointer :: desc(:) => null()
    integer, pointer :: nueq(:) => null()
    integer, pointer :: cata_to_field(:) => null()
    integer, pointer :: field_to_cata(:) => null()
    character(len=8), pointer :: cmp_name(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    cno = cnoz
    cns = cnsz
    base = basez
!
! - Check ?
!
    sdveri=.false.
    if (sdveri) then
        call cheksd(cno, 'sd_cham_no', ierr)
        ASSERT(ierr.eq.0)
    endif
!
! - Old is erased
!
    call detrsd('CHAM_NO_S', cns)
!
! - Informations
!
    call dismoi('NOM_MAILLA', cno, 'CHAM_NO', repk=mesh)
    call dismoi('NOM_GD', cno, 'CHAM_NO', repk=gran_name)
!
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node)
!
    call dismoi('NB_EC', gran_name, 'GRANDEUR', repi=nb_ec)
    call dismoi('NUM_GD', gran_name, 'GRANDEUR', repi=idx_gd)
    call dismoi('TYPE_SCA', gran_name, 'GRANDEUR', repk=tsca)

    call jeveuo(cno//'.REFE', 'L', jrefe)
    call jeveuo(cno//'.VALE', 'L', jvale)
    call jeveuo(cno//'.DESC', 'L', vi=desc)
!
! - Create objects for global components (catalog) <=> local components (field)
!
    call cmpcha(cno      , cmp_name, cata_to_field, field_to_cata, nb_cmp,&
                nb_cmp_mx)
!
! - Allocate CNS
!
    call cnscre(mesh, gran_name, nb_cmp, cmp_name, base,&
                cns)
!
! - Access to CNS
!
    call jeveuo(cns//'.CNSL', 'E', jcnsl)
    call jeveuo(cns//'.CNSV', 'E', jcnsv)
!
! - Constant profiling ?
!
    if (desc(2) .lt. 0) then
        prof_chno = ' '
    else
        call dismoi('PROF_CHNO', cno, 'CHAM_NO', repk=prof_chno)
    endif
!
! - Set values in CNS
!
    if (prof_chno .eq. ' ') then
        do i_node = 1, nb_node
            do i_cmp_field = 1, nb_cmp
                zl(jcnsl-1+(i_node-1)*nb_cmp+i_cmp_field) = .true.
                ieq = (i_node-1)*nb_cmp + i_cmp_field
                if (tsca .eq. 'R') then
                    zr(jcnsv-1+ieq) = zr(jvale-1+ieq)
                else if (tsca.eq.'I') then
                    zi(jcnsv-1+ieq) = zi(jvale-1+ieq)
                else if (tsca.eq.'C') then
                    zc(jcnsv-1+ieq) = zc(jvale-1+ieq)
                else if (tsca.eq.'L') then
                    zl(jcnsv-1+ieq) = zl(jvale-1+ieq)
                else if (tsca.eq.'K8') then
                    zk8(jcnsv-1+ieq) = zk8(jvale-1+ieq)
                else
                    ASSERT(.false.)
                endif
            end do
        end do
    else
        call jeveuo(jexnum(prof_chno//'.PRNO', 1), 'L', jprno)
        call jeveuo(prof_chno//'.NUEQ', 'L', vi=nueq)
        do i_node = 1, nb_node
!
!         NCMP : NOMBRE DE CMPS SUR LE NOEUD INO
!         IVAL : ADRESSE DU DEBUT DU NOEUD INO DANS .NUEQ
!         IADG : DEBUT DU DESCRIPTEUR GRANDEUR DU NOEUD INO
            ncmp = zi(jprno-1+ (i_node-1)* (nb_ec+2)+2)
            if (ncmp .ne. 0) then
                ival = zi(jprno-1+ (i_node-1)* (nb_ec+2)+1)
                iadg = jprno - 1 + (i_node-1)* (nb_ec+2)+3
                ico = 0
                do i_cmp = 1, nb_cmp
                    i_cmp_cata = field_to_cata(i_cmp)
                    if (exisdg(zi(iadg),i_cmp_cata)) then
                        ico = ico + 1
                        ieq = nueq(ival-1+ico)
                        i_cmp_field = cata_to_field(i_cmp_cata)
!             ASSERT(ic_mp_field.EQ.ic_mp)  COUTEUX ?

                        zl(jcnsl-1+(i_node-1)*nb_cmp+i_cmp_field) = .true.

                        if (tsca .eq. 'R') then
                            zr(jcnsv-1+ (i_node-1)*nb_cmp+i_cmp_field) = zr(jvale-1+ ieq)
                        else if (tsca.eq.'I') then
                            zi(jcnsv-1+ (i_node-1)*nb_cmp+i_cmp_field) = zi(jvale-1+ ieq)
                        else if (tsca.eq.'C') then
                            zc(jcnsv-1+ (i_node-1)*nb_cmp+i_cmp_field) = zc(jvale-1+ ieq)
                        else if (tsca.eq.'L') then
                            zl(jcnsv-1+ (i_node-1)*nb_cmp+i_cmp_field) = zl(jvale-1+ ieq)
                        else if (tsca.eq.'K8') then
                            zk8(jcnsv-1+ (i_node-1)*nb_cmp+i_cmp_field) = zk8(jvale-1+ieq)
                        else
                            ASSERT(.false.)
                        endif
                    endif
                end do
            endif
        end do
    endif
!
! - Check
!
    if (sdveri)  then
        call cheksd(cns,'sd_cham_no_s',ierr)
        ASSERT(ierr.eq.0)
    endif
!
    AS_DEALLOCATE(vi = cata_to_field)
    AS_DEALLOCATE(vi = field_to_cata)
    AS_DEALLOCATE(vk8 = cmp_name)
    call jedema()
end subroutine
