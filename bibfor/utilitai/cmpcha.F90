subroutine cmpcha(fieldz    , cmp_name, cata_to_field, field_to_cata, nb_cmpz,&
                  nb_cmp_mxz)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/utmess.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: nicolas.sellenet at edf.fr
!
    character(len=*), intent(in) :: fieldz
    character(len=8), pointer, intent(out) :: cmp_name(:)
    integer, pointer, intent(out) :: cata_to_field(:)
    integer, pointer, intent(out) :: field_to_cata(:)
    integer, optional, intent(out) :: nb_cmpz
    integer, optional, intent(out) :: nb_cmp_mxz
!
! --------------------------------------------------------------------------------------------------
!
! Create objects for global components (catalog) <=> local components (field)
!
! --------------------------------------------------------------------------------------------------
!
! In  field         : name of field
! Out nb_cmp_mx     : number of components in GRANDEUR (catalog)
! Out nb_cmp        : number of components in GRANDEUR (field)
! Out cmp_name      : pointer to name of components in field
! Out cata_to_field : pointer to converter from global components (catalog) to local (field)
! Out field_to_cata : pointer to converter from local components (field) to global (catalog)
!
!
! --------------------------------------------------------------------------------------------------
!
!  EXEMPLE :
!  SI cmp_name EST UN CHAMP DE DEPL_R NE CONTENANT QUE 'DY' ET 'DRZ' :
!    NCMP=2
!    NOMCP=('DY','DRZ')
!    CORR1=(0,1,0,0,2,0,...)
!    CORR2=(2,5)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_grel, jceld, nb_ec, jcmpgd, nb_cmp, nb_cmp_mx
    integer :: igr, imolo, jmolo, idx_gd, nb_pt, i_pt, k, iadg, i_cmp
    integer :: jdesc, long, jprno, nb_node, i_node, ncmpp
    integer :: ngrmx, nbedit, igd, ient, debgd, dg(100), ior, kpt, kcmp
    aster_logical :: diff
    character(len=8) :: gran_name, mesh
    character(len=16) :: typsd
    character(len=19) :: field, prof_chno
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
    field=fieldz
!
! - Informations about field
!
    call dismoi('TYPE_CHAMP', fieldz, 'CHAMP', repk=typsd)
    if (typsd .eq. 'NOEU') then
        call dismoi('NOM_GD', field, 'CHAM_NO', repk=gran_name)
    else if (typsd(1:2).eq.'EL') then
        call dismoi('NOM_GD', field, 'CHAM_ELEM', repk=gran_name)
    else if (typsd.eq.'CART') then
        call dismoi('NOM_GD', field, 'CARTE', repk=gran_name)
    else
        ASSERT(.false.)
    endif
!
    call dismoi('NB_EC', gran_name, 'GRANDEUR', repi=nb_ec)
    call dismoi('NB_CMP_MAX', gran_name, 'GRANDEUR', repi=nb_cmp_mx)
    call dismoi('NUM_GD', gran_name, 'GRANDEUR', repi=idx_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', idx_gd), 'L', jcmpgd)
!
!
!
!     -- 1. POUR ECONOMISER LES APPELS A EXISDG, ON VA CALCULER
!           UN DESCRIPTEUR_GRANDEUR (DG) "ENVELOPPE" DE TOUS LES
!           POINTS DU CHAMP.
!     ----------------------------------------------------------------
    ASSERT(nb_ec.le.100)
    dg(1:100)=0
!
!
!     -- 1.1 CAS DES CHAM_NO
!     ----------------------------------------------------------------
    if (typsd .eq. 'NOEU') then
        call jeveuo(field//'.DESC', 'L', jdesc)
        call dismoi('NOM_MAILLA', field, 'CHAM_NO', repk=mesh)
        call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nb_node)
!
!        -- 1.1.1 CAS DES CHAM_NO A REPRESENTATION CONSTANTE :
        if (zi(jdesc-1+2) .lt. 0) then
            call jelira(field//'.DESC', 'LONMAX', long)
            ASSERT(long.eq.(2+nb_ec))
            iadg=jdesc-1+3
            do k = 1, nb_ec
                dg(k)=zi(iadg-1+k)
            end do
!
!        -- 1.1.2 CAS DES CHAM_NO A PROF_CHNO:
        else
            call dismoi('PROF_CHNO', field, 'CHAM_NO', repk=prof_chno)
            call jeveuo(jexnum(prof_chno//'.PRNO', 1), 'L', jprno)
            do i_node = 1, nb_node
                ncmpp=zi(jprno-1+(i_node-1)*(nb_ec+2)+2)
                if (ncmpp .ne. 0) then
                    iadg=jprno-1+(i_node-1)*(nb_ec+2)+3
                    do k = 1, nb_ec
                        dg(k)=ior(dg(k),zi(iadg-1+k))
                    end do
                endif
            end do
        endif
!
!
!     -- 1.2 CAS DES CHAM_ELEM
!     ----------------------------------------------------------------
    else if (typsd(1:2).eq.'EL') then
        call jeveuo(field//'.CELD', 'L', jceld)
        nb_grel=zi(jceld-1+2)
!
        do igr = 1, nb_grel
            imolo=zi(jceld-1+zi(jceld-1+4+igr)+2)
            if (imolo .ne. 0) then
                call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
                ASSERT(zi(jmolo-1+1).le.3)
                ASSERT(zi(jmolo-1+2).eq.idx_gd)
                diff=(zi(jmolo-1+4).gt.10000)
                nb_pt=mod(zi(jmolo-1+4),10000)
                do i_pt = 1, nb_pt
                    kpt=1
                    if (diff) kpt=i_pt
                    iadg=jmolo-1+4+(kpt-1)*nb_ec+1
                    do k = 1, nb_ec
                        dg(k)=ior(dg(k),zi(iadg-1+k))
                    end do
                end do
            endif
        end do
!
!
!     -- 1.3 CAS DES CARTES
!     ----------------------------------------------------------------
    else if (typsd.eq.'CART') then
        call jeveuo(field//'.DESC', 'L', jdesc)
        ngrmx=zi(jdesc-1+2)
        nbedit=zi(jdesc-1+3)
        do igd = 1, nbedit
            ient=zi(jdesc-1+3+2*igd)
            if (ient .ne. 0) then
                debgd=3+2*ngrmx+(igd-1)*nb_ec+1
                do k = 1, nb_ec
                    dg(k)=ior(dg(k),zi(jdesc-1+debgd-1+k))
                end do
            endif
        end do
!
    else
        ASSERT(.false.)
    endif
!
! - Create cata_to_field object
!
    AS_ALLOCATE(vi = cata_to_field, size = nb_cmp_mx)
    nb_cmp=0
    do i_cmp = 1, nb_cmp_mx
        if (exisdg(dg(1),i_cmp)) then
            nb_cmp=nb_cmp+1
            cata_to_field(i_cmp)=nb_cmp
        endif
    end do
    ASSERT(nb_cmp.ne.0)
!
! - Create field_to_cata and cmp_name objects
!
    AS_ALLOCATE(vi = field_to_cata, size = nb_cmp)
    AS_ALLOCATE(vk8 = cmp_name, size = nb_cmp)
    kcmp=0
    do i_cmp = 1, nb_cmp_mx
        if (cata_to_field(i_cmp) .gt. 0) then
            kcmp=kcmp+1
            field_to_cata(kcmp) = i_cmp
            cmp_name(kcmp)      = zk8(jcmpgd-1+i_cmp)
        endif
    end do
    ASSERT(kcmp.eq.nb_cmp)
!
    if (present(nb_cmpz)) then
        nb_cmpz = nb_cmp
    endif
    if (present(nb_cmp_mxz)) then
        nb_cmp_mxz = nb_cmp_mx
    endif
!
    call jedema()
end subroutine
