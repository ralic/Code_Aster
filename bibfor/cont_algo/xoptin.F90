subroutine xoptin(mesh, model, sdcont_defi, sdcont_solv)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmgaus.h"
#include "asterfort/mminfi.h"
#include "asterfort/mminfl.h"
#include "asterfort/xmcoor.h"
#include "asterfort/xmrlst.h"
#include "asterfort/xpivit.h"
#include "asterfort/xxmmvd.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=24), intent(in) :: sdcont_defi
    character(len=24), intent(in) :: sdcont_solv
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM method - Initial options (*_INIT)
!
! --------------------------------------------------------------------------------------------------
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  sdcont_defi      : name of contact definition datastructure (from DEFI_CONTACT)
! In  sdcont_solv      : name of contact solving datastructure
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zmesx, ztabf, zxain
    integer :: ifm, niv
    integer :: i_cont_poin, type_inte, i
    integer :: model_ndim, nt_elem_slav, nt_cont_poin, nb_cont_poin
    integer :: i_facet, i_elem_slav, i_zone
    integer :: jcesd(10), jcesv(10), jcesl(10), iad
    integer :: group, state_slave
    integer :: elem_slav_nume
    integer :: nb_poin_facet, nb_facet, nvit, naret
    real(kind=8) :: ksipc1, ksipc2, r8dummy
    real(kind=8) :: rre, coor(3)
    character(len=8) :: elem_type
    character(len=19) :: chs(7)
    character(len=24) :: xfimai, cncte
    character(len=24) :: tabfin, maescx
    integer :: jtabf, jmaesx
    aster_logical :: l_cont_init, l_gliss
    integer :: jfimai, i_crack, i_poin_facet, numpi
    integer, pointer :: xfem_cont(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> . Initial options'
    endif
!
! - Initializations
!
    chs(1) = '&&XOPTIN.CHSLO'
    chs(2) = '&&XOPTIN.CHSAI'
    chs(3) = '&&XOPTIN.CHSPI'
    chs(4) = '&&XOPTIN.CHSCF'
    chs(5) = '&&XOPTIN.CHSGE'
    chs(6) = '&&XOPTIN.CHSGM'
    chs(7) = '&&XOPTIN.CHSLT'
!
! - Parameters
!
    model_ndim   = cfdisi(sdcont_defi,'NDIM' )
    nt_elem_slav = cfdisi(sdcont_defi,'NTMAE')
!
! - Datastructure for contact solving
!
    tabfin = sdcont_solv(1:14)//'.TABFIN'
    maescx = sdcont_defi(1:16)//'.MAESCX'
    xfimai = sdcont_defi(1:16)//'.XFIMAI'
    call jeveuo(tabfin, 'E', jtabf)
    call jeveuo(maescx, 'L', jmaesx)
    call jeveuo(xfimai, 'L', jfimai)
    ztabf = cfmmvd('ZTABF')
    zmesx = cfmmvd('ZMESX')
    zxain = xxmmvd('ZXAIN')
!
! - Get fields
!
    call celces(model//'.TOPOFAC.LO', 'V', chs(1))
    call celces(model//'.TOPOFAC.AI', 'V', chs(2))
    call celces(model//'.TOPOFAC.PI', 'V', chs(3))
    call celces(model//'.TOPOFAC.CF', 'V', chs(4))
    call celces(model//'.TOPOFAC.GE', 'V', chs(5))
    call celces(model//'.TOPOFAC.GM', 'V', chs(6))
    call celces(model//'.LTNO', 'V', chs(7))
    do i = 1, 7
        call jeveuo(chs(i)//'.CESD', 'L', jcesd(i))
        call jeveuo(chs(i)//'.CESV', 'L', jcesv(i))
        call jeveuo(chs(i)//'.CESL', 'L', jcesl(i))
    end do
!
    nt_cont_poin = 0
!
! - Loop on slave elements
!
    do i_elem_slav = 1, nt_elem_slav
!
! ----- Contact zone and crack
!
        i_zone  = zi(jmaesx+zmesx*(i_elem_slav-1)+2-1)
        i_crack = zi(jmaesx+zmesx*(i_elem_slav-1)+5-1)
!
! ----- Parameters
!
        type_inte   =  mminfi(sdcont_defi, 'INTEGRATION'   , i_zone)
        l_gliss     =  mminfl(sdcont_defi, 'GLISSIERE_ZONE', i_zone)
        l_cont_init = (mminfi(sdcont_defi, 'CONTACT_INIT'  , i_zone).eq.1)
        cncte       = zk8(jfimai-1+i_zone)//'.CNCTE'
!
! ----- Current slave element
!
        elem_slav_nume = zi(jmaesx+zmesx*(i_elem_slav-1)+1-1)
        nb_cont_poin   = zi(jmaesx+zmesx*(i_elem_slav-1)+3-1)
        state_slave    = zi(jmaesx+zmesx*(i_elem_slav-1)+4-1)
!
! ----- Type of element
!
        call jeveuo(model//'.XFEM_CONT', 'L', vi=xfem_cont)
        if (model_ndim .eq. 2) then
            if (xfem_cont(1) .le. 2) elem_type='SE2'
            if (xfem_cont(1) .eq. 3) elem_type='SE3'
        else if (model_ndim.eq.3) then
            if (xfem_cont(1) .le. 2) elem_type='TR3'
            if (xfem_cont(1) .eq. 3) elem_type='TR3'
        endif
!
! ----- Number of points for each facet
!
        call cesexi('C', jcesd(1), jcesl(1), elem_slav_nume, 1,&
                    i_crack, 3, iad)
        ASSERT(iad.gt.0)
        nb_poin_facet = zi(jcesv(1)-1+iad)
!
! ----- Number of facets for current slave element
!
        call cesexi('C', jcesd(1), jcesl(1), elem_slav_nume, 1,&
                    i_crack, 2, iad)
        ASSERT(iad.gt.0)
        nb_facet = max(1,zi(jcesv(1)-1+iad))
!
        if (nb_cont_poin .eq. 0) then
            goto 100
        endif
!
! ----- Loop on facets
!
        do i_facet = 1, nb_facet
!
! --------- Loop on contact points
!
            do i_cont_poin = 1, nb_cont_poin
                if (state_slave .lt. 0) then
                    do i_poin_facet = 1, nb_poin_facet
                        call cesexi('S', jcesd(4), jcesl(4), elem_slav_nume, 1,&
                                    i_crack, i_poin_facet, iad)
                        numpi = zi(jcesv(4)-1+iad)
                        call cesexi('S', jcesd(2), jcesl(2), elem_slav_nume, 1,&
                                    i_crack, zxain*(numpi-1)+1, iad)
                        if (zr(jcesv(2)-1+iad) .ne. 0) goto 130
                        call cesexi('S', jcesd(2), jcesl(2), elem_slav_nume, 1,&
                                    i_crack, zxain*(numpi-1)+2, iad)
                        if (zr(jcesv(2)-1+iad) .ne. 0) goto 130
                    end do
                    ASSERT(.false.)
130                 continue
                    call mmgaus(elem_type, type_inte, i_poin_facet, ksipc1, ksipc2,&
                                r8dummy)
                else
                    call mmgaus(elem_type, type_inte, i_cont_poin, ksipc1, ksipc2,&
                                r8dummy)
                endif
!
! ------------- Local index for slave crack
!
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+33) = i_crack
!
! ------------- Coordinates in reference slave element
!
                call xmcoor(jcesd        , jcesv         , jcesl  , i_crack, model_ndim,&
                            nb_poin_facet, elem_slav_nume, i_facet, ksipc1 , ksipc2    ,&
                            coor)
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+17) = coor(1)
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+18) = coor(2)
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+19) = coor(3)
!
! ------------- Compute sqrt(level_set) of slave contact point
!
                call xmrlst(jcesd, jcesv, jcesl, mesh, elem_slav_nume,&
                            coor , rre)
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+30) = rre
!
! ------------- Essential integration point
!
                if (mod(type_inte,10) .eq. 2) then
                    nvit  = 1
                    group = 0
                    naret = 0
                else
                    call xpivit(jcesd      , jcesv         , jcesl  , i_crack, cncte ,&
                                model_ndim , elem_slav_nume, i_facet, ksipc1 , ksipc2,&
                                nvit       , group         , naret)
                    if (state_slave .lt. 0) then
                        nvit = 0
                    endif
                endif
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+27) = nvit
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+4)  = group
                zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+5)  = naret
!
! ------------- Initial contact state
!
                if (l_cont_init) then
                    zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+13) = 1.d0
                    zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+28) = 1.d0
                else
                    zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+13) = 0.d0
                    zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+28) = 0.d0
                endif
!
! ------------- For bilateral contact
!
                if (l_gliss) then
                    zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+29) = 1.d0
                else
                    zr(jtabf+ztabf*nt_cont_poin+ztabf*(i_cont_poin-1)+29) = 0.d0
                endif
            end do
            nt_cont_poin = nt_cont_poin + nb_cont_poin
        end do
100     continue
    end do
!
! - Clean
!
    call detrsd('CHAM_ELEM_S', chs(1))
    call detrsd('CHAM_ELEM_S', chs(2))
    call detrsd('CHAM_ELEM_S', chs(3))
    call detrsd('CHAM_ELEM_S', chs(4))
    call detrsd('CHAM_ELEM_S', chs(5))
    call detrsd('CHAM_ELEM_S', chs(6))
    call detrsd('CHAM_ELEM_S', chs(7))
!
    call jedema()
end subroutine
