subroutine xappar(mesh, model, ds_contact)
!
use NonLin_Datastructure_type
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
#include "asterfort/normev.h"
#include "asterfort/provec.h"
#include "asterfort/xcopco.h"
#include "asterfort/xmcoor.h"
#include "asterfort/xmrema.h"
#include "asterfort/xmrept.h"
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
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM method - Pairing
!
! --------------------------------------------------------------------------------------------------
!
! TRAVAIL EFFECTUE EN COLLABORATION AVEC I.F.P.
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: zmesx, ztabf, zxain
    integer :: ifm, niv
    integer :: i, i_cont_poin, type_inte
    integer :: model_ndim, nt_elem_slav,  nt_cont_poin, nb_cont_poin
    integer :: i_facet, i_elem_slav, i_zone, ifamin
    integer :: jcesd(10), jcesv(10), jcesl(10), iad
    integer :: mmait, amait, nmait, state_slave, stamin
    integer :: elem_slav_nume, elem_mast_nume
    integer :: nb_poin_facet, nb_facet
    real(kind=8) :: geom(3), ksipc1, ksipc2, wpc
    real(kind=8) :: t1min(3), t2min(3), ximin, yimin
    real(kind=8) :: jeumin, coor(3), norm(3), noor
    real(kind=8) :: rrm
    character(len=8) :: elem_type
    character(len=19) :: chs(7)
    character(len=24) :: xfimai
    character(len=24) :: tabfin, maescx
    integer :: jtabf, jmaesx, ninter
    aster_logical :: projin
    integer :: jfimai, i_crack, ifism, i_poin_facet, numpi
    integer, pointer :: xfem_cont(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> .. Pairing'
    endif
!
! - Initializations
!
    chs(1) = '&&XAPPAR.CHSLO'
    chs(2) = '&&XAPPAR.CHSAI'
    chs(3) = '&&XAPPAR.CHSPI'
    chs(4) = '&&XAPPAR.CHSCF'
    chs(5) = '&&XAPPAR.CHSGE'
    chs(6) = '&&XAPPAR.CHSGM'
    chs(7) = '&&XAPPAR.CHSLT'
     nt_cont_poin = 0
    geom(1:3) = 0.d0
    t1min(1:3) = 0.d0
    t2min(1:3) = 0.d0
!
! - Parameters
!
    model_ndim   = cfdisi(ds_contact%sdcont_defi,'NDIM' )
    nt_elem_slav = cfdisi(ds_contact%sdcont_defi,'NTMAE')
!
! - Datastructure for contact solving
!
    tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    maescx = ds_contact%sdcont_defi(1:16)//'.MAESCX'
    xfimai = ds_contact%sdcont_defi(1:16)//'.XFIMAI'
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
        type_inte = mminfi(ds_contact%sdcont_defi,'INTEGRATION' ,i_zone )
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
! ----- Number of intersection points
!
        call cesexi('C', jcesd(1), jcesl(1), elem_slav_nume, 1,&
                    i_crack, 1, iad)
        ASSERT(iad.gt.0)
        ninter = zi(jcesv(1)-1+iad)
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
!
! ------------- COORDONNEES DANS ELEMENT DE REFERENCE ET POIDS DU POINT DE CONTACT
! ------------- FAUX POINT D'INTEGRATION (POUR L'ELIMINATION DES DDL EN TROP)
!
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
                                wpc)
                else
                    call mmgaus(elem_type, type_inte, i_cont_poin, ksipc1, ksipc2,&
                                wpc)
                endif
!
! ------------- Coordinates of contact point
!
                call xcopco(jcesd        , jcesv         , jcesl  , i_crack, elem_type,&
                            model_ndim   , elem_slav_nume, i_facet, ksipc1 , ksipc2   ,&
                            nb_poin_facet, geom)
!
! ------------ Get nearest integration point on master side nearest contact point
!
                if (state_slave .gt. 0 .and. state_slave .ne. 2) then
                    call xmrept(jcesd     , jcesv, jcesl      , i_zone, model_ndim,&
                                ds_contact, geom , state_slave, mmait , amait     ,&
                                nmait)
                endif
!
! ------------- Projection of contact point on nearest master element
!
                call xmrema(jcesd         , jcesv      , jcesl      , mesh     , model_ndim    ,&
                            i_crack       , ds_contact , i_zone     , elem_type, mmait         ,&
                            amait         , nmait      , state_slave, geom     , elem_mast_nume,&
                            elem_slav_nume, ifamin     , i_facet    , jeumin   , t1min         ,&
                            t2min         , ximin      , yimin      , projin   , stamin        ,&
                            ifism)
!
! ------------- Save elements index
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+1) = elem_slav_nume
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+2) = elem_mast_nume
!
! ------------- Save contact point coordinates
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+3) = ksipc1
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+12) = ksipc2
!
! ------------- Local basis at contact point
!
                if (model_ndim .eq. 3) then
                    call provec(t1min, t2min, norm)
                    call normev(norm, noor)
                    if (abs(norm(1)) .ne. 1) then
                        t1min(1) = 1-norm(1)**2
                        t1min(2) = -norm(1)*norm(2)
                        t1min(3) = -norm(1)*norm(3)
                    else
                        t1min(1) = -norm(2)*norm(1)
                        t1min(2) = 1-norm(2)**2
                        t1min(3) = -norm(2)*norm(3)
                    endif
                    call provec(norm, t1min, t2min)
                    call normev(t1min, noor)
                    call normev(t2min, noor)
                endif
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+6)  = t1min(1)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+7)  = t1min(2)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+8)  = t1min(3)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+9)  = t2min(1)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+10) = t2min(2)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+11) = t2min(3)
!
! ------------- Contact zone index for current crack
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+15) = i_zone
!
! ------------- Contact point weight
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+16) = wpc
!
! ------------- Number of slave intersection points
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+24) = nb_poin_facet
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+14) = ninter
!
! ------------- Index of slave facet 
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+25) = i_facet
!
! ------------- Number of slave facets
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+26) = nb_facet
!
! ------------- Coordinates in master element
!
                call xmcoor(jcesd        , jcesv         , jcesl , ifism, model_ndim,&
                            nb_poin_facet, elem_mast_nume, ifamin, ximin, yimin     ,&
                            coor)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+20) = coor(1)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+21) = coor(2)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+23) = coor(3)
!
! ------------- State of master element
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+32) = stamin
!
! ------------- Compute sqrt(level_set) of master contact point
!
                call xmrlst(jcesd, jcesv, jcesl, mesh, elem_mast_nume,&
                            coor , rrm)
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+31) = rrm
!
! ------------- Local index of slave crack
!
                zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+34) = ifism
!
! ------------- Outside projction
!
                if (.not. projin) then
                    zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+22) = 1.d0
                else
                    zr(jtabf+ztabf* nt_cont_poin+ztabf*(i_cont_poin-1)+22) = 0.d0
                endif
            end do
             nt_cont_poin =  nt_cont_poin + nb_cont_poin
        end do
100     continue
    end do
    zr(jtabf) =  nt_cont_poin
    ASSERT( nt_cont_poin.eq.cfdisi(ds_contact%sdcont_defi, 'NTPC'))
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
