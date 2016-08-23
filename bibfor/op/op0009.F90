subroutine op0009()
!
implicit none
!
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jemarq.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/meamac.h"
#include "asterfort/meamgy.h"
#include "asterfort/meamme.h"
#include "asterfort/mecact.h"
#include "asterfort/medome.h"
#include "asterfort/meimme.h"
#include "asterfort/memaac.h"
#include "asterfort/memame.h"
#include "asterfort/memsth.h"
#include "asterfort/meonme.h"
#include "asterfort/meriac.h"
#include "asterfort/merifs.h"
#include "asterfort/merige.h"
#include "asterfort/merigy.h"
#include "asterfort/merime.h"
#include "asterfort/meriro.h"
#include "asterfort/merith.h"
#include "asterfort/redetr.h"
#include "asterfort/sdmpic.h"
#include "asterfort/cme_getpara.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --------------------------------------------------------------------------------------------------
!
!                       COMMANDE:  CALC_MATR_ELEM
!
! --------------------------------------------------------------------------------------------------
!

!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nb_cmp = 6
    character(len=8), parameter :: list_cmp(nb_cmp) = (/'INST    ','DELTAT  ','THETA   ',&
                                                        'KHI     ','R       ','RHO     '/)
    real(kind=8) :: list_vale(nb_cmp)   = (/0.d0,1.d0,1.d0,0.d0,0.d0,0.d0/)
    integer :: nb_load, nbresu, iresu, iexi, nh
    character(len=1) :: base
    character(len=4) :: kmpic
    character(len=8) :: model, cara_elem, sigm, strx, disp
    character(len=16) :: k8dummy, option
    character(len=19) :: matr_elem, rigi_meca, mass_meca, resuel
    character(len=24) :: chtime, mate, compor_mult
    character(len=8), pointer :: v_list_load8(:) => null()
    character(len=24), pointer :: relr(:) => null()
    real(kind=8) :: time, time_incr
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
! - Initializations
!
    base   = 'G'
    chtime = '&&CHTIME'
!
! - Get results
!
    call getres(matr_elem, k8dummy, k8dummy)
!
! - Get parameters
!
    call cme_getpara(option      ,&
                     model       , cara_elem, mate, compor_mult,&
                     v_list_load8, nb_load  ,&
                     rigi_meca   , mass_meca,&
                     time        , time_incr, nh       ,&
                     sigm        , strx     , disp)
    list_vale(1) = time
    list_vale(2) = time_incr
!
! --------------------------------------------------------------------------------------------------
    if (option .eq. 'RIGI_MECA') then
        call merime(model, nb_load     , v_list_load8, mate, cara_elem,&
                    time , compor_mult , matr_elem   , nh  , base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_FLUI_STRU') then
        call merifs(model, nb_load  , v_list_load8, mate, cara_elem,&
                    time , matr_elem, nh)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_GEOM') then
        call merige(model, cara_elem, sigm      , strx      , matr_elem,&
                    base , nh       , deplr=disp, mater=mate)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_ROTA') then
        call meriro(model, cara_elem  , nb_load  , v_list_load8, mate,&
                    time , compor_mult, matr_elem)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'MECA_GYRO') then
        call meamgy(model  , mate        , cara_elem, compor_mult, matr_elem,&
                    nb_load, v_list_load8)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_GYRO') then
        call merigy(model  , mate        , cara_elem, compor_mult, matr_elem,&
                    nb_load, v_list_load8)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'MASS_MECA') then
        call memame(option     , model    , mate, cara_elem, time,&
                    compor_mult, matr_elem, base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'MASS_FLUI_STRU') then
        call memame(option     , model    , mate, cara_elem, time,&
                    compor_mult, matr_elem, base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'MASS_MECA_DIAG') then
        call memame(option     , model    , mate, cara_elem, time,&
                    compor_mult, matr_elem, base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'AMOR_MECA') then
        call meamme(option   , model, nb_load, v_list_load8, mate     ,&
                    cara_elem, time , base   , rigi_meca   , mass_meca,&
                    matr_elem, ' ')
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'IMPE_MECA') then
        call meimme(model, nb_load, v_list_load8, mate, matr_elem)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'ONDE_FLUI') then
        call meonme(model, nb_load, v_list_load8, mate, matr_elem)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_MECA_HYST') then
        call meamme(option   , model, nb_load, v_list_load8, mate     ,&
                    cara_elem, time , base   , rigi_meca   , mass_meca,&
                    matr_elem, ' ')
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_THER') then
        call mecact('V', chtime, 'MODELE', model//'.MODELE', 'INST_R',&
                    ncmp=nb_cmp, lnomcmp=list_cmp, vr=list_vale)
        call merith(model , nb_load  , v_list_load8, mate, cara_elem,&
                    chtime, matr_elem, nh, base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'MASS_THER') then
        call mecact('V', chtime, 'MODELE', model//'.MODELE', 'INST_R',&
                    ncmp=nb_cmp, lnomcmp=list_cmp, vr=list_vale)
        call memsth(model, cara_elem, mate, chtime, matr_elem, base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'RIGI_ACOU') then
        call meriac(model, nb_load, v_list_load8, mate, matr_elem, base)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'MASS_ACOU') then
        call memaac(model, mate, matr_elem)
!
! --------------------------------------------------------------------------------------------------
    else if (option.eq.'AMOR_ACOU') then
        call meamac(model, nb_load, v_list_load8, mate, matr_elem, base)
!
! --------------------------------------------------------------------------------------------------
    else
        ASSERT(.false.)
    endif
!
! --------------------------------------------------------------------------------------------------
!   si MATEL n'est pas MPI_COMPLET, on le complete :
    call jeexin(matr_elem//'.RELR', iexi)
    if (iexi .gt. 0) then
        call jelira(matr_elem//'.RELR', 'LONMAX', nbresu)
        call jeveuo(matr_elem//'.RELR', 'L', vk24=relr)
        do iresu = 1, nbresu
            resuel=relr(iresu)(1:19)
            call jeexin(resuel//'.RESL', iexi)
            if (iexi .eq. 0) cycle
            call dismoi('MPI_COMPLET', resuel, 'RESUELEM', repk=kmpic)
            ASSERT((kmpic.eq.'OUI').or.(kmpic.eq.'NON'))
            if (kmpic .eq. 'NON') call sdmpic('RESUELEM', resuel)
        end do
    endif
!
! - DESTRUCTION DES RESUELEM NULS
!
    call redetr(matr_elem)

    AS_DEALLOCATE(vk8 = v_list_load8)
    call jedetr('&MERITH1           .RELR')
    call jedetr('&MERITH2           .RELR')
    call jedetr('&MERITH3           .RELR')
    call jedetr('&MERITH1           .RERR')
    call jedetr('&MERITH2           .RERR')
    call jedetr('&MERITH3           .RERR')
!
    call jedema()
end subroutine
