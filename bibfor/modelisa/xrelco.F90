subroutine xrelco(mesh   , model, nb_dim, sdline_crack, nb_rela_line, list_rela_line,&
                  nb_edge)
!
implicit none
!
#include "jeveux.h"
#include "asterf_types.h"
#include "asterfort/afrela.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cncinv.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    integer, intent(in) :: nb_dim
    character(len=14), intent(in) :: sdline_crack
    character(len=19), intent(in) :: list_rela_line
    integer, intent(out) :: nb_rela_line
    integer, intent(out) :: nb_edge
!
! --------------------------------------------------------------------------------------------------
!
! XFEM - Contact definition
!
! Create kinematic load
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh           : name of mesh
! In  model          : name of model
! In  nb_dim         : dimension of space
! In  sdline_crack   : name of datastructure of linear relations for crack
! In  list_rela_line : name of linear relation object
! Out nb_rela_line   : number of linear relation
! Out nb_edge        : number of "VITAL" edge
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nbddl1, nbddl2, nbddl3, nbddl4
    parameter  (nbddl1=12)
    parameter  (nbddl2=8)
    parameter  (nbddl3=27)
    parameter  (nbddl4=36)
    character(len=8) :: ddlc1(nbddl1)
    character(len=8) :: ddlc2(nbddl2)
    character(len=8) :: ddlc3(nbddl3)
    character(len=8) :: ddlc4(nbddl4)
!
    real(kind=8) :: vale_real, coef_real(6)
    integer :: ier, repe_type(8), i_edge, fonact(100)
    integer :: node_nume(8), i_dim, jcesl, iad
    integer :: nlag, contac, i
    integer :: nuno, nbnoma1, nbnoma2, nbmano1, nbmano2, ima1, ima2, numa1
    integer :: numa2, adrma1, adrma2, jonc1, jonc2, ino1, ino2, jcesd, jconx2
    character(len=8) :: node_name(8), vale_func_dumm, cmp_name(8), repk
    character(len=8) :: node_nameb(8), noma
    character(len=19) :: chjon, cnxinv
    complex(kind=8) :: coef_cplx_dumm, value_cplx_dumm
    aster_logical :: l_mult_crack, lxthm
    integer, pointer :: v_rela_node(:) => null()
    integer, pointer :: v_rela_cmp(:) => null()
    integer, pointer :: xfem_cont(:) => null()
    integer, pointer :: cesv(:) => null()
    integer, pointer :: connex(:) => null()
    character(len=8), pointer :: lgrf(:) => null()
!
    data ddlc1 /'LAGS_C','LAGS_F1','LAGS_F2',&
                'LAG2_C','LAG2_F1','LAG2_F2',&
                'LAG3_C','LAG3_F1','LAG3_F2',&
                'LAG4_C','LAG4_F1','LAG4_F2'/
!
    data ddlc2 /'LAGS_C','LAGS_F1',&
                'LAG2_C','LAG2_F1',&
                'LAG3_C','LAG3_F1',&
                'LAG4_C','LAG4_F1'/
!
    data ddlc3 /'PRE_FLU','LAG_FLI','LAG_FLS','LAGS_C','LAGS_F1',&
                'LAG2_C','LAG2_F1','LAG3_C','LAG3_F1',&
                'PR2_FLU','LA2_FLI','LA2_FLS','D1X','D1Y',&
                'D2X','D2Y','D3X','D3Y',&
                'PR3_FLU','LA3_FLI','LA3_FLS','V11','V12',&
                'V21','V22','V31','V32'/
!
    data ddlc4 /'PRE_FLU','LAG_FLI','LAG_FLS','LAGS_C','LAGS_F1','LAGS_F2',&
                'LAG2_C','LAG2_F1','LAG2_F2','LAG3_C','LAG3_F1','LAG3_F2',&
                'PR2_FLU','LA2_FLI','LA2_FLS','D1X','D1Y','D1Z',&
                'D2X','D2Y','D2Z','D3X','D3Y','D3Z',&
                'PR3_FLU','LA3_FLI','LA3_FLS','V11','V12','V13',&
                'V21','V22','V23','V31','V32','V33'/
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    vale_real      = 0.d0
    repe_type(1:8) = 0
    nb_rela_line   = 0
    nb_edge        = 0
    value_cplx_dumm=cmplx(0.d0,0.d0)
    coef_cplx_dumm=cmplx(0.d0,0.d0)
!
    do i = 1, 100 
       fonact(i) = 0
    end do
!
! - Get access 
!
! --- TYPE DE CONTACT ET NOMBRE DE MULTIPLICATEURS
!
    call jeveuo(model(1:8)//'.XFEM_CONT','L',vi=xfem_cont)
    contac = xfem_cont(1)
    if(contac.eq.2) nlag = 3
    if(contac.eq.1.or.contac.eq.3) nlag = 1
!
! --- 1) RELATIONS D'EGALITE
!
    call jeexin(sdline_crack, ier)
    if (ier .eq. 0) then
        nb_edge = 0
    else
        call jeveuo(sdline_crack, 'L', vi = v_rela_node)
        call jelira(sdline_crack, 'LONMAX', nb_edge)
        call jeexin(sdline_crack(1:14)//'_LAGR', ier)
        if (ier .eq. 0) then
            l_mult_crack = .false.
        else
            l_mult_crack = .true.
            call jeveuo(sdline_crack(1:14)//'_LAGR', 'L', vi = v_rela_cmp)
        endif
    endif
    
! --- MODELE HM-XFEM ?
!
    call dismoi('EXI_THM', model, 'MODELE', repk=repk)
    if (repk .eq. 'OUI') fonact(37) = 1
    
    lxthm = isfonc(fonact,'THM')
!
    if (lxthm.and.l_mult_crack) then
       chjon = '&&XRELCO.CHJON'
       call celces(model//'.TOPOSE.PJO', 'V', chjon)
       call jeveuo(chjon//'.CESD', 'L', jcesd)
       call jeveuo(chjon//'.CESV', 'L', vi=cesv)
       call jeveuo(chjon//'.CESL', 'L', jcesl)
       cnxinv = '&&CHXFEM.CNXINV'
       call cncinv(mesh, [0], 0, 'V', cnxinv)
       call jeveuo(model//'.MODELE    .LGRF', 'L', vk8=lgrf)
       noma = lgrf(1)
       call jeveuo(noma//'.CONNEX', 'L', vi=connex)
       call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    endif
!
    nb_edge = nb_edge/2
!
!
! - Create kinematic load
!
    do i_edge = 1, nb_edge
!
! ----- Get nodes of linear relation
!
        node_nume(1) = v_rela_node(2*(i_edge-1)+1)
        node_nume(2) = v_rela_node(2*(i_edge-1)+2)
        call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_name(1))
        call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_name(2))
!
! ----- Coefficients of linear relation
!
        coef_real(1) = 1.d0
        coef_real(2) = -1.d0
!
! ----- Set linear relation
!
! --- RELATION POUR LES MULTIPLICATEURS DE CONTACT ET FROTTEMENT
!        SI lxthm=.true. CONTACT POUR LE MODELE HM-XFEM
!        SINON CAS DU CONTACT-FROTTEMENT CLASSIQUE XFEM
!
        if (lxthm) then
!
           if (l_mult_crack) then
!        SI lxthm=.true. ON RECUPERE LA SD JONCNO
              call jelira(jexnum(cnxinv, node_nume(1)), 'LONMAX', nbmano1)
              call jeveuo(jexnum(cnxinv, node_nume(1)), 'L', adrma1)
              call jelira(jexnum(cnxinv, node_nume(2)), 'LONMAX', nbmano2)
              call jeveuo(jexnum(cnxinv, node_nume(2)), 'L', adrma2)
              do 10 ima1 = 1, nbmano1
                 numa1 = zi(adrma1-1 + ima1)
                 nbnoma1 = zi(jconx2+numa1) - zi(jconx2+numa1-1)
                 if (nbnoma1.lt.nb_dim**2) go to 10
                 do 20 ima2 = 1, nbmano2
                    numa2 = zi(adrma2-1 + ima2)
                    nbnoma2 = zi(jconx2+numa2) - zi(jconx2+numa2-1)
                    if (nbnoma2.lt.nb_dim**2) go to 20
                    if (numa1.eq.numa2) go to 30
20               continue
10            continue
30            continue
              do i = 1, nbnoma2
                 nuno = connex(zi(jconx2+numa1-1)+i-1)
                 if (nuno.eq.node_nume(1)) ino1=i
                 if (nuno.eq.node_nume(2)) ino2=i
              end do
              jonc1=0
              jonc2=0
              call cesexi('C', jcesd, jcesl, numa1, 1,&
                          1, ino1, iad)
              if (iad.gt.0) jonc1=cesv(iad)
              call cesexi('C', jcesd, jcesl, numa1, 1,&
                          1, ino2, iad)
              if (iad.gt.0) jonc2=cesv(iad)
           endif
!
           if (nb_dim.eq.2) then
              do i = 1, 3+2*nlag
                  if (l_mult_crack) then
                     cmp_name(1) = ddlc3(9*(v_rela_cmp(2*(i_edge-1)+1)-1)+i)
                     cmp_name(2) = ddlc3(9*(v_rela_cmp(2*(i_edge-1)+2)-1)+i)
                  else
                     cmp_name(1) = ddlc3(i)
                     cmp_name(2) = ddlc3(i)
                  endif
                  call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_name, repe_type,&
                              [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                              'REEL', 'REEL', '12', 0.d0, list_rela_line)
                  nb_rela_line = nb_rela_line + 1
              end do
           else
              do i = 1, 3+3*nlag
                  if (l_mult_crack) then
                     cmp_name(1) = ddlc4(12*(v_rela_cmp(2*(i_edge-1)+1)-1)+i)
                     cmp_name(2) = ddlc4(12*(v_rela_cmp(2*(i_edge-1)+2)-1)+i)
                  else
                     cmp_name(1) = ddlc4(i)
                     cmp_name(2) = ddlc4(i)
                  endif
                  call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_name, repe_type,&
                              [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                              'REEL', 'REEL', '12', 0.d0, list_rela_line)
                  nb_rela_line = nb_rela_line + 1
              end do
           endif
!
           if (l_mult_crack) then
!         EN PRESENCE DE JONCTIONS POUR LES MODELES HM-XFEM, ON IMPOSE L'EGALITE
!         DU CHAMP PRE_FLU DANS CHAQUE BRANCHE AU NIVEAU DE LA JONCTION
              if (v_rela_cmp(2*(i_edge-1)+1).eq.2.and.jonc1.gt.0) then
                 cmp_name(1) = ddlc3(1)
                 cmp_name(2) = ddlc3(9*1+1)
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_nameb(1))
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_nameb(2))
                 call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_nameb, repe_type,&
                             [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                             'REEL', 'REEL', '12', 0.d0, list_rela_line)
                 nb_rela_line = nb_rela_line + 1
              elseif (v_rela_cmp(2*i_edge).eq.2.and.jonc1.gt.0) then
                 cmp_name(1) = ddlc3(1)
                 cmp_name(2) = ddlc3(9*1+1)
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_nameb(1))
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_nameb(2))
                 call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_nameb, repe_type,&
                             [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                             'REEL', 'REEL', '12', 0.d0, list_rela_line)
                 nb_rela_line = nb_rela_line + 1
              endif
              if (v_rela_cmp(2*(i_edge-1)+1).eq.3.and.jonc1.gt.0) then
                 cmp_name(1) = ddlc3(1)
                 cmp_name(2) = ddlc3(9*2+1)
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_nameb(1))
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(1)), node_nameb(2))
                 call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_nameb, repe_type,&
                             [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                             'REEL', 'REEL', '12', 0.d0, list_rela_line)
                 nb_rela_line = nb_rela_line + 1
              elseif (v_rela_cmp(2*i_edge).eq.3.and.jonc1.gt.0) then
                 cmp_name(1) = ddlc3(1)
                 cmp_name(2) = ddlc3(9*2+1)
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_nameb(1))
                 call jenuno(jexnum(mesh(1:8)//'.NOMNOE', node_nume(2)), node_nameb(2))
                 call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_nameb, repe_type,&
                             [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                             'REEL', 'REEL', '12', 0.d0, list_rela_line)
                 nb_rela_line = nb_rela_line + 1
              endif
           endif
!
        else
           do i_dim = 1, nlag*nb_dim
               if (l_mult_crack) then
                   cmp_name(1) = ddlc1(3*(v_rela_cmp(2*(i_edge-1)+1)-1)+i_dim)
                   cmp_name(2) = ddlc1(3*(v_rela_cmp(2*(i_edge-1)+2)-1)+i_dim)
               else
                   if(nb_dim.eq.3) then
                       cmp_name(1) = ddlc1(i_dim)
                       cmp_name(2) = ddlc1(i_dim)
                   else
                       cmp_name(1) = ddlc2(i_dim)
                       cmp_name(2) = ddlc2(i_dim)
                   endif
               endif
               call afrela(coef_real, [coef_cplx_dumm], cmp_name, node_name, repe_type,&
                           [0.d0]   , 2, vale_real, value_cplx_dumm, vale_func_dumm,&
                           'REEL', 'REEL', '12', 0.d0, list_rela_line)
               nb_rela_line = nb_rela_line + 1
           end do
        endif
    end do
!
    if (lxthm.and.l_mult_crack) then
       call detrsd('CHAM_ELEM_S', chjon)
       call jedetr(cnxinv)
    endif
!
    call jedema()
end subroutine
