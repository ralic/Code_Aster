subroutine calc_norm_coef(model       , name_gd     , nb_cmp_max  , nb_cmp_in, norm  , &
                          calc_elem   , list_cmp    , nb_coef_user, coef_user, chcoef, &
                          chcalc      , nb_cmp_act)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/codent.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/wkvect.h"
#include "asterfort/u2mesk.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=8), intent(in) :: name_gd
    character(len=8), intent(in) :: model
    integer, intent(in) :: nb_cmp_max
    integer, intent(in) :: nb_cmp_in
    character(len=16) , intent(in) :: norm
    character(len=4) , intent(in) :: calc_elem
    integer, intent(in) :: nb_coef_user
    real(kind=8), intent(in) :: coef_user(*)
    character(len=24), intent(in) :: list_cmp
    character(len=19), intent(in) :: chcoef
    character(len=19), intent(in) :: chcalc
    integer, intent(out) :: nb_cmp_act
!
! --------------------------------------------------------------------------------------------------
!
! Construction of <CARTE> of <NEUT_R> by selection of components
!
! Modify stress and strain to help contracted tensorial product with non-diagonal term
!
! --------------------------------------------------------------------------------------------------
!
! Example:
!  <IN>  <name_gd>          SIEF_R
!  <IN>  <nb_cmp_max>       30
!  <IN>  <nb_cmp_in>        5
!  <IN>  <nb_coef_user>     0
!  <IN>  <list_cmp>         SIXX SIYY SIXY TOTO TATA 
!  <OUT> <chcoef>           <CARTE of [NEUT_R]>
!                           X1   X2   X3   X4   X5   (...) X30
!                           1.0  1.0  2.0  0.0  0.0        0.0  
!  <OUT> <nb_cmp_act>       3
!
! --------------------------------------------------------------------------------------------------
!
! In  model        : name of model
! In  name_gd      : name of <GRANDEUR> 
! In  nb_cmp_max   : maximum number of components in <GRANDEUR>
! In  nb_cmp_in    : number of components in <list_cmp>
! In  norm         : type of norm
! In  calc_elem    : computation on element
!                       'NORM' : norm
!                       'SQUA' : norm * norm
! In  nb_coef_user : number of coefficients provided by user (0 if not provided)
! In  coef_user    : list of coefficients provided by user 
! In  list_cmp     : name of list of components to filter
! In  chcoef       : name of <CARTE> contains coefficient for each component
! In  chcalc       : name of <CARTE> for type of calc_elem (NORM or SQUA)
! Out nb_cmp_act   : number of selected components in <list_cmp_act>
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: list_coe_act, list_cmp_act
    integer :: j_lcoe_act, j_lcmp_act
    integer :: j_lcmp_act_in
    integer :: icmp, i_calc_elem
    character(len=4) :: ki
    real(kind=8) :: coef_init, coef_cross, r8dummy
    integer :: idummy
    character(len=8) :: k8dummy
    complex(kind=8) :: c16dummy
    character(len=4) :: prod_type
!
! --------------------------------------------------------------------------------------------------
!
    nb_cmp_act = 0
    list_coe_act = '&&CALC_NORM_COEF.CO'
    list_cmp_act = '&&CALC_NORM_COEF.CM'
    if (nb_cmp_in .gt. nb_cmp_max) then
        call u2mesk('F', 'CHAMPS_18', 1, name_gd)
    endif
    call jeveuo(list_cmp, 'L', j_lcmp_act_in)
!
! - Options
!       prod_type    : coefficient for non-diagonal terms
!                       'TENS' : tensor product PROD = A x A = SIXY * SIXY
!                       'CONT' : contracted product PROD = (A : A) = 2 * SIXY * SIXY
!
    if (norm.eq.'L2') then
        prod_type = 'CONT'
    elseif (norm.eq.'FROBENIUS') then
        prod_type = 'TENS'
    else
        write(6,*) 'NORM: ',norm
        ASSERT(.false.)
    endif
!
! - Create <CARTE>
!
    call wkvect(list_coe_act, 'V V R' , nb_cmp_max, j_lcoe_act)
    call wkvect(list_cmp_act, 'V V K8', nb_cmp_max, j_lcmp_act)
!
! - User coefficients: only for NEUT_R
!
    if ((nb_coef_user .ne. 0) .and. (name_gd(1:6).eq.'NEUT_R')) then
        call u2mess('A','CHAMPS_16')
    endif
!
! - Coefficient for cross components
!
    if (prod_type .eq. 'TENS') then
        coef_cross = 1.d0
    elseif (prod_type .eq. 'CONT') then
        coef_cross = 2.d0
    else
        ASSERT(.false.)
    endif
!
! - Init <CARTE>
!
    coef_init = 1.d0
    if (nb_coef_user .eq. 0) coef_init = 0.d0
    do icmp = 1, nb_cmp_max
        call codent(icmp, 'G', ki)
        zk8(j_lcmp_act-1+icmp) = 'X'//ki(1:len(ki))
        zr(j_lcoe_act-1+icmp)  = coef_init
    enddo
!
! - Set <CARTE>
!
    if (name_gd(1:4) .eq. 'DEPL') then
        do icmp = 1, nb_cmp_in 
            if (zk8(j_lcmp_act_in-1+icmp) .eq. 'DX' .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'DY' .or. &
                zk8(j_lcmp_act_in-1+icmp) .eq. 'DZ') then
                zr(j_lcoe_act-1+icmp) = 1.d0
                nb_cmp_act = nb_cmp_act+1
            else
                zr(j_lcoe_act-1+icmp) = 0.d0
            endif
        enddo
    else if (name_gd(1:4).eq.'TEMP') then
        do icmp = 1, nb_cmp_in
            if (zk8(j_lcmp_act_in-1+icmp) .eq. 'TEMP') then
                zr(j_lcoe_act-1+icmp) = 1.d0
                nb_cmp_act = nb_cmp_act+1
            else
                zr(j_lcoe_act-1+icmp) = 0.d0
            endif
        enddo
    else if (name_gd(1:4).eq.'FLUX') then
        do icmp = 1, nb_cmp_in
            if (zk8(j_lcmp_act_in-1+icmp) .eq. 'FLUX' .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'FLUY' &
                .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'FLUZ') then
                zr(j_lcoe_act-1+icmp) = 1.d0
                nb_cmp_act = nb_cmp_act+1
            else
                zr(j_lcoe_act-1+icmp) = 0.d0
            endif
        enddo
   else if (name_gd(1:4).eq.'EPSI') then
        do icmp = 1, nb_cmp_in
            if (zk8(j_lcmp_act_in-1+icmp) .eq. 'EPXX' .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'EPYY' &
                .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'EPZZ    ') then
                zr(j_lcoe_act-1+icmp) = 1.d0
                nb_cmp_act = nb_cmp_act+1
            elseif (zk8(j_lcmp_act_in-1+icmp).eq.'EPXY'.or. zk8(j_lcmp_act_in-1+icmp).eq.'EPXZ'  &
                    .or. zk8(j_lcmp_act_in-1+icmp).eq.'EPYZ')then
                zr(j_lcoe_act-1+icmp) = coef_cross
                nb_cmp_act = nb_cmp_act+1
            else
                zr(j_lcoe_act-1+icmp) = 0.d0
            endif
        enddo
    else if (name_gd(1:4).eq.'SIEF') then
        do icmp = 1, nb_cmp_in
            if (zk8(j_lcmp_act_in-1+icmp) .eq. 'SIXX' .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'SIYY'&
                .or. zk8(j_lcmp_act_in-1+icmp) .eq. 'SIZZ') then
                zr(j_lcoe_act-1+icmp) = 1.d0
                nb_cmp_act = nb_cmp_act+1
            elseif(zk8(j_lcmp_act_in-1+icmp).eq.'SIXY' .or. zk8(j_lcmp_act_in-1+icmp).eq.'SIXZ'&
                   .or. zk8(j_lcmp_act_in-1+icmp).eq.'SIYZ') then
                zr(j_lcoe_act-1+icmp) = coef_cross
                nb_cmp_act = nb_cmp_act+1
            else
                zr(j_lcoe_act-1+icmp) = 0.d0
            endif
        enddo
    else if (name_gd(1:6).eq.'NEUT_R') then
        if (nb_coef_user .ne. 0) then
            do icmp = 1, nb_coef_user
                zr(j_lcoe_act-1+icmp) = coef_user(icmp)
                if (coef_user(icmp) .ne. 0.d0) nb_cmp_act = nb_cmp_act+1
            enddo
        endif
    else
        ASSERT(.false.)
    endif
!
! - Set calc_elem option
!
    if (calc_elem.eq.'NORM') then
        i_calc_elem = 1
    elseif (calc_elem.eq.'SQUA') then
        i_calc_elem = -1
    else
        ASSERT(.false.)
    endif
!
! - Construct <CARTE> of coefficients to filter components
!
    call mecact('V', chcoef, 'MODELE', model, 'NEUT_R',&
                nb_cmp_max, zk8(j_lcmp_act), idummy, zr(j_lcoe_act), c16dummy,&
                k8dummy)
!
! - Construct <CARTE> for type of calc_elem (NORM or SQUA)
!
    call mecact('V', chcalc, 'MODELE', model, 'NEUT_I',&
                1, 'X1', i_calc_elem, r8dummy, c16dummy,&
                k8dummy)
!
    call jedetr(list_coe_act)
    call jedetr(list_cmp_act)
!
end subroutine
