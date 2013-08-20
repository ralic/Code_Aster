subroutine afrela(coef_real, coef_cplx, ddl_name, node_name, repe_type,&
                  repe_defi, nbterm, vale_real, vale_cplx, vale_func,&
                  type_coef, type_vale, type_lagr, epsi, lisrez)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/crelrl.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
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
!
    integer, intent(in) :: nbterm
    real(kind=8), intent(in) :: coef_real(nbterm)
    complex(kind=8), intent(in) :: coef_cplx(nbterm)
    character(len=8), intent(in) :: ddl_name(nbterm)
    character(len=8), intent(in) :: node_name(nbterm)
    integer, intent(in) :: repe_type(nbterm)
    real(kind=8), intent(in) :: repe_defi(3, nbterm)
    real(kind=8), intent(in) :: vale_real
    complex(kind=8), intent(in) :: vale_cplx
    character(len=*), intent(in) :: vale_func
    character(len=4), intent(in) :: type_coef
    character(len=4), intent(in) :: type_vale
    character(len=2), intent(in) :: type_lagr
    real(kind=8), intent(in) :: epsi
    character(len=*), intent(in) :: lisrez
!
! --------------------------------------------------------------------------------------------------
!
! New linear relation 
!
!       coef(iterm) . ddl_name(iterm) = vale
!
! With:  
!       iterm = 1,nbterm
!       coef = coef_real if type_coef = 'REEL'
!       coef = coef_cplx if type_coef = 'COMP'
!       vale = vale_real if type_vale = 'REEL'
!       vale = vale_cplx if type_vale = 'COMP'
!       vale = vale_func if type_vale = 'FONC'
!
! Saving in lisrel object (created if not exist)
!
! --------------------------------------------------------------------------------------------------
!
! In  coef_real : real coefficient
! In  coef_cplx : complex coefficient
! In  ddl_name  : name of dof in linear relation
! In  node_name : name of nodes in linear relation
! In  repe_type : type of coordiante system to apply linear relation
!                   if 0 - Global coordinate system
!                   if 2 - Local (2D) coordinate system
!                   if 3 - Local (2D) coordinate system
! In  repe_defi : defintion of local coordinate system
! In  nbterm    : number of terms in linear relation
! In  type_vale : affected value type (real, complex or function)
! In  vale_real : affected value if real
! In  vale_func : affected value if function
! In  vale_cplx : affected value if complex
! In  coef_type : type of coefficient (real or complex)
! In  lagr_type : type of lagrange multpilers (position of Lagrange and physical dof)
!                   if '12' - First Lagrange multiplier before physical dof, second after
!                   if '22' - First Lagrange multiplier after physical dof, second after too
! In  epsi      : tolerance to detect "zero" coefficient
! In  lisrel    : JEVEUX object list_rela for relations list management 
! --------------------------------------------------------------------------------------------------
!
    real(kind=8) :: vale_real_norm
    complex(kind=8) :: vale_cplx_norm
    integer :: imult
    character(len=8) :: k8bid
    character(len=8) :: ddl_name_tran(3), ddl_name_rota(3)
    character(len=19) :: lisrel
    integer :: idbeta, idcoef, iddl, idim, idlagr, idnbre
    integer :: idnoeu, idpoin, idsurc, idterm, ifm, ipoint, iret
    integer :: iterm, idirect, lonuti, lveclr, mdim, nbrel0
    integer :: nbrela, nbrmax, nbterr, niv, k
    real(kind=8) :: norm_coef
    logical :: l_rota
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! - Initializations
!
    lisrel = lisrez
    ddl_name_tran(1) = 'DX'
    ddl_name_tran(2) = 'DY'
    ddl_name_tran(3) = 'DZ'
    ddl_name_rota(1) = 'DRX'
    ddl_name_rota(2) = 'DRY'
    ddl_name_rota(3) = 'DRZ'
    l_rota = .false.
    vale_real_norm = vale_real
    vale_cplx_norm = vale_cplx
!
! - Information about linear relation before normalization
!
    if (niv .eq. 2) then
        write (ifm,*) ' '
        write (ifm,*) '_RELA IMPRESSION D''UNE RELATION LINEAIRE ENTRE '&
     &    ,nbterm,' DDLS. (AVANT NORMALISATION DE LA RELATION)'
        do iterm = 1,nbterm
            if (repe_type(iterm) .eq. 0) then
                if (type_coef .eq. 'REEL') then
                    write (ifm,101) coef_real(iterm),node_name(iterm),ddl_name(iterm)
                else if (type_coef.eq.'COMP') then
                    write (ifm,103) dble(coef_cplx(iterm)),dimag(coef_cplx(iterm)),&
                                     node_name(iterm), ddl_name(iterm)
                else
                    ASSERT(.false.)
                endif
            else
                if (type_coef .eq. 'REEL') then
                    write (ifm,102) coef_real(iterm),node_name(iterm),ddl_name(iterm),&
                                    (repe_defi(idirect,iterm),idirect=1,repe_type(iterm))
                else if (type_coef.eq.'COMP') then
                    write (ifm,104) dble(coef_cplx(iterm)),dimag(coef_cplx(iterm)),&
                                     node_name(iterm), ddl_name(iterm), &
                                     (repe_defi(idirect,iterm),idirect=1,repe_type(iterm))
                else
                    ASSERT(.false.)
                endif
            endif
        enddo
        if (type_vale .eq. 'REEL') then
            write (ifm,*) '_RELA = ',vale_real
        else if (type_vale.eq.'COMP') then
            write (ifm,*) '_RELA = ',vale_cplx
        else if (type_vale.eq.'FONC') then
            write (ifm,*) '_RELA = ',vale_func
        else
            ASSERT(.false.)
        endif
    endif
!
! - Normalization ratio
!
    if (type_coef .eq. 'REEL') then
        norm_coef = 0.d0
        do iterm = 1,nbterm
            norm_coef = max(norm_coef,abs(coef_real(iterm)))
        enddo
        if (norm_coef .eq. 0.d0) call u2mess('F', 'CHARGES2_97')
    else if (type_coef.eq.'COMP') then
        norm_coef = 0.d0
        do iterm = 1,nbterm
                norm_coef = max(norm_coef,abs(coef_cplx(iterm)))
        enddo
        if (norm_coef .eq. 0.d0) call u2mess('F', 'CHARGES2_97')
    else
        ASSERT(.false.)
    endif
!
! - Normalization of values
!
    if (type_vale .eq. 'REEL') then
        vale_real_norm = vale_real_norm/norm_coef
    else if (type_vale.eq.'COMP') then
        vale_cplx_norm = vale_cplx_norm/norm_coef
    else if (type_vale.eq.'FONC') then
! ----- Alarm if normalization ratio too much different from 1 ...
        if ((norm_coef.gt.1.d3) .or. (norm_coef.lt.1.d-3)) call u2mess('A', 'CHARGES2_99')
! ----- ... but cannot normalize function value !
        norm_coef=1.d0
    endif
!
! - No <LIST_RELA> object -> creation
!
    call jeexin(lisrel//'.RLCO', iret)
    if (iret .eq. 0) call crelrl(type_coef, type_vale, 'V', lisrel)
!
! - How many linear relations ?
!
    call jeveuo(lisrel//'.RLNR', 'E', idnbre)
    nbrel0 = zi(idnbre)
    nbrela = nbrel0 + 1
!
! - Initial maximum linear relations number 
!
    call jelira(lisrel//'.RLNT', 'LONMAX', nbrmax, k8bid)
!
! - Length of vectors for all relation terms
!
    call jelira(lisrel//'.RLCO', 'LONMAX', lveclr, k8bid)
!
! - Real length used
!
    call jeveuo(lisrel//'.RLPO', 'E', idpoin)
    if (nbrel0 .eq. 0) then
        lonuti = 0
    else
        lonuti = zi(idpoin+nbrel0-1)
    endif
!
! - Real number of terms: zero (epsi) terms vanished + active terms (local coordinate system)
!
    nbterr = 0
    do iterm = 1, nbterm
        if (type_coef .eq. 'COMP') then
            if (abs(coef_cplx(iterm)) .gt. epsi) then
                if (repe_type(iterm) .eq. 0) then
                    nbterr = nbterr + 1
                else
                    nbterr = nbterr + repe_type(iterm)
                endif
            endif
        else if (type_coef.eq.'REEL') then
            if (abs(coef_real(iterm)) .gt. epsi) then
                if (repe_type(iterm) .eq. 0) then
                    nbterr = nbterr + 1
                else
                    nbterr = nbterr + repe_type(iterm)
                endif
            endif
        else
            ASSERT(.false.)
        endif
    end do
!
! - Increase object size if necessary
!
    if (lonuti+nbterr .ge. lveclr) then
        imult = (lonuti+nbterr)/lveclr + 1
        call juveca(lisrel//'.RLCO', imult*lveclr)
        call juveca(lisrel//'.RLDD', imult*lveclr)
        call juveca(lisrel//'.RLNO', imult*lveclr)
    endif
!
! - No enough place for linear relation -> increase objects size
!
    if (nbrela .ge. nbrmax) then
        imult = nbrela/nbrmax + 1
        call juveca(lisrel//'.RLBE', imult*nbrmax)
        call juveca(lisrel//'.RLNT', imult*nbrmax)
        call juveca(lisrel//'.RLPO', imult*nbrmax)
        call juveca(lisrel//'.RLSU', imult*nbrmax)
        call juveca(lisrel//'.RLLA', imult*nbrmax)
    endif
!
! - Linear relation access
!
    call jeveuo(lisrel//'.RLNR', 'E', idnbre)
    call jeveuo(lisrel//'.RLCO', 'E', idcoef)
    call jeveuo(lisrel//'.RLDD', 'E', iddl)
    call jeveuo(lisrel//'.RLNO', 'E', idnoeu)
    call jeveuo(lisrel//'.RLBE', 'E', idbeta)
    call jeveuo(lisrel//'.RLNT', 'E', idterm)
    call jeveuo(lisrel//'.RLPO', 'E', idpoin)
    call jeveuo(lisrel//'.RLSU', 'E', idsurc)
    call jeveuo(lisrel//'.RLLA', 'E', idlagr)
!
! - New length
!
    zi(idnbre) = nbrela
    zk8(idlagr+nbrela-1) (1:2) = type_lagr
    if (nbrel0 .eq. 0) then
        ipoint = 0
    else
        ipoint = zi(idpoin+nbrel0-1)
    endif
    zi(idterm+nbrela-1) = nbterr
    if (nbrel0 .eq. 0) then
        zi(idpoin) = nbterr
    else
        zi(idpoin+nbrela-1) = zi(idpoin+nbrel0-1) + nbterr
    endif
!
! - New linear relation affectation
!
    k = 0
!
    if (type_coef .eq. 'COMP') then
        do iterm = 1, nbterm
            if (abs(coef_cplx(iterm)) .gt. epsi) then
                if (repe_type(iterm) .eq. 0) then
!
! ----------------- Global coordinate system
!
                    k = k + 1
                    zc(idcoef+ipoint+k-1) = coef_cplx(iterm)/norm_coef
                    zk8(iddl+ipoint+k-1) = ddl_name(iterm)
                    zk8(idnoeu+ipoint+k-1) = node_name(iterm)
                else
!
! ----------------- Local coordinate system: rotation or translation ?
!
                    if (ddl_name(iterm) .eq. 'DEPL') then
                        l_rota = .false.
                    else if (ddl_name(iterm).eq.'ROTA') then
                        l_rota = .true.
                    else
                        ASSERT(.false.)
                    endif
!
! ----------------- Change coordinate system
! ----------------- DEPL --> repe_defi(1)*U  + repe_defi(2)*V  + repe_defi(3)*W
! ----------------- ROTA --> repe_defi(1)*RX + repe_defi(2)*RY + repe_defi(3)*RZ
!
                    mdim = repe_type(iterm)
                    do idim = 1, mdim
                        k = k + 1
                        zc(idcoef+ipoint+k-1) = coef_cplx(iterm)/norm_coef*repe_defi(idim,iterm)
                        zk8(idnoeu+ipoint+k-1) = node_name(iterm)
                        if (.not.l_rota) then
                            zk8(iddl+ipoint+k-1) = ddl_name_tran(idim)
                        else
                            zk8(iddl+ipoint+k-1) = ddl_name_rota(idim)
                        endif
                    enddo
                endif
            endif
        enddo
!
    elseif (type_coef .eq. 'REEL') then
        do iterm = 1, nbterm
            if (abs(coef_real(iterm)) .gt. epsi) then
                if (repe_type(iterm) .eq. 0) then
!
! ----------------- Global coordinate system
!
                    k = k + 1
                    zr(idcoef+ipoint+k-1) = coef_real(iterm)/norm_coef
                    zk8(iddl+ipoint+k-1) = ddl_name(iterm)
                    zk8(idnoeu+ipoint+k-1) = node_name(iterm)
                else
!
! ----------------- Local coordinate system: rotation or translation ?
!
                    if (ddl_name(iterm) .eq. 'DEPL') then
                        l_rota = .false.
                    else if (ddl_name(iterm).eq.'ROTA') then
                        l_rota = .true.
                    else
                        ASSERT(.false.)
                    endif
!
! ----------------- Change coordinate system
! ----------------- DEPL --> repe_defi(1)*U  + repe_defi(2)*V  + repe_defi(3)*W
! ----------------- ROTA --> repe_defi(1)*RX + repe_defi(2)*RY + repe_defi(3)*RZ
!
                    mdim = repe_type(iterm)
                    do idim = 1, mdim
                        k = k + 1
                        zr(idcoef+ipoint+k-1) = coef_real(iterm)/norm_coef*repe_defi(idim,iterm)
                        zk8(idnoeu+ipoint+k-1) = node_name(iterm)
                        if (.not.l_rota) then
                            zk8(iddl+ipoint+k-1) = ddl_name_tran(idim)
                        else
                            zk8(iddl+ipoint+k-1) = ddl_name_rota(idim)
                        endif
                    enddo
                endif
            endif
        enddo
    else
        ASSERT(.false.)
    endif
!
! - Value affectation
!
    if (type_vale .eq. 'REEL') then
        zr(idbeta+nbrela-1) = vale_real_norm
    else if (type_vale.eq.'COMP') then
        zc(idbeta+nbrela-1) = vale_cplx_norm
    else if (type_vale.eq.'FONC') then
        zk24(idbeta+nbrela-1) = vale_func
    else
        ASSERT(.false.)
    endif
!
    call jedema()
!
    101 format (' _RELA ',e14.7,a10,a10)
    103 format (' _RELA ',e14.7,1x,e14.7,a10,a10)
    102 format (' _RELA ',e14.7,a10,a10,3x,3 (1x,e14.7))
    104 format (' _RELA ',e14.7,1x,e14.7,a10,a10,3x,3 (1x,e14.7))
!
end subroutine
