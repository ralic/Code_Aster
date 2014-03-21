subroutine rctype(jmat     , nb_para_list, para_list_name, para_list_vale, para_vale,&
                  para_type, keyw_factz  , keywz)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
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
    integer, intent(in) :: jmat
    integer, intent(in) :: nb_para_list
    character(len=*), intent(in) :: para_list_name(*)
    real(kind=8), intent(in) :: para_list_vale(*) 
    real(kind=8), intent(out) :: para_vale
    character(len=*), intent(out) :: para_type
    character(len=*), optional, intent(in) :: keyw_factz
    character(len=*), optional, intent(in) :: keywz
!
! --------------------------------------------------------------------------------------------------
!
! Comportment - Utility
!
! Get informations (value and type) about parameters for traction curve
!
! --------------------------------------------------------------------------------------------------
!
! In  ktrac          : simple keyword for traction curve (SIGM by default)
!                       1  - 'TRACTION'
!                       2  - 'META_TRACTION'
! In  jmat           : JEVEUX adress to coded material
! In  nb_para_list   : number of possible parameters (except EPSI)
! In  para_list_name : list of possible parameters name (except EPSI)
! In  para_list_vale : list of parameters value (except EPSI)
! Out para_vale      : parameter value (except EPSI)
! Out para_type      : parameter type (except EPSI)
! In  keyw_factz     : factor keyword for traction curve (TRACTION by default)
! In  keywz          : simple keyword for traction curve (SIGM by default)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: lfct, lmat, lsup
    parameter ( lmat = 7 , lfct = 9, lsup=2)
!
    integer :: icomp, ipi, idf, nbf, ivalk, ik, ipif, jpro
    integer :: imate, nbmat
    character(len=24) :: para_name(2)
    character(len=16) :: keyw_fact
    character(len=8) :: keyw
    integer :: ipara, nb_para, i_para_list
!
! --------------------------------------------------------------------------------------------------
!
    para_type = ' '
    para_vale = 0.d0
!
! - Number of materials data for current element (only one by element)
!
    nbmat = zi(jmat)
    ASSERT(nbmat.eq.1)
!
! - Coded material
!
    imate = jmat+zi(jmat+nbmat+1)
!
! - Simple keyword for traction curve
!
    if (present(keywz)) then
        keyw  = keywz
    else
        keyw = 'SIGM'
    endif
!
! - Factor keyword for traction curve
!
    if (present(keyw_factz)) then
        keyw_fact = keyw_factz
    else
        keyw_fact = 'TRACTION'
    endif
    
!
! - Get index for factor keyword
!
    do icomp = 1, zi(imate+1)
        if (keyw_fact .eq. zk16(zi(imate)+icomp-1)) then
            ipi = zi(imate+2+icomp-1)
            goto 11
        endif
    end do
    call utmess('F', 'COMPOR5_1', sk = keyw_fact)
11  continue
!
! - Get index for simple keyword
!
    idf   = zi(ipi)+zi(ipi+1)
    nbf   = zi(ipi+2)
    ivalk = zi(ipi+3)
    do ik = 1, nbf
        if (keyw .eq. zk8(ivalk+idf+ik-1)) then
            if (keyw.eq.'SIGM') then
                ipif = ipi+lmat-1+lfct*(ik-1)
            elseif ((keyw.eq.'SIGM_F1').or.&
                    (keyw.eq.'SIGM_F2').or.&
                    (keyw.eq.'SIGM_F3').or.&
                    (keyw.eq.'SIGM_F4').or.&
                    (keyw.eq.'SIGM_C')) then
                ipif = ipi+lmat-1+lfct*(ik-1)+lsup*(ik-1)
            else
                ASSERT(.false.)
            endif
            goto 21
        endif
    end do
    ASSERT(.false.)
21  continue
!
! - Get name of parameter(s)
!
    jpro = zi(ipif+1)
!
    if (zk24(jpro) .eq. 'NAPPE') then
        nb_para = 2
        para_name(1) = zk24(jpro+2)
        para_name(2) = zk24(jpro+5)
    else
        nb_para = 1
        para_name(1) = zk24(jpro+2)
        if (para_name(1) .eq. 'EPSI') then
            para_vale = para_list_vale(1)
            goto 999
        else
            call utmess('F', 'COMPOR5_2', sk = para_name(1))
        endif
    endif
!
! - <NAPPE> case
!
    do ipara = 1, nb_para
        if (para_name(ipara)(1:4) .ne. 'EPSI') then
            do i_para_list = 1, nb_para_list
                if (para_list_name(i_para_list) .eq. para_name(ipara)) then
                    para_vale = para_list_vale(i_para_list)
                    para_type = para_list_name(i_para_list)
                    goto 999
                endif
            end do
        endif
    end do
!
    call utmess('F', 'COMPOR5_4')
!
999 continue
!
end subroutine
