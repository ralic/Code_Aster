subroutine char_excl_keyw(keywordfact, n_suffix, list_suffix, keywordexcl, n_keyexcl)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getexm.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
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
    character(len=16), intent(in) :: keywordfact
    integer, intent(in) :: n_suffix
    character(len=8), intent(in) :: list_suffix(n_suffix)
    character(len=24), intent(in) :: keywordexcl
    integer, intent(out) :: n_keyexcl
!
! --------------------------------------------------------------------------------------------------
!
! Loads affectation
!
! Create list of excluded keywords for using in char_read_keyw
!
! --------------------------------------------------------------------------------------------------
!
! In  n_suffix    : number of sufixes excluded for topological keywords
! In  list_suffix : list of sufixes excluded for topological keywords
! In  keywordfact : factor keyword
! In  keywordexcl : name of JEVEUX object for excluded keywords
! Out n_keyexcl   : number of excluded keywords
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n_keyexcl_affe
    parameter (n_keyexcl_affe = 8)
    character(len=24) :: excl_affe(n_keyexcl_affe)
    integer :: leng_affe(n_keyexcl_affe)
!
    integer :: j_kexcl, i_keyw, i_suffix
    character(len=24) :: keyword
    character(len=8) :: suffix
!
    data excl_affe  /'GROUP_MA'     , 'MAILLE'      , 'GROUP_NO'    , 'NOEUD', &
                     'SANS_GROUP_MA', 'SANS_MAILLE' , 'SANS_GROUP_NO', 'SANS_NOEUD'  /
    data leng_affe  /8, 6, 8, 5, &
                     13, 11 , 13, 10 /
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Global affectation keywords - Count
! 
    n_keyexcl = 0  
    if (getexm(keywordfact,'TOUT') .eq. 1) n_keyexcl = n_keyexcl + 1
!
! - Global affectation keywords - Count
! 
    if (n_suffix.eq.0) then
        do i_keyw = 1, n_keyexcl_affe
            keyword = excl_affe(i_keyw)
            if (getexm(keywordfact,keyword) .eq. 1) n_keyexcl = n_keyexcl + 1
        enddo
    else
        do i_suffix = 1, n_suffix
            suffix = list_suffix(i_suffix)
            do i_keyw = 1, n_keyexcl_affe
                keyword = excl_affe(i_keyw)(1:leng_affe(i_keyw))//suffix
                if (getexm(keywordfact,keyword) .eq. 1) n_keyexcl = n_keyexcl + 1
            enddo
        enddo
    endif
!
! - Other keywords - Count
! 
    if (keywordfact.eq.'FACE_IMPO') then
        n_keyexcl = n_keyexcl + 2
    elseif (keywordfact.eq.'ARETE_IMPO') then
        n_keyexcl = n_keyexcl + 1
    elseif (keywordfact.eq.'LIAISON_OBLIQUE') then
        n_keyexcl = n_keyexcl + 1
    elseif (keywordfact.eq.'DDL_IMPO') then
        n_keyexcl = n_keyexcl + 1
    elseif (keywordfact.eq.'TEMP_IMPO') then
! ----- Nothing else components
    elseif (keywordfact.eq.'PRES_IMPO') then
! ----- Nothing else components
    elseif (keywordfact.eq.'DDL_POUTRE') then
        n_keyexcl = n_keyexcl + 4
    elseif (keywordfact.eq.'LIAISON_SOLIDE') then
        n_keyexcl = n_keyexcl + 3
    else
        ASSERT(.false.)
    endif
!
    if (n_keyexcl.eq.0) goto 99
!
! - Create excluded keyword object
!
    call wkvect(keywordexcl,'V V K24', n_keyexcl, j_kexcl)
!
! - Global affectation keywords - Affect
! 
    n_keyexcl = 0
    if (getexm(keywordfact,'TOUT') .eq. 1) then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'TOUT'
    endif
    if (n_suffix.eq.0) then
        do i_keyw = 1, n_keyexcl_affe
            keyword = excl_affe(i_keyw)
            if (getexm(keywordfact,keyword) .eq. 1) then
                n_keyexcl = n_keyexcl + 1
                zk24(j_kexcl-1+n_keyexcl) = keyword
            endif
        enddo
    else
        do i_suffix = 1, n_suffix
            suffix = list_suffix(i_suffix)
            do i_keyw = 1, n_keyexcl_affe
                keyword = excl_affe(i_keyw)(1:leng_affe(i_keyw))//suffix
                if (getexm(keywordfact,keyword) .eq. 1) then
                    n_keyexcl = n_keyexcl + 1
                    zk24(j_kexcl-1+n_keyexcl) = keyword
                endif
            enddo
        enddo
    endif
!
! - Other keywords - Affect
!
    if (keywordfact.eq.'FACE_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'DNOR'
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'DTAN'
    elseif (keywordfact.eq.'ARETE_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'DTAN'
    elseif (keywordfact.eq.'ARETE_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'DTAN'
    elseif (keywordfact.eq.'LIAISON_OBLIQUE') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'ANGL_NAUT'
    elseif (keywordfact.eq.'DDL_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'LIAISON'
    elseif (keywordfact.eq.'TEMP_IMPO') then
! ----- Nothing else components
    elseif (keywordfact.eq.'PRES_IMPO') then
! ----- Nothing else components
    elseif (keywordfact.eq.'DDL_POUTRE') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'ANGL_VRIL'
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'VECT_Y'
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'MAILLE_REPE'
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'GROUP_MA_REPE'
    elseif (keywordfact.eq.'LIAISON_SOLIDE') then
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'TRAN'
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'DIST_MIN'
        n_keyexcl = n_keyexcl + 1
        zk24(j_kexcl-1+n_keyexcl) = 'NUME_LAGR'
    else
        ASSERT(.false.)
    endif
!
99  continue

    call jedema()
end subroutine
