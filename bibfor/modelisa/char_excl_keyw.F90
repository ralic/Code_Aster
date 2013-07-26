subroutine char_excl_keyw(keywordfact, keywordexcl, n_keyexcl)
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
    character(len=24), intent(in) :: keywordexcl
    integer, intent(out) :: n_keyexcl
!
! --------------------------------------------------------------------------------------------------
!
! AFFE_CHAR_MECA
!
! Create list of excluded keywords for using in char_read_keyw
!
! --------------------------------------------------------------------------------------------------
!
! In  keywordfact : factor keyword
! In  keywordexcl : name of JEVEUX object for excluded keywords
! Out n_keyexcl   : number of excluded keywords
!
! --------------------------------------------------------------------------------------------------
!
    integer :: n_keyexcl_affe
    parameter (n_keyexcl_affe = 9)
    character(len=16) :: excl_affe(n_keyexcl_affe)
!
    integer :: j_kexcl, i_keyw
    character(len=16) :: keyword
!
    data excl_affe  /'TOUT'         , 'GROUP_MA'    , 'MAILLE'          , 'GROUP_NO'    , 'NOEUD', &
                     'SANS_GROUP_MA', 'SANS_MAILLE' , 'SANS_GROUP_NO'   , 'SANS_NOEUD'  /
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Global affectation keywords - Count
! 
    n_keyexcl = 0  
    do i_keyw = 1,n_keyexcl_affe
        keyword = excl_affe(i_keyw)
        if (getexm(keywordfact,keyword) .eq. 1) n_keyexcl = n_keyexcl + 1
    enddo
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
    else
        call assert(.false.)
    endif
!
    if (n_keyexcl.eq.0) goto 99
!
! - Create excluded keyword object
!
    call wkvect(keywordexcl,'V V K16',n_keyexcl,j_kexcl)
!
! - Global affectation keywords - Affect
! 
    n_keyexcl = 0
    do i_keyw = 1,n_keyexcl_affe
        keyword = excl_affe(i_keyw)
        if (getexm(keywordfact,keyword) .eq. 1) then
            n_keyexcl = n_keyexcl + 1
            zk16(j_kexcl-1+n_keyexcl) = keyword
        endif
    enddo
!
! - Other keywords - Affect
!
    if (keywordfact.eq.'FACE_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk16(j_kexcl-1+n_keyexcl) = 'DNOR'
        n_keyexcl = n_keyexcl + 1
        zk16(j_kexcl-1+n_keyexcl) = 'DTAN'
    elseif (keywordfact.eq.'ARETE_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk16(j_kexcl-1+n_keyexcl) = 'DTAN'
    elseif (keywordfact.eq.'ARETE_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk16(j_kexcl-1+n_keyexcl) = 'DTAN'
    elseif (keywordfact.eq.'LIAISON_OBLIQUE') then
        n_keyexcl = n_keyexcl + 1
        zk16(j_kexcl-1+n_keyexcl) = 'ANGL_NAUT'
    elseif (keywordfact.eq.'DDL_IMPO') then
        n_keyexcl = n_keyexcl + 1
        zk16(j_kexcl-1+n_keyexcl) = 'LIAISON'
    elseif (keywordfact.eq.'TEMP_IMPO') then
! ----- Nothing else components
    elseif (keywordfact.eq.'PRES_IMPO') then
! ----- Nothing else components
    else
        call assert(.false.)
    endif
!
99  continue

    call jedema()
end subroutine
