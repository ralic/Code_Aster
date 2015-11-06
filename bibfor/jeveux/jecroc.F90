subroutine jecroc(nomlu)
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
    implicit none
#include "asterfort/jjallc.h"
#include "asterfort/jjcroc.h"
#include "asterfort/jjvern.h"
#include "asterfort/jxveuo.h"
#include "asterfort/utmess.h"
    character(len=*), intent(in) :: nomlu
!     ------------------------------------------------------------------
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
    character(len=32) :: noml32
    integer :: icre, iret, jctab, itab(1)
    character(len=8) :: nume
!-----------------------------------------------------------------------
    integer :: ibacol, l
!-----------------------------------------------------------------------
    data             nume  / '$$XNUM  '/
! DEB ------------------------------------------------------------------
    l = len(nomlu)
    if (l .ne. 32) then
        call utmess('F', 'JEVEUX_95')
    endif
!
    icre = 3
    noml32 = nomlu
    call jjvern(noml32, icre, iret)
!
    if (iret .eq. 0) then
        call utmess('F', 'JEVEUX_25', sk=noml32(1:24))
    else
        if (iret .eq. 1) then
!         ----- OBJET DE TYPE REPERTOIRE
            if (nomlu(25:32) .eq. nume) then
                call utmess('F', 'JEVEUX_96', sk=noml32)
            endif
            call jxveuo('E', itab, 1, jctab)
            call jjcroc('        ', icre)
        else if (iret .eq. 2) then
!         ----- REPERTOIRE DE COLLECTION --
            call jjallc(iclaco, idatco, 'E', ibacol)
            call jjcroc(nomlu(25:32), icre)
        else
            call utmess('F', 'JEVEUX_97', sk=noml32)
        endif
    endif
! FIN ------------------------------------------------------------------
end subroutine
