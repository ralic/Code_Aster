subroutine ibcael(type)
    implicit none
#include "asterfort/jedupc.h"
#include "asterfort/jeinif.h"
#include "asterfort/jelibf.h"
#include "asterfort/utmess.h"
    character(len=*) :: type
!     -----------------------------------------------------------------
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
!     -----------------------------------------------------------------
    character(len=8) :: nomf
    integer :: info
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    info = 1
    nomf = 'ELEMBASE'
    if (type .eq. 'ECRIRE') then
        call jeinif('DEBUT', 'SAUVE', nomf, 'C', 300,&
                    512, 100)
        call jedupc('G', '&CATA', 1, 'C', '&BATA',&
                    .false._1)
        call jelibf('SAUVE', 'C', info)
        call utmess('I', 'SUPERVIS_16')
    else
        call jeinif('POURSUITE', 'LIBERE', nomf, 'C', 300,&
                    512, 100)
        call jedupc('C', '&BATA', 1, 'G', '&CATA',&
                    .false._1)
        call jelibf('LIBERE', 'C', info)
        call utmess('I', 'SUPERVIS_17')
    endif
end subroutine
