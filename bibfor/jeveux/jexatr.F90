function jexatr(nomc, noma)
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
! person_in_charge: j-pierre.lefebvre at edf.fr
    implicit none
    character(len=32) :: jexatr
#include "jeveux_private.h"
#include "asterfort/jjallc.h"
#include "asterfort/jjvern.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=*) :: nomc, noma
! ----------------------------------------------------------------------
    integer :: lk1zon, jk1zon, liszon, jiszon
    common /izonje/  lk1zon , jk1zon , liszon , jiszon
    integer :: iclas, iclaos, iclaco, idatos, idatco, idatoc
    common /iatcje/  iclas ,iclaos , iclaco , idatos , idatco , idatoc
!     ------------------------------------------------------------------
    integer :: numatr
    common /idatje/  numatr
!     ------------------------------------------------------------------
    integer :: idiadd, idlong, idlono, idluti
!-----------------------------------------------------------------------
    integer :: ibacol, ixiadd, ixlong, ixlono, ixluti
!-----------------------------------------------------------------------
    parameter    ( idiadd = 2  ,&
     &                             idlong = 7 ,&
     &               idlono = 8 , idluti = 9  )
!     ------------------------------------------------------------------
    integer :: icre, iret
    character(len=24) :: nom24
    character(len=6) :: nomalu
    character(len=8) :: ch8
    data             ch8      / '$$XATR  ' /
! DEB ------------------------------------------------------------------
!
    nom24 = nomc
    nomalu = noma
    if (nomalu .ne. 'LONCUM' .and. nomalu .ne. 'LONMAX' .and. nomalu .ne. 'LONUTI') then
        call u2mesk('F', 'JEVEUX1_28', 1, nomalu)
    endif
    icre = 0
    call jjvern(nom24//'        ', icre, iret)
    if (iret .ne. 2) then
        call u2mess('F', 'JEVEUX1_29')
    else
        call jjallc(iclaco, idatco, 'L', ibacol)
        if (nomalu .eq. 'LONCUM') then
            ixiadd = iszon ( jiszon + ibacol + idiadd )
            if (ixiadd .ne. 0) then
                call u2mess('F', 'JEVEUX1_30')
            endif
            ixlono = iszon ( jiszon + ibacol + idlono )
            if (ixlono .eq. 0) then
                call u2mesk('F', 'JEVEUX1_63', 1, 'LONCUM')
            endif
            jexatr = nom24//ch8
            numatr = ixlono
        else if (nomalu .eq. 'LONMAX') then
            ixlong = iszon ( jiszon + ibacol + idlong )
            if (ixlong .eq. 0) then
                call u2mesk('F', 'JEVEUX1_63', 1, 'LONMAX')
            endif
            jexatr = nom24//ch8
            numatr = ixlong
        else if (nomalu .eq. 'LONUTI') then
            ixluti = iszon ( jiszon + ibacol + idluti )
            if (ixluti .eq. 0) then
                call u2mesk('F', 'JEVEUX1_63', 1, 'LONUTI')
            endif
            jexatr = nom24//ch8
            numatr = ixluti
        endif
    endif
! DEB ------------------------------------------------------------------
end function
