subroutine dtmfext(sd_dtm_, time, fext, buffdtm)
    implicit none
!
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
! person_in_charge: hassan.berro at edf.fr
!
! dtmfext : Calculate the external forces at instant "time"
!
#include "jeveux.h"
#include "asterfort/dtmget.h"
#include "asterfort/mdfext.h"
#include "asterfort/r8inir.h"


!
!   -0.1- Input/output arguments
    character(len=*)      , intent(in)  :: sd_dtm_
    real(kind=8)          , intent(out) :: time
    real(kind=8), pointer , intent(out) :: fext(:)
    integer     , pointer , intent(in)  :: buffdtm(:)

!
!   -0.2- Local variables
    integer          :: nbmode, ntotex
    real(kind=8)     :: r8b
    character(len=8) :: sd_dtm
!
    integer         , pointer :: idescf(:)  => null()
    integer         , pointer :: liad(:)    => null()
    integer         , pointer :: inumor(:)  => null()
    real(kind=8)    , pointer :: coefm(:)   => null()
    character(len=8), pointer :: nomfon(:)  => null()
!
!   0 - Initializations
    sd_dtm = sd_dtm_
!
    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode, buffer=buffdtm)
    call dtmget(sd_dtm, _NB_EXC_T, iscal=ntotex, buffer=buffdtm)

    call r8inir(nbmode, 0.d0, fext, 1)
!
    if (ntotex .ne. 0) then
        call dtmget(sd_dtm, _DESC_FRC,vi=idescf, buffer=buffdtm)
        call dtmget(sd_dtm, _FUNC_NAM,vk8=nomfon, buffer=buffdtm)
        call dtmget(sd_dtm, _COEF_MLT,vr=coefm, buffer=buffdtm)
        call dtmget(sd_dtm, _ADRES_VC,vi=liad, buffer=buffdtm)
        call dtmget(sd_dtm, _N_ORD_VC,vi=inumor, buffer=buffdtm)
        call mdfext(time, r8b, nbmode, ntotex, idescf,&
                    nomfon, coefm, liad, inumor, 1,&
                    fext)
    end if

end subroutine
