subroutine rsliso(fami, kpg, ksp, poum, imat,&
                  p, rp, drdp)
    implicit none
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!       LOI ECROUISSAGE ISOTROPE R(P,T) ENTREE POINT PAR POINT
!       ET  DERIVEE LOI ECROUISSAGE ISOTROPE R(P,T)/ P
!       IN  P      :  DEFORMATION CUMULEE
!           IMAT   :  ADRESSE DU MATERIAU CODE
!       OUT RP     :  R (P,TEMP)
!       OUT DRDP   :  DRDP ( P,TEMP) = INTERPOLATION LINEAIRE SUR P,TEMP
!       ----------------------------------------------------------------
#include "asterfort/rcfonc.h"
#include "asterfort/rctrac.h"
#include "asterfort/rctype.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
    real(kind=8) :: temp, p, rp, e, drdp, airerp, dum, resu
    integer :: imat, jprol, jvale, nbvale, kpg, ksp, iret
    character(len=*) :: fami
    character(len=1) :: poum
    character(len=8) :: type
!       ----------------------------------------------------------------
! --  TEMPERATURE
    call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                ksp, temp, iret)
    call rctype(imat, 1, 'TEMP', temp, resu,&
                type)
    if ((type.eq.'TEMP') .and. (iret.eq.1)) then
        call utmess('F', 'CALCULEL_31')
    endif
    call rctrac(imat, 1, 'SIGM', resu, jprol,&
                jvale, nbvale, e)
    call rcfonc('V', 1, jprol, jvale, nbvale,&
                dum, dum, dum, p, rp,&
                drdp, airerp, dum, dum)
end subroutine
