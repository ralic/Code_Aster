subroutine matrth(fami, npg, young, nu, alpha,&
                  indith)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
!
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvala.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    integer :: iret
!
    real(kind=8) :: valres(26), valpar
    integer :: icodre(26)
    character(len=8) :: nomres(26), nompar
    character(len=10) :: phenom
    real(kind=8) :: young, nu, alpha
    integer :: npg
    character(len=4) :: fami
!
!
!-----------------------------------------------------------------------
    integer :: indith, jcou, jmate
    real(kind=8) :: temp
!-----------------------------------------------------------------------
    indith=0
    nompar = 'TEMP'
!
    call jevech('PMATERC', 'L', jmate)
!
    call rccoma(zi(jmate), 'ELAS', 1, phenom, icodre(1))
!
    if (phenom .eq. 'ELAS') then
!
        call jevech('PNBSP_I', 'L', jcou)
!
        nomres(1)='E'
        nomres(2)='NU'
        nomres(3)='ALPHA'
!
        call moytem(fami, npg, 3*zi(jcou), '+', temp,&
                    iret)
        call rcvala(zi(jmate), ' ', phenom, 1, 'TEMP',&
                    [temp], 3, nomres, valres, icodre,&
                    1)
        if (icodre(3) .ne. 0) then
            indith = -1
            goto 9999
        endif
!
!     MATERIAU ISOTROPE
!
        young = valres(1)
        nu = valres(2)
        alpha = valres(3)
!
    else if (phenom .eq. 'ELAS_ORTH') then
        nomres(1)='ALPHA_L'
        nomres(2)='ALPHA_T'
        call rcvalb(fami, 1, 1, '+', zi(jmate),&
                    ' ', phenom, 0, nompar, [valpar],&
                    2, nomres, valres, icodre, 1)
        if (icodre(1) .ne. 0) then
            indith = -1
            goto 9999
        else
            if ((valres(1).eq.0.d0) .and. (valres(2).eq.0.d0)) then
                indith = -1
                goto 9999
            else
                call utmess('F', 'ELEMENTS2_33')
            endif
        endif
    else
        call utmess('F', 'ELEMENTS_42')
    endif
!
!
9999  continue
end subroutine
