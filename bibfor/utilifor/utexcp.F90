subroutine utexcp(num, idmess)
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     ARGUMENTS :
!        NUM    = NUMERO DE L'EXCEPTION
!        IDMESS = IDENTIFIANT DU MESSAGE
!     ------------------------------------------------------------------
    implicit none
#include "asterc/isnnem.h"
#include "asterc/r8nnem.h"
#include "asterfort/u2mesg.h"
    integer :: num
    character(len=*) :: idmess
!     ------------------------------------------------------------------
    real(kind=8) :: valr(1)
    character(len=8) :: valk(1)
    integer :: nr, nk, ni, vali(1)
    integer :: nexcep
    common /utexc /  nexcep
!
    nexcep = num
!     ------------------------------------------------------------------
    ni = 0
    nk = 0
    nr = 0
    valk(1) = ' '
    vali(1) = isnnem()
    valr(1) = r8nnem()
    call u2mesg('Z', idmess, nk, valk, ni,&
                vali, nr, valr)
!     ------------------------------------------------------------------
end subroutine
