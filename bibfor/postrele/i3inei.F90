subroutine i3inei(e1, e2, n1, n2, iret)
    implicit none
!
    integer :: e1(*), e2(*), n1, n2, iret
!
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     INCLUSION DE E1 DANS E2
!     ------------------------------------------------------------------
! IN  E1     : I : ENSEMBLE 1
! IN  E2     : I : ENSEMBLE 2
! IN  N1     : I : CARD(E1)
! IN  N2     : I : CARD(E2)
! OUT IRET   : I : REPONSE : E1 C E2 ==> IRET = 1  SINON  IRET = 0
!     ------------------------------------------------------------------
!
    integer :: i1, i2, n, cmpt
    logical(kind=1) :: inclus
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    inclus = .true.
    i1 = 1
100  continue
    if (inclus .and. (i1 .le. n1)) then
        n = e1(i1)
        cmpt = 0
        do 10, i2 = 1, n2, 1
        cmpt = cmpt + max(0,1-abs(n-e2(i2)))
10      continue
        inclus = (cmpt .gt. 0)
        i1 = i1 + 1
        goto 100
    endif
    if (inclus) then
        iret = 1
    else
        iret = 0
    endif
end subroutine
