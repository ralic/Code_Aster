subroutine nbbit1(ec, nb1)
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
!
    integer :: ec, nb1
!
!*********************************************************************
!
!       NB1 := NBR DE BIT A 1 DANS LA REPRESENTATION BINAIRE DE EC
!
!       (APPLICATION DIRECT AU COMPTAGE DE COMPOSANTES ACTIVES
!        D' UNE GRANDEUR DECRITE PAR ENTIER CODE)
!
!*********************************************************************
!
    integer :: test, i
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nb1 = 0
    test = 1
!
    do 10, i= 1, 30, 1
!
    test = 2*test
!
    if (iand(ec,test) .gt. 0) then
!
        nb1 = nb1 + 1
!
    endif
!
    10 end do
!
end subroutine
