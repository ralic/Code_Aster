subroutine nthydr(hydrat)
    implicit   none
    include 'asterc/getfac.h'
    include 'asterc/getvtx.h'
    logical :: hydrat
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ----------------------------------------------------------------------
    integer :: nbocc, n1, i
    character(len=16) :: comp
    integer :: iarg
!     ------------------------------------------------------------------
!
    hydrat = .false.
!
    call getfac('COMP_THER_NL', nbocc)
!
    do 10 i = 1, nbocc
!
        call getvtx('COMP_THER_NL', 'RELATION', i, iarg, 1,&
                    comp, n1)
!
        if (comp(1:9) .eq. 'THER_HYDR') hydrat = .true.
!
10  end do
!
! FIN ------------------------------------------------------------------
end subroutine
