subroutine satura(hydr, p1, sat, dsatp1)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    include 'asterfort/u2mess.h'
    real(kind=8) :: p1, sat, dsatp1
    character(len=16) :: hydr
!
    if (hydr .eq. 'HYDR') then
!
        call u2mess('F', 'ALGORITH9_80')
!
        if (p1 .gt. 0) then
            sat = (1.d0+(2.907d-8*p1)**(1.388d0))**(-.963d0)
            dsatp1 = (-.963d0)*1.388d0*2.907d-8*(2.907d-8*p1)**( .388d0) *(1.d0+(2.907d-8*p1)**1.&
                     &388d0)**(-1.963d0)
        else
            sat = 1.d0
            dsatp1 = 0.d0
        endif
!
    endif
!
end subroutine
