subroutine ccl11j(fronti, frn, n, t)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
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
! VERSION COMPLEXE DE COL11J
    implicit none
    integer :: n
    complex(kind=8) :: fronti(*), t(n), frn(*)
!
    integer :: i, j, ic1, id1, jd1, l
    ic1 = 2
    l = n
    jd1 = 1
    do 120 j = 1, n
        id1 = ic1
        do 110 i = 1, l
            frn(jd1) = frn(jd1) - t(j)*fronti(id1)
            jd1 = jd1 + 1
            id1 = id1 + 1
110      continue
        l = l - 1
        ic1 = ic1 + 1
120  end do
end subroutine
