subroutine dicor3(k0, dur, dryr, si1, si2,&
                  dnsdu, dmsdt, dnsdt)
! ----------------------------------------------------------------------
    implicit none
    real(kind=8) :: k0(78), dur, dryr, si1(12), si2(12)
    real(kind=8) :: dnsdu, dmsdt, dnsdt
! ----------------------------------------------------------------------
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
!     UTILITAIRE POUR LE COMPORTEMENT CORNIERE.
!
! ----------------------------------------------------------------------
!
! IN  : K0     : COEFFICIENTS DE RAIDEUR TANGENTE
!       DUR    : INCREMENT DE DEPLACEMENT
!       DRYR   : INCREMENT DE ROTATION
!       SI1    : EFFORTS GENERALISES PRECEDENTS
!       SI2    : EFFORTS GENERALISES COURANTS
!
! OUT : DNSDU  :
!       DMSDT  :
!       DNSDT  :
!
! ----------------------------------------------------------------------
    if (dur .ne. 0.d0) then
        dnsdu = (si2(7)-si1(7)) / dur
    else
        dnsdu = k0(1)
    endif
!
    if (dryr .ne. 0.d0) then
        dmsdt = (si2(11)-si1(11)) / dryr
    else
        dmsdt = k0(15)
    endif
    dnsdt = 0.d0
! ----------------------------------------------------------------------
!
end subroutine
