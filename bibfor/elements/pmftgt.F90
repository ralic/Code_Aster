subroutine pmftgt(nf, e, sim, sip, dep,&
                  mod)
    implicit none
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!
! ======================================================================
! -----------------------------------------------------------
! ---  INTEGRATIONS SUR LA SECTION
! IN
!          NF     : NOMBRE DE FIBRES
!          E      : MODULE D'YOUNG
!          SIM(*) : CONTRAINTE DANS LES FIBRES "-"
!          SIP(*) : CONTRAINTE DANS LES FIBRES "+"
!          DEP(*) : INCREMENT DE DEFORMATION DANS LES FIBRES
!
! OUT
!          MOD(*) : MODULE TANGENT DES FIBRES
! -----------------------------------------------------------
    integer :: nf, i
    real(kind=8) :: sim(nf), sip(nf), dep(nf), mod(nf), e
!
    do 20 i = 1, nf
        if (abs( dep(i) ) .gt. 1.0d-10) then
            mod(i) = abs( (sip(i) - sim(i)) / dep(i) )
        else
            mod(i) = e
        endif
20  end do
!
end subroutine
