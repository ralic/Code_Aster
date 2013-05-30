subroutine pmffor(nf, ncf, vf, se, ff)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!          NCF    : NOMBRE DE CARACTERISTIQUES SUR CHAQUE FIBRE
!          VF     : VF(1,*) : Y FIBRES
!                   VF(2,*) : Z FIBRES
!                   VF(3,*) : S FIBRES
!                   VF(4-6,*) : AUTRES CARACTERISTIQUES
!
!          SE(*)  : CONTRAINTES DANS LES FIBRES
!
! OUT
!          EFFORTS GENERALISES INTEGRES DANS LA SECTION
!          FF  : FF(1) = +INT(SE.DS)   = N
!                FF(2) = +INT(SE.Z.DS) = MY
!                FF(3) = -INT(SE.Y.DS) = MZ
! -----------------------------------------------------------
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    integer :: nf, ncf, i
    real(kind=8) :: vf(ncf, nf), se(nf), ff(3), zero, sf
    parameter (zero=0.0d+0)
    character(len=2) :: kncf
!
    do 10 i = 1, 3
        ff(i) = zero
10  end do
!
    if (ncf .eq. 3) then
! --- 3 CARACTERISTIQUES PAR FIBRE : Y, Z ET S
        do 20 i = 1, nf
            sf = se(i)*vf(3,i)
            ff(1) = ff(1) + sf
            ff(2) = ff(2) + vf(2,i)*sf
            ff(3) = ff(3) - vf(1,i)*sf
20      continue
!
    else
! --- ERREUR SUR NCARFI
        call codent(ncf, 'G', kncf)
        call u2mesk('F', 'ELEMENTS2_40', 1, kncf)
    endif
!
end subroutine
