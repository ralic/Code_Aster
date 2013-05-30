subroutine pmfits(nf, ncf, vf, vsig, vs)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
! -----------------------------------------------------------
! ---  INTEGRATION DES CONTRAINTES SUR LA SECTION
!      (CALCUL DES FORCES INTERIEURES)
! --- IN : FIBRES
!          NF : NOMBRE DE FIBRES
!          NCF: NOMBRE DE CARACTERISTIQUES SUR CHAQUE FIBRE
!          VF(1,*) : Y FIBRES
!          VF(2,*) : Z FIBRES
!          VF(3,*) : S FIBRES
!          VF(4-6,*) : AUTRES CARACTERISTIQUES
!          VSIG(*) : CONTRAINTE NORMALE DANS CHAQUE FIBRE
!
! --- OUT : SECTION
!          VS(1) : INT(SIG.DS)      = N0
!          VS(2) : INT(SIG.Y.DS)    = -MFZ0
!          VS(3) : INT(SIG.Z.DS)    = MFY0
! -----------------------------------------------------------
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nf, ncf, i
    real(kind=8) :: vf(ncf, nf), vsig(nf), vs(3), zero, sigsf
    parameter (zero=0.0d+0)
    character(len=2) :: kncf
!
    do 10 i = 1, 3
        vs(i)=zero
10  end do
!
    if (ncf .eq. 3) then
! --- ON A 3 CARACTERISTIQUES PAR FIBRE : Y, Z ET S
        do 100 i = 1, nf
            sigsf=vsig(i)*vf(3,i)
            vs(1)=vs(1)+sigsf
            vs(2)=vs(2)+vf(1,i)*sigsf
            vs(3)=vs(3)+vf(2,i)*sigsf
100      continue
    else if (ncf.eq.6) then
! --- ON A 6 CARACTERISTIQUES PAR FIBRE : Y, Z, S, IZ, IY ET IYZ
        call u2mess('F', 'ELEMENTS2_41')
    else
! --- ERREUR SUR NCARFIB
        call codent(ncf, 'G', kncf)
        call u2mesk('F', 'ELEMENTS2_40', 1, kncf)
    endif
!
!
end subroutine
