subroutine pmfitg(nf, ncf, vf, vs)
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
! ======================================================================
    implicit none
! -----------------------------------------------------------
! ---  INTEGRATIONS SUR LA SECTION (GEOMETRIQUE)
! --- IN : FIBRES
!          NF : NOMBRE DE FIBRES
!          NCF: NOMBRE DE CARACTERISTIQUES PAR FIBRE
!          VF(1,*) : Y FIBRES
!          VF(2,*) : Z FIBRES
!          VF(3,*) : S FIBRES
!          VF(4-6,*) : CARACTERISTIQUES SUPPLEMENTAIRES
!                      (PAS FAIT ACTUELLEMENT)
!
! --- OUT : SECTION
!   VS(1) : SURFACE TOTALE
!   VS(2) : MOMENT STATIQUE / OZ   (POUR FLEXION XOY) = INT(Y.DS)
!   VS(3) : MOMENT STATIQUE / OY   (POUR FLEXION XOZ) = INT(Z.DS)
!   VS(4) : MOMENT QUADRATIQUE / OZ   (POUR FLEXION XOY) = INT(Y.Y.DS)
!   VS(5) : MOMENT QUADRATIQUE / OY   (POUR FLEXION XOZ) = INT(Z.Z.DS)
!   VS(6) : MOMENT PRODUIT  = INT(Y.Z.DS)
! -----------------------------------------------------------
    include 'asterfort/codent.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    integer :: nf, ncf, i
    real(kind=8) :: vf(ncf, nf), vs(6), zero
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    parameter (zero=0.0d+0)
    character(len=2) :: kncf
!
    do 10 i = 1, 6
        vs(i) = zero
10  end do
!
    if (ncf .eq. 3) then
! --- ON A 3 CARACTERISTIQUES PAR FIBRE : Y, Z ET S
        do 20 i = 1, nf
            vs(1) = vs(1) + vf(3,i)
            vs(2) = vs(2) + vf(1,i)*vf(3,i)
            vs(3) = vs(3) + vf(2,i)*vf(3,i)
            vs(4) = vs(4) + vf(1,i)*vf(1,i)*vf(3,i)
            vs(5) = vs(5) + vf(2,i)*vf(2,i)*vf(3,i)
            vs(6) = vs(6) + vf(1,i)*vf(2,i)*vf(3,i)
20      continue
    else if (ncf.eq.6) then
! --- ON A 6 CARACTERISTIQUES PAR FIBRE : Y, Z, S, IZ, IY ET IYZ
        call u2mess('F', 'ELEMENTS2_41')
    else
! --- ERREUR SUR NCARFI
        call codent(ncf, 'G', kncf)
        call u2mesk('F', 'ELEMENTS2_40', 1, kncf)
    endif
!
end subroutine
