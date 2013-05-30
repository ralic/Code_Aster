subroutine pmfdef(nf, ncf, vf, dege, deff)
    implicit none
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
! -----------------------------------------------------------
! ---  DEFORMATION AXIALE DES FIBRES
! --- IN : FIBRES
!          NF : NOMBRE DE FIBRES
!          NCF: NOMBRE DE CARACTERISTIQUES PAR FIBRE
!          VF(1,*) : Y FIBRES
!          VF(2,*) : Z FIBRES
!          VF(3,*) : S FIBRES
!          VF(4-6,*) : CARACTERISTIQUES SUPPLEMENTAIRES
!                      (PAS FAIT ACTUELLEMENT)
! --- IN : DEFORMATIONS GENERALISEES SUR L'ELEMENT
!          DEGE(6)
! --- OUT : DEFORMATIONS AXIALES DES FIBRES
!            DEFF(NF)
! -----------------------------------------------------------
    integer :: nf, ncf, i
    real(kind=8) :: vf(ncf, nf), dege(6), deff(nf)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    do 10 i = 1, nf
        deff(i) = dege(1) - vf(1,i)*dege(6) + vf(2,i)*dege(5)
10  end do
end subroutine
