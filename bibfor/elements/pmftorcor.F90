subroutine pmftorcor(tygrfi, nbpout, gxjx, gxjxpout, deplm, deplp, xl, fl)


    implicit none
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
! -----------------------------------------------------------
! 
!            CORRECTION DES EFFORTS GENERALISES POUR TORSION
!
! -----------------------------------------------------------
! --- IN :
!       typgrfi    : type des fibres : 1 ou 2      
!       nbpou      : nombre de sous-poutres
!       gxjx       : module de torsion multifibre
!       gxjxpou(*) : Module de torsion pour multipoutres
!       deplm      : champs de deplacement au temps -
!       deplp      : champs de deplacement au temps +
!       xl         : longueur de l'element

! --- OUT 
!       fl         : efforts nodaux corriges pour torsion


! -----------------------------------------------------------
#include "asterfort/codent.h"
#include "asterfort/utmess.h"
    integer :: tygrfi, nbpout, ii
    real(kind=8) :: gxjx, gxjxpout(*), deplm(12), deplp(12), xl, fl(12)
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!    parameter (zero=0.0d+0)

    if (tygrfi .eq. 1 ) then
      fl(10) = gxjx*(deplm(10)+deplp(10)-deplm(4)-deplp(4))/xl
      fl(4) = -fl(10)
    elseif (tygrfi .eq. 2 ) then
      do ii =1, nbpout 
        fl(10) = fl(10) + gxjxpout(ii)*(deplm(10)+deplp(10)-deplm(4)-deplp(4))/xl
        fl(4) = fl(4) - gxjxpout(ii)*(deplm(10)+deplp(10)-deplm(4)-deplp(4))/xl
      enddo
    endif


end subroutine
