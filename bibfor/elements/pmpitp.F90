subroutine pmpitp(flp, nbpout, yj, zj, fl)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
! -----------------------------------------------------------
! ---  INTEGRATION DES CONTRAINTES SUR LA SECTION MULTIPOUTRE
!      (CALCUL DES FORCES INTERIEURES)
! --- IN 
!       flp(12,*) : tableau de forces elementaires sur sous-poutres
!       nbpout    : nombre d assemblages de fibres
!       yi(*)     : position Y des sous-poutres
!       zi(*)     : position Z des sous-poutres
!
! --- OUT
!       fl(12)    : forces elementaires
! -----------------------------------------------------------
#include "asterfort/codent.h"
#include "asterfort/utmess.h"
    integer :: i, nbpout
    real(kind=8) :: fl(12), flp(12,*), yj(*), zj(*)
!
    do  i = 1, nbpout
        fl(1)=fl(1)+flp(1,i)
        fl(2)=fl(2)+flp(2,i)
        fl(3)=fl(3)+flp(3,i)
        fl(4)=fl(4)+flp(4,i)+flp(3,i)*yj(i)-flp(2,i)*zj(i)
        fl(5)=fl(5)+flp(5,i)+flp(1,i)*zj(i)
        fl(6)=fl(6)+flp(6,i)-flp(1,i)*yj(i)
        fl(7)=fl(7)+flp(7,i)
        fl(8)=fl(8)+flp(8,i)
        fl(9)=fl(9)+flp(9,i)
        fl(10)=fl(10)+flp(10,i)+flp(9,i)*yj(i)-flp(8,i)*zj(i)
        fl(11)=fl(11)+flp(11,i)+flp(7,i)*zj(i)
        fl(12)=fl(12)+flp(12,i)-flp(7,i)*yj(i)
    enddo
!

!
end subroutine
