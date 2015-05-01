subroutine utrcyl(point, dire, orig, p)
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
    implicit none
!
    real(kind=8) :: point(3), dire(3), orig(3), p(3, 3)
    real(kind=8) :: sca, xnorm
!-----------------------------------------------------------------------
!
    sca=(point(1)-orig(1))*dire(1)+&
     &    (point(2)-orig(2))*dire(2)+&
     &    (point(3)-orig(3))*dire(3)
!
    p(3,1)=(point(1)-orig(1))-sca*dire(1)
    p(3,2)=(point(2)-orig(2))-sca*dire(2)
    p(3,3)=(point(3)-orig(3))-sca*dire(3)
!
    xnorm=sqrt(p(3,1)**2+p(3,2)**2+p(3,3)**2)
    p(3,1)=p(3,1)/xnorm
    p(3,2)=p(3,2)/xnorm
    p(3,3)=p(3,3)/xnorm
!
    xnorm=sqrt(dire(1)**2+dire(2)**2+dire(3)**2)
    p(1,1)=dire(1)/xnorm
    p(1,2)=dire(2)/xnorm
    p(1,3)=dire(3)/xnorm
!
    p(2,1)= p(3,2)*p(1,3)-p(3,3)*p(1,2)
    p(2,2)= p(3,3)*p(1,1)-p(3,1)*p(1,3)
    p(2,3)= p(3,1)*p(1,2)-p(3,2)*p(1,1)
!
end subroutine
