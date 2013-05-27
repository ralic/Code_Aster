subroutine casurf(ndim, nno, geom, surff)
    implicit none
    integer :: ndim, nno
    real(kind=8) :: geom(ndim, nno), surff
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! ROUTINE CASURF : CALCUL DE LA SURFACE DE L ELEMENT 2D
! GEOM CONTIENT LES COORDONN2ES DES NOEUDS
!
!
! ----------------------------------------------------------------------
!
    integer :: maxfa1, maxdi1
    parameter    (maxfa1=6,maxdi1=3)
!
    real(kind=8) :: t(maxdi1, maxfa1)
    integer :: ifa, i, ideb, ifin
    real(kind=8) :: vol, pdvd2, pdvd1
!
! ----------------------------------------------------------------------
!
    do 2 ifa = 1, nno
!
        ideb=ifa
        ifin=ifa+1
        if (ifin .gt. nno) then
            ifin=ifin-nno
        endif
        do 2 i = 1, ndim
            t(i,ifa)=geom(i,ifin)-geom(i,ideb)
!
 2      continue
!
    if (nno .eq. 3) then
        vol=abs(t(1,1)*t(2,2)-t(2,1)*t(1,2))/2.d0
    else
        pdvd1 = t(1,1)*t(2,4)-t(2,1)*t(1,4)
        pdvd2 = t(1,3)*t(2,2)-t(2,3)*t(1,2)
        vol = (abs(pdvd1)+abs(pdvd2))/2.d0
    endif
!
    surff=vol
!
end subroutine
