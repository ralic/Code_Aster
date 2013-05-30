subroutine prvite(vec1, long, ip1, ip2, itp)
    implicit none
! ----------------------------------------------------------------------
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
!      CALCUL DU PROFIL DE VITESSE
! ----------------------------------------------------------------------
!
    include 'jeveux.h'
    real(kind=8) :: angle(71), vite(71), angl, vec1(long)
    integer :: ip(3)
!
!-----------------------------------------------------------------------
    integer :: i, ij, ip1, ip2, itp, j, k
    integer :: kk, long, long2
    real(kind=8) :: alfa, beta
!-----------------------------------------------------------------------
    data (angle(i),i=1,17)   /&
     &   0.d0   ,  15.3d0  ,  30.d0   ,  40.d0   ,  48.5d0  ,  61.3d0  ,&
     &  75.d0   ,  83.6d0  ,  90.d0   ,  96.4d0  , 105.d0   , 118.7d0  ,&
     & 131.5d0  , 140.d0   , 150.d0   , 164.7d0  , 180.d0 /
!
    data (angle(i),i=18,42)  /&
     &   0.d0   ,  14.5d0  ,  29.d0   ,  33.49d0 ,  33.51d0 ,  39.d0   ,&
     &  47.4d0  ,  60.d0   ,  70.99d0 ,  71.01d0 ,  75.4d0  ,  84.d0   ,&
     &  90.d0   ,  96.d0   , 104.6d0  , 108.99d0 , 109.01d0 , 120.d0   ,&
     & 132.6d0  , 141.d0   , 146.49d0 , 146.51d0 , 151.d0   , 165.d0   ,&
     & 180.d0   /
!
    data (angle(i),i=43,71)  /&
     &   0.d0   ,  14.8d0  ,  25.7d0  ,  25.72d0 ,  30.7d0  ,  39.6d0  ,&
     &  48.d0   ,  51.42d0 ,  51.44d0 ,  60.3d0  ,  75.2d0  ,  77.13d0 ,&
     &  77.15d0 ,  84.d0   ,  90.d0   ,  96.d0   , 102.85d0 , 102.87d0 ,&
     & 104.8d0  , 119.7d0  , 128.56d0 , 128.58d0 , 132.d0   , 140.4d0  ,&
     & 149.3d0  , 154.28d0 , 154.3d0  , 165.2d0  , 180.d0  /
!
    data (vite(j),j=1,17)   /&
     &   0.d0   ,   1.08d0 ,   1.77d0 ,   1.72d0 ,   1.33d0 ,   0.95d0 ,&
     &   0.67d0 ,   0.59d0 ,   0.58d0 ,   0.59d0 ,   0.67d0 ,   0.95d0 ,&
     &   1.33d0 ,   1.72d0 ,   1.77d0 ,   1.08d0 ,   0.d0  /
!
    data (vite(j),j=18,42)  /&
     &   0.d0   ,   0.2d0  ,   0.7d0  ,   1.d0   ,   0.18d0 ,   0.32d0 ,&
     &   0.48d0 ,   0.61d0 ,   0.7d0  ,   0.16d0 ,   0.29d0 ,   0.5d0  ,&
     &   0.54d0 ,   0.5d0  ,   0.29d0 ,   0.16d0 ,   0.7d0  ,   0.61d0 ,&
     &   0.48d0 ,   0.32d0 ,   0.18d0 ,   1.d0   ,   0.7d0  ,   0.2d0  ,&
     &   0.d0   /
!
    data (vite(j),j=43,71) /&
     &   0.d0   ,   0.48d0 ,   0.69d0 ,   0.27d0 ,   0.54d0 ,   0.89d0 ,&
     &   1.16d0 ,   1.24d0 ,   0.02d0 ,   0.08d0 ,   0.79d0 ,   0.86d0 ,&
     &   0.17d0 ,   0.49d0 ,   0.55d0 ,   0.49d0 ,   0.17d0 ,   0.86d0 ,&
     &   0.79d0 ,   0.08d0 ,   0.02d0 ,   1.24d0 ,   1.16d0 ,   0.89d0 ,&
     &   0.54d0 ,   0.27d0 ,   0.69d0 ,   0.48d0 ,   0.d0 /
!
    data (ip(ij),ij=1,3)  / 1 , 18 , 43 /
!
!
    long2 = long/2
!
    do 10 kk = 1, long2
        if (kk .gt. ip1 .and. kk .lt. ip2) then
            angl = 180.d0*(vec1(kk)-vec1(ip1))/(vec1(ip2)-vec1(ip1))
            k = ip(itp)
20          continue
            if (angl .gt. angle(k+1)) then
                k = k+1
                goto 20
            endif
            alfa = (vite(k+1)-vite(k))/(angle(k+1)-angle(k))
            beta = ( vite(k)*angle(k+1)-(vite(k+1)*angle(k)))/ (angle( k+1)-angle(k) )
            vec1(long2+kk) = alfa*angl+beta
        else
            vec1(long2+kk) = 0.d0
        endif
10  end do
end subroutine
