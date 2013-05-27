        interface
          subroutine matrkb(nb1,ndimx,nddlx,nddlet,ktdc,alpha,rig1,&
     &coef)
            integer :: nddlx
            integer :: ndimx
            integer :: nb1
            integer :: nddlet
            real(kind=8) :: ktdc(ndimx,ndimx)
            real(kind=8) :: alpha
            real(kind=8) :: rig1(nddlx,nddlx)
            real(kind=8) :: coef
          end subroutine matrkb
        end interface
