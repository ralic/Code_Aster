        interface
          subroutine dfdm1d(nno,poids,dfrdk,coor,dfdx,cour,jacp,cosa,&
     &sina)
            integer :: nno
            real(kind=8) :: poids
            real(kind=8) :: dfrdk(1)
            real(kind=8) :: coor(*)
            real(kind=8) :: dfdx(1)
            real(kind=8) :: cour
            real(kind=8) :: jacp
            real(kind=8) :: cosa
            real(kind=8) :: sina
          end subroutine dfdm1d
        end interface
