        interface
          subroutine apchoi(dist,distm,posmai,posmam,tau1,tau1m,tau2,&
     &tau2m,ksi1,ksi1m,ksi2,ksi2m,iproj,iprojm,vect,vectm)
            real(kind=8) :: dist
            real(kind=8) :: distm
            integer :: posmai
            integer :: posmam
            real(kind=8) :: tau1(3)
            real(kind=8) :: tau1m(3)
            real(kind=8) :: tau2(3)
            real(kind=8) :: tau2m(3)
            real(kind=8) :: ksi1
            real(kind=8) :: ksi1m
            real(kind=8) :: ksi2
            real(kind=8) :: ksi2m
            integer :: iproj
            integer :: iprojm
            real(kind=8) :: vect(3)
            real(kind=8) :: vectm(3)
          end subroutine apchoi
        end interface
