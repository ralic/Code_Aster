        interface
          subroutine inithm(imate,yachai,yamec,phi0,em,alpha0,k0,cs,&
     &biot,t,epsv,depsv,epsvm)
            integer :: imate
            logical :: yachai
            integer :: yamec
            real(kind=8) :: phi0
            real(kind=8) :: em
            real(kind=8) :: alpha0
            real(kind=8) :: k0
            real(kind=8) :: cs
            real(kind=8) :: biot
            real(kind=8) :: t
            real(kind=8) :: epsv
            real(kind=8) :: depsv
            real(kind=8) :: epsvm
          end subroutine inithm
        end interface
