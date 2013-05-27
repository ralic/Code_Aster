        interface
          subroutine cvmcvg(dy,ddy,nr,itmax,toler,iter,intg,typess,&
     &essai,icomp,irteti)
            real(kind=8) :: dy(*)
            real(kind=8) :: ddy(*)
            integer :: nr
            integer :: itmax
            real(kind=8) :: toler
            integer :: iter
            integer :: intg
            integer :: typess
            real(kind=8) :: essai
            integer :: icomp
            integer :: irteti
          end subroutine cvmcvg
        end interface
