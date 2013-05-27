        interface
          subroutine mahsms(ind1,nb1,xi,ksi3s2,intsr,xr,epais,vectn,&
     &vectg,vectt,hsfm,hss)
            integer :: ind1
            integer :: nb1
            real(kind=8) :: xi(3,*)
            real(kind=8) :: ksi3s2
            integer :: intsr
            real(kind=8) :: xr(*)
            real(kind=8) :: epais
            real(kind=8) :: vectn(9,3)
            real(kind=8) :: vectg(2,3)
            real(kind=8) :: vectt(3,3)
            real(kind=8) :: hsfm(3,9)
            real(kind=8) :: hss(2,9)
          end subroutine mahsms
        end interface
