        interface
          subroutine i2rgma(epsi,sor,sex,ror,rex,m1,m2,for,fex,tsor,&
     &tsex,tror,trex,tfor,tfex,tm1,tm2,adr)
            real(kind=8) :: epsi
            real(kind=8) :: sor
            real(kind=8) :: sex
            real(kind=8) :: ror
            real(kind=8) :: rex
            integer :: m1
            integer :: m2
            integer :: for
            integer :: fex
            real(kind=8) :: tsor(*)
            real(kind=8) :: tsex(*)
            real(kind=8) :: tror(*)
            real(kind=8) :: trex(*)
            integer :: tfor(*)
            integer :: tfex(*)
            integer :: tm1(*)
            integer :: tm2(*)
            integer :: adr
          end subroutine i2rgma
        end interface
