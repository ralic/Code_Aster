        interface
          subroutine caldfe(df,nr,nvi,vind,dfpds,fe,dfpdbs,msdgdt,drdy&
     &)
            integer :: nr
            real(kind=8) :: df(3,3)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: dfpds(3,3,3,3)
            real(kind=8) :: fe(3,3)
            real(kind=8) :: dfpdbs(3,3,30)
            real(kind=8) :: msdgdt(6,6)
            real(kind=8) :: drdy(nr,nr)
          end subroutine caldfe
        end interface
