        interface
          subroutine nmedpi(spg,sdg,qg,d,npg,typmod,mate,up,ud,geom,&
     &nno,def)
            integer :: nno
            real(kind=8) :: spg(2)
            real(kind=8) :: sdg(2)
            real(kind=8) :: qg(2,2)
            real(kind=8) :: d(4,2)
            integer :: npg
            character(len=8) :: typmod(*)
            integer :: mate
            real(kind=8) :: up(8)
            real(kind=8) :: ud(8)
            real(kind=8) :: geom(2,nno)
            real(kind=8) :: def(4,nno,2)
          end subroutine nmedpi
        end interface
