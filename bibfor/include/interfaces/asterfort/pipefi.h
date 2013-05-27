        interface
          subroutine pipefi(npg,lgpg,mate,geom,vim,ddepl,deplm,ddepl0,&
     &ddepl1,dtau,copilo,typmod)
            integer :: lgpg
            integer :: npg
            integer :: mate
            real(kind=8) :: geom(2,4)
            real(kind=8) :: vim(lgpg,npg)
            real(kind=8) :: ddepl(2,4)
            real(kind=8) :: deplm(2,4)
            real(kind=8) :: ddepl0(2,4)
            real(kind=8) :: ddepl1(2,4)
            real(kind=8) :: dtau
            real(kind=8) :: copilo(5,npg)
            character(len=8) :: typmod(2)
          end subroutine pipefi
        end interface
