        interface
          subroutine nmedel(ndim,typmod,imate,deps,sigm,option,sigp,&
     &dsidep)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: deps(6)
            real(kind=8) :: sigm(6)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: dsidep(6,6)
          end subroutine nmedel
        end interface
