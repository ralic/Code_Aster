        interface
          subroutine nmhoff(ndim,imate,inst,epsm,deps,option,sigp,&
     &dsidep)
            integer :: ndim
            integer :: imate
            real(kind=8) :: inst
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            character(len=16) :: option
            real(kind=8) :: sigp(6)
            real(kind=8) :: dsidep(6,6)
          end subroutine nmhoff
        end interface
