        interface
          subroutine lckimp(ndim,typmod,option,mat,epsm,deps,vim,&
     &nonloc,sig,vip,dsidep)
            integer :: ndim
            character(len=8) :: typmod
            character(len=16) :: option
            integer :: mat
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(2)
            real(kind=8) :: nonloc(3)
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidep(6,6,4)
          end subroutine lckimp
        end interface
