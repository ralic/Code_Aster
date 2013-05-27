        interface
          subroutine lcfrlo(ndim,typmod,imate,epsm,deps,vim,option,sig&
     &,vip,dsidep)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: epsm(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidep(6,6)
          end subroutine lcfrlo
        end interface
