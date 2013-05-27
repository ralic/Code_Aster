        interface
          subroutine lcfrge(ndim,typmod,imate,epsm,deps,vim,option,sig&
     &,vip,dsidpt,proj)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: epsm(12)
            real(kind=8) :: deps(12)
            real(kind=8) :: vim(2)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(2)
            real(kind=8) :: dsidpt(6,6,2)
            real(kind=8) :: proj(6,6)
          end subroutine lcfrge
        end interface
