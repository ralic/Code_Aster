        interface
          subroutine lcbrgm(ndim,typmod,imate,epsm,deps,vim,option,sig&
     &,vip,dsidpt,proj,cdrett)
            integer :: ndim
            character(len=8) :: typmod(*)
            integer :: imate
            real(kind=8) :: epsm(12)
            real(kind=8) :: deps(12)
            real(kind=8) :: vim(*)
            character(len=16) :: option
            real(kind=8) :: sig(6)
            real(kind=8) :: vip(*)
            real(kind=8) :: dsidpt(6,6,2)
            real(kind=8) :: proj(6,6)
            integer :: cdrett
          end subroutine lcbrgm
        end interface
