        interface
          subroutine mazacp(option,ndimsi,epsm,deps,epsane,ee,mazars,&
     &varm,varp,sigp,dsidep)
            character(len=16) :: option
            integer :: ndimsi
            real(kind=8) :: epsm(*)
            real(kind=8) :: deps(*)
            real(kind=8) :: epsane
            real(kind=8) :: ee
            real(kind=8) :: mazars(*)
            real(kind=8) :: varm(*)
            real(kind=8) :: varp(*)
            real(kind=8) :: sigp(*)
            real(kind=8) :: dsidep(6,6)
          end subroutine mazacp
        end interface
