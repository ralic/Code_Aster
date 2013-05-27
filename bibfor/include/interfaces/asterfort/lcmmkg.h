        interface
          subroutine lcmmkg(zinv,nvi,vind,vinf,nmat,materf,mod,nr,dsde&
     &)
            real(kind=8) :: zinv(6,6)
            integer :: nvi
            real(kind=8) :: vind(*)
            real(kind=8) :: vinf(*)
            integer :: nmat
            real(kind=8) :: materf(*)
            character(len=8) :: mod
            integer :: nr
            real(kind=8) :: dsde(6,3,3)
          end subroutine lcmmkg
        end interface
