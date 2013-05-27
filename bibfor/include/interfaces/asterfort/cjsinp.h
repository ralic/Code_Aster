        interface
          subroutine cjsinp(mater,epsd,deps,sigf,vinf,niter,nvi,nivcjs&
     &,ndec,epscon)
            integer :: nvi
            real(kind=8) :: mater(14,2)
            real(kind=8) :: epsd(6)
            real(kind=8) :: deps(6)
            real(kind=8) :: sigf(6)
            real(kind=8) :: vinf(nvi)
            integer :: niter
            character(len=4) :: nivcjs
            integer :: ndec
            real(kind=8) :: epscon
          end subroutine cjsinp
        end interface
