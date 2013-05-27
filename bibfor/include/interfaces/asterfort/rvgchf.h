        interface
          subroutine rvgchf(epsi,criter,nomsd,chpsym,acces,ival,rval,&
     &nbval,ncheff)
            real(kind=8) :: epsi
            character(*) :: criter
            character(len=8) :: nomsd
            character(len=16) :: chpsym
            character(len=2) :: acces
            integer :: ival(*)
            real(kind=8) :: rval(*)
            integer :: nbval
            character(len=16) :: ncheff
          end subroutine rvgchf
        end interface
