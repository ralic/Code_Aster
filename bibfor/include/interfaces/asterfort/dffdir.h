        interface
          subroutine dffdir(ndim,baslo,inoff,vdir)
            integer :: ndim
            real(kind=8) :: baslo(*)
            integer :: inoff
            real(kind=8) :: vdir(ndim)
          end subroutine dffdir
        end interface
