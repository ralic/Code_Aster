        interface
          subroutine dffnor(ndim,baslo,inoff,vnor)
            integer :: ndim
            real(kind=8) :: baslo(*)
            integer :: inoff
            real(kind=8) :: vnor(ndim)
          end subroutine dffnor
        end interface
