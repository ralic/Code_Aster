        interface
          subroutine dfmdf(dim,f,dsidep)
            integer :: dim
            real(kind=8) :: f(dim)
            real(kind=8) :: dsidep(dim,dim)
          end subroutine dfmdf
        end interface
