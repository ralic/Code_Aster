        interface
          subroutine dfpdf(dim,f,dsidep)
            integer :: dim
            real(kind=8) :: f(dim)
            real(kind=8) :: dsidep(dim,dim)
          end subroutine dfpdf
        end interface
