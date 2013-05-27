        interface
          subroutine pmffor(nf,ncf,vf,se,ff)
            integer :: ncf
            integer :: nf
            real(kind=8) :: vf(ncf,nf)
            real(kind=8) :: se(nf)
            real(kind=8) :: ff(3)
          end subroutine pmffor
        end interface
