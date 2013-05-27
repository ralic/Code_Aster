        interface
          subroutine pmfits(nf,ncf,vf,vsig,vs)
            integer :: ncf
            integer :: nf
            real(kind=8) :: vf(ncf,nf)
            real(kind=8) :: vsig(nf)
            real(kind=8) :: vs(3)
          end subroutine pmfits
        end interface
