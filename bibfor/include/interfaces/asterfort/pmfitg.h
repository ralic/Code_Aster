        interface
          subroutine pmfitg(nf,ncf,vf,vs)
            integer :: ncf
            integer :: nf
            real(kind=8) :: vf(ncf,nf)
            real(kind=8) :: vs(6)
          end subroutine pmfitg
        end interface
