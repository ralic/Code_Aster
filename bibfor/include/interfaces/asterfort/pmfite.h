        interface
          subroutine pmfite(nf,ncf,vf,ve,vs)
            integer :: ncf
            integer :: nf
            real(kind=8) :: vf(ncf,nf)
            real(kind=8) :: ve(nf)
            real(kind=8) :: vs(6)
          end subroutine pmfite
        end interface
