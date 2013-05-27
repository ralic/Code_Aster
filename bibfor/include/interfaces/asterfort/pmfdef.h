        interface
          subroutine pmfdef(nf,ncf,vf,dege,deff)
            integer :: ncf
            integer :: nf
            real(kind=8) :: vf(ncf,nf)
            real(kind=8) :: dege(6)
            real(kind=8) :: deff(nf)
          end subroutine pmfdef
        end interface
