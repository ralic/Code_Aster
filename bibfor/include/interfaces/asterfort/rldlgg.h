        interface
          subroutine rldlgg(lmat,xsol,cxsol,nbsol)
            integer :: lmat
            real(kind=8) :: xsol(*)
            complex(kind=8) :: cxsol(*)
            integer :: nbsol
          end subroutine rldlgg
        end interface
