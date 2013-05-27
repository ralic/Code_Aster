        interface
          subroutine rldlg3(metres,lmat,xsol,cxsol,nbsol)
            character(*) :: metres
            integer :: lmat
            real(kind=8) :: xsol(*)
            complex(kind=8) :: cxsol(*)
            integer :: nbsol
          end subroutine rldlg3
        end interface
