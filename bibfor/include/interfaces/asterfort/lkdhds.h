        interface
          subroutine lkdhds(nbmat,mater,invar,s,dhds,retcom)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: dhds(6)
            integer :: retcom
          end subroutine lkdhds
        end interface
