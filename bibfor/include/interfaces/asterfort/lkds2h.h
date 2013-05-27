        interface
          subroutine lkds2h(nbmat,mater,invar,s,dhds,ds2hds,retcom)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: dhds(6)
            real(kind=8) :: ds2hds(6)
            integer :: retcom
          end subroutine lkds2h
        end interface
