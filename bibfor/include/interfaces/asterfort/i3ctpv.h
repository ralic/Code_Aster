        interface
          subroutine i3ctpv(epsi,noeud,nbn,coordo,pave,coupe)
            real(kind=8) :: epsi
            integer :: noeud(*)
            integer :: nbn
            real(kind=8) :: coordo(*)
            real(kind=8) :: pave(*)
            logical :: coupe
          end subroutine i3ctpv
        end interface
