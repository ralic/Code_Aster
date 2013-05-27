        interface
          subroutine betinc(materf,nmat,sige,nseuil,dpc,dpt,sigf,&
     &verifc,verift)
            integer :: nmat
            real(kind=8) :: materf(nmat,2)
            real(kind=8) :: sige(6)
            integer :: nseuil
            real(kind=8) :: dpc
            real(kind=8) :: dpt
            real(kind=8) :: sigf(6)
            real(kind=8) :: verifc
            real(kind=8) :: verift
          end subroutine betinc
        end interface
