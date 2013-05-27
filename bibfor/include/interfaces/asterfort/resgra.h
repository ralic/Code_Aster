        interface
          subroutine resgra(mat,matf,vcine,niter,epsi,criter,nsecm,&
     &rsolu,solveu,istop,iret)
            character(*) :: mat
            character(*) :: matf
            character(*) :: vcine
            integer :: niter
            real(kind=8) :: epsi
            character(len=19) :: criter
            integer :: nsecm
            real(kind=8) :: rsolu(*)
            character(len=19) :: solveu
            integer :: istop
            integer :: iret
          end subroutine resgra
        end interface
