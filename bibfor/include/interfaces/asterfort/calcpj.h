        interface
          subroutine calcpj(nbmat,mater,gamp,evp,sigd,sige,epssig,&
     &invare,gamps,evps,invars,b)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: gamp
            real(kind=8) :: evp
            real(kind=8) :: sigd(6)
            real(kind=8) :: sige(6)
            real(kind=8) :: epssig
            real(kind=8) :: invare
            real(kind=8) :: gamps
            real(kind=8) :: evps
            real(kind=8) :: invars
            real(kind=8) :: b
          end subroutine calcpj
        end interface
