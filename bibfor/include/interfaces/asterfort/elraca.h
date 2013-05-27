        interface
          subroutine elraca(elrefz,ndim,nno,nnos,nbfpg,fapg,nbpg,x,vol&
     &)
            character(*) :: elrefz
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nbfpg
            character(len=8) :: fapg(*)
            integer :: nbpg(20)
            real(kind=8) :: x(*)
            real(kind=8) :: vol
          end subroutine elraca
        end interface
