        interface
          subroutine mmdepm(nbdm,ndim,nne,nnm,jdepm,jdepde,ffe,ffm,&
     &ddeple,ddeplm,deplme,deplmm)
            integer :: nbdm
            integer :: ndim
            integer :: nne
            integer :: nnm
            integer :: jdepm
            integer :: jdepde
            real(kind=8) :: ffe(9)
            real(kind=8) :: ffm(9)
            real(kind=8) :: ddeple(3)
            real(kind=8) :: ddeplm(3)
            real(kind=8) :: deplme(3)
            real(kind=8) :: deplmm(3)
          end subroutine mmdepm
        end interface
