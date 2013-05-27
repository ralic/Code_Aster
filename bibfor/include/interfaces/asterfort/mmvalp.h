        interface
          subroutine mmvalp(ndim,alias,nno,ncmp,ksi1,ksi2,valend,&
     &valept)
            integer :: ndim
            character(len=8) :: alias
            integer :: nno
            integer :: ncmp
            real(kind=8) :: ksi1
            real(kind=8) :: ksi2
            real(kind=8) :: valend(*)
            real(kind=8) :: valept(*)
          end subroutine mmvalp
        end interface
