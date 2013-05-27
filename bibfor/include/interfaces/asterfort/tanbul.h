        interface
          subroutine tanbul(option,ndim,g,mate,compor,resi,mini,alpha,&
     &dsbdep,trepst)
            integer :: ndim
            character(len=16) :: option
            integer :: g
            integer :: mate
            character(len=16) :: compor
            logical :: resi
            logical :: mini
            real(kind=8) :: alpha
            real(kind=8) :: dsbdep(2*ndim,2*ndim)
            real(kind=8) :: trepst
          end subroutine tanbul
        end interface
