        interface
          subroutine elpiv2(xjvmax,ndim,indic,nbliac,ajliai,spliai,llf&
     &,llf1,llf2,noma,defico,resoco)
            real(kind=8) :: xjvmax
            integer :: ndim
            integer :: indic
            integer :: nbliac
            integer :: ajliai
            integer :: spliai
            integer :: llf
            integer :: llf1
            integer :: llf2
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
          end subroutine elpiv2
        end interface
