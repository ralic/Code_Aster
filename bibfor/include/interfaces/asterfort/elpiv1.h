        interface
          subroutine elpiv1(xjvmax,indic,nbliac,ajliai,spliai,spavan,&
     &noma,defico,resoco)
            real(kind=8) :: xjvmax
            integer :: indic
            integer :: nbliac
            integer :: ajliai
            integer :: spliai
            integer :: spavan
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
          end subroutine elpiv1
        end interface
