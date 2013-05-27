        interface
          subroutine cfgli3(noma,defico,resoco,neq,nesmax,nbliai,&
     &nbliac,llf,llf1,llf2,ajliai,spliai,indic,xmul,liasup)
            character(len=8) :: noma
            character(len=24) :: defico
            character(len=24) :: resoco
            integer :: neq
            integer :: nesmax
            integer :: nbliai
            integer :: nbliac
            integer :: llf
            integer :: llf1
            integer :: llf2
            integer :: ajliai
            integer :: spliai
            integer :: indic
            real(kind=8) :: xmul
            logical :: liasup
          end subroutine cfgli3
        end interface
