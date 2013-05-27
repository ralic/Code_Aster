        interface
          subroutine numeok(acces,ilu,rlu,listrz,listiz,precis,crit,&
     &epsi,astock)
            character(*) :: acces
            integer :: ilu
            real(kind=8) :: rlu
            character(*) :: listrz
            character(*) :: listiz
            integer :: precis
            character(*) :: crit
            real(kind=8) :: epsi
            logical :: astock
          end subroutine numeok
        end interface
