        interface
          subroutine dsipdp(thmc,adcome,addep1,addep2,dimcon,dimdef,&
     &dsde,dspdp1,dspdp2,pre2tr)
            integer :: dimdef
            integer :: dimcon
            character(len=16) :: thmc
            integer :: adcome
            integer :: addep1
            integer :: addep2
            real(kind=8) :: dsde(dimcon,dimdef)
            real(kind=8) :: dspdp1
            real(kind=8) :: dspdp2
            logical :: pre2tr
          end subroutine dsipdp
        end interface
