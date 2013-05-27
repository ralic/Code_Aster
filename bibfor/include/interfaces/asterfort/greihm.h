        interface
          subroutine greihm(nomte,perman,ndim,mecani,press1,press2,&
     &tempe,dimdef,dimcon)
            character(len=16) :: nomte
            logical :: perman
            integer :: ndim
            integer :: mecani(8)
            integer :: press1(9)
            integer :: press2(9)
            integer :: tempe(5)
            integer :: dimdef
            integer :: dimcon
          end subroutine greihm
        end interface
