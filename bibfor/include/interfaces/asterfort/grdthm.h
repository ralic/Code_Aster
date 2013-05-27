        interface
          subroutine grdthm(nomte,perman,vf,ndim,mecani,press1,press2,&
     &tempe,dimdep,dimdef,dimcon,nmec,np1,np2)
            character(len=16) :: nomte
            logical :: perman
            logical :: vf
            integer :: ndim
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            integer :: dimdep
            integer :: dimdef
            integer :: dimcon
            integer :: nmec
            integer :: np1
            integer :: np2
          end subroutine grdthm
        end interface
