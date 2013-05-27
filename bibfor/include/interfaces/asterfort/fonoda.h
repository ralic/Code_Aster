        interface
          subroutine fonoda(imate,perman,mecani,press1,press2,tempe,&
     &dimdef,dimcon,ndim,dt,fnoevo,congem,r)
            integer :: dimcon
            integer :: dimdef
            integer :: imate
            logical :: perman
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            integer :: ndim
            real(kind=8) :: dt
            logical :: fnoevo
            real(kind=8) :: congem(dimcon)
            real(kind=8) :: r(dimdef+1)
          end subroutine fonoda
        end interface
