        interface
          subroutine fnoesu(option,nno,nnos,nnom,nface,congem,vectu,&
     &mecani,press1,press2,tempe,dimcon,dimuel,typvf,axi,ipoids,ivf,&
     &idfde,ipoid2,ivf2,idfde2,npi2,jgano,codret)
            integer :: dimuel
            integer :: dimcon
            character(len=16) :: option
            integer :: nno
            integer :: nnos
            integer :: nnom
            integer :: nface
            real(kind=8) :: congem(dimcon,7)
            real(kind=8) :: vectu(dimuel)
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            integer :: typvf
            logical :: axi
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: ipoid2
            integer :: ivf2
            integer :: idfde2
            integer :: npi2
            integer :: jgano
            integer :: codret
          end subroutine fnoesu
        end interface
