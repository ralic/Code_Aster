        interface
          subroutine itgthm(vf,typvf,modint,mecani,press1,press2,tempe&
     &,ndim,nno,nnos,nnom,nface,npi,npg,nddls,nddlk,nddlm,nddlfa,dimuel,&
     &ipoids,ivf,idfde,ipoid2,ivf2,idfde2,npi2,jgano)
            logical :: vf
            integer :: typvf
            character(len=3) :: modint
            integer :: mecani(5)
            integer :: press1(7)
            integer :: press2(7)
            integer :: tempe(5)
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnom
            integer :: nface
            integer :: npi
            integer :: npg
            integer :: nddls
            integer :: nddlk
            integer :: nddlm
            integer :: nddlfa
            integer :: dimuel
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: ipoid2
            integer :: ivf2
            integer :: idfde2
            integer :: npi2
            integer :: jgano
          end subroutine itgthm
        end interface
