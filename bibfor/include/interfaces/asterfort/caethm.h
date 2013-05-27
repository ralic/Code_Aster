        interface
          subroutine caethm(nomte,axi,perman,vf,typvf,typmod,modint,&
     &mecani,press1,press2,tempe,dimdep,dimdef,dimcon,nmec,np1,np2,ndim,&
     &nno,nnos,nnom,nface,npi,npg,nddls,nddlm,nddlfa,nddlk,dimuel,ipoids&
     &,ivf,idfde,ipoid2,ivf2,idfde2,npi2,jgano)
            character(len=16) :: nomte
            logical :: axi
            logical :: perman
            logical :: vf
            integer :: typvf
            character(len=8) :: typmod(2)
            character(len=3) :: modint
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
            integer :: ndim
            integer :: nno
            integer :: nnos
            integer :: nnom
            integer :: nface
            integer :: npi
            integer :: npg
            integer :: nddls
            integer :: nddlm
            integer :: nddlfa
            integer :: nddlk
            integer :: dimuel
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: ipoid2
            integer :: ivf2
            integer :: idfde2
            integer :: npi2
            integer :: jgano
          end subroutine caethm
        end interface
