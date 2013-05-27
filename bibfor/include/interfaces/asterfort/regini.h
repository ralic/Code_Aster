        interface
          subroutine regini(option,nomte,ivf,ivf2,idfde,idfde2,jgano,&
     &ndim,ipoids,ipoid2,npi,dimdef,nddls,nddlm,dimcon,typmod,dimuel,nno&
     &,nnom,nnos,regula,axi)
            character(len=16) :: option
            character(len=16) :: nomte
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            integer :: jgano
            integer :: ndim
            integer :: ipoids
            integer :: ipoid2
            integer :: npi
            integer :: dimdef
            integer :: nddls
            integer :: nddlm
            integer :: dimcon
            character(len=8) :: typmod(2)
            integer :: dimuel
            integer :: nno
            integer :: nnom
            integer :: nnos
            integer :: regula(6)
            logical :: axi
          end subroutine regini
        end interface
