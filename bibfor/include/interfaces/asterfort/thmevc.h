        interface
          subroutine thmevc(option,nomte,axi,nno,npg,ipoids,ivf,idfde,&
     &nddls,nnos,nddlm,nnom)
            character(len=16) :: option
            character(len=16) :: nomte
            logical :: axi
            integer :: nno
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: idfde
            integer :: nddls
            integer :: nnos
            integer :: nddlm
            integer :: nnom
          end subroutine thmevc
        end interface
