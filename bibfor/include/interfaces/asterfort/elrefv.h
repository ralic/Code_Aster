        interface
          subroutine elrefv(nomte,famil,ndim,nno,nno2,nnos,npg,ipoids,&
     &ivf,ivf2,idfde,idfde2,jgano,jgano2)
            character(len=16) :: nomte
            character(len=4) :: famil
            integer :: ndim
            integer :: nno
            integer :: nno2
            integer :: nnos
            integer :: npg
            integer :: ipoids
            integer :: ivf
            integer :: ivf2
            integer :: idfde
            integer :: idfde2
            integer :: jgano
            integer :: jgano2
          end subroutine elrefv
        end interface
