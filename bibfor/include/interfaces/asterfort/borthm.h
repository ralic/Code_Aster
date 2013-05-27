        interface
          subroutine borthm(nomte,axi,vf,perman,typvf,typmod,ndim,&
     &ndlno,ndlnm)
            character(len=16) :: nomte
            logical :: axi
            logical :: vf
            logical :: perman
            integer :: typvf
            character(len=8) :: typmod(2)
            integer :: ndim
            integer :: ndlno
            integer :: ndlnm
          end subroutine borthm
        end interface
