        interface
          subroutine typthm(nomte,axi,perman,vf,typvf,typmod,ndim)
            character(len=16) :: nomte
            logical :: axi
            logical :: perman
            logical :: vf
            integer :: typvf
            character(len=8) :: typmod(2)
            integer :: ndim
          end subroutine typthm
        end interface
