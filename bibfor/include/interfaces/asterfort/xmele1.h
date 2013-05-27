        interface
          subroutine xmele1(noma,modele,defico,ligrel,nfiss,chelem,&
     &param,option)
            character(len=8) :: noma
            character(len=8) :: modele
            character(len=24) :: defico
            character(len=19) :: ligrel
            integer :: nfiss
            character(len=19) :: chelem
            character(*) :: param
            character(*) :: option
          end subroutine xmele1
        end interface
