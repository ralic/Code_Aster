        interface
          subroutine cgcrtb(table,option,lmelas,cas,typfis,lmoda,&
     &nbprup,noprup,typrup)
            integer :: nbprup
            character(len=8) :: table
            character(len=16) :: option
            logical :: lmelas
            character(len=16) :: cas
            character(len=8) :: typfis
            logical :: lmoda
            character(len=16) :: noprup(nbprup)
            character(len=8) :: typrup(nbprup)
          end subroutine cgcrtb
        end interface
