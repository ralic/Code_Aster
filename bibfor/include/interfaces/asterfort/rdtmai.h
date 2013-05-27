        interface
          subroutine rdtmai(noma,nomare,base,corrn,corrm,bascor,nbmal,&
     &lima)
            character(len=8) :: noma
            character(len=8) :: nomare
            character(len=1) :: base
            character(*) :: corrn
            character(*) :: corrm
            character(len=1) :: bascor
            integer :: nbmal
            integer :: lima(*)
          end subroutine rdtmai
        end interface
