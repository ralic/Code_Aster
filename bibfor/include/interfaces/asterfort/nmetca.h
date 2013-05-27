        interface
          subroutine nmetca(modele,noma,mate,sddisc,sdcriq,numins,&
     &valinc)
            character(len=24) :: modele
            character(len=8) :: noma
            character(len=24) :: mate
            character(len=19) :: sddisc
            character(len=24) :: sdcriq
            integer :: numins
            character(len=19) :: valinc(*)
          end subroutine nmetca
        end interface
