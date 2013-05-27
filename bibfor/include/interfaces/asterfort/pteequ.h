        interface
          subroutine pteequ(prchno,basz,neq,gds,ncmp,corr2)
            integer :: ncmp
            character(len=19) :: prchno
            character(*) :: basz
            integer :: neq
            integer :: gds
            integer :: corr2(ncmp)
          end subroutine pteequ
        end interface
