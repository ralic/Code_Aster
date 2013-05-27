        interface
          subroutine assvss(base,vec,vecel,nu,vecpro,motcle,type,&
     &fomult,instap)
            character(*) :: base
            character(*) :: vec
            character(len=19) :: vecel
            character(*) :: nu
            character(*) :: vecpro
            character(len=4) :: motcle
            integer :: type
            character(len=24) :: fomult
            real(kind=8) :: instap
          end subroutine assvss
        end interface
