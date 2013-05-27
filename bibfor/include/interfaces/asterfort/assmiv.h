        interface
          subroutine assmiv(base,vec,nbvec,tlivec,licoef,nu,vecpro,&
     &motcle,type)
            character(*) :: base
            character(*) :: vec
            integer :: nbvec
            character(*) :: tlivec(*)
            real(kind=8) :: licoef(*)
            character(*) :: nu
            character(*) :: vecpro
            character(len=4) :: motcle
            integer :: type
          end subroutine assmiv
        end interface
