        interface
          subroutine dlfext(nveca,nchar,temps,neq,liad,lifo,charge,&
     &infoch,fomult,modele,mate,carele,numedd,f)
            integer :: nveca
            integer :: nchar
            real(kind=8) :: temps
            integer :: neq
            integer :: liad(*)
            character(len=24) :: lifo(*)
            character(len=24) :: charge
            character(len=24) :: infoch
            character(len=24) :: fomult
            character(len=24) :: modele
            character(len=24) :: mate
            character(len=24) :: carele
            character(len=24) :: numedd
            real(kind=8) :: f(*)
          end subroutine dlfext
        end interface
