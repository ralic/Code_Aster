        interface
          subroutine asgnbc(ibla,bloca,nbterm,inobl,iadbl,nomblo,&
     &numblo,fact)
            integer :: nbterm
            integer :: ibla
            complex(kind=8) :: bloca(*)
            integer :: inobl(nbterm)
            integer :: iadbl(nbterm)
            character(len=24) :: nomblo
            integer :: numblo
            real(kind=8) :: fact
          end subroutine asgnbc
        end interface
