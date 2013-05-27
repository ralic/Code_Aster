        interface
          subroutine mptran(nombas,nommes,nbmesu,nbmode,basepr,vnoeud,&
     &vrange,vcham)
            character(len=8) :: nombas
            character(len=8) :: nommes
            integer :: nbmesu
            integer :: nbmode
            character(len=24) :: basepr
            character(len=24) :: vnoeud
            character(len=24) :: vrange
            character(len=24) :: vcham
          end subroutine mptran
        end interface
