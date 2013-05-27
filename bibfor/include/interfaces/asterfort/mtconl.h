        interface
          subroutine mtconl(nbcomb,typcst,const,lmat,typres,lres)
            integer :: nbcomb
            character(*) :: typcst(*)
            real(kind=8) :: const(*)
            integer :: lmat(*)
            character(*) :: typres
            integer :: lres
          end subroutine mtconl
        end interface
