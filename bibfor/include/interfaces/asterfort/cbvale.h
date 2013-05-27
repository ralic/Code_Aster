        interface
          subroutine cbvale(nbcomb,typcst,const,lmat,typres,lres,&
     &ddlexc,matd)
            integer :: nbcomb
            character(*) :: typcst(*)
            real(kind=8) :: const(*)
            integer :: lmat(*)
            character(*) :: typres
            integer :: lres
            character(*) :: ddlexc
            logical :: matd
          end subroutine cbvale
        end interface
