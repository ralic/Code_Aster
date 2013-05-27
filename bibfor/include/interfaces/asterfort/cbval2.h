        interface
          subroutine cbval2(nbcomb,typcst,const,lmat,typres,lres,&
     &ddlexc)
            integer :: nbcomb
            character(*) :: typcst(*)
            real(kind=8) :: const(*)
            integer :: lmat(*)
            character(*) :: typres
            integer :: lres
            character(*) :: ddlexc
          end subroutine cbval2
        end interface
