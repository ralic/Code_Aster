        interface
          subroutine mtcmbl(nbcomb,typcst,const,limat,matrez,ddlexc,&
     &numedd,elim)
            integer :: nbcomb
            character(*) :: typcst(nbcomb)
            real(kind=8) :: const(*)
            character(*) :: limat(nbcomb)
            character(*) :: matrez
            character(*) :: ddlexc
            character(*) :: numedd
            character(len=5) :: elim
          end subroutine mtcmbl
        end interface
