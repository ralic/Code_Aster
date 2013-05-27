        interface
          subroutine prosmo(matrez,limat,nbmat,basez,numedd,lsym,rouc)
            integer :: nbmat
            character(*) :: matrez
            character(*) :: limat(nbmat)
            character(*) :: basez
            character(*) :: numedd
            logical :: lsym
            character(len=1) :: rouc
          end subroutine prosmo
        end interface
