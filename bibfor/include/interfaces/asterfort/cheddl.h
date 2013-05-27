        interface
          subroutine cheddl(ideeq,neq,ino,ityp,iran,nbran)
            integer :: nbran
            integer :: neq
            integer :: ideeq(2,neq)
            integer :: ino
            integer :: ityp
            integer :: iran(nbran)
          end subroutine cheddl
        end interface
