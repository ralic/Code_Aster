        interface
          subroutine recddl(nbcmp,lino,nbno,nbec,ideeq,neq,mcoddl,idec&
     &)
            integer :: neq
            integer :: nbec
            integer :: nbno
            integer :: nbcmp
            integer :: lino(nbno)
            integer :: ideeq(2,neq)
            integer :: mcoddl(nbno*nbec,2)
            integer :: idec(nbcmp,2)
          end subroutine recddl
        end interface
