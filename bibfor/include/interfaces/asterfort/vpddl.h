        interface
          subroutine vpddl(raide,masse,neq,nblagr,nbcine,neqact,dlagr,&
     &dbloq,ier)
            integer :: neq
            character(len=19) :: raide
            character(len=19) :: masse
            integer :: nblagr
            integer :: nbcine
            integer :: neqact
            integer :: dlagr(neq)
            integer :: dbloq(neq)
            integer :: ier
          end subroutine vpddl
        end interface
