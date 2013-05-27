        interface
          subroutine recfor(numpas,nbpal,force,typal,finpal,cnpal,&
     &prdeff,conv)
            integer :: nbpal
            integer :: numpas
            real(kind=8) :: force(nbpal,*)
            character(len=6) :: typal(20)
            character(len=3) :: finpal(20)
            character(len=8) :: cnpal(20)
            logical :: prdeff
            real(kind=8) :: conv
          end subroutine recfor
        end interface
