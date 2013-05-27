        interface
          subroutine singue(cherrs,chenes,nomail,ndim,nnoem,nelem,xy,&
     &prec,ligrmo,chelem,types)
            integer :: nelem
            integer :: nnoem
            character(len=19) :: cherrs
            character(len=19) :: chenes
            character(len=8) :: nomail
            integer :: ndim
            real(kind=8) :: xy(3,nnoem)
            real(kind=8) :: prec
            character(len=24) :: ligrmo
            character(len=24) :: chelem
            character(len=16) :: types
          end subroutine singue
        end interface
