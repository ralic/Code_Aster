        interface
          subroutine ssingu(nomail,nelem,nbr,ligrmo,alpha,re,he,chelem&
     &)
            integer :: nelem
            character(len=8) :: nomail
            integer :: nbr(nelem)
            character(len=24) :: ligrmo
            real(kind=8) :: alpha(nelem)
            real(kind=8) :: re(nelem)
            real(kind=8) :: he(nelem)
            character(len=24) :: chelem
          end subroutine ssingu
        end interface
