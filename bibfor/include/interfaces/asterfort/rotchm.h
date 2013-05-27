        interface
          subroutine rotchm(profno,vale,tetss,nbss,invsk,nbnot,nbcmp,&
     &iax)
            integer :: nbnot
            integer :: nbss
            character(len=19) :: profno
            real(kind=8) :: vale(*)
            real(kind=8) :: tetss(nbss)
            integer :: invsk(nbnot,2)
            integer :: nbcmp
            integer :: iax
          end subroutine rotchm
        end interface
