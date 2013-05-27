        interface
          subroutine nmdire(noeu1,noeu2,ndim,cnsln,grln,grlt,compo,&
     &vect)
            integer :: noeu1
            integer :: noeu2
            integer :: ndim
            character(len=19) :: cnsln
            character(len=19) :: grln
            character(len=19) :: grlt
            character(len=8) :: compo
            real(kind=8) :: vect(3)
          end subroutine nmdire
        end interface
