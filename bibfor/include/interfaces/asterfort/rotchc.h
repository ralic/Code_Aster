        interface
          subroutine rotchc(profno,cvale,tetss,nbss,invsk,nbnot,nbcmp,&
     &iax)
            integer :: nbnot
            integer :: nbss
            character(len=19) :: profno
            complex(kind=8) :: cvale(*)
            real(kind=8) :: tetss(nbss)
            integer :: invsk(nbnot,2)
            integer :: nbcmp
            integer :: iax
          end subroutine rotchc
        end interface
