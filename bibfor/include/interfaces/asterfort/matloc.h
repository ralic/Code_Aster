        interface
          subroutine matloc(noma,ncncin,motfac,ioc,ino,nbma,listma,pgl&
     &)
            character(len=8) :: noma
            character(len=24) :: ncncin
            character(len=16) :: motfac
            integer :: ioc
            integer :: ino
            integer :: nbma
            integer :: listma(*)
            real(kind=8) :: pgl(3,3)
          end subroutine matloc
        end interface
