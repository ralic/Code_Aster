        interface
          subroutine cfcglc(ndim,neq,nesmax,resoco,iliai,jdecal,nbddl,&
     &jdepc,japddl,japcof,glis)
            integer :: ndim
            integer :: neq
            integer :: nesmax
            character(len=24) :: resoco
            integer :: iliai
            integer :: jdecal
            integer :: nbddl
            integer :: jdepc
            integer :: japddl
            integer :: japcof
            real(kind=8) :: glis
          end subroutine cfcglc
        end interface
