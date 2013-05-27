        interface
          subroutine vfnulo(maxfa,maxar,ndim,nnos,nface,nbnofa,nosar,&
     &nosfa,narfa)
            integer :: nface
            integer :: maxar
            integer :: maxfa
            integer :: ndim
            integer :: nnos
            integer :: nbnofa(1:nface)
            integer :: nosar(1:maxar,2)
            integer :: nosfa(1:maxfa,*)
            integer :: narfa(1:maxfa,*)
          end subroutine vfnulo
        end interface
