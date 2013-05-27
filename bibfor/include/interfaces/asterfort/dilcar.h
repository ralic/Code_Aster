        interface
          subroutine dilcar(option,icompo,icontm,ideplm,ideplp,igeom,&
     &imate,imatuu,ivectu,icontp,ivarip,ichg,ichn,jcret,idefo)
            character(len=16) :: option
            integer :: icompo
            integer :: icontm
            integer :: ideplm
            integer :: ideplp
            integer :: igeom
            integer :: imate
            integer :: imatuu
            integer :: ivectu
            integer :: icontp
            integer :: ivarip
            integer :: ichg
            integer :: ichn
            integer :: jcret
            integer :: idefo
          end subroutine dilcar
        end interface
