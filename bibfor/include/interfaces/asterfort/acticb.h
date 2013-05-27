        interface
          subroutine acticb(nbcmp,nbno,nbec,mcoddl,icono,icodac)
            integer :: nbec
            integer :: nbno
            integer :: nbcmp
            integer :: mcoddl(nbno*nbec,2)
            integer :: icono(nbno*nbec)
            integer :: icodac(nbno*nbec)
          end subroutine acticb
        end interface
