        interface
          subroutine actimn(nbcmp,nbno,nbec,mcoddl,icodac)
            integer :: nbec
            integer :: nbno
            integer :: nbcmp
            integer :: mcoddl(nbno*nbec,2)
            integer :: icodac(nbno*nbec)
          end subroutine actimn
        end interface
