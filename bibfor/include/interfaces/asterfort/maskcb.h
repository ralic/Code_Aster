        interface
          subroutine maskcb(nbcmp,nbno,nbec,mcoddl,imask,numord,nbdef)
            integer :: nbec
            integer :: nbno
            integer :: nbcmp
            integer :: mcoddl(nbno*nbec,2)
            integer :: imask(nbno*nbec)
            integer :: numord(nbno)
            integer :: nbdef
          end subroutine maskcb
        end interface
