        interface
          subroutine extmod(basemo,numddl,nume,nbnumo,dmode,nbeq,nbnoe&
     &,iddl,nbddl)
            integer :: nbddl
            integer :: nbnoe
            integer :: nbnumo
            character(len=8) :: basemo
            character(len=14) :: numddl
            integer :: nume(nbnumo)
            real(kind=8) :: dmode(nbddl*nbnoe*nbnumo)
            integer :: nbeq
            integer :: iddl(nbddl)
          end subroutine extmod
        end interface
