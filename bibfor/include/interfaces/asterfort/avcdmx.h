        interface
          subroutine avcdmx(nbvec,domtot,cudomx,vnormx,nbplan)
            integer :: nbvec
            real(kind=8) :: domtot(nbvec)
            real(kind=8) :: cudomx
            integer :: vnormx(2)
            integer :: nbplan
          end subroutine avcdmx
        end interface
