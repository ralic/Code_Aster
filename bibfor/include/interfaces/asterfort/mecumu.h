        interface
          subroutine mecumu(scal,ncmp,iad1,iad2,nec,dg1,dg2)
            integer :: nec
            character(len=8) :: scal
            integer :: ncmp
            integer :: iad1
            integer :: iad2
            integer :: dg1(nec)
            integer :: dg2(nec)
          end subroutine mecumu
        end interface
