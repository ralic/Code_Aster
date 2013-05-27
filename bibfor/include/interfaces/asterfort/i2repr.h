        interface
          subroutine i2repr(conec,type,maille,chemin,ptchm,nbchm,m1,m2&
     &)
            character(len=24) :: conec
            character(len=24) :: type
            integer :: maille(*)
            integer :: chemin(*)
            integer :: ptchm(*)
            integer :: nbchm
            integer :: m1(*)
            integer :: m2(*)
          end subroutine i2repr
        end interface
