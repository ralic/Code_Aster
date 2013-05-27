        interface
          subroutine rsacch(nomsdz,numch,nomch,nbord,liord,nbcmp,&
     &liscmp)
            character(*) :: nomsdz
            integer :: numch
            character(len=16) :: nomch
            integer :: nbord
            integer :: liord(*)
            integer :: nbcmp
            character(len=8) :: liscmp(*)
          end subroutine rsacch
        end interface
