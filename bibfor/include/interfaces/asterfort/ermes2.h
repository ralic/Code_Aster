        interface
          subroutine ermes2(ino,typema,typmav,iref1,ivois,isig,nbcmp,&
     &dsg11,dsg22,dsg12)
            integer :: ino
            character(len=8) :: typema
            character(len=8) :: typmav
            integer :: iref1
            integer :: ivois
            integer :: isig
            integer :: nbcmp
            real(kind=8) :: dsg11(3)
            real(kind=8) :: dsg22(3)
            real(kind=8) :: dsg12(3)
          end subroutine ermes2
        end interface
