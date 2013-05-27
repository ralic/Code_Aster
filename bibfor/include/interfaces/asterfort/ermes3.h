        interface
          subroutine ermes3(noe,ifa,tymvol,nnof,typmav,iref1,ivois,&
     &isig,nbcmp,dsg11,dsg22,dsg33,dsg12,dsg13,dsg23)
            integer :: noe(9,6,4)
            integer :: ifa
            integer :: tymvol
            integer :: nnof
            character(len=8) :: typmav
            integer :: iref1
            integer :: ivois
            integer :: isig
            integer :: nbcmp
            real(kind=8) :: dsg11(9)
            real(kind=8) :: dsg22(9)
            real(kind=8) :: dsg33(9)
            real(kind=8) :: dsg12(9)
            real(kind=8) :: dsg13(9)
            real(kind=8) :: dsg23(9)
          end subroutine ermes3
        end interface
