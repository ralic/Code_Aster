        interface
          subroutine calfft(np1,np4,nbm,n,dtext,fext,omegaf,aa,bb)
            integer :: n
            integer :: np4
            integer :: np1
            integer :: nbm
            real(kind=8) :: dtext
            real(kind=8) :: fext(np4,*)
            real(kind=8) :: omegaf(*)
            real(kind=8) :: aa(np4,*)
            real(kind=8) :: bb(np4,*)
          end subroutine calfft
        end interface
