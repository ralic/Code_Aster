        interface
          subroutine exchnn(descn,numn,tcmp,nbc,tvale,tnueq,b,valcmp,&
     &taber)
            integer :: descn(*)
            integer :: numn
            integer :: tcmp(*)
            integer :: nbc
            real(kind=8) :: tvale(*)
            integer :: tnueq(*)
            logical :: b
            real(kind=8) :: valcmp(*)
            integer :: taber(*)
          end subroutine exchnn
        end interface
