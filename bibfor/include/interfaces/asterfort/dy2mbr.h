        interface
          subroutine dy2mbr(numddl,neq,lischa,freq,vediri,veneum,&
     &vevoch,vassec,j2nd)
            character(len=14) :: numddl
            integer :: neq
            character(len=19) :: lischa
            real(kind=8) :: freq
            character(len=19) :: vediri
            character(len=19) :: veneum
            character(len=19) :: vevoch
            character(len=19) :: vassec
            integer :: j2nd
          end subroutine dy2mbr
        end interface
