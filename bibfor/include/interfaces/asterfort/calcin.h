        interface
          subroutine calcin(option,max,may,maz,model,veprj,modx,mody,&
     &modz,i,j,mij)
            character(*) :: option
            character(len=19) :: max
            character(len=19) :: may
            character(len=19) :: maz
            character(*) :: model
            character(len=19) :: veprj
            character(len=19) :: modx
            character(len=19) :: mody
            character(len=19) :: modz
            integer :: i
            integer :: j
            real(kind=8) :: mij
          end subroutine calcin
        end interface
