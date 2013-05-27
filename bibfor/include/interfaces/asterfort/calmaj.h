        interface
          subroutine calmaj(option,max,may,maz,model,vesto,modmec,&
     &chamno,num,vrai,i,j,mij)
            character(len=9) :: option
            character(len=19) :: max
            character(len=19) :: may
            character(len=19) :: maz
            character(*) :: model
            character(len=19) :: vesto
            character(*) :: modmec
            character(*) :: chamno
            character(len=14) :: num
            logical :: vrai
            integer :: i
            integer :: j
            real(kind=8) :: mij
          end subroutine calmaj
        end interface
