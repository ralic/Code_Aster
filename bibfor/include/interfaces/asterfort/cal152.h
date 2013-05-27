        interface
          subroutine cal152(option,max,may,maz,model,phib24,iphi1,&
     &iphi2,imade,modmec,chamno,num,vrai,i,j,mij,cij,kij)
            character(len=9) :: option
            character(len=19) :: max
            character(len=19) :: may
            character(len=19) :: maz
            character(len=2) :: model
            character(len=24) :: phib24
            integer :: iphi1
            integer :: iphi2
            integer :: imade
            character(len=8) :: modmec
            character(len=19) :: chamno
            character(len=14) :: num
            logical :: vrai
            integer :: i
            integer :: j
            real(kind=8) :: mij
            real(kind=8) :: cij
            real(kind=8) :: kij
          end subroutine cal152
        end interface
