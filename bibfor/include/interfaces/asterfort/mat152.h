        interface
          subroutine mat152(option,model,moint,nocham,ivalk,nbmo,max,&
     &may,maz,num)
            character(len=9) :: option
            character(len=2) :: model
            character(len=8) :: moint
            character(len=24) :: nocham
            integer :: ivalk
            integer :: nbmo
            character(len=19) :: max
            character(len=19) :: may
            character(len=19) :: maz
            character(len=14) :: num
          end subroutine mat152
        end interface
