        interface
          subroutine lisopt(prefob,nomo,typech,indxch,option,parain,&
     &paraou,carte,ligcal)
            character(len=13) :: prefob
            character(len=8) :: nomo
            character(len=8) :: typech
            integer :: indxch
            character(len=16) :: option
            character(len=8) :: parain
            character(len=8) :: paraou
            character(len=19) :: carte
            character(len=19) :: ligcal
          end subroutine lisopt
        end interface
