        interface
          subroutine ploint(vesto,modmec,chamno,num,i,vrai,model,veprj&
     &,modx,mody,modz)
            character(len=19) :: vesto
            character(*) :: modmec
            character(*) :: chamno
            character(len=14) :: num
            integer :: i
            logical :: vrai
            character(*) :: model
            character(len=19) :: veprj
            character(len=19) :: modx
            character(len=19) :: mody
            character(len=19) :: modz
          end subroutine ploint
        end interface
