        interface
          subroutine crpcvg(ma1,ma2,gma1,gma2,tran,prec,lima1,lima2,&
     &linoeu)
            character(len=8) :: ma1
            character(len=8) :: ma2
            character(len=24) :: gma1
            character(len=24) :: gma2
            real(kind=8) :: tran(3)
            real(kind=8) :: prec
            character(*) :: lima1
            character(*) :: lima2
            integer :: linoeu(*)
          end subroutine crpcvg
        end interface
