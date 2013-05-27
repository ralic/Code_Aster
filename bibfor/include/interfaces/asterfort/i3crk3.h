        interface
          subroutine i3crk3(typk,face,crf,crk)
            integer :: typk
            integer :: face
            real(kind=8) :: crf(*)
            real(kind=8) :: crk(*)
          end subroutine i3crk3
        end interface
