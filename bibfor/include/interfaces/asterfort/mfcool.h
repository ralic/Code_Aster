        interface
          subroutine mfcool(fid,maa,coo,modcoo,cret)
            integer :: fid
            character(*) :: maa
            real(kind=8) :: coo(*)
            integer :: modcoo
            integer :: cret
          end subroutine mfcool
        end interface
