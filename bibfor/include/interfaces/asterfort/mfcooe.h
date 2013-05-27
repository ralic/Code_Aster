        interface
          subroutine mfcooe(fid,maa,coo,modcoo,n,cret)
            integer :: fid
            character(*) :: maa
            real(kind=8) :: coo(*)
            integer :: modcoo
            integer :: n
            integer :: cret
          end subroutine mfcooe
        end interface
