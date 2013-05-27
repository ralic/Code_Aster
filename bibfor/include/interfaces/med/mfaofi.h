        interface
          subroutine mfaofi(fid,maa,it,fam,attnum,attval,attdes,num,&
     &gro,cret)
            integer :: fid
            character(*) :: maa
            integer :: it
            character(*) :: fam
            integer :: attnum(*)
            integer :: attval(*)
            character(*) :: attdes
            integer :: num
            character(*) :: gro
            integer :: cret
          end subroutine mfaofi
        end interface
