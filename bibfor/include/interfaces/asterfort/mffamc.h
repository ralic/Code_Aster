        interface
          subroutine mffamc(fid,maa,fam,num,ngro,gro,cret)
            integer :: fid
            character(*) :: maa
            character(*) :: fam
            integer :: num
            integer :: ngro
            character(len=80) :: gro(*)
            integer :: cret
          end subroutine mffamc
        end interface
