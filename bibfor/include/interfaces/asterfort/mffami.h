        interface
          subroutine mffami(fid,maa,ind,fam,num,attid,attval,attdes,&
     &natt,gro,cret)
            integer :: fid
            character(*) :: maa
            integer :: ind
            character(*) :: fam
            integer :: num
            integer :: attid(*)
            integer :: attval(*)
            character(*) :: attdes(*)
            integer :: natt
            character(*) :: gro(*)
            integer :: cret
          end subroutine mffami
        end interface
