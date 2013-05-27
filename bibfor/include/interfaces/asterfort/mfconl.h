        interface
          subroutine mfconl(fid,maa,conn,csize,switch,typent,typgeo,&
     &typcon,cret)
            integer :: fid
            character(*) :: maa
            integer :: conn(*)
            integer :: csize
            integer :: switch
            integer :: typent
            integer :: typgeo
            integer :: typcon
            integer :: cret
          end subroutine mfconl
        end interface
