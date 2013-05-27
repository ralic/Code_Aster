        interface
          subroutine mfcone(fid,maa,conn,csize,switch,n,typent,typgeo,&
     &typcon,cret)
            integer :: fid
            character(*) :: maa
            integer :: conn(*)
            integer :: csize
            integer :: switch
            integer :: n
            integer :: typent
            integer :: typgeo
            integer :: typcon
            integer :: cret
          end subroutine mfcone
        end interface
