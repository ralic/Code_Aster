        interface
          subroutine i2imam(conec,type,lstmai,nbmlst,chemin,ptchm,&
     &nbchm,mail1,mail2)
            character(len=24) :: conec
            character(len=24) :: type
            integer :: lstmai(*)
            integer :: nbmlst
            integer :: chemin(*)
            integer :: ptchm(*)
            integer :: nbchm
            integer :: mail1(*)
            integer :: mail2(*)
          end subroutine i2imam
        end interface
