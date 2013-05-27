        interface
          subroutine i3imas(epsi,nil,tete,queue,succ,prec,desc,desctm,&
     &sgt,conex,vlc,coordo,sdrp1d,sdrpom,nbsgte)
            real(kind=8) :: epsi
            integer :: nil
            integer :: tete
            integer :: queue
            integer :: succ(*)
            integer :: prec(*)
            integer :: desc(*)
            integer :: desctm(*)
            real(kind=8) :: sgt(*)
            integer :: conex(*)
            integer :: vlc(*)
            real(kind=8) :: coordo(*)
            character(len=24) :: sdrp1d
            character(len=24) :: sdrpom
            integer :: nbsgte
          end subroutine i3imas
        end interface
