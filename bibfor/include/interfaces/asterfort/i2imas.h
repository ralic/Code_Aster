        interface
          subroutine i2imas(epsi,conec,coord,typ,nbm,numail,xa,ya,xb,&
     &yb,nbseg,sgtor,sgtex,mail1,mail2,facor,facex,paror,parex)
            real(kind=8) :: epsi
            character(len=24) :: conec
            character(len=24) :: coord
            character(len=24) :: typ
            integer :: nbm
            integer :: numail(*)
            real(kind=8) :: xa
            real(kind=8) :: ya
            real(kind=8) :: xb
            real(kind=8) :: yb
            integer :: nbseg
            real(kind=8) :: sgtor(*)
            real(kind=8) :: sgtex(*)
            integer :: mail1(*)
            integer :: mail2(*)
            integer :: facor(*)
            integer :: facex(*)
            real(kind=8) :: paror(*)
            real(kind=8) :: parex(*)
          end subroutine i2imas
        end interface
