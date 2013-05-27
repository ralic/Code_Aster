        interface
          subroutine i2imac(epsi,conec,coord,typ,nbm,numail,xc,yc,r,&
     &alfinf,alfsup,nbseg,sgtor,sgtex,mail1,mail2,facor,facex,paror,&
     &parex)
            real(kind=8) :: epsi
            character(len=24) :: conec
            character(len=24) :: coord
            character(len=24) :: typ
            integer :: nbm
            integer :: numail(*)
            real(kind=8) :: xc
            real(kind=8) :: yc
            real(kind=8) :: r
            real(kind=8) :: alfinf
            real(kind=8) :: alfsup
            integer :: nbseg
            real(kind=8) :: sgtor(*)
            real(kind=8) :: sgtex(*)
            integer :: mail1(*)
            integer :: mail2(*)
            integer :: facor(*)
            integer :: facex(*)
            real(kind=8) :: paror(*)
            real(kind=8) :: parex(*)
          end subroutine i2imac
        end interface
