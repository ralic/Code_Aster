        interface
          subroutine stkmai(ifl,icl,iv,rv,cv,cnl,mcl,nbm,nume,numn,cnx&
     &,typ,fmt,irteti)
            integer :: nbm
            integer :: ifl
            integer :: icl
            integer :: iv
            real(kind=8) :: rv
            character(*) :: cv
            character(len=14) :: cnl
            character(len=8) :: mcl(nbm)
            integer :: nume
            integer :: numn
            character(len=24) :: cnx
            character(len=24) :: typ
            integer :: fmt(nbm)
            integer :: irteti
          end subroutine stkmai
        end interface
