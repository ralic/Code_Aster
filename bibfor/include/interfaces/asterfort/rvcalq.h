        interface
          subroutine rvcalq(iocc,sdeval,vec1,vec2,repere,nomcp,nbcpnc,&
     &nbcpcd,option,quant,sdlieu,codir,valdir,sdcalq,courbe)
            integer :: iocc
            character(len=24) :: sdeval
            real(kind=8) :: vec1(*)
            real(kind=8) :: vec2(*)
            character(len=8) :: repere
            character(len=8) :: nomcp(*)
            integer :: nbcpnc
            integer :: nbcpcd
            character(len=16) :: option
            character(len=24) :: quant
            character(len=24) :: sdlieu
            integer :: codir
            real(kind=8) :: valdir(*)
            character(len=19) :: sdcalq
            character(len=8) :: courbe
          end subroutine rvcalq
        end interface
