        interface
          subroutine dfllsv(lisifr,lisevr,lisevk,lisesu,isauve,even,&
     &action,submet,subaut,pasmin,nbrpas,niveau,pcplus,cmmaxi,delcol,&
     &durdec,penmax,cricmp,valere,nocham,nocmp)
            character(len=24) :: lisifr
            character(len=24) :: lisevr
            character(len=24) :: lisevk
            character(len=24) :: lisesu
            integer :: isauve
            character(len=16) :: even
            character(len=16) :: action
            character(len=16) :: submet
            character(len=16) :: subaut
            real(kind=8) :: pasmin
            integer :: nbrpas
            integer :: niveau
            real(kind=8) :: pcplus
            real(kind=8) :: cmmaxi
            real(kind=8) :: delcol
            real(kind=8) :: durdec
            real(kind=8) :: penmax
            character(len=16) :: cricmp
            real(kind=8) :: valere
            character(len=16) :: nocham
            character(len=16) :: nocmp
          end subroutine dfllsv
        end interface
