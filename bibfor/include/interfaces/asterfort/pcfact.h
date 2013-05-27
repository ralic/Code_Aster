        interface
          subroutine pcfact(matas,nequ,in,ip,ac,prc,vect,epsi)
            integer :: nequ
            character(len=19) :: matas
            integer :: in(nequ)
            integer(kind=4) :: ip(*)
            real(kind=8) :: ac(*)
            real(kind=8) :: prc(*)
            real(kind=8) :: vect(nequ)
            real(kind=8) :: epsi
          end subroutine pcfact
        end interface
