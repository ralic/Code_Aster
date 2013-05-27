        interface
          subroutine gcharg(modele,nchar,lchar,chvolu,cf1d2d,cf2d3d,&
     &chpres,chepsi,chpesa,chrota,fonc,epsi,time,iord)
            character(len=8) :: modele
            integer :: nchar
            character(len=8) :: lchar(*)
            character(len=19) :: chvolu
            character(len=19) :: cf1d2d
            character(len=19) :: cf2d3d
            character(len=19) :: chpres
            character(len=19) :: chepsi
            character(len=19) :: chpesa
            character(len=19) :: chrota
            logical :: fonc
            logical :: epsi
            real(kind=8) :: time
            integer :: iord
          end subroutine gcharg
        end interface
