        interface
          subroutine nmmaji(numedd,lgrot,lendo,sdnume,coef,incmoz,&
     &ddincz,incplz,ordre)
            character(len=24) :: numedd
            logical :: lgrot
            logical :: lendo
            character(len=19) :: sdnume
            real(kind=8) :: coef
            character(*) :: incmoz
            character(*) :: ddincz
            character(*) :: incplz
            integer :: ordre
          end subroutine nmmaji
        end interface
