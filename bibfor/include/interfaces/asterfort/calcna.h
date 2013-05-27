        interface
          subroutine calcna(nomfin,nomfon,nbvalp,valep,noparp,nbvalf,&
     &valef,noparf)
            character(len=19) :: nomfin
            character(len=19) :: nomfon
            integer :: nbvalp
            real(kind=8) :: valep(*)
            character(len=24) :: noparp
            integer :: nbvalf
            real(kind=8) :: valef(*)
            character(len=24) :: noparf
          end subroutine calcna
        end interface
