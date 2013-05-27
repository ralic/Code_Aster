        interface
          subroutine nmexti(nomnoe,champ,nbcmp,listcp,extrcp,nvalcp,&
     &valres)
            character(len=8) :: nomnoe
            character(len=19) :: champ
            integer :: nbcmp
            character(len=24) :: listcp
            character(len=8) :: extrcp
            integer :: nvalcp
            real(kind=8) :: valres(*)
          end subroutine nmexti
        end interface
