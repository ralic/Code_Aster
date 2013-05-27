        interface
          subroutine vefcur(vec1,nbn,knom,vec2,nbvale,nomnoe)
            integer :: nbvale
            integer :: nbn
            integer :: vec1(nbn)
            character(len=8) :: knom(nbvale)
            integer :: vec2(nbvale)
            character(len=24) :: nomnoe
          end subroutine vefcur
        end interface
