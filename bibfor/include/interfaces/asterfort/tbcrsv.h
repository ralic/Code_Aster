        interface
          subroutine tbcrsv(nomta,baseta,nbpar,nompar,typpar,nblign)
            character(*) :: nomta
            character(*) :: baseta
            integer :: nbpar
            character(*) :: nompar(*)
            character(*) :: typpar(*)
            integer :: nblign
          end subroutine tbcrsv
        end interface
