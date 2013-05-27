        interface
          subroutine rctrac(jmat,ktrac,nomcl,temp,jprol,jvale,nbvale,e&
     &)
            integer :: jmat
            integer :: ktrac
            character(*) :: nomcl
            real(kind=8) :: temp
            integer :: jprol
            integer :: jvale
            integer :: nbvale
            real(kind=8) :: e
          end subroutine rctrac
        end interface
