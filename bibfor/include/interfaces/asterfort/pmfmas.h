        interface
          subroutine pmfmas(nomte,option,rhoflu,icdmat,kanl,mlv)
            character(*) :: nomte
            character(*) :: option
            real(kind=8) :: rhoflu
            integer :: icdmat
            integer :: kanl
            real(kind=8) :: mlv(*)
          end subroutine pmfmas
        end interface
