        interface
          subroutine putcon(nomres,nbind,ind,valr,vali,num,ier)
            integer :: nbind
            character(*) :: nomres
            integer :: ind(nbind)
            real(kind=8) :: valr(nbind)
            real(kind=8) :: vali(nbind)
            integer :: num
            integer :: ier
          end subroutine putcon
        end interface
