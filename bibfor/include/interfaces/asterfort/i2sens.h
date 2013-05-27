        interface
          subroutine i2sens(chemin,nbrma2,limail,nbrma,connex,typmai)
            integer :: nbrma
            integer :: nbrma2
            integer :: chemin(nbrma2)
            integer :: limail(nbrma)
            character(*) :: connex
            character(*) :: typmai
          end subroutine i2sens
        end interface
