        interface
          subroutine cnvesl(lischa,typres,neq,nompar,valpar,cnvass)
            character(len=19) :: lischa
            character(len=1) :: typres
            integer :: neq
            character(len=8) :: nompar
            real(kind=8) :: valpar
            character(len=19) :: cnvass
          end subroutine cnvesl
        end interface
