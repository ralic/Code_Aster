        interface
          subroutine errcou(nomprg,numpas,nomvar,info,nprog,nlu)
            character(len=8) :: nomprg
            integer(kind=4) :: numpas
            character(len=144) :: nomvar
            integer(kind=4) :: info
            integer(kind=4) :: nprog
            integer(kind=4) :: nlu
          end subroutine errcou
        end interface
