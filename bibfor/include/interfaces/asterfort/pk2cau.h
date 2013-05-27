        interface
          subroutine pk2cau(nomte,ncmp,pk2,sigma)
            integer :: ncmp
            character(len=16) :: nomte
            real(kind=8) :: pk2(ncmp,1)
            real(kind=8) :: sigma(ncmp,1)
          end subroutine pk2cau
        end interface
