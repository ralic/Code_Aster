        interface
          subroutine nmthmc(comp,modelz,moclef,k,comel,ncomel,nbnvi)
            character(len=16) :: comp
            character(*) :: modelz
            character(len=16) :: moclef
            integer :: k
            character(len=16) :: comel(*)
            integer :: ncomel
            integer :: nbnvi(4)
          end subroutine nmthmc
        end interface
