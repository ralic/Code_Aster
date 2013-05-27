        interface
          subroutine merime(modelz,nchar,lchar,mate,carelz,exitim,time&
     &,compoz,matelz,nh,basz)
            character(*) :: modelz
            integer :: nchar
            character(*) :: lchar(*)
            character(*) :: mate
            character(*) :: carelz
            logical :: exitim
            real(kind=8) :: time
            character(*) :: compoz
            character(*) :: matelz
            integer :: nh
            character(*) :: basz
          end subroutine merime
        end interface
