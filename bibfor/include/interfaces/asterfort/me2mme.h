        interface
          subroutine me2mme(modelz,nchar,lchar,mate,caraz,exitim,time,&
     &matelz,nh,basez)
            character(*) :: modelz
            integer :: nchar
            character(*) :: lchar(*)
            character(*) :: mate
            character(*) :: caraz
            logical :: exitim
            real(kind=8) :: time
            character(*) :: matelz
            integer :: nh
            character(*) :: basez
          end subroutine me2mme
        end interface
