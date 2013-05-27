        interface
          subroutine amumpt(option,kmonit,temps,rang,nbproc,kxmps,&
     &lquali,type,ietdeb,ietrat,rctdeb,ldist)
            integer :: option
            character(len=24) :: kmonit(12)
            real(kind=8) :: temps(6)
            integer :: rang
            integer :: nbproc
            integer :: kxmps
            logical :: lquali
            character(len=1) :: type
            integer :: ietdeb
            integer :: ietrat
            real(kind=8) :: rctdeb
            logical :: ldist
          end subroutine amumpt
        end interface
