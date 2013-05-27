        interface
          subroutine lrmmno(fid,nomam2,ndim,nbnoeu,nomu,nomnoe,coordo,&
     &coodsc,cooref,ifm,infmed)
            integer :: fid
            character(*) :: nomam2
            integer :: ndim
            integer :: nbnoeu
            character(len=8) :: nomu
            character(len=24) :: nomnoe
            character(len=24) :: coordo
            character(len=24) :: coodsc
            character(len=24) :: cooref
            integer :: ifm
            integer :: infmed
          end subroutine lrmmno
        end interface
