        interface
          subroutine rvpstd(valee,type,codir,valdir,valeq)
            real(kind=8) :: valee(*)
            character(len=2) :: type
            integer :: codir
            real(kind=8) :: valdir(*)
            real(kind=8) :: valeq(*)
          end subroutine rvpstd
        end interface
