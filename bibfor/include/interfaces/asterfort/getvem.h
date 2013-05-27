        interface
          subroutine getvem(noma,typent,motfac,motcle,iocc,iarg,mxval,&
     &vk,nbval)
            character(*) :: noma
            character(*) :: typent
            character(*) :: motfac
            character(*) :: motcle
            integer :: iocc
            integer :: iarg
            integer :: mxval
            character(*) :: vk(*)
            integer :: nbval
          end subroutine getvem
        end interface
