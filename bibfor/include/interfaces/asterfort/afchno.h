        interface
          subroutine afchno(chamn,base,gran,noma,nbnoeu,nbcpno,desc,&
     &lonval,typval,rval,cval,kval)
            character(*) :: chamn
            character(*) :: base
            character(*) :: gran
            character(*) :: noma
            integer :: nbnoeu
            integer :: nbcpno(*)
            integer :: desc(*)
            integer :: lonval
            character(*) :: typval
            real(kind=8) :: rval(*)
            complex(kind=8) :: cval(*)
            character(*) :: kval(*)
          end subroutine afchno
        end interface
