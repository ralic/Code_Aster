        interface
          subroutine xprpfi(p,lsnp,lcmin,poifis,trifis,fiss,ndim,lsn,&
     &lst)
            real(kind=8) :: p(3)
            real(kind=8) :: lsnp
            real(kind=8) :: lcmin
            character(len=19) :: poifis
            character(len=19) :: trifis
            character(len=8) :: fiss
            integer :: ndim
            real(kind=8) :: lsn
            real(kind=8) :: lst
          end subroutine xprpfi
        end interface
