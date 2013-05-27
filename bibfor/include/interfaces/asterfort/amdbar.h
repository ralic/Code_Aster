        interface
          subroutine amdbar(n,pe,iw,len,iwlen,pfree,nv,next,last,head,&
     &elen,degree,ncmpa,w,iovflo)
            integer :: iwlen
            integer :: n
            integer :: pe(n)
            integer :: iw(iwlen)
            integer :: len(n)
            integer :: pfree
            integer :: nv(n)
            integer :: next(n)
            integer :: last(n)
            integer :: head(n)
            integer :: elen(n)
            integer :: degree(n)
            integer :: ncmpa
            integer :: w(n)
            integer :: iovflo
          end subroutine amdbar
        end interface
