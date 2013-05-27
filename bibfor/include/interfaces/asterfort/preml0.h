        interface
          subroutine preml0(n1,n2,diag,col,delg,prno,deeq,nec,p,q,lbd1&
     &,lbd2,rl,rl1,rl2,nrl,lt,lmat)
            integer :: n1
            integer :: n2
            integer :: diag(0:*)
            integer :: col(*)
            integer :: delg(*)
            integer :: prno(*)
            integer :: deeq(*)
            integer :: nec
            integer :: p(*)
            integer :: q(*)
            integer :: lbd1(n1)
            integer :: lbd2(n1)
            integer :: rl(4,*)
            integer :: rl1(*)
            integer :: rl2(*)
            integer :: nrl
            integer :: lt
            integer :: lmat
          end subroutine preml0
        end interface
