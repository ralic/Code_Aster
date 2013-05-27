        interface
          subroutine calajt(j1,j,diag,col,n,itab,deb,tab,suiv,lt,ier)
            integer :: n
            integer :: j1
            integer :: j
            integer :: diag(0:n)
            integer :: col(*)
            integer :: itab
            integer :: deb(1:n)
            integer :: tab(*)
            integer :: suiv(*)
            integer :: lt
            integer :: ier
          end subroutine calajt
        end interface
