        interface
          subroutine tensk2(icabl,nbno,s,alpha,f0,delta,ea,frco,frli,&
     &sa,f)
            integer :: icabl
            integer :: nbno
            real(kind=8) :: s(*)
            real(kind=8) :: alpha(*)
            real(kind=8) :: f0
            real(kind=8) :: delta
            real(kind=8) :: ea
            real(kind=8) :: frco
            real(kind=8) :: frli
            real(kind=8) :: sa
            real(kind=8) :: f(*)
          end subroutine tensk2
        end interface
