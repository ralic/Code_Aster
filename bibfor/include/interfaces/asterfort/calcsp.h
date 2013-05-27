        interface
          subroutine calcsp(casint,nomu,table,freq,masg,nbm,nbmr,imod1&
     &,nuor,ivite)
            logical :: casint
            character(len=8) :: nomu
            character(len=8) :: table
            real(kind=8) :: freq(*)
            real(kind=8) :: masg(*)
            integer :: nbm
            integer :: nbmr
            integer :: imod1
            integer :: nuor(*)
            integer :: ivite
          end subroutine calcsp
        end interface
