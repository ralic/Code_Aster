        interface
          subroutine lcrksg(comp,nvi,vinf,fd,df,nmat,coefl,sigi)
            integer :: nmat
            character(len=16) :: comp(*)
            integer :: nvi
            real(kind=8) :: vinf(*)
            real(kind=8) :: fd(9)
            real(kind=8) :: df(9)
            real(kind=8) :: coefl(nmat)
            real(kind=8) :: sigi(6)
          end subroutine lcrksg
        end interface
