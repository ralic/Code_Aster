        interface
          subroutine dpmat2(mod,mater,alpha,beta,dp,dpdeno,pplus,se,&
     &seq,plas,dsde)
            character(len=8) :: mod
            real(kind=8) :: mater(5,2)
            real(kind=8) :: alpha
            real(kind=8) :: beta
            real(kind=8) :: dp
            real(kind=8) :: dpdeno
            real(kind=8) :: pplus
            real(kind=8) :: se(6)
            real(kind=8) :: seq
            real(kind=8) :: plas
            real(kind=8) :: dsde(6,6)
          end subroutine dpmat2
        end interface
