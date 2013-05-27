        interface
          subroutine lkdfdx(nbmat,mater,ucrip,invar,s,paraep,varpl,&
     &derpar,dfdxip)
            integer :: nbmat
            real(kind=8) :: mater(nbmat,2)
            real(kind=8) :: ucrip
            real(kind=8) :: invar
            real(kind=8) :: s(6)
            real(kind=8) :: paraep(3)
            real(kind=8) :: varpl(4)
            real(kind=8) :: derpar(3)
            real(kind=8) :: dfdxip
          end subroutine lkdfdx
        end interface
