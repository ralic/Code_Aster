        interface
          subroutine utintc(zrino2,zrino1,zrjno2,zrjno1,x3,y3,inst,&
     &insold,k8cart,ltheta,nsomm,valfp,valfm,ifm,niv,option)
            real(kind=8) :: zrino2
            real(kind=8) :: zrino1
            real(kind=8) :: zrjno2
            real(kind=8) :: zrjno1
            real(kind=8) :: x3
            real(kind=8) :: y3
            real(kind=8) :: inst
            real(kind=8) :: insold
            character(len=8) :: k8cart
            logical :: ltheta
            integer :: nsomm
            real(kind=8) :: valfp(9)
            real(kind=8) :: valfm(9)
            integer :: ifm
            integer :: niv
            integer :: option
          end subroutine utintc
        end interface
