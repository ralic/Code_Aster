        interface
          subroutine xmrema(jcesd,jcesv,jcesl,noma,ndim,ifise,defico,&
     &izone,alias,mmait,amait,nmait,statue,geom,nummin,nummae,ifamin,&
     &ifacee,jeumin,t1min,t2min,ximin,yimin,projin,stamin,ifism)
            integer :: jcesd(10)
            integer :: jcesv(10)
            integer :: jcesl(10)
            character(len=8) :: noma
            integer :: ndim
            integer :: ifise
            character(len=24) :: defico
            integer :: izone
            character(len=8) :: alias
            integer :: mmait
            integer :: amait
            integer :: nmait
            integer :: statue
            real(kind=8) :: geom(3)
            integer :: nummin
            integer :: nummae
            integer :: ifamin
            integer :: ifacee
            real(kind=8) :: jeumin
            real(kind=8) :: t1min(3)
            real(kind=8) :: t2min(3)
            real(kind=8) :: ximin
            real(kind=8) :: yimin
            logical :: projin
            integer :: stamin
            integer :: ifism
          end subroutine xmrema
        end interface
