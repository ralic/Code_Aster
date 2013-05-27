        interface
          subroutine impc0(isor,ibl,nbc,tcm,tcmax,tcmin,nrebo,trebm,&
     &tct,t,nbpt)
            integer :: isor
            integer :: ibl
            integer :: nbc
            real(kind=8) :: tcm
            real(kind=8) :: tcmax
            real(kind=8) :: tcmin
            integer :: nrebo
            real(kind=8) :: trebm
            real(kind=8) :: tct
            real(kind=8) :: t(*)
            integer :: nbpt
          end subroutine impc0
        end interface
