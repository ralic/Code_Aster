        interface
          subroutine oriema(nomail,tpmail,nbnmai,lnmail,typ3d,lnm3d,&
     &ndim,coor,reorie,norien,ifm,niv)
            character(len=8) :: nomail
            character(len=8) :: tpmail
            integer :: nbnmai
            integer :: lnmail(*)
            character(len=8) :: typ3d
            integer :: lnm3d(*)
            integer :: ndim
            real(kind=8) :: coor(*)
            logical :: reorie
            integer :: norien
            integer :: ifm
            integer :: niv
          end subroutine oriema
        end interface
