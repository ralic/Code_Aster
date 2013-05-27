        interface
          subroutine nmextj(nomcha,nbcmp,listcp,extrcp,num,snum,nvalcp&
     &,nummai,jcesd,jcesv,jcesl,jcesc,valres)
            character(len=24) :: nomcha
            integer :: nbcmp
            character(len=24) :: listcp
            character(len=8) :: extrcp
            integer :: num
            integer :: snum
            integer :: nvalcp
            integer :: nummai
            integer :: jcesd
            integer :: jcesv
            integer :: jcesl
            integer :: jcesc
            real(kind=8) :: valres(*)
          end subroutine nmextj
        end interface
