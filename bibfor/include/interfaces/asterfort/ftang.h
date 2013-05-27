        interface
          subroutine ftang(fn,xlocal,vitloc,cfrotd,cfrots,ktang,ctang,&
     &iadher,oldvt,oldft,oldxlo,cost,sint,ftange,flocal,vt)
            real(kind=8) :: fn
            real(kind=8) :: xlocal(3)
            real(kind=8) :: vitloc(3)
            real(kind=8) :: cfrotd
            real(kind=8) :: cfrots
            real(kind=8) :: ktang
            real(kind=8) :: ctang
            integer :: iadher
            real(kind=8) :: oldvt(2)
            real(kind=8) :: oldft(2)
            real(kind=8) :: oldxlo(3)
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: ftange(2)
            real(kind=8) :: flocal(3)
            real(kind=8) :: vt(2)
          end subroutine ftang
        end interface
