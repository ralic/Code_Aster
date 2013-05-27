        interface
          subroutine fortan(fn,xlocal,vitloc,cfrotd,cfrots,ktang,ctang&
     &,iadher,oldvt,oldft,oldxlo,cost,sint,ftange,flocal,vt)
            real(kind=8) :: fn
            real(kind=8) :: xlocal(*)
            real(kind=8) :: vitloc(*)
            real(kind=8) :: cfrotd
            real(kind=8) :: cfrots
            real(kind=8) :: ktang
            real(kind=8) :: ctang
            integer :: iadher
            real(kind=8) :: oldvt(*)
            real(kind=8) :: oldft(*)
            real(kind=8) :: oldxlo(*)
            real(kind=8) :: cost
            real(kind=8) :: sint
            real(kind=8) :: ftange(*)
            real(kind=8) :: flocal(*)
            real(kind=8) :: vt(*)
          end subroutine fortan
        end interface
